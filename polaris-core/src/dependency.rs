use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg},
    log::Logger,
    parse::CodeSpan,
    symbol::SymbolId,
    visit_ast_children,
};

pub fn dependency_resolution_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut deps_ctx = DepResolutionPassContext::new(ctx);
    deps_ctx.run_dep_resolution_pass(ctx)?;

    ctx.errors.extend(deps_ctx.errors);
    ctx.warnings.extend(deps_ctx.warnings);
    Ok(())
}

struct DepResolutionPassContext {
    _logger: Logger,
    current_file: String,
    rewrite_map: HashMap<SymbolId, SymbolId>,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
}

impl DepResolutionPassContext {
    fn new(ctx: &mut CompileContext) -> Self {
        Self {
            _logger: ctx.logger.clone(),
            current_file: String::new(),
            rewrite_map: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    fn run_dep_resolution_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        for scc in &ctx.modules.condensed_import_graph {
            let mut unresolved_links = ctx
                .symbol_table
                .symbol_links
                .iter()
                .filter(|(module_id, _)| scc.contains(module_id))
                .map(|(module_id, export_refs)| {
                    (
                        *module_id,
                        export_refs
                            .iter()
                            .filter_map(|(export_name, ref_ids)| {
                                if ref_ids.is_empty() {
                                    None
                                } else {
                                    Some((export_name.clone(), ref_ids.clone()))
                                }
                            })
                            .collect::<HashMap<_, Vec<SymbolId>>>(),
                    )
                })
                .collect::<HashMap<_, _>>();

            while !unresolved_links.is_empty() {
                let mut new_unresolved_links = Vec::new();
                let mut remove_unresolved_links = Vec::new();

                for module_id in scc {
                    let export_refs = unresolved_links.get(module_id);
                    let module = ctx.modules.get_module_borrow(*module_id).unwrap();

                    if export_refs.is_none() || export_refs.as_ref().unwrap().is_empty() {
                        continue;
                    }

                    let export_refs = export_refs.as_ref().unwrap();

                    for (export_name, ref_ids) in export_refs.iter() {
                        remove_unresolved_links.push((module_id, export_name));

                        if export_name.contains("::") {
                            let (head, tail) = export_name.split_once("::").unwrap();
                            let head_export = module.exports.get(head);
                            if head_export.is_none() {
                                self.errors.push(Diagnostic {
                                    primary: DiagnosticMsg {
                                        message: format!(
                                            "Module '{}' does not export '{}'",
                                            module.name, head
                                        ),
                                        file: module.file.clone(),
                                        span: CodeSpan::new(0, 0),
                                        err_type:
                                            crate::diagnostic::DiagnosticMsgType::UndeclaredSymbol,
                                    },
                                    notes: vec![],
                                    hints: vec![],
                                });
                                continue;
                            }

                            //need to get the module that head is referencing
                            let head_ref = ctx.symbol_table.exported_imports.get(head);
                            if head_ref.is_none() {
                                self.errors.push(Diagnostic {
                                    primary: DiagnosticMsg {
                                        message: format!(
                                            "Head ref not found: module '{}' does not export '{}'",
                                            module.name, head
                                        ),
                                        file: module.file.clone(),
                                        span: CodeSpan::new(0, 0),
                                        err_type:
                                            crate::diagnostic::DiagnosticMsgType::UndeclaredSymbol,
                                    },
                                    notes: vec![],
                                    hints: vec![],
                                });
                                continue;
                            }

                            let module_id = head_ref.unwrap().2;
                            if !unresolved_links.contains_key(&module_id) {
                                self.errors.push(Diagnostic {
                                    primary: DiagnosticMsg {
                                        message: format!(
                                            "No unresolved links for module '{}', export '{}'",
                                            module.name, head
                                        ),
                                        file: module.file.clone(),
                                        span: CodeSpan::new(0, 0),
                                        err_type:
                                            crate::diagnostic::DiagnosticMsgType::UndeclaredSymbol,
                                    },
                                    notes: vec![],
                                    hints: vec![],
                                });
                                continue;
                            }

                            new_unresolved_links.push((module_id, (tail.to_string(), ref_ids)));
                            continue;
                        }

                        let export = module.exports.get(export_name);
                        if export.is_none() {
                            self.errors.push(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!(
                                        "Module '{}' does not export '{}'",
                                        module.name, export_name
                                    ),
                                    file: module.file.clone(),
                                    span: CodeSpan::new(0, 0),
                                    err_type:
                                        crate::diagnostic::DiagnosticMsgType::UndeclaredSymbol,
                                },
                                notes: vec![],
                                hints: vec![],
                            });
                            continue;
                        }

                        let export_id = export.unwrap().id;
                        for symbol_id in ref_ids {
                            self.rewrite_map.insert(*symbol_id, export_id);
                        }
                    }
                }

                // remove processed unresolved links (module_id, export_name)
                let mut unresolved_links_p = unresolved_links.clone();
                for (module_id, export_name) in remove_unresolved_links {
                    if let Some(export_refs) = unresolved_links_p.get_mut(&module_id) {
                        export_refs.remove(export_name.as_str());
                        if export_refs.is_empty() {
                            unresolved_links_p.remove(&module_id);
                        }
                    }
                }

                //add second order resolution links
                for (module_id, (export_name, ref_ids)) in new_unresolved_links {
                    if let Some(export_refs) = unresolved_links_p.get_mut(&module_id) {
                        export_refs.insert(export_name, ref_ids.clone());
                    } else {
                        unresolved_links_p
                            .insert(module_id, HashMap::from([(export_name, ref_ids.clone())]));
                    }
                }

                unresolved_links = unresolved_links_p;
            }
        }

        //execute the id rewrites
        for (file, ast) in &mut ctx.asts {
            self.current_file = file.clone();
            self.id_rewrite_visitor(ast)?;
        }

        Ok(())
    }

    fn id_rewrite_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        match &mut ast.variant {
            Variant::Expr(ExprNode::Ident { id, .. }) => {
                if let Some(id) = id
                    && let Some(new_id) = self.rewrite_map.get(&id)
                {
                    *id = *new_id;
                }
            }
            _ => {}
        }
        visit_ast_children!(ast.variant, self, id_rewrite_visitor, {});
        Ok(())
    }
}
