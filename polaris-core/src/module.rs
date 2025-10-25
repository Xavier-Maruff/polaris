use petgraph::{
    algo::{condensation, toposort},
    graph::DiGraph,
};
use rustc_hash::FxHashMap as HashMap;

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    log::Logger,
    symbol::{SymbolContext, SymbolId},
};

pub type ModuleId = String;

#[derive(Clone, Debug)]
pub struct ModuleContext {
    pub name: String,
    pub file: String,
    pub ast: Node,
}

#[derive(Debug, Default)]
pub struct DepGraphContext {
    pub sccs: Vec<Vec<ModuleId>>,
    pub modules: HashMap<ModuleId, ModuleContext>,
}

pub fn dependency_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let mut dep_ctx = DependencyContext::new(compile_ctx);
    dep_ctx.build_condensation_graph();
    dep_ctx.link_module_deps();
    dep_ctx.finalise();
    Ok(())
}

struct DependencyContext<'a> {
    errors: &'a mut Vec<Diagnostic>,
    _warnings: &'a mut Vec<Diagnostic>,
    symbols: &'a SymbolContext,
    deps: &'a mut DepGraphContext,
    logger: Logger,
}

impl<'a> DependencyContext<'a> {
    fn new(compile_ctx: &'a mut CompileContext) -> Self {
        Self {
            errors: &mut compile_ctx.errors,
            _warnings: &mut compile_ctx.warnings,
            symbols: &compile_ctx.symbols,
            logger: compile_ctx.logger.clone(),
            deps: &mut compile_ctx.dependencies,
        }
    }

    fn finalise(self) {
        //noop because everything is done in place
    }

    //not the most efficient implementation, building sub maps for each module separately
    fn link_module_deps(&mut self) {
        for scc in self.deps.sccs.iter() {
            for module_id in scc.iter() {
                let module = self.deps.modules.get_mut(module_id).unwrap();
                let ast = &mut module.ast;

                let imports = self
                    .symbols
                    .imports
                    .get(module_id)
                    .cloned()
                    .unwrap_or_default();

                let type_imports = self
                    .symbols
                    .type_imports
                    .get(module_id)
                    .cloned()
                    .unwrap_or_default();

                let mut subs: HashMap<SymbolId, HashMap<String, SymbolId>> = HashMap::default();

                //get real ids for each imported symbol, create substitution map
                for (imported_module, (local_module_id, symbols)) in imports.iter() {
                    let mut map = HashMap::default();

                    if let Some(exports) = self.symbols.exports.get(imported_module) {
                        for export_name in symbols.iter() {
                            let export_id = exports.get(export_name);
                            //not going to error here, will be caught during substitution visitor
                            //where code span info is available
                            if let Some(export_id) = export_id {
                                map.insert(export_name.clone(), export_id.clone());
                            }
                        }
                    } else {
                        self.logger.error(&format!(
                            "Module '{}' imported by '{}' has no exports (module may not exist or has no public members)",
                            imported_module, module_id
                        ));
                    }

                    subs.insert(local_module_id.clone(), map);
                }

                //everything again for types because I am dumb
                for (imported_module, (local_module_id, symbols)) in type_imports.iter() {
                    let mut map = HashMap::default();

                    if let Some(exports) = self.symbols.type_exports.get(imported_module) {
                        for export_name in symbols.iter() {
                            let export_id = exports.get(export_name);
                            if let Some(export_id) = export_id {
                                map.insert(export_name.clone(), export_id.clone());
                            }
                        }
                    } else {
                        self.logger.error(&format!(
                            "Module '{}' imported by '{}' has no type exports (module may not exist or has no public types)",
                            imported_module, module_id
                        ));
                    }

                    subs.entry(local_module_id.clone())
                        .and_modify(|e| e.extend(map.clone()))
                        .or_insert(map);
                }

                if subs.is_empty() {
                    continue;
                }

                self.logger.debug(&format!(
                    "Applying substitutions for module '{}': {:?}",
                    module_id, subs
                ));

                DependencyContext::apply_symbol_substitutions(
                    ast,
                    &subs,
                    &module.file,
                    self.errors,
                    &self.logger,
                );
            }
        }
    }

    /// target node, map module symbol id in target module -> map (export symbol name -> symbol id in imported module)
    fn apply_symbol_substitutions(
        node: &mut Node,
        subs: &HashMap<SymbolId, HashMap<String, SymbolId>>,
        current_file: &String,
        errors: &mut Vec<Diagnostic>,
        logger: &Logger,
    ) {
        use NodeKind::*;
        match &mut node.kind {
            Import { .. } => {}
            Module { children } => {
                for child in children.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        child,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            TypeAlias { actual, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    actual,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }

            TypeDecl { variants, .. } => {
                for variant in variants.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        variant,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            TypeConstructor { fields, .. } => {
                for (_, field, _) in fields.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        field,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            Type {
                symbol, type_vars, ..
            } => {
                //if imported type, sub node id from import module id -> imported symbol id
                if let Some(symbol_id) = node.symbol_id
                    && let Some(sub) = subs.get(&symbol_id).and_then(|m| m.get(symbol))
                {
                    logger.debug(&format!(
                        "Substituting type symbol '{}' (id {}) with imported symbol id {}",
                        symbol, symbol_id, sub
                    ));
                    node.symbol_id = Some(*sub);
                }

                for ty in type_vars.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        ty,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
            FnType {
                args, return_type, ..
            } => {
                for arg in args.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        arg,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
                if let Some(return_type) = return_type.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        return_type,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            TupleType { elements } => {
                for elem in elements.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        elem,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            FnDecl {
                args,
                return_type,
                expr,
                ..
            } => {
                for arg in args.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        arg.0.as_mut(),
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                    if let Some(arg_type) = arg.1.as_mut() {
                        DependencyContext::apply_symbol_substitutions(
                            arg_type,
                            subs,
                            current_file,
                            errors,
                            logger,
                        );
                    }
                }
                if let Some(return_type) = return_type.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        return_type,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
                if let Some(expr) = expr.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        expr,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            ConstDecl {
                const_type, expr, ..
            } => {
                if let Some(const_type) = const_type.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        const_type,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }

            Expr { .. } => {
                DependencyContext::apply_symbol_substitution_expr(
                    node,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
        }
    }

    fn apply_symbol_substitution_expr(
        node: &mut Node,
        subs: &HashMap<SymbolId, HashMap<String, SymbolId>>,
        current_file: &String,
        errors: &mut Vec<Diagnostic>,
        logger: &Logger,
    ) {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => unreachable!(),
        };

        use ExprKind::*;
        match expr {
            IntLit(..) | RealLit { .. } | StringLit(..) | Discard => {}
            ListLit(elements) | TupleLit(elements) => {
                for elem in elements.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        elem,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
            ListPattern(elements) => {
                use crate::ast::ListPatternElement;
                for elem in elements.iter_mut() {
                    match elem {
                        ListPatternElement::Element(node) => {
                            DependencyContext::apply_symbol_substitutions(
                                node,
                                subs,
                                current_file,
                                errors,
                                logger,
                            );
                        }
                        ListPatternElement::Rest(Some(node)) => {
                            DependencyContext::apply_symbol_substitutions(
                                node,
                                subs,
                                current_file,
                                errors,
                                logger,
                            );
                        }
                        ListPatternElement::Rest(None) => {
                            //anon rest pattern
                        }
                    }
                }
            }
            MapLit(pairs) => {
                for (key, value) in pairs.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        key,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                    DependencyContext::apply_symbol_substitutions(
                        value,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            Symbol { name, .. } => {
                if let Some(symbol_id) = node.symbol_id {
                    //check if imported module alias
                    if let Some(export_map) = subs.get(&symbol_id) {
                        if let Some(sub) = export_map.get(name) {
                            logger.debug(&format!(
                                "Substituting expr symbol '{}' (id {}) with imported symbol id {}",
                                name, symbol_id, sub
                            ));
                            node.symbol_id = Some(*sub);
                        } else {
                            errors.push(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!("Module does not export a member {}", name),
                                    err_type: DiagnosticMsgType::UndefinedVariable,
                                    span: node.span.clone(),
                                    file: current_file.clone(),
                                },
                                hints: vec![],
                                notes: vec![],
                            });
                        }
                    }
                    // else: symbol_id not in subs map - it's a regular local symbol, no substitution needed
                }
            }

            LetBinding {
                symbol_type, expr, ..
            } => {
                if let Some(symbol_type) = symbol_type.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        symbol_type,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }

            FieldAccess { expr, field } => {
                //if base expr is a symbol, the field might need to be substituted
                if let NodeKind::Expr {
                    expr: ExprKind::Symbol { name, .. },
                } = &expr.kind
                    && let Some(symbol_id) = expr.symbol_id
                    && subs.contains_key(&symbol_id)
                {
                    //if field in sub map, replace field access with symbol node
                    if let Some(sub) = subs.get(&symbol_id).and_then(|m| m.get(field)) {
                        node.kind = NodeKind::Expr {
                            expr: ExprKind::Symbol {
                                name: sub.to_string(),
                            },
                        };
                        node.symbol_id = Some(*sub);
                    } else {
                        errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Module {} does not export a member {}",
                                    name, field
                                ),
                                err_type: DiagnosticMsgType::UndefinedVariable,
                                span: node.span.clone(),
                                file: current_file.clone(),
                            },
                            hints: vec![],
                            notes: vec![],
                        });
                    }
                } else {
                    DependencyContext::apply_symbol_substitutions(
                        expr,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }

            UnaryOp { expr, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
            BinaryOp { left, right, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    left,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                DependencyContext::apply_symbol_substitutions(
                    right,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
            Match { expr, arms, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                for (pats, arm_expr) in arms.iter_mut() {
                    for pat in pats.iter_mut() {
                        DependencyContext::apply_symbol_substitutions(
                            pat,
                            subs,
                            current_file,
                            errors,
                            logger,
                        );
                    }
                    DependencyContext::apply_symbol_substitutions(
                        arm_expr,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
            FnCall { callee, args, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    callee,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                for (_, arg) in args.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        arg,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
            Closure {
                args,
                return_type,
                expr,
                ..
            } => {
                for arg in args.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        arg.0.as_mut(),
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                    if let Some(arg_type) = arg.1.as_mut() {
                        DependencyContext::apply_symbol_substitutions(
                            arg_type,
                            subs,
                            current_file,
                            errors,
                            logger,
                        );
                    }
                }
                if let Some(return_type) = return_type.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        return_type,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
            For {
                start, end, body, ..
            } => {
                DependencyContext::apply_symbol_substitutions(
                    start,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                DependencyContext::apply_symbol_substitutions(
                    end,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                DependencyContext::apply_symbol_substitutions(
                    body,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
            IndexAccess { expr, index, .. } => {
                DependencyContext::apply_symbol_substitutions(
                    expr,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                DependencyContext::apply_symbol_substitutions(
                    index,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
            }
            IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                DependencyContext::apply_symbol_substitutions(
                    condition,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                DependencyContext::apply_symbol_substitutions(
                    then_branch,
                    subs,
                    current_file,
                    errors,
                    logger,
                );
                if let Some(else_branch) = else_branch.as_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        else_branch,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
            Block(statements) => {
                for stmt in statements.iter_mut() {
                    DependencyContext::apply_symbol_substitutions(
                        stmt,
                        subs,
                        current_file,
                        errors,
                        logger,
                    );
                }
            }
        }
    }

    /// import graph -> sccs -> condensation dag -> topological sort
    fn build_condensation_graph(&mut self) {
        let mut import_graph: DiGraph<ModuleId, ()> = DiGraph::new();
        let mut node_indices = HashMap::default();

        //register all modules as nodes
        for module in self.deps.modules.keys() {
            node_indices
                .entry(module)
                .or_insert_with(|| import_graph.add_node(module.clone()));
        }

        //add import edges
        for (module, deps) in self.symbols.imports.iter() {
            let module_idx = node_indices.get(module).unwrap();
            let imports = deps.keys();
            for import in imports {
                if let Some(dep_idx) = node_indices.get(&import) {
                    import_graph.add_edge(*module_idx, *dep_idx, ());
                } else {
                    //imported module not found - should have been caught in symbol resolution
                    self.logger.critical(&format!(
                        "Internal error: imported module '{}' not found for module '{}'. If you are seeing this message, please report a bug.",
                        import, module
                    ));
                }
            }
        }

        //feels dumb to just write this twice, who caaaares though truly
        for (module, deps) in self.symbols.type_imports.iter() {
            let module_idx = node_indices.get(module).unwrap();
            let imports = deps.keys();
            for import in imports {
                if let Some(dep_idx) = node_indices.get(&import) {
                    import_graph.add_edge(*module_idx, *dep_idx, ());
                } else {
                    //imported module not found - should have been caught in symbol resolution
                    self.logger.critical(&format!(
                                "Internal error: imported module '{}' not found for module '{}'. If you are seeing this message, please report a bug.",
                                import, module
                            ));
                }
            }
        }

        //sccs
        let sccs = condensation(import_graph, true);
        //shouldn't theoretically be possible to fail
        let order = toposort(&sccs, None).unwrap();

        for node_idx in order {
            let scc = &sccs[node_idx];
            let scc_modules = scc.iter().cloned().collect::<Vec<ModuleId>>();
            self.deps.sccs.push(scc_modules);
        }

        self.deps.sccs.reverse();
    }
}
