use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{intrinsic_symbols, intrinsic_type_symbols},
    module::ModuleContext,
};

pub type SymbolId = usize;

pub fn symbol_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let mut ctx = SymbolPassContext::new();

    ctx.resolve_symbols(compile_ctx);
    ctx.finalise(compile_ctx);

    Ok(())
}

struct SymbolPassContext {
    current_file: String,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    //module name -> (imported module -> (module symbol id, [imported symbols]))
    module_imports: HashMap<String, HashMap<String, (SymbolId, HashSet<String>)>>,
    module_type_imports: HashMap<String, HashMap<String, (SymbolId, HashSet<String>)>>,
    //module name -> (exported symbol -> symbol id)
    module_exports: HashMap<String, HashMap<String, SymbolId>>,
    module_type_exports: HashMap<String, HashMap<String, SymbolId>>,
    //module name -> (alias -> original module name)
    module_aliases: HashMap<String, HashMap<String, String>>,
    scope_stack: Vec<HashMap<String, SymbolId>>,
    type_scope_stack: Vec<HashMap<String, SymbolId>>,
    symbol_idx: SymbolId,
}

impl SymbolPassContext {
    fn new() -> Self {
        Self {
            current_file: String::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            module_exports: HashMap::new(),
            module_imports: HashMap::new(),
            module_type_exports: HashMap::new(),
            module_type_imports: HashMap::new(),
            module_aliases: HashMap::new(),
            scope_stack: Vec::new(),
            type_scope_stack: Vec::new(),
            symbol_idx: 0,
        }
    }

    fn finalise(self, ctx: &mut CompileContext) {
        //sync with compile ctx
        ctx.errors.extend(self.errors);
        ctx.warnings.extend(self.warnings);
        //sync exports and imports
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
        self.type_scope_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
        self.type_scope_stack.pop();
    }

    fn declare_symbol(&mut self, name: String, type_: bool) -> SymbolId {
        let id = self.symbol_idx;
        self.symbol_idx += 1;

        let scope_stack = if type_ {
            &mut self.type_scope_stack
        } else {
            &mut self.scope_stack
        };

        if let Some(scope) = scope_stack.last_mut() {
            scope.insert(name, id);
        }
        id
    }

    fn find_in_scope(&self, name: &str, type_: bool) -> Option<SymbolId> {
        let scope_stack = if type_ {
            &self.type_scope_stack
        } else {
            &self.scope_stack
        };
        for scope in scope_stack.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        None
    }

    fn resolve_symbols(&mut self, ctx: &mut CompileContext) {
        self.scope_stack = vec![intrinsic_symbols(&mut self.symbol_idx)];
        self.type_scope_stack = vec![intrinsic_type_symbols(&mut self.symbol_idx)];

        for (package_name, modules) in &mut ctx.packages {
            for (_, module_ctx) in modules {
                self.current_file = module_ctx.file.clone();
                self.push_scope();

                self.resolve_top_level_module_symbols(
                    package_name.as_str(),
                    module_ctx.name.as_str(),
                    &mut module_ctx.ast,
                );
            }
        }
    }

    fn declare_binding(
        &mut self,
        symbol_node: &mut Node,
        in_match: bool,
    ) -> Result<Vec<(String, SymbolId)>, ()> {
        if let NodeKind::Expr { expr } = &mut symbol_node.kind {
            use ExprKind::*;
            match expr {
                Symbol { name, .. } => {
                    let symbol_id = self.declare_symbol(name.clone(), false);
                    symbol_node.symbol_id = Some(symbol_id);
                    Ok(vec![(name.clone(), symbol_id)])
                }
                TupleLit(elements) => {
                    let mut symbol_ids = vec![];
                    for elem in elements {
                        symbol_ids.extend(self.declare_binding(elem, in_match)?);
                    }

                    Ok(symbol_ids)
                }
                ListLit(elements) => {
                    let mut symbol_ids = vec![];
                    for elem in elements {
                        symbol_ids.extend(self.declare_binding(elem, in_match)?);
                    }

                    Ok(symbol_ids)
                }
                FnCall { args, .. } if in_match => {
                    let mut symbol_ids = vec![];
                    for arg in args {
                        symbol_ids.extend(self.declare_binding(&mut arg.1, in_match)?);
                    }

                    Ok(symbol_ids)
                }
                _ => {
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Invalid binding pattern".to_string(),
                            file: self.current_file.clone(),
                            span: symbol_node.span,
                            err_type: DiagnosticMsgType::InvalidBindingPattern,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                    Err(())
                }
            }
        } else {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Invalid binding pattern".to_string(),
                    file: self.current_file.clone(),
                    span: symbol_node.span,
                    err_type: DiagnosticMsgType::InvalidBindingPattern,
                },
                notes: vec![],
                hints: vec![],
            });
            Err(())
        }
    }

    fn resolve_scoped_symbols(&mut self, module_name: &str, node: &mut Node) {
        use ExprKind::*;
        use NodeKind::*;
        match &mut node.kind {
            Expr { expr } => match expr {
                Symbol { name, .. } => {
                    let symbol_id = self.find_in_scope(name, false);
                    if let Some(id) = symbol_id {
                        node.symbol_id = Some(id);
                        return;
                    }

                    //can't be a module, they're all field accessed
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Undeclared symbol '{}'", name),
                            file: self.current_file.clone(),
                            span: node.span,
                            err_type: DiagnosticMsgType::UndeclaredSymbol,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                }

                FieldAccess { expr, field } => {
                    //only resolve symbols on the base expr
                    if let Expr {
                        expr: Symbol { name },
                    } = &mut expr.kind
                    {
                        //if imported module alias, resolve module and register imported symbol (i.e. field)
                        if let Some(imported_module) = self
                            .module_aliases
                            .get(module_name)
                            .and_then(|a| a.get(name))
                        {
                            let module_imports = match self
                                .module_imports
                                .get_mut(module_name)
                                .and_then(|a| a.get_mut(imported_module))
                            {
                                Some(a) => a,
                                None => {
                                    self.errors.push(Diagnostic {
                                        primary: DiagnosticMsg {
                                            message: format!(
                                                "Module '{}' not imported",
                                                imported_module
                                            ),
                                            file: self.current_file.clone(),
                                            span: node.span,
                                            err_type: DiagnosticMsgType::ModuleNotFound,
                                        },
                                        notes: vec![],
                                        hints: vec![],
                                    });
                                    return;
                                }
                            };

                            //link to module symbol
                            expr.symbol_id = Some(module_imports.0);
                            //register imported symbol
                            module_imports.1.insert(field.clone());
                        } else {
                            //not imported, try resolve as normal symbol
                            self.resolve_scoped_symbols(module_name, expr);
                        }
                    } else {
                        //not the base expr, resolve one level deeper
                        self.resolve_scoped_symbols(module_name, expr);
                    }
                }

                _ => {}
            },
            _ => {}
        }
    }

    fn resolve_top_level_module_symbols(
        &mut self,
        _package: &str,
        module_name: &str,
        node: &mut Node,
    ) {
        use NodeKind::*;
        match &mut node.kind {
            Program { children, .. } => {
                for child in children {
                    self.resolve_top_level_module_symbols(module_name, module_name, child);
                }
            }

            Import {
                module,
                symbol,
                top_level,
                top_level_types,
                ..
            } => {
                let symbol_id = self.declare_symbol(symbol.clone(), false);
                node.symbol_id = Some(symbol_id);

                //register the import
                self.module_imports
                    .entry(module_name.to_string())
                    .or_insert_with(HashMap::new)
                    .insert(module.clone(), (symbol_id, HashSet::new()));

                //register alias for symbol resolution
                self.module_aliases
                    .entry(module_name.to_string())
                    .or_insert_with(HashMap::new)
                    .insert(symbol.clone(), module.clone());

                //register directly imported types
                for top_level_type in top_level_types {
                    let _ = self.declare_symbol(top_level_type.clone(), true);
                    let imported_types = self
                        .module_type_imports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::new)
                        .entry(module.to_string())
                        .or_insert_with(|| (symbol_id, HashSet::new()));

                    imported_types.1.insert(top_level_type.clone());
                }

                //register directly imported symbols
                for top_level_symbol in top_level {
                    let _ = self.declare_symbol(top_level_symbol.clone(), false);
                    let imported_symbols = self
                        .module_imports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::new)
                        .entry(module.to_string())
                        .or_insert_with(|| (symbol_id, HashSet::new()));

                    imported_symbols.1.insert(top_level_symbol.clone());
                }
            }

            FnDecl { symbol, public, .. } => {
                let symbol_id = self.declare_symbol(symbol.clone(), false);
                node.symbol_id = Some(symbol_id);
                if *public {
                    self.module_exports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::new)
                        .insert(symbol.clone(), symbol_id);
                }
            }

            ConstDecl { symbol, public, .. } => {
                if let Ok(symbol_ids) = self.declare_binding(symbol, false) {
                    if *public {
                        for (symbol_name, symbol_id) in &symbol_ids {
                            self.module_exports
                                .entry(module_name.to_string())
                                .or_insert_with(HashMap::new)
                                .insert(symbol_name.clone(), *symbol_id);
                        }
                    }
                }
            }

            TypeDecl {
                symbol,
                public,
                variants,
                ..
            } => {
                //declare type
                let symbol_id = self.declare_symbol(symbol.clone(), true);
                node.symbol_id = Some(symbol_id);
                if *public {
                    self.module_type_exports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::new)
                        .insert(symbol.clone(), symbol_id);
                }

                if !variants.is_empty() {
                    //declare the type constructors
                    for variant in variants {
                        match &mut variant.kind {
                            NodeKind::TypeConstructor { symbol, .. } => {
                                //type constructors are not types, treated as normal funcs
                                let symbol_id = self.declare_symbol(symbol.clone(), false);
                                variant.symbol_id = Some(symbol_id);
                                if *public {
                                    self.module_exports
                                        .entry(module_name.to_string())
                                        .or_insert_with(HashMap::new)
                                        .insert(symbol.clone(), symbol_id);
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }

            _ => {}
        }
    }
}
