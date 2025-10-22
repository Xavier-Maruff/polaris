//use std::collections::{HashMap, HashSet};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{intrinsic_symbols, intrinsic_type_symbols},
    parse::CodeSpan,
};

pub type SymbolId = usize;
///module name -> (imported module -> (module symbol id, [imported symbols]))
pub type ImportedSymbolMap = HashMap<String, HashMap<String, (SymbolId, HashSet<String>)>>;
///module name -> (exported symbol -> symbol id)
pub type ExportedSymbolMap = HashMap<String, HashMap<String, SymbolId>>;

pub fn symbol_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let mut ctx = SymbolPassContext::new();

    ctx.resolve_symbols(compile_ctx);
    ctx.finalise(compile_ctx);

    Ok(())
}

#[derive(Clone, Debug, Default)]
pub struct SymbolContext {
    pub imports: ImportedSymbolMap,
    pub type_imports: ImportedSymbolMap,
    pub exports: ExportedSymbolMap,
    pub type_exports: ExportedSymbolMap,
    pub intrinsic_symbols: HashMap<String, SymbolId>,
    pub intrinsic_types: HashMap<String, SymbolId>,
    //constructor symbol id -> type symbol id
    pub type_constructors: HashMap<SymbolId, SymbolId>,
    pub symbol_idx: usize,
    pub symbol_names: HashMap<SymbolId, String>,
}

struct SymbolPassContext {
    current_file: String,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    symbol_decl_locs: HashMap<SymbolId, (String, CodeSpan)>,
    symbols: SymbolContext,
    //module name -> (alias -> original module name)
    module_aliases: HashMap<String, HashMap<String, String>>,
    scope_stack: Vec<HashMap<String, SymbolId>>,
    type_scope_stack: Vec<HashMap<String, SymbolId>>,
    //anonymous scopes are additional scopes that are bound to the current scope,
    //and popped in groups with the current scope rather than individually
    //this allows for bindings to redeclare symbols in the same nominal scope
    anonymous_scope_depth: Vec<usize>,
}

impl SymbolContext {
    pub fn merge(&mut self, other: SymbolContext) {
        self.imports.extend(other.imports);
        self.type_imports.extend(other.type_imports);
        self.exports.extend(other.exports);
        self.type_exports.extend(other.type_exports);
        self.symbol_idx = self.symbol_idx.max(other.symbol_idx);
    }
}

impl SymbolPassContext {
    fn new() -> Self {
        Self {
            current_file: String::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            symbol_decl_locs: HashMap::default(),
            symbols: SymbolContext::default(),
            module_aliases: HashMap::default(),
            scope_stack: Vec::new(),
            type_scope_stack: Vec::new(),
            anonymous_scope_depth: Vec::new(),
        }
    }

    fn finalise(self, ctx: &mut CompileContext) {
        //sync with compile ctx
        ctx.errors.extend(self.errors);
        ctx.warnings.extend(self.warnings);
        ctx.symbols = self.symbols;
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::default());
        self.type_scope_stack.push(HashMap::default());
        self.anonymous_scope_depth.push(0);
    }

    fn pop_scope(&mut self) {
        //anonymous scopes are added when declaring let bindings
        //to allow for redeclarations in the same scope
        let anonymous_depth = self.anonymous_scope_depth.last().unwrap_or(&0).clone() as u64;
        for _ in 0..anonymous_depth {
            self.scope_stack.pop();
            self.type_scope_stack.pop();
        }
        self.anonymous_scope_depth.pop();

        self.scope_stack.pop();
        self.type_scope_stack.pop();
    }

    fn push_anonymous_scope(&mut self) {
        self.scope_stack.push(HashMap::default());
        self.type_scope_stack.push(HashMap::default());
        self.anonymous_scope_depth.last_mut().map(|d| *d += 1);
    }

    fn declare_symbol(
        &mut self,
        span: CodeSpan,
        name: String,
        type_: bool,
        error_on_redeclare: bool,
        mirror_existing: Option<SymbolId>,
    ) -> SymbolId {
        //pass an existing symbol that this symbol aliases at this stage
        //to be used for import resolution
        if let Some(mirror_id) = mirror_existing {
            let scope_stack = if type_ {
                &mut self.type_scope_stack
            } else {
                &mut self.scope_stack
            };

            if let Some(scope) = scope_stack.last_mut() {
                scope.insert(name, mirror_id);
            }
            return mirror_id;
        }

        if error_on_redeclare && let Some(id) = self.find_in_scope(&name, type_) {
            let prior_decl_loc = self
                .symbol_decl_locs
                .get(&id)
                .cloned()
                .unwrap_or((self.current_file.clone(), span));

            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!("Duplicate declaration of symbol '{}'", name),
                    file: self.current_file.clone(),
                    span,
                    err_type: DiagnosticMsgType::MultipleDeclarations,
                },
                notes: vec![DiagnosticMsg {
                    message: format!("Previous declaration of '{}' here", name),
                    file: prior_decl_loc.0.clone(),
                    span: prior_decl_loc.1,
                    err_type: DiagnosticMsgType::MultipleDeclarations,
                }],
                hints: vec![],
            });
            return id;
        }

        let id = self.symbols.symbol_idx;
        self.symbols.symbol_idx += 1;

        let scope_stack = if type_ {
            &mut self.type_scope_stack
        } else {
            &mut self.scope_stack
        };

        if let Some(scope) = scope_stack.last_mut() {
            scope.insert(name.clone(), id);
        }

        self.symbol_decl_locs
            .insert(id, (self.current_file.clone(), span));
        self.symbols.symbol_names.insert(id, name);

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
        self.symbols.intrinsic_symbols = intrinsic_symbols(&mut self.symbols.symbol_idx);
        self.symbols.intrinsic_types = intrinsic_type_symbols(&mut self.symbols.symbol_idx);

        //add reverse lookup for error messages
        //not super efficient but that is a provlem for the future
        self.symbols.symbol_names.extend(
            self.symbols
                .intrinsic_symbols
                .iter()
                .map(|(k, v)| (*v, k.clone())),
        );
        self.symbols.symbol_names.extend(
            self.symbols
                .intrinsic_types
                .iter()
                .map(|(k, v)| (*v, k.clone())),
        );

        self.scope_stack = vec![self.symbols.intrinsic_symbols.clone()];
        self.type_scope_stack = vec![self.symbols.intrinsic_types.clone()];

        for (package_name, modules) in &mut ctx.packages {
            for module in modules {
                let module_ctx = ctx
                    .dependencies
                    .modules
                    .get_mut(module)
                    .expect("Internal error: module not found in dependencies");
                self.current_file = module_ctx.file.clone();
                self.push_scope();

                self.resolve_top_level_module_symbols(
                    package_name.as_str(),
                    module_ctx.name.as_str(),
                    &mut module_ctx.ast,
                );

                self.resolve_scoped_symbols(module_ctx.name.as_str(), &mut module_ctx.ast, true);

                self.pop_scope();
            }
        }
    }

    fn declare_binding(
        &mut self,
        symbol_node: &mut Node,
        in_match: bool,
        mirror_symbols: &Option<HashMap<String, SymbolId>>,
    ) -> Result<HashMap<String, SymbolId>, ()> {
        if let NodeKind::Expr { expr } = &mut symbol_node.kind {
            use ExprKind::*;
            match expr {
                Discard => Ok(HashMap::default()),
                Symbol { name, .. } => {
                    //if capitalised, is a type constructor, don't declare, instead link to type constructor symbol
                    if name
                        .chars()
                        .next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false)
                    {
                        symbol_node.symbol_id = self.find_in_scope(name, false);
                        if symbol_node.symbol_id.is_none() {
                            self.errors.push(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!("Undeclared type constructor '{}'", name),
                                    file: self.current_file.clone(),
                                    span: symbol_node.span,
                                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                                },
                                notes: vec![],
                                hints: vec![],
                            });
                            return Err(());
                        }
                        return Ok(HashMap::default());
                    }

                    //check if symbol needs to be mirror prior declaration
                    //only happens in match patterns
                    let symbol_id = if let Some(symbol_id) =
                        mirror_symbols.as_ref().and_then(|s| s.get(name))
                    {
                        symbol_id.clone()
                    } else {
                        self.declare_symbol(symbol_node.span, name.clone(), false, false, None)
                    };

                    symbol_node.symbol_id = Some(symbol_id);
                    //Ok(HashMap::from([(name.clone(), symbol_id)]))
                    Ok([(name.clone(), symbol_id)].into_iter().collect())
                }
                TupleLit(elements) => {
                    let mut symbol_ids = HashMap::default();
                    for elem in elements {
                        symbol_ids.extend(self.declare_binding(elem, in_match, mirror_symbols)?);
                    }

                    Ok(symbol_ids)
                }
                ListLit(elements) => {
                    //list patterns only allowed in match exprs, otherwise not typesafe
                    if !in_match {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "List patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a tuple pattern instead".to_string()],
                        });
                        return Err(());
                    }

                    let mut symbol_ids = HashMap::default();
                    for elem in elements {
                        symbol_ids.extend(self.declare_binding(elem, in_match, mirror_symbols)?);
                    }

                    Ok(symbol_ids)
                }
                ListPattern(elements) => {
                    use crate::ast::ListPatternElement;

                    if !in_match {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "List patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a tuple pattern instead".to_string()],
                        });
                        return Err(());
                    }

                    let mut symbol_ids = HashMap::default();
                    for elem in elements {
                        match elem {
                            ListPatternElement::Element(node) => {
                                symbol_ids.extend(self.declare_binding(
                                    node,
                                    in_match,
                                    mirror_symbols,
                                )?);
                            }
                            ListPatternElement::Wildcard => {
                                //
                            }
                            ListPatternElement::Rest(Some(node)) => {
                                symbol_ids.extend(self.declare_binding(
                                    node,
                                    in_match,
                                    mirror_symbols,
                                )?);
                            }
                            ListPatternElement::Rest(None) => {
                                //
                            }
                        }
                    }

                    Ok(symbol_ids)
                }
                FnCall { callee, args, .. } => {
                    //constructor pattern only allowed in match
                    if !in_match {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "Constructor patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a tuple pattern instead".to_string()],
                        });
                        return Err(());
                    }

                    //only allow type constructors in match patterns
                    if let NodeKind::Expr {
                        expr: ExprKind::Symbol { name, .. },
                    } = &mut callee.kind
                    {
                        if name
                            .chars()
                            .next()
                            .map(|c| c.is_uppercase())
                            .unwrap_or(false)
                        {
                            callee.symbol_id = self.find_in_scope(name, false);
                            if callee.symbol_id.is_none() {
                                self.errors.push(Diagnostic {
                                    primary: DiagnosticMsg {
                                        message: format!("Undeclared type constructor '{}'", name),
                                        file: self.current_file.clone(),
                                        span: callee.span,
                                        err_type: DiagnosticMsgType::UndeclaredSymbol,
                                    },
                                    notes: vec![],
                                    hints: vec![],
                                });
                                return Err(());
                            }

                            let mut symbol_ids = HashMap::default();
                            for arg in args {
                                symbol_ids.extend(self.declare_binding(
                                    &mut arg.1,
                                    in_match,
                                    mirror_symbols,
                                )?);
                            }
                            return Ok(symbol_ids);
                        }
                    }

                    //not a constructor pattern
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Invalid binding pattern - function calls in patterns must be type constructors".to_string(),
                            file: self.current_file.clone(),
                            span: symbol_node.span,
                            err_type: DiagnosticMsgType::InvalidBindingPattern,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                    Err(())
                }
                IntLit(_) => {
                    if in_match {
                        Ok(HashMap::default())
                    } else {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "Integer literal patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a variable pattern instead".to_string()],
                        });
                        Err(())
                    }
                }
                RealLit { .. } => {
                    if in_match {
                        Ok(HashMap::default())
                    } else {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "Real literal patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a variable pattern instead".to_string()],
                        });
                        Err(())
                    }
                }
                StringLit(_) => {
                    if in_match {
                        Ok(HashMap::default())
                    } else {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "String literal patterns are not allowed in let bindings - only type-safe destructuring patterns like tuples are permitted".to_string(),
                                file: self.current_file.clone(),
                                span: symbol_node.span,
                                err_type: DiagnosticMsgType::InvalidBindingPattern,
                            },
                            notes: vec![],
                            hints: vec!["Consider using a variable pattern instead".to_string()],
                        });
                        Err(())
                    }
                }
                //todo: add binary op destructuring patterns
                _ => {
                    let message = if in_match {
                        "Invalid pattern in match expression".to_string()
                    } else {
                        "Invalid pattern in let binding - only identifiers and tuple patterns are allowed".to_string()
                    };

                    let hints = if in_match {
                        vec![]
                    } else {
                        vec!["Let bindings support only type-safe destructuring patterns like (a, b) = tuple".to_string()]
                    };

                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message,
                            file: self.current_file.clone(),
                            span: symbol_node.span,
                            err_type: DiagnosticMsgType::InvalidBindingPattern,
                        },
                        notes: vec![],
                        hints,
                    });
                    Err(())
                }
            }
        } else {
            let message = if in_match {
                "Invalid pattern in match expression".to_string()
            } else {
                "Invalid pattern in let binding - only identifiers and tuple patterns are allowed"
                    .to_string()
            };

            let hints = if in_match {
                vec![]
            } else {
                vec!["Let bindings support only type-safe destructuring patterns like (a, b) = tuple".to_string()]
            };

            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message,
                    file: self.current_file.clone(),
                    span: symbol_node.span,
                    err_type: DiagnosticMsgType::InvalidBindingPattern,
                },
                notes: vec![],
                hints,
            });
            Err(())
        }
    }

    fn resolve_scoped_symbols(&mut self, module_name: &str, node: &mut Node, top_level: bool) {
        use NodeKind::*;
        match &mut node.kind {
            Module { children, .. } => {
                for child in children {
                    self.resolve_scoped_symbols(module_name, child, true);
                }
            }
            TypeAlias { actual, .. } => {
                self.resolve_scoped_symbols(module_name, actual, false);
            }
            TypeDecl { variants, .. } => {
                for variant in variants {
                    self.resolve_scoped_symbols(module_name, variant, false);
                }
            }
            TypeConstructor { fields, .. } => {
                for (_, ty, _) in fields {
                    self.resolve_scoped_symbols(module_name, ty, true);
                }
            }

            Type {
                symbol,
                type_vars,
                parent_module,
                ..
            } => {
                //resolve type vars
                for ty in type_vars {
                    self.resolve_scoped_symbols(module_name, ty, true);
                }

                //resolve type symbol
                if let Some(type_id) = self.find_in_scope(symbol, true) {
                    node.symbol_id = Some(type_id);
                } else {
                    //if lowercase, it's a parametric type var, implicitly declare on first use
                    if symbol
                        .chars()
                        .next()
                        .map(|c| c.is_lowercase())
                        .unwrap_or(false)
                    {
                        let type_id =
                            self.declare_symbol(node.span, symbol.clone(), true, false, None);
                        node.symbol_id = Some(type_id);
                        return;
                    } else if let Some(parent_module) = parent_module {
                        //this is an imported type
                        //look up the module in the imports
                        let import_module_id = self.find_in_scope(parent_module, false);
                        //set type parent module to the imported module symbol id
                        if let Some(import_module_id) = import_module_id {
                            node.symbol_id = Some(import_module_id)
                        } else {
                            //import not found
                            self.errors.push(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!(
                                        "Undeclared module '{}' for type '{}'",
                                        parent_module, symbol
                                    ),
                                    file: self.current_file.clone(),
                                    span: node.span,
                                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                                },
                                notes: vec![],
                                hints: vec![],
                            });
                        }
                        return;
                    }

                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Undeclared type '{}'", symbol,),
                            file: self.current_file.clone(),
                            span: node.span,
                            err_type: DiagnosticMsgType::UndeclaredSymbol,
                        },
                        notes: vec![],
                        hints: vec![format!(
                            "If this is a parametric type variable, use lowercase (e.g. '{}')",
                            symbol.to_lowercase()
                        )],
                    });
                }
            }

            FnType {
                args, return_type, ..
            } => {
                for arg in args {
                    self.resolve_scoped_symbols(module_name, arg, true);
                }
                if let Some(return_type) = return_type {
                    self.resolve_scoped_symbols(module_name, return_type, true);
                }
            }

            TupleType { elements } => {
                for elem in elements {
                    self.resolve_scoped_symbols(module_name, elem, true);
                }
            }

            FnDecl {
                expr,
                return_type,
                args,
                ..
            } => {
                self.push_scope();
                for arg in args {
                    if let Ok(_) = self.declare_binding(&mut arg.0, false, &None) {
                        if let Some(ty) = &mut arg.1 {
                            self.resolve_scoped_symbols(module_name, ty, false);
                        }
                    }
                }
                if return_type.is_some() {
                    self.resolve_scoped_symbols(module_name, return_type.as_mut().unwrap(), false);
                }
                if expr.is_some() {
                    self.resolve_scoped_symbols(module_name, expr.as_mut().unwrap(), false);
                }
                self.pop_scope();
            }
            ConstDecl {
                expr,
                symbol,
                const_type,
                ..
            } => {
                if !top_level {
                    let _ = self.declare_binding(symbol, false, &None);
                }
                if let Some(ty) = const_type {
                    self.resolve_scoped_symbols(module_name, ty, false);
                }
                self.resolve_scoped_symbols(module_name, expr, false);
            }
            Expr { .. } => self.resolve_expr(module_name, node),
            Import { .. } => {}
        }
    }

    fn resolve_expr(&mut self, module_name: &str, node: &mut Node) {
        use ExprKind::*;
        use NodeKind::*;

        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => return,
        };

        match expr {
            IntLit { .. } | StringLit { .. } | RealLit { .. } | Discard => {}
            LetBinding {
                symbols,
                symbol_type,
                expr,
                ..
            } => {
                self.resolve_scoped_symbols(module_name, expr, false);
                if let Some(symbol_type) = symbol_type {
                    self.resolve_scoped_symbols(module_name, symbol_type, false);
                }
                self.push_anonymous_scope();
                let _ = self.declare_binding(symbols, false, &None);
            }
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
                            .symbols
                            .imports
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
                        self.resolve_scoped_symbols(module_name, expr, false);
                    }
                } else {
                    //not the base expr, resolve one level deeper
                    self.resolve_scoped_symbols(module_name, expr, false);
                }
            }

            ListLit(elements) | TupleLit(elements) => {
                for elem in elements {
                    self.resolve_scoped_symbols(module_name, elem, false);
                }
            }
            ListPattern(elements) => {
                use crate::ast::ListPatternElement;
                for elem in elements {
                    match elem {
                        ListPatternElement::Element(node) => {
                            self.resolve_scoped_symbols(module_name, node, false);
                        }
                        ListPatternElement::Wildcard => {
                            //
                        }
                        ListPatternElement::Rest(Some(node)) => {
                            self.resolve_scoped_symbols(module_name, node, false);
                        }
                        ListPatternElement::Rest(None) => {
                            //
                        }
                    }
                }
            }

            MapLit(pairs) => {
                for (key, value) in pairs {
                    self.resolve_scoped_symbols(module_name, key, false);
                    self.resolve_scoped_symbols(module_name, value, false);
                }
            }

            BinaryOp { left, right, .. } => {
                self.resolve_scoped_symbols(module_name, left, false);
                self.resolve_scoped_symbols(module_name, right, false);
            }

            UnaryOp { expr, .. } => {
                self.resolve_scoped_symbols(module_name, expr, false);
            }

            FnCall { callee, args } => {
                self.resolve_scoped_symbols(module_name, callee, false);
                for arg in args {
                    self.resolve_scoped_symbols(module_name, &mut arg.1, false);
                }
            }

            Match { expr, arms } => {
                self.resolve_scoped_symbols(module_name, expr, false);
                for arm in arms {
                    self.push_scope();
                    let mut mirrored_symbols = None;

                    for pattern in &mut arm.0 {
                        if let Ok(symbols) = self.declare_binding(pattern, true, &mirrored_symbols)
                        {
                            if mirrored_symbols.is_none() {
                                mirrored_symbols = Some(symbols);
                            } else {
                                //assert all symbols match previous symbols - all conditions must declare the same symbols
                                if mirrored_symbols.as_ref().unwrap().len() != symbols.len() {
                                    self.errors.push(Diagnostic {
                                        primary: DiagnosticMsg {
                                            message:
                                                "Mismatched symbol count in match arm patterns"
                                                    .to_string(),
                                            file: self.current_file.clone(),
                                            span: pattern.span,
                                            err_type: DiagnosticMsgType::MismatchedPattern,
                                        },
                                        notes: vec![],
                                        hints: vec![],
                                    });
                                    continue;
                                }

                                for (name, id) in mirrored_symbols.as_ref().unwrap() {
                                    if let Some(other_id) = symbols.get(name) {
                                        if id != other_id {
                                            self.errors.push(Diagnostic {
                                                primary: DiagnosticMsg {
                                                    message: format!("Mismatched symbol '{}' in match arm patterns", name),
                                                    file: self.current_file.clone(),
                                                    span: pattern.span,
                                                    err_type: DiagnosticMsgType::MismatchedPattern,
                                                },
                                                notes: vec![],
                                                hints: vec![],
                                            });
                                        }
                                    } else {
                                        self.errors.push(Diagnostic {
                                            primary: DiagnosticMsg {
                                                message: format!(
                                                    "Mismatched symbol '{}' in match arm patterns",
                                                    name
                                                ),
                                                file: self.current_file.clone(),
                                                span: pattern.span,
                                                err_type: DiagnosticMsgType::MismatchedPattern,
                                            },
                                            notes: vec![],
                                            hints: vec![],
                                        });
                                    }
                                }
                            }
                        }
                    }

                    self.resolve_scoped_symbols(module_name, &mut arm.1, false);

                    self.pop_scope();
                }
            }

            Closure {
                args,
                return_type,
                expr,
            } => {
                self.symbols.symbol_idx += 1;
                node.symbol_id = Some(self.symbols.symbol_idx);
                self.push_scope();
                for arg in args {
                    if let Ok(_) = self.declare_binding(&mut arg.0, false, &None) {
                        if let Some(ty) = &mut arg.1 {
                            self.resolve_scoped_symbols(module_name, ty, false);
                        }
                    }
                }
                if let Some(ret_ty) = return_type {
                    self.resolve_scoped_symbols(module_name, ret_ty, false);
                }
                self.resolve_scoped_symbols(module_name, expr, false);
                self.pop_scope();
            }

            For {
                binding,
                start,
                end,
                body,
            } => {
                self.push_scope();
                let _ = self.declare_binding(binding, false, &None);

                self.resolve_scoped_symbols(module_name, start, false);
                self.resolve_scoped_symbols(module_name, end, false);
                self.resolve_scoped_symbols(module_name, body, false);

                self.pop_scope();
            }

            IndexAccess { expr, index } => {
                self.resolve_scoped_symbols(module_name, expr, false);
                self.resolve_scoped_symbols(module_name, index, false);
            }

            IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_scoped_symbols(module_name, condition, false);
                self.resolve_scoped_symbols(module_name, then_branch, false);
                if let Some(else_branch) = else_branch {
                    self.resolve_scoped_symbols(module_name, else_branch, false);
                }
            }

            Block(statements) => {
                self.push_scope();
                for stmt in statements {
                    self.resolve_scoped_symbols(module_name, stmt, false);
                }
                self.pop_scope();
            }
        }

        return;
    }

    fn resolve_top_level_module_symbols(
        &mut self,
        _package: &str,
        module_name: &str,
        node: &mut Node,
    ) {
        use NodeKind::*;
        match &mut node.kind {
            Module { children, .. } => {
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
                let symbol_id = self.declare_symbol(node.span, symbol.clone(), false, true, None);
                node.symbol_id = Some(symbol_id);

                //register the import
                self.symbols
                    .imports
                    .entry(module_name.to_string())
                    .or_insert_with(HashMap::default)
                    .insert(module.clone(), (symbol_id, HashSet::default()));

                //register alias for symbol resolution
                self.module_aliases
                    .entry(module_name.to_string())
                    .or_insert_with(HashMap::default)
                    .insert(symbol.clone(), module.clone());

                //register directly imported types
                for top_level_type in top_level_types {
                    let _ = self.declare_symbol(
                        node.span,
                        top_level_type.clone(),
                        true,
                        true,
                        Some(symbol_id),
                    );
                    let imported_types = self
                        .symbols
                        .type_imports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::default)
                        .entry(module.to_string())
                        .or_insert_with(|| (symbol_id, HashSet::default()));

                    imported_types.1.insert(top_level_type.clone());
                }

                //register directly imported symbols
                for top_level_symbol in top_level {
                    let _ = self.declare_symbol(
                        node.span,
                        top_level_symbol.clone(),
                        false,
                        true,
                        Some(symbol_id),
                    );
                    let imported_symbols = self
                        .symbols
                        .imports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::default)
                        .entry(module.to_string())
                        .or_insert_with(|| (symbol_id, HashSet::default()));

                    imported_symbols.1.insert(top_level_symbol.clone());
                }
            }

            FnDecl { symbol, public, .. } => {
                let symbol_id = self.declare_symbol(node.span, symbol.clone(), false, true, None);
                node.symbol_id = Some(symbol_id);
                if *public {
                    self.symbols
                        .exports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::default)
                        .insert(symbol.clone(), symbol_id);
                }
            }

            ConstDecl { symbol, public, .. } => {
                if let Ok(symbol_ids) = self.declare_binding(symbol, false, &None) {
                    if *public {
                        for (symbol_name, symbol_id) in &symbol_ids {
                            self.symbols
                                .exports
                                .entry(module_name.to_string())
                                .or_insert_with(HashMap::default)
                                .insert(symbol_name.clone(), *symbol_id);
                        }
                    }
                }
            }

            TypeAlias { symbol, public, .. } => {
                let symbol_id = self.declare_symbol(node.span, symbol.clone(), true, true, None);
                node.symbol_id = Some(symbol_id);
                if *public {
                    self.symbols
                        .type_exports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::default)
                        .insert(symbol.clone(), symbol_id);
                }
            }

            TypeDecl {
                symbol,
                public,
                variants,
                type_vars,
                ..
            } => {
                //declare type
                let symbol_id = self.declare_symbol(node.span, symbol.clone(), true, true, None);
                node.symbol_id = Some(symbol_id);
                if *public {
                    self.symbols
                        .type_exports
                        .entry(module_name.to_string())
                        .or_insert_with(HashMap::default)
                        .insert(symbol.clone(), symbol_id);
                }

                //rewriting type vars in type decl to be globally unique, avoiding the scoping issue
                // a -> a$TypeName
                //should be fine - if there were an issue, there must be a user-land double decl of the whole type

                let scoped_type_var = |a: &(String, Option<SymbolId>, CodeSpan)| {
                    (a.0.clone(), a.0.clone() + "$" + symbol.as_str())
                };
                let symbol_map = type_vars.iter().map(scoped_type_var).collect();

                for variant in variants.iter_mut() {
                    self.rewrite_symbols(&symbol_map, variant);
                }

                for ty_var in type_vars.iter_mut() {
                    ty_var.0 = scoped_type_var(&ty_var).1;
                    ty_var.1 =
                        Some(self.declare_symbol(ty_var.2, ty_var.0.clone(), true, false, None));
                }

                if !variants.is_empty() {
                    //declare the type constructors
                    for variant in variants {
                        match &mut variant.kind {
                            NodeKind::TypeConstructor { symbol, .. } => {
                                //type constructors are not types, treated as normal funcs
                                let variant_id = self.declare_symbol(
                                    variant.span,
                                    symbol.clone(),
                                    false,
                                    true,
                                    None,
                                );
                                variant.symbol_id = Some(variant_id);
                                //register type constructor
                                self.symbols.type_constructors.insert(variant_id, symbol_id);
                                //register export
                                if *public {
                                    self.symbols
                                        .exports
                                        .entry(module_name.to_string())
                                        .or_insert_with(HashMap::default)
                                        .insert(symbol.clone(), variant_id);
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

    fn rewrite_symbols(&mut self, map: &HashMap<String, String>, node: &mut Node) {
        match &mut node.kind {
            NodeKind::TypeDecl {
                symbol,
                type_vars,
                variants,
                ..
            } => {
                if let Some(new_name) = map.get(symbol) {
                    *symbol = new_name.clone();
                }

                for ty_var in type_vars.iter_mut() {
                    if let Some(new_name) = map.get(&ty_var.0) {
                        ty_var.0 = new_name.clone();
                    }
                }

                for variant in variants {
                    self.rewrite_symbols(map, variant);
                }
            }

            NodeKind::TypeConstructor { symbol, fields } => {
                if let Some(new_name) = map.get(symbol) {
                    *symbol = new_name.clone();
                }

                for (_, ty, _) in fields {
                    self.rewrite_symbols(map, ty);
                }
            }

            NodeKind::Type {
                parent_module,
                symbol,
                type_vars,
                ..
            } => {
                if let Some(new_name) = map.get(symbol) {
                    *symbol = new_name.clone();
                }

                for ty in type_vars {
                    self.rewrite_symbols(map, ty);
                }

                //if parent module is being renamed, it must be an import alias
                if let Some(parent_module) = parent_module {
                    if let Some(new_name) = map.get(parent_module) {
                        *parent_module = new_name.clone();
                    }
                }
            }

            NodeKind::Expr {
                expr: ExprKind::Symbol { name },
            } => {
                if let Some(new_name) = map.get(name) {
                    *name = new_name.clone();
                }
            }

            //not implementing all for the moment, only needed
            _ => {}
        }
    }
}
