use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, UnaryOp, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::declare_intrinsics,
    module::{ModuleId, ModuleTable},
    parse::CodeSpan,
    visit_ast_children,
};
use edit_distance::edit_distance;

pub type SymbolId = usize;
pub const INVALID_SYMBOL_ID: SymbolId = usize::MAX;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<SymbolId, Symbol>,
    //module_id -> symbols in module
    pub module_symbols: HashMap<ModuleId, Vec<SymbolId>>,
    // module_id -> (symbol_name -> linked symbol_ids)
    pub symbol_links: HashMap<ModuleId, HashMap<String, Vec<SymbolId>>>,
    //import symbol id -> local ident name
    pub import_symbols: HashMap<SymbolId, String>,
    //symbol name -> (symbol id, symbol module id, imported module id)
    pub exported_imports: HashMap<String, (SymbolId, ModuleId, ModuleId)>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub module_id: ModuleId,
    pub scope_id: SymbolId,
    pub name: String,
    pub span: Option<CodeSpan>,
    pub mutable: bool,
    pub is_ref: bool,
    pub is_type: bool,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            symbol_links: HashMap::new(),
            module_symbols: HashMap::new(),
            import_symbols: HashMap::new(),
            exported_imports: HashMap::new(),
        }
    }

    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&id)
    }

    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(&id)
    }

    pub fn remove(&mut self, id: SymbolId) -> Option<Symbol> {
        self.symbols.remove(&id)
    }

    pub fn merge(&mut self, other: SymbolTable) {
        self.symbols.extend(other.symbols);
        self.symbol_links.extend(other.symbol_links);
        self.import_symbols.extend(other.import_symbols);
        self.exported_imports.extend(other.exported_imports);
        for (module_id, symbols) in other.module_symbols {
            self.module_symbols
                .entry(module_id)
                .or_default()
                .extend(symbols);
        }
    }
}

impl Symbol {
    pub fn new_var(
        id: SymbolId,
        module_id: ModuleId,
        scope_id: SymbolId,
        name: String,
        span: Option<CodeSpan>,
        is_ref: bool,
        mutable: bool,
    ) -> Self {
        Self {
            id,
            module_id,
            scope_id,
            name,
            span,
            is_ref,
            mutable,
            is_type: false,
        }
    }

    pub fn new_type(
        id: SymbolId,
        module_id: ModuleId,
        scope_id: SymbolId,
        name: Option<String>,
        span: Option<CodeSpan>,
    ) -> Self {
        Self {
            id,
            module_id,
            scope_id,
            name: if let Some(name) = name {
                name
            } else {
                format!("$anon_type_{}", id)
            },
            span,
            is_ref: false,
            mutable: false,
            is_type: true,
        }
    }
}

pub fn name_resolution_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut name_resolver = NameResolverPassContext::new(&ctx.modules, ctx.symbol_id_counter);

    for (file, ast) in ctx.asts.iter_mut() {
        name_resolver.current_module_id = ctx.modules.module_file_ids[file];
        name_resolver.current_file = file.clone();

        let module_scope_id = name_resolver
            .module_top_level_scope_ids
            .entry(name_resolver.current_module_id)
            .or_insert_with(|| {
                let id = name_resolver.scope_id_counter;
                name_resolver.scope_id_counter += 1;
                id
            });

        name_resolver.scope_id_counter += 1;
        name_resolver.scopes.push(Scope {
            id: module_scope_id.clone(),
            entries: HashMap::new(),
            import_label_entries: HashMap::new(),
        });

        name_resolver.top_level_decl_visitor(ast)?;
        if let Variant::Program { children, .. } = &mut ast.variant {
            for child in children.iter_mut() {
                name_resolver.is_top_level = true;
                name_resolver.name_resolution_visitor(child)?;
            }
        } else {
            unreachable!("Expected Program variant for top-level AST");
        }

        name_resolver.scopes.pop();
    }

    ctx.errors.extend(name_resolver.errors);
    ctx.warnings.extend(name_resolver.warnings);

    ctx.symbol_table.merge(name_resolver.table);
    ctx.symbol_id_counter = name_resolver.id_counter;
    Ok(())
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: SymbolId,
    pub entries: HashMap<String, SymbolId>,
    pub import_label_entries: HashMap<String, SymbolId>,
}

pub struct NameResolverPassContext<'a> {
    pub id_counter: SymbolId,
    pub scope_id_counter: SymbolId,
    pub current_file: String,
    pub current_module_id: ModuleId,
    pub current_is_ref: bool,
    pub is_top_level: bool,
    pub scopes: Vec<Scope>,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub table: SymbolTable,
    pub module_top_level_scope_ids: HashMap<ModuleId, SymbolId>,
    pub module_table: &'a ModuleTable,
    //map symbol id of import label symbols to the module they import from
    pub import_label_ids: HashMap<SymbolId, ModuleId>,
    pub import_symbol_ids: Vec<SymbolId>,
    pub type_context_override: bool,
}

impl<'a> NameResolverPassContext<'a> {
    pub fn new(module_table: &'a ModuleTable, id_offset: SymbolId) -> Self {
        let mut ret = Self {
            scopes: vec![Scope {
                id: 0,
                entries: HashMap::new(),
                import_label_entries: HashMap::new(),
            }],
            errors: Vec::new(),
            warnings: Vec::new(),
            current_file: String::new(),
            id_counter: id_offset,
            scope_id_counter: 1,
            current_module_id: ModuleId::default(),
            current_is_ref: false,
            table: SymbolTable::new(),
            module_top_level_scope_ids: HashMap::new(),
            import_label_ids: HashMap::new(),
            import_symbol_ids: Vec::new(),
            is_top_level: true,
            type_context_override: false,
            module_table,
        };
        declare_intrinsics(&mut ret);
        ret
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope {
            id: self.scope_id_counter,
            entries: HashMap::new(),
            import_label_entries: HashMap::new(),
        });
        self.scope_id_counter += 1;
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn new_id(&mut self) -> SymbolId {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    pub fn declare(
        &mut self,
        module_id: ModuleId,
        name: String,
        span: Option<CodeSpan>,
        mutable: bool,
        is_ref: bool,
    ) -> SymbolId {
        let id = self.new_id();

        self.table
            .module_symbols
            .entry(module_id)
            .or_default()
            .push(id);

        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.entries.insert(name.clone(), id);

        self.table.symbols.insert(
            id,
            Symbol::new_var(id, module_id, current_scope.id, name, span, is_ref, mutable),
        );

        id
    }

    pub fn declare_type(
        &mut self,
        module_id: ModuleId,
        name: Option<String>,
        span: Option<CodeSpan>,
    ) -> SymbolId {
        let id = self.new_id();

        self.table
            .module_symbols
            .entry(module_id)
            .or_default()
            .push(id);

        self.table.symbols.insert(
            id,
            Symbol::new_type(
                id,
                module_id,
                self.scopes.last().unwrap().id,
                name.clone(),
                span,
            ),
        );

        if let Some(name) = name {
            let current_scope = self.scopes.last_mut().unwrap();
            current_scope.entries.insert(name.clone(), id);
        }

        id
    }

    fn link_symbol_to_module(&mut self, symbol_id: SymbolId, module_id: ModuleId, name: String) {
        let mut module_links = self.table.symbol_links.get_mut(&module_id);
        if module_links.is_none() {
            self.table.symbol_links.insert(module_id, HashMap::new());
            module_links = self.table.symbol_links.get_mut(&module_id);
        }
        module_links
            .unwrap()
            .entry(name)
            .or_default()
            .push(symbol_id);
    }

    fn lookup_import_label(&self, module_name: &str) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.import_label_entries.get(module_name) {
                return Some(id);
            }
        }

        None
    }

    fn lookup(&mut self, name: &str) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.entries.get(name) {
                return Some(id);
            }
        }

        //check namespaced symbols, which reference imports
        //every symbol referencing a namespaced import

        let new_symbol_id = self.new_id();

        if let Some((module_name, symbol_name)) = name.split_once("::") {
            let module_symbol_id = self.lookup_import_label(module_name);
            if module_symbol_id.is_none() {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Unknown namespace '{}'", module_name),
                        span: CodeSpan::new(0, 0), //todo: use actual span
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndeclaredSymbol,
                    },
                    notes: vec![],
                    hints: vec!["Did you forget to import a module?".to_string()],
                });
                return None;
            }

            //not going to actually declare the symbol, just get a unique id, link it, and return it
            let module_id = self.import_label_ids.get(&module_symbol_id.unwrap());
            if module_id.is_none() {
                return None;
            }

            self.import_symbol_ids.push(new_symbol_id);
            self.table
                .import_symbols
                .insert(new_symbol_id, symbol_name.to_string());
            self.link_symbol_to_module(new_symbol_id, *module_id.unwrap(), symbol_name.to_string());

            return Some(new_symbol_id);
        }

        None
    }

    fn lookup_current_scope(&self, name: &str) -> Option<SymbolId> {
        if let Some(scope) = self.scopes.last() {
            if let Some(&id) = scope.entries.get(name) {
                return Some(id);
            }
        }
        None
    }

    fn fuzzy_lookup(&self, name: &str, is_type: Option<bool>) -> Vec<SymbolId> {
        let mut results = Vec::new();
        for scope in self.scopes.iter().rev() {
            for (key, val) in &scope.entries {
                if is_type.is_none()
                    || (self.is_type(val) == is_type.unwrap()) && edit_distance(name, key) <= 2
                {
                    results.push(val.clone());
                }
            }
        }

        results
    }

    fn is_type(&self, symbol_id: &SymbolId) -> bool {
        if let Some(symbol) = self.table.symbols.get(symbol_id) {
            symbol.is_type
        } else {
            false
        }
    }

    fn check_reference(
        &mut self,
        name: &String,
        span: CodeSpan,
        is_type: Option<bool>,
    ) -> Result<SymbolId, ()> {
        let symbol_id = self.lookup(name);

        if let Some(symbol_id) = symbol_id {
            if let Some(symbol) = self.table.get(symbol_id)
                && symbol.is_type != is_type.unwrap_or(false)
            {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "'{}' is a {} but was used as a {}",
                            name,
                            if symbol.is_type { "type" } else { "variable" },
                            if is_type.unwrap_or(false) {
                                "type"
                            } else {
                                "variable"
                            }
                        ),
                        span: span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::IllegalName,
                    },
                    notes: vec![],
                    hints: vec![],
                });
                return Err(());
            }

            return Ok(symbol_id);
        } else {
            let notes = self
                .fuzzy_lookup(name, is_type)
                .iter()
                .map(|id| {
                    let symbol = self.table.symbols.get(id);
                    if symbol.is_none() {
                        return None;
                    }
                    let symbol = symbol.unwrap();

                    Some(DiagnosticMsg {
                        message: format!(
                            "Similar symbol '{}' exists in the current scope",
                            symbol.name
                        ),
                        span: symbol.span.unwrap_or(CodeSpan::new(0, 0)),
                        //todo: this could throw, should handle better
                        file: self
                            .module_table
                            .get_module_borrow(symbol.module_id)
                            .unwrap()
                            .file
                            .clone(),
                        err_type: DiagnosticMsgType::UndeclaredSymbol,
                    })
                })
                .flatten()
                .take(3)
                .map(|msg| msg.clone())
                .collect();

            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!(
                        "{}'{}' has not been declared",
                        match is_type {
                            Some(is_type) =>
                                if is_type {
                                    "Type "
                                } else {
                                    "Symbol "
                                },
                            None => "",
                        },
                        name
                    ),
                    span: span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                },
                notes,
                hints: vec![],
            });

            return Err(());
        }
    }

    fn name_resolution_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        if !self.is_top_level {
            match ast.variant {
                Variant::Expr(ExprNode::Ident {
                    ref mut name,
                    ref mut is_type,
                    ref mut id,
                    ..
                }) => {
                    if let Ok(symbol_id) = self.check_reference(
                        name,
                        ast.span,
                        Some(*is_type || self.type_context_override),
                    ) {
                        *id = Some(symbol_id);
                    }
                }

                _ => {}
            };
            self.register_decl(ast);
        }
        self.is_top_level = false;

        let push_scope = Self::variant_pushes_scope(ast);
        if push_scope {
            self.push_scope();
        }

        visit_ast_children!(ast.variant, self, name_resolution_visitor, {
            Variant::FuncDecl {
                ref mut ident,
                ref mut capture_list,
                ref mut params,
                ref mut return_type,
                ref mut body,
                ..
            } => {
                self.visit_func_decl_children(ident, capture_list, params, return_type, body)?;
            },

            Variant::InterfaceDecl{
                ref mut ident,
                ref mut interface
            } => {
                let type_context_override = self.type_context_override;
                self.type_context_override = true;

                self.declare_type_args(ident)?;
                self.name_resolution_visitor(ident)?;

                for method in interface.iter_mut() {
                    self.name_resolution_visitor(method)?;
                }

                self.type_context_override = type_context_override;
            },

            Variant::StructDecl {
                ref mut ident,
                ref mut fields,
                ..
            } => {
                let type_context_override = self.type_context_override;
                self.type_context_override = true;

                self.declare_type_args(ident)?;
                self.name_resolution_visitor(ident)?;

                for (_, field) in fields.iter_mut() {
                    self.name_resolution_visitor(field)?;
                }

                self.type_context_override = type_context_override;
            },


            Variant::TypeDecl {
                ref mut ident,
                ref mut alias_of
            } => {
                let type_context_override = self.type_context_override;
                self.type_context_override = true;

                self.declare_type_args(ident)?;
                self.name_resolution_visitor(ident)?;

                self.name_resolution_visitor(ident)?;
                self.name_resolution_visitor(alias_of)?;

                self.type_context_override = type_context_override;

            },


            Variant::EnumDecl {
                ref mut ident,
                ref mut variants,
                ..
            } => {
                let type_context_override = self.type_context_override;
                self.type_context_override = true;

                self.declare_type_args(ident)?;
                self.name_resolution_visitor(ident)?;

                for (_, variant) in variants.iter_mut() {
                    if let Some(inner) = variant {
                        self.name_resolution_visitor(inner)?;
                    }
                }

                self.type_context_override = type_context_override;
            },


            Variant::Expr(ExprNode::Match {
                ref mut subject,
                ref mut cases,
                ..
            }) => {
                self.name_resolution_visitor(subject)?;

                for (cond, arm) in cases.iter_mut() {
                    self.push_scope();
                    self.declare_pattern(cond)?;
                    self.name_resolution_visitor(arm)?;
                    self.pop_scope();
                }
            },
        });

        if push_scope {
            self.pop_scope();
        }

        Ok(())
    }

    fn declare_pattern(&mut self, pattern: &mut Node) -> Result<(), ()> {
        match &mut pattern.variant {
            Variant::Expr(ExprNode::UnaryOp { operand, op }) => {
                let current_is_ref = self.current_is_ref;
                if matches!(op, UnaryOp::Ref) {
                    self.current_is_ref = true;
                }

                self.declare_pattern(operand)?;
                self.current_is_ref = current_is_ref;
            }

            Variant::Expr(ExprNode::Call { args, .. }) => {
                //parsed as a func call, but is actually an enum pattern
                //ignore callee, declare inner args as pattern variables
                //enum variant will be checked during type checking
                for arg in args.iter_mut() {
                    self.declare_pattern(arg)?;
                }
            }
            Variant::Expr(ExprNode::ListLit { elements }) => {
                //list pattern, declare inner elements as pattern variables
                for element in elements.iter_mut() {
                    self.declare_pattern(element)?;
                }
            }
            Variant::Expr(ExprNode::TupleLit { elements }) => {
                //tuple pattern, declare inner elements as pattern variables
                if elements.is_empty() {
                    //empty tuple pattern, nothing to declare
                    return Ok(());
                }

                for element in elements.iter_mut() {
                    self.declare_pattern(element)?;
                }
            }

            Variant::Expr(ExprNode::Ident {
                name,
                type_args,
                is_type,
                ..
            }) => {
                if name != "_"
                    && let None = self.lookup(name)
                {
                    //this ident cannot have a namespace if is pattern decl
                    if Self::is_qualified(name) || !type_args.is_empty() || *is_type {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Pattern variable '{}' must not have namespaces, type parameters, or be a type",
                                    name
                                ),
                                span: pattern.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::IllegalName,
                            },
                            notes: vec![],
                            hints: vec![],
                        });
                        return Err(());
                    }

                    //is a pattern decl, not a matching ident, so declare for inner scope
                    self.declare(
                        self.current_module_id,
                        name.clone(),
                        Some(pattern.span.clone()),
                        false,
                        self.current_is_ref,
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn is_qualified(name: &str) -> bool {
        name.contains("::")
    }

    fn declare_type_args(&mut self, ident: &Box<Node>) -> Result<(), ()> {
        if let Some(type_args) = &ident.get_type_args() {
            for arg in type_args.iter() {
                if let Some(arg_ident) = arg.get_ident() {
                    let symbol_id = self.lookup_current_scope(arg_ident.as_str());
                    if symbol_id.is_none() {
                        self.declare_type(
                            self.current_module_id,
                            Some(arg_ident.clone()),
                            Some(ident.span.clone()),
                        );
                    } else {
                        let symbol = &self.table.symbols[&symbol_id.unwrap()];
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Type parameter '{}' has already been declared as a {}",
                                    arg_ident,
                                    if self.is_type(&symbol_id.unwrap()) {
                                        "type"
                                    } else {
                                        "runtime symbol"
                                    }
                                ),
                                span: arg.span,
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::MultipleDeclarations,
                            },
                            notes: vec![DiagnosticMsg {
                                message: format!("'{}' was previously declared here", symbol.name),
                                span: symbol.span.unwrap_or(CodeSpan::new(0, 0)),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::MultipleDeclarations,
                            }],
                            hints: vec![],
                        });
                    }
                } else {
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Type parameter must have a valid identifier".to_string(),
                            span: arg.span,
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::IllegalName,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                }
            }
        }
        Ok(())
    }

    fn visit_func_decl_children(
        &mut self,
        ident: &mut Option<Box<Node>>,
        capture_list: &mut Option<Vec<Node>>,
        params: &mut Vec<Node>,
        return_type: &mut Option<Box<Node>>,
        body: &mut Option<Box<Node>>,
    ) -> Result<(), ()> {
        if let Some(ident) = &ident {
            self.declare_type_args(ident)?;
        }

        if let Some(ident) = ident {
            let ctx_override = self.type_context_override;
            self.type_context_override = true;

            if let Variant::Expr(ExprNode::Ident { type_args, .. }) = &mut ident.variant {
                for arg in type_args.iter_mut() {
                    self.name_resolution_visitor(arg)?;
                }
            }

            self.type_context_override = ctx_override;
        }

        //default child visitor

        if let Some(capture_list) = capture_list {
            for capture in capture_list.iter_mut() {
                self.name_resolution_visitor(capture)?;
            }
        }

        for param in params.iter_mut() {
            self.name_resolution_visitor(param)?;
        }

        if let Some(return_type) = return_type {
            self.name_resolution_visitor(return_type)?;
        }

        if let Some(body) = body {
            self.name_resolution_visitor(body)?;
        }

        Ok(())
    }

    fn variant_pushes_scope(ast: &Node) -> bool {
        match &ast.variant {
            Variant::ActorDecl { .. }
            | Variant::InterfaceDecl { .. }
            | Variant::EnumDecl { .. }
            | Variant::StructDecl { .. }
            | Variant::FuncDecl { .. }
            | Variant::ImplDecl { .. }
            | Variant::TypeDecl { .. }
           // | Variant::VarDecl {..} //pushes scope so that labeled imports are scoped to the label
            | Variant::Block { .. }
            | Variant::If { .. }
            | Variant::For { .. }
            | Variant::Expr(ExprNode::Match { .. }) => true,
            _ => false,
        }
    }

    fn register_decl(&mut self, ast: &mut Node) {
        //check for import directive, override variable declaration
        if let Variant::VarDecl {
            name,
            initialiser,
            var_type,
            id,
            ..
        } = &mut ast.variant
        {
            if var_type.is_some() {
                if self
                    .check_reference(name, ast.span.clone(), Some(true))
                    .is_err()
                {
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Type '{}' has not been declared", name),
                            span: ast.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UndeclaredSymbol,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                }
            }

            if initialiser.is_some()
                && initialiser.as_ref().unwrap().get_directive() == Some("@import".to_string())
            {
                //this is an import declaration, just looks like a variable declaration
                let module_name = match &initialiser.as_ref().unwrap().variant {
                    Variant::Expr(ExprNode::Call { args, .. }) => {
                        //already validated in module resolution pass
                        args[0].get_string().unwrap().clone()
                    }
                    _ => unreachable!("Expected import call expression"),
                };

                let module_id = self
                    .module_table
                    .get_module_by_name_borrow(module_name.as_str())
                    .unwrap()
                    .id;

                //add an import label symbol
                let new_symbol_id = self.new_id();
                //self.pop_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .import_label_entries
                    .insert(module_name.clone(), new_symbol_id);

                if ast.export {
                    self.table
                        .exported_imports
                        //todo: check if self.current_module_id or module_id should be used
                        .insert(
                            name.clone(),
                            (new_symbol_id, self.current_module_id, module_id),
                        );
                }
                self.import_label_ids.insert(new_symbol_id, module_id);

                //delete the node
                ast.variant = Variant::Dead;

                return;
            }
        }

        let mut mutable = false;
        let mut is_type = true;
        let mut id = None;

        let (ident, name) = match &mut ast.variant {
            Variant::ActorDecl { ident, .. }
            | Variant::InterfaceDecl { ident, .. }
            | Variant::EnumDecl { ident, .. }
            | Variant::StructDecl { ident, .. } => {
                let name = ident.get_ident();
                (Some(ident), name)
            }
            Variant::TypeDecl { ident, .. } => {
                is_type = true;
                let name = ident.get_ident();
                (Some(ident), name)
            }
            Variant::FuncDecl { ident, .. } => {
                is_type = false;
                match ident {
                    Some(ident) => {
                        let name = ident.get_ident();
                        (Some(ident), name)
                    }
                    None => (None, None),
                }
            }
            Variant::VarDecl {
                name, modifiable, ..
            } => {
                is_type = false;
                mutable = *modifiable;
                (None, Some(name.to_string()))
            }
            Variant::Expr(ExprNode::Call { callee, args }) => {
                if Some("@import".to_string()) == callee.get_directive() {
                    //args.len already asserted == 1
                    if let Some(mod_name) = args.first().unwrap().get_string() {
                        //module must exist at this point
                        let module = self
                            .module_table
                            .get_module_by_name_borrow(mod_name.as_str())
                            .unwrap()
                            .clone();

                        for (_, export) in module.exports {
                            let local_id = match export.is_type {
                                false => self.declare(
                                    self.current_module_id,
                                    export.name.clone(),
                                    export.span,
                                    export.mutable,
                                    export.is_ref,
                                ),
                                true => self.declare_type(
                                    self.current_module_id,
                                    Some(export.name.clone()),
                                    export.span,
                                ),
                            };

                            self.link_symbol_to_module(local_id, module.id, export.name);
                        }
                    }
                }

                (None, None)
            }
            _ => (None, None),
        };

        if let Some(name) = &name {
            if let Some(existing_id) = self.lookup_current_scope(name) {
                let existing_is_type = self.is_type(&existing_id);
                let symbol = self.table.symbols.get(&existing_id).unwrap();
                if existing_is_type != is_type {
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!(
                                "'{}' has already been declared as a {}",
                                name,
                                if existing_is_type {
                                    "type"
                                } else {
                                    "runtime symbol"
                                }
                            ),
                            span: ast.span,
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::MultipleDeclarations,
                        },
                        notes: vec![DiagnosticMsg {
                            message: format!(
                                "'{}' is a {}, but is being redeclared as a {}",
                                name,
                                if existing_is_type {
                                    "type"
                                } else {
                                    "runtime symbol"
                                },
                                if is_type { "type" } else { "runtime symbol" }
                            ),
                            span: symbol.span.unwrap_or(CodeSpan::new(0, 0)),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::MultipleDeclarations,
                        }],
                        hints: vec![],
                    });
                    return;
                }

                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("'{}' has already been declared", name),
                        span: ast.span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::MultipleDeclarations,
                    },
                    notes: vec![DiagnosticMsg {
                        message: format!("'{}' was previously declared here", name),
                        span: symbol.span.unwrap_or(CodeSpan::new(0, 0)),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::MultipleDeclarations,
                    }],
                    hints: vec![],
                });
            } else {
                if is_type {
                    id = Some(self.declare_type(
                        self.current_module_id,
                        Some(name.clone()),
                        Some(ast.span.clone()),
                    ));
                } else {
                    id = Some(self.declare(
                        self.current_module_id,
                        name.clone(),
                        Some(ast.span),
                        mutable,
                        false,
                    ));
                }
            }
        }
        if let Some(ident) = ident {
            match &mut ident.variant {
                Variant::VarDecl {
                    id: expr_id, name, ..
                }
                | Variant::Expr(ExprNode::Ident {
                    id: expr_id, name, ..
                }) => {
                    println!("Registering {} with id {:?}", name, id);
                    if let Some(symbol_id) = id {
                        *expr_id = Some(symbol_id);
                    } else {
                        *expr_id = None;
                    }
                }
                _ => {}
            }
        }
    }

    fn top_level_decl_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        if let Variant::Program { children, .. } = &mut ast.variant {
            for child in children.iter_mut() {
                match child.variant {
                    Variant::ActorDecl { .. }
                    | Variant::InterfaceDecl { .. }
                    | Variant::EnumDecl { .. }
                    | Variant::StructDecl { .. }
                    | Variant::FuncDecl { .. }
                    | Variant::ImplDecl { .. }
                    | Variant::VarDecl { .. }
                    | Variant::TypeDecl { .. } => {
                        self.register_decl(child);
                    }
                    _ => {
                        if child.get_directive().is_none() {
                            self.errors.push(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!(
                                        "Top-level declarations must be actors, interfaces, enums, structs, functions, impls, variables, or types, found {}",
                                        child.variant.user_friendly_name()
                                    ),
                                    span: child.span.clone(),
                                    file: self.current_file.clone(),
                                    err_type: DiagnosticMsgType::InvalidTopLevelDeclaration,
                                },
                                notes: vec![],
                                hints: vec![],
                            });
                        } else {
                            self.register_decl(child);
                        }
                    }
                }
            }
        } else {
            self.register_decl(ast);
        }
        Ok(())
    }
}
