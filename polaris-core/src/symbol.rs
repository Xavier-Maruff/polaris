use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, UnaryOp, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::declare_intrinsics,
    module::{INVALID_MODULE_ID, Module, ModuleId, ModuleTable},
    parse::CodeSpan,
    visit_ast_children,
};
use edit_distance::edit_distance;

pub type SymbolId = usize;
pub const INVALID_SYMBOL_ID: SymbolId = usize::MAX;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub module_id: ModuleId,
    pub scope_id: SymbolId,
    pub name: String,
    pub span: Option<CodeSpan>,
    pub variant: SymbolVariant,
}

#[derive(Debug, Clone)]
pub enum SymbolVariant {
    Var(VarSymbol),
    Type(TypeSymbol),
}

#[derive(Debug, Clone)]
pub struct VarSymbol {
    pub type_id: Option<SymbolId>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub generics: Vec<(String, Option<Vec<SymbolId>>)>, //type variables and required interfaces
    pub variant: TypeVariant,
}

impl Symbol {
    pub fn new_var(
        id: SymbolId,
        module_id: ModuleId,
        scope_id: SymbolId,
        name: String,
        span: Option<CodeSpan>,
        type_id: Option<SymbolId>,
        mutable: bool,
    ) -> Self {
        Self {
            id,
            module_id,
            scope_id,
            name,
            span,
            variant: SymbolVariant::Var(VarSymbol { type_id, mutable }),
        }
    }

    pub fn new_type(
        id: SymbolId,
        module_id: ModuleId,
        scope_id: SymbolId,
        name: Option<String>,
        generics: Vec<(String, Option<Vec<SymbolId>>)>,
        variant: TypeVariant,
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
            variant: SymbolVariant::Type(TypeSymbol::new(generics, variant)),
        }
    }
}

impl TypeSymbol {
    pub fn new(generics: Vec<(String, Option<Vec<SymbolId>>)>, variant: TypeVariant) -> Self {
        Self { generics, variant }
    }
}

#[derive(Debug, Clone)]
pub enum TypeVariant {
    Primitive(PrimitiveType),
    Directive {
        args: Vec<TypeVariant>,
    },
    Struct {
        fields: HashMap<String, TypeVariant>,
        generics: Vec<String>,
    },
    Enum {
        variants: HashMap<String, TypeVariant>,
        generics: Vec<String>,
    },
    Interface {
        methods: HashMap<String, TypeVariant>,
        generics: Vec<String>,
    },
    Actor {
        methods: HashMap<String, TypeVariant>,
        fields: HashMap<String, TypeVariant>,
        generics: Vec<String>,
    },
    Vector {
        element_type: Box<TypeVariant>,
        generics: Vec<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Bool,
    String,
    Void,
    Vector,
    Any, //for internal use only
}

pub fn resolve_names_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut name_resolver = NameResolverContext::new(ctx.modules.clone());

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
    Ok(())
}

pub struct Scope {
    pub id: SymbolId,
    pub entries: HashMap<String, SymbolId>,
}

pub struct NameResolverContext {
    pub id_counter: SymbolId,
    pub scope_id_counter: SymbolId,
    pub current_file: String,
    pub current_module_id: ModuleId,
    pub is_top_level: bool,
    pub scopes: Vec<Scope>,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub symbol_table: HashMap<SymbolId, Symbol>,
    pub symbol_links: HashMap<SymbolId, (ModuleId, String)>,
    pub module_top_level_scope_ids: HashMap<ModuleId, SymbolId>,
    pub module_table: ModuleTable,
    pub type_context_override: bool,
}

impl NameResolverContext {
    pub fn new(module_table: ModuleTable) -> Self {
        let mut ret = Self {
            scopes: vec![Scope {
                id: 0,
                entries: HashMap::new(),
            }],
            errors: Vec::new(),
            warnings: Vec::new(),
            current_file: String::new(),
            id_counter: 0,
            scope_id_counter: 1,
            current_module_id: ModuleId::default(),
            symbol_table: HashMap::new(),
            module_top_level_scope_ids: HashMap::new(),
            symbol_links: HashMap::new(),
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
        });
        self.scope_id_counter += 1;
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn is_intrinsic_type(&self, symbol: &Symbol) -> bool {
        match &symbol.variant {
            SymbolVariant::Type(t) => match &t.variant {
                TypeVariant::Primitive(primitive) => match primitive {
                    PrimitiveType::Any => false,
                    _ => true,
                },
                _ => false,
            },
            SymbolVariant::Var(_) => false,
        }
    }

    pub fn declare(
        &mut self,
        module_id: ModuleId,
        name: String,
        span: Option<CodeSpan>,
        type_id: Option<SymbolId>,
        mutable: bool,
    ) -> SymbolId {
        let id = self.id_counter;
        self.id_counter += 1;

        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.entries.insert(name.clone(), id);

        self.symbol_table.insert(
            id,
            Symbol::new_var(
                id,
                module_id,
                current_scope.id,
                name,
                span,
                type_id,
                mutable,
            ),
        );

        id
    }

    pub fn declare_type(
        &mut self,
        module_id: ModuleId,
        name: Option<String>,
        generics: Vec<(String, Option<Vec<SymbolId>>)>,
        variant: TypeVariant,
        span: Option<CodeSpan>,
    ) -> SymbolId {
        let id = self.id_counter;
        self.id_counter += 1;

        self.symbol_table.insert(
            id,
            Symbol::new_type(
                id,
                module_id,
                self.scopes.last().unwrap().id,
                name.clone(),
                generics,
                variant,
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
        self.symbol_links.insert(symbol_id, (module_id, name));
    }

    fn lookup(&self, name: &str) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.entries.get(name) {
                return Some(id);
            }
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

    fn fuzzy_lookup(&self, name: &str, is_type: bool) -> Vec<SymbolId> {
        let mut results = Vec::new();
        for scope in self.scopes.iter().rev() {
            for (key, val) in &scope.entries {
                if self.is_type(val) == is_type && edit_distance(name, key) <= 2 {
                    results.push(val.clone());
                }
            }
        }

        results
    }

    fn is_type(&self, symbol_id: &SymbolId) -> bool {
        if let Some(symbol) = self.symbol_table.get(symbol_id) {
            match &symbol.variant {
                SymbolVariant::Type(_) => true,
                SymbolVariant::Var(_) => false,
            }
        } else {
            false
        }
    }

    fn check_reference(
        &mut self,
        name: &String,
        span: CodeSpan,
        is_type: bool,
    ) -> Result<SymbolId, ()> {
        //todo: deal with namespaces and type_args
        let symbol_id = self.lookup(name);
        if let Some(symbol_id) = symbol_id {
            if self.is_type(&symbol_id) != is_type {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "'{}' is a {} but was used as a {}",
                            name,
                            if self.is_type(&symbol_id) {
                                "type"
                            } else {
                                "runtime symbol"
                            },
                            if is_type { "type" } else { "runtime symbol" }
                        ),
                        span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndeclaredSymbol,
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
                    let symbol = self.symbol_table.get(id);
                    if symbol.is_none() {
                        return None;
                    }
                    let symbol = symbol.unwrap();
                    if self.is_intrinsic_type(symbol) {
                        return Some(DiagnosticMsg {
                            message: format!("Did you mean the intrinsic type '{}'?", symbol.name),
                            span: CodeSpan::new(0, 0),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UndeclaredSymbol,
                        });
                    }
                    Some(DiagnosticMsg {
                        message: format!(
                            "Similar symbol '{}' exists in the current scope",
                            symbol.name
                        ),
                        span: symbol.span.unwrap_or(CodeSpan::new(0, 0)),
                        //todo: this could throw, should handle better
                        file: self
                            .module_table
                            .get_module(symbol.module_id)
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
                        "{} '{}' has not been declared",
                        if is_type { "Type" } else { "Symbol" },
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
                    ref mut type_args,
                    ref mut memory_mode,
                    ref mut id,
                    ref mut is_directive,
                }) => {
                    if let Ok(symbol_id) =
                        self.check_reference(name, ast.span, *is_type || self.type_context_override)
                    {
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
            Variant::Expr(ExprNode::Call { callee, args }) => {
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
            Variant::Expr(ExprNode::UnaryOp {
                operand: inner_operand,
                ..
            }) => {
                //unary op pattern, declare inner operand as pattern variable
                self.declare_pattern(inner_operand)?;
            }
            Variant::Expr(ExprNode::Ident {
                name,
                type_args,
                memory_mode,
                id,
                is_directive,
                is_type,
            }) => {
                if name != "_"
                    && let None = self.lookup(name)
                {
                    //this ident cannot have a namespace if is pattern decl
                    if Self::is_qualified(name) || !type_args.is_empty() || *is_type {
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Pattern variable '{}' must not have namespaces, type arguments, or be a type",
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
                        None,
                        false,
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
                            Vec::new(), //no second order generics for now
                            TypeVariant::Primitive(PrimitiveType::Any),
                            Some(ident.span.clone()),
                        );
                    } else {
                        let symbol = &self.symbol_table[&symbol_id.unwrap()];
                        self.errors.push(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Type argument '{}' has already been declared as a {}",
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
                            message: "Type argument must have a valid identifier".to_string(),
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
            | Variant::VarDecl {..} //pushes scope so that labeled imports are scoped to the label
            | Variant::Block { .. }
            | Variant::If { .. }
            | Variant::For { .. }
            | Variant::Expr(ExprNode::Match { .. }) => true,
            _ => false,
        }
    }

    fn register_decl(&mut self, ast: &mut Node) {
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
                            .get_module_by_name(mod_name.as_str())
                            .unwrap()
                            .clone();

                        for export in module.exports {
                            let local_id = match export.variant {
                                SymbolVariant::Var(var) => self.declare(
                                    module.id,
                                    export.name.clone(),
                                    export.span,
                                    var.type_id,
                                    var.mutable,
                                ),
                                SymbolVariant::Type(t) => self.declare_type(
                                    module.id,
                                    Some(export.name.clone()),
                                    t.generics,
                                    t.variant,
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
                let symbol = self.symbol_table.get(&existing_id).unwrap();
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
                        Vec::new(),                                 //todo: generics
                        TypeVariant::Primitive(PrimitiveType::Any), //todo: default type variant
                        Some(ast.span.clone()),
                    ));
                } else {
                    id = Some(self.declare(
                        self.current_module_id,
                        name.clone(),
                        Some(ast.span),
                        None,
                        mutable,
                    ));
                }
            }
        }
        if let Some(ident) = ident {
            if let Variant::Expr(ExprNode::Ident { id: expr_id, .. }) = &mut ident.variant {
                if let Some(symbol_id) = id {
                    *expr_id = Some(symbol_id);
                } else {
                    *expr_id = None;
                }
            }
        }
    }

    fn top_level_decl_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        if let Variant::Program { children, .. } = &mut ast.variant {
            for child in children.iter_mut() {
                self.register_decl(child);
            }
        } else {
            self.register_decl(ast);
        }
        Ok(())
    }
}
