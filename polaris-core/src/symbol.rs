use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::load_intrinsic_symbols,
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
    pub type_info: Option<Type>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: Option<String>,
    pub generics: Vec<String>,
    pub variant: TypeVariant,
    //todo: generic requirements
}

impl Type {
    pub fn new(name: String, generics: Vec<String>, variant: TypeVariant) -> Self {
        Self {
            name: Some(name),
            generics,
            variant,
        }
    }

    pub fn new_anon(variant: TypeVariant) -> Self {
        Self {
            name: None,
            generics: Vec::new(),
            variant,
        }
    }

    pub fn new_anon_generics(variant: TypeVariant, generics: Vec<String>) -> Self {
        Self {
            name: None,
            generics,
            variant,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeVariant {
    Primitive(String),
    Directive {
        args: Vec<Type>,
    },
    Struct {
        fields: HashMap<String, Type>,
        generics: Vec<String>,
    },
    Enum {
        variants: HashMap<String, Type>,
        generics: Vec<String>,
    },
    Interface {
        methods: HashMap<String, Type>,
        generics: Vec<String>,
    },
    Actor {
        methods: HashMap<String, Type>,
        fields: HashMap<String, Type>,
        generics: Vec<String>,
    },
    Vector {
        element_type: Box<Type>,
        generics: Vec<String>,
    },
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

struct Scope {
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
    pub qualified_symbols: HashMap<String, SymbolId>,
    pub global_symbol_table: HashMap<SymbolId, Symbol>,
    pub symbol_decl_locs: HashMap<SymbolId, CodeSpan>,
    pub module_top_level_scope_ids: HashMap<ModuleId, SymbolId>,
    pub module_table: ModuleTable,
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
            qualified_symbols: HashMap::new(),
            current_module_id: ModuleId::default(),
            symbol_decl_locs: HashMap::new(),
            global_symbol_table: HashMap::new(),
            module_top_level_scope_ids: HashMap::new(),
            is_top_level: true,
            module_table,
        };
        load_intrinsic_symbols(&mut ret);
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
        //not decr scope_id_counter, all scopes are unique
    }

    pub fn declare(
        &mut self,
        name: String,
        span: Option<CodeSpan>,
        type_info: Option<Type>,
        mutable: bool,
    ) -> Option<SymbolId> {
        let id = self.id_counter;
        self.id_counter += 1;

        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.entries.insert(name.clone(), id);

        //should probably create a composite hash key type for qualified names
        //a problem for future me
        let qualified_name = format!("{}::{}::{}", self.current_module_id, current_scope.id, name);
        self.qualified_symbols.insert(qualified_name, id);

        if let Some(span) = span {
            self.symbol_decl_locs.insert(id, span);
        }
        self.global_symbol_table.insert(
            id,
            Symbol {
                id,
                module_id: self.current_module_id,
                scope_id: current_scope.id,
                name,
                span: span,
                type_info,
                mutable,
            },
        );
        Some(id)
    }

    fn lookup(&self, name: &str) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.entries.get(name) {
                return Some(id);
            }
        }
        None
    }

    fn fuzzy_lookup(&self, name: &str) -> Vec<SymbolId> {
        let mut results = Vec::new();
        for scope in self.scopes.iter().rev() {
            for (key, val) in &scope.entries {
                if edit_distance(name, key) <= 2 {
                    results.push(val.clone());
                }
            }
        }

        results
    }

    fn check_reference(&mut self, name: &String, span: CodeSpan) -> Result<SymbolId, ()> {
        //todo: deal with namespaces and type_args
        let symbol_id = self.lookup(name);
        if let Some(symbol_id) = symbol_id {
            return Ok(symbol_id);
        } else {
            let notes = self
                .fuzzy_lookup(name)
                .iter()
                .map(|id| {
                    let symbol = self.global_symbol_table.get(id);
                    if symbol.is_none() {
                        return None;
                    }
                    Some(DiagnosticMsg {
                        message: format!(
                            "Similar symbol '{}' declared here - did you mean this?",
                            symbol.unwrap().name
                        ),
                        span: symbol.unwrap().span.unwrap_or(span),
                        //todo: this could throw, should handle better
                        file: self
                            .module_table
                            .get_module(symbol.unwrap().module_id)
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
                    message: format!("Symbol '{}' is not declared", name),
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
                Variant::Expr(ExprNode::QualifiedIdent {
                    ref mut namespaces,
                    ref mut name,
                    ref mut type_args,
                    ref mut memory_mode,
                    ref mut id,
                }) => {
                    if namespaces.is_empty() {
                        if let Ok(symbol_id) = self.check_reference(name, ast.span) {
                            *id = Some(symbol_id);
                        }
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

        visit_ast_children!(ast.variant, self, name_resolution_visitor, {});

        if push_scope {
            self.pop_scope();
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
            | Variant::For { .. } => true,
            _ => false,
        }
    }

    fn register_decl(&mut self, ast: &mut Node) {
        let mut mutable = false;

        let name = match &ast.variant {
            Variant::ActorDecl { ident, .. }
            | Variant::InterfaceDecl { ident, .. }
            | Variant::EnumDecl { ident, .. }
            | Variant::StructDecl { ident, .. } => ident.get_qualified_ident_str(),
            Variant::FuncDecl { ident, .. } => match ident {
                Some(ident) => ident.get_qualified_ident_str(),
                None => None,
            },

            Variant::VarDecl {
                name, modifiable, ..
            } => {
                mutable = *modifiable;
                Some(name.to_string())
            }
            _ => None,
        };

        if let Some(name) = &name {
            if let Some(existing_id) = self.lookup(name) {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Variable '{}' is already declared", name),
                        span: ast.span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::MultipleDeclarations,
                    },
                    notes: vec![DiagnosticMsg {
                        message: format!("Variable '{}' was previously declared here", name),
                        span: self
                            .symbol_decl_locs
                            .get(&existing_id)
                            .cloned()
                            .unwrap_or(ast.span),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::MultipleDeclarations,
                    }],
                    hints: vec![],
                });
            } else {
                self.declare(name.clone(), Some(ast.span), None, mutable);
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
