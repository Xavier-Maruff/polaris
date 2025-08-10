use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    module::ModuleId,
    visit_ast_children,
};

pub type SymbolId = usize;
pub const INVALID_SYMBOL_ID: SymbolId = usize::MAX;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub id: SymbolId,
    pub module_id: ModuleId,
    pub name: String,
    //type info
}

pub fn resolve_names_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut name_resolver = NameResolverContext::new();

    for (file, ast) in ctx.asts.iter_mut() {
        name_resolver.current_module_id = ctx.modules.module_file_ids[file];
        name_resolver.current_file = file.clone();
        name_resolver.scopes.push(Scope {
            entries: HashMap::new(),
            id: name_resolver.scope_id_counter,
        });
        name_resolver.scope_id_counter += 1;
        name_resolver.name_resolver_visitor(ast)?;
    }

    ctx.errors.extend(name_resolver.errors);
    ctx.warnings.extend(name_resolver.warnings);
    Ok(())
}

struct Scope {
    pub id: SymbolId,
    pub entries: HashMap<String, SymbolId>,
}

struct NameResolverContext {
    pub id_counter: SymbolId,
    pub scope_id_counter: SymbolId,
    pub current_file: String,
    pub current_module_id: ModuleId,
    pub scopes: Vec<Scope>,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub qualified_symbols: HashMap<String, SymbolId>,
}

impl NameResolverContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                entries: HashMap::new(),
                id: 0,
            }],
            errors: Vec::new(),
            warnings: Vec::new(),
            current_file: String::new(),
            id_counter: 0,
            scope_id_counter: 1,
            qualified_symbols: HashMap::new(),
            current_module_id: ModuleId::default(),
        }
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

    fn declare(&mut self, name: String) -> Option<SymbolId> {
        let id = self.id_counter;
        self.id_counter += 1;
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.entries.insert(name.clone(), id);
        let qualified_name = format!("{}::{}::{}", self.current_module_id, current_scope.id, name);
        self.qualified_symbols.insert(qualified_name, id);
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

    fn name_resolver_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        self.push_scope();
        match &ast.variant {
            Variant::VarDecl { name, .. } => {
                let name_str = name.to_string();
                if let Some(existing_id) = self.lookup(&name_str) {
                    self.errors.push(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Variable '{}' is already declared", name_str),
                            span: ast.span,
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::MultipleDeclarations,
                        },
                        notes: vec![],
                        hints: vec![],
                    });
                } else {
                    self.declare(name_str.clone());
                }
            }
            _ => {}
        };

        visit_ast_children!(ast.variant, self, name_resolver_visitor, {
            Variant::Expr(ExprNode::QualifiedIdent{
                ref mut namespaces,
                ref mut name,
                ref mut type_args,
                ref mut memory_mode,
                ref mut id
            }) => {
                //
            }
        });
        self.pop_scope();
        Ok(())
    }
}
