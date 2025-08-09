use std::collections::HashMap;

use crate::{ast::ast::Node, compile::CompileContext};

pub type SymbolId = usize;

pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    //type info
}

pub fn resolve_names(ast: &mut Node, ctx: &mut CompileContext) -> Result<(), ()> {
    let mut name_resolver = NameResolver::new();
    //
    Ok(())
}

struct Scope {
    pub entries: HashMap<String, SymbolId>,
}

struct NameResolver {
    pub scopes: Vec<Scope>,
}

impl NameResolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                entries: HashMap::new(),
            }],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            entries: HashMap::new(),
        });
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn declare(&mut self, name: String, id: SymbolId) -> Option<SymbolId> {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.entries.insert(name, id)
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.entries.get(name) {
                return Some(id);
            }
        }
        None
    }
}
