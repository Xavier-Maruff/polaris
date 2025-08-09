use std::collections::HashMap;

use crate::ast::ast::Node;

pub type ModuleId = usize;

#[derive(Debug, Clone)]
pub struct ModuleTable {
    modules: HashMap<String, Module>,
}

#[derive(Debug, Clone)]
pub struct Module {
    name: String,
    id: ModuleId,
    dependencies: Vec<ModuleId>,
    exports: Vec<Node>,
}

impl ModuleTable {
    pub fn new() -> Self {
        ModuleTable {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(
        &mut self,
        name: String,
        id: ModuleId,
        dependencies: Vec<ModuleId>,
        exports: Vec<Node>,
    ) {
        let module = Module {
            name,
            id,
            dependencies,
            exports,
        };
        self.modules.insert(module.name.clone(), module);
    }

    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }
}

struct ModuleContext {
    //
}
