use std::collections::HashMap;

use crate::module::ModuleContext;
use crate::parse::parse;
use crate::symbol::symbol_pass;
use crate::{diagnostic::Diagnostic, log};

pub type Pass = (&'static str, fn(&mut CompileContext) -> Result<(), ()>);

#[derive(Clone)]
pub struct CompileContext {
    pub logger: log::Logger,
    pub packages: HashMap<String, HashMap<String, ModuleContext>>,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub passes: Vec<Pass>,
    pub symbol_idx: usize,
    pub symbol_imports: HashMap<String, Vec<String>>,
    pub symbol_exports: HashMap<String, Vec<String>>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            packages: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            symbol_idx: 0,
            passes: vec![("Symbol resolution", symbol_pass)],
            symbol_imports: HashMap::new(),
            symbol_exports: HashMap::new(),
        }
    }

    pub fn merge(&mut self, other: CompileContext) {
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    pub fn ingest_source(&mut self, package: String, module: String, file: String, source: String) {
        let ast = parse(file.clone(), source, self);

        let module_ctx = ModuleContext {
            name: module.clone(),
            file,
            ast,
        };
        self.packages
            .entry(package)
            .or_insert_with(HashMap::new)
            .insert(module, module_ctx);
    }

    pub fn run_passes(&mut self) -> Result<(), ()> {
        let passes = self.passes.clone();
        for (name, pass) in passes {
            self.logger.step("Pass", name);
            if let Err(_) = pass(self) {
                self.logger.error(&format!("Pass {} failed", name));
                return Err(());
            }
        }

        Ok(())
    }

    pub fn get_diagnostics(&self) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
        (self.warnings.clone(), self.errors.clone())
    }
}
