use std::collections::HashMap;

use crate::{
    ast::ast::Node,
    dependency::dependency_resolution_pass,
    desugar::desugar_pass,
    diagnostic::Diagnostic,
    log,
    module::{ModuleTable, module_graph_pass},
    parse::parse,
    symbol::{SymbolId, SymbolTable, name_resolution_pass},
};

pub type Pass = (&'static str, fn(&mut CompileContext) -> Result<(), ()>);

#[derive(Debug, Clone)]
pub struct CompileContext {
    pub logger: log::Logger,
    pub asts: HashMap<String, Node>,
    pub symbol_id_counter: SymbolId,
    pub modules: ModuleTable,
    pub symbol_table: SymbolTable,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            asts: HashMap::new(),
            modules: ModuleTable::new(),
            symbol_table: SymbolTable::new(),
            symbol_id_counter: 0,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: Diagnostic) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: Diagnostic) {
        self.warnings.push(warning);
    }

    pub fn merge(&mut self, other: CompileContext) {
        self.asts.extend(other.asts);
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    //parallelisable initial lowering
    pub fn ingest_source(&mut self, file: String, source: String) -> Result<(), ()> {
        let mut ast = parse(file.clone(), source, self)?.unwrap();

        desugar_pass(&mut ast, self, &file)?;

        self.asts.insert(file.clone(), ast);
        Ok(())
    }

    pub fn run_passes(&mut self, passes: Option<Vec<Pass>>) -> Result<(), ()> {
        for (name, pass) in match passes {
            Some(p) => p,
            None => CompileContext::get_default_passes(),
        } {
            self.logger.step("Pass", name);
            match pass(self) {
                Ok(_) => {}
                Err(_) => {
                    self.logger.error(&format!("{} failed", name));
                    return Err(());
                }
            }

            // for (file, ast) in &self.asts {
            //     self.logger
            //         .info(&format!("AST for {} after {}:\n{}", file, name, ast));
            // }
        }

        Ok(())
    }

    pub fn get_default_passes() -> Vec<Pass> {
        vec![
            ("Module Resolution", module_graph_pass),
            ("Name Resolution", name_resolution_pass),
            ("Deps Resolution", dependency_resolution_pass),
        ]
    }

    pub fn get_diagnostics(&self) -> (&Vec<Diagnostic>, &Vec<Diagnostic>) {
        (&self.warnings, &self.errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::DefaultLogger;

    #[test]
    fn module_resolution() {
        let (logger, hdl) = DefaultLogger::start_thread(log::LogLevel::Debug);
        let mut ctx = CompileContext::new(logger.clone());

        let test_paths = vec![
            "../test/modules/mod1.pls",
            "../test/modules/mod2.pls",
            "../test/modules/mod3.pls",
        ];

        for path in test_paths {
            let source = std::fs::read_to_string(path).expect("Failed to read test file");
            ctx.ingest_source(path.to_string(), source).unwrap();
            logger.info(&format!("Ingested source from {}", path));
        }

        ctx.run_passes(None).unwrap();

        assert!(
            ctx.errors.is_empty(),
            "Errors should be empty after module resolution"
        );

        logger.quit();
        hdl.join().expect("Failed to join logger thread");
    }
}
