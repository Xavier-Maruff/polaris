use std::collections::HashMap;

use crate::desugar::desugar_pass;
use crate::module::{DepGraphContext, ModuleContext, ModuleId, dependency_pass};
use crate::parse::parse;
use crate::symbol::{SymbolContext, symbol_pass};
use crate::types::typecheck_pass;
use crate::{diagnostic::Diagnostic, log};

pub type Pass = (&'static str, fn(&mut CompileContext) -> Result<(), ()>);

#[derive(Clone)]
pub struct CompileConfig {
    pub warnings_as_errors: bool,
    pub optimise: String,
    pub out_dir: String,
}

pub struct CompileContext {
    pub logger: log::Logger,
    pub config: CompileConfig,
    //package name -> contained modules
    pub packages: HashMap<String, Vec<ModuleId>>,
    pub dependencies: DepGraphContext,
    pub symbols: SymbolContext,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub passes: Vec<Pass>,
}

impl Default for CompileConfig {
    fn default() -> Self {
        Self {
            warnings_as_errors: false,
            optimise: "none".to_string(),
            out_dir: "build".to_string(),
        }
    }
}

impl CompileContext {
    pub fn new(logger: log::Logger, config: CompileConfig) -> Self {
        Self {
            logger,
            config,
            packages: HashMap::new(),
            dependencies: DepGraphContext::default(),
            symbols: SymbolContext::default(),
            errors: Vec::new(),
            warnings: Vec::new(),
            passes: vec![
                ("Desugaring", desugar_pass),
                ("Symbol resolution", symbol_pass),
                ("Dependencies", dependency_pass),
                ("Typechecking", typecheck_pass),
            ],
        }
    }

    pub fn merge(&mut self, other: CompileContext) {
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
        self.packages.extend(other.packages);
        self.symbols.merge(other.symbols);
    }

    pub fn ingest_source(&mut self, package: String, module: String, file: String, source: String) {
        let ast = parse(file.clone(), source, self);

        let module_ctx = ModuleContext {
            name: module.clone(),
            file,
            ast,
        };

        self.dependencies.modules.insert(module.clone(), module_ctx);
        self.packages
            .entry(package)
            .or_insert_with(Vec::new)
            .push(module)
    }

    pub fn run_passes(&mut self) -> Result<(), ()> {
        let passes = self.passes.clone();
        for (name, pass) in passes {
            self.logger.step("Pass", name);
            if pass(self).is_err()
                || !self.errors.is_empty()
                || (self.config.warnings_as_errors && !self.warnings.is_empty())
            {
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
