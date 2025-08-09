use crate::{
    ast::ast::Node, desugar::desugar, diagnostic::Diagnostic, log, module::ModuleTable,
    parse::parse,
};

#[derive(Debug, Clone)]
pub struct CompileContext {
    pub logger: log::Logger,
    pub stage_one_asts: Vec<Node>,
    pub modules: ModuleTable,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            stage_one_asts: Vec::new(),
            modules: ModuleTable::new(),
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
        self.stage_one_asts.extend(other.stage_one_asts);
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    //parallelisable initial lowering
    pub fn ingest_source(&mut self, file: String, source: String) -> Result<(), ()> {
        let mut ast = parse(file.clone(), source, self)?.unwrap();

        desugar(&mut ast, self, &file)?;

        self.stage_one_asts.push(ast);
        Ok(())
    }

    pub fn get_diagnostics(&self) -> (&Vec<Diagnostic>, &Vec<Diagnostic>) {
        (&self.warnings, &self.errors)
    }
}
