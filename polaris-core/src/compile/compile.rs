use crate::{
    ast::{ast::Node, pass::PassContext},
    desugar::desugar,
    diagnostic::Diagnostic,
    log,
    parse::parse,
};

#[derive(Debug, Clone)]
pub struct CompileContext {
    pub logger: log::Logger,
    pub stage_one_asts: Vec<Node>,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            stage_one_asts: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn merge(&mut self, other: CompileContext) {
        self.stage_one_asts.extend(other.stage_one_asts);
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    //parallelisable initial lowering
    pub fn ingest_source(
        &mut self,
        file: String,
        mut source: String,
    ) -> Result<PassContext<Node>, ()> {
        let mut ctx = PassContext::new(self.logger.clone(), file.clone());
        let mut ast = parse(&mut source, &mut ctx)?.unwrap();

        desugar(&mut ast, &mut ctx)?;

        if !ctx.errors.is_empty() {
            self.errors.extend(ctx.errors.clone());
            self.warnings.extend(ctx.warnings.clone());
            return Err(());
        }

        Ok(ctx)
    }

    pub fn get_diagnostics(&self) -> (&Vec<Diagnostic>, &Vec<Diagnostic>) {
        (&self.errors, &self.warnings)
    }
}
