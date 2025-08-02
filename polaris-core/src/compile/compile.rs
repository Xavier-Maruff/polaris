use crate::{
    log,
    parse::{
        error::DiagnosticErr,
        parse::{AstNode, ParseContext},
    },
};

pub struct CompileContext<'a> {
    pub logger: &'a log::Logger,
}

impl<'a> CompileContext<'a> {
    pub fn new(logger: &'a log::Logger) -> Self {
        Self { logger }
    }

    pub fn parse(&self, file: String, source: String) -> Result<AstNode, Vec<DiagnosticErr>> {
        let parser = ParseContext::new(file, source, self.logger);
        parser.parse()
    }
}
