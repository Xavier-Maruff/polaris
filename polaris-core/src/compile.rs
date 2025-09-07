use crate::{diagnostic::Diagnostic, log};

#[derive(Clone)]
pub struct CompileContext {
    pub logger: log::Logger,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn merge(&mut self, other: CompileContext) {
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    pub fn ingest_source(&mut self, file: String, source: String) -> Result<(), ()> {
        match crate::parse::parse(file, source, self) {
            Ok(_) => Ok(()),
            Err(_) => Err(()),
        }
    }

    pub fn run_passes(&mut self) -> Result<(), ()> {
        Ok(())
    }

    pub fn get_diagnostics(&self) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
        (self.warnings.clone(), self.errors.clone())
    }
}
