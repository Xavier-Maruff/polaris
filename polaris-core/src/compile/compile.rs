use crate::{
    log,
    parse::{ast::Node, diagnostic::Diagnostic, parse::ParseContext},
};
use std::sync::Arc;
use tokio::sync::Mutex;

pub struct CompileContext {
    pub logger: log::Logger,
    pub translation_units: Arc<Mutex<Vec<Node>>>,
}

impl CompileContext {
    pub fn new(logger: log::Logger) -> Self {
        Self {
            logger,
            translation_units: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub async fn add_translation_unit(&mut self, unit: Node) {
        let mut units = self.translation_units.lock().await;
        units.push(unit);
    }

    pub async fn get_diagnostics(&mut self) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let units = self.translation_units.lock().await;

        //todo: parallel impl
        for unit in units.iter() {
            warnings.extend(unit.warnings());
            errors.extend(unit.errors());
        }

        (warnings, errors)
    }
}
