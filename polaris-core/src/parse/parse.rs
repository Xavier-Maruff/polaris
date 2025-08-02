use crate::log;
use crate::parse::{error, error::DiagnosticErr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

pub struct ParseContext<'a> {
    pub file: String,
    pub source: String,
    pub errors: Vec<DiagnosticErr>,
    logger: &'a log::Logger,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub span: CodeSpan,
    pub children: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstNodeKind {
    Program,
    Function,
    Variable,
    Expression,
    Statement,
    Type,
    Literal,
    Operator,
    ControlFlow,
    // todo
}

impl<'a> ParseContext<'a> {
    pub fn new(file: String, source: String, logger: &'a log::Logger) -> Self {
        Self {
            file,
            source,
            errors: Vec::new(),
            logger,
        }
    }

    pub fn parse(&self) -> Result<AstNode, Vec<DiagnosticErr>> {
        self.logger.step("Parsing", self.file.as_str());
        Err(vec![DiagnosticErr {
            primary: error::SourceErr {
                message: "Parsing not implemented yet".to_string(),
                span: CodeSpan { start: 64, end: 78 },
                file: self.file.clone(),
                err_type: error::SourceErrType::UnknownError,
            },
            notes: vec![error::SourceErr {
                message: "This is a placeholder note".to_string(),
                span: CodeSpan { start: 84, end: 89 },
                file: self.file.clone(),
                err_type: error::SourceErrType::UnknownError,
            }],
            hints: vec!["Maybe try implementing parsing you dingus".to_string()],
        }])
    }
}
