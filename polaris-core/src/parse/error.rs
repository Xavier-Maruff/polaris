use super::parse;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceErr {
    pub message: String,
    pub span: parse::CodeSpan,
    pub file: String,
    pub err_type: SourceErrType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticErr {
    pub primary: SourceErr,
    pub notes: Vec<SourceErr>,
    pub hints: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceErrType {
    UnknownError,
    UnexpectedToken,
    UnmatchedDelimiter,
    UndefinedVariable,
    UndefinedType,
    InvalidArgument,
    IoError,
    //todo
}

pub fn code(c: &SourceErrType) -> &'static str {
    match c {
        SourceErrType::UnknownError => "E0001",
        SourceErrType::UnexpectedToken => "E0002",
        SourceErrType::UnmatchedDelimiter => "E0003",
        SourceErrType::UndefinedVariable => "E0004",
        SourceErrType::UndefinedType => "E0005",
        SourceErrType::InvalidArgument => "E0006",
        SourceErrType::IoError => "E0007",
        // todo
    }
}
