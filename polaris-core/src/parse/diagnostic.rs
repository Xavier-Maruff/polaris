use super::parse;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticMsg {
    pub message: String,
    pub span: parse::CodeSpan,
    pub file: String,
    pub err_type: DiagnosticMsgType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub primary: DiagnosticMsg,
    pub notes: Vec<DiagnosticMsg>,
    pub hints: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticMsgType {
    UnknownError,
    UnexpectedToken,
    UnmatchedDelimiter,
    UndefinedVariable,
    UndefinedType,
    InvalidArgument,
    IoError,
    IllegalName,
    UnterminatedString,
    UnexpectedEOF,
    //todo
}

pub fn code(c: &DiagnosticMsgType) -> &'static str {
    match c {
        DiagnosticMsgType::UnknownError => "E0001",
        DiagnosticMsgType::UnexpectedToken => "E0002",
        DiagnosticMsgType::UnmatchedDelimiter => "E0003",
        DiagnosticMsgType::UndefinedVariable => "E0004",
        DiagnosticMsgType::UndefinedType => "E0005",
        DiagnosticMsgType::InvalidArgument => "E0006",
        DiagnosticMsgType::IoError => "E0007",
        DiagnosticMsgType::IllegalName => "E0008",
        DiagnosticMsgType::UnterminatedString => "E0009",
        DiagnosticMsgType::UnexpectedEOF => "E0010",
        // todo
    }
}

pub fn is_error(c: &DiagnosticMsgType, strict_warn: bool) -> bool {
    let code = code(c);
    code.starts_with('E') || (strict_warn && code.starts_with('W'))
}
