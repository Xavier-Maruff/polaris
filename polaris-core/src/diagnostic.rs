use crate::parse::CodeSpan;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticMsg {
    pub message: String,
    pub span: CodeSpan,
    pub file: String,
    pub err_type: DiagnosticMsgType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub primary: DiagnosticMsg,
    pub notes: Vec<DiagnosticMsg>,
    pub hints: Vec<String>,
}

impl Diagnostic {
    pub fn new(primary: DiagnosticMsg) -> Self {
        Self {
            primary,
            notes: Vec::new(),
            hints: Vec::new(),
        }
    }

    pub fn add_note(&mut self, note: DiagnosticMsg) {
        self.notes.push(note);
    }

    pub fn add_hint(&mut self, hint: String) {
        self.hints.push(hint);
    }

    pub fn with_file(mut self, file: String) -> Self {
        self.primary.file = file;
        self
    }
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
    InvalidAstOperation,
    UnsupportedFeature,
    InvalidOperator,
    InvalidModuleName,
    DuplicateModuleDeclaration,
    ModuleNotFound,
    MultipleDeclarations,
}

pub fn code(c: &DiagnosticMsgType) -> &'static str {
    // {Error, Warning}{Syntax, Name, Type, Argument, IO, Compiler}
    match c {
        DiagnosticMsgType::UnknownError => "EC0001",
        DiagnosticMsgType::UnexpectedToken => "ES0002",
        DiagnosticMsgType::UnmatchedDelimiter => "ES0003",
        DiagnosticMsgType::UndefinedVariable => "EN0004",
        DiagnosticMsgType::UndefinedType => "EN0005",
        DiagnosticMsgType::InvalidArgument => "ET0006",
        DiagnosticMsgType::IoError => "EI0007",
        DiagnosticMsgType::IllegalName => "EN0008",
        DiagnosticMsgType::UnterminatedString => "ES0009",
        DiagnosticMsgType::UnexpectedEOF => "ES0010",
        DiagnosticMsgType::InvalidAstOperation => "EC0011",
        DiagnosticMsgType::UnsupportedFeature => "EC0012",
        DiagnosticMsgType::InvalidOperator => "ES0012",
        DiagnosticMsgType::InvalidModuleName => "EN0013",
        DiagnosticMsgType::DuplicateModuleDeclaration => "EN0014",
        DiagnosticMsgType::ModuleNotFound => "EN0015",
        DiagnosticMsgType::MultipleDeclarations => "EN0016",
        // todo
    }
}

pub fn is_error(c: &DiagnosticMsgType, strict_warn: bool) -> bool {
    let code = code(c);
    code.starts_with('E') || (strict_warn && code.starts_with('W'))
}

impl fmt::Display for DiagnosticMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} at {}",
            code(&self.err_type),
            self.message,
            self.span.start
        )
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.primary)?;
        for note in &self.notes {
            writeln!(f, "|- Note: {}", note)?;
        }
        for hint in &self.hints {
            writeln!(f, "|- Hint: {}", hint)?;
        }
        Ok(())
    }
}
