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

    pub fn set_primary_span(&mut self, span: CodeSpan) {
        self.primary.span = span;
    }

    pub fn set_primary_message(&mut self, message: String) {
        self.primary.message = message;
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
    UndeclaredSymbol,
    InvalidTopLevelDeclaration,
    TypeMismatch,
    UnificationFailure,
    OccursCheckFailure,
    InfiniteType,
    ArityMismatch,
    UndeclaredFunction,
    InvalidFunctionCall,
    RecursiveTypeDefinition,
    InvalidBindingPattern,
    MismatchedPattern,
    NonExhaustivePatterns,
    UnreachablePattern,
    TypeAliasCycle,
    TypeAliasArityMismatch,
    TypeAliasExpansionFailure,
    Unimplemented,
}

pub fn code(c: &DiagnosticMsgType) -> &'static str {
    // {Error, Warning}{Syntax, Name, Type, Argument, IO, Compiler}
    use DiagnosticMsgType::*;
    match c {
        UnknownError => "EC0001",
        UnexpectedToken => "ES0002",
        UnmatchedDelimiter => "ES0003",
        UndefinedVariable => "EN0004",
        UndefinedType => "EN0005",
        InvalidArgument => "ET0006",
        IoError => "EI0007",
        IllegalName => "EN0008",
        UnterminatedString => "ES0009",
        UnexpectedEOF => "ES0010",
        InvalidAstOperation => "EC0011",
        UnsupportedFeature => "EC0012",
        InvalidOperator => "ES0012",
        InvalidModuleName => "EN0013",
        DuplicateModuleDeclaration => "EN0014",
        ModuleNotFound => "EN0015",
        MultipleDeclarations => "EN0016",
        UndeclaredSymbol => "EN0017",
        InvalidBindingPattern => "ES0028",
        InvalidTopLevelDeclaration => "ET0018",
        TypeMismatch => "ET0019",
        UnificationFailure => "ET0020",
        OccursCheckFailure => "ET0021",
        InfiniteType => "ET0022",
        ArityMismatch => "ET0023",
        UndeclaredFunction => "EN0024",
        InvalidFunctionCall => "ET0025",
        RecursiveTypeDefinition => "ET0026",
        MismatchedPattern => "ET0027",
        NonExhaustivePatterns => "ET0034",
        UnreachablePattern => "WW0001",
        TypeAliasCycle => "ET0031",
        TypeAliasArityMismatch => "ET0032",
        TypeAliasExpansionFailure => "ET0033",
        Unimplemented => "EC0027", // todo
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
