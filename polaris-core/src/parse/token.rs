use crate::parse::CodeSpan;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenVariant {
    //literals
    Ident(String),
    IntLit(String),
    FloatLit(String),
    BoolLit(bool),
    StringLit(String),
    CharLit(String),

    //operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equiv,
    NotEquiv,
    LessThan,
    LessThanEquiv,
    GreaterThan,
    GreaterThanEquiv,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    And,
    Or,
    Not,
    Assign,

    //delimiters
    Comma,
    Semicolon,
    Colon,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Arrow,
    DoubleColon,
    QuestionMark,
    Ellipsis,

    //keywords
    Func,
    Let,
    Mod,
    Return,
    If,
    Else,
    For,
    In,
    Struct,
    Interface,
    Impl,
    Enum,
    Type,
    Ref,
    Weak,
    Match,
    Yield,
    Break,
    Continue,
    Assert,
    Async,
    Await,
    Block,
    Spawn,
    Actor,
    Export,
    Extern,
    Defer,

    //comment (token, multiline)
    Comment((String, bool)),

    //end of file
    EOF,
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub variant: TokenVariant,
    pub span: CodeSpan,
}

impl TokenVariant {
    pub fn precedence(&self) -> u8 {
        match self {
            TokenVariant::Assign => 1,
            TokenVariant::Or => 2,
            TokenVariant::And => 3,
            TokenVariant::BitOr => 4,
            TokenVariant::BitXor => 5,
            TokenVariant::BitAnd => 6,
            TokenVariant::Equiv | TokenVariant::NotEquiv => 7,
            TokenVariant::LessThan
            | TokenVariant::LessThanEquiv
            | TokenVariant::GreaterThan
            | TokenVariant::GreaterThanEquiv => 8,
            TokenVariant::Plus | TokenVariant::Minus => 9,
            TokenVariant::Star | TokenVariant::Slash | TokenVariant::Percent => 10,
            TokenVariant::Not | TokenVariant::BitNot => 11,
            TokenVariant::LParen
            | TokenVariant::RParen
            | TokenVariant::LBracket
            | TokenVariant::RBracket
            | TokenVariant::LBrace
            | TokenVariant::RBrace
            | TokenVariant::Dot
            | TokenVariant::Comma
            | TokenVariant::Semicolon
            | TokenVariant::Colon
            | TokenVariant::Arrow
            | TokenVariant::DoubleColon
            | TokenVariant::QuestionMark
            | TokenVariant::Ellipsis => 12,
            _ => 0,
        }
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenVariant::Plus
                | TokenVariant::Minus
                | TokenVariant::Star
                | TokenVariant::Slash
                | TokenVariant::Percent
                | TokenVariant::Equiv
                | TokenVariant::NotEquiv
                | TokenVariant::LessThan
                | TokenVariant::LessThanEquiv
                | TokenVariant::GreaterThan
                | TokenVariant::GreaterThanEquiv
                | TokenVariant::BitAnd
                | TokenVariant::BitOr
                | TokenVariant::BitXor
                | TokenVariant::BitNot
                | TokenVariant::And
                | TokenVariant::Or
                | TokenVariant::Not
        )
    }
}

impl std::fmt::Display for TokenVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenVariant::Ident(s) => write!(f, "{}", s),
            TokenVariant::IntLit(s) => write!(f, "{}", s),
            TokenVariant::FloatLit(s) => write!(f, "{}", s),
            TokenVariant::BoolLit(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            TokenVariant::StringLit(s) => write!(f, "\"{}\"", s),
            TokenVariant::CharLit(s) => write!(f, "'{}'", s),
            TokenVariant::Plus => write!(f, "+"),
            TokenVariant::Minus => write!(f, "-"),
            TokenVariant::Star => write!(f, "*"),
            TokenVariant::Slash => write!(f, "/"),
            TokenVariant::Percent => write!(f, "%"),
            TokenVariant::Equiv => write!(f, "=="),
            TokenVariant::NotEquiv => write!(f, "!="),
            TokenVariant::LessThan => write!(f, "<"),
            TokenVariant::LessThanEquiv => write!(f, "<="),
            TokenVariant::GreaterThan => write!(f, ">"),
            TokenVariant::GreaterThanEquiv => write!(f, ">="),
            TokenVariant::BitAnd => write!(f, "&"),
            TokenVariant::BitOr => write!(f, "|"),
            TokenVariant::BitXor => write!(f, "^"),
            TokenVariant::BitNot => write!(f, "~"),
            TokenVariant::And => write!(f, "&&"),
            TokenVariant::Or => write!(f, "||"),
            TokenVariant::Not => write!(f, "!"),
            TokenVariant::Assign => write!(f, "="),
            TokenVariant::Comma => write!(f, ","),
            TokenVariant::Semicolon => write!(f, ";"),
            TokenVariant::Colon => write!(f, ":"),
            TokenVariant::Dot => write!(f, "."),
            TokenVariant::LParen => write!(f, "("),
            TokenVariant::RParen => write!(f, ")"),
            TokenVariant::LBracket => write!(f, "["),
            TokenVariant::RBracket => write!(f, "]"),
            TokenVariant::LBrace => write!(f, "{{"),
            TokenVariant::RBrace => write!(f, "}}"),
            TokenVariant::Arrow => write!(f, "=>"),
            TokenVariant::DoubleColon => write!(f, "::"),
            TokenVariant::QuestionMark => write!(f, "?"),
            TokenVariant::Ellipsis => write!(f, "..."),
            TokenVariant::Func => write!(f, "func"),
            TokenVariant::Let => write!(f, "let"),
            TokenVariant::Type => write!(f, "type"),
            TokenVariant::Mod => write!(f, "mod"),
            TokenVariant::Return => write!(f, "return"),
            TokenVariant::If => write!(f, "if"),
            TokenVariant::Else => write!(f, "else"),
            TokenVariant::For => write!(f, "for"),
            TokenVariant::In => write!(f, "in"),
            TokenVariant::Struct => write!(f, "struct"),
            TokenVariant::Interface => write!(f, "interface"),
            TokenVariant::Impl => write!(f, "impl"),
            TokenVariant::Enum => write!(f, "enum"),
            TokenVariant::Ref => write!(f, "ref"),
            TokenVariant::Weak => write!(f, "weak"),
            TokenVariant::Match => write!(f, "match"),
            TokenVariant::Yield => write!(f, "yield"),
            TokenVariant::Break => write!(f, "break"),
            TokenVariant::Continue => write!(f, "continue"),
            TokenVariant::Assert => write!(f, "assert"),
            TokenVariant::Async => write!(f, "async"),
            TokenVariant::Await => write!(f, "await"),
            TokenVariant::Block => write!(f, "block"),
            TokenVariant::Spawn => write!(f, "spawn"),
            TokenVariant::Actor => write!(f, "actor"),
            TokenVariant::Export => write!(f, "export"),
            TokenVariant::Extern => write!(f, "extern"),
            TokenVariant::Defer => write!(f, "defer"),
            TokenVariant::Comment(s) => match s {
                (text, true) => write!(f, "/*{}*/", text),
                (text, false) => write!(f, "//{}", text),
            },
            TokenVariant::EOF => Ok(()),
            TokenVariant::Empty => Ok(()),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.variant)
    }
}
