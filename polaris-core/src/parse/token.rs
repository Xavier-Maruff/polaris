use crate::parse::parse;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenVariant {
    //literals
    Ident(String),
    IntLit(String),
    FloatLit(String),
    StringLit(String),
    CharLit(String),
    MacroIdent(String),
    DirectiveIdent(String),

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
    Return,
    If,
    Else,
    For,
    Struct,
    Interface,
    Impl,
    Enum,
    Priv,

    //comment (token, multiline)
    Comment((String, bool)),

    //end of file
    EOF,
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub variant: TokenVariant,
    pub span: parse::CodeSpan,
}

impl TokenVariant {
    pub fn precedence(&self) -> Option<u8> {
        match self {
            TokenVariant::Plus | TokenVariant::Minus => Some(1),
            TokenVariant::Star | TokenVariant::Slash | TokenVariant::Percent => Some(2),
            TokenVariant::BitAnd | TokenVariant::BitOr | TokenVariant::BitXor => Some(3),
            TokenVariant::LessThan
            | TokenVariant::LessThanEquiv
            | TokenVariant::GreaterThan
            | TokenVariant::GreaterThanEquiv => Some(4),
            TokenVariant::Equiv | TokenVariant::NotEquiv => Some(5),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenVariant::Ident(s) => write!(f, "{}", s),
            TokenVariant::IntLit(s) => write!(f, "{}", s),
            TokenVariant::FloatLit(s) => write!(f, "{}", s),
            TokenVariant::StringLit(s) => write!(f, "\"{}\"", s),
            TokenVariant::CharLit(s) => write!(f, "'{}'", s),
            TokenVariant::MacroIdent(s) => write!(f, "{}!", s),
            TokenVariant::DirectiveIdent(s) => write!(f, "@{}", s),
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
            TokenVariant::Return => write!(f, "return"),
            TokenVariant::If => write!(f, "if"),
            TokenVariant::Else => write!(f, "else"),
            TokenVariant::For => write!(f, "for"),
            TokenVariant::Struct => write!(f, "struct"),
            TokenVariant::Interface => write!(f, "interface"),
            TokenVariant::Impl => write!(f, "impl"),
            TokenVariant::Enum => write!(f, "enum"),
            TokenVariant::Priv => write!(f, "priv"),
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
