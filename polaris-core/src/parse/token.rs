use crate::parse::CodeSpan;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenVariant {
    //literals
    Ident(String),
    IntLit(String),
    RealLit(String),
    StringLit(String),

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
    Pipeline,

    //delimiters
    Comma,
    Colon,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Arrow,
    QuestionMark,
    Ellipsis,
    Octothorpe, //the more fun name

    //keywords
    Fn,
    Type,
    Let,
    If,
    Else,
    For,
    Import,
    As,
    Assert,
    Harness,
    Pub,
    Const,
    Nocrypt,
    To,
    Match,

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
    pub fn prefix_bind_power(&self) -> Option<u8> {
        use TokenVariant::*;
        match self {
            Plus | Minus => Some(6),
            Star | Slash | Percent => Some(7),
            BitNot | Not => Some(8),
            _ => None,
        }
    }

    pub fn infix_bind_power(&self) -> Option<(u8, u8)> {
        use TokenVariant::*;
        match self {
            Plus | Minus => Some((1, 2)),
            Star | Slash | Percent => Some((3, 4)),
            Equiv | NotEquiv | LessThan | LessThanEquiv | GreaterThan | GreaterThanEquiv => {
                Some((0, 1))
            }
            BitAnd => Some((5, 6)),
            BitXor => Some((7, 8)),
            BitOr => Some((9, 10)),
            And => Some((11, 12)),
            Or => Some((13, 14)),
            Pipeline => Some((17, 18)),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenVariant::*;
        match self {
            Ident(s) => write!(f, "{}", s),
            IntLit(s) => write!(f, "{}", s),
            RealLit(s) => write!(f, "{}", s),
            StringLit(s) => write!(f, "\"{}\"", s),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            Octothorpe => write!(f, "#"),
            Equiv => write!(f, "=="),
            NotEquiv => write!(f, "!="),
            LessThan => write!(f, "<"),
            LessThanEquiv => write!(f, "<="),
            GreaterThan => write!(f, ">"),
            GreaterThanEquiv => write!(f, ">="),
            BitAnd => write!(f, "&"),
            BitOr => write!(f, "|"),
            BitXor => write!(f, "^"),
            BitNot => write!(f, "~"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Not => write!(f, "!"),
            Assign => write!(f, "="),
            Pipeline => write!(f, "|>"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Dot => write!(f, "."),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Arrow => write!(f, "->"),
            QuestionMark => write!(f, "?"),
            Ellipsis => write!(f, "..."),
            Let => write!(f, "let"),
            Type => write!(f, "type"),
            Const => write!(f, "const"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            For => write!(f, "for"),
            To => write!(f, "to"),
            Fn => write!(f, "fn"),
            Import => write!(f, "import"),
            Pub => write!(f, "pub"),
            Harness => write!(f, "harness"),
            As => write!(f, "as"),
            Match => write!(f, "match"),
            Assert => write!(f, "assert"),
            Nocrypt => write!(f, "nocrypt"),
            Comment(s) => match s {
                (text, true) => write!(f, "/*{}*/", text),
                (text, false) => write!(f, "//{}", text),
            },
            EOF => Ok(()),
            Empty => Ok(()),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.variant)
    }
}
