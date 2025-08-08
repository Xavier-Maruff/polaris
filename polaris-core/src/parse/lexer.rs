use std::cmp;

use anyhow::Result;

use crate::{
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    parse::{
        CodeSpan,
        token::{Token, TokenVariant},
    },
};

pub struct Lexer{
    source: String,
    pub file: String,
    pub position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(file: String, source: String) -> Self {
        let mut lexer = Lexer {
            source,
            file,
            position: 0,
            current_char: None,
        };
        lexer.advance();
        lexer
    }

    pub fn advance(&mut self) {
        if self.position < self.source.len() {
            self.current_char = Some(self.source[self.position..].chars().next().unwrap());
            self.position += self.current_char.unwrap().len_utf8();
        } else {
            self.current_char = None;
        }
    }

    pub fn peek(&self) -> Option<char> {
        if self.position < self.source.len() {
            Some(self.source[self.position..].chars().next().unwrap())
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Diagnostic> {
        self.consume_whitespace();
        if let Some(c) = self.current_char {
            match c {
                'a'..='z' | 'A'..='Z' | '_' => return Ok(self.consume_identifier()),
                '0'..='9' => return Ok(self.consume_number()),
                '"' => return self.consume_string(),
                '\'' => return self.consume_char(),
                '@' => return Ok(self.consume_directive()),

                '+' | '-' | '*' | '%' | '^' | '~' | '&' | '|' | '!' | '<' | '>' => {
                    return Ok(self.consume_operator());
                }
                '=' => {
                    if let Some(next_char) = self.peek() {
                        if next_char == '>' {
                            self.advance();
                            self.advance();
                            return Ok(Token {
                                variant: TokenVariant::Arrow,
                                span: CodeSpan {
                                    start: self.position - 2,
                                    end: self.position,
                                },
                            });
                        }

                        return Ok(self.consume_operator());
                    }
                }
                '/' => {
                    if let Some(next_char) = self.peek() {
                        if next_char == '/' {
                            return Ok(self.consume_comment());
                        } else if next_char == '*' {
                            return Ok(self.consume_multiline_comment());
                        }
                    }
                    return Ok(self.consume_operator());
                }
                '?' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::QuestionMark,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                ':' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == ':' {
                            self.advance();
                            return Ok(Token {
                                variant: TokenVariant::DoubleColon,
                                span: CodeSpan {
                                    start: self.position - 2,
                                    end: self.position,
                                },
                            });
                        }
                    }
                    return Ok(Token {
                        variant: TokenVariant::Colon,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                ';' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::Semicolon,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                ',' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::Comma,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                '.' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == '.' {
                            self.advance();
                            if let Some(next_next_char) = self.current_char {
                                if next_next_char == '.' {
                                    self.advance();
                                    return Ok(Token {
                                        variant: TokenVariant::Ellipsis,
                                        span: CodeSpan {
                                            start: self.position - 3,
                                            end: self.position,
                                        },
                                    });
                                }
                            }
                        }
                    }
                    return Ok(Token {
                        variant: TokenVariant::Dot,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                '(' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::LParen,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                ')' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::RParen,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                '{' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::LBrace,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                '}' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::RBrace,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                '[' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::LBracket,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                ']' => {
                    self.advance();
                    return Ok(Token {
                        variant: TokenVariant::RBracket,
                        span: CodeSpan {
                            start: self.position - 1,
                            end: self.position,
                        },
                    });
                }
                _ => {
                    return Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Unexpected character: '{}'", c),
                            span: CodeSpan {
                                start: self.position - 1,
                                end: self.position,
                            },
                            file: self.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec![
                            "Check the syntax and ensure all characters are valid.".to_string(),
                        ],
                    });
                }
            }
        }

        Ok(Token {
            variant: TokenVariant::EOF,
            span: CodeSpan {
                start: self.position,
                end: self.position,
            },
        })
    }

    pub fn consume_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn consume_identifier(&mut self) -> Token {
        let start = self.position - 1;
        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }


        let ident_str = self.source[start..self.position - 1].to_string();
        Token {
            variant: match ident_str.as_str() {
                "func" => TokenVariant::Func,
                "let" => TokenVariant::Let,
                "mod" => TokenVariant::Mod,
                "return" => TokenVariant::Return,
                "if" => TokenVariant::If,
                "else" => TokenVariant::Else,
                "for" => TokenVariant::For,
                "struct" => TokenVariant::Struct,
                "interface" => TokenVariant::Interface,
                "impl" => TokenVariant::Impl,
                "enum" => TokenVariant::Enum,
                "priv" => TokenVariant::Priv,
                "type" => TokenVariant::Type,
                "in" => TokenVariant::In,
                "ref" => TokenVariant::Ref,
                "weak" => TokenVariant::Weak,
                "match" => TokenVariant::Match,
                "yield" => TokenVariant::Yield,
                "break" => TokenVariant::Break,
                "continue" => TokenVariant::Continue,
                "assert" => TokenVariant::Assert,
                "async" => TokenVariant::Async,
                "await" => TokenVariant::Await,
                "block" => TokenVariant::Block,
                "actor" => TokenVariant::Actor,
                _ => TokenVariant::Ident(ident_str),
            },
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        }
    }

    //todo: add hex and octal support
    pub fn consume_number(&mut self) -> Token {
        let start = self.position - 1;
        let mut is_float = false;

        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                self.advance();
            } else if c == '.' && !is_float {
                is_float = true;
                self.advance();
            } else {
                break;
            }
        }

        let number_str = self.source[start..self.position - 1].to_string();
        Token {
            variant: match is_float {
            true => TokenVariant::FloatLit(number_str),
            false => TokenVariant::IntLit(number_str),
            },
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        }
    }

    pub fn consume_string(&mut self) -> Result<Token, Diagnostic> {
        let start = self.position;
        self.advance(); // skip opening quote
        while let Some(c) = self.current_char {
            if c == '"' {
                self.advance(); // skip closing quote
                break;
            } else if c == '\\' {
                self.advance(); // skip escape character
                self.advance(); // skip next character
            } else {
                self.advance();
            }
        }
        if self.current_char.is_none() {
            return Err(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Unterminated string literal".to_string(),
                    span: CodeSpan {
                        start,
                        end: start + cmp::min(self.source.len() - start, 5),
                    },
                    file: self.file.clone(),
                    err_type: DiagnosticMsgType::UnterminatedString,
                },
                notes: vec![],
                hints: vec![
                    "Add a '\"' at the end of the string.".to_string(),
                    "Check for missing escape characters.".to_string(),
                ],
            });
        }

        Ok(Token{
            variant: TokenVariant::StringLit(
            self.source[start..self.position - 2].to_string(),
        ),
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        })
    }

    pub fn consume_char(&mut self) -> Result<Token, Diagnostic> {
        self.advance(); // skip opening quote
        let start = self.position; // start after opening quote
        if let Some(c) = self.current_char {
            if c != '\'' {
                self.advance(); // skip character
            }
        }

        if self.current_char.is_none() || self.current_char.unwrap() != '\'' {
            return Err(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Unterminated character literal".to_string(),
                    span: CodeSpan {
                        start,
                        end: start + cmp::min(self.source.len() - start, 5),
                    },
                    file: self.file.clone(),
                    err_type: DiagnosticMsgType::UnterminatedString,
                },
                notes: vec![],
                hints: vec![
                    "Add the missing \"'\" at the end of the character literal.".to_string(),
                    "Check for missing escape characters.".to_string(),
                ],
            });
        }

        self.advance(); // skip closing quote
        /*Ok(Token::CharLit(
            self.source[start..self.position - 1].to_string(),
        ))*/
        Ok(Token {
            variant: TokenVariant::CharLit(
                self.source[start..self.position - 2].to_string(), // Exclude the closing quote
            ),
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        })
    }

    pub fn consume_directive(&mut self) -> Token {
        let start = self.position; // skip '@'
        self.advance();
        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        Token {
            variant: TokenVariant::DirectiveIdent(
                self.source[start..self.position - 1].to_string(),
            ),
            span: CodeSpan {
                start: start - 1,
                end: self.position - 1,
            },
        }
    }

    pub fn consume_operator(&mut self) -> Token {
        let start = self.position - 1;
        if let Some(c) = self.current_char {
            match c {
                '=' | '!' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == '=' {
                            self.advance();
                        }
                    }
                }
                '&' | '|' | '*' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == c {
                            self.advance();
                        }
                    }
                }
                '<' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == '=' || next_char == '<' {
                            self.advance();
                        }
                    }
                }
                '>' => {
                    self.advance();
                    if let Some(next_char) = self.current_char {
                        if next_char == '=' || next_char == '>' {
                            self.advance();
                        }
                    }
                }
                '+' | '-' | '/' | '%' | '^' | '~' => {
                    self.advance();
                }
                _ => {}
            }
        }

        let operator_str = self.source[start..self.position - 1].to_string();
       Token{
           variant: match operator_str.as_str() {
                "+" => TokenVariant::Plus,
                "-" => TokenVariant::Minus,
                "*" => TokenVariant::Star,
                "/" => TokenVariant::Slash,
                "%" => TokenVariant::Percent,
                "==" => TokenVariant::Equiv,
                "!=" => TokenVariant::NotEquiv,
                "<" => TokenVariant::LessThan,
                "<=" => TokenVariant::LessThanEquiv,
                ">" => TokenVariant::GreaterThan,
                ">=" => TokenVariant::GreaterThanEquiv,
                "&" => TokenVariant::BitAnd,
                "|" => TokenVariant::BitOr,
                "^" => TokenVariant::BitXor,
                "~" => TokenVariant::BitNot,
                "&&" => TokenVariant::And,
                "||" => TokenVariant::Or,
                "!" => TokenVariant::Not,
                "=" => TokenVariant::Assign,
                _ => {
                    unreachable!()
                }
           },
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
       }
    }

    pub fn consume_comment(&mut self) -> Token {
        self.advance(); // skip /
        self.advance(); // skip /
        let start = self.position-1;

        while let Some(c) = self.current_char {
            if c == '\n' {
                self.advance();
                break;
            } else {
                self.advance();
            }
        }

        let comment = self.source[start..self.position - 1].to_string(); // exclude newline
        //Token::Comment((comment, false))
        Token {
            variant: TokenVariant::Comment((comment, false)),
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        }
    }

    pub fn consume_multiline_comment(&mut self) -> Token {
        self.advance(); // skip /
        self.advance(); // skip *
        let start = self.position;
        while let Some(c) = self.current_char {
            if c == '*' {
                self.advance();
                if let Some(next_char) = self.current_char {
                    if next_char == '/' {
                        self.advance(); // skip closing */
                        break;
                    }
                }
            } else {
                self.advance();
            }
        }

        let comment = self.source[start..self.position - 2].to_string(); // Exclude the closing */
        //Token::Comment((comment, true))
        Token {
            variant: TokenVariant::Comment((comment, true)),
            span: CodeSpan {
                start,
                end: self.position - 1,
            },
        }
    }

    pub fn current_position(&self) -> usize {
        self.position
    }
}
