use crate::log;
use crate::parse::lexer::Lexer;
use crate::parse::token::{Token, TokenVariant};
use crate::parse::{diagnostic, diagnostic::Diagnostic};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

pub struct ParseContext<'a> {
    logger: &'a log::Logger,
    curr_tok: Token,
    next_tok: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    variant: AstNodeVariant,
    children: Vec<AstNode>,
    warnings: Vec<Diagnostic>,
    errors: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstNodeVariant {
    Program {
        //
    },
    Directive {
        name: String,
    },
}

impl AstNode {
    pub fn new(variant: AstNodeVariant) -> Self {
        Self {
            variant,
            children: Vec::new(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn add_child(&mut self, child: AstNode) {
        self.children.push(child);
    }

    pub fn add_error(&mut self, error: Diagnostic) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: Diagnostic) {
        self.warnings.push(warning);
    }

    pub fn warnings(&self) -> Vec<Diagnostic> {
        let mut warnings = self.warnings.clone();
        for child in &self.children {
            warnings.extend(child.warnings());
        }
        warnings
    }

    pub fn errors(&self) -> Vec<Diagnostic> {
        let mut errors = self.errors.clone();
        for child in &self.children {
            errors.extend(child.errors());
        }
        errors
    }
}

macro_rules! or_return {
    ($ast:ident, $s:expr) => {
        match $s {
            Ok(_) => {}
            Err(err) => {
                $ast.add_error(err);
                return Err(());
            }
        };
    };
}

impl<'a> ParseContext<'a> {
    pub fn new(logger: &'a log::Logger) -> Self {
        Self {
            logger,
            curr_tok: Token {
                variant: TokenVariant::Empty,
                span: CodeSpan { start: 0, end: 0 },
            },
            next_tok: Token {
                variant: TokenVariant::Empty,
                span: CodeSpan { start: 0, end: 0 },
            },
        }
    }

    fn advance(&mut self, lexer: &mut Lexer) -> Result<(), Diagnostic> {
        match lexer.next_token() {
            Ok(token) => {
                std::mem::swap(&mut self.curr_tok, &mut self.next_tok);
                self.next_tok = token;

                if self.curr_tok.variant == TokenVariant::EOF {
                    return Err(Diagnostic {
                        primary: diagnostic::DiagnosticMsg {
                            message: "Unexpected end of input".to_string(),
                            span: CodeSpan {
                                start: lexer.current_position(),
                                end: lexer.current_position(),
                            },
                            file: lexer.file.clone(),
                            err_type: diagnostic::DiagnosticMsgType::UnexpectedEOF,
                        },
                        notes: vec![],
                        hints: vec![
                            "Check for missing tokens or unmatched delimiters.".to_string(),
                        ],
                    });
                }
                Ok(())
            }
            Err(err) => {
                self.logger.diagnostic(&err);
                Err(err)
            }
        }
    }

    pub fn parse(&mut self, file: String, source: String) -> AstNode {
        let mut lexer = Lexer::new(source, file);
        self.parse_translation_unit(&mut lexer)
    }

    fn expect(&mut self, lexer: &mut Lexer, kind: TokenVariant) -> Result<(), Diagnostic> {
        while matches!(self.curr_tok.variant, TokenVariant::Comment(_)) {
            self.advance(lexer)?;
        }

        if self.curr_tok.variant == kind {
            self.advance(lexer)?;
            return Ok(());
        }

        Err(Diagnostic {
            primary: diagnostic::DiagnosticMsg {
                message: format!("Expected '{}', found '{}'", kind, self.curr_tok),
                span: CodeSpan {
                    start: self.curr_tok.span.start,
                    end: self.curr_tok.span.end,
                },
                file: lexer.file.clone(),
                err_type: diagnostic::DiagnosticMsgType::UnexpectedToken,
            },
            notes: vec![],
            hints: vec!["Check your syntax.".to_string()],
        })
    }

    fn parse_translation_unit(&mut self, lexer: &mut Lexer) -> AstNode {
        let mut ast = AstNode::new(AstNodeVariant::Program {});

        for _ in 0..2 {
            match self.advance(lexer) {
                Ok(_) => {
                    //
                }
                Err(err) => {
                    ast.add_error(err);
                    return ast; // stop parsing on error
                }
            }
        }

        while lexer.peek().is_some() {
            match self.parse_statement(&mut ast, lexer) {
                Ok(_) => {
                    //
                }
                Err(err) => {
                    //todo: impl error recovery
                    break;
                }
            }
        }

        ast
    }

    fn parse_statement(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        match self.curr_tok.variant {
            TokenVariant::Func => self.parse_func_decl(ast, lexer),
            TokenVariant::DirectiveIdent(_) => self.parse_directive(ast, lexer),
            TokenVariant::Let => self.parse_var_decl(ast, lexer),
            TokenVariant::Struct => self.parse_struct_decl(ast, lexer),
            TokenVariant::Interface => self.parse_interface_decl(ast, lexer),
            TokenVariant::Impl => self.parse_impl_decl(ast, lexer),
            _ => {
                ast.add_error(Diagnostic {
                    primary: diagnostic::DiagnosticMsg {
                        message: format!(
                            "Unexpected token {:?} in statement position",
                            self.curr_tok.variant
                        ),
                        span: CodeSpan {
                            start: self.curr_tok.span.start,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: diagnostic::DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                });
                Err(())
            }
        }
    }

    fn parse_func_decl(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Function declaration parsing logic here
        Ok(())
    }

    fn parse_directive(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        let node = AstNode::new(AstNodeVariant::Directive {
            name: match &self.curr_tok.variant {
                TokenVariant::DirectiveIdent(name) => name.clone(),
                _ => unreachable!(),
            },
        });
        or_return!(ast, self.advance(lexer));
        //parse exprs separated by commas until semicolon
        or_return!(ast, self.expect(lexer, TokenVariant::Semicolon));

        ast.add_child(node);
        self.logger
            .debug(&format!("Parsed directive: {}", self.curr_tok.variant));
        Ok(())
    }

    fn parse_var_decl(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Variable declaration parsing logic here
        Ok(())
    }

    fn parse_struct_decl(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Struct declaration parsing logic here
        Ok(())
    }

    fn parse_interface_decl(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Interface declaration parsing logic here
        Ok(())
    }

    fn parse_impl_decl(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Implementation declaration parsing logic here
        Ok(())
    }

    fn parse_expr(&mut self, ast: &mut AstNode, lexer: &mut Lexer) -> Result<(), ()> {
        // Expression parsing logic here
        Ok(())
    }
}
