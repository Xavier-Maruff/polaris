pub mod lexer;
pub mod token;

use crate::ast::{ExprKind, Node, NodeKind};
use crate::compile::CompileContext;
use crate::diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType};
use crate::log;
use crate::parse::lexer::Lexer;
use crate::parse::token::{Token, TokenVariant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

pub fn parse(file: String, source: String, ctx: &mut CompileContext) -> Result<Option<Node>, ()> {
    let mut parse_ctx = ParseContext::new(&ctx.logger);
    let mut lexer = Lexer::new(file, source);

    let ast = parse_ctx.parse_module(&mut lexer);

    ctx.errors.extend(ast.all_errors());
    ctx.warnings.extend(ast.all_warnings());

    Ok(Some(ast))
}

//internal

struct ParseContext<'a> {
    _logger: &'a log::Logger,
    curr_tok: Token,
    next_tok: Token,
    prev_tok: Token,
}

macro_rules! wrap_err {
    ($ast:ident, $s:expr) => {
        match $s {
            Ok(ret) => ret,
            Err(err) => {
                $ast.add_error(err);
                return Err(());
            }
        };
    };

    //add optional hint
    ($ast:ident, $s:expr, $hint:expr) => {
        match $s {
            Ok(ret) => ret,
            Err(mut err) => {
                err.hints.push($hint.to_string());
                $ast.add_error(err);
                return Err(());
            }
        };
    };
}

impl<'a> ParseContext<'a> {
    pub fn new(logger: &'a log::Logger) -> Self {
        Self {
            _logger: logger,
            curr_tok: Token {
                variant: TokenVariant::Empty,
                span: CodeSpan { start: 0, end: 0 },
            },
            next_tok: Token {
                variant: TokenVariant::Empty,
                span: CodeSpan { start: 0, end: 0 },
            },
            prev_tok: Token {
                variant: TokenVariant::Empty,
                span: CodeSpan { start: 0, end: 0 },
            },
        }
    }

    fn advance(&mut self, lexer: &mut Lexer) -> Result<(), Diagnostic> {
        match lexer.next_token() {
            Ok(token) => {
                if matches!(token.variant, TokenVariant::Comment(_)) {
                    return self.advance(lexer);
                }

                self.prev_tok = self.curr_tok.clone();
                std::mem::swap(&mut self.curr_tok, &mut self.next_tok);
                self.next_tok = token;

                Ok(())
            }
            Err(err) => Err(err),
        }
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
            primary: DiagnosticMsg {
                message: format!(
                    "Expected '{}' after '{}', found '{}'",
                    kind, self.prev_tok, self.curr_tok
                ),
                span: CodeSpan {
                    start: self.curr_tok.span.start,
                    end: self.curr_tok.span.end,
                },
                file: lexer.file.clone(),
                err_type: DiagnosticMsgType::UnexpectedToken,
            },
            notes: vec![],
            hints: vec!["Check your syntax.".to_string()],
        })
    }

    fn parse_module(&mut self, lexer: &mut Lexer) -> Node {
        let mut node = Node::new(
            NodeKind::Program { children: vec![] },
            CodeSpan { start: 0, end: 0 },
        );

        while !matches!(self.curr_tok.variant, TokenVariant::EOF) {
            let _ = self.parse_top_level(&mut node, lexer);
        }

        node
    }

    fn parse_top_level(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<(), ()> {
        match self.curr_tok.variant {
            TokenVariant::Pub => {
                wrap_err!(node, self.advance(lexer));
                self.parse_top_level_inner(node, lexer, true)
            }
            TokenVariant::Import => self.parse_import(node, lexer),
            _ => self.parse_top_level_inner(node, lexer, false),
        }
    }

    fn parse_ident(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        allow_discard: bool,
    ) -> Result<String, ()> {
        if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
            let ident = ident.clone();
            wrap_err!(node, self.advance(lexer));

            if ident == "_" && !allow_discard {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Discard pattern '_' is not allowed here".to_string(),
                            span: CodeSpan {
                                start: self.curr_tok.span.start,
                                end: self.curr_tok.span.end,
                            },
                            file: lexer.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec!["Use a valid identifier.".to_string()],
                    })
                );
                return Err(());
            }
            Ok(ident)
        } else {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected identifier, found '{}'", self.curr_tok),
                        span: CodeSpan {
                            start: self.curr_tok.span.start,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                })
            );
            Err(())
        }
    }

    fn parse_import(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<(), ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Import));

        let mut path_parts = vec![];

        loop {
            if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                if ident == "_" {
                    wrap_err!(
                        node,
                        Err(Diagnostic {
                            primary: DiagnosticMsg {
                                message: "Discard pattern '_' is not allowed in import paths"
                                    .to_string(),
                                span: CodeSpan {
                                    start: self.curr_tok.span.start,
                                    end: self.curr_tok.span.end,
                                },
                                file: lexer.file.clone(),
                                err_type: DiagnosticMsgType::UnexpectedToken,
                            },
                            notes: vec![],
                            hints: vec!["Use a valid identifier.".to_string()],
                        })
                    );
                    return Err(());
                }
                path_parts.push(ident.clone());
                wrap_err!(node, self.expect(lexer, TokenVariant::Slash));
            } else {
                break;
            }
        }

        if path_parts.is_empty() {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "Expected identifier after 'import', found '{}'",
                            self.curr_tok
                        ),
                        span: CodeSpan {
                            start: self.curr_tok.span.start,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                })
            );
        }

        let symbol = if let TokenVariant::As = self.curr_tok.variant {
            wrap_err!(node, self.expect(lexer, TokenVariant::As));
            if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                ident.clone()
            } else {
                node.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "Expected identifier after 'as', found '{}'",
                            self.curr_tok
                        ),
                        span: CodeSpan {
                            start: self.curr_tok.span.start,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                });
                return Err(());
            }
        } else {
            path_parts.last().unwrap().clone()
        };

        let end_span = self.prev_tok.span.end;

        let import_node = Node::new(
            NodeKind::Import { path_parts, symbol },
            CodeSpan {
                start: start_span,
                end: end_span,
            },
        );

        if let NodeKind::Program { children } = &mut node.kind {
            children.push(import_node);
        } else {
            unreachable!();
        }

        Ok(())
    }

    fn parse_top_level_inner(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
    ) -> Result<(), ()> {
        let n = match self.curr_tok.variant {
            TokenVariant::Host | TokenVariant::Fn => self.parse_function(node, lexer, is_pub, true),
            TokenVariant::Type => self.parse_type_decl(node, lexer, is_pub),
            //todo
            //TokenVariant::Const => self.parse_const_decl(node, lexer, is_pub),
            //or directive - todo
            _ => {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Unexpected token '{}' at top level", self.curr_tok),
                            span: CodeSpan {
                                start: self.curr_tok.span.start,
                                end: self.curr_tok.span.end,
                            },
                            file: lexer.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec!["Check your syntax.".to_string()],
                    })
                );
                Err(())
            }
        };

        if let NodeKind::Program { children } = &mut node.kind {
            if let Ok(n) = n {
                children.push(n);
            }
        } else {
            unreachable!();
        }

        Ok(())
    }

    fn parse_params(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
    ) -> Result<Vec<(String, Node, CodeSpan)>, ()> {
        Err(())
    }

    fn parse_function(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
        top_level: bool,
    ) -> Result<Node, ()> {
        Err(())
    }

    fn parse_type_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
    ) -> Result<Node, ()> {
        Err(())
    }

    fn parse_type(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let nocrypt = if matches!(self.curr_tok.variant, TokenVariant::Nocrypt) {
            wrap_err!(node, self.expect(lexer, TokenVariant::Nocrypt));
            true
        } else {
            false
        };

        let mut n = Node::new(
            NodeKind::Type {
                nocrypt,
                symbol: "".to_string(),
                type_vars: vec![],
            },
            CodeSpan::new(lexer.current_position(), lexer.current_position()),
        );

        match self.curr_tok.variant.clone() {
            TokenVariant::Ident(ident) => {
                let start_span = self.curr_tok.span.start;
                wrap_err!(node, self.advance(lexer));

                let mut type_params = vec![];
                if matches!(&self.curr_tok.variant, TokenVariant::LParen) {
                    wrap_err!(node, self.expect(lexer, TokenVariant::LParen));
                    while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                        type_params.push(self.parse_type(&mut n, lexer)?);
                        if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                            wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
                        } else {
                            break;
                        }
                    }
                    wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
                }

                n.kind = NodeKind::Type {
                    nocrypt,
                    symbol: ident,
                    type_vars: type_params,
                };
                n.span = CodeSpan {
                    start: start_span,
                    end: self.prev_tok.span.end,
                };
                Ok(n)
            }
            TokenVariant::Octothorpe => {
                //parse tuple
                let start_span = self.curr_tok.span.start;
                wrap_err!(node, self.expect(lexer, TokenVariant::Octothorpe));
                wrap_err!(node, self.expect(lexer, TokenVariant::LParen));
                let mut type_params = vec![];
                while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                    type_params.push(self.parse_type(&mut n, lexer)?);
                    if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                        wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
                    } else {
                        break;
                    }
                }
                wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
                n.kind = NodeKind::Type {
                    nocrypt,
                    symbol: "#".to_string(),
                    type_vars: type_params,
                };
                n.span = CodeSpan {
                    start: start_span,
                    end: self.prev_tok.span.end,
                };
                Ok(n)
            }
            _ => {
                node.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected type identifier, found '{}'", self.curr_tok),
                        span: CodeSpan {
                            start: self.curr_tok.span.start,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                });
                return Err(());
            }
        }
    }
}
