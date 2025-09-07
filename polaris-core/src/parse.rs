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

    fn parse_import(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<(), ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Import));

        let mut path_parts = vec![];

        loop {
            if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
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

        //todo
    }

    fn parse_function(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
        top_level: bool,
    ) -> Result<Node, ()> {
        //note: type annotations optional for params and return type
        //top_level:
        //host fn name(params) -> RetType { ... }
        //fn name(params) -> RetType { ... }

        //else
        // fn(param) -> RetType { ... }
        //

        let start_span = self.curr_tok.span.start;
        let is_host = if matches!(self.curr_tok.variant, TokenVariant::Host) {
            wrap_err!(node, self.expect(lexer, TokenVariant::Host));
            true
        } else {
            false
        };

        wrap_err!(node, self.expect(lexer, TokenVariant::Fn));
        let func_name = if top_level {
            if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                let name = ident.clone();
                wrap_err!(node, self.advance(lexer));
                Some(name)
            } else {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!(
                                "Expected function identifier after 'fn', found '{}'",
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
                return Err(());
            }
        } else {
            None
        };

        wrap_err!(node, self.expect(lexer, TokenVariant::LParen));
        let mut params = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
            //parse param
            let param_start_span = self.curr_tok.span.start;
            let param_name = if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                let name = ident.clone();
                wrap_err!(node, self.advance(lexer));
                name
            } else {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!(
                                "Expected parameter identifier in function declaration, found '{}'",
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
                return Err(());
            };

            let param_type = if matches!(self.curr_tok.variant, TokenVariant::Colon) {
                wrap_err!(node, self.expect(lexer, TokenVariant::Colon));
                Some(self.parse_type(node, lexer)?)
            } else {
                None
            };
            let param_end_span = if let Some(t) = &param_type {
                t.span.end
            } else {
                self.prev_tok.span.end
            };
            params.push((
                param_name,
                param_type,
                CodeSpan {
                    start: param_start_span,
                    end: param_end_span,
                },
            ));
            if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
            } else {
                break;
            }
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
        let return_type = if matches!(self.curr_tok.variant, TokenVariant::Arrow) {
            wrap_err!(node, self.expect(lexer, TokenVariant::Arrow));
            Some(self.parse_type(node, lexer)?)
        } else {
            None
        };

        //parse body
        //TODO - actually implement this
        let body = if matches!(self.curr_tok.variant, TokenVariant::LBrace) {
            wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));
            //for now, just skip to the matching }
            let mut brace_count = 1;
            while brace_count > 0 && !matches!(self.curr_tok.variant, TokenVariant::EOF) {
                if matches!(self.curr_tok.variant, TokenVariant::LBrace) {
                    brace_count += 1;
                } else if matches!(self.curr_tok.variant, TokenVariant::RBrace) {
                    brace_count -= 1;
                }
                wrap_err!(node, self.advance(lexer));
            }
            if brace_count == 0 {
                //we found the matching }
                let body_end_span = self.prev_tok.span.end;
                Some(Node::new(
                    //block expressions
                    NodeKind::Expr {
                        expr: ExprKind::Block(vec![]),
                    },
                    CodeSpan {
                        start: self.curr_tok.span.start,
                        end: body_end_span,
                    },
                ))
            } else {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Unclosed '{' in function body".to_string(),
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
                return Err(());
            }
        } else {
            None
        };

        let end_span = if let Some(b) = &body {
            b.span.end
        } else if let Some(t) = &return_type {
            t.span.end
        } else if let Some(p) = params.last() {
            p.2.end
        } else if let Some(name) = &func_name {
            self.prev_tok.span.end
        } else {
            self.prev_tok.span.end
        };

        let func_node = Node::new(
            NodeKind::Fn {
                public: is_pub,
                host: is_host,
                symbol: func_name,
                params,
                return_type: return_type.map(Box::new),
                body: body.map(Box::new),
            },
            CodeSpan {
                start: start_span,
                end: end_span,
            },
        );

        Ok(func_node)
    }

    fn parse_type_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
    ) -> Result<(), ()> {
        //type Name(param1, param2) {
        //  Variant1(val1: param1, val2: Type2)
        // Variant2(val1: param2)
        // Variant3
        //}

        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Type));

        //get name
        let type_name = if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
            let name = ident.clone();
            wrap_err!(node, self.advance(lexer));
            name
        } else {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "Expected type identifier after 'type', found '{}'",
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
            return Err(());
        };

        //check for parens - type params
        let mut type_params = vec![];
        if matches!(&self.curr_tok.variant, TokenVariant::LParen) {
            wrap_err!(node, self.expect(lexer, TokenVariant::LParen));
            while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                    type_params.push((
                        ident.clone(),
                        CodeSpan::new(self.curr_tok.span.start, self.curr_tok.span.end),
                    ));
                    wrap_err!(node, self.advance(lexer));
                    if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                        wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
                    } else {
                        break;
                    }
                } else {
                    wrap_err!(
                        node,
                        Err(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Expected type parameter identifier, found '{}'",
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
                    return Err(());
                }
            }
            wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
        }

        //either { or =
        match &self.curr_tok.variant {
            TokenVariant::Assign => {
                //type alias
                wrap_err!(node, self.expect(lexer, TokenVariant::Assign));
                let aliased_type = self.parse_type(node, lexer)?;

                let end_span = aliased_type.span.end;

                let type_alias_node = Node::new(
                    NodeKind::TypeAlias {
                        public: is_pub,
                        alias: Box::new(Node::new(
                            NodeKind::Type {
                                symbol: type_name,
                                type_vars: type_params
                                    .iter()
                                    .map(|p| {
                                        Node::new(
                                            NodeKind::Type {
                                                symbol: p.0.clone(),
                                                type_vars: vec![],
                                                nocrypt: false,
                                            },
                                            p.1.clone(),
                                        )
                                    })
                                    .collect(),
                                nocrypt: false,
                            },
                            CodeSpan {
                                start: start_span,
                                end: aliased_type.span.end,
                            },
                        )),
                        actual: Box::new(aliased_type),
                    },
                    CodeSpan {
                        start: start_span,
                        end: end_span,
                    },
                );

                if let NodeKind::Program { children } = &mut node.kind {
                    children.push(type_alias_node);
                } else {
                    unreachable!();
                }

                Ok(())
            }

            TokenVariant::LBrace => {
                //type decl
                wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));
                let mut variants = vec![];
                while !matches!(&self.curr_tok.variant, TokenVariant::RBrace) {
                    //parse variant
                    let variant_start_span = self.curr_tok.span.start;
                    let variant_name = if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                        let name = ident.clone();
                        wrap_err!(node, self.advance(lexer));
                        name
                    } else {
                        wrap_err!(
                            node,
                            Err(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!(
                                        "Expected variant identifier in type declaration, found '{}'",
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
                        return Err(());
                    };

                    let mut fields = vec![];
                    if matches!(&self.curr_tok.variant, TokenVariant::LParen) {
                        wrap_err!(node, self.expect(lexer, TokenVariant::LParen));
                        while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                            //parse field
                            //field can either be "name: Type" or just "Type"
                            let field_start_span = self.curr_tok.span.start;
                            let field_name =
                                if let TokenVariant::Ident(ident) = &self.curr_tok.variant {
                                    let name = ident.clone();
                                    wrap_err!(node, self.advance(lexer));
                                    if matches!(&self.curr_tok.variant, TokenVariant::Colon) {
                                        wrap_err!(node, self.expect(lexer, TokenVariant::Colon));
                                        Some(name)
                                    } else {
                                        //not a named field, just a type
                                        None
                                    }
                                } else {
                                    None
                                };
                            let field_type = self.parse_type(node, lexer)?;
                            let field_end_span = field_type.span.end;
                            fields.push((
                                field_name,
                                field_type,
                                CodeSpan {
                                    start: field_start_span,
                                    end: field_end_span,
                                },
                            ));
                            if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                                wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
                            } else {
                                break;
                            }
                        }
                        wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
                    }
                    let variant_end_span = self.prev_tok.span.end;
                    variants.push(Node::new(
                        NodeKind::TypeConstructor {
                            symbol: variant_name,
                            fields,
                        },
                        CodeSpan {
                            start: variant_start_span,
                            end: variant_end_span,
                        },
                    ));
                }

                wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));
                let end_span = self.prev_tok.span.end;
                let type_decl_node = Node::new(
                    NodeKind::TypeDecl {
                        public: is_pub,
                        symbol: type_name,
                        type_vars: type_params.iter().map(|p| p.0.clone()).collect(),
                        variants,
                    },
                    CodeSpan {
                        start: start_span,
                        end: end_span,
                    },
                );

                if let NodeKind::Program { children } = &mut node.kind {
                    children.push(type_decl_node);
                } else {
                    unreachable!();
                }
                Ok(())
            }
            _ => {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!(
                                "Expected '=' or '{{' after type declaration, found '{}'",
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
                return Err(());
            }
        }
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
