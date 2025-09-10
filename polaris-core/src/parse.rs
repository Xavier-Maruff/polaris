pub mod lexer;
pub mod token;

use crate::ast::{ExprKind, Node, NodeKind, UnaryOp, get_binary_op, get_unary_op};
use crate::compile::CompileContext;
use crate::diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType};
use crate::log;
use crate::parse::lexer::Lexer;
use crate::parse::token::{Token, TokenVariant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

pub fn parse(file: String, source: String, ctx: &mut CompileContext) -> Node {
    ctx.logger.debug(&format!("Parsing file: {}", file));

    let mut parse_ctx = ParseContext::new(&ctx.logger);
    let mut lexer = Lexer::new(file, source);

    let ast = parse_ctx.parse_module(&mut lexer);

    ctx.errors.extend(ast.all_errors());
    ctx.warnings.extend(ast.all_warnings());

    ast
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

        self.advance(lexer).unwrap();
        self.advance(lexer).unwrap();

        while !matches!(self.curr_tok.variant, TokenVariant::EOF) {
            match self.parse_top_level(&mut node, lexer) {
                Err(()) => break,
                Ok(()) => {}
            }
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
        wrap_err!(node, self.expect(lexer, TokenVariant::Import));
        let mut paths = vec![self.parse_ident(node, lexer, false)?];

        while matches!(self.curr_tok.variant, TokenVariant::Slash) {
            wrap_err!(node, self.advance(lexer));
            paths.push(self.parse_ident(node, lexer, false)?);
        }

        let package = paths[0].clone();
        let module = paths[paths.len() - 1].clone();

        //.{ symbol, type other}
        let (top_level, top_level_types) = if matches!(
            (&self.curr_tok.variant, &self.next_tok.variant),
            (TokenVariant::Dot, TokenVariant::LBrace)
        ) {
            wrap_err!(node, self.advance(lexer));
            wrap_err!(node, self.advance(lexer));

            let mut top_level_types = vec![];
            let mut top_level = vec![];

            while !matches!(self.curr_tok.variant, TokenVariant::RBrace) {
                let target = if matches!(self.curr_tok.variant, TokenVariant::Type) {
                    wrap_err!(node, self.advance(lexer));
                    &mut top_level_types
                } else {
                    &mut top_level
                };

                let ident = self.parse_ident(node, lexer, false)?;
                target.push(ident);

                if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                    wrap_err!(node, self.advance(lexer));
                } else {
                    break;
                }
            }

            wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));

            (top_level, top_level_types)
        } else {
            (vec![], vec![])
        };

        let symbol = if matches!(self.curr_tok.variant, TokenVariant::As) {
            wrap_err!(node, self.advance(lexer));
            self.parse_ident(node, lexer, false)?
        } else {
            module.clone()
        };

        let import_node = Node::new(
            NodeKind::Import {
                package,
                module,
                symbol,
                top_level,
                top_level_types,
            },
            CodeSpan {
                start: self.prev_tok.span.start,
                end: self.prev_tok.span.end,
            },
        );

        if let NodeKind::Program { children } = &mut node.kind {
            children.push(import_node);
        } else {
            unreachable!();
        };

        Ok(())
    }

    fn parse_top_level_inner(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
    ) -> Result<(), ()> {
        let n = match self.curr_tok.variant {
            TokenVariant::Harness | TokenVariant::Fn => self.parse_func_decl(node, lexer, is_pub),
            TokenVariant::Type => self.parse_type_decl(node, lexer, is_pub),
            TokenVariant::Const => self.parse_const_decl(node, lexer, is_pub),
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

    fn parse_match_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Match));

        let expr = Box::new(self.parse_expr(node, lexer)?);
        wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));

        let mut arms = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RBrace) {
            let mut patterns = vec![self.parse_expr(node, lexer)?];
            while matches!(self.curr_tok.variant, TokenVariant::BitOr) {
                wrap_err!(node, self.advance(lexer));
                patterns.push(self.parse_expr(node, lexer)?);
            }

            wrap_err!(node, self.expect(lexer, TokenVariant::Arrow));
            let body = self.parse_expr(node, lexer)?;

            arms.push((patterns, body));
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::Match { expr, arms },
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_const_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        public: bool,
    ) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Const));

        let (symbol, const_type, span) = self.parse_binding_decl(node, lexer, false)?;
        wrap_err!(node, self.expect(lexer, TokenVariant::Assign));
        let expr = self.parse_expr(node, lexer)?;

        Ok(Node::new(
            NodeKind::ConstDecl {
                public,
                symbol,
                const_type,
                expr: Box::new(expr),
            },
            CodeSpan {
                start: start_span,
                end: span.end,
            },
        ))
    }

    fn parse_int_literal(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let value = if let TokenVariant::IntLit(value) = &self.curr_tok.variant {
            value.clone()
        } else {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected integer literal, found '{}'", self.curr_tok),
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
        let start_span = self.curr_tok.span.start;
        let parsed = value.parse::<i64>();
        if parsed.is_err() {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Invalid integer literal '{}'", value),
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
        let int_node = Node::new(
            NodeKind::Expr {
                expr: ExprKind::IntLit(parsed.unwrap()),
            },
            CodeSpan {
                start: start_span,
                end: self.curr_tok.span.end,
            },
        );
        wrap_err!(node, self.advance(lexer));
        Ok(int_node)
    }

    fn parse_real_literal(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let value = if let TokenVariant::RealLit(value) = &self.curr_tok.variant {
            value.clone()
        } else {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected real literal, found '{}'", self.curr_tok),
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
        let start_span = self.curr_tok.span.start;
        let parsed = value.parse::<f64>();
        if parsed.is_err() {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Invalid real literal '{}'", value),
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

        let places = if let Some(pos) = value.find('.') {
            value.len() - pos - 1
        } else {
            0
        };

        let real_node = Node::new(
            NodeKind::Expr {
                expr: ExprKind::RealLit {
                    value: parsed.unwrap(),
                    places,
                },
            },
            CodeSpan {
                start: start_span,
                end: self.curr_tok.span.end,
            },
        );
        wrap_err!(node, self.advance(lexer));
        Ok(real_node)
    }

    fn parse_string_literal(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let value = if let TokenVariant::StringLit(value) = &self.curr_tok.variant {
            value.clone()
        } else {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected string literal, found '{}'", self.curr_tok),
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
        let start_span = self.curr_tok.span.start;

        let string_node = Node::new(
            NodeKind::Expr {
                expr: ExprKind::StringLit(value),
            },
            CodeSpan {
                start: start_span,
                end: self.curr_tok.span.end,
            },
        );
        wrap_err!(node, self.advance(lexer));
        Ok(string_node)
    }

    fn parse_if_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::If));

        let condition = Box::new(self.parse_expr(node, lexer)?);
        let then_branch = Box::new(self.parse_block_expr(node, lexer)?);

        let else_branch = if matches!(self.curr_tok.variant, TokenVariant::Else) {
            wrap_err!(node, self.advance(lexer));
            Some(Box::new(self.parse_block_expr(node, lexer)?))
        } else {
            None
        };

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::IfElse {
                    condition,
                    then_branch,
                    else_branch,
                },
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_block_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));

        let mut exprs = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RBrace) {
            let expr = self.parse_expr(node, lexer)?;
            exprs.push(expr);
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::Block(exprs),
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_for_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;

        wrap_err!(node, self.expect(lexer, TokenVariant::For));
        let binding = Box::new(self.parse_expr(node, lexer)?);
        wrap_err!(node, self.expect(lexer, TokenVariant::Assign));

        let start = Box::new(self.parse_expr(node, lexer)?);
        wrap_err!(node, self.expect(lexer, TokenVariant::To));
        let end = Box::new(self.parse_expr(node, lexer)?);

        let body = Box::new(self.parse_block_expr(node, lexer)?);

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::For {
                    binding,
                    start,
                    end,
                    body,
                },
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        self.parse_expr_bind_power(node, lexer, 0)
    }

    fn parse_map_literal(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Octothorpe));
        wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));

        let mut entries = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RBrace) {
            let key = self.parse_expr(node, lexer)?;
            wrap_err!(node, self.expect(lexer, TokenVariant::Colon));
            let value = self.parse_expr(node, lexer)?;
            entries.push((key, value));

            if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                wrap_err!(node, self.advance(lexer));
            } else {
                break;
            }
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::MapLit(entries),
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_tuple_literal(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Octothorpe));
        wrap_err!(node, self.expect(lexer, TokenVariant::LParen));

        let mut elements = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
            let elem = self.parse_expr(node, lexer)?;
            elements.push(elem);

            if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                wrap_err!(node, self.advance(lexer));
            } else {
                break;
            }
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RParen));

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::TupleLit(elements),
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_expr_bind_power(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        min_bp: u8,
    ) -> Result<Node, ()> {
        use TokenVariant::*;

        let mut lhs = match self.curr_tok.variant.clone() {
            If => self.parse_if_expr(node, lexer)?,
            For => self.parse_for_expr(node, lexer)?,
            Fn => self.parse_closure_expr(node, lexer)?,
            Match => self.parse_match_expr(node, lexer)?,
            Let => {
                let (symbols, symbol_type, span) = self.parse_binding_decl(node, lexer, true)?;
                wrap_err!(node, self.expect(lexer, TokenVariant::Assign));
                let expr = self.parse_expr(node, lexer)?;

                return Ok(Node::new(
                    NodeKind::Expr {
                        expr: ExprKind::LetBinding {
                            symbols,
                            symbol_type,
                            expr: Box::new(expr),
                        },
                    },
                    span,
                ));
            }
            LParen => {
                let start_span = self.curr_tok.span.start;
                wrap_err!(node, self.advance(lexer));

                let mut expr = self.parse_expr(node, lexer)?;
                wrap_err!(node, self.expect(lexer, RParen));

                expr.span = CodeSpan {
                    start: start_span,
                    end: self.prev_tok.span.end,
                };

                expr
            }
            LBrace => self.parse_block_expr(node, lexer)?,
            Octothorpe => match &self.next_tok.variant {
                LBrace => self.parse_map_literal(node, lexer)?,
                LParen => self.parse_tuple_literal(node, lexer)?,
                _ => {
                    wrap_err!(
                        node,
                        Err(Diagnostic {
                            primary: DiagnosticMsg {
                                message: format!(
                                    "Expected '{{' or '(' after '#', found '{}'",
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
            },
            IntLit(_) => self.parse_int_literal(node, lexer)?,
            RealLit(_) => self.parse_real_literal(node, lexer)?,
            StringLit(_) => self.parse_string_literal(node, lexer)?,
            Ident(_) => {
                let ident = self.parse_ident(node, lexer, true)?;

                let expr_kind = match ident.as_str() {
                    "_" => ExprKind::Discard,
                    _ => ExprKind::Symbol { name: ident },
                };

                Node::new(
                    NodeKind::Expr { expr: expr_kind },
                    CodeSpan {
                        start: self.prev_tok.span.start,
                        end: self.prev_tok.span.end,
                    },
                )
            }

            t if t.prefix_bind_power().is_some() => {
                let op = match get_unary_op(self.curr_tok.variant.clone()) {
                    Some(op) => op,
                    None => {
                        wrap_err!(
                            node,
                            Err(Diagnostic {
                                primary: DiagnosticMsg {
                                    message: format!(
                                        "'{}' is not a valid unary operator",
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
                };

                let rbp = t.prefix_bind_power().unwrap();

                wrap_err!(node, self.advance(lexer));
                let rhs = self.parse_expr_bind_power(node, lexer, rbp);

                Node::new(
                    NodeKind::Expr {
                        expr: ExprKind::UnaryOp {
                            op,
                            expr: Box::new(rhs?),
                        },
                    },
                    CodeSpan {
                        start: self.prev_tok.span.start,
                        end: self.prev_tok.span.end,
                    },
                )
            }
            _ => {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!("Unexpected token '{}' in expression", self.curr_tok),
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
        };

        self.parse_expr_postfix(node, lexer, &mut lhs, min_bp)?;

        Ok(lhs)
    }

    fn parse_expr_postfix(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        lhs: &mut Node,
        min_bp: u8,
    ) -> Result<(), ()> {
        use TokenVariant::*;
        loop {
            match self.curr_tok.variant.clone() {
                //monad bind
                QuestionMark => {
                    let start_span = self.curr_tok.span.start;
                    wrap_err!(node, self.advance(lexer));
                    *lhs = Node::new(
                        NodeKind::Expr {
                            expr: ExprKind::UnaryOp {
                                op: UnaryOp::MonadBind,
                                expr: Box::new(lhs.clone()),
                            },
                        },
                        CodeSpan {
                            start: start_span,
                            end: self.prev_tok.span.end,
                        },
                    );
                    continue;
                }
                //func call
                LParen => {
                    let start_span = self.curr_tok.span.start;

                    wrap_err!(node, self.advance(lexer));
                    let mut args = vec![];

                    let named_args = matches!(self.curr_tok.variant, TokenVariant::Ident(_))
                        && matches!(self.next_tok.variant, TokenVariant::Colon);

                    while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
                        let start_span = self.curr_tok.span.start;
                        let name = if named_args {
                            let name = self.parse_ident(node, lexer, false)?;
                            wrap_err!(node, self.expect(lexer, TokenVariant::Colon));
                            Some(name)
                        } else {
                            None
                        };

                        //allow name:,
                        if named_args {
                            if matches!(
                                self.curr_tok.variant,
                                TokenVariant::Comma | TokenVariant::RParen
                            ) {
                                wrap_err!(node, self.advance(lexer));
                                args.push((
                                    name.clone(),
                                    Node::new(
                                        NodeKind::Expr {
                                            expr: ExprKind::Symbol {
                                                name: name.unwrap(),
                                            },
                                        },
                                        CodeSpan::new(start_span, self.prev_tok.span.end),
                                    ),
                                ));
                            }
                        } else {
                            args.push((name, self.parse_expr(node, lexer)?));
                        }

                        if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                            wrap_err!(node, self.advance(lexer));
                        } else {
                            break;
                        }
                    }
                    wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
                    *lhs = Node::new(
                        NodeKind::Expr {
                            expr: ExprKind::FnCall {
                                callee: Box::new(lhs.clone()),
                                args,
                            },
                        },
                        CodeSpan {
                            start: start_span,
                            end: self.prev_tok.span.end,
                        },
                    );
                    continue;
                }
                Dot => {
                    //field access
                    let start_span = self.curr_tok.span.start;
                    wrap_err!(node, self.advance(lexer));
                    let name = self.parse_ident(node, lexer, false)?;
                    *lhs = Node::new(
                        NodeKind::Expr {
                            expr: ExprKind::FieldAccess {
                                expr: Box::new(lhs.clone()),
                                field: name,
                            },
                        },
                        CodeSpan {
                            start: start_span,
                            end: self.prev_tok.span.end,
                        },
                    );
                    continue;
                }
                LBracket => {
                    //index access
                    let start_span = self.curr_tok.span.start;
                    wrap_err!(node, self.advance(lexer));
                    let index = self.parse_expr(node, lexer)?;
                    wrap_err!(node, self.expect(lexer, RBracket));
                    *lhs = Node::new(
                        NodeKind::Expr {
                            expr: ExprKind::IndexAccess {
                                expr: Box::new(lhs.clone()),
                                index: Box::new(index),
                            },
                        },
                        CodeSpan {
                            start: start_span,
                            end: self.prev_tok.span.end,
                        },
                    );
                    continue;
                }
                //binary op
                op if op.infix_bind_power().is_some() => {
                    let start_span = self.curr_tok.span.start;
                    let (lbp, rbp) = op.infix_bind_power().unwrap();
                    if lbp < min_bp {
                        break;
                    }
                    let op = match get_binary_op(self.curr_tok.variant.clone()) {
                        Some(op) => op,
                        None => {
                            wrap_err!(
                                node,
                                Err(Diagnostic {
                                    primary: DiagnosticMsg {
                                        message: format!(
                                            "'{}' is not a valid binary operator",
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
                    };
                    wrap_err!(node, self.advance(lexer));

                    let rhs = self.parse_expr_bind_power(node, lexer, rbp)?;
                    *lhs = Node::new(
                        NodeKind::Expr {
                            expr: ExprKind::BinaryOp {
                                //this feels too expensive - don't super care though right now
                                left: Box::new(lhs.clone()),
                                op,
                                right: Box::new(rhs),
                            },
                        },
                        CodeSpan {
                            start: start_span,
                            end: self.prev_tok.span.end,
                        },
                    );
                    continue;
                }
                _ => break,
            }
        }

        Ok(())
    }

    //pattern: type | ident: type | ident | pattern -> (ident/pattern, Option<type>, span)
    fn parse_binding_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        with_let: bool,
    ) -> Result<(Box<Node>, Option<Box<Node>>, CodeSpan), ()> {
        let start_span = self.curr_tok.span.start;
        if with_let {
            wrap_err!(node, self.expect(lexer, TokenVariant::Let));
        }

        let mut type_node = None;

        let symbols = Box::new(self.parse_expr(node, lexer)?);
        if matches!(self.curr_tok.variant, TokenVariant::Colon) {
            wrap_err!(node, self.advance(lexer));
            type_node = Some(Box::new(self.parse_type(node, lexer)?));
        }

        Ok((
            symbols,
            type_node,
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_closure_expr(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;

        wrap_err!(node, self.expect(lexer, TokenVariant::Fn));
        wrap_err!(node, self.expect(lexer, TokenVariant::LParen));

        let mut args = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
            args.push(self.parse_binding_decl(node, lexer, false)?);
            if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                wrap_err!(node, self.advance(lexer));
            } else {
                break;
            }
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
        let return_type = if matches!(self.curr_tok.variant, TokenVariant::Arrow) {
            wrap_err!(node, self.advance(lexer));
            Some(Box::new(self.parse_type(node, lexer)?))
        } else {
            None
        };

        let expr = Box::new(self.parse_block_expr(node, lexer)?);

        Ok(Node::new(
            NodeKind::Expr {
                expr: ExprKind::Closure {
                    args,
                    return_type,
                    expr,
                },
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_func_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        public: bool,
    ) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        let harness = if matches!(self.curr_tok.variant, TokenVariant::Harness) {
            wrap_err!(node, self.advance(lexer));
            true
        } else {
            false
        };

        wrap_err!(node, self.expect(lexer, TokenVariant::Fn));
        let symbol = self.parse_ident(node, lexer, false)?;

        wrap_err!(node, self.expect(lexer, TokenVariant::LParen));

        let mut args = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
            args.push(self.parse_binding_decl(node, lexer, false)?);
            if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                wrap_err!(node, self.advance(lexer));
            } else {
                break;
            }
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RParen));

        let return_type = if matches!(self.curr_tok.variant, TokenVariant::Arrow) {
            wrap_err!(node, self.advance(lexer));
            Some(Box::new(self.parse_type(node, lexer)?))
        } else {
            None
        };

        let expr = if matches!(self.curr_tok.variant, TokenVariant::LBrace) {
            let span = self.curr_tok.span;
            let expr = self.parse_block_expr(node, lexer)?;

            if harness {
                wrap_err!(
                    node,
                    Err(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "Harness function cannot have a body".to_string(),
                            span,
                            file: lexer.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec![
                            "Harness functions are implemented in the harness, and only declared in Polaris.".to_string(),
                            format!(
                                "Remove the body from 'harness fn {} {{ ... }}', or declare {} as a normal function without the 'harness' keyword: fn {} {{ ... }}",
                                symbol, symbol, symbol
                            )
                        ],
                    })
                );
                return Err(());
            }

            Some(Box::new(expr))
        } else {
            None
        };

        if expr.is_none() && !harness {
            wrap_err!(
                node,
                Err(Diagnostic {
                    primary: DiagnosticMsg {
                        message: "Function declaration must have a body".to_string(),
                        span: CodeSpan {
                            start: start_span,
                            end: self.curr_tok.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec![format!(
                        "Add a body to ' fn {} {{ ... }}', or declare {} as a harness function with the 'harness' keyword: harness fn {} {{ ... }}",
                        symbol, symbol, symbol
                    )],
                })
            );
            return Err(());
        }

        Ok(Node::new(
            NodeKind::FnDecl {
                public,
                host: harness,
                symbol,
                return_type,
                args,
                expr,
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    // type MyType
    // type AliasType(a) = Result(a, String)
    // type SumType(a) {
    //   TypeCons(name: String, other: #(String, Int))
    //   TypeCons2(String)
    //   TypeCons3
    // }
    fn parse_type_decl(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        public: bool,
    ) -> Result<Node, ()> {
        let start_span = self.curr_tok.span.start;
        wrap_err!(node, self.expect(lexer, TokenVariant::Type));
        let symbol = self.parse_ident(node, lexer, false)?;

        let mut type_vars = vec![];
        if matches!(self.curr_tok.variant, TokenVariant::LParen) {
            wrap_err!(node, self.advance(lexer));
            while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
                type_vars.push(self.parse_ident(node, lexer, false)?);
                if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                    wrap_err!(node, self.advance(lexer));
                } else {
                    break;
                }
            }
            wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
        }

        match &self.curr_tok.variant {
            TokenVariant::Assign => {
                self.parse_type_alias(node, lexer, public, start_span, symbol, type_vars)
            }
            TokenVariant::LBrace => {
                self.parse_type_constructors(node, lexer, public, start_span, symbol, type_vars)
            }
            _ => Ok(Node::new(
                NodeKind::TypeDecl {
                    public,
                    symbol,
                    type_vars,
                    variants: vec![],
                },
                CodeSpan::new(start_span, self.prev_tok.span.end),
            )),
        }
    }

    // {
    //   TypeCons(name: String, other: #(String, Int))
    //   TypeCons2(String)
    //   TypeCons3
    // }
    fn parse_type_constructors(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
        start_span: usize,
        symbol: String,
        type_vars: Vec<String>,
    ) -> Result<Node, ()> {
        wrap_err!(node, self.expect(lexer, TokenVariant::LBrace));

        let mut variants = vec![];
        while !matches!(self.curr_tok.variant, TokenVariant::RBrace) {
            let constructor_start_span = self.curr_tok.span.start;
            let symbol = self.parse_ident(node, lexer, false)?;

            let mut fields = vec![];
            if matches!(self.curr_tok.variant, TokenVariant::LParen) {
                wrap_err!(node, self.advance(lexer));

                while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
                    let start_span = self.curr_tok.span.start;
                    let label = if matches!(self.next_tok.variant, TokenVariant::Colon)
                        && let TokenVariant::Ident(name) = self.curr_tok.variant.clone()
                    {
                        wrap_err!(node, self.advance(lexer));
                        wrap_err!(node, self.advance(lexer));
                        Some(name.clone())
                    } else {
                        None
                    };

                    fields.push((
                        label,
                        self.parse_type(node, lexer)?,
                        CodeSpan::new(start_span, self.prev_tok.span.end),
                    ));

                    if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                        wrap_err!(node, self.advance(lexer));
                    } else {
                        break;
                    }
                }
                wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
            }

            variants.push(Node::new(
                NodeKind::TypeConstructor { symbol, fields },
                CodeSpan {
                    start: constructor_start_span,
                    end: self.prev_tok.span.end,
                },
            ));
        }

        wrap_err!(node, self.expect(lexer, TokenVariant::RBrace));

        Ok(Node::new(
            NodeKind::TypeDecl {
                public: is_pub,
                symbol,
                type_vars,
                variants,
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    // = Result(a, String)
    fn parse_type_alias(
        &mut self,
        node: &mut Node,
        lexer: &mut Lexer,
        is_pub: bool,
        start_span: usize,
        symbol: String,
        type_vars: Vec<String>,
    ) -> Result<Node, ()> {
        wrap_err!(node, self.expect(lexer, TokenVariant::Assign));
        let actual = Box::new(self.parse_type(node, lexer)?);

        let type_vars = type_vars
            .iter()
            .map(|s| {
                Node::new(
                    NodeKind::Type {
                        nocrypt: false,
                        symbol: s.clone(),
                        type_vars: vec![],
                    },
                    CodeSpan {
                        start: start_span,
                        end: self.prev_tok.span.end,
                    },
                )
            })
            .collect();

        Ok(Node::new(
            NodeKind::TypeAlias {
                public: is_pub,
                actual,
                symbol: symbol.clone(),
                alias: Box::new(Node::new(
                    NodeKind::Type {
                        nocrypt: false,
                        symbol,
                        type_vars,
                    },
                    CodeSpan {
                        start: start_span,
                        end: self.prev_tok.span.end,
                    },
                )),
            },
            CodeSpan {
                start: start_span,
                end: self.prev_tok.span.end,
            },
        ))
    }

    fn parse_type(&mut self, node: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let nocrypt = if matches!(self.curr_tok.variant, TokenVariant::Nocrypt) {
            wrap_err!(node, self.expect(lexer, TokenVariant::Nocrypt));
            true
        } else {
            false
        };

        match self.curr_tok.variant.clone() {
            TokenVariant::Fn => {
                if nocrypt {
                    node.add_error(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "The 'nocrypt' modifier cannot be applied to function types"
                                .to_string(),
                            span: CodeSpan {
                                start: self.curr_tok.span.start,
                                end: self.curr_tok.span.end,
                            },
                            file: lexer.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec!["Remove the 'nocrypt' modifier.".to_string()],
                    });
                    return Err(());
                }

                let start_span = self.curr_tok.span.start;
                wrap_err!(node, self.advance(lexer));
                wrap_err!(node, self.expect(lexer, TokenVariant::LParen));

                let mut args = vec![];

                while !matches!(self.curr_tok.variant, TokenVariant::RParen) {
                    args.push(self.parse_type(node, lexer)?);
                    if matches!(self.curr_tok.variant, TokenVariant::Comma) {
                        wrap_err!(node, self.advance(lexer));
                    } else {
                        break;
                    }
                }

                wrap_err!(node, self.expect(lexer, TokenVariant::RParen));

                let return_type = if matches!(self.curr_tok.variant, TokenVariant::Arrow) {
                    wrap_err!(node, self.advance(lexer));
                    Some(Box::new(self.parse_type(node, lexer)?))
                } else {
                    None
                };

                Ok(Node::new(
                    NodeKind::FnType { args, return_type },
                    CodeSpan {
                        start: start_span,
                        end: self.prev_tok.span.end,
                    },
                ))
            }
            TokenVariant::Ident(ident) => {
                let start_span = self.curr_tok.span.start;
                wrap_err!(node, self.advance(lexer));

                let mut type_params = vec![];
                if matches!(&self.curr_tok.variant, TokenVariant::LParen) {
                    wrap_err!(node, self.advance(lexer));

                    while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                        type_params.push(self.parse_type(node, lexer)?);

                        if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                            wrap_err!(node, self.advance(lexer));
                        } else {
                            break;
                        }
                    }
                    wrap_err!(node, self.expect(lexer, TokenVariant::RParen));
                }

                Ok(Node::new(
                    NodeKind::Type {
                        nocrypt,
                        symbol: ident,
                        type_vars: type_params,
                    },
                    CodeSpan {
                        start: start_span,
                        end: self.prev_tok.span.end,
                    },
                ))
            }
            TokenVariant::Octothorpe => {
                //parse tuple

                if nocrypt {
                    node.add_error(Diagnostic {
                        primary: DiagnosticMsg {
                            message: "The 'nocrypt' modifier cannot be applied to tuple types"
                                .to_string(),
                            span: CodeSpan {
                                start: self.curr_tok.span.start,
                                end: self.curr_tok.span.end,
                            },
                            file: lexer.file.clone(),
                            err_type: DiagnosticMsgType::UnexpectedToken,
                        },
                        notes: vec![],
                        hints: vec!["Remove the 'nocrypt' modifier.".to_string()],
                    });
                    return Err(());
                }

                let start_span = self.curr_tok.span.start;

                wrap_err!(node, self.expect(lexer, TokenVariant::Octothorpe));
                wrap_err!(node, self.expect(lexer, TokenVariant::LParen));

                let mut elements = vec![];

                while !matches!(&self.curr_tok.variant, TokenVariant::RParen) {
                    elements.push(self.parse_type(node, lexer)?);

                    if matches!(&self.curr_tok.variant, TokenVariant::Comma) {
                        wrap_err!(node, self.expect(lexer, TokenVariant::Comma));
                    } else {
                        break;
                    }
                }
                wrap_err!(node, self.expect(lexer, TokenVariant::RParen));

                Ok(Node::new(
                    NodeKind::TupleType { elements },
                    CodeSpan {
                        start: start_span,
                        end: self.prev_tok.span.end,
                    },
                ))
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
