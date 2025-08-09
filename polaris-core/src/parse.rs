pub mod lexer;
pub mod token;

use crate::ast::ast::*;
use crate::compile::CompileContext;
use crate::diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType};
use crate::log;
use crate::parse::lexer::Lexer;
use crate::parse::token::{Token, TokenVariant};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

//mutability of source is unnecessary, but means adheres to general Pass type
pub fn parse(file: String, source: String, ctx: &mut CompileContext) -> Result<Option<Node>, ()> {
    let mut parse_ctx = ParseContext::new(&ctx.logger);
    let mut lexer = Lexer::new(file, source);

    let ast = parse_ctx.parse_module(&mut lexer);

    ctx.errors.extend(ast.errors());
    ctx.warnings.extend(ast.warnings());

    Ok(Some(ast))
}

//internal

struct ParseContext<'a> {
    _logger: &'a log::Logger,
    curr_tok: Token,
    next_tok: Token,
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
        }
    }

    fn advance(&mut self, lexer: &mut Lexer) -> Result<(), Diagnostic> {
        match lexer.next_token() {
            Ok(token) => {
                std::mem::swap(&mut self.curr_tok, &mut self.next_tok);
                self.next_tok = token;

                match self.curr_tok.variant {
                    TokenVariant::Comment(_) => {
                        // Skip comments
                        return self.advance(lexer);
                    }
                    _ => {}
                }
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
                message: format!("Expected '{}', found '{}'", kind, self.curr_tok),
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
        let mut ast = Node::new(Variant::Program {
            children: Vec::new(),
            file: lexer.file.clone().clone(),
        });

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
            let start_pos = lexer.current_position();
            let node = match self.parse_statement(&mut ast, lexer, true) {
                Ok(node) => node,
                Err(_) => Node::new_with_span(
                    Variant::Failed,
                    CodeSpan::new(start_pos, lexer.current_position()),
                ),
            };
            match &mut ast.variant {
                Variant::Program { children, .. } => {
                    children.push(node);
                }
                _ => unreachable!(),
            }
        }

        ast
    }

    fn parse_return_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Return));

        let mut value = None;
        if self.curr_tok.variant != TokenVariant::Semicolon {
            value = Some(Box::new(self.parse_expr(ast, lexer, false)?));
        }

        span.end = lexer.current_position();
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));

        Ok(Node::new_with_span(Variant::Return { value }, span))
    }

    fn parse_if_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::If));

        //not allowing if let yet until pattern matching working properly
        /*  let condition;
        if matches!(self.curr_tok.variant, TokenVariant::Let) {
            condition = Box::new(self.parse_var_decl(ast, lexer)?);
        } else {
            condition = Box::new(self.parse_expr(ast, lexer, false)?);
        }*/

        if matches!(self.curr_tok.variant, TokenVariant::Let) {
            ast.add_error(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Pattern matching in 'if' statements is not yet supported."
                        .to_string(),
                    span: CodeSpan {
                        start: self.curr_tok.span.start,
                        end: self.curr_tok.span.end,
                    },
                    file: lexer.file.clone(),
                    err_type: DiagnosticMsgType::UnsupportedFeature,
                },
                notes: vec![],
                hints: vec!["Use a match expression instead if unwrapping an enum.".to_string()],
            });
            return Err(());
        }

        let condition = Box::new(self.parse_expr(ast, lexer, false)?);
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        let mut body = Vec::new();
        while self.curr_tok.variant != TokenVariant::RBrace {
            body.push(self.parse_statement(ast, lexer, false)?);
        }
        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        let else_body = if self.curr_tok.variant == TokenVariant::Else {
            wrap_err!(ast, self.advance(lexer));
            if self.curr_tok.variant == TokenVariant::If {
                Some(Box::new(self.parse_if_stmt(ast, lexer)?))
            } else {
                wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));
                let mut else_body = Vec::new();
                while self.curr_tok.variant != TokenVariant::RBrace {
                    else_body.push(self.parse_statement(ast, lexer, false)?);
                }
                wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));
                Some(Box::new(Node::new(Variant::Block {
                    children: else_body,
                })))
            }
        } else {
            None
        };

        span.end = lexer.current_position();

        Ok(Node::new_with_span(
            Variant::If {
                condition,
                then_branch: Box::new(Node::new(Variant::Block { children: body })),
                else_branch: else_body,
            },
            span,
        ))
    }

    fn parse_for_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::For));

        let mut variant = ForVariant::ForInfinite;

        match self.curr_tok.variant {
            TokenVariant::Let => {
                wrap_err!(ast, self.advance(lexer));
                wrap_err!(
                    ast,
                    self.expect(lexer, TokenVariant::Mod),
                    "Iteration variables must always be modifiable."
                );

                let ident = Box::new(self.parse_ident(ast, lexer)?);

                wrap_err!(ast, self.expect(lexer, TokenVariant::In));
                let iterable = Box::new(self.parse_expr(ast, lexer, false)?);

                variant = ForVariant::ForIter { ident, iterable };
            }
            TokenVariant::LBrace => {
                // infinite for loop
            }
            _ => {
                let cond = Box::new(self.parse_expr(ast, lexer, false)?);
                variant = ForVariant::ForWhile { condition: cond };
            }
        };

        wrap_err!(
            ast,
            self.expect(lexer, TokenVariant::LBrace),
            "If you are trying to use a range-based for loop, you must use 'let mod' before the variable declaration."
        );
        let mut body = Vec::new();
        while self.curr_tok.variant != TokenVariant::RBrace {
            body.push(self.parse_statement(ast, lexer, false)?);
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));
        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::For {
                variant,
                body: Box::new(Node::new(Variant::Block { children: body })),
            },
            span,
        ))
    }

    fn parse_type_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);

        wrap_err!(ast, self.expect(lexer, TokenVariant::Type));

        let ident = Box::new(self.parse_ident(ast, lexer)?);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Assign));

        let alias_of = Box::new(self.parse_ident(ast, lexer)?);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));

        span.end = lexer.current_position();

        Ok(Node::new_with_span(
            Variant::TypeDecl { ident, alias_of },
            span,
        ))
    }

    fn parse_yield_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Yield));

        let value = if self.curr_tok.variant != TokenVariant::Semicolon {
            Some(Box::new(self.parse_expr(ast, lexer, false)?))
        } else {
            None
        };

        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));
        span.end = lexer.current_position();

        Ok(Node::new_with_span(Variant::Yield { value }, span))
    }

    fn parse_assert_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Assert));
        wrap_err!(ast, self.expect(lexer, TokenVariant::LParen));

        let condition = Box::new(self.parse_expr(ast, lexer, false)?);
        let mut messages = Vec::new();
        while self.curr_tok.variant == TokenVariant::Comma {
            wrap_err!(ast, self.advance(lexer));
            messages.push(self.parse_expr(ast, lexer, false)?);
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));

        span.end = lexer.current_position();

        Ok(Node::new_with_span(
            Variant::Assert {
                condition,
                messages,
            },
            span,
        ))
    }

    fn parse_break_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Break));
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));
        span.end = lexer.current_position();

        Ok(Node::new_with_span(Variant::Break, span))
    }

    fn parse_continue_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Continue));
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));
        span.end = lexer.current_position();

        Ok(Node::new_with_span(Variant::Continue, span))
    }

    fn parse_expr_stmt(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let expr = self.parse_expr(ast, lexer, false)?;
        wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));

        Ok(expr)
    }

    fn parse_statement(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        _top_level: bool,
    ) -> Result<Node, ()> {
        let failed_start = lexer.current_position();

        let export = if self.curr_tok.variant == TokenVariant::Export {
            wrap_err!(ast, self.advance(lexer));
            true
        } else {
            false
        };

        let node = match self.curr_tok.variant {
            TokenVariant::If => self.parse_if_stmt(ast, lexer),
            TokenVariant::For => self.parse_for_stmt(ast, lexer),
            TokenVariant::Func | TokenVariant::Async => self.parse_func_decl(ast, lexer, false),
            TokenVariant::Return => self.parse_return_stmt(ast, lexer),
            TokenVariant::Yield => self.parse_yield_stmt(ast, lexer),
            TokenVariant::Let => self.parse_var_decl(ast, lexer),
            TokenVariant::Struct => self.parse_struct_decl(ast, lexer),
            TokenVariant::Interface => self.parse_interface_decl(ast, lexer),
            TokenVariant::Actor => self.parse_actor_decl(ast, lexer),
            TokenVariant::Enum => self.parse_enum_decl(ast, lexer),
            TokenVariant::Impl => self.parse_impl_decl(ast, lexer),
            TokenVariant::Type => self.parse_type_decl(ast, lexer),
            TokenVariant::Break => self.parse_break_stmt(ast, lexer),
            TokenVariant::Continue => self.parse_continue_stmt(ast, lexer),
            TokenVariant::Assert => self.parse_assert_stmt(ast, lexer),
            _ => self.parse_expr_stmt(ast, lexer),
        };

        match node {
            Err(_) => Ok(Node::new_with_span(
                Variant::Failed,
                CodeSpan::new(failed_start, self.curr_tok.span.end),
            )),
            Ok(mut node) => {
                node.export = export;
                Ok(node)
            }
        }
    }

    fn parse_func_decl(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        body_required: bool,
    ) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        let is_async = match self.curr_tok.variant {
            TokenVariant::Async => {
                wrap_err!(ast, self.advance(lexer));
                true
            }
            _ => false,
        };

        wrap_err!(ast, self.expect(lexer, TokenVariant::Func));

        let mut ident = None;

        if matches!(self.curr_tok.variant, TokenVariant::Ident(_)) {
            ident = Some(Box::new(self.parse_ident(ast, lexer)?));
        }

        let mut capture_list = None;
        if matches!(self.curr_tok.variant, TokenVariant::LBracket) {
            wrap_err!(ast, self.advance(lexer));
            let mut captures = Vec::new();
            while !matches!(self.curr_tok.variant, TokenVariant::RBracket) {
                captures.push(self.parse_ident(ast, lexer)?);
            }
            wrap_err!(ast, self.advance(lexer));

            capture_list = Some(captures);
        }

        let mut params = Vec::new();
        wrap_err!(ast, self.expect(lexer, TokenVariant::LParen));

        while self.curr_tok.variant != TokenVariant::RParen {
            params.push(self.parse_func_param(ast, lexer)?);
            if self.curr_tok.variant == TokenVariant::Comma {
                wrap_err!(ast, self.advance(lexer));
            } else if self.curr_tok.variant != TokenVariant::RParen {
                ast.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected ',' or ')', found {:?}", self.curr_tok.variant),
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

        wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));

        let return_type = if matches!(self.curr_tok.variant, TokenVariant::Colon) {
            wrap_err!(ast, self.advance(lexer));
            Some(Box::new(self.parse_ident(ast, lexer)?))
        } else {
            None
        };

        let body = if self.curr_tok.variant == TokenVariant::LBrace {
            wrap_err!(ast, self.advance(lexer));
            let mut body = Vec::new();
            while self.curr_tok.variant != TokenVariant::RBrace {
                body.push(self.parse_statement(ast, lexer, false)?);
            }
            wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));
            Some(Box::new(Node::new(Variant::Block { children: body })))
        } else {
            wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));
            None
        };

        if body.is_none() && body_required {
            ast.add_error(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Function body is required".to_string(),
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

        span.end = lexer.current_position();

        Ok(Node::new_with_span(
            Variant::FuncDecl {
                ident,
                params,
                return_type,
                body,
                capture_list,
                is_async,
            },
            span,
        ))
    }

    fn parse_directive(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Directive));

        let mut args = Vec::new();
        let ident = Box::new(self.parse_ident(ast, lexer)?);

        if matches!(self.curr_tok.variant, TokenVariant::LParen) {
            wrap_err!(ast, self.advance(lexer));
            args = self.parse_list(ast, lexer, false)?;
            wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));
        }

        span.end = lexer.current_position();

        let node = Node::new_with_span(Variant::Expr(ExprNode::Directive { ident, args }), span);

        Ok(node)
    }

    fn parse_list(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        disallow_angles: bool,
    ) -> Result<Vec<Node>, ()> {
        let mut elements = vec![self.parse_expr(ast, lexer, disallow_angles)?];
        while self.curr_tok.variant == TokenVariant::Comma {
            wrap_err!(ast, self.advance(lexer));
            elements.push(self.parse_expr(ast, lexer, disallow_angles)?);
        }

        Ok(elements)
    }

    fn parse_var_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Let));

        let mut modifiable = false;
        if self.curr_tok.variant == TokenVariant::Mod {
            modifiable = true;
            wrap_err!(ast, self.advance(lexer));
        }

        if let TokenVariant::Ident(name) = self.curr_tok.variant.clone() {
            wrap_err!(ast, self.advance(lexer));

            let mut var_type = None;
            let mut initialiser = None;

            if self.curr_tok.variant == TokenVariant::Colon {
                wrap_err!(ast, self.advance(lexer));
                var_type = Some(Box::new(self.parse_ident(ast, lexer)?));
            }

            if self.curr_tok.variant == TokenVariant::Assign {
                wrap_err!(ast, self.advance(lexer));
                initialiser = Some(Box::new(self.parse_expr(ast, lexer, false)?));
            }

            wrap_err!(ast, self.expect(lexer, TokenVariant::Semicolon));

            span.end = lexer.current_position();
            Ok(Node::new_with_span(
                Variant::VarDecl {
                    name,
                    var_type,
                    modifiable,
                    initialiser,
                },
                span,
            ))
        } else {
            ast.add_error(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!(
                        "Expected variable identifier, found {:?}",
                        self.curr_tok.variant
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
            Err(())
        }
    }

    fn parse_struct_decl_fields(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
    ) -> Result<Vec<(String, Node)>, ()> {
        let mut fields = Vec::new();

        let get_field = |ast: &mut Node, lexer: &mut Lexer, c: Token| {
            if let TokenVariant::Ident(name) = c.variant {
                Ok(name)
            } else {
                ast.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected identifier, found {:?}", c.variant),
                        span: CodeSpan {
                            start: c.span.start,
                            end: c.span.end,
                        },
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                });
                Err(())
            }
        };

        while matches!(self.curr_tok.variant, TokenVariant::Ident(_)) {
            let field = get_field(ast, lexer, self.curr_tok.clone())?;

            wrap_err!(ast, self.advance(lexer));
            wrap_err!(ast, self.expect(lexer, TokenVariant::Colon));

            let field_type = self.parse_ident(ast, lexer)?;
            wrap_err!(ast, self.expect(lexer, TokenVariant::Comma));

            fields.push((field, field_type));
        }

        Ok(fields)
    }

    fn parse_struct_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Struct));

        let ident = self.parse_ident(ast, lexer)?;

        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));
        let fields = self.parse_struct_decl_fields(ast, lexer)?;
        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::StructDecl {
                ident: Box::new(ident),
                fields,
            },
            span,
        ))
    }

    fn parse_func_param(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        let mut modifiable = false;

        if matches!(self.curr_tok.variant, TokenVariant::Mod) {
            modifiable = true;
            wrap_err!(ast, self.advance(lexer));
        }

        let name = match &self.curr_tok.variant {
            TokenVariant::Ident(name) => name.clone(),
            _ => {
                ast.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected identifier, found {:?}", self.curr_tok.variant),
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
        };

        wrap_err!(ast, self.advance(lexer));
        wrap_err!(ast, self.expect(lexer, TokenVariant::Colon));

        let field_type = self.parse_ident(ast, lexer)?;

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::VarDecl {
                name,
                var_type: Some(Box::new(field_type)),
                initialiser: None,
                modifiable,
            },
            span,
        ))
    }

    fn parse_interface_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Interface));

        let ident = Box::new(self.parse_ident(ast, lexer)?);
        wrap_err!(ast, self.advance(lexer));

        let mut functions = Vec::new();

        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        while matches!(self.curr_tok.variant, TokenVariant::Func) {
            functions.push(self.parse_func_decl(ast, lexer, false)?);
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::InterfaceDecl {
                ident,
                interface: functions,
            },
            span,
        ))
    }

    fn parse_ident_expr(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        self.parse_ident(ast, lexer)
    }

    fn parse_struct_literal(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Struct));

        let mut struct_ident = None;
        if matches!(self.curr_tok.variant, TokenVariant::DoubleColon) {
            wrap_err!(ast, self.advance(lexer));
            struct_ident = Some(Box::new(self.parse_ident(ast, lexer)?));
        }
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        let fields = self.parse_struct_literal_fields(ast, lexer)?;

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::Expr(ExprNode::StructLit {
                struct_ident,
                fields,
            }),
            span,
        ))
    }

    fn parse_struct_literal_fields(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
    ) -> Result<Vec<(String, Node)>, ()> {
        let mut fields = Vec::new();

        while let TokenVariant::Ident(field_name) = self.curr_tok.variant.clone() {
            wrap_err!(ast, self.advance(lexer));

            if matches!(self.curr_tok.variant, TokenVariant::Colon) {
                wrap_err!(ast, self.advance(lexer));

                let field_value = self.parse_expr(ast, lexer, false)?;
                fields.push((field_name, field_value));

                wrap_err!(ast, self.expect(lexer, TokenVariant::Comma));
            } else {
                fields.push((
                    field_name.clone(),
                    Node::new(Variant::Expr(ExprNode::QualifiedIdent {
                        namespaces: vec![],
                        name: field_name,
                        type_args: vec![],
                        memory_mode: MemoryMode::Auto,
                        id: None,
                    })),
                ));
            }
        }

        Ok(fields)
    }

    fn parse_impl_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);

        wrap_err!(ast, self.expect(lexer, TokenVariant::Impl));

        let mut interface_ident = None;
        let mut target_struct = Box::new(self.parse_ident(ast, lexer)?);

        if matches!(self.curr_tok.variant, TokenVariant::For) {
            wrap_err!(ast, self.advance(lexer));
            interface_ident = Some(target_struct);
            target_struct = Box::new(self.parse_ident(ast, lexer)?);
        }

        let mut methods = Vec::new();
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        if !matches!(self.curr_tok.variant, TokenVariant::Func) {
            ast.add_error(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Expected function declarations in implementation block".to_string(),
                    span: CodeSpan {
                        start: self.curr_tok.span.start,
                        end: self.curr_tok.span.end,
                    },
                    file: lexer.file.clone(),
                    err_type: DiagnosticMsgType::UnexpectedToken,
                },
                notes: vec![],
                hints: vec!["You might be missing a 'func' keyword.".to_string()],
            });
            return Err(());
        }

        while matches!(self.curr_tok.variant, TokenVariant::Func) {
            methods.push(self.parse_func_decl(ast, lexer, true)?);
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::ImplDecl {
                interface: interface_ident,
                target: target_struct,
                methods,
            },
            span,
        ))
    }

    fn parse_match_expr(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Match));

        let subject = Box::new(self.parse_expr(ast, lexer, false)?);
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        let mut cases = Vec::new();
        while self.curr_tok.variant != TokenVariant::RBrace {
            let pattern = self.parse_expr(ast, lexer, false)?;
            wrap_err!(ast, self.expect(lexer, TokenVariant::Arrow));

            if matches!(self.curr_tok.variant, TokenVariant::LBrace) {
                wrap_err!(ast, self.advance(lexer));

                let mut body = Vec::new();

                while self.curr_tok.variant != TokenVariant::RBrace {
                    body.push(self.parse_statement(ast, lexer, false)?);
                }

                wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));
                cases.push((pattern, Node::new(Variant::Block { children: body })));
            } else {
                let body = self.parse_expr(ast, lexer, false)?;
                cases.push((pattern, body));
            }
            wrap_err!(ast, self.expect(lexer, TokenVariant::Comma));
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::Expr(ExprNode::Match { subject, cases }),
            span,
        ))
    }

    fn parse_actor_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Actor));

        let ident = self.parse_ident(ast, lexer)?;
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        let fields = self.parse_struct_decl_fields(ast, lexer)?;

        let mut methods = Vec::new();
        while matches!(
            self.curr_tok.variant,
            TokenVariant::Func | TokenVariant::Async
        ) {
            methods.push(self.parse_func_decl(ast, lexer, true)?);
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::ActorDecl {
                ident: Box::new(ident),
                fields,
                methods,
            },
            span,
        ))
    }

    fn parse_actor_literal(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Actor));
        wrap_err!(ast, self.expect(lexer, TokenVariant::DoubleColon));

        let ident = self.parse_ident(ast, lexer)?;
        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        let fields = self.parse_struct_literal_fields(ast, lexer)?;

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::Expr(ExprNode::ActorLit {
                actor_ident: Box::new(ident),
                fields,
            }),
            span,
        ))
    }

    fn parse_enum_decl(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        wrap_err!(ast, self.expect(lexer, TokenVariant::Enum));

        let ident = self.parse_ident(ast, lexer)?;
        let mut variants = Vec::new();

        wrap_err!(ast, self.expect(lexer, TokenVariant::LBrace));

        while let TokenVariant::Ident(name) = self.curr_tok.variant.clone() {
            wrap_err!(ast, self.advance(lexer));
            match self.curr_tok.variant {
                TokenVariant::LParen => {
                    wrap_err!(ast, self.advance(lexer));
                    let variant_ident = self.parse_ident(ast, lexer)?;
                    wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));
                    variants.push((name, Some(variant_ident)));
                }
                TokenVariant::LBrace => {
                    wrap_err!(ast, self.advance(lexer));
                    let fields = self.parse_struct_literal_fields(ast, lexer)?;
                    wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));
                    variants.push((
                        name,
                        Some(Node::new(Variant::Expr(ExprNode::StructLit {
                            struct_ident: None,
                            fields,
                        }))),
                    ));
                }
                _ => {
                    variants.push((name, None));
                }
            }
            wrap_err!(ast, self.expect(lexer, TokenVariant::Comma));
        }

        wrap_err!(ast, self.expect(lexer, TokenVariant::RBrace));

        span.end = lexer.current_position();
        Ok(Node::new_with_span(
            Variant::EnumDecl {
                ident: Box::new(ident),
                variants,
            },
            span,
        ))
    }

    fn parse_expr(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        disallow_angles: bool,
    ) -> Result<Node, ()> {
        self.parse_prec(ast, lexer, 0, disallow_angles)
    }

    fn parse_expr_prefix(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
    ) -> Result<Option<UnaryOp>, ()> {
        match self.curr_tok.variant {
            TokenVariant::BitNot
            | TokenVariant::Minus
            | TokenVariant::Not
            | TokenVariant::Star
            | TokenVariant::Assign
            | TokenVariant::Await
            | TokenVariant::Block => {
                let op = match self.curr_tok.variant {
                    TokenVariant::Minus => UnaryOp::Minus,
                    TokenVariant::Not => UnaryOp::Not,
                    TokenVariant::BitNot => UnaryOp::BitNot,
                    TokenVariant::Star => UnaryOp::Deref,
                    TokenVariant::Await => UnaryOp::Await,
                    TokenVariant::Block => UnaryOp::Block,
                    TokenVariant::Assign => UnaryOp::FusedAssign,

                    _ => unreachable!(),
                };
                wrap_err!(ast, self.advance(lexer));
                Ok(Some(op))
            }
            _ => Ok(None),
        }
    }

    fn parse_prec(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        min_prec: u8,
        disallow_angles: bool,
    ) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);

        let prefix = self.parse_expr_prefix(ast, lexer)?;
        let mut lhs = self.parse_primary_expr(ast, lexer)?;

        lhs = self.parse_expr_postfix(ast, lexer, lhs)?;

        if let Some(prefix) = prefix {
            span.end = lexer.current_position();
            lhs = Node::new_with_span(
                Variant::Expr(ExprNode::UnaryOp {
                    op: prefix,
                    operand: Box::new(lhs),
                }),
                span.clone(),
            );
        }

        while let Some(op) = self.get_binary_op(&self.curr_tok.variant) {
            if disallow_angles && matches!(op, BinaryOp::LessThan | BinaryOp::GreaterThan) {
                break; // don't parse angle brackets as binary ops
                //hack for working around parse ambiguity with generics
                // quite shit
            }

            let prec = self.curr_tok.variant.precedence();
            if prec < min_prec {
                break;
            }

            wrap_err!(ast, self.advance(lexer));

            let rhs = self.parse_prec(ast, lexer, prec + 1, disallow_angles)?;

            span.end = lexer.current_position();
            lhs = Node::new_with_span(
                Variant::Expr(ExprNode::BinaryOp {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                }),
                span.clone(),
            );
        }

        while matches!(self.curr_tok.variant, TokenVariant::LBracket) {
            wrap_err!(ast, self.advance(lexer));
            let index = Box::new(self.parse_expr(ast, lexer, false)?);
            wrap_err!(ast, self.expect(lexer, TokenVariant::RBracket));

            span.end = lexer.current_position();
            lhs = Node::new_with_span(
                Variant::Expr(ExprNode::Index {
                    base: Box::new(lhs),
                    index,
                }),
                span.clone(),
            );
        }

        //lhs = self.parse_expr_postfix(ast, lexer, lhs)?;

        Ok(lhs)
    }

    fn parse_expr_postfix(
        &mut self,
        ast: &mut Node,
        lexer: &mut Lexer,
        parent: Node,
    ) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);
        let mut parent = parent;
        loop {
            match self.curr_tok.variant {
                TokenVariant::LBracket => {
                    wrap_err!(ast, self.advance(lexer));
                    let index = Box::new(self.parse_prec(ast, lexer, 0, false)?);
                    wrap_err!(ast, self.expect(lexer, TokenVariant::RBracket));

                    span.end = lexer.current_position();
                    parent = Node::new_with_span(
                        Variant::Expr(ExprNode::Index {
                            base: Box::new(parent),
                            index,
                        }),
                        span,
                    );
                }
                TokenVariant::LParen => {
                    wrap_err!(ast, self.advance(lexer));
                    let mut args = Vec::new();
                    if self.curr_tok.variant != TokenVariant::RParen {
                        args = self.parse_list(ast, lexer, false)?;
                    }
                    wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));

                    span.end = lexer.current_position();
                    parent = Node::new_with_span(
                        Variant::Expr(ExprNode::Call {
                            callee: Box::new(parent),
                            args,
                        }),
                        span,
                    );
                }
                TokenVariant::Dot => {
                    wrap_err!(ast, self.advance(lexer));
                    let field = self.parse_ident(ast, lexer)?;

                    span.end = lexer.current_position();
                    parent = Node::new_with_span(
                        Variant::Expr(ExprNode::FieldAccess {
                            base: Box::new(parent),
                            field: Box::new(field),
                        }),
                        span,
                    );
                }
                TokenVariant::QuestionMark => {
                    wrap_err!(ast, self.advance(lexer));

                    span.end = lexer.current_position();
                    parent = Node::new_with_span(
                        Variant::Expr(ExprNode::UnaryOp {
                            op: UnaryOp::BindMonad,
                            operand: Box::new(parent),
                        }),
                        span,
                    );
                }
                _ => {
                    break; // no more postfix operators
                }
            }
        }
        Ok(parent)
    }

    fn get_binary_op(&self, tok: &TokenVariant) -> Option<BinaryOp> {
        Some(match tok {
            TokenVariant::Plus => BinaryOp::Add,
            TokenVariant::Minus => BinaryOp::Subtract,
            TokenVariant::Star => BinaryOp::Multiply,
            TokenVariant::Slash => BinaryOp::Divide,
            TokenVariant::Percent => BinaryOp::Modulo,
            TokenVariant::Equiv => BinaryOp::Equiv,
            TokenVariant::NotEquiv => BinaryOp::NotEquiv,
            TokenVariant::LessThan => BinaryOp::LessThan,
            TokenVariant::GreaterThan => BinaryOp::GreaterThan,
            TokenVariant::GreaterThanEquiv => BinaryOp::GreaterThanEquiv,
            TokenVariant::LessThanEquiv => BinaryOp::LessThanEquiv,
            TokenVariant::BitAnd => BinaryOp::BitAnd,
            TokenVariant::BitOr => BinaryOp::BitOr,
            TokenVariant::BitXor => BinaryOp::BitXor,
            TokenVariant::BitNot => BinaryOp::BitNot,
            TokenVariant::And => BinaryOp::And,
            TokenVariant::Or => BinaryOp::Or,
            TokenVariant::Not => BinaryOp::Not,
            TokenVariant::Assign => BinaryOp::Assign,
            _ => {
                return None;
            }
        })
    }

    fn parse_ident_unqualified(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);

        let mut node = match &self.curr_tok.variant {
            TokenVariant::Ident(name) => Node::new(Variant::Expr(ExprNode::Ident {
                qualifier: false,
                name: name.clone(),
                type_args: Vec::new(),
            })),
            _ => {
                ast.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Expected identifier, found {:?}", self.curr_tok.variant),
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
        };

        wrap_err!(ast, self.advance(lexer));

        let set_qualifier = |node: &mut Node, value: bool| match node.variant {
            Variant::Expr(ExprNode::Ident {
                ref mut qualifier, ..
            }) => {
                *qualifier = value;
            }
            _ => unreachable!(),
        };

        let set_type_args = |node: &mut Node, type_args: Vec<Node>| match node.variant {
            Variant::Expr(ExprNode::Ident {
                type_args: ref mut args,
                ..
            }) => {
                *args = type_args;
            }
            _ => unreachable!(),
        };

        if matches!(self.curr_tok.variant, TokenVariant::DoubleColon) {
            wrap_err!(ast, self.advance(lexer));
            if !matches!(self.curr_tok.variant, TokenVariant::LessThan) {
                set_qualifier(&mut node, true);
                return Ok(node);
            } else {
                wrap_err!(ast, self.advance(lexer));
                set_type_args(&mut node, self.parse_list(ast, lexer, true)?);
                wrap_err!(ast, self.expect(lexer, TokenVariant::GreaterThan));
            }

            if matches!(self.curr_tok.variant, TokenVariant::DoubleColon) {
                set_qualifier(&mut node, true);
                wrap_err!(node, self.advance(lexer));
            }
        }

        span.end = lexer.current_position();
        node.span = span;
        Ok(node)
    }

    fn parse_ident(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut span = CodeSpan::new(lexer.current_position(), 0);

        let memory_mode = match self.curr_tok.variant {
            TokenVariant::Ref => {
                wrap_err!(ast, self.advance(lexer));
                MemoryMode::Ref
            }
            TokenVariant::Weak => {
                wrap_err!(ast, self.advance(lexer));
                MemoryMode::Weak
            }
            _ => MemoryMode::Auto,
        };

        let mut unqualifieds = Vec::new();
        while matches!(self.curr_tok.variant, TokenVariant::Ident(_)) {
            unqualifieds.push(self.parse_ident_unqualified(ast, lexer)?);
            if self.curr_tok.variant != TokenVariant::DoubleColon {
                break; // no more unqualified identifiers
            }
        }

        //
        //map idents to qualified idents
        //wildly wildly inefficient, copying a million things
        //but for the moment I don't care about performance too much
        //
        let mut name = String::new();
        let mut namespaces = Vec::new();
        let mut type_args: Vec<Node> = Vec::new();
        let mut warnings = None;
        let mut errors = None;

        for ident in unqualifieds.iter().rev() {
            if let Variant::Expr(ExprNode::Ident {
                name: ident_name,
                qualifier,
                type_args: ident_type_args,
            }) = &ident.variant
            {
                if *qualifier {
                    namespaces.push(ident.clone());
                } else {
                    name = ident_name.clone();
                    type_args = ident_type_args.clone();
                    warnings = ident.warnings.clone();
                    errors = ident.errors.clone();
                }
            } else {
                break;
            }
        }

        namespaces.reverse();

        span.end = lexer.current_position();
        Ok(Node {
            variant: Variant::Expr(ExprNode::QualifiedIdent {
                namespaces: namespaces,
                name,
                type_args,
                memory_mode,
                id: None,
            }),
            warnings,
            errors,
            span,
            export: false,
        })
    }

    fn parse_primary_expr(&mut self, ast: &mut Node, lexer: &mut Lexer) -> Result<Node, ()> {
        let mut advance = true;
        let node = match &self.curr_tok.variant {
            TokenVariant::LParen => {
                advance = false;
                wrap_err!(ast, self.advance(lexer));
                let expr = self.parse_expr(ast, lexer, false)?;
                wrap_err!(ast, self.expect(lexer, TokenVariant::RParen));
                expr
            }
            TokenVariant::LBracket => {
                let mut span = CodeSpan::new(lexer.current_position(), 0);

                advance = false;
                wrap_err!(ast, self.advance(lexer));
                let elements = self.parse_list(ast, lexer, false)?;
                wrap_err!(ast, self.expect(lexer, TokenVariant::RBracket));

                span.end = lexer.current_position();
                Node::new_with_span(Variant::Expr(ExprNode::ListLit { elements }), span)
            }
            TokenVariant::Directive => {
                advance = false;
                self.parse_directive(ast, lexer)?
            }
            TokenVariant::Match => {
                advance = false;
                self.parse_match_expr(ast, lexer)?
            }
            TokenVariant::Func | TokenVariant::Async => {
                advance = false;
                self.parse_func_decl(ast, lexer, true)?
            }
            TokenVariant::Ref | TokenVariant::Weak | TokenVariant::Ident(_) => {
                advance = false;
                self.parse_ident_expr(ast, lexer)?
            }
            TokenVariant::Struct => {
                advance = false;
                self.parse_struct_literal(ast, lexer)?
            }
            TokenVariant::Actor => {
                advance = false;
                self.parse_actor_literal(ast, lexer)?
            }
            TokenVariant::StringLit(s) => Node::new_with_span(
                Variant::Expr(ExprNode::String(s.clone())),
                self.curr_tok.span,
            ),
            TokenVariant::IntLit(i) => Node::new_with_span(
                Variant::Expr(ExprNode::IntLit(i.parse::<i64>().unwrap())),
                self.curr_tok.span,
            ),
            TokenVariant::FloatLit(f) => Node::new_with_span(
                Variant::Expr(ExprNode::FloatLit(f.parse::<f64>().unwrap())),
                self.curr_tok.span,
            ),
            TokenVariant::CharLit(c) => Node::new_with_span(
                Variant::Expr(ExprNode::CharLit(c.clone())),
                self.curr_tok.span,
            ),
            _ => {
                ast.add_error(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "Unexpected token in expression: {:?}",
                            self.curr_tok.variant
                        ),
                        span: self.curr_tok.span,
                        file: lexer.file.clone(),
                        err_type: DiagnosticMsgType::UnexpectedToken,
                    },
                    notes: vec![],
                    hints: vec!["Check your syntax.".to_string()],
                });
                wrap_err!(ast, self.advance(lexer));
                return Err(());
            }
        };

        if advance {
            wrap_err!(ast, self.advance(lexer));
        }

        Ok(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::Logger;

    #[allow(unused_macros)]
    macro_rules! create_comp_ctx {
        ($input:expr) => {{
            (
                Logger::dummy(),
                Lexer::new("test_file".to_string(), $input.to_string()),
                Node::new(Variant::Program { children: vec![] }),
            )
        }};
    }

    #[test]
    fn parse_sanity() -> Result<(), ()> {
        let source = "\
            @module my_module;

           struct MyStruct::<T> {
               field1: int32,
               field2: string,
           }

           impl MyStruct::<T> {
               func my_func(param: int32): String {
                   return \"Hello, World!\";
               }
           }

           actor Actor {
               x: int32,
               async func actor_func() {
                   for let mod i in range(0, 10) {
                       self.x += 12;
                   }
               }
           }

           enum MyEnum {
               Variant1,
               Variant2(i32),
               Variant3{ field: string, },
           }

           func main() {
               let mod x: MyStruct::<i32> = struct::MyStruct::<i32> {
                   field1: 42,
                   field2: \"test\",
               };

               let y = ref x;
               x.my_func(x.field1);

               let b = [12, 3, x.field1][0];

               let my_actor = actor::Actor {
                   x: 10,
               };

               block my_actor.actor_func();

               (func[ref x]() {
                   x.field1 += 1;
               })();
           }

        "
        .to_string();

        let mut ctx = CompileContext::new(Logger::dummy());

        let ast = match parse("test_file".to_string(), source, &mut ctx)? {
            Some(ast) => ast,
            None => {
                panic!("Parsing failed with no AST produced");
            }
        };

        assert!(
            ast.warnings().is_empty(),
            "Warnings found during parsing: {}",
            ast.warnings()
                .into_iter()
                .map(|w| w.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        assert!(
            ast.errors().is_empty(),
            "Errors found during parsing: {}",
            ast.errors()
                .into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        Ok(())
    }
}
