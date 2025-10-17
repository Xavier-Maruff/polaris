use crate::{
    ast::{BinaryOp as BinOp, ExprKind, Node, NodeKind},
    compile::CompileContext,
};

pub fn desugar_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    DesugarContext::new().run_pass(compile_ctx)
}

struct DesugarContext {
    current_file: String,
}

impl DesugarContext {
    fn new() -> Self {
        Self {
            current_file: String::new(),
        }
    }

    fn run_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        let mut failed = false;
        for (_, module) in ctx.dependencies.modules.iter_mut() {
            self.current_file = module.file.clone();
            match self.visit_node(&mut module.ast, failed) {
                Ok(_) => {}
                Err(_) => {
                    failed = true;
                }
            }
        }

        if failed { Err(()) } else { Ok(()) }
    }

    fn visit_node(&mut self, node: &mut Node, failed: bool) -> Result<(), ()> {
        use ExprKind::*;
        use NodeKind::*;
        match &mut node.kind {
            Module { children } => {
                let mut failed = failed;
                for child in children.iter_mut() {
                    failed |= self.visit_node(child, failed).is_err();
                }
                ok((), failed)
            }

            FnDecl { expr, .. } if expr.is_some() => {
                self.visit_node(expr.as_mut().unwrap(), failed)
            }

            Expr { expr, .. } => match expr {
                //atm just desugaring pipeline operator
                //to consider: unify branch nodes here? if/else -> match?
                ExprKind::BinaryOp {
                    op: BinOp::Pipeline,
                    left,
                    right,
                } => {
                    self.visit_node(left, failed)?;
                    //maybe this shouldn't be here?
                    self.visit_node(right, failed)?;
                    // a |> b => b(a)
                    // a |> b(c) => b(a, c)

                    //todo: really don't want to clone here,
                    //but the bc must be sated for now
                    match &mut right.kind {
                        Expr {
                            expr: FnCall { args, .. },
                        } => {
                            args.insert(0, (None, *left.clone()));
                            *node = *right.clone();
                        }

                        _ => {
                            *node = Node {
                                kind: NodeKind::Expr {
                                    expr: ExprKind::FnCall {
                                        callee: right.clone(),
                                        args: vec![(None, *left.clone())],
                                    },
                                },
                                span: node.span.clone(),
                                symbol_id: right.symbol_id,
                                warnings: node.warnings.clone(),
                                errors: node.errors.clone(),
                                ty: node.ty.clone(),
                            };
                        }
                    }

                    ok((), failed)
                }

                LetBinding { expr, .. } => self.visit_node(expr, failed),
                Match { expr, arms, .. } => {
                    let mut failed = self.visit_node(expr, failed).is_err();
                    for arm in arms.iter_mut() {
                        failed |= self.visit_node(&mut arm.1, failed).is_err();
                    }
                    ok((), failed)
                }
                FnCall { args, .. } => {
                    let mut failed = failed;
                    for arg in args.iter_mut() {
                        failed |= self.visit_node(&mut arg.1, failed).is_err();
                    }
                    ok((), failed)
                }

                Closure { args, expr, .. } => {
                    let mut failed = failed;
                    for arg in args.iter_mut() {
                        failed |= self.visit_node(&mut arg.0, failed).is_err();
                    }
                    failed |= self.visit_node(expr, failed).is_err();
                    ok((), failed)
                }

                For {
                    start, end, body, ..
                } => {
                    let mut failed = self.visit_node(start, failed).is_err();
                    failed |= self.visit_node(end, failed).is_err();
                    failed |= self.visit_node(body, failed).is_err();
                    ok((), failed)
                }

                ListLit(elems) | TupleLit(elems) => {
                    let mut failed = failed;
                    for elem in elems.iter_mut() {
                        failed |= self.visit_node(elem, failed).is_err();
                    }
                    ok((), failed)
                }

                ListPattern(elements) => {
                    use crate::ast::ListPatternElement;
                    let mut failed = failed;
                    for elem in elements.iter_mut() {
                        match elem {
                            ListPatternElement::Element(node) => {
                                failed |= self.visit_node(node, failed).is_err();
                            }
                            ListPatternElement::Wildcard => {
                                //
                            }
                            ListPatternElement::Rest(Some(node)) => {
                                failed |= self.visit_node(node, failed).is_err();
                            }
                            ListPatternElement::Rest(None) => {
                                //
                            }
                        }
                    }
                    ok((), failed)
                }

                MapLit(entries) => {
                    let mut failed = failed;
                    for (key, value) in entries.iter_mut() {
                        failed |= self.visit_node(key, failed).is_err();
                        failed |= self.visit_node(value, failed).is_err();
                    }
                    ok((), failed)
                }

                FieldAccess { expr, .. } => self.visit_node(expr, failed),
                BinaryOp { left, right, .. } => {
                    let mut failed = self.visit_node(left, failed).is_err();
                    failed |= self.visit_node(right, failed).is_err();
                    ok((), failed)
                }

                IfElse {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let mut failed = self.visit_node(condition, failed).is_err();
                    failed |= self.visit_node(then_branch, failed).is_err();
                    if let Some(else_branch) = else_branch {
                        failed |= self.visit_node(else_branch, failed).is_err();
                    }
                    ok((), failed)
                }

                IndexAccess { expr, index, .. } => {
                    let mut failed = self.visit_node(expr, failed).is_err();
                    failed |= self.visit_node(index, failed).is_err();
                    ok((), failed)
                }

                Block(exprs) => {
                    let mut failed = failed;
                    for expr in exprs.iter_mut() {
                        failed |= self.visit_node(expr, failed).is_err();
                    }
                    ok((), failed)
                }

                _ => ok((), failed),
            },

            _ => ok((), failed),
        }
    }
}

fn ok<T>(val: T, failed: bool) -> Result<T, ()> {
    if failed { Err(()) } else { Ok(val) }
}
