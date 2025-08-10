use crate::{
    ast::ast::{BinaryOp, ExprNode, ForVariant, Node, UnaryOp, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg},
    visit_ast_children,
};

pub fn desugar_pass(ast: &mut Node, ctx: &mut CompileContext, file: &String) -> Result<(), ()> {
    let mut desugar_ctx = DesugarContext {
        file: file,
        ctx: ctx,
    };

    desugar_ctx.visit(ast)?;

    Ok(())
}

//internal

struct DesugarContext<'a> {
    file: &'a String,
    ctx: &'a mut CompileContext,
}

impl DesugarContext<'_> {
    fn visit_expr(&mut self, expr: &mut ExprNode) -> Result<(), ()> {
        match expr {
            ExprNode::BinaryOp { lhs, rhs, op } => {
                if let Variant::Expr(ExprNode::UnaryOp {
                    op: rhs_op,
                    operand: rhs_operand,
                }) = &mut rhs.variant
                {
                    if matches!(rhs_op, UnaryOp::FusedAssign) {
                        //replace expr_node with a new binary operation a += b => a = a + b
                        //rewrite rhs from unary(fused_assign, rhs_operand) to binary(lhs, rhs, op)
                        match op {
                        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide
                        | BinaryOp::Modulo | BinaryOp::BitOr | BinaryOp::BitXor
                        | BinaryOp::BitAnd | BinaryOp::BitNot => {}
                        _ => {
                            self.ctx.add_error(Diagnostic{
                                primary: DiagnosticMsg {
                                    message: format!("Unsupported fused assignment operator: {:?}", op),
                                    file: self.file.clone(),
                                    span: lhs.span,
                                    err_type: crate::diagnostic::DiagnosticMsgType::InvalidOperator,
                                },
                                notes: vec![],
                                hints: vec!["Only +=, -=, *=, /=, %=, |=, ^=, ~=, and &= are supported for fused assignment.".to_string()],
                            })
                        }
                    };

                        rhs.variant = Variant::Expr(ExprNode::BinaryOp {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs_operand.clone(),
                        });

                        *op = BinaryOp::Assign;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn visit(&mut self, ast: &mut Node) -> Result<(), ()> {
        match ast.variant {
            Variant::Expr(ref mut expr) => {
                self.visit_expr(expr)?;
            }
            _ => {}
        };

        visit_ast_children!(ast.variant, self, visit, {});

        Ok(())
    }
}
