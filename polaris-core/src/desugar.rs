use crate::{
    ast::{
        ast::{BinaryOp, ExprNode, ForVariant, Node, UnaryOp, Variant},
        pass::PassContext,
    },
    diagnostic::{Diagnostic, DiagnosticMsg},
    visit_ast_children,
};

pub fn desugar(ast: &mut Node, ctx: &mut PassContext) -> Result<(), ()> {
    visit(ast, ctx)
}

// enum DesugarCmd {
//     ReplaceBinOp { new_op: BinaryOp },
// }

pub fn visit_expr(expr: &mut ExprNode, ctx: &mut PassContext) -> Result<(), ()> {
    match expr {
        ExprNode::BinaryOp { lhs, rhs, op } => {
            if let Variant::Expr(ExprNode::UnaryOp {
                op: rhs_op,
                operand: rhs_operand,
            }) = &mut rhs.variant
            {
                if matches!(rhs_op, UnaryOp::FusedAssign) {
                    ctx.logger.debug(&format!(
                        "Desugaring fused assignment: {:?} {} {:?}",
                        lhs.variant, op, rhs_operand.variant
                    ));
                    //replace expr_node with a new binary operation a += b => a = a + b
                    //rewrite rhs from unary(fused_assign, rhs_operand) to binary(lhs, rhs, op)
                    match op {
                        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide
                        | BinaryOp::Modulo | BinaryOp::BitOr | BinaryOp::BitXor
                        | BinaryOp::BitAnd | BinaryOp::BitNot => {}
                        _ => {
                            lhs.add_error(Diagnostic{
                                primary: DiagnosticMsg {
                                    message: format!("Unsupported fused assignment operator: {:?}", op),
                                    file: ctx.file.clone(),
                                    span: lhs.span.clone(),
                                    err_type: crate::diagnostic::DiagnosticMsgType::UnexpectedToken,
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

pub fn visit(ast: &mut Node, ctx: &mut PassContext) -> Result<(), ()> {
    match ast.variant {
        Variant::Expr(ref mut expr) => {
            visit_expr(expr, ctx)?;
        }
        _ => {}
    };

    visit_ast_children!(ast.variant, ctx, visit, {});

    Ok(())
}
