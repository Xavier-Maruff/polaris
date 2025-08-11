//implemented as a macro so that passes can override default arms
//that otherwise just visit all children recursively
#[macro_export]
macro_rules! visit_ast_children {
    ($node:expr, $obj:expr, $visit:ident, { $($pat:pat => $body:expr),* $(,)? }) => {
        #[allow(unreachable_patterns)]
        match $node {
            $($pat => $body)*

            Variant::Program {
                ref mut children, ..
            } => {
                for child in children.iter_mut() {
                   $obj.$visit(child)?;
                }
            }

            /*Variant::Directive { ref mut args, .. } => {
                for arg in args.iter_mut() {
                    $obj.$visit(arg)?;
                }
            }*/

            Variant::Expr(ref mut expr) => {
                match expr {
                    ExprNode::Ident { type_args, .. } => {
                        for type_arg in type_args.iter_mut() {
                            $obj.$visit(type_arg)?;
                        }
                    }
                    ExprNode::BinaryOp { lhs, rhs, .. } => {
                        $obj.$visit(lhs)?;
                        $obj.$visit(rhs)?;
                    }
                    ExprNode::StructLit {struct_ident, fields, ..} => {
                        if let Some(ident) = struct_ident {
                            $obj.$visit(ident)?;
                        }
                        for (_, field) in fields.iter_mut() {
                            $obj.$visit(field)?;
                        }
                    }
                    ExprNode::ActorLit { actor_ident, fields, .. } => {
                        $obj.$visit(actor_ident)?;
                        for (_, field) in fields.iter_mut() {
                            $obj.$visit(field)?;
                        }
                    }
                    ExprNode::Call {callee, args, ..} => {
                        $obj.$visit(callee)?;
                        for arg in args.iter_mut() {
                            $obj.$visit(arg)?;
                        }
                    }
                    ExprNode::ListLit { elements, .. } => {
                        for element in elements.iter_mut() {
                            $obj.$visit(element)?;
                        }
                    }

                    ExprNode::Match { subject, cases, .. } => {
                        $obj.$visit(subject)?;
                        for (cond, arm) in cases.iter_mut() {
                            $obj.$visit(cond)?;
                            $obj.$visit(arm)?;
                        }
                    }
                    ExprNode::FieldAccess { base, .. } => {
                        $obj.$visit(base)?;
                    }
                    ExprNode::Index{base, index, ..} => {
                        $obj.$visit(base)?;
                        $obj.$visit(index)?;
                    }
                    ExprNode::UnaryOp { operand, .. } => {
                        $obj.$visit(operand)?;
                    }


                    _ => {}
                }
            }

            Variant::StructDecl { ref mut fields, .. } => {
                for (_, field) in fields.iter_mut() {
                    $obj.$visit(field)?;
                }
            }

            Variant::FuncDecl { ref mut ident, ref mut capture_list, ref mut params, ref mut return_type, ref mut body, .. } => {
                if let Some(ident) = ident {
                    $obj.$visit(ident)?;
                }
                if let Some(capture_list) = capture_list {
                    for param in capture_list.iter_mut() {
                        $obj.$visit(param)?;
                    }
                }
                for param in params.iter_mut() {
                    $obj.$visit(param)?;
                }
                if let Some(return_type) = return_type {
                    $obj.$visit(return_type)?;
                }
                if let Some(body) = body {
                    $obj.$visit(body)?;
                }
            }

            Variant::VarDecl { ref mut var_type, ref mut initialiser, .. } => {
                if let Some(var_type) = var_type {
                    $obj.$visit(var_type)?;
                }
                if let Some(initialiser) = initialiser {
                    $obj.$visit(initialiser)?;
                }
            }

            Variant::EnumDecl {ref mut ident, ref mut variants, ..} => {
                $obj.$visit(ident)?;
                for (_, variant) in variants.iter_mut() {
                   if let Some(inner) = variant {
                        $obj.$visit(inner)?;
                    }
                }
            }

            Variant::InterfaceDecl {ref mut ident, ref mut interface, ..} => {
                $obj.$visit(ident)?;
                for method in interface.iter_mut() {
                    $obj.$visit(method)?;
                }
            }
            Variant::ImplDecl {ref mut interface, ref mut target, ref mut methods, ..} => {
                if let Some(interface) = interface {
                    $obj.$visit(interface)?;
                }
                $obj.$visit(target)?;
                for method in methods.iter_mut() {
                    $obj.$visit(method)?;
                }
            }
            Variant::Block { ref mut children, .. } => {
                for child in children.iter_mut() {
                    $obj.$visit(child)?;
                }
            }

            Variant::Return { ref mut value, .. } => {
                if let Some(value) = value {
                    $obj.$visit(value)?;
                }
            }

            Variant::Yield { ref mut value, .. } => {
                if let Some(value) = value {
                    $obj.$visit(value)?;
                }
            }

            Variant::Assert { ref mut condition, ref mut messages, .. } => {
                $obj.$visit(condition)?;
                for message in messages.iter_mut() {
                    $obj.$visit(message)?;
                }
            }

            Variant::If { ref mut condition, ref mut then_branch, ref mut else_branch, .. } => {
                $obj.$visit(condition)?;
                $obj.$visit(then_branch)?;
                if let Some(else_branch) = else_branch {
                    $obj.$visit(else_branch)?;
                }
            }

            Variant::For { ref mut body, ref mut variant, ..} => {
                $obj.$visit(body)?;
                match variant {
                    ForVariant::ForWhile { condition } => {
                        $obj.$visit(condition)?;
                    }

                    ForVariant::ForIter { ident, iterable, .. } => {
                        $obj.$visit(ident)?;
                        $obj.$visit(iterable)?;
                    }
                    _ => {}
                }
            }

            Variant::TypeDecl { ref mut ident, ref mut alias_of, .. } => {
                $obj.$visit(ident)?;
                $obj.$visit(alias_of)?;
            }

            Variant::ActorDecl { ref mut ident, ref mut methods, ref mut fields, .. } => {
                $obj.$visit(ident)?;
                for method in methods.iter_mut() {
                    $obj.$visit(method)?;
                }
                for (_, field) in fields.iter_mut() {
                    $obj.$visit(field)?;
                }
            }

            _ => {}

        }
    };
}
