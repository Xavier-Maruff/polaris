use crate::{diagnostic::Diagnostic, log::Logger};

pub type Pass<I, O> = fn(&mut I, &mut PassContext<I>) -> Result<Option<O>, ()>;

pub struct PassContext<N> {
    pub logger: Logger,
    pub file: String,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub ast: Option<N>,
}

impl<N> PassContext<N> {
    pub fn new(logger: Logger, file: String) -> Self {
        Self {
            logger,
            file,
            errors: Vec::new(),
            warnings: Vec::new(),
            ast: None,
        }
    }

    pub fn add_error(&mut self, err: Diagnostic) {
        self.errors.push(err);
    }

    pub fn add_warning(&mut self, warning: Diagnostic) {
        self.warnings.push(warning);
    }
}

//implemented as a macro so that passes can override default arms
//that otherwise just visit all children recursively
#[macro_export]
macro_rules! visit_ast_children {
    ($node:expr, $ctx:expr, $visit:expr, { $($pat:pat => $body:expr),* $(,)? }) => {

        #[allow(unreachable_patterns)]
        match $node {
            $($pat => $body)*

            Variant::Program {
                ref mut children, ..
            } => {
                for child in children.iter_mut() {
                    $visit(child, $ctx)?;
                }
            }

            Variant::Directive { ref mut args, .. } => {
                for arg in args.iter_mut() {
                    $visit(arg, $ctx)?;
                }
            }

            Variant::Expr(ref mut expr) => {
                match expr {
                    ExprNode::Ident { type_args, .. } => {
                        for type_arg in type_args.iter_mut() {
                            $visit(type_arg, $ctx)?;
                        }
                    }
                    ExprNode::QualifiedIdent { namespaces, type_args, .. } => {
                        for namespace in namespaces.iter_mut() {
                            $visit(namespace, $ctx)?;
                        }
                        for type_arg in type_args.iter_mut() {
                            $visit(type_arg, $ctx)?;
                        }
                    }
                    ExprNode::BinaryOp { lhs, rhs, .. } => {
                        $visit(lhs, $ctx)?;
                        $visit(rhs, $ctx)?;
                    }
                    ExprNode::StructLit {struct_ident, fields, ..} => {
                        if let Some(ident) = struct_ident {
                            $visit(ident, $ctx)?;
                        }
                        for (_, field) in fields.iter_mut() {
                            $visit(field, $ctx)?;
                        }
                    }
                    ExprNode::ActorLit { actor_ident, fields, .. } => {
                        $visit(actor_ident, $ctx)?;
                        for (_, field) in fields.iter_mut() {
                            $visit(field, $ctx)?;
                        }
                    }
                    ExprNode::Call {callee, args, ..} => {
                        $visit(callee, $ctx)?;
                        for arg in args.iter_mut() {
                            $visit(arg, $ctx)?;
                        }
                    }
                    ExprNode::ListLit { elements, .. } => {
                        for element in elements.iter_mut() {
                            $visit(element, $ctx)?;
                        }
                    }

                    ExprNode::Match { subject, cases, .. } => {
                        $visit(subject, $ctx)?;
                        for (cond, arm) in cases.iter_mut() {
                            $visit(cond, $ctx)?;
                            $visit(arm, $ctx)?;
                        }
                    }
                    ExprNode::FieldAccess { base, field, .. } => {
                        $visit(base, $ctx)?;
                        $visit(field, $ctx)?;
                    }
                    ExprNode::Index{base, index, ..} => {
                        $visit(base, $ctx)?;
                        $visit(index, $ctx)?;
                    }
                    ExprNode::UnaryOp { operand, .. } => {
                        $visit(operand, $ctx)?;
                    }

                    _ => {}
                }
            }

            Variant::StructDecl { ref mut fields, .. } => {
                for (_, field) in fields.iter_mut() {
                    $visit(field, $ctx)?;
                }
            }

            Variant::FuncDecl { ref mut ident, ref mut capture_list, ref mut params, ref mut return_type, ref mut body, .. } => {
                if let Some(ident) = ident {
                    $visit(ident, $ctx)?;
                }
                if let Some(capture_list) = capture_list {
                    for param in capture_list.iter_mut() {
                        $visit(param, $ctx)?;
                    }
                }
                for param in params.iter_mut() {
                    $visit(param, $ctx)?;
                }
                if let Some(return_type) = return_type {
                    $visit(return_type, $ctx)?;
                }
                if let Some(body) = body {
                    $visit(body, $ctx)?;
                }
            }

            Variant::VarDecl { ref mut var_type, ref mut initialiser, .. } => {
                if let Some(var_type) = var_type {
                    $visit(var_type, $ctx)?;
                }
                if let Some(initialiser) = initialiser {
                    $visit(initialiser, $ctx)?;
                }
            }

            Variant::EnumDecl {ref mut ident, ref mut variants, ..} => {
                $visit(ident, $ctx)?;
                for (_, variant) in variants.iter_mut() {
                   if let Some(inner) = variant {
                        $visit(inner, $ctx)?;
                    }
                }
            }

            Variant::InterfaceDecl {ref mut ident, ref mut interface, ..} => {
                $visit(ident, $ctx)?;
                for method in interface.iter_mut() {
                    $visit(method, $ctx)?;
                }
            }
            Variant::ImplDecl {ref mut interface, ref mut target, ref mut methods, ..} => {
                if let Some(interface) = interface {
                    $visit(interface, $ctx)?;
                }
                $visit(target, $ctx)?;
                for method in methods.iter_mut() {
                    $visit(method, $ctx)?;
                }
            }
            Variant::Block { ref mut children, .. } => {
                for child in children.iter_mut() {
                    $visit(child, $ctx)?;
                }
            }

            Variant::Return { ref mut value, .. } => {
                if let Some(value) = value {
                    $visit(value, $ctx)?;
                }
            }

            Variant::Yield { ref mut value, .. } => {
                if let Some(value) = value {
                    $visit(value, $ctx)?;
                }
            }

            Variant::Assert { ref mut condition, ref mut messages, .. } => {
                $visit(condition, $ctx)?;
                for message in messages.iter_mut() {
                    $visit(message, $ctx)?;
                }
            }

            Variant::If { ref mut condition, ref mut then_branch, ref mut else_branch, .. } => {
                $visit(condition, $ctx)?;
                $visit(then_branch, $ctx)?;
                if let Some(else_branch) = else_branch {
                    $visit(else_branch, $ctx)?;
                }
            }

            Variant::For { ref mut body, ref mut variant, ..} => {
                $visit(body, $ctx)?;
                match variant {
                    ForVariant::ForWhile { condition } => {
                        $visit(condition, $ctx)?;
                    }

                    ForVariant::ForIter { ident, iterable, .. } => {
                        $visit(ident, $ctx)?;
                        $visit(iterable, $ctx)?;
                    }
                    _ => {}
                }
            }

            Variant::TypeDecl { ref mut ident, ref mut alias_of, .. } => {
                $visit(ident, $ctx)?;
                $visit(alias_of, $ctx)?;
            }

            Variant::ActorDecl { ref mut ident, ref mut methods, ref mut fields, .. } => {
                $visit(ident, $ctx)?;
                for method in methods.iter_mut() {
                    $visit(method, $ctx)?;
                }
                for (_, field) in fields.iter_mut() {
                    $visit(field, $ctx)?;
                }
            }

            _ => {}

        }
    };
}
