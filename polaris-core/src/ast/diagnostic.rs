#[macro_export]
macro_rules! collect_node_diagnostics {
    ($self:expr, $diagnostics:ident, $method:ident) => {
        match &$self.variant {
            Variant::Expr(expr) => $diagnostics.extend(expr.$method()),

            Variant::StructDecl { fields, .. } => {
                for (_, field) in fields {
                    $diagnostics.extend(field.$method());
                }
            }

            Variant::FuncDecl {
                params,
                return_type,
                body,
                capture_list,
                ..
            } => {
                for param in params {
                    $diagnostics.extend(param.$method());
                }
                if let Some(ret) = return_type {
                    $diagnostics.extend(ret.$method());
                }
                if let Some(b) = body {
                    $diagnostics.extend(b.$method());
                }
                if let Some(capture_list) = capture_list {
                    for capture in capture_list {
                        $diagnostics.extend(capture.$method());
                    }
                }
            }

            Variant::VarDecl {
                var_type,
                initialiser,
                ..
            } => {
                if let Some(var_type) = var_type {
                    $diagnostics.extend(var_type.$method());
                }
                if let Some(initialiser) = initialiser {
                    $diagnostics.extend(initialiser.$method());
                }
            }

            Variant::EnumDecl { variants, .. } => {
                for (_, variant) in variants {
                    if let Some(inner) = variant {
                        $diagnostics.extend(inner.$method());
                    }
                }
            }

            Variant::InterfaceDecl { interface, .. } => {
                for method in interface {
                    $diagnostics.extend(method.$method());
                }
            }

            Variant::ImplDecl {
                interface,
                target,
                methods,
            } => {
                if let Some(interface) = interface {
                    $diagnostics.extend(interface.$method());
                }
                $diagnostics.extend(target.$method());
                for method in methods {
                    $diagnostics.extend(method.$method());
                }
            }

            Variant::Block { children } => {
                for child in children {
                    $diagnostics.extend(child.$method());
                }
            }

            Variant::If {
                condition,
                then_branch,
                else_branch,
            } => {
                $diagnostics.extend(condition.$method());
                $diagnostics.extend(then_branch.$method());
                if let Some(else_branch) = else_branch {
                    $diagnostics.extend(else_branch.$method());
                }
            }

            Variant::Assert {
                condition,
                messages,
            } => {
                $diagnostics.extend(condition.$method());
                for message in messages {
                    $diagnostics.extend(message.$method());
                }
            }

            Variant::For { variant, body } => {
                match variant {
                    ForVariant::ForWhile { condition } => {
                        $diagnostics.extend(condition.$method());
                    }
                    ForVariant::ForIter { ident, iterable } => {
                        $diagnostics.extend(ident.$method());
                        $diagnostics.extend(iterable.$method());
                    }
                    ForVariant::ForInfinite => {}
                }
                $diagnostics.extend(body.$method());
            }

            Variant::TypeDecl { ident, alias_of } => {
                $diagnostics.extend(ident.$method());
                $diagnostics.extend(alias_of.$method());
            }

            Variant::ActorDecl {
                ident,
                methods,
                fields,
            } => {
                $diagnostics.extend(ident.$method());
                for method in methods {
                    $diagnostics.extend(method.$method());
                }
                for (_, field) in fields {
                    $diagnostics.extend(field.$method());
                }
            }

            Variant::Return { value } => {
                if let Some(val) = value {
                    $diagnostics.extend(val.$method());
                }
            }

            Variant::Yield { value } => {
                if let Some(val) = value {
                    $diagnostics.extend(val.$method());
                }
            }

            _ => {}
        }
    };
}

/*
#[derive(Debug, Clone, PartialEq)]
pub enum ExprNode {
    String(String),
    Ident {
        name: String,
        qualifier: bool,
        type_args: Vec<Node>,
    },
    Ident {
        namespaces: Vec<String>,
        name: String,
        type_args: Vec<Node>,
        memory_mode: MemoryMode,
        id: Option<SymbolId>,
    },
    Directive {
        ident: Box<Node>,
        args: Vec<Node>,
    },
    IntLit(i64),
    FloatLit(f64),
    CharLit(String),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    StructLit {
        struct_ident: Option<Box<Node>>,
        fields: Vec<(String, Node)>,
    },
    ActorLit {
        actor_ident: Box<Node>,
        fields: Vec<(String, Node)>,
    },
    Call {
        callee: Box<Node>,
        args: Vec<Node>,
    },
    ListLit {
        elements: Vec<Node>,
    },
    Match {
        subject: Box<Node>,
        cases: Vec<(Node, Node)>,
    },
    FieldAccess {
        base: Box<Node>,
        field: Box<Node>,
    },
    Index {
        base: Box<Node>,
        index: Box<Node>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Node>,
    },
}
 */

#[macro_export]
macro_rules! collect_expr_diagnostics {
    ($self:expr, $diagnostics:ident, $method:ident) => {
        match &$self {
            ExprNode::Ident { type_args, .. } => {
                for type_arg in type_args.iter() {
                    $diagnostics.extend(type_arg.$method());
                }
            }

            ExprNode::BinaryOp { lhs, rhs, .. } => {
                $diagnostics.extend(lhs.$method());
                $diagnostics.extend(rhs.$method());
            }

            ExprNode::StructLit {
                struct_ident,
                fields,
                ..
            } => {
                if let Some(ident) = struct_ident {
                    $diagnostics.extend(ident.$method());
                }
                for (_, field) in fields.iter() {
                    $diagnostics.extend(field.$method());
                }
            }

            ExprNode::ActorLit {
                actor_ident,
                fields,
                ..
            } => {
                $diagnostics.extend(actor_ident.$method());
                for (_, field) in fields.iter() {
                    $diagnostics.extend(field.$method());
                }
            }

            ExprNode::Call { callee, args, .. } => {
                $diagnostics.extend(callee.$method());
                for arg in args.iter() {
                    $diagnostics.extend(arg.$method());
                }
            }

            ExprNode::ListLit { elements, .. } => {
                for element in elements.iter() {
                    $diagnostics.extend(element.$method());
                }
            }

            ExprNode::Match { subject, cases, .. } => {
                $diagnostics.extend(subject.$method());
                for (cond, arm) in cases.iter() {
                    $diagnostics.extend(cond.$method());
                    $diagnostics.extend(arm.$method());
                }
            }

            ExprNode::FieldAccess { base, .. } => {
                $diagnostics.extend(base.$method());
            }

            ExprNode::Index { base, index, .. } => {
                $diagnostics.extend(base.$method());
                $diagnostics.extend(index.$method());
            }

            ExprNode::UnaryOp { operand, .. } => {
                $diagnostics.extend(operand.$method());
            }

            _ => {}
        }
    };
}
