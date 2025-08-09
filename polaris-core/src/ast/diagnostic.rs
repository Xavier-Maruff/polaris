#[macro_export]
macro_rules! collect_diagnostics {
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
