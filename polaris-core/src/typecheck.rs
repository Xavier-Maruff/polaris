use std::collections::HashMap;

use crate::{
    ast::ast::{BinaryOp, ExprNode, ForVariant, Node, TypeIdent, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    log::Logger,
    parse::CodeSpan,
    symbol::SymbolId,
    visit_ast_children,
};

pub fn typecheck_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut typecheck_ctx = TypecheckPassContext::new(ctx.logger.clone());
    let ret = typecheck_ctx.run_typecheck_pass(ctx);

    ctx.errors.extend(typecheck_ctx.errors);
    ctx.warnings.extend(typecheck_ctx.warnings);

    ret
}

/**
 * Uses a bodged Hindley-Milner-esque type inference system
 */

type TypeId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    pub body: Type,
    pub quantified_vars: Vec<TypeId>,
}

impl TypeScheme {
    pub fn new(body: Type, quantified_vars: Vec<TypeId>) -> Self {
        TypeScheme {
            body,
            quantified_vars,
        }
    }

    pub fn instantiate<T: TypeGenerator>(&self, type_gen: &mut T) -> Self {
        TypeScheme {
            body: self.body.with_fresh_vars(type_gen, &self.quantified_vars),
            quantified_vars: self.quantified_vars.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Never,
    Int32,
    Int64,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Bool,
    String,
    Char,
    Void,
    Lambda {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        symbol: SymbolId,
        fields: HashMap<String, Type>,
    },
    Actor {
        symbol: SymbolId,
        methods: Option<HashMap<String, Type>>,
        fields: HashMap<String, Type>,
    },
    List(Box<Type>),
    Tuple(Vec<Type>),
    Enum {
        symbol: SymbolId,
        variants: Option<HashMap<String, Type>>,
    },
    TypeVar(TypeId),
}

trait TypeGenerator {
    fn fresh_type_var(&mut self) -> TypeId;
}

impl Type {
    // replace all free type variables with new free type variables,
    // keep fixed type variables, as we want to don't want to re-generialise the type
    fn with_fresh_vars<T: TypeGenerator>(
        &self,
        type_gen: &mut T,
        quantified: &Vec<TypeId>,
    ) -> Self {
        let mut m = move |id: &TypeId| {
            if !quantified.contains(id) {
                type_gen.fresh_type_var()
            } else {
                id.clone()
            }
        };

        self.apply_subst_func(&mut m)
    }

    fn apply_subst_func<M: FnMut(&TypeId) -> TypeId>(&self, m: &mut M) -> Self {
        match self {
            Type::Lambda {
                params,
                return_type,
            } => Type::Lambda {
                params: params.iter().map(|t| t.apply_subst_func(m)).collect(),
                return_type: Box::new(return_type.apply_subst_func(m)),
            },

            Type::Struct { symbol, fields } => Type::Struct {
                symbol: *symbol,
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply_subst_func(m)))
                    .collect(),
            },

            Type::Actor {
                symbol,
                methods,
                fields,
            } => Type::Actor {
                symbol: *symbol,
                methods: match methods {
                    Some(methods) => Some(
                        methods
                            .iter()
                            .map(|(k, v)| (k.clone(), v.apply_subst_func(m)))
                            .collect(),
                    ),
                    None => None,
                },
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply_subst_func(m)))
                    .collect(),
            },

            Type::List(t) => Type::List(Box::new(t.apply_subst_func(m))),
            Type::Tuple(t) => Type::Tuple(t.iter().map(|t| t.apply_subst_func(m)).collect()),

            Type::Enum { symbol, variants } => Type::Enum {
                symbol: *symbol,
                variants: match variants {
                    Some(variants) => Some(
                        variants
                            .iter()
                            .map(|(k, v)| (k.clone(), v.apply_subst_func(m)))
                            .collect(),
                    ),
                    None => None,
                },
            },

            Type::TypeVar(id) => {
                let new_id = m(id);
                return Type::TypeVar(new_id);
            }

            //monotype primitives
            _ => self.clone(),
        }
    }
}

enum Constraint {
    Equality(Type, Type),
    Instance(Type, TypeScheme),
}

struct TypecheckPassContext {
    _logger: Logger,
    current_file: String,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    constraints: Vec<Constraint>,
    //could just use the symbol id and offset typeid gen by current symbol id counter
    symbol_type_vars: HashMap<SymbolId, TypeId>,
    parent_function: Option<SymbolId>,
    type_var_counter: usize,
}

impl TypeGenerator for TypecheckPassContext {
    fn fresh_type_var(&mut self) -> TypeId {
        self.type_var_counter += 1;
        self.type_var_counter
    }
}

impl TypecheckPassContext {
    fn new(logger: Logger) -> Self {
        Self {
            _logger: logger,
            current_file: String::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            constraints: Vec::new(),
            symbol_type_vars: HashMap::new(),
            type_var_counter: 0,
            parent_function: None,
        }
    }

    fn get_symbol_type_var(&mut self, symbol: SymbolId) -> TypeId {
        self.type_var_counter += 1;
        self.symbol_type_vars
            .entry(symbol)
            .or_insert_with(|| self.type_var_counter)
            .clone()
    }

    fn run_typecheck_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        let mut ret = Ok(());

        for (file, ast) in ctx.asts.iter_mut() {
            self.current_file = file.clone();
            ret = match self.inference_visitor(ast) {
                Ok(_) => ret,
                Err(_) => Err(()),
            };
        }

        ret
    }

    fn expr_type(&mut self, ast: &mut Node) -> Type {
        let mut expr = match &mut ast.variant {
            //todo: match on other nodes to unwrap effects like async, ref, etc.
            Variant::Expr(expr) => expr,
            _ => {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!(
                            "Cannot derive expression type for node variant '{}'",
                            ast.variant.user_friendly_name()
                        ),
                        span: ast.span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::Unimplemented,
                    },
                    notes: vec![],
                    hints: vec![],
                });
                return Type::Never;
            }
        };

        match &mut expr {
            ExprNode::Ident { id: Some(id), .. } => Type::TypeVar(self.get_symbol_type_var(*id)),
            ExprNode::String(_) => Type::String,
            ExprNode::CharLit(_) => Type::Char,
            //default to int64, will implement subtyping later??
            ExprNode::IntLit(_) => Type::Int64,
            ExprNode::FloatLit(_) => Type::Float64,
            ExprNode::TupleLit { elements } => {
                Type::Tuple(elements.iter_mut().map(|x| self.expr_type(x)).collect())
            }
            ExprNode::StructLit {
                struct_ident,
                fields,
            } => {
                let ident_type_var = match struct_ident {
                    Some(ident) => match ident.get_symbol_id() {
                        Some(symbol_id) => self.get_symbol_type_var(*symbol_id),
                        None => self.fresh_type_var(),
                    },
                    None => self.fresh_type_var(),
                };
                Type::Struct {
                    symbol: ident_type_var,
                    fields: fields
                        .iter_mut()
                        .map(|(name, node)| (name.clone(), self.expr_type(node)))
                        .collect(),
                }
            }
            ExprNode::ActorLit {
                actor_ident,
                fields,
            } => Type::Actor {
                symbol: actor_ident.get_symbol_id().unwrap_or(self.fresh_type_var()),
                methods: None,
                fields: fields
                    .iter_mut()
                    .map(|(name, node)| (name.clone(), self.expr_type(node)))
                    .collect(),
            },

            //todo: in desugaring map binops to interface calls
            //also add internal flag for commutative interfaces
            //todo: handle calls - will include all unary and bin ops after symbol rewrite
            //todo: sort field accessing
            _ => {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Unimplemented expression type for '{}'", expr),
                        span: ast.span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::Unimplemented,
                    },
                    notes: vec![],
                    hints: vec![],
                });
                Type::Never
            }
        }
    }

    fn visit_var_decl(
        &mut self,
        name: &String,
        var_type: &mut Option<Box<Node>>,
        id: &mut Option<SymbolId>,
        initialiser: &mut Option<Box<Node>>,
        span: CodeSpan,
    ) -> Result<(), ()> {
        if id.is_none() {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!("Undefined symbol '{}'", name),
                    span: span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                },
                notes: vec![],
                hints: vec![],
            });
            return Err(());
        }

        let symbol_type_var = self.get_symbol_type_var(id.unwrap());
        if let Some(node) = initialiser {
            let expr_type = self.expr_type(node);
            self.constraints.push(Constraint::Equality(
                Type::TypeVar(symbol_type_var),
                expr_type,
            ));
        }

        //if annotated with a type, add an instantiation constraint
        //binding symbol_type_var to the annotated type scheme
        if let Some(t) = var_type {
            let type_ids = t.get_symbol_ids();
            if type_ids.is_none() {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("No type scheme found for symbol '{}'", name),
                        span: span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndefinedType,
                    },
                    notes: vec![],
                    hints: vec![],
                });
                return Err(());
            }

            let type_scheme_symbols = t.get_symbol_ids();
            if type_scheme_symbols.is_none() {
                self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!(
                        "Could not get symbol IDs during typecheck of variable type instance decl"
                    ),
                    span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::Unimplemented,
                },
                notes: vec![],
                hints: vec![],
            });
                return Err(());
            }

            let type_scheme = self.get_type_ident_scheme(type_scheme_symbols.unwrap(), span)?;
            self.constraints.push(Constraint::Instance(
                Type::TypeVar(symbol_type_var),
                type_scheme,
            ))
        }

        Ok(())
    }

    fn get_type_ident_scheme(
        &mut self,
        ident: TypeIdent,
        span: CodeSpan,
    ) -> Result<TypeScheme, ()> {
        let (ret, _) = self.get_type_ident_scheme_internal(ident);
        if ret.is_none() {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!(
                        "Could not get symbol IDs during type ident scheme generation"
                    ),
                    span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::Unimplemented,
                },
                notes: vec![],
                hints: vec![],
            });
            return Err(());
        }

        Ok(ret.unwrap())
    }

    fn get_type_ident_scheme_internal(
        &mut self,
        ident: TypeIdent,
    ) -> (Option<TypeScheme>, Option<Vec<TypeId>>) {
        if ident.params.is_empty() {
            return (
                None,
                Some(vec![self.get_symbol_type_var(ident.body_symbol_id)]),
            );
        }

        let mut type_vars: Vec<TypeId> = ident
            .params
            .iter()
            .flat_map(|x| {
                self.get_type_ident_scheme_internal(x.clone())
                    .1
                    .unwrap_or(vec![])
            })
            .collect::<Vec<_>>();

        type_vars.dedup();

        return (
            Some(TypeScheme {
                body: Type::TypeVar(self.get_symbol_type_var(ident.body_symbol_id)),
                quantified_vars: type_vars,
            }),
            None,
        );
    }

    fn inference_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        match &mut ast.variant {
            Variant::VarDecl {
                name,
                var_type,
                modifiable,
                initialiser,
                id,
            } => self.visit_var_decl(name, var_type, id, initialiser, ast.span)?,
            _ => {}
        }

        visit_ast_children!(ast.variant, self, inference_visitor, {});
        Ok(())
    }
}
