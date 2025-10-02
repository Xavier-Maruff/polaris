use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{INT, LIST, REAL, STRING, VOID, create_intrinsic_type_env},
    module::DepGraphContext,
    parse::CodeSpan,
    symbol::{SymbolContext, SymbolId},
};

pub fn typecheck_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let mut ctx = TypecheckContext::new(compile_ctx);
    ctx.typecheck()
}

// it's hindley-milner time hell yeah

struct TypecheckContext<'a> {
    type_env: TypeEnv,
    type_var_counter: TypeVar,
    errors: &'a mut Vec<Diagnostic>,
    warnings: &'a mut Vec<Diagnostic>,
    symbols: &'a mut SymbolContext,
    deps: &'a mut DepGraphContext,
    current_file: String,
    undetermined_symbol_counter: usize,
}

pub type TypeVar = usize;

pub struct TypeError {
    kind: TypeErrorKind,
    types: Vec<Ty>,
    hints: Vec<String>,
}

pub enum TypeErrorKind {
    UnificationFail,
    InfiniteType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Var(TypeVar),
    Concrete(SymbolId),
    //ty1 -> ty2
    Fn(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    //f(ty1, ty2, ...)
    Ctor(SymbolId, Vec<Ty>),
    //nocrypt ty
    Nocrypt(Box<Ty>),
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub bound_vars: Vec<TypeVar>,
    pub body: Ty,
}

#[derive(Clone, Debug)]
struct Substitution(HashMap<TypeVar, Ty>);

pub type TypeEnv = HashMap<SymbolId, Scheme>;

impl<'a> TypecheckContext<'a> {
    fn new(ctx: &'a mut CompileContext) -> Self {
        let undetermined_symbol_counter = ctx.symbols.symbol_idx.clone();

        Self {
            symbols: &mut ctx.symbols,
            type_env: HashMap::new(),
            type_var_counter: 0,
            errors: &mut ctx.errors,
            deps: &mut ctx.dependencies,
            warnings: &mut ctx.warnings,
            current_file: String::new(),
            undetermined_symbol_counter,
        }
    }

    pub fn typecheck(&mut self) -> Result<(), ()> {
        self.initialise_type_env();

        let sccs = self.deps.sccs.clone();
        for scc in &sccs {
            for module_id in scc {
                let (file_name, mut ast) = {
                    let module = self.deps.modules.get(module_id).ok_or(())?;
                    (module.file.clone(), module.ast.clone())
                };

                self.current_file = file_name;
                let type_env_clone = self.type_env.clone();

                match self.algo_w(&type_env_clone, &mut ast) {
                    Ok((subst, _ty)) => {
                        self.type_env = subst.apply_env(&self.type_env);
                    }
                    Err(_) => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!("Type checking failed for module {}", module_id),
                            span: ast.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        continue;
                    }
                }

                if let Some(module) = self.deps.modules.get_mut(module_id) {
                    module.ast = ast;
                }
            }
        }

        // for (symbol_id, scheme) in &self.type_env {
        //     if !self
        //         .symbols
        //         .intrinsic_types
        //         .values()
        //         .any(|&id| id == *symbol_id)
        //         && !self
        //             .symbols
        //             .intrinsic_symbols
        //             .values()
        //             .any(|&id| id == *symbol_id)
        //     {
        //         let rendered = scheme.body.render(self);
        //         println!("{}: {}", self.symbols.symbol_names[symbol_id], rendered)
        //     }
        // }

        Ok(())
    }

    fn algo_w(&mut self, env: &TypeEnv, node: &mut Node) -> Result<(Substitution, Ty), ()> {
        match &mut node.kind {
            NodeKind::Module { children } => {
                let mut subst = Substitution::new();
                let mut current_env = env.clone();
                let mut last_type = self.fresh_type_var();

                for child in children {
                    let (s, t) = self.algo_w(&current_env, child)?;
                    subst = subst.compose(&s);
                    current_env = subst.apply_env(&current_env);
                    last_type = subst.apply(&t);

                    self.add_declaration_to_env(child, &t, &mut current_env);
                }

                Ok((subst, last_type))
            }
            NodeKind::Expr { .. } => self.algo_w_expr(env, node),
            NodeKind::ConstDecl {
                const_type, expr, ..
            } => {
                let (s1, t1) = self.algo_w(env, expr)?;

                if let Some(type_node) = const_type {
                    let declared_type = self.type_from_node(type_node)?;
                    let s2 = self.unify(&s1.apply(&t1), &declared_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some(("Annotated type does not match inferred type".into(), vec![])),
                    )?;
                    let final_subst = s1.compose(&s2);
                    Ok((final_subst, s2.apply(&t1)))
                } else {
                    Ok((s1, t1))
                }
            }
            NodeKind::FnDecl {
                args,
                return_type,
                expr,
                ..
            } => {
                if let Some(body) = expr {
                    let mut arg_types = Vec::new();
                    let mut new_env = env.clone();

                    for (arg_pattern, arg_type, _) in args {
                        let arg_ty = if let Some(type_node) = arg_type {
                            self.type_from_node(type_node)?
                        } else {
                            self.fresh_type_var()
                        };
                        arg_types.push(arg_ty.clone());

                        if let NodeKind::Expr {
                            expr: ExprKind::Symbol { .. },
                        } = &arg_pattern.kind
                        {
                            if let Some(symbol_id) = arg_pattern.symbol_id {
                                new_env.insert(
                                    symbol_id,
                                    Scheme {
                                        bound_vars: vec![],
                                        body: arg_ty,
                                    },
                                );
                            }
                        }
                    }

                    let (s1, body_type) = self.algo_w(&new_env, body)?;

                    let final_return_type = if let Some(ret_type_node) = return_type {
                        let declared_ret_type = self.type_from_node(ret_type_node)?;
                        let s2 = self.unify(&s1.apply(&body_type), &declared_ret_type);
                        let s2 = self.bind_err_ctx(
                            s2,
                            node.span,
                            Some((
                                "Annotated return type does not match inferred type".into(),
                                vec![],
                            )),
                        )?;

                        let final_subst = s1.compose(&s2);
                        (final_subst, s2.apply(&body_type))
                    } else {
                        (s1, body_type)
                    };

                    let mut fn_type = final_return_type.1;

                    if arg_types.len() == 0 {
                        fn_type = Ty::Fn(
                            Box::new(Ty::Concrete(self.symbols.intrinsic_types[VOID])),
                            Box::new(fn_type),
                        );
                    }

                    for arg_type in arg_types.into_iter().rev() {
                        fn_type = Ty::Fn(
                            Box::new(final_return_type.0.apply(&arg_type)),
                            Box::new(fn_type),
                        );
                    }

                    Ok((final_return_type.0, fn_type))
                } else {
                    let mut arg_types = Vec::new();
                    for (_, arg_type, _) in args {
                        if let Some(type_node) = arg_type {
                            arg_types.push(self.type_from_node(type_node)?);
                        } else {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                                message: "Host function argument missing type annotation"
                                    .to_string(),
                                span: node.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::UndefinedType,
                            }));
                            return Err(());
                        }
                    }

                    let return_ty = if let Some(ret_type_node) = return_type {
                        self.type_from_node(ret_type_node)?
                    } else {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: "Host function missing return type annotation".to_string(),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UndefinedType,
                        }));

                        return Err(());
                    };

                    let mut fn_type = return_ty;
                    for arg_type in arg_types.into_iter().rev() {
                        fn_type = Ty::Fn(Box::new(arg_type), Box::new(fn_type));
                    }

                    Ok((Substitution::new(), fn_type))
                }
            }
            _ => Ok((Substitution::new(), self.fresh_type_var())),
        }
    }

    fn bind_err_ctx<T>(
        &mut self,
        result: Result<T, TypeError>,
        span: CodeSpan,
        msg: Option<(String, Vec<String>)>,
    ) -> Result<T, ()> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => {
                let rendered = e.types.iter().map(|t| t.render(self)).collect::<Vec<_>>();

                //this is wildly inefficient but only runs on errors so who really cares ig

                let (message, hints) = match msg {
                    Some((mut m, mut hints)) => {
                        for i in 0..rendered.len() {
                            let source_str = format!("${}", i + 1);
                            let target_str = format!("{}", rendered[i]);
                            m = m.replace(&source_str, &target_str);
                            hints = hints
                                .iter()
                                .map(|h| h.replace(&source_str, &target_str))
                                .collect();
                        }
                        hints.extend(e.hints);
                        (m, hints)
                    }
                    None => match e.kind {
                        TypeErrorKind::UnificationFail => (
                            format!(
                                "Type mismatch: could not unify types: {}",
                                rendered.join(", ")
                            ),
                            e.hints,
                        ),
                        TypeErrorKind::InfiniteType => (
                            format!(
                                "Infinite type detected involving types: {}",
                                rendered.join(", "),
                            ),
                            e.hints,
                        ),
                    },
                };

                let mut e = Diagnostic::new(DiagnosticMsg {
                    message,
                    span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::TypeMismatch,
                });

                for hint in hints {
                    e.add_hint(hint)
                }

                self.errors.push(e);

                Err(())
            }
        }
    }

    fn algo_w_expr(&mut self, env: &TypeEnv, node: &mut Node) -> Result<(Substitution, Ty), ()> {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => unreachable!(),
        };

        match expr {
            ExprKind::IntLit(_) => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types[INT]),
            )),
            ExprKind::RealLit { .. } => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types[REAL]),
            )),
            ExprKind::StringLit(_) => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types[STRING]),
            )),

            ExprKind::Symbol { name } => {
                if let Some(symbol_id) = node.symbol_id {
                    if let Some(scheme) = env.get(&symbol_id) {
                        let ty = scheme.clone().instantiate(&mut self.type_var_counter);
                        Ok((Substitution::new(), ty))
                    } else {
                        Ok((Substitution::new(), self.fresh_type_var()))
                    }
                } else {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!("Unresolved symbol: {}", name),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndeclaredSymbol,
                    }));
                    Err(())
                }
            }

            ExprKind::TupleLit(elements) => {
                let mut subst = Substitution::new();
                let mut types = Vec::new();
                let mut current_env = env.clone();

                for elem in elements {
                    let (s, t) = self.algo_w(&current_env, elem)?;
                    subst = subst.compose(&s);
                    current_env = subst.apply_env(&current_env);
                    types.push(subst.apply(&t));
                }

                Ok((subst, Ty::Tuple(types)))
            }

            ExprKind::ListLit(elements) => {
                if elements.is_empty() {
                    let elem_type = self.fresh_type_var();
                    Ok((
                        Substitution::new(),
                        Ty::Ctor(self.symbols.intrinsic_types[LIST], vec![elem_type]),
                    ))
                } else {
                    let (s1, t1) = self.algo_w(env, &mut elements[0])?;
                    let mut subst = s1;
                    let mut current_env = subst.apply_env(env);
                    let mut elem_type = subst.apply(&t1);

                    for elem in elements.iter_mut().skip(1) {
                        let (s, t) = self.algo_w(&current_env, elem)?;
                        let s2 = self.unify(&subst.apply(&elem_type), &s.apply(&t));
                        let s2 = self.bind_err_ctx(
                            s2,
                            elem.span,
                            Some(("List element type mismatch".into(), vec![])),
                        )?;
                        subst = subst.compose(&s).compose(&s2);
                        current_env = subst.apply_env(&current_env);
                        elem_type = subst.apply(&elem_type);
                    }

                    Ok((
                        subst,
                        Ty::Ctor(self.symbols.intrinsic_types[LIST], vec![elem_type]),
                    ))
                }
            }

            ExprKind::FnCall { callee, args } => {
                let (s1, callee_type) = self.algo_w(env, callee)?;
                let env1 = s1.apply_env(env);

                let mut subst = s1;
                let mut current_env = env1;
                let mut arg_types = Vec::new();

                for (_, arg) in args {
                    let (s, t) = self.algo_w(&current_env, arg)?;
                    subst = subst.compose(&s);
                    current_env = subst.apply_env(&current_env);
                    arg_types.push(subst.apply(&t));
                }

                let return_type = self.fresh_type_var();
                let mut expected_fn_type = return_type.clone();

                for arg_type in arg_types.into_iter().rev() {
                    expected_fn_type = Ty::Fn(Box::new(arg_type), Box::new(expected_fn_type));
                }

                let s_unify = self.unify(&subst.apply(&callee_type), &expected_fn_type);
                let s_unify = self.bind_err_ctx(
                    s_unify,
                    node.span,
                    Some((
                        "Function argument types do not match function signature".into(),
                        vec![],
                    )),
                )?;
                let final_subst = subst.compose(&s_unify);

                Ok((final_subst, s_unify.apply(&return_type)))
            }

            ExprKind::BinaryOp { left, op, right } => {
                let (s1, t1) = self.algo_w(env, left)?;
                let env1 = s1.apply_env(env);
                let (s2, t2) = self.algo_w(&env1, right)?;
                let s3 = s1.compose(&s2);

                use crate::ast::BinaryOp::*;
                match op {
                    Add | Subtract | Multiply | Divide | Modulus | Exponent => {
                        let s4 = self.unify(&s3.apply(&t1), &s3.apply(&t2));
                        let s4 = self.bind_err_ctx(
                            s4,
                            node.span,
                            Some((
                                    "Operands of arithmetic operations must be of the same type - tried to equate $1 and $2.".into(),
                                    vec![]
                            )),
                        )?;
                        let final_subst = s3.compose(&s4);
                        Ok((final_subst, s4.apply(&s3.apply(&t1))))
                    }
                    Equal | NotEqual | LessThan | LessThanEquiv | GreaterThan
                    | GreaterThanEquiv => {
                        let s4 = self.unify(&s3.apply(&t1), &s3.apply(&t2));
                        let s4 = self.bind_err_ctx(
                            s4,
                            node.span,
                            Some((
                                "Operands of comparison operations must be of the same type - tried to equate $1 and $2.".into(),
                                vec![],
                            )),
                        )?;
                        let final_subst = s3.compose(&s4);
                        Ok((final_subst, self.fresh_type_var()))
                    }
                    And | Or => {
                        let bool_type = self.fresh_type_var();
                        let s4 = self.unify(&s3.apply(&t1), &bool_type);
                        let s4 = self.bind_err_ctx(
                            s4,
                            node.span,
                            Some((
                                "Left operand of logical operation must be boolean".into(),
                                vec![],
                            )),
                        )?;
                        let s5 = self.unify(&s4.apply(&s3.apply(&t2)), &s4.apply(&bool_type));
                        let s5 = self.bind_err_ctx(
                            s5,
                            node.span,
                            Some((
                                "Right operand of logical operation must be boolean".into(),
                                vec![],
                            )),
                        )?;
                        let final_subst = s3.compose(&s4).compose(&s5);
                        Ok((final_subst, s5.apply(&s4.apply(&bool_type))))
                    }
                    _ => {
                        let s4 = self.unify(&s3.apply(&t1), &s3.apply(&t2));
                        let s4 = self.bind_err_ctx(
                            s4,
                            node.span,
                            Some(("Operands must be of the same type".into(), vec![])),
                        )?;
                        let final_subst = s3.compose(&s4);
                        Ok((final_subst, s4.apply(&s3.apply(&t1))))
                    }
                }
            }

            ExprKind::UnaryOp { expr: operand, .. } => {
                let (s, t) = self.algo_w(env, operand)?;
                Ok((s, t))
            }

            //todo: this is pretty iffy - not sure whether let bindings should be void of the type of the expr?
            ExprKind::LetBinding {
                symbol_type,
                expr: value_expr,
                ..
            } => {
                let (s1, t1) = self.algo_w(env, value_expr)?;

                let final_type = if let Some(type_node) = symbol_type {
                    let declared_type = self.type_from_node(type_node)?;
                    let s2 = self.unify(&s1.apply(&t1), &declared_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some(("Annotated type does not match inferred type".into(), vec![])),
                    )?;
                    let final_subst = s1.compose(&s2);
                    //(final_subst, s2.apply(&t1))
                    (
                        final_subst,
                        Ty::Concrete(self.symbols.intrinsic_types[VOID]),
                    )
                } else {
                    //(s1, t1)
                    (s1, Ty::Concrete(self.symbols.intrinsic_types[VOID]))
                };

                Ok(final_type)
            }

            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let (s1, _cond_type) = self.algo_w(env, condition)?;
                let env1 = s1.apply_env(env);

                let (s2, then_type) = self.algo_w(&env1, then_branch)?;
                let s3 = s1.compose(&s2);
                let env2 = s3.apply_env(&env1);

                if let Some(else_expr) = else_branch {
                    let (s4, else_type) = self.algo_w(&env2, else_expr)?;
                    let s5 = self.unify(&s3.apply(&then_type), &s4.apply(&else_type));
                    let s5 = self.bind_err_ctx(
                        s5,
                        node.span,
                        Some(("Then and else branch types do not match".into(), vec![])),
                    )?;
                    let final_subst = s3.compose(&s4).compose(&s5);
                    Ok((final_subst, s5.apply(&s4.apply(&else_type))))
                } else {
                    Ok((s3, then_type))
                }
            }

            ExprKind::Block(statements) => {
                if statements.is_empty() {
                    Ok((Substitution::new(), self.fresh_type_var()))
                } else {
                    let mut subst = Substitution::new();
                    let mut current_env = env.clone();
                    let mut last_type = self.fresh_type_var();

                    for stmt in statements {
                        let (s, t) = self.algo_w(&current_env, stmt)?;
                        subst = subst.compose(&s);
                        current_env = subst.apply_env(&current_env);
                        last_type = subst.apply(&t);
                    }

                    Ok((subst, last_type))
                }
            }

            ExprKind::Closure {
                args,
                return_type,
                expr: body,
            } => {
                let mut arg_types = Vec::new();
                let mut new_env = env.clone();

                for (arg_pattern, arg_type, _) in args {
                    let arg_ty = if let Some(type_node) = arg_type {
                        self.type_from_node(type_node)?
                    } else {
                        self.fresh_type_var()
                    };
                    arg_types.push(arg_ty.clone());

                    if let NodeKind::Expr {
                        expr: ExprKind::Symbol { .. },
                    } = &arg_pattern.kind
                    {
                        if let Some(symbol_id) = arg_pattern.symbol_id {
                            new_env.insert(
                                symbol_id,
                                Scheme {
                                    bound_vars: vec![],
                                    body: arg_ty,
                                },
                            );
                        }
                    }
                }

                let (s1, body_type) = self.algo_w(&new_env, body)?;

                let final_return_type = if let Some(ret_type_node) = return_type {
                    let declared_ret_type = self.type_from_node(ret_type_node)?;
                    let s2 = self.unify(&s1.apply(&body_type), &declared_ret_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some((
                            "Annotated return type does not match inferred type".into(),
                            vec![],
                        )),
                    )?;
                    let final_subst = s1.compose(&s2);
                    (final_subst, s2.apply(&body_type))
                } else {
                    (s1, body_type)
                };

                let mut fn_type = final_return_type.1;
                for arg_type in arg_types.into_iter().rev() {
                    fn_type = Ty::Fn(
                        Box::new(final_return_type.0.apply(&arg_type)),
                        Box::new(fn_type),
                    );
                }

                Ok((final_return_type.0, fn_type))
            }

            ExprKind::Discard => Ok((Substitution::new(), self.fresh_type_var())),

            _ => Ok((Substitution::new(), self.fresh_type_var())),
        }
    }

    fn initialise_type_env(&mut self) {
        //add intrinsics
        self.type_env = create_intrinsic_type_env(self.symbols, &mut self.type_var_counter);
    }

    fn type_from_node(&mut self, node: &Node) -> Result<Ty, ()> {
        match &node.kind {
            NodeKind::Type {
                type_vars,
                symbol,
                nocrypt,
                ..
            } => {
                let ok = |a| {
                    if *nocrypt {
                        Ok(Ty::Nocrypt(Box::new(a)))
                    } else {
                        Ok(a)
                    }
                };

                let symbol_id = if let Some(id) = node.symbol_id {
                    Some(id)
                } else {
                    self.symbols.intrinsic_types.get(symbol.as_str()).cloned()
                };

                if let Some(symbol_id) = symbol_id {
                    if type_vars.is_empty() {
                        //starts lowercase, must be a type param
                        if symbol.chars().next().map_or(false, |c| c.is_lowercase()) {
                            //maybe lookup in env?
                            return ok(self.fresh_type_var());
                        }

                        ok(Ty::Concrete(symbol_id))
                    } else {
                        let mut args = Vec::new();
                        for type_var in type_vars {
                            args.push(self.type_from_node(type_var)?);
                        }
                        ok(Ty::Ctor(symbol_id, args))
                    }
                } else {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!(
                            "Unresolved type with id {:?}: {:?}",
                            node.symbol_id, node.kind
                        ),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndefinedType,
                    }));
                    Err(())
                }
            }
            NodeKind::FnType { args, return_type } => {
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.type_from_node(arg)?);
                }

                let ret_type = if let Some(ret) = return_type {
                    self.type_from_node(ret)?
                } else {
                    self.fresh_type_var()
                };

                let mut fn_type = ret_type;
                for arg_type in arg_types.into_iter().rev() {
                    fn_type = Ty::Fn(Box::new(arg_type), Box::new(fn_type));
                }

                Ok(fn_type)
            }
            NodeKind::TupleType { elements } => {
                let mut types = Vec::new();
                for elem in elements {
                    types.push(self.type_from_node(elem)?);
                }
                Ok(Ty::Tuple(types))
            }
            _ => {
                self.errors.push(Diagnostic::new(DiagnosticMsg {
                    message: format!("Invalid type node: {:?}", node.kind),
                    span: node.span.clone(),
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UndefinedType,
                }));
                Err(())
            }
        }
    }

    fn fresh_type_var(&mut self) -> Ty {
        fresh_type_var(&mut self.type_var_counter)
    }

    fn add_declaration_to_env(&mut self, node: &Node, ty: &Ty, env: &mut TypeEnv) {
        match &node.kind {
            NodeKind::ConstDecl { symbol, .. } => {
                if let NodeKind::Expr {
                    expr: ExprKind::Symbol { .. },
                } = &symbol.kind
                {
                    if let Some(symbol_id) = symbol.symbol_id {
                        let scheme = if self.is_nonexpansive_node(node) {
                            Scheme::generalise(env, ty)
                        } else {
                            Scheme {
                                bound_vars: vec![],
                                body: ty.clone(),
                            }
                        };
                        env.insert(symbol_id, scheme);
                        self.type_env
                            .insert(symbol_id, env.get(&symbol_id).unwrap().clone());
                    }
                }
            }
            NodeKind::FnDecl { .. } => {
                if let Some(symbol_id) = node.symbol_id {
                    let scheme = Scheme::generalise(env, ty);
                    env.insert(symbol_id, scheme);
                    self.type_env
                        .insert(symbol_id, env.get(&symbol_id).unwrap().clone());
                }
            }
            _ => {}
        }
    }

    fn is_nonexpansive_node(&self, node: &Node) -> bool {
        match &node.kind {
            NodeKind::Expr { expr } => self.is_nonexpansive(expr),
            NodeKind::ConstDecl { expr, .. } => {
                if let NodeKind::Expr { expr } = &expr.kind {
                    self.is_nonexpansive(expr)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn is_nonexpansive(&self, e: &ExprKind) -> bool {
        match e {
            ExprKind::IntLit(_)
            | ExprKind::RealLit { .. }
            | ExprKind::StringLit(_)
            | ExprKind::Discard => true,
            ExprKind::TupleLit(xs) | ExprKind::ListLit(xs) => xs
                .iter()
                .all(|n| matches!(&n.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))),
            ExprKind::MapLit(kvs) => kvs.iter().all(|(k, v)| {
                matches!(&k.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
                    && matches!(&v.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
            }),
            ExprKind::Closure { .. } => true,
            ExprKind::Symbol { .. } => true,
            _ => false,
        }
    }

    fn occurs(&mut self, a: TypeVar, t: &Ty) -> bool {
        use Ty::*;
        match t {
            Var(b) => a == *b,
            Fn(arg, ret) => self.occurs(a, arg) || self.occurs(a, ret),
            Ctor(_, args) | Tuple(args) => args.iter().any(|arg| self.occurs(a, arg)),
            Concrete(_) => false,
            Nocrypt(t) => self.occurs(a, t),
        }
    }

    fn bind(&mut self, a: TypeVar, t: &Ty) -> Result<Substitution, TypeError> {
        if let Ty::Var(b) = t
            && *b == a
        {
            return Ok(Substitution::new());
        }

        if self.occurs(a, t) {
            return Err(TypeError {
                kind: TypeErrorKind::InfiniteType,
                types: vec![Ty::Var(a), t.clone()],
                hints: vec![],
            });
        }

        let mut m = HashMap::new();
        m.insert(a, t.clone());

        Ok(Substitution(m))
    }

    fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<Substitution, TypeError> {
        use Ty::*;
        match (t1, t2) {
            (a, b) if a == b => Ok(Substitution::new()),

            (Var(a), Var(b)) if a == b => Ok(Substitution::new()),
            (Var(a), _) => self.bind(*a, t2),
            (_, Var(b)) => self.bind(*b, t1),

            (Concrete(a), Concrete(b)) if a == b => Ok(Substitution::new()),

            (Fn(arg1, ret1), Fn(arg2, ret2)) => {
                let s1 = self.unify(arg1, arg2)?;
                let s2 = self.unify(&s1.apply(ret1), &s1.apply(ret2))?;
                Ok(s1.compose(&s2))
            }

            (Nocrypt(a), Nocrypt(b)) => self.unify(a, b),
            (Nocrypt(_), _) | (_, Nocrypt(_)) => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec!["Attempted to mix usage of encrypted and 'nocrypt' types - remove/add 'nocrypt' type modifiers to fix.".into()],
            }),

            (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => {
                let mut s = Substitution::new();
                for (ty1, ty2) in types1.iter().zip(types2.iter()) {
                    let s2 = self.unify(&s.apply(ty1), &s.apply(ty2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            (Ctor(id1, args1), Ctor(id2, args2)) if id1 == id2 && args1.len() == args2.len() => {
                let mut s = Substitution::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let s2 = self.unify(&s.apply(arg1), &s.apply(arg2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            _ => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec![],
            }),
        }
    }
}

impl Ty {
    fn free_type_vars(&self) -> HashSet<TypeVar> {
        use Ty::*;
        match self {
            Var(a) => {
                let mut set = HashSet::new();
                set.insert(*a);
                set
            }
            Fn(arg, ret) => {
                let mut set = arg.free_type_vars();
                set.extend(ret.free_type_vars());
                set
            }
            Tuple(types) => {
                let mut set = HashSet::new();
                for ty in types {
                    set.extend(ty.free_type_vars());
                }
                set
            }
            Ctor(_, args) => {
                let mut set = HashSet::new();
                for arg in args {
                    set.extend(arg.free_type_vars());
                }
                set
            }
            Concrete(_) => HashSet::new(),
            Nocrypt(t) => t.free_type_vars(),
        }
    }

    fn free_type_vars_in_scheme(scheme: &Scheme) -> HashSet<TypeVar> {
        let mut set = scheme.body.free_type_vars();
        for var in &scheme.bound_vars {
            set.remove(var);
        }
        set
    }

    fn free_type_vars_in_env(env: &TypeEnv) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        for scheme in env.values() {
            set.extend(Ty::free_type_vars_in_scheme(scheme));
        }
        set
    }

    fn render(&self, ctx: &TypecheckContext) -> String {
        use Ty::*;
        match self {
            Var(a) => format!("t{}", a),
            Concrete(id) => ctx
                .symbols
                .intrinsic_types
                .iter()
                .find(|(_, sym_id)| *sym_id == id)
                .map_or("<unknown>".to_string(), |(name, _)| name.clone()),
            Fn(arg, ret) => format!("({} -> {})", arg.render(ctx), ret.render(ctx)),
            Tuple(types) => {
                let elems: Vec<String> = types.iter().map(|ty| ty.render(ctx)).collect();
                format!("({})", elems.join(", "))
            }
            Nocrypt(t) => format!("nocrypt {}", t.render(ctx)),
            Ctor(id, args) => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.render(ctx)).collect();

                format!(
                    "{}({})",
                    ctx.symbols
                        .symbol_names
                        .get(id)
                        .cloned()
                        .unwrap_or("<unknown>".to_string()),
                    args_str.join(", ")
                )
            }
        }
    }
}

impl Scheme {
    fn instantiate(self, counter: &mut TypeVar) -> Ty {
        let mut subst = Substitution::new();
        for &var in &self.bound_vars {
            subst.0.insert(var, fresh_type_var(counter));
        }
        subst.apply(&self.body)
    }

    fn generalise(env: &TypeEnv, t: &Ty) -> Scheme {
        let env_vars = Ty::free_type_vars_in_env(env);
        let type_vars = t.free_type_vars();
        let bound_vars: Vec<_> = type_vars.difference(&env_vars).cloned().collect();
        Scheme {
            bound_vars,
            body: t.clone(),
        }
    }
}

impl Substitution {
    fn new() -> Self {
        Substitution(HashMap::new())
    }

    fn compose(&self, other: &Substitution) -> Substitution {
        other.apply_to_substitution(&self);
        let mut new_map = self.0.clone();
        new_map.extend(other.0.clone());

        Substitution(new_map)
    }

    fn apply_to_substitution(&self, other: &Substitution) -> Substitution {
        let new_map = other.0.iter().map(|(k, v)| (*k, self.apply(v))).collect();
        Substitution(new_map)
    }

    fn apply(&self, t: &Ty) -> Ty {
        use Ty::*;
        match t {
            Var(a) => self.0.get(a).cloned().unwrap_or(Var(*a)),
            Fn(arg, ret) => {
                let new_arg = self.apply(arg);
                let new_ret = self.apply(ret);
                Fn(Box::new(new_arg), Box::new(new_ret))
            }
            Tuple(types) => {
                let new_types = types.iter().map(|ty| self.apply(ty)).collect();
                Tuple(new_types)
            }
            Ctor(name, args) => {
                let new_args = args.iter().map(|arg| self.apply(arg)).collect();
                Ctor(*name, new_args)
            }
            Concrete(_) => t.clone(),
            Nocrypt(t) => Nocrypt(Box::new(self.apply(t))),
        }
    }

    fn apply_scheme(&self, scheme: &Scheme) -> Scheme {
        //skip bound vars
        let mask: HashSet<_> = scheme.bound_vars.iter().cloned().collect();
        let filtered = Substitution(
            self.0
                .iter()
                .filter(|(k, _)| !mask.contains(k))
                .map(|(k, v)| (*k, v.clone()))
                .collect(),
        );
        Scheme {
            bound_vars: scheme.bound_vars.clone(),
            body: filtered.apply(&scheme.body),
        }
    }

    fn apply_env(&self, env: &TypeEnv) -> TypeEnv {
        env.iter()
            .map(|(k, v)| (*k, self.apply_scheme(v)))
            .collect()
    }
}

pub fn fresh_type_var_id(counter: &mut TypeVar) -> TypeVar {
    let var = *counter;
    *counter += 1;
    var
}

pub fn fresh_type_var(counter: &mut TypeVar) -> Ty {
    Ty::Var(fresh_type_var_id(counter))
}
