use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{INT, REAL, STRING},
    module::ModuleContext,
    symbol::{SymbolContext, SymbolId},
};

pub fn typecheck_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let ctx = TypecheckContext::new(compile_ctx);

    Ok(())
}

// it's hindley-milner time hell yeah

struct TypecheckContext<'a> {
    type_env: TypeEnv,
    type_var_counter: TypeVar,
    errors: &'a mut Vec<Diagnostic>,
    warnings: &'a mut Vec<Diagnostic>,
    symbols: &'a mut SymbolContext,
    all_modules: Vec<&'a mut ModuleContext>,
    current_file: String,
    undetermined_symbol_counter: usize,
}

type TypeVar = usize;

#[derive(Clone, Debug, PartialEq)]
enum Ty {
    Var(TypeVar),
    Concrete(SymbolId),
    Fn(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    App(SymbolId, Vec<Ty>),
}

#[derive(Clone, Debug)]
struct Scheme {
    bound_vars: Vec<TypeVar>,
    body: Ty,
}

#[derive(Clone, Debug)]
struct Substitution(HashMap<TypeVar, Ty>);

type TypeEnv = HashMap<SymbolId, Scheme>;

impl<'a> TypecheckContext<'a> {
    fn new(ctx: &'a mut CompileContext) -> Self {
        let mut all_modules = Vec::new();
        for modules in ctx.packages.values_mut() {
            for module in modules.values_mut() {
                all_modules.push(module);
            }
        }

        let undetermined_symbol_counter = ctx.symbols.symbol_idx.clone();

        Self {
            symbols: &mut ctx.symbols,
            type_env: HashMap::new(),
            type_var_counter: 0,
            errors: &mut ctx.errors,
            warnings: &mut ctx.warnings,
            all_modules,
            current_file: String::new(),
            undetermined_symbol_counter,
        }
    }

    pub fn typecheck(&mut self) -> Result<(), ()> {
        Ok(())
    }

    fn fresh_type_var(&mut self) -> Ty {
        fresh_type_var(&mut self.type_var_counter)
    }

    fn is_nonexpansive(&mut self, e: &ExprKind) -> bool {
        use ExprKind::*;
        match e {
            IntLit(_) | RealLit { .. } | StringLit(_) | Discard => true,
            TupleLit(xs) | ListLit(xs) => xs
                .iter()
                .all(|n| matches!(&n.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))),
            MapLit(kvs) => kvs.iter().all(|(k, v)| {
                matches!(&k.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
                    && matches!(&v.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
            }),
            Closure { .. } => true,
            Symbol { .. } => true,
            _ => false,
        }
    }

    fn w_expr(&mut self, node: &mut Node) -> Result<(Substitution, Ty), ()> {
        use ExprKind::*;
        let e = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => unreachable!(),
        };

        match e {
            Symbol { name } => {
                let sc = self
                    .type_env
                    .get(node.symbol_id.as_ref().unwrap())
                    .unwrap()
                    .clone();

                Ok((
                    Substitution::new(),
                    sc.instantiate(&mut self.type_var_counter),
                ))
            }

            IntLit(_) => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types.get(INT).unwrap().clone()),
            )),
            RealLit { .. } => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types.get(REAL).unwrap().clone()),
            )),
            StringLit(_) => Ok((
                Substitution::new(),
                Ty::Concrete(self.symbols.intrinsic_types.get(STRING).unwrap().clone()),
            )),
            TupleLit(elems) => {
                let mut s = Substitution::new();
                let mut types = Vec::new();
                for elem in elems {
                    let (s2, t) = self.w_expr(elem)?;
                    s = s.compose(&s2);
                    types.push(s.apply(&t));
                }
                Ok((s, Ty::Tuple(types)))
            }

            _ => {
                self.errors.push(Diagnostic::new(DiagnosticMsg {
                    message: "Typechecking for this expression is not yet implemented".to_string(),
                    span: node.span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UnsupportedFeature,
                }));
                Err(())
            }
        }
    }

    fn occurs(&mut self, a: TypeVar, t: &Ty) -> bool {
        use Ty::*;
        match t {
            Var(b) => a == *b,
            Fn(arg, ret) => self.occurs(a, arg) || self.occurs(a, ret),
            App(_, args) | Tuple(args) => args.iter().any(|arg| self.occurs(a, arg)),
            Concrete(_) => false,
        }
    }

    fn bind(&mut self, a: TypeVar, t: &Ty) -> Result<Substitution, ()> {
        if let Ty::Var(b) = t
            && *b == a
        {
            return Ok(Substitution::new());
        }

        if self.occurs(a, t) {
            return Err(());
        }

        let mut m = HashMap::new();
        m.insert(a, t.clone());

        Ok(Substitution(m))
    }

    fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<Substitution, ()> {
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

            (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => {
                let mut s = Substitution::new();
                for (ty1, ty2) in types1.iter().zip(types2.iter()) {
                    let s2 = self.unify(&s.apply(ty1), &s.apply(ty2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            (App(name1, args1), App(name2, args2))
                if name1 == name2 && args1.len() == args2.len() =>
            {
                let mut s = Substitution::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let s2 = self.unify(&s.apply(arg1), &s.apply(arg2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            _ => Err(()),
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
            App(_, args) => {
                let mut set = HashSet::new();
                for arg in args {
                    set.extend(arg.free_type_vars());
                }
                set
            }
            Concrete(_) => HashSet::new(),
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
}

impl Scheme {
    fn instantiate(self, counter: &mut TypeVar) -> Ty {
        let mut subst = Substitution::new();
        for &var in &self.bound_vars {
            subst.0.insert(var, fresh_type_var(counter));
        }
        subst.apply(&self.body)
    }

    fn generalize(env: &TypeEnv, t: &Ty) -> Scheme {
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
            App(name, args) => {
                let new_args = args.iter().map(|arg| self.apply(arg)).collect();
                App(*name, new_args)
            }
            Concrete(_) => t.clone(),
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

fn fresh_type_var(counter: &mut TypeVar) -> Ty {
    let var = *counter;
    *counter += 1;
    Ty::Var(var)
}
