use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    symbol::SymbolId,
};

pub fn typecheck_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    Ok(())
}

struct TypecheckContext {
    type_env: TypeEnv,
    type_var_counter: TypeVar,
}

type TypeVar = usize;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Var(TypeVar),
    Concrete(SymbolId),
    Fn(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Param(SymbolId, Vec<Type>),
}

#[derive(Clone, Debug)]
struct Scheme {
    bound_vars: Vec<TypeVar>,
    body: Type,
}

#[derive(Clone, Debug)]
struct Substitution(HashMap<TypeVar, Type>);

type TypeEnv = HashMap<SymbolId, Scheme>;

impl TypecheckContext {
    fn new() -> Self {
        Self {
            type_env: HashMap::new(),
            type_var_counter: 0,
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        fresh_type_var(&mut self.type_var_counter)
    }

    fn algorithm_w(node: &mut Node) -> Result<(Substitution, Type), String> {
        Err("Not implemented".to_string())
    }
}

impl Type {
    fn free_type_vars(&self) -> HashSet<TypeVar> {
        use Type::*;
        match self {
            Var(a) => {
                let mut set = HashSet::new();
                set.insert(*a);
                set
            }
            Fn(args, ret) => {
                let mut set = HashSet::new();
                for arg in args {
                    set.extend(arg.free_type_vars());
                }
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
            Param(_, args) => {
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
            set.extend(Type::free_type_vars_in_scheme(scheme));
        }
        set
    }
}

impl Scheme {
    fn instantiate(counter: &mut TypeVar, scheme: &Scheme) -> Type {
        let mut subst = Substitution::new();
        for &var in &scheme.bound_vars {
            subst.0.insert(var, fresh_type_var(counter));
        }
        subst.apply(&scheme.body)
    }

    fn generalize(env: &TypeEnv, t: &Type) -> Scheme {
        let env_vars = Type::free_type_vars_in_env(env);
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

    fn apply(&self, t: &Type) -> Type {
        use Type::*;
        match t {
            Var(a) => self.0.get(a).cloned().unwrap_or(Var(*a)),
            Fn(args, ret) => {
                let new_args = args.iter().map(|arg| self.apply(arg)).collect();
                let new_ret = Box::new(self.apply(ret));
                Fn(new_args, new_ret)
            }
            Tuple(types) => {
                let new_types = types.iter().map(|ty| self.apply(ty)).collect();
                Tuple(new_types)
            }
            Param(name, args) => {
                let new_args = args.iter().map(|arg| self.apply(arg)).collect();
                Param(*name, new_args)
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

fn occurs(a: TypeVar, t: &Type) -> bool {
    use Type::*;
    match t {
        Var(b) => a == *b,
        Fn(args, ret) => args.iter().any(|arg| occurs(a, arg)) || occurs(a, ret),
        Tuple(types) => types.iter().any(|ty| occurs(a, ty)),
        Param(_, args) => args.iter().any(|arg| occurs(a, arg)),
        Concrete(_) => false,
    }
}

fn bind(a: TypeVar, t: &Type) -> Result<Substitution, String> {
    if let Type::Var(b) = t
        && *b == a
    {
        return Ok(Substitution::new());
    }

    if occurs(a, t) {
        return Err(format!("Occurs check failed: {} in {:?}", a, t));
    }

    let mut m = HashMap::new();
    m.insert(a, t.clone());

    Ok(Substitution(m))
}

fn unify(t1: &Type, t2: &Type) -> Result<Substitution, String> {
    use Type::*;
    match (t1, t2) {
        (a, b) if a == b => Ok(Substitution::new()),

        (Var(a), Var(b)) if a == b => Ok(Substitution::new()),
        (Var(a), _) => bind(*a, t2),
        (_, Var(b)) => bind(*b, t1),

        (Concrete(a), Concrete(b)) if a == b => Ok(Substitution::new()),

        (Fn(args1, ret1), Fn(args2, ret2)) if args1.len() == args2.len() => {
            let mut s = unify(ret1, ret2)?;
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                let s2 = unify(&s.apply(arg1), &s.apply(arg2))?;
                s = s.compose(&s2);
            }
            Ok(s)
        }

        (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => {
            let mut s = Substitution::new();
            for (ty1, ty2) in types1.iter().zip(types2.iter()) {
                let s2 = unify(&s.apply(ty1), &s.apply(ty2))?;
                s = s.compose(&s2);
            }
            Ok(s)
        }

        (Param(name1, args1), Param(name2, args2))
            if name1 == name2 && args1.len() == args2.len() =>
        {
            let mut s = Substitution::new();
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                let s2 = unify(&s.apply(arg1), &s.apply(arg2))?;
                s = s.compose(&s2);
            }
            Ok(s)
        }

        _ => Err(format!("Cannot unify {:?} with {:?}", t1, t2)),
    }
}

fn fresh_type_var(counter: &mut TypeVar) -> Type {
    let var = *counter;
    *counter += 1;
    Type::Var(var)
}
