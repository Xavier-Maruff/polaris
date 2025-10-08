//might reimplement this as just polaris code in the core lib in the future?
//this isn't super robust

use crate::{
    ast::BinaryOp,
    symbol::{SymbolContext, SymbolId},
    types::{Scheme, Ty, TyKind, TypeEnv, TypeVar, fresh_type_var_id},
};
use rustc_hash::FxHashMap as HashMap;

macro_rules! define_intrinsic_types {
    ($($name:ident => $str:literal),* $(,)?) => {
        $(
            pub const $name: &'static str = $str;
        )*

        pub const INTRINSIC_TYPES: &[&'static str] = &[
            $($str,)*
        ];
    };
}

macro_rules! define_intrinsic_symbols {
    ($($name:ident => $str:literal),* $(,)?) => {
        $(
            pub const $name: &'static str = $str;
        )*

        pub const INTRINSIC_SYMBOLS: &[&'static str] = &[
            $($str,)*
        ];
    };
}

define_intrinsic_types! {
    VOID => "Void",
    INT => "Int",
    I8 => "I8",
    U8 => "U8",
    I16 => "I16",
    U16 => "U16",
    I32 => "I32",
    U32 => "U32",
    I64 => "I64",
    U64 => "U64",
    REAL => "Real",
    FIXED1 => "Fixed1",
    FIXED2 => "Fixed2",
    FIXED4 => "Fixed4",
    BOOL => "Bool",
    STRING => "String",
    CHAR => "Char",
    RESULT => "Result",
    OPTION => "Option",
    ARRAY => "Array",
    LIST => "List",
    MAP => "Map",
}

define_intrinsic_symbols! {
    TRUE => "True",
    FALSE => "False",
    SOME => "Some",
    NONE => "None",
    OK => "Ok",
    ERR => "Err",
    ASSERT => "assert",
    PANIC => "panic",
}

macro_rules! create_symbol_map {
    ($items:expr, $symbol_idx:expr) => {{
        let mut symbols = HashMap::default();
        for item in $items.iter() {
            symbols.insert(item.to_string(), *$symbol_idx);
            *$symbol_idx += 1;
        }
        symbols
    }};
}

pub fn intrinsic_type_symbols(symbol_idx: &mut usize) -> HashMap<String, SymbolId> {
    create_symbol_map!(INTRINSIC_TYPES, symbol_idx)
}

pub fn intrinsic_symbols(symbol_idx: &mut usize) -> HashMap<String, SymbolId> {
    let intrinsics = [INTRINSIC_SYMBOLS, &[VOID]].concat();
    create_symbol_map!(intrinsics, symbol_idx)
}

pub fn create_intrinsic_type_env(symbols: &mut SymbolContext, counter: &mut TypeVar) -> TypeEnv {
    let mut type_env = TypeEnv::default();
    let mut type_var_map = HashMap::default();

    macro_rules! decl_concrete {
        //intrinsic type
        ($type_name:ident) => {
            type_env.insert(
                symbols.intrinsic_types[$type_name].clone(),
                Scheme {
                    bound_vars: vec![],
                    body: Ty::new(TyKind::Concrete(
                        symbols.intrinsic_types[$type_name].clone(),
                    )),
                },
            );
        };
    }

    ///fugly macro but the resultant dsl is actually pretty nice
    macro_rules! decl_ctor {
        //no-arg constructor - not a function, just a value of the type
        ($type_name:ident ( $($params:ident),* ), $constructor:ident) => {
            $(
                let $params = fresh_type_var_id(counter);
                type_var_map.insert(stringify!($params), Ty::new(TyKind::Var($params)));
            )*

            let res_type = Ty::new(TyKind::Ctor(
                symbols.intrinsic_types[$type_name].clone(),
                vec![ $( type_var_map[stringify!($params)].clone() ),*]
            ));


            type_env.insert(
                symbols.intrinsic_symbols[$constructor].clone(),
                Scheme {
                    bound_vars: vec![$( $params ),*],
                    body: res_type,
                },
            );
        };

        //arg constructor
        ($type_name:ident ( $($params:ident),* ), $constructor:ident ( $($arg_params:ident),* )) => {{
            type_var_map.clear();

            //param -> fresh type var
            $(
                let $params = fresh_type_var_id(counter);
                type_var_map.insert(stringify!($params), Ty::new(TyKind::Var($params)));
            )*

            //Type (a, b, c, ...)
            let res_type = Ty::new(TyKind::Ctor(
                symbols.intrinsic_types[$type_name].clone(),
                vec![ $( type_var_map[stringify!($params)].clone() ),*]
            ));


            //arg1 --> arg2 -> arg3 -> ... -> Type
            let mut curried = res_type;
            $(
                let arg_type = type_var_map[stringify!($arg_params)].clone();
                curried = Ty::new(TyKind::Fn(Box::new(arg_type), Box::new(curried)));
            )*

            //[a, b, c, ...]
            let bound = vec![ $( $params ),* ];

            type_env.insert(
              symbols.intrinsic_symbols[$constructor].clone(),
              Scheme {
                  bound_vars: bound,
                  body: curried,
              },
            );
        }};
    }

    decl_concrete!(VOID);
    decl_ctor!(VOID(), VOID);

    decl_concrete!(INT);
    decl_concrete!(I8);
    decl_concrete!(U8);
    decl_concrete!(I16);
    decl_concrete!(U16);
    decl_concrete!(I32);
    decl_concrete!(U32);
    decl_concrete!(I64);
    decl_concrete!(U64);
    decl_concrete!(REAL);
    decl_concrete!(FIXED1);
    decl_concrete!(FIXED2);
    decl_concrete!(FIXED4);

    decl_concrete!(BOOL);
    decl_ctor!(BOOL(), TRUE);
    decl_ctor!(BOOL(), FALSE);

    decl_concrete!(STRING);
    decl_concrete!(CHAR);

    decl_ctor!(OPTION(t), SOME(t));
    decl_ctor!(OPTION(t), NONE);

    decl_ctor!(RESULT(t, e), OK(t));
    decl_ctor!(RESULT(t, e), ERR(e));

    type_env
}

/// (op -> (lhs, rhs) -> return type
pub fn create_intrinsic_binops(
    symbols: &SymbolContext,
    env: &TypeEnv,
) -> HashMap<BinaryOp, HashMap<(Ty, Ty), Ty>> {
    let mut map: HashMap<BinaryOp, HashMap<(Ty, Ty), Ty>> = HashMap::default();

    macro_rules! add_binop {
        (($lhs:ident, $op:expr, $rhs:ident) => $res:ident) => {
            let lhs_ty = env
                .get(&symbols.intrinsic_types[$lhs])
                .unwrap()
                .body
                .clone();
            let rhs_ty = env
                .get(&symbols.intrinsic_types[$rhs])
                .unwrap()
                .body
                .clone();
            let res_ty = env
                .get(&symbols.intrinsic_types[$res])
                .unwrap()
                .body
                .clone();

            map.entry($op)
                .or_insert_with(HashMap::default)
                .insert((lhs_ty, rhs_ty), res_ty);
        };

        (($lhs:expr, $op:expr, $rhs:expr) => $res:expr) => {
            map.entry($op)
                .or_insert_with(HashMap::default)
                .insert(($lhs, $rhs), $res);
        };
    }

    macro_rules! int_kind {
        ($name:ident) => {
            add_binop!(($name, BinaryOp::Add, $name) => $name);
            add_binop!(($name, BinaryOp::Subtract, $name) => $name);
            add_binop!(($name, BinaryOp::Multiply, $name) => $name);
            add_binop!(($name, BinaryOp::Modulus, $name) => $name);
            add_binop!(($name, BinaryOp::Exponent, $name) => $name);
            add_binop!(($name, BinaryOp::GreaterThan, $name) => BOOL);
            add_binop!(($name, BinaryOp::LessThan, $name) => BOOL);
            add_binop!(($name, BinaryOp::GreaterThanEquiv, $name) => BOOL);
            add_binop!(($name, BinaryOp::LessThanEquiv, $name) => BOOL);
            add_binop!(($name, BinaryOp::Equal, $name) => BOOL);
            add_binop!(($name, BinaryOp::NotEqual, $name) => BOOL);
        };
    }

    macro_rules! real_kind {
        ($name:ident) => {
            int_kind!($name);
            add_binop!(($name, BinaryOp::Divide, $name) => $name);
        };
    }

    int_kind!(INT);
    int_kind!(I8);
    int_kind!(U8);
    int_kind!(I16);
    int_kind!(U16);
    int_kind!(I32);
    int_kind!(U32);
    int_kind!(I64);
    int_kind!(U64);

    real_kind!(REAL);
    real_kind!(FIXED1);
    real_kind!(FIXED2);
    real_kind!(FIXED4);

    add_binop!((STRING, BinaryOp::Add, STRING) => STRING);
    add_binop!((CHAR, BinaryOp::Add, CHAR) => STRING);
    add_binop!((STRING, BinaryOp::Add, CHAR) => STRING);
    add_binop!((CHAR, BinaryOp::Add, STRING) => STRING);
    add_binop!((STRING, BinaryOp::Equal, STRING) => BOOL);
    add_binop!((CHAR, BinaryOp::Equal, CHAR) => BOOL);
    add_binop!((STRING, BinaryOp::NotEqual, STRING) => BOOL);
    add_binop!((CHAR, BinaryOp::NotEqual, CHAR) => BOOL);

    add_binop!((BOOL, BinaryOp::And, BOOL) => BOOL);
    add_binop!((BOOL, BinaryOp::Or, BOOL) => BOOL);

    //generic equality
    let bool_ty = env[&symbols.intrinsic_types[BOOL]].body.clone();
    add_binop!((
        Ty::new(TyKind::Var(usize::MAX)),
        BinaryOp::Equal,
        Ty::new(TyKind::Var(usize::MAX))
    ) => bool_ty.clone());

    add_binop!((
        Ty::new(TyKind::Var(usize::MAX)),
        BinaryOp::NotEqual,
        Ty::new(TyKind::Var(usize::MAX))
    ) => bool_ty);

    map
}
