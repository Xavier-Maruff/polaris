use std::f64::consts::E;

use crate::ast::ast::{BinaryOp, UnaryOp};
use crate::symbol::SymbolTable;
use crate::typecheck::{Constraint, Op, Type, TypeScheme, TypecheckPassContext};
use crate::{module::INVALID_MODULE_ID, symbol::NameResolverPassContext};

const INTR_BOOL: &str = "bool";
const INTR_INT32: &str = "int32";
const INTR_INT64: &str = "int64";
const INTR_UINT32: &str = "uint32";
const INTR_UINT64: &str = "uint64";
const INTR_FLOAT32: &str = "float32";
const INTR_FLOAT64: &str = "float64";
const INTR_STRING: &str = "string";
const INTR_VECTOR: &str = "vector";
const INTR_VOID: &str = "void";

pub fn declare_intrinsic_symbols(nr: &mut NameResolverPassContext) {
    declare_intrinsic_primitives(nr);
    declare_intrinsic_directives(nr);
}

pub fn declare_intrinsic_types(tc: &mut TypecheckPassContext, st: &SymbolTable) {
    declare_intrinsic_interfaces(tc);
    declare_intrinsic_ops(tc);
    declare_intrinsic_impls(tc, st);
}

fn declare_intrinsic_primitives(nr: &mut NameResolverPassContext) {
    macro_rules! decl_type_symbol {
        ($nr:expr, $name:expr) => {{
            $nr.declare_type(INVALID_MODULE_ID, Some($name.to_string()), None);
        }};
        ($nr:expr, $name:expr, $type_scheme:expr) => {{
            let symbol_id = $nr.declare_type(INVALID_MODULE_ID, Some($name.to_string()), None);
            $nr.table.type_schemes.insert(symbol_id, $type_scheme);
        }};
    }

    decl_type_symbol!(nr, INTR_BOOL, TypeScheme::new(Type::Bool, vec![]));
    decl_type_symbol!(nr, INTR_INT32, TypeScheme::new(Type::Int32, vec![]));
    decl_type_symbol!(nr, INTR_INT64, TypeScheme::new(Type::Int64, vec![]));
    decl_type_symbol!(nr, INTR_UINT32, TypeScheme::new(Type::UInt32, vec![]));
    decl_type_symbol!(nr, INTR_UINT64, TypeScheme::new(Type::UInt64, vec![]));
    decl_type_symbol!(nr, INTR_FLOAT32, TypeScheme::new(Type::Float32, vec![]));
    decl_type_symbol!(nr, INTR_FLOAT64, TypeScheme::new(Type::Float64, vec![]));
    decl_type_symbol!(nr, INTR_STRING, TypeScheme::new(Type::String, vec![]));
    decl_type_symbol!(
        nr,
        INTR_VECTOR,
        TypeScheme::new(Type::Vector(Box::new(Type::TypeVar(0))), vec![])
    );
    decl_type_symbol!(nr, INTR_VOID, TypeScheme::new(Type::Void, vec![]));
}

fn declare_intrinsic_directives(nr: &mut NameResolverPassContext) {
    macro_rules! decl_symbol {
        ($nr:expr, $name:expr) => {{
            $nr.declare(INVALID_MODULE_ID, $name.to_string(), None, false, false);
        }};

        ($nr:expr, $name:expr, $type_scheme:expr) => {{
            let symbol_id = $nr.declare(INVALID_MODULE_ID, $name.to_string(), None, false, false);
            $nr.table.type_schemes.insert(symbol_id, $type_scheme);
        }};
    }

    decl_symbol!(
        nr,
        "@module",
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::String],
                return_type: Box::new(Type::Void),
            },
            vec![]
        )
    );
    decl_symbol!(
        nr,
        "@import",
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::String],
                return_type: Box::new(Type::Void),
            },
            vec![]
        )
    );
    decl_symbol!(nr, "@host", TypeScheme::new(Type::Void, vec![]));
    decl_symbol!(nr, "@blocking", TypeScheme::new(Type::Void, vec![]));
    decl_symbol!(nr, "@nonblocking", TypeScheme::new(Type::Void, vec![]));
    decl_symbol!(nr, "@gpu", TypeScheme::new(Type::Void, vec![]));
    decl_symbol!(nr, "@cpu", TypeScheme::new(Type::Void, vec![]));
}

fn declare_intrinsic_interfaces(tc: &mut TypecheckPassContext) {
    macro_rules! d {
        ($name:expr) => {
            tc.decl_intrinsic_interface($name)
        };
    }

    d!("Add");
    d!("Negate");
    d!("Multiply");
    d!("Divide");
    d!("Modulo");
    d!("BitOr");
    d!("BitXor");
    d!("BitAnd");
    d!("BitNot");
    d!("Equal");
    d!("BoolNot");
    d!("BoolOr");
    d!("BoolAnd");
    d!("Ordered");
}

fn declare_intrinsic_ops(tc: &mut TypecheckPassContext) {
    macro_rules! decl_op {
        ($op:expr, $type_scheme:expr, $($constraints:expr),*) => {{
            let mut type_scheme = $type_scheme;
            type_scheme.constraints.extend(vec![
                $(Constraint::Implementation(
                    Type::TypeVar($constraints.0),
                    tc.intrinsic_interface_id($constraints.1)
                )),
            *]);
            tc.decl_op($op, type_scheme);
        }}
    }

    decl_op!(
        Op::BinOp(BinaryOp::Assign),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
    );

    decl_op!(
        Op::UOp(UnaryOp::Deref),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::Ref(Box::new(Type::TypeVar(0)))],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
    );

    decl_op!(
        Op::BinOp(BinaryOp::Add),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Add")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Subtract),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Add"),
        (0, "Negate")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Multiply),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Multiply")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Divide),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Divide")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Modulo),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Modulo")
    );

    decl_op!(
        Op::BinOp(BinaryOp::BitOr),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "BitOr")
    );

    decl_op!(
        Op::BinOp(BinaryOp::BitXor),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "BitXor")
    );

    decl_op!(
        Op::BinOp(BinaryOp::BitAnd),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "BitAnd")
    );

    decl_op!(
        Op::UOp(UnaryOp::BitNot),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "BitNot")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Equiv),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "Equal")
    );

    decl_op!(
        Op::UOp(UnaryOp::Not),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::Bool],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "BoolNot")
    );

    decl_op!(
        Op::BinOp(BinaryOp::And),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::Bool, Type::Bool],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "BoolAnd")
    );

    decl_op!(
        Op::BinOp(BinaryOp::Or),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::Bool, Type::Bool],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "BoolOr")
    );

    decl_op!(
        Op::BinOp(BinaryOp::LessThan),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "Ordered")
    );

    decl_op!(
        Op::BinOp(BinaryOp::LessThanEquiv),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "Ordered")
    );

    decl_op!(
        Op::BinOp(BinaryOp::GreaterThan),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "Ordered")
    );

    decl_op!(
        Op::BinOp(BinaryOp::GreaterThanEquiv),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0), Type::TypeVar(0)],
                return_type: Box::new(Type::Bool),
            },
            vec![]
        ),
        (0, "Ordered")
    );

    decl_op!(
        Op::UOp(UnaryOp::Negate),
        TypeScheme::new(
            Type::Lambda {
                params: vec![Type::TypeVar(0)],
                return_type: Box::new(Type::TypeVar(0)),
            },
            vec![]
        ),
        (0, "Negate")
    );
}

fn declare_intrinsic_impls(tc: &mut TypecheckPassContext, st: &SymbolTable) {
    macro_rules! d {
        ($name: expr, $($interface:expr),*) => {{
            let interface_ids = vec![$(tc.intrinsic_interface_id($interface)),*];
            let symbol_id = st.get_id_by_name($name).unwrap_or_else(|| {
                panic!("Intrinsic type {} not found in symbol table", $name)
            });
            tc.decl_intrinsic_interface_impls(symbol_id, interface_ids);
        }};
    }

    d!(INTR_BOOL, "BoolNot", "BoolOr", "BoolAnd", "Equal");
    d!(
        INTR_INT32, "Add", "Negate", "Multiply", "Modulo", "BitOr", "BitXor", "BitAnd", "BitNot",
        "Equal", "Ordered"
    );
    d!(
        INTR_INT64, "Add", "Negate", "Multiply", "Modulo", "BitOr", "BitXor", "BitAnd", "BitNot",
        "Equal", "Ordered"
    );
    d!(
        INTR_UINT32,
        "Add",
        "Negate",
        "Multiply",
        "Modulo",
        "BitOr",
        "BitXor",
        "BitAnd",
        "BitNot",
        "Equal",
        "Ordered"
    );
    d!(
        INTR_UINT64,
        "Add",
        "Negate",
        "Multiply",
        "Modulo",
        "BitOr",
        "BitXor",
        "BitAnd",
        "BitNot",
        "Equal",
        "Ordered"
    );
    d!(
        INTR_FLOAT32,
        "Add",
        "Negate",
        "Multiply",
        "Divide",
        "Modulo",
        "BitOr",
        "BitXor",
        "BitAnd",
        "BitNot",
        "Equal",
        "Ordered"
    );
    d!(
        INTR_FLOAT64,
        "Add",
        "Negate",
        "Multiply",
        "Divide",
        "Modulo",
        "BitOr",
        "BitXor",
        "BitAnd",
        "BitNot",
        "Equal",
        "Ordered"
    );
    d!(INTR_STRING, "Add", "Equal", "Ordered");
    //no default impls for vector, as it is entirely dependent on the parametric type
    d!(INTR_VOID, "Equal");
}
