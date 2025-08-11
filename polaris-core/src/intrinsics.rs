use crate::symbol::{NameResolverContext, PrimitiveType, TypeVariant};

pub fn declare_intrinsics(nr: &mut NameResolverContext) {
    declare_intrinsic_primitives(nr);
    declare_intrinsic_directives(nr);
}

macro_rules! declare {
    ($nr:expr, $name:expr, $type_info:expr) => {{
        let type_id = $nr.declare_type(None, vec![], $type_info, None);
        $nr.declare($name.to_string(), None, Some(type_id), false);
    }};
}

macro_rules! declare_primitive {
    ($nr:expr, $name:expr, $primitive:expr) => {{
        $nr.declare_type(
            Some($name.to_string()),
            vec![],
            TypeVariant::Primitive($primitive),
            None,
        );
    }};

    ($nr:expr, $name:expr, $primitive:expr, [$($generics:expr),*]) => {{
        $nr.declare_type(
            Some($name.to_string()),
            vec![$($generics),*],
            TypeVariant::Primitive($primitive),
            None,
        );
    }};
}

macro_rules! s {
    ($s:expr, $generics:expr) => {
        ($s.to_string(), $generics)
    };
}

pub fn declare_intrinsic_primitives(nr: &mut NameResolverContext) {
    declare_primitive!(nr, "bool", PrimitiveType::Bool);
    declare_primitive!(nr, "int32", PrimitiveType::Int32);
    declare_primitive!(nr, "int64", PrimitiveType::Int64);
    declare_primitive!(nr, "uint32", PrimitiveType::UInt32);
    declare_primitive!(nr, "uint64", PrimitiveType::UInt64);
    declare_primitive!(nr, "float32", PrimitiveType::Float32);
    declare_primitive!(nr, "float64", PrimitiveType::Float64);
    declare_primitive!(nr, "string", PrimitiveType::String);
    declare_primitive!(nr, "void", PrimitiveType::Void);
    declare_primitive!(nr, "vector", PrimitiveType::Vector, [s!("T", None)]);
}

pub fn declare_intrinsic_directives(nr: &mut NameResolverContext) {
    declare!(
        nr,
        "module",
        TypeVariant::Directive {
            args: vec![TypeVariant::Primitive(PrimitiveType::String)]
        }
    );
}
