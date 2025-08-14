use crate::{module::INVALID_MODULE_ID, symbol::NameResolverPassContext};

pub fn declare_intrinsics(nr: &mut NameResolverPassContext) {
    declare_intrinsic_primitives(nr);
    declare_intrinsic_directives(nr);
}

macro_rules! decl_symbol {
    ($nr:expr, $name:expr) => {{
        $nr.declare(INVALID_MODULE_ID, $name.to_string(), None, false, false);
    }};
}

macro_rules! decl_type_symbol {
    ($nr:expr, $name:expr) => {{
        $nr.declare_type(INVALID_MODULE_ID, Some($name.to_string()), None);
    }};
}

pub fn declare_intrinsic_primitives(nr: &mut NameResolverPassContext) {
    decl_type_symbol!(nr, "bool");
    decl_type_symbol!(nr, "int32");
    decl_type_symbol!(nr, "int64");
    decl_type_symbol!(nr, "uint32");
    decl_type_symbol!(nr, "uint64");
    decl_type_symbol!(nr, "float32");
    decl_type_symbol!(nr, "float64");
    decl_type_symbol!(nr, "string");
    decl_type_symbol!(nr, "void");
    decl_type_symbol!(nr, "vector");
}

pub fn declare_intrinsic_directives(nr: &mut NameResolverPassContext) {
    decl_symbol!(nr, "@module");
    decl_symbol!(nr, "@import");
    decl_symbol!(nr, "@host");
    decl_symbol!(nr, "@blocking");
    decl_symbol!(nr, "@nonblocking");
    decl_symbol!(nr, "@gpu");
    decl_symbol!(nr, "@cpu");
}
