use crate::symbol::{NameResolverContext, Type, TypeVariant};

macro_rules! declare {
    ($nr:expr, $name:expr, $type_info:expr) => {
        $nr.declare($name.to_string(), None, Some($type_info), false);
    };
}

pub fn load_intrinsic_symbols(nr: &mut NameResolverContext) {
    declare!(
        nr,
        "module",
        Type::new_anon(TypeVariant::Directive { args: vec![] })
    );
}
