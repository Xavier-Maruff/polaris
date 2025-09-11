//might reimplement this as just polaris code in the core lib in the future?
//this isn't super robust

use crate::symbol::SymbolId;
use std::collections::HashMap;

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
    ASSERT => "assert",
    PANIC => "panic",
}

macro_rules! create_symbol_map {
    ($items:expr, $symbol_idx:expr) => {{
        let mut symbols = HashMap::new();
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
