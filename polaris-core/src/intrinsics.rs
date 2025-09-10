use std::collections::HashMap;

use crate::symbol::SymbolId;

pub fn intrinsic_type_symbols(symbol_idx: &mut usize) -> HashMap<String, SymbolId> {
    let mut symbols = HashMap::new();

    let intrinsic_types = vec![
        "Void", "Int", "I8", "U8", "I16", "U16", "I32", "U32", "I64", "U64", "Real", "Fixed1",
        "Fixed2", "Fixed4", "Bool", "String", "Char", "Result", "Option", "Array", "List", "Map",
    ];

    for ty in intrinsic_types {
        symbols.insert(ty.to_string(), *symbol_idx);
        *symbol_idx += 1;
    }

    symbols
}

pub fn intrinsic_symbols(symbol_idx: &mut usize) -> HashMap<String, SymbolId> {
    let mut symbols = HashMap::new();

    let intrinsics = vec!["True", "False", "Some", "None", "Void", "assert", "panic"];

    for name in intrinsics {
        symbols.insert(name.to_string(), *symbol_idx);
        *symbol_idx += 1;
    }

    symbols
}
