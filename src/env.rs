use std::collections::HashMap;
use std::collections::hash_map::Iter;

use ast::{binary, host};

enum Binding {
    Abs(binary::Kind),
    Struct(host::Type<String>),
}

/// An environment of bindings and types
///
/// ```plain
/// Γ ::=
///         ·           empty environment
///         Γ, x:σ      environment extension
/// ```
#[derive(Debug)]
pub struct Env<'parent> {
    parent: Option<&'parent Env<'parent>>,
    tys: HashMap<String, host::Type<String>>,
    bindings: HashMap<String, host::Type<String>>,
}

impl<'parent> Env<'parent> {
    pub fn new() -> Env<'static> {
        Env {
            parent: None,
            tys: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn extend(&self) -> Env {
        Env {
            parent: Some(self),
            tys: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn add_ty<S>(&mut self, name: S, ty: host::Type<String>)
    where
        S: Into<String>,
    {
        self.tys.insert(name.into(), ty);
    }

    pub fn add_binding<S>(&mut self, name: S, ty: host::Type<String>)
    where
        S: Into<String>,
    {
        self.bindings.insert(name.into(), ty);
    }

    pub fn lookup_ty(&self, name: &str) -> Option<&host::Type<String>> {
        self.tys
            .get(name)
            .or_else(|| self.parent.and_then(|env| env.lookup_ty(name)))
    }

    pub fn lookup_binding(&self, name: &str) -> Option<&host::Type<String>> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.and_then(|env| env.lookup_binding(name)))
    }

    pub fn tys(&self) -> Iter<String, host::Type<String>> {
        self.tys.iter()
    }
}

// const BUILT_INS: [(&str, Type); 30] = [
//     ("u8", Type::UInt(1, Endianness::Target)),
//     ("u16", Type::UInt(2, Endianness::Target)),
//     ("u32", Type::UInt(4, Endianness::Target)),
//     ("u64", Type::UInt(8, Endianness::Target)),
//     ("i8", Type::SInt(1, Endianness::Target)),
//     ("i16", Type::SInt(2, Endianness::Target)),
//     ("i32", Type::SInt(4, Endianness::Target)),
//     ("i64", Type::SInt(8, Endianness::Target)),
//     ("f32", Type::Float(4, Endianness::Target)),
//     ("f64", Type::Float(8, Endianness::Target)),

//     ("u8le", Type::UInt(1, Endianness::Little)),
//     ("u16le", Type::UInt(2, Endianness::Little)),
//     ("u32le", Type::UInt(4, Endianness::Little)),
//     ("u64le", Type::UInt(8, Endianness::Little)),
//     ("i8le", Type::SInt(1, Endianness::Little)),
//     ("i16le", Type::SInt(2, Endianness::Little)),
//     ("i32le", Type::SInt(4, Endianness::Little)),
//     ("i64le", Type::SInt(8, Endianness::Little)),
//     ("f32le", Type::Float(4, Endianness::Little)),
//     ("f64le", Type::Float(8, Endianness::Little)),

//     ("u8be", Type::UInt(1, Endianness::Big)),
//     ("u16be", Type::UInt(2, Endianness::Big)),
//     ("u32be", Type::UInt(4, Endianness::Big)),
//     ("u64be", Type::UInt(8, Endianness::Big)),
//     ("i8be", Type::SInt(1, Endianness::Big)),
//     ("i16be", Type::SInt(2, Endianness::Big)),
//     ("i32be", Type::SInt(4, Endianness::Big)),
//     ("i64be", Type::SInt(8, Endianness::Big)),
//     ("f32be", Type::Float(4, Endianness::Big)),
//     ("f64be", Type::Float(8, Endianness::Big)),
// ];

impl Default for Env<'static> {
    fn default() -> Env<'static> {
        // let mut env = Env::new();
        let env = Env::new();

        // for &(ident, ref ty) in &BUILT_INS {
        //     env.add_ty(ident, ty.clone());
        // }

        env
    }
}
