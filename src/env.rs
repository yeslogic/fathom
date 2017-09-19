use std::collections::HashMap;

use ast::{Endianness, Type, TypeConst};

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
    tys: HashMap<String, Type>,
    bindings: HashMap<String, Type>,
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

    pub fn add_ty<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.tys.insert(name.into(), ty);
    }

    pub fn add_binding<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.bindings.insert(name.into(), ty);
    }

    pub fn lookup_ty(&self, name: &str) -> Option<&Type> {
        self.tys.get(name).or_else(|| {
            self.parent.and_then(|env| env.lookup_ty(name))
        })
    }

    pub fn lookup_binding(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name).or_else(|| {
            self.parent.and_then(|env| env.lookup_binding(name))
        })
    }
}

const BUILT_INS: [(&str, TypeConst); 30] = [
    ("u8", TypeConst::U(1, Endianness::Target)),
    ("u16", TypeConst::U(2, Endianness::Target)),
    ("u32", TypeConst::U(4, Endianness::Target)),
    ("u64", TypeConst::U(8, Endianness::Target)),
    ("i8", TypeConst::I(1, Endianness::Target)),
    ("i16", TypeConst::I(2, Endianness::Target)),
    ("i32", TypeConst::I(4, Endianness::Target)),
    ("i64", TypeConst::I(8, Endianness::Target)),
    ("f32", TypeConst::F(4, Endianness::Target)),
    ("f64", TypeConst::F(8, Endianness::Target)),

    ("u8le", TypeConst::U(1, Endianness::Little)),
    ("u16le", TypeConst::U(2, Endianness::Little)),
    ("u32le", TypeConst::U(4, Endianness::Little)),
    ("u64le", TypeConst::U(8, Endianness::Little)),
    ("i8le", TypeConst::I(1, Endianness::Little)),
    ("i16le", TypeConst::I(2, Endianness::Little)),
    ("i32le", TypeConst::I(4, Endianness::Little)),
    ("i64le", TypeConst::I(8, Endianness::Little)),
    ("f32le", TypeConst::F(4, Endianness::Little)),
    ("f64le", TypeConst::F(8, Endianness::Little)),

    ("u8be", TypeConst::U(1, Endianness::Big)),
    ("u16be", TypeConst::U(2, Endianness::Big)),
    ("u32be", TypeConst::U(4, Endianness::Big)),
    ("u64be", TypeConst::U(8, Endianness::Big)),
    ("i8be", TypeConst::I(1, Endianness::Big)),
    ("i16be", TypeConst::I(2, Endianness::Big)),
    ("i32be", TypeConst::I(4, Endianness::Big)),
    ("i64be", TypeConst::I(8, Endianness::Big)),
    ("f32be", TypeConst::F(4, Endianness::Big)),
    ("f64be", TypeConst::F(8, Endianness::Big)),
];

impl Default for Env<'static> {
    fn default() -> Env<'static> {
        let mut env = Env::new();

        for &(ident, ty_const) in &BUILT_INS {
            env.add_ty(ident, Type::Const(ty_const));
        }

        env
    }
}
