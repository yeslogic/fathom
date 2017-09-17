use std::collections::HashMap;

use ast::{Type, TypeConst};
use source::Span;

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

const BUILT_INS: [(&str, TypeConst); 10] = [
    ("u8", TypeConst::U8),
    ("u16", TypeConst::U16),
    ("u32", TypeConst::U32),
    ("u64", TypeConst::U64),
    ("i8", TypeConst::I8),
    ("i16", TypeConst::I16),
    ("i32", TypeConst::I32),
    ("i64", TypeConst::I64),
    ("f32", TypeConst::F32),
    ("f64", TypeConst::F64),
];

impl Default for Env<'static> {
    fn default() -> Env<'static> {
        let mut env = Env::new();

        for &(ident, ty_const) in &BUILT_INS {
            env.add_ty(ident, Type::Const(Span::start(), ty_const));
        }

        env
    }
}
