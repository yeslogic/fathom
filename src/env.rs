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
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    tys: HashMap<String, Type>,
    bindings: HashMap<String, Type>,
}

impl<'a> Env<'a> {
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

impl Default for Env<'static> {
    fn default() -> Env<'static> {
        let mut env = Env::new();
        env.add_ty("u8", Type::Const(Span::start(), TypeConst::U8));
        env.add_ty("u16", Type::Const(Span::start(), TypeConst::U16));
        env.add_ty("u32", Type::Const(Span::start(), TypeConst::U32));
        env.add_ty("u64", Type::Const(Span::start(), TypeConst::U64));
        env.add_ty("i8", Type::Const(Span::start(), TypeConst::I8));
        env.add_ty("i16", Type::Const(Span::start(), TypeConst::I16));
        env.add_ty("i32", Type::Const(Span::start(), TypeConst::I32));
        env.add_ty("i64", Type::Const(Span::start(), TypeConst::I64));
        env
    }
}
