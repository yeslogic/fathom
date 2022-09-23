use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Debug;
use std::slice::SliceIndex;
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Elim, Head, Value};
use crate::core::{Const, Prim, Term, UIntStyle};
use crate::env::{EnvLen, SharedEnv, SliceEnv};
use crate::source::{Span, Spanned};
use crate::StringId;

pub struct Context<'arena, 'env> {
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    rigid_exprs: &'env SharedEnv<ArcValue<'arena>>,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    pending_formats: Vec<(usize, ArcValue<'arena>)>,
    // cached_refs: HashMap<usize, Vec<ParsedRef<'arena>>>,
}

#[derive(Debug)]
enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    /// A vector: array with unknown length
    Vec(Box<Type>),
    /// Array with a known length
    Array(usize, Box<Type>),
}

#[derive(Debug)]
struct Struct {
    fields: Vec<(StringId, Type)>,
}

impl<'arena, 'env, 'data> Context<'arena, 'env> {
    pub fn new(
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        rigid_exprs: &'env SharedEnv<ArcValue<'arena>>,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> Context<'arena, 'env> {
        Context {
            item_exprs,
            rigid_exprs,
            flexible_exprs,
            // initial_buffer,
            pending_formats: Vec::new(),
            // cached_refs: HashMap::new(),
        }
    }

    fn elim_env(&self) -> semantics::ElimEnv<'arena, 'env> {
        semantics::ElimEnv::new(self.item_exprs, self.flexible_exprs)
    }

    fn conversion_env(&self) -> semantics::ConversionEnv<'arena, 'env> {
        semantics::ConversionEnv::new(self.item_exprs, EnvLen::new(), self.flexible_exprs)
    }

    pub fn compile_format(
        &mut self,
        format: &Term<'arena>,
        // TODO: Return a Rust module containing all the compiled elements?
    ) -> Result<(), ()> {
        match format {
            Term::ItemVar(_, _) => unimplemented! {},
            Term::LocalVar(_, _) => unimplemented! {},
            Term::MetaVar(_, _) => unimplemented! {},
            Term::InsertedMeta(_, _, _) => unimplemented! {},
            Term::Ann(_, _, _) => unimplemented! {},
            Term::Let(_, _, _, _, _) => unimplemented! {},
            Term::Universe(_) => unimplemented! {},
            Term::FunType(_, _, _, _) => unimplemented! {},
            Term::FunLit(_, _, _) => unimplemented! {},
            Term::FunApp(_, _, _) => unimplemented! {},
            Term::RecordType(_, _, _) => unimplemented! {},
            Term::RecordLit(_, _, _) => unimplemented! {},
            Term::RecordProj(_, _, _) => unimplemented! {},
            Term::ArrayLit(_, _) => unimplemented! {},
            Term::FormatRecord(_, labels, formats) => {
                println!(
                    r#"generate a new rust struct as a "representation" of that format, and a function to to decode that struct."#
                );
                let mut fields = Vec::with_capacity(labels.len());
                for (label, format) in labels.iter().copied().zip(formats.iter()) {
                    let ty = self.compile_rep(format);
                    fields.push((label, ty));
                }
                let r#struct = Struct { fields };
                dbg!(r#struct);
            }
            Term::FormatCond(_, _, _, _) => unimplemented! {},
            Term::FormatOverlap(_, _, _) => unimplemented! {},
            Term::Prim(_, _) => unimplemented! {},
            Term::ConstLit(_, _) => unimplemented! {},
            Term::ConstMatch(_, _, _, _) => unimplemented! {},
        }

        Ok(())
    }

    fn compile_rep(&mut self, format: &Term<'arena>) -> Type {
        // We already have `format_repr` but that operates on Values...

        match format {
            Term::ItemVar(_, _) => todo! {},
            Term::LocalVar(_, var) => {
                // TODO: How to resolve var?
                match self.rigid_exprs.get_index(*var) {
                    Some(value) => dbg!(value),
                    None => panic!("invalid rigid var"),
                };
                todo!("rigid var");
            }
            Term::MetaVar(_, _) => todo! {},
            Term::InsertedMeta(_, _, _) => todo! {},
            Term::Ann(_, _, _) => todo! {},
            Term::Let(_, _, _, _, _) => todo! {},
            Term::Universe(_) => todo! {},
            Term::FunType(_, _, _, _) => todo! {},
            Term::FunLit(_, _, _) => todo! {},
            Term::FunApp(
                _,
                Term::FunApp(_, Term::Prim(_, Prim::FormatArray32), len),
                ele_format,
            ) => {
                // How to compile function application?
                // In the simple case we're calling a primitive function, so know how to compile
                // that..?
                // let len = self.compile_rep(len); // ignore len for now as we might not know it
                // at this point
                let item_type = self.compile_rep(ele_format);
                // dbg!(("array32", len));
                Type::Vec(Box::new(item_type))
            }
            Term::FunApp(_, head, input) => todo!("fun app"),
            Term::RecordType(_, _, _) => todo! {},
            Term::RecordLit(_, _, _) => todo! {},
            Term::RecordProj(_, _, _) => todo! {},
            Term::ArrayLit(_, _) => todo! {},
            Term::FormatRecord(_, _, _) => todo! {},
            Term::FormatCond(_, _, _, _) => todo! {},
            Term::FormatOverlap(_, _, _) => todo! {},
            Term::Prim(_, prim) => Self::compile_prim(prim),
            Term::ConstLit(_, _) => todo! {},
            Term::ConstMatch(_, _, _, _) => todo! {},
        }
    }

    fn compile_prim(prim: &Prim) -> Type {
        match prim {
            Prim::FormatU8 => Type::U8,
            Prim::FormatU16Be | Prim::FormatU16Le => Type::U16,
            Prim::FormatU32Be | Prim::FormatU32Le => Type::U32,
            Prim::FormatU64Be | Prim::FormatU64Le => Type::U64,
            Prim::FormatS8 => Type::I8,
            Prim::FormatS16Be | Prim::FormatS16Le => Type::I16,
            Prim::FormatS32Be | Prim::FormatS32Le => Type::I32,
            Prim::FormatS64Be | Prim::FormatS64Le => Type::I64,
            _ => todo! {},
        }
    }
}
