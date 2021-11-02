//! Binary semantics of the data description language

use std::convert::TryInto;
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Head, Value};
use crate::core::{Const, Prim, Term};
use crate::env::{SharedEnv, SliceEnv};

pub struct Context<'arena, 'env> {
    rigid_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(
        rigid_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> Context<'arena, 'env> {
        Context {
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn eval_context(&mut self) -> semantics::EvalContext<'arena, '_> {
        semantics::EvalContext::new(self.rigid_exprs, self.flexible_exprs)
    }

    fn elim_context(&mut self) -> semantics::ElimContext<'arena, '_> {
        semantics::ElimContext::new(self.flexible_exprs)
    }

    pub fn read<'bytes>(
        &mut self,
        format: &Term<'arena>,
        bytes: &'bytes [u8],
    ) -> Option<(ArcValue<'arena>, &'bytes [u8])> {
        let format = self.eval_context().eval(format);
        self.read_value(&format, bytes)
    }

    fn read_value<'bytes>(
        &mut self,
        format: &ArcValue<'arena>,
        mut bytes: &'bytes [u8],
    ) -> Option<(ArcValue<'arena>, &'bytes [u8])> {
        use crate::core::semantics::Elim::Fun;

        match self.elim_context().force(format).as_ref() {
            Value::Stuck(Head::Prim(prim), slice) => match (*prim, &slice[..]) {
                (Prim::FormatU8, []) => read_const(Const::U8, read_u8, bytes),
                (Prim::FormatU16Be, []) => read_const(Const::U16, read_u16be, bytes),
                (Prim::FormatU16Le, []) => read_const(Const::U16, read_u16le, bytes),
                (Prim::FormatU32Be, []) => read_const(Const::U32, read_u32be, bytes),
                (Prim::FormatU32Le, []) => read_const(Const::U32, read_u32le, bytes),
                (Prim::FormatU64Be, []) => read_const(Const::U64, read_u64be, bytes),
                (Prim::FormatU64Le, []) => read_const(Const::U64, read_u64le, bytes),
                (Prim::FormatS8, []) => read_const(Const::S8, read_s8, bytes),
                (Prim::FormatS16Be, []) => read_const(Const::S16, read_s16be, bytes),
                (Prim::FormatS16Le, []) => read_const(Const::S16, read_s16le, bytes),
                (Prim::FormatS32Be, []) => read_const(Const::S32, read_s32be, bytes),
                (Prim::FormatS32Le, []) => read_const(Const::S32, read_s32le, bytes),
                (Prim::FormatS64Be, []) => read_const(Const::S64, read_s64be, bytes),
                (Prim::FormatS64Le, []) => read_const(Const::S64, read_s64le, bytes),
                (Prim::FormatF32Be, []) => read_const(Const::F32, read_f32be, bytes),
                (Prim::FormatF32Le, []) => read_const(Const::F32, read_f32le, bytes),
                (Prim::FormatF64Be, []) => read_const(Const::F64, read_f64be, bytes),
                (Prim::FormatF64Le, []) => read_const(Const::F64, read_f64le, bytes),
                (Prim::FormatArray8, [Fun(len), Fun(elem)]) => self.read_array(len, elem, bytes),
                (Prim::FormatArray16, [Fun(len), Fun(elem)]) => self.read_array(len, elem, bytes),
                (Prim::FormatArray32, [Fun(len), Fun(elem)]) => self.read_array(len, elem, bytes),
                (Prim::FormatArray64, [Fun(len), Fun(elem)]) => self.read_array(len, elem, bytes),
                _ => None,
            },
            Value::FormatRecord(labels, formats) => {
                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) =
                    self.elim_context().split_telescope(formats)
                {
                    let (expr, next_bytes) = self.read_value(&format, bytes)?;
                    exprs.push(expr.clone());
                    bytes = next_bytes;
                    formats = next_formats(expr);
                }

                Some((Arc::new(Value::RecordIntro(labels, exprs)), bytes))
            }

            Value::Stuck(Head::RigidVar(_), _)
            | Value::Stuck(Head::FlexibleVar(_), _)
            | Value::Universe
            | Value::FunType(_, _, _)
            | Value::FunIntro(_, _)
            | Value::RecordType(_, _)
            | Value::RecordIntro(_, _)
            | Value::ArrayIntro(_)
            | Value::Const(_) => None,
        }
    }

    fn read_array<'bytes>(
        &mut self,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
        mut bytes: &'bytes [u8],
    ) -> Option<(ArcValue<'arena>, &'bytes [u8])> {
        let (len, mut elem_exprs) = match self.elim_context().force(len).as_ref() {
            Value::Const(Const::U8(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U16(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U32(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U64(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            _ => return None,
        };

        for _ in 0..len {
            let (expr, next_bytes) = self.read_value(elem_format, bytes)?;
            elem_exprs.push(expr);
            bytes = next_bytes;
        }

        Some((Arc::new(Value::ArrayIntro(elem_exprs)), bytes))
    }
}

fn read_const<'arena, 'bytes, T>(
    wrap_const: fn(T) -> Const,
    read: for<'b> fn(&'b [u8]) -> Option<(T, &'b [u8])>,
    bytes: &'bytes [u8],
) -> Option<(ArcValue<'arena>, &'bytes [u8])> {
    let (data, bytes) = read(bytes)?;
    Some((Arc::new(Value::Const(wrap_const(data))), bytes))
}

fn read_u8(bytes: &[u8]) -> Option<(u8, &[u8])> {
    bytes.split_first().map(|(b0, bytes)| (*b0, bytes))
}

fn read_s8(bytes: &[u8]) -> Option<(i8, &[u8])> {
    bytes.split_first().map(|(b0, bytes)| (*b0 as i8, bytes))
}

fn split_array<const N: usize>(bytes: &[u8]) -> Option<([u8; N], &[u8])> {
    let (left, right) = bytes.split_at(N);
    Some((left.try_into().ok()?, right))
}

/// Generates a function that reads a multi-byte primitive.
macro_rules! read_multibyte_prim {
    ($read_multibyte_prim:ident, $from_bytes:ident, $T:ident) => {
        fn $read_multibyte_prim(bytes: &[u8]) -> Option<($T, &[u8])> {
            let (data, bytes) = split_array(bytes)?;
            Some(($T::$from_bytes(data), bytes))
        }
    };
}

read_multibyte_prim!(read_u16le, from_le_bytes, u16);
read_multibyte_prim!(read_u16be, from_be_bytes, u16);
read_multibyte_prim!(read_u32le, from_le_bytes, u32);
read_multibyte_prim!(read_u32be, from_be_bytes, u32);
read_multibyte_prim!(read_u64le, from_le_bytes, u64);
read_multibyte_prim!(read_u64be, from_be_bytes, u64);
read_multibyte_prim!(read_s16le, from_le_bytes, i16);
read_multibyte_prim!(read_s16be, from_be_bytes, i16);
read_multibyte_prim!(read_s32le, from_le_bytes, i32);
read_multibyte_prim!(read_s32be, from_be_bytes, i32);
read_multibyte_prim!(read_s64le, from_le_bytes, i64);
read_multibyte_prim!(read_s64be, from_be_bytes, i64);
read_multibyte_prim!(read_f32le, from_le_bytes, f32);
read_multibyte_prim!(read_f32be, from_be_bytes, f32);
read_multibyte_prim!(read_f64le, from_le_bytes, f64);
read_multibyte_prim!(read_f64be, from_be_bytes, f64);
