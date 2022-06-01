//! Binary semantics of the data description language

use std::collections::HashMap;
use std::io::{self, Read, Seek, SeekFrom};
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Elim, Head, Value};
use crate::core::{Const, Prim, UIntStyle};
use crate::env::{EnvLen, SliceEnv};

pub struct Context<'arena, 'env> {
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    pending_formats: Vec<(u64, ArcValue<'arena>)>,
    cached_refs: HashMap<u64, Vec<ParsedRef<'arena>>>,
}

pub struct ParsedRef<'arena> {
    /// The format that this reference was parsed with
    // Invariant: `format : Format`
    pub format: ArcValue<'arena>,
    /// The expression that was parsed for this reference
    // Invariant: `expr : Repr format`
    pub expr: ArcValue<'arena>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>) -> Context<'arena, 'env> {
        Context {
            flexible_exprs,
            pending_formats: Vec::new(),
            cached_refs: HashMap::new(),
        }
    }

    fn elim_context(&self) -> semantics::ElimContext<'arena, 'env> {
        semantics::ElimContext::new(self.flexible_exprs)
    }

    fn conversion_context(&self) -> semantics::ConversionContext<'arena, 'env> {
        semantics::ConversionContext::new(EnvLen::new(), self.flexible_exprs)
    }

    // TODO: allow refs to be streamed
    pub fn read_entrypoint(
        mut self,
        reader: &mut dyn SeekRead,
        format: ArcValue<'arena>,
    ) -> io::Result<HashMap<u64, Vec<ParsedRef<'arena>>>> {
        // Parse the entrypoint from the beginning start of the binary data
        self.pending_formats.push((0, format));

        while let Some((pos, format)) = self.pending_formats.pop() {
            self.read_cached_ref(reader, pos, &format)?;
        }

        Ok(self.cached_refs)
    }

    fn read_format(
        &mut self,
        reader: &mut dyn SeekRead,
        format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        match self.elim_context().force(format).as_ref() {
            Value::Stuck(Head::Prim(prim), slice) => self.read_prim(reader, *prim, slice),
            Value::FormatRecord(labels, formats) => {
                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) =
                    self.elim_context().split_telescope(formats)
                {
                    let expr = self.read_format(reader, &format)?;
                    exprs.push(expr.clone());
                    formats = next_formats(expr);
                }

                Ok(Arc::new(Value::RecordLit(labels, exprs)))
            }
            Value::FormatCond(_label, format, cond) => {
                let value = self.read_format(reader, &format)?;
                let cond_res = self.elim_context().apply_closure(cond, value.clone());

                match *cond_res {
                    Value::ConstLit(Const::Bool(true)) => Ok(value),
                    Value::ConstLit(Const::Bool(false)) => {
                        // TODO: better user experience for this case
                        Err(io::Error::new(io::ErrorKind::Other, "cond failed"))
                    }
                    _ => {
                        // This shouldn't happen since we check that the cond type is Bool earlier
                        Err(io::Error::new(io::ErrorKind::Other, "expected bool"))
                    }
                }
            }
            Value::FormatOverlap(labels, formats) => {
                let initial_pos = reader.stream_position()?;
                let mut max_pos = initial_pos;

                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) =
                    self.elim_context().split_telescope(formats)
                {
                    // Reset the stream to the start
                    reader.seek(SeekFrom::Start(initial_pos))?;

                    let expr = self.read_format(reader, &format)?;
                    exprs.push(expr.clone());
                    formats = next_formats(expr);

                    // Update the max position
                    max_pos = std::cmp::max(max_pos, reader.stream_position()?);
                }

                // Seek to the maximum stream length
                reader.seek(SeekFrom::Start(max_pos))?;

                Ok(Arc::new(Value::RecordLit(labels, exprs)))
            }

            Value::Stuck(Head::RigidVar(_), _)
            | Value::Stuck(Head::FlexibleVar(_), _)
            | Value::Universe
            | Value::FunType(_, _, _)
            | Value::FunLit(_, _)
            | Value::RecordType(_, _)
            | Value::RecordLit(_, _)
            | Value::ArrayLit(_)
            | Value::ConstLit(_) => Err(io::Error::new(io::ErrorKind::Other, "invalid format")),
        }
    }

    #[rustfmt::skip]
    fn read_prim(
        &mut self,
        reader: &mut dyn SeekRead,
        prim: Prim,
        slice: &[Elim<'arena>],
    ) -> io::Result<ArcValue<'arena>> {
        use crate::core::semantics::Elim::FunApp;

        match (prim, &slice[..]) {
            (Prim::FormatU8, []) => read_const(reader, |num| Const::U8(num, UIntStyle::Decimal), read_u8),
            (Prim::FormatU16Be, []) => read_const(reader, |num| Const::U16(num, UIntStyle::Decimal), read_u16be),
            (Prim::FormatU16Le, []) => read_const(reader, |num| Const::U16(num, UIntStyle::Decimal), read_u16le),
            (Prim::FormatU32Be, []) => read_const(reader, |num| Const::U32(num, UIntStyle::Decimal), read_u32be),
            (Prim::FormatU32Le, []) => read_const(reader, |num| Const::U32(num, UIntStyle::Decimal), read_u32le),
            (Prim::FormatU64Be, []) => read_const(reader, |num| Const::U64(num, UIntStyle::Decimal), read_u64be),
            (Prim::FormatU64Le, []) => read_const(reader, |num| Const::U64(num, UIntStyle::Decimal), read_u64le),
            (Prim::FormatS8, []) => read_const(reader, Const::S8, read_s8),
            (Prim::FormatS16Be, []) => read_const(reader, Const::S16, read_s16be),
            (Prim::FormatS16Le, []) => read_const(reader, Const::S16, read_s16le),
            (Prim::FormatS32Be, []) => read_const(reader, Const::S32, read_s32be),
            (Prim::FormatS32Le, []) => read_const(reader, Const::S32, read_s32le),
            (Prim::FormatS64Be, []) => read_const(reader, Const::S64, read_s64be),
            (Prim::FormatS64Le, []) => read_const(reader, Const::S64, read_s64le),
            (Prim::FormatF32Be, []) => read_const(reader, Const::F32, read_f32be),
            (Prim::FormatF32Le, []) => read_const(reader, Const::F32, read_f32le),
            (Prim::FormatF64Be, []) => read_const(reader, Const::F64, read_f64be),
            (Prim::FormatF64Le, []) => read_const(reader, Const::F64, read_f64le),
            (Prim::FormatArray8, [FunApp(len), FunApp(elem_format)]) => self.read_array(reader, len, elem_format),
            (Prim::FormatArray16, [FunApp(len), FunApp(elem_format)]) => self.read_array(reader, len, elem_format),
            (Prim::FormatArray32, [FunApp(len), FunApp(elem_format)]) => self.read_array(reader, len, elem_format),
            (Prim::FormatArray64, [FunApp(len), FunApp(elem_format)]) => self.read_array(reader, len, elem_format),
            (Prim::FormatLink, [FunApp(pos), FunApp(elem_format)]) => self.read_link(pos, elem_format),
            (Prim::FormatDeref, [FunApp(elem_format), FunApp(r#ref)]) => self.read_deref(reader, elem_format, r#ref),
            (Prim::FormatStreamPos, []) => read_stream_pos(reader),
            (Prim::FormatSucceed, [_, FunApp(elem)]) => Ok(elem.clone()),
            (Prim::FormatFail, []) => Err(io::Error::new(io::ErrorKind::Other, "parse failure")),
            (Prim::FormatUnwrap, [_, FunApp(option)]) => match option.match_prim_spine() {
                Some((Prim::OptionSome, [FunApp(elem)])) => Ok(elem.clone()),
                Some((Prim::OptionNone, [])) => Err(io::Error::new(io::ErrorKind::Other, "unwrapped none")),
                _ => Err(io::Error::new(io::ErrorKind::Other, "invalid option")),
            },
            _ => Err(io::Error::new(io::ErrorKind::Other, "invalid format")),
        }
    }

    fn read_array(
        &mut self,
        reader: &mut dyn SeekRead,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        let len = match self.elim_context().force(len).as_ref() {
            Value::ConstLit(Const::U8(len, _)) => *len as u64,
            Value::ConstLit(Const::U16(len, _)) => *len as u64,
            Value::ConstLit(Const::U32(len, _)) => *len as u64,
            Value::ConstLit(Const::U64(len, _)) => *len as u64,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid array length")),
        };

        let elem_exprs = (0..len)
            .map(|_| self.read_format(reader, elem_format))
            .collect::<Result<_, _>>()?;

        Ok(Arc::new(Value::ArrayLit(elem_exprs)))
    }

    pub fn read_link(
        &mut self,
        pos: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        let pos = match self.elim_context().force(pos).as_ref() {
            Value::ConstLit(Const::Pos(pos)) => *pos,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid link pos")),
        };

        self.pending_formats.push((pos, elem_format.clone()));

        Ok(Arc::new(Value::ConstLit(Const::Ref(pos))))
    }

    fn read_deref(
        &mut self,
        reader: &mut dyn SeekRead,
        format: &ArcValue<'arena>,
        r#ref: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        let pos = match self.elim_context().force(r#ref).as_ref() {
            Value::ConstLit(Const::Ref(pos)) => *pos,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "invalid format reference",
                ))
            }
        };

        self.read_cached_ref(reader, pos, format)
    }

    fn lookup_cached_ref<'context>(
        &'context self,
        pos: u64,
        format: &ArcValue<'_>,
    ) -> Option<&'context ParsedRef<'arena>> {
        // NOTE: The number of calls to `semantics::ConversionContext::is_equal`
        // when looking up cached references is a bit of a pain. If this ever
        // becomes a problem we could improve performance by pre-allocating a
        // `ParsedRef` in the cache during `read_link`, and storing the index of
        // that parsed reference alongside the position in `Const::Ref`.

        (self.cached_refs.get(&pos)?.iter())
            .find(|r| self.conversion_context().is_equal(&r.format, &format))
    }

    fn read_cached_ref(
        &mut self,
        reader: &mut dyn SeekRead,
        pos: u64,
        format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        if let Some(parsed_ref) = self.lookup_cached_ref(pos, &format) {
            return Ok(parsed_ref.expr.clone());
        }

        let initial_pos = reader.stream_position()?;

        // Seek to current current ref location
        reader.seek(SeekFrom::Start(pos))?;
        // Parse the data at that location
        let expr = self.read_format(reader, &format)?;
        // Reset reader back to the original position
        reader.seek(SeekFrom::Start(initial_pos))?;

        // We might have parsed the current reference during the above call to
        // `read_format`. It's unclear if this could ever happen in practice,
        // especially without succumbing to non-termination, but we'll panic
        // here just in case.
        if let Some(_) = self.lookup_cached_ref(pos, &format) {
            panic!("recursion found when storing cached reference {}", pos);
        }

        // Store the parsed reference in the reference cache
        self.cached_refs
            .entry(pos)
            .or_insert(Vec::with_capacity(1))
            .push(ParsedRef {
                format: format.clone(),
                expr: expr.clone(),
            });

        Ok(expr)
    }
}

pub trait SeekRead: Seek + Read {}

impl<T: Seek + Read> SeekRead for T {}

fn read_stream_pos<'arena>(reader: &mut dyn SeekRead) -> io::Result<ArcValue<'arena>> {
    let pos = reader.stream_position()?;
    Ok(Arc::new(Value::ConstLit(Const::Pos(pos))))
}

fn read_const<'arena, T>(
    reader: &mut dyn SeekRead,
    wrap_const: fn(T) -> Const,
    read: fn(&mut dyn SeekRead) -> io::Result<T>,
) -> io::Result<ArcValue<'arena>> {
    let data = read(reader)?;
    Ok(Arc::new(Value::ConstLit(wrap_const(data))))
}

fn read_u8(reader: &mut dyn SeekRead) -> io::Result<u8> {
    let [byte] = read_array(reader)?;
    Ok(byte)
}

fn read_s8(reader: &mut dyn SeekRead) -> io::Result<i8> {
    let [byte] = read_array(reader)?;
    Ok(byte as i8)
}

fn read_array<const N: usize>(reader: &mut dyn SeekRead) -> io::Result<[u8; N]> {
    let mut buf = [0; N];
    reader.read_exact(&mut buf)?;
    Ok(buf)
}

/// Generates a function that reads a multi-byte primitive.
macro_rules! read_multibyte_prim {
    ($read_multibyte_prim:ident, $from_bytes:ident, $T:ident) => {
        fn $read_multibyte_prim(reader: &mut dyn SeekRead) -> io::Result<$T> {
            let data = read_array(reader)?;
            Ok($T::$from_bytes(data))
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
