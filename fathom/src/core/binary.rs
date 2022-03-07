//! Binary semantics of the data description language

use std::collections::HashMap;
use std::io::{self, Read, Seek, SeekFrom};
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Head, Value};
use crate::core::{Const, Prim};
use crate::env::{EnvLen, SliceEnv};

pub struct Context<'arena, 'env> {
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    pending_formats: Vec<(u64, ArcValue<'arena>)>,
}

pub struct RefData<'arena> {
    pub r#type: ArcValue<'arena>,
    pub expr: ArcValue<'arena>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>) -> Context<'arena, 'env> {
        Context {
            flexible_exprs,
            pending_formats: Vec::new(),
        }
    }

    fn elim_context(&self) -> semantics::ElimContext<'arena, '_> {
        semantics::ElimContext::new(self.flexible_exprs)
    }

    fn conversion_context(&self) -> semantics::ConversionContext<'arena, '_> {
        semantics::ConversionContext::new(EnvLen::new(), self.flexible_exprs)
    }

    // TODO: allow refs to be streamed
    pub fn read_entrypoint(
        &mut self,
        reader: &mut dyn SeekRead,
        format: ArcValue<'arena>,
    ) -> io::Result<HashMap<u64, RefData<'arena>>> {
        let initial_pos = reader.stream_position()?;
        let mut refs = HashMap::<_, RefData<'_>>::new();

        // Parse the entrypoint from the beginning start of the binary data
        self.pending_formats.push((0, format));

        // NOTE: This could lead to non-termination if we aren't careful!
        while let Some((pos, format)) = self.pending_formats.pop() {
            use std::collections::hash_map::Entry;

            let format_repr = self.elim_context().apply_repr(&format);

            match refs.entry(pos) {
                Entry::Occupied(entry) => {
                    let RefData { r#type, .. } = entry.get();
                    // Ensure that the format's representation type matches the
                    // type of the stored reference.
                    if !self.conversion_context().is_equal(r#type, &format_repr) {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "ref is occupied by an incompatible type",
                        ));
                    }
                }
                Entry::Vacant(entry) => {
                    // Seek to current current ref location
                    reader.seek(SeekFrom::Start(pos))?;
                    // Parse the data at that location
                    let expr = self.read_format(reader, &format)?;
                    // Record the data in the `refs` hashmap
                    entry.insert(RefData {
                        r#type: format_repr,
                        expr,
                    });
                }
            }
        }

        // Reset reader back to the start
        reader.seek(SeekFrom::Start(initial_pos))?;

        Ok(refs)
    }

    fn read_format(
        &mut self,
        reader: &mut dyn SeekRead,
        format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        use crate::core::semantics::Elim::Fun;

        match self.elim_context().force(format).as_ref() {
            Value::Stuck(Head::Prim(prim), slice) => match (*prim, &slice[..]) {
                (Prim::FormatSucceed, [_, Fun(r#elem)]) => Ok(r#elem.clone()),
                (Prim::FormatU8, []) => read_const(reader, Const::U8, read_u8),
                (Prim::FormatU16Be, []) => read_const(reader, Const::U16, read_u16be),
                (Prim::FormatU16Le, []) => read_const(reader, Const::U16, read_u16le),
                (Prim::FormatU32Be, []) => read_const(reader, Const::U32, read_u32be),
                (Prim::FormatU32Le, []) => read_const(reader, Const::U32, read_u32le),
                (Prim::FormatU64Be, []) => read_const(reader, Const::U64, read_u64be),
                (Prim::FormatU64Le, []) => read_const(reader, Const::U64, read_u64le),
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
                (Prim::FormatArray8, [Fun(len), Fun(elem)]) => self.read_array(reader, len, elem),
                (Prim::FormatArray16, [Fun(len), Fun(elem)]) => self.read_array(reader, len, elem),
                (Prim::FormatArray32, [Fun(len), Fun(elem)]) => self.read_array(reader, len, elem),
                (Prim::FormatArray64, [Fun(len), Fun(elem)]) => self.read_array(reader, len, elem),
                (Prim::FormatLink8, [Fun(pos), Fun(offset), Fun(elem)]) => {
                    self.read_link(pos, offset, elem)
                }
                (Prim::FormatLink16, [Fun(pos), Fun(offset), Fun(elem)]) => {
                    self.read_link(pos, offset, elem)
                }
                (Prim::FormatLink32, [Fun(pos), Fun(offset), Fun(elem)]) => {
                    self.read_link(pos, offset, elem)
                }
                (Prim::FormatLink64, [Fun(pos), Fun(offset), Fun(elem)]) => {
                    self.read_link(pos, offset, elem)
                }
                (Prim::FormatStreamPos, []) => read_stream_pos(reader),
                (Prim::FormatFail, []) => {
                    Err(io::Error::new(io::ErrorKind::Other, "parse failure"))
                }
                _ => Err(io::Error::new(io::ErrorKind::Other, "invalid format")),
            },
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

                Ok(Arc::new(Value::RecordIntro(labels, exprs)))
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

                Ok(Arc::new(Value::RecordIntro(labels, exprs)))
            }

            Value::Stuck(Head::RigidVar(_), _)
            | Value::Stuck(Head::FlexibleVar(_), _)
            | Value::Universe
            | Value::FunType(_, _, _)
            | Value::FunIntro(_, _)
            | Value::RecordType(_, _)
            | Value::RecordIntro(_, _)
            | Value::ArrayIntro(_)
            | Value::Const(_) => Err(io::Error::new(io::ErrorKind::Other, "invalid format")),
        }
    }

    fn read_array(
        &mut self,
        reader: &mut dyn SeekRead,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        let (len, mut elem_exprs) = match self.elim_context().force(len).as_ref() {
            Value::Const(Const::U8(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U16(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U32(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            Value::Const(Const::U64(len)) => (*len as u64, Vec::with_capacity(*len as usize)),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid array length")),
        };

        for _ in 0..len {
            let expr = self.read_format(reader, elem_format)?;
            elem_exprs.push(expr);
        }

        Ok(Arc::new(Value::ArrayIntro(elem_exprs)))
    }

    pub fn read_link(
        &mut self,
        pos: &ArcValue<'arena>,
        offset: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> io::Result<ArcValue<'arena>> {
        let pos = match self.elim_context().force(pos).as_ref() {
            Value::Const(Const::Pos(pos)) => *pos,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid link pos")),
        };
        let offset = match self.elim_context().force(offset).as_ref() {
            Value::Const(Const::U8(len)) => *len as u64,
            Value::Const(Const::U16(len)) => *len as u64,
            Value::Const(Const::U32(len)) => *len as u64,
            Value::Const(Const::U64(len)) => *len as u64,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid link offset")),
        };

        let r#ref = u64::checked_add(pos, offset)
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "overflowing link"))?;

        self.pending_formats.push((r#ref, elem_format.clone()));

        Ok(Arc::new(Value::Const(Const::Ref(r#ref))))
    }
}

pub trait SeekRead: Seek + Read {}

impl<T: Seek + Read> SeekRead for T {}

fn read_stream_pos<'arena>(reader: &mut dyn SeekRead) -> io::Result<ArcValue<'arena>> {
    let pos = reader.stream_position()?;
    Ok(Arc::new(Value::Const(Const::Pos(pos))))
}

fn read_const<'arena, T>(
    reader: &mut dyn SeekRead,
    wrap_const: fn(T) -> Const,
    read: fn(&mut dyn SeekRead) -> io::Result<T>,
) -> io::Result<ArcValue<'arena>> {
    let data = read(reader)?;
    Ok(Arc::new(Value::Const(wrap_const(data))))
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
