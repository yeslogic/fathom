//! Binary semantics of the data description language

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::slice::SliceIndex;
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Elim, Head, Value};
use crate::core::{Const, Prim, Span, UIntStyle};
use crate::env::{EnvLen, SliceEnv};

#[derive(Clone, Debug)]
pub enum ReadError {
    InvalidFormat(Span),
    InvalidValue(Span),
    UnknownItem,
    UnwrappedNone(Span),
    ReadFailFormat,
    CondFailure(Span),
    SetOffsetBeforeStartOfBuffer { offset: usize },
    SetOffsetAfterEndOfBuffer { offset: Option<usize> },
    UnexpectedEndOfBuffer,
    PositionOverflow,
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadError::InvalidFormat(_) => f.write_str("invalid format"),
            ReadError::InvalidValue(_) => f.write_str("invalid value"),
            ReadError::UnwrappedNone(_) => f.write_str("unwrapped none"),
            ReadError::UnknownItem => f.write_str("unknown item"),
            ReadError::ReadFailFormat => f.write_str("read a fail format"),
            ReadError::CondFailure(_) => f.write_str("conditional format failed"),
            ReadError::SetOffsetBeforeStartOfBuffer { .. } => {
                f.write_str("attempt to set buffer offset before the start of the buffer")
            }
            ReadError::SetOffsetAfterEndOfBuffer { .. } => {
                f.write_str("attempt to set buffer offset after the end of the buffer")
            }
            ReadError::UnexpectedEndOfBuffer => f.write_str("unexpected end of buffer"),
            ReadError::PositionOverflow => f.write_str("position overflow"),
        }
    }
}

impl std::error::Error for ReadError {}

/// A buffer that starts at an offset into a larger buffer.
///
/// ```text
///  ┌─────────────────────────┬─────────────────────────────────────────────────────────────────────┐
///  │                         │                                data                                 │
///  │                         ├─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬ ─ ─ ─ ─ ─ ─ ─ ┬─────┤
///  │                         │  ?  │  ?  │  ?  │  ?  │  ?  │  ?  │  ?  │  ?  │               │  ?  |
///  └─────────────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴ ─ ─ ─ ─ ─ ─ ─ ┴─────┘
///  0           ...           n    n+1   n+2   n+3   n+4   n+5   n+6   n+7   n+8     ...    n+m-1  n+m
/// -n           ...           0     1     2     3     4     5     6     7     8      ...      m-1   m
///  │                         │                                                                     │
///  └─ Buffer::start_offset ─►└─────────────────────── Buffer::remaining_len ──────────────────────►│
///  │                         │                                   │                                 │
///  │                         └── BufferReader::relative_offset ─►│                                 │
///  │                                                             │                                 │
///  └───────────────────── BufferReader::offset ─────────────────►└── BufferReader::remaining_len ─►│
/// ```
#[derive(Copy, Clone)]
pub struct Buffer<'data> {
    /// Offset from the starting position.
    start_offset: usize,
    /// A slice of data, starting from an offset from the start of a larger buffer.
    data: &'data [u8],
}

impl<'data> Buffer<'data> {
    /// Create a new buffer at an offset into a base buffer.
    pub fn new(start_offset: usize, data: &'data [u8]) -> Buffer<'data> {
        Buffer { start_offset, data }
    }

    /// The offset from the start of the base buffer.
    pub fn start_offset(&self) -> usize {
        self.start_offset
    }

    /// Total length of the buffer, including the base buffer.
    pub fn len(&self) -> Result<usize, ReadError> {
        usize::checked_add(self.start_offset, self.data.len()).ok_or(ReadError::PositionOverflow)
    }

    /// Remaining number of bytes in the buffer.
    pub fn remaining_len(&self) -> usize {
        self.data.len()
    }

    /// Return a buffer limited to the supplied length.
    pub fn with_remaining_len(&self, len: usize) -> Result<Buffer<'data>, ReadError> {
        Ok(Buffer {
            start_offset: self.start_offset,
            data: self.get_relative(..len)?,
        })
    }

    /// Get a slice of the bytes in the buffer, relative to the start of the buffer.
    fn get_relative<I: SliceIndex<[u8]>>(&self, index: I) -> Result<&'data I::Output, ReadError> {
        self.data.get(index).ok_or(ReadError::UnexpectedEndOfBuffer)
    }

    /// Create a reader at the start of the buffer.
    pub fn reader(&self) -> BufferReader<'data> {
        BufferReader::from(*self)
    }

    /// Create a reader at an offset measured from the start of the base buffer.
    pub fn reader_with_offset(&self, offset: usize) -> Result<BufferReader<'data>, ReadError> {
        let mut reader = self.reader();
        reader.set_offset(offset)?;
        Ok(reader)
    }
}

impl<'data> From<&'data [u8]> for Buffer<'data> {
    fn from(data: &'data [u8]) -> Buffer<'data> {
        Buffer {
            start_offset: 0,
            data,
        }
    }
}

/// Stateful reader with a backing buffer.
#[derive(Clone)]
pub struct BufferReader<'data> {
    /// Offset relative to the start of the buffer.
    // Invariant: self.relative_offset <= self.buffer.remaining_len()
    relative_offset: usize,
    /// Backing buffer.
    buffer: Buffer<'data>,
}

impl<'data> BufferReader<'data> {
    /// Return the underlying buffer.
    pub fn buffer(&self) -> &Buffer<'data> {
        &self.buffer
    }

    /// The offset from the start of the underlying buffer.
    pub fn relative_offset(&self) -> usize {
        self.relative_offset
    }

    /// The offset from the start position.
    pub fn offset(&self) -> Result<usize, ReadError> {
        usize::checked_add(self.buffer.start_offset, self.relative_offset)
            .ok_or(ReadError::PositionOverflow)
    }

    /// Remaining number of bytes from the current position to the end of the buffer.
    pub fn remaining_len(&self) -> usize {
        self.buffer.remaining_len() - self.relative_offset
    }

    /// Return a buffer of the remaining data from the current relative offset.
    pub fn remaining_buffer(&self) -> Result<Buffer<'data>, ReadError> {
        Ok(Buffer::new(
            self.offset()?,
            self.buffer.get_relative(self.relative_offset..)?,
        ))
    }

    /// Set the offset of the reader relative to the start of the backing buffer.
    pub fn set_relative_offset(&mut self, relative_offset: usize) -> Result<(), ReadError> {
        (relative_offset <= self.buffer.remaining_len())
            .then(|| self.relative_offset = relative_offset)
            .ok_or(ReadError::SetOffsetAfterEndOfBuffer {
                offset: self.buffer.start_offset.checked_add(relative_offset),
            })
    }

    /// Set the offset of the reader relative to the start position.
    pub fn set_offset(&mut self, offset: usize) -> Result<(), ReadError> {
        usize::checked_sub(offset, self.buffer.start_offset)
            .ok_or_else(|| ReadError::SetOffsetBeforeStartOfBuffer { offset })
            .and_then(|relative_offset| self.set_relative_offset(relative_offset))
    }

    /// Get a slice of the bytes in the buffer, relative to the current offset in the buffer.
    fn get_relative<I: SliceIndex<[u8]>>(&self, index: I) -> Result<&'data I::Output, ReadError> {
        let data = self.buffer.get_relative(self.relative_offset..)?;
        data.get(index).ok_or(ReadError::UnexpectedEndOfBuffer)
    }

    /// Read a byte and advance the reader.
    pub fn read_byte(&mut self) -> Result<u8, ReadError> {
        let first = self.buffer.get_relative(self.relative_offset)?;
        self.relative_offset += 1;
        Ok(*first)
    }

    /// Read an array of bytes and advance the offset into the buffer.
    pub fn read_byte_array<const N: usize>(&mut self) -> Result<&'data [u8; N], ReadError> {
        let slice = self.get_relative(..N)?;
        // SAFETY: slice points to [u8; N]? Yes it's [u8] of length N (checked by BufferReader::get_relative)
        let array = unsafe { &*(slice.as_ptr() as *const [u8; N]) };
        self.relative_offset += N;
        Ok(array)
    }
}

impl<'data> From<Buffer<'data>> for BufferReader<'data> {
    fn from(buffer: Buffer<'data>) -> BufferReader<'data> {
        BufferReader {
            relative_offset: 0,
            buffer,
        }
    }
}

pub struct Context<'arena, 'env, 'data> {
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    initial_buffer: Buffer<'data>,
    pending_formats: Vec<(usize, ArcValue<'arena>)>,
    cached_refs: HashMap<usize, Vec<ParsedRef<'arena>>>,
}

pub struct ParsedRef<'arena> {
    /// The format that this reference was parsed with
    // Invariant: `format : Format`
    pub format: ArcValue<'arena>,
    /// The expression that was parsed for this reference
    // Invariant: `expr : Repr format`
    pub expr: ArcValue<'arena>,
}

impl<'arena, 'env, 'data> Context<'arena, 'env, 'data> {
    pub fn new(
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
        initial_buffer: Buffer<'data>,
    ) -> Context<'arena, 'env, 'data> {
        Context {
            item_exprs,
            flexible_exprs,
            initial_buffer,
            pending_formats: Vec::new(),
            cached_refs: HashMap::new(),
        }
    }

    fn elim_context(&self) -> semantics::ElimContext<'arena, 'env> {
        semantics::ElimContext::new(self.item_exprs, self.flexible_exprs)
    }

    fn conversion_context(&self) -> semantics::ConversionContext<'arena, 'env> {
        semantics::ConversionContext::new(self.item_exprs, EnvLen::new(), self.flexible_exprs)
    }

    pub fn read_entrypoint(
        mut self,
        format: ArcValue<'arena>,
    ) -> Result<HashMap<usize, Vec<ParsedRef<'arena>>>, ReadError> {
        // Parse the entrypoint from the start of the binary data
        self.pending_formats
            .push((self.initial_buffer.start_offset(), format));

        while let Some((pos, format)) = self.pending_formats.pop() {
            self.lookup_or_read_ref(pos, &format)?;
        }

        Ok(self.cached_refs)
    }

    fn read_format(
        &mut self,
        reader: &mut BufferReader<'data>,
        format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        match self.elim_context().force(format).as_ref() {
            Value::Stuck(span, Head::Prim(prim), slice) => {
                self.read_prim(reader, *prim, slice, *span)
            }
            Value::FormatRecord(span, labels, formats) => {
                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) =
                    self.elim_context().split_telescope(formats)
                {
                    let expr = self.read_format(reader, &format)?;
                    exprs.push(expr.clone());
                    formats = next_formats(expr);
                }

                Ok(Arc::new(Value::RecordLit(*span, labels, exprs)))
            }
            Value::FormatCond(_span, _label, format, cond) => {
                let value = self.read_format(reader, &format)?;
                let cond_res = self.elim_context().apply_closure(cond, value.clone());

                match *cond_res {
                    Value::ConstLit(_, Const::Bool(true)) => Ok(value),
                    Value::ConstLit(_, Const::Bool(false)) => {
                        Err(ReadError::CondFailure(cond.span()))
                    }
                    _ => {
                        // This shouldn't happen since we check that the cond type is Bool earlier
                        Err(ReadError::InvalidValue(cond_res.span()))
                    }
                }
            }
            Value::FormatOverlap(span, labels, formats) => {
                let mut max_relative_offset = reader.relative_offset();

                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) =
                    self.elim_context().split_telescope(formats)
                {
                    let mut reader = reader.clone();

                    let expr = self.read_format(&mut reader, &format)?;
                    exprs.push(expr.clone());
                    formats = next_formats(expr);

                    max_relative_offset =
                        std::cmp::max(max_relative_offset, reader.relative_offset());
                }

                // Seek to the maximum stream length. unwrap is safe due to that offset being
                // reached in loop above.
                reader.set_relative_offset(max_relative_offset).unwrap();

                Ok(Arc::new(Value::RecordLit(*span, labels, exprs)))
            }

            Value::Stuck(span, Head::RigidVar(_), _)
            | Value::Stuck(span, Head::FlexibleVar(_), _)
            | Value::Universe(span)
            | Value::FunType(span, _, _, _)
            | Value::FunLit(span, _, _)
            | Value::RecordType(span, _, _)
            | Value::RecordLit(span, _, _)
            | Value::ArrayLit(span, _)
            | Value::ConstLit(span, _) => Err(ReadError::InvalidFormat(*span)),
        }
    }

    #[rustfmt::skip]
    fn read_prim(
        &mut self,
        reader: &mut BufferReader<'data>,
        prim: Prim,
        slice: &[Elim<'arena>],
        span: Span,
    ) -> Result<ArcValue<'arena>, ReadError> {
        use crate::core::semantics::Elim::FunApp;

        match (prim, &slice[..]) {
            (Prim::FormatU8, []) => read_const(reader, read_u8, |num| Const::U8(num, UIntStyle::Decimal)),
            (Prim::FormatU16Be, []) => read_const(reader, read_u16be, |num| Const::U16(num, UIntStyle::Decimal)),
            (Prim::FormatU16Le, []) => read_const(reader, read_u16le, |num| Const::U16(num, UIntStyle::Decimal)),
            (Prim::FormatU32Be, []) => read_const(reader, read_u32be, |num| Const::U32(num, UIntStyle::Decimal)),
            (Prim::FormatU32Le, []) => read_const(reader, read_u32le, |num| Const::U32(num, UIntStyle::Decimal)),
            (Prim::FormatU64Be, []) => read_const(reader, read_u64be, |num| Const::U64(num, UIntStyle::Decimal)),
            (Prim::FormatU64Le, []) => read_const(reader, read_u64le, |num| Const::U64(num, UIntStyle::Decimal)),
            (Prim::FormatS8, []) => read_const(reader, read_s8, Const::S8),
            (Prim::FormatS16Be, []) => read_const(reader, read_s16be, Const::S16),
            (Prim::FormatS16Le, []) => read_const(reader, read_s16le, Const::S16),
            (Prim::FormatS32Be, []) => read_const(reader, read_s32be, Const::S32),
            (Prim::FormatS32Le, []) => read_const(reader, read_s32le, Const::S32),
            (Prim::FormatS64Be, []) => read_const(reader, read_s64be, Const::S64),
            (Prim::FormatS64Le, []) => read_const(reader, read_s64le, Const::S64),
            (Prim::FormatF32Be, []) => read_const(reader, read_f32be, Const::F32),
            (Prim::FormatF32Le, []) => read_const(reader, read_f32le, Const::F32),
            (Prim::FormatF64Be, []) => read_const(reader, read_f64be, Const::F64),
            (Prim::FormatF64Le, []) => read_const(reader, read_f64le, Const::F64),
            (Prim::FormatArray8, [FunApp(len), FunApp(format)]) => self.read_array(reader, len, format),
            (Prim::FormatArray16, [FunApp(len), FunApp(format)]) => self.read_array(reader, len, format),
            (Prim::FormatArray32, [FunApp(len), FunApp(format)]) => self.read_array(reader, len, format),
            (Prim::FormatArray64, [FunApp(len), FunApp(format)]) => self.read_array(reader, len, format),
            (Prim::FormatRepeatUntilEnd, [FunApp(format)]) => self.read_repeat_until_end(reader, format),
            (Prim::FormatLimit8, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit16, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit32, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit64, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLink, [FunApp(pos), FunApp(format)]) => self.read_link(pos, format),
            (Prim::FormatDeref, [FunApp(format), FunApp(r#ref)]) => self.read_deref(format, r#ref),
            (Prim::FormatStreamPos, []) => read_stream_pos(reader),
            (Prim::FormatSucceed, [_, FunApp(elem)]) => Ok(elem.clone()),
            (Prim::FormatFail, []) => Err(ReadError::ReadFailFormat),
            (Prim::FormatUnwrap, [_, FunApp(option)]) => match option.match_prim_spine() {
                Some((Prim::OptionSome, [FunApp(elem)])) => Ok(elem.clone()),
                Some((Prim::OptionNone, [])) => Err(ReadError::UnwrappedNone(span)),
                _ => Err(ReadError::InvalidValue(span)),
            },
            _ => Err(ReadError::InvalidFormat(span)),
        }
    }

    fn read_array(
        &mut self,
        reader: &mut BufferReader<'data>,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        let len = match self.elim_context().force(len).as_ref() {
            Value::ConstLit(_, Const::U8(len, _)) => u64::from(*len),
            Value::ConstLit(_, Const::U16(len, _)) => u64::from(*len),
            Value::ConstLit(_, Const::U32(len, _)) => u64::from(*len),
            Value::ConstLit(_, Const::U64(len, _)) => u64::from(*len),
            _ => return Err(ReadError::InvalidValue(Span::fixme())),
        };

        let elem_exprs = (0..len)
            .map(|_| self.read_format(reader, elem_format))
            .collect::<Result<_, _>>()?;

        Ok(Arc::new(Value::ArrayLit(elem_format.span(), elem_exprs)))
    }

    fn read_repeat_until_end(
        &mut self,
        reader: &mut BufferReader<'data>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        let mut current_offset = reader.relative_offset();
        let mut elems = Vec::new();

        loop {
            match self.read_format(reader, elem_format) {
                Ok(elem) => {
                    elems.push(elem);
                    current_offset = reader.relative_offset();
                }
                Err(ReadError::UnexpectedEndOfBuffer) => {
                    // unwrap shouldn't panic as we're rewinding to a known good offset
                    // Should this be set to the end of the current buffer?
                    reader.set_relative_offset(current_offset).unwrap();
                    return Ok(Arc::new(Value::ArrayLit(elem_format.span(), elems)));
                }
                Err(err) => return Err(err),
            };
        }
    }

    fn read_limit(
        &mut self,
        reader: &BufferReader<'data>,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        let len = match self.elim_context().force(len).as_ref() {
            Value::ConstLit(_, Const::U8(len, _)) => Some(usize::from(*len)),
            Value::ConstLit(_, Const::U16(len, _)) => Some(usize::from(*len)),
            Value::ConstLit(_, Const::U32(len, _)) => usize::try_from(*len).ok(),
            Value::ConstLit(_, Const::U64(len, _)) => usize::try_from(*len).ok(),
            _ => return Err(ReadError::InvalidValue(Span::fixme())),
        }
        .ok_or(ReadError::PositionOverflow)?;

        let buffer = reader.remaining_buffer()?.with_remaining_len(len)?;

        self.read_format(&mut buffer.reader(), elem_format)
    }

    fn read_link(
        &mut self,
        pos_value: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        let pos = match self.elim_context().force(pos_value).as_ref() {
            Value::ConstLit(_, Const::Pos(pos)) => *pos,
            value => return Err(ReadError::InvalidValue(value.span())),
        };

        self.pending_formats.push((pos, elem_format.clone()));

        Ok(Arc::new(Value::ConstLit(pos_value.span(), Const::Ref(pos))))
    }

    fn read_deref(
        &mut self,
        format: &ArcValue<'arena>,
        r#ref: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        let pos = match self.elim_context().force(r#ref).as_ref() {
            Value::ConstLit(_, Const::Ref(pos)) => *pos,
            value => return Err(ReadError::InvalidValue(value.span())),
        };

        self.lookup_or_read_ref(pos, format)
    }

    fn lookup_ref<'context>(
        &'context self,
        pos: usize,
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

    fn lookup_or_read_ref(
        &mut self,
        pos: usize,
        format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError> {
        if let Some(parsed_ref) = self.lookup_ref(pos, &format) {
            return Ok(parsed_ref.expr.clone());
        }

        // Read the data at the ref location
        let mut reader = self.initial_buffer.reader_with_offset(pos)?;
        let expr = self.read_format(&mut reader, &format)?;

        // We might have parsed the current reference during the above call to
        // `read_format`. It's unclear if this could ever happen in practice,
        // especially without succumbing to non-termination, but we'll panic
        // here just in case.
        if let Some(_) = self.lookup_ref(pos, &format) {
            panic!("recursion found when storing cached reference {}", pos);
        }

        // Store the parsed reference in the reference cache
        self.cached_refs
            .entry(pos)
            .or_insert_with(|| Vec::with_capacity(1))
            .push(ParsedRef {
                format: format.clone(),
                expr: expr.clone(),
            });

        Ok(expr)
    }
}

fn read_stream_pos<'arena, 'data>(
    reader: &mut BufferReader<'data>,
) -> Result<ArcValue<'arena>, ReadError> {
    Ok(Arc::new(Value::ConstLit(
        Span::Empty,
        Const::Pos(reader.offset()?),
    )))
}

fn read_const<'arena, 'data, T>(
    reader: &mut BufferReader<'data>,
    read: fn(&mut BufferReader<'data>) -> Result<T, ReadError>,
    wrap_const: fn(T) -> Const,
) -> Result<ArcValue<'arena>, ReadError> {
    let data = read(reader)?;
    Ok(Arc::new(Value::ConstLit(Span::Empty, wrap_const(data))))
}

fn read_u8<'data>(reader: &mut BufferReader<'data>) -> Result<u8, ReadError> {
    reader.read_byte()
}

fn read_s8<'data>(reader: &mut BufferReader<'data>) -> Result<i8, ReadError> {
    reader.read_byte().map(|b| b as i8)
}

/// Generates a function that reads a multi-byte primitive.
macro_rules! read_multibyte_prim {
    ($read_multibyte_prim:ident, $from_bytes:ident, $T:ident) => {
        fn $read_multibyte_prim<'data>(reader: &mut BufferReader<'data>) -> Result<$T, ReadError> {
            Ok($T::$from_bytes(*reader.read_byte_array()?))
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
