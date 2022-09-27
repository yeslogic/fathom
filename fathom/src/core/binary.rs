//! Binary semantics of the data description language

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Debug;
use std::slice::SliceIndex;
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Elim, Head, Value};
use crate::core::{Const, Prim, UIntStyle};
use crate::env::{EnvLen, SliceEnv};
use crate::source::{Span, Spanned};

#[derive(Clone, Debug)]
pub enum ReadError<'arena> {
    InvalidFormat(Span),
    InvalidValue(Span),
    UnknownItem,
    UnwrappedNone(Span),
    ReadFailFormat(Span),
    CondFailure(Span, ArcValue<'arena>),
    BufferError(Span, BufferError),
}

impl<'arena> fmt::Display for ReadError<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadError::InvalidFormat(_) => f.write_str("invalid format"),
            ReadError::InvalidValue(_) => f.write_str("invalid value"),
            ReadError::UnwrappedNone(_) => f.write_str("unwrapped none"),
            ReadError::UnknownItem => f.write_str("unknown item"),
            ReadError::ReadFailFormat(_) => f.write_str("read a fail format"),
            ReadError::CondFailure(_, _) => f.write_str("conditional format failed"),
            ReadError::BufferError(_, err) => fmt::Display::fmt(&err, f),
        }
    }
}

impl<'arena> std::error::Error for ReadError<'arena> {}

impl<'arena> From<BufferError> for ReadError<'arena> {
    fn from(err: BufferError) -> Self {
        ReadError::BufferError(Span::Empty, err)
    }
}

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
    pub fn len(&self) -> Result<usize, BufferError> {
        usize::checked_add(self.start_offset, self.data.len()).ok_or(BufferError::PositionOverflow)
    }

    /// Remaining number of bytes in the buffer.
    pub fn remaining_len(&self) -> usize {
        self.data.len()
    }

    /// Return a buffer limited to the supplied length.
    pub fn with_remaining_len(&self, len: usize) -> Result<Buffer<'data>, BufferError> {
        Ok(Buffer {
            start_offset: self.start_offset,
            data: self.get_relative(..len)?,
        })
    }

    /// Get a slice of the bytes in the buffer, relative to the start of the buffer.
    fn get_relative<I: SliceIndex<[u8]>>(&self, index: I) -> Result<&'data I::Output, BufferError> {
        self.data
            .get(index)
            .ok_or(BufferError::UnexpectedEndOfBuffer)
    }

    /// Create a reader at the start of the buffer.
    pub fn reader(&self) -> BufferReader<'data> {
        BufferReader::from(*self)
    }

    /// Create a reader at an offset measured from the start of the base buffer.
    pub fn reader_with_offset(&self, offset: usize) -> Result<BufferReader<'data>, BufferError> {
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
    pub fn offset(&self) -> Result<usize, BufferError> {
        usize::checked_add(self.buffer.start_offset, self.relative_offset)
            .ok_or(BufferError::PositionOverflow)
    }

    /// Remaining number of bytes from the current position to the end of the buffer.
    pub fn remaining_len(&self) -> usize {
        self.buffer.remaining_len() - self.relative_offset
    }

    /// Return a buffer of the remaining data from the current relative offset.
    pub fn remaining_buffer(&self) -> Result<Buffer<'data>, BufferError> {
        Ok(Buffer::new(
            self.offset()?,
            self.buffer.get_relative(self.relative_offset..)?,
        ))
    }

    /// Set the offset of the reader relative to the start of the backing buffer.
    pub fn set_relative_offset(&mut self, relative_offset: usize) -> Result<(), BufferError> {
        (relative_offset <= self.buffer.remaining_len())
            .then(|| self.relative_offset = relative_offset)
            .ok_or(BufferError::SetOffsetAfterEndOfBuffer {
                offset: self.buffer.start_offset.checked_add(relative_offset),
            })
    }

    /// Set the offset of the reader relative to the start position.
    pub fn set_offset(&mut self, offset: usize) -> Result<(), BufferError> {
        usize::checked_sub(offset, self.buffer.start_offset)
            .ok_or(BufferError::SetOffsetBeforeStartOfBuffer { offset })
            .and_then(|relative_offset| self.set_relative_offset(relative_offset))
    }

    /// Get a slice of the bytes in the buffer, relative to the current offset in the buffer.
    fn get_relative<I: SliceIndex<[u8]>>(&self, index: I) -> Result<&'data I::Output, BufferError> {
        let data = self.buffer.get_relative(self.relative_offset..)?;
        data.get(index).ok_or(BufferError::UnexpectedEndOfBuffer)
    }

    /// Read a byte and advance the reader.
    pub fn read_byte(&mut self) -> Result<u8, BufferError> {
        let first = self.buffer.get_relative(self.relative_offset)?;
        self.relative_offset += 1;
        Ok(*first)
    }

    /// Read an array of bytes and advance the offset into the buffer.
    pub fn read_byte_array<const N: usize>(&mut self) -> Result<&'data [u8; N], BufferError> {
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

#[derive(Clone, Debug)]
pub enum BufferError {
    SetOffsetBeforeStartOfBuffer { offset: usize },
    SetOffsetAfterEndOfBuffer { offset: Option<usize> },
    UnexpectedEndOfBuffer,
    PositionOverflow,
}

impl BufferError {
    fn with_span<'arena>(self, span: Span) -> ReadError<'arena> {
        ReadError::BufferError(span, self)
    }
}

impl fmt::Display for BufferError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BufferError::SetOffsetBeforeStartOfBuffer { .. } => {
                f.write_str("attempt to set buffer offset before the start of the buffer")
            }
            BufferError::SetOffsetAfterEndOfBuffer { .. } => {
                f.write_str("attempt to set buffer offset after the end of the buffer")
            }
            BufferError::UnexpectedEndOfBuffer => f.write_str("unexpected end of buffer"),
            BufferError::PositionOverflow => f.write_str("position overflow"),
        }
    }
}

impl std::error::Error for BufferError {}

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

    fn elim_env(&self) -> semantics::ElimEnv<'arena, 'env> {
        semantics::ElimEnv::new(self.item_exprs, self.flexible_exprs)
    }

    fn conversion_env(&self) -> semantics::ConversionEnv<'arena, 'env> {
        semantics::ConversionEnv::new(self.item_exprs, EnvLen::new(), self.flexible_exprs)
    }

    pub fn read_entrypoint(
        mut self,
        format: ArcValue<'arena>,
    ) -> Result<HashMap<usize, Vec<ParsedRef<'arena>>>, ReadError<'arena>> {
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
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let val = self.elim_env().force(format);
        let format_span = val.span();
        match val.as_ref() {
            Value::Stuck(Head::Prim(prim), slice) => {
                self.read_prim(reader, *prim, slice, format_span)
            }
            Value::FormatRecord(labels, formats) => {
                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) = self.elim_env().split_telescope(formats) {
                    let expr = self.read_format(reader, &format)?;
                    exprs.push(expr.clone());
                    formats = next_formats(expr);
                }

                Ok(Spanned::new(
                    format_span,
                    Arc::new(Value::RecordLit(labels, exprs)),
                ))
            }
            Value::FormatCond(_label, format, cond) => {
                let value = self.read_format(reader, format)?;
                let cond_res = self.elim_env().apply_closure(cond, value.clone());

                match cond_res.as_ref() {
                    Value::ConstLit(Const::Bool(true)) => Ok(value),
                    Value::ConstLit(Const::Bool(false)) => {
                        Err(ReadError::CondFailure(cond.span(), value))
                    }
                    _ => {
                        // This shouldn't happen since we check that the cond type is Bool earlier
                        Err(ReadError::InvalidValue(Span::Empty))
                    }
                }
            }
            Value::FormatOverlap(labels, formats) => {
                let mut max_relative_offset = reader.relative_offset();

                let mut formats = formats.clone();
                let mut exprs = Vec::with_capacity(formats.len());

                while let Some((format, next_formats)) = self.elim_env().split_telescope(formats) {
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

                Ok(Spanned::new(
                    format_span,
                    Arc::new(Value::RecordLit(labels, exprs)),
                ))
            }

            Value::Stuck(Head::LocalVar(_), _)
            | Value::Stuck(Head::FlexibleVar(_), _)
            | Value::Universe
            | Value::FunType(_, _, _)
            | Value::FunLit(_, _)
            | Value::RecordType(_, _)
            | Value::RecordLit(_, _)
            | Value::ArrayLit(_)
            | Value::ConstLit(_) => Err(ReadError::InvalidFormat(format_span)),
        }
    }

    #[rustfmt::skip]
    fn read_prim(
        &mut self,
        reader: &mut BufferReader<'data>,
        prim: Prim,
        slice: &[Elim<'arena>],
        span: Span,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        use crate::core::semantics::Elim::FunApp;

        match (prim, slice) {
            (Prim::FormatU8, []) => read_const(reader, span, read_u8, |num| Const::U8(num, UIntStyle::Decimal)),
            (Prim::FormatU16Be, []) => read_const(reader, span, read_u16be, |num| Const::U16(num, UIntStyle::Decimal)),
            (Prim::FormatU16Le, []) => read_const(reader, span, read_u16le, |num| Const::U16(num, UIntStyle::Decimal)),
            (Prim::FormatU32Be, []) => read_const(reader, span, read_u32be, |num| Const::U32(num, UIntStyle::Decimal)),
            (Prim::FormatU32Le, []) => read_const(reader, span, read_u32le, |num| Const::U32(num, UIntStyle::Decimal)),
            (Prim::FormatU64Be, []) => read_const(reader, span, read_u64be, |num| Const::U64(num, UIntStyle::Decimal)),
            (Prim::FormatU64Le, []) => read_const(reader, span, read_u64le, |num| Const::U64(num, UIntStyle::Decimal)),
            (Prim::FormatS8, []) => read_const(reader, span, read_s8, Const::S8),
            (Prim::FormatS16Be, []) => read_const(reader, span, read_s16be, Const::S16),
            (Prim::FormatS16Le, []) => read_const(reader, span, read_s16le, Const::S16),
            (Prim::FormatS32Be, []) => read_const(reader, span, read_s32be, Const::S32),
            (Prim::FormatS32Le, []) => read_const(reader, span, read_s32le, Const::S32),
            (Prim::FormatS64Be, []) => read_const(reader, span, read_s64be, Const::S64),
            (Prim::FormatS64Le, []) => read_const(reader, span, read_s64le, Const::S64),
            (Prim::FormatF32Be, []) => read_const(reader, span, read_f32be, Const::F32),
            (Prim::FormatF32Le, []) => read_const(reader, span, read_f32le, Const::F32),
            (Prim::FormatF64Be, []) => read_const(reader, span, read_f64be, Const::F64),
            (Prim::FormatF64Le, []) => read_const(reader, span, read_f64le, Const::F64),
            (Prim::FormatArray8, [FunApp(len), FunApp(format)]) => self.read_array(reader, span, len, format),
            (Prim::FormatArray16, [FunApp(len), FunApp(format)]) => self.read_array(reader, span, len, format),
            (Prim::FormatArray32, [FunApp(len), FunApp(format)]) => self.read_array(reader, span, len, format),
            (Prim::FormatArray64, [FunApp(len), FunApp(format)]) => self.read_array(reader, span, len, format),
            (Prim::FormatRepeatUntilEnd, [FunApp(format)]) => self.read_repeat_until_end(reader, format),
            (Prim::FormatLimit8, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit16, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit32, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLimit64, [FunApp(limit), FunApp(format)]) => self.read_limit(reader, limit, format),
            (Prim::FormatLink, [FunApp(pos), FunApp(format)]) => self.read_link(span, pos, format),
            (Prim::FormatDeref, [FunApp(format), FunApp(r#ref)]) => self.read_deref(format, r#ref),
            (Prim::FormatStreamPos, []) => read_stream_pos(reader, span),
            (Prim::FormatSucceed, [_, FunApp(elem)]) => Ok(elem.clone()),
            (Prim::FormatFail, []) => Err(ReadError::ReadFailFormat(span)),
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
        span: Span,
        len: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let len = match self.elim_env().force(len).as_ref() {
            Value::ConstLit(Const::U8(len, _)) => u64::from(*len),
            Value::ConstLit(Const::U16(len, _)) => u64::from(*len),
            Value::ConstLit(Const::U32(len, _)) => u64::from(*len),
            Value::ConstLit(Const::U64(len, _)) => *len,
            _ => return Err(ReadError::InvalidValue(len.span())),
        };

        let elem_exprs = (0..len)
            .map(|_| self.read_format(reader, elem_format))
            .collect::<Result<_, _>>()?;

        Ok(Spanned::new(span, Arc::new(Value::ArrayLit(elem_exprs))))
    }

    fn read_repeat_until_end(
        &mut self,
        reader: &mut BufferReader<'data>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let mut current_offset = reader.relative_offset();
        let mut elems = Vec::new();

        loop {
            match self.read_format(reader, elem_format) {
                Ok(elem) => {
                    elems.push(elem);
                    current_offset = reader.relative_offset();
                }
                Err(ReadError::BufferError(_, BufferError::UnexpectedEndOfBuffer)) => {
                    // unwrap shouldn't panic as we're rewinding to a known good offset
                    // Should this be set to the end of the current buffer?
                    reader.set_relative_offset(current_offset).unwrap();
                    return Ok(Spanned::new(
                        elem_format.span(),
                        Arc::new(Value::ArrayLit(elems)),
                    ));
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
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let len_span = len.span();
        let len = match self.elim_env().force(len).as_ref() {
            Value::ConstLit(Const::U8(len, _)) => Some(usize::from(*len)),
            Value::ConstLit(Const::U16(len, _)) => Some(usize::from(*len)),
            Value::ConstLit(Const::U32(len, _)) => usize::try_from(*len).ok(),
            Value::ConstLit(Const::U64(len, _)) => usize::try_from(*len).ok(),
            _ => return Err(ReadError::InvalidValue(len_span)),
        }
        .ok_or_else(|| BufferError::PositionOverflow.with_span(len_span))?;

        let buffer = reader
            .remaining_buffer()
            .and_then(|buf| buf.with_remaining_len(len))
            .map_err(|err| err.with_span(len_span))?;

        self.read_format(&mut buffer.reader(), elem_format)
    }

    fn read_link(
        &mut self,
        span: Span,
        pos_value: &ArcValue<'arena>,
        elem_format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let pos = match self.elim_env().force(pos_value).as_ref() {
            Value::ConstLit(Const::Pos(pos)) => *pos,
            _ => return Err(ReadError::InvalidValue(pos_value.span())),
        };

        self.pending_formats.push((pos, elem_format.clone()));

        Ok(Spanned::new(
            span,
            Arc::new(Value::ConstLit(Const::Ref(pos))),
        ))
    }

    fn read_deref(
        &mut self,
        format: &ArcValue<'arena>,
        r#ref: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        let pos = match self.elim_env().force(r#ref).as_ref() {
            Value::ConstLit(Const::Ref(pos)) => *pos,
            _ => return Err(ReadError::InvalidValue(r#ref.span())),
        };

        self.lookup_or_read_ref(pos, format)
    }

    fn lookup_ref<'context>(
        &'context self,
        pos: usize,
        format: &ArcValue<'_>,
    ) -> Option<&'context ParsedRef<'arena>> {
        // NOTE: The number of calls to `semantics::ConversionEnv::is_equal`
        // when looking up cached references is a bit of a pain. If this ever
        // becomes a problem we could improve performance by pre-allocating a
        // `ParsedRef` in the cache during `read_link`, and storing the index of
        // that parsed reference alongside the position in `Const::Ref`.

        (self.cached_refs.get(&pos)?.iter())
            .find(|r| self.conversion_env().is_equal(&r.format, format))
    }

    fn lookup_or_read_ref(
        &mut self,
        pos: usize,
        format: &ArcValue<'arena>,
    ) -> Result<ArcValue<'arena>, ReadError<'arena>> {
        if let Some(parsed_ref) = self.lookup_ref(pos, format) {
            return Ok(parsed_ref.expr.clone());
        }

        // Read the data at the ref location
        let mut reader = self.initial_buffer.reader_with_offset(pos)?;
        let expr = self.read_format(&mut reader, format)?;

        // We might have parsed the current reference during the above call to
        // `read_format`. It's unclear if this could ever happen in practice,
        // especially without succumbing to non-termination, but we'll panic
        // here just in case.
        if self.lookup_ref(pos, format).is_some() {
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
    span: Span,
) -> Result<ArcValue<'arena>, ReadError<'arena>> {
    Ok(Spanned::new(
        span,
        Arc::new(Value::ConstLit(Const::Pos(
            reader.offset().map_err(|err| err.with_span(span))?,
        ))),
    ))
}

fn read_const<'arena, 'data, T>(
    reader: &mut BufferReader<'data>,
    span: Span,
    read: fn(&mut BufferReader<'data>) -> Result<T, BufferError>,
    wrap_const: fn(T) -> Const,
) -> Result<ArcValue<'arena>, ReadError<'arena>> {
    let data = read(reader).map_err(|err| err.with_span(span))?;
    Ok(Spanned::new(
        span,
        Arc::new(Value::ConstLit(wrap_const(data))),
    ))
}

fn read_u8(reader: &mut BufferReader<'_>) -> Result<u8, BufferError> {
    reader.read_byte()
}

fn read_s8(reader: &mut BufferReader<'_>) -> Result<i8, BufferError> {
    reader.read_byte().map(|b| b as i8)
}

/// Generates a function that reads a multi-byte primitive.
macro_rules! read_multibyte_prim {
    ($read_multibyte_prim:ident, $from_bytes:ident, $T:ident) => {
        fn $read_multibyte_prim<'data>(
            reader: &mut BufferReader<'data>,
        ) -> Result<$T, BufferError> {
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
