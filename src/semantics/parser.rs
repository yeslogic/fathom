use im;
use moniker::{Binder, Embed, FreeVar, Var};
use std::io;

use semantics::{nf_term, Context, Definition, InternalError};
use syntax::core;
use syntax::Label;

#[derive(Debug)]
pub enum ParseError {
    InvalidType(core::RcType),
    Internal(InternalError),
    BadArrayIndex(core::RcValue),
    OffsetPointedToDifferentTypes(core::RcType, core::RcType),
    ParametrizedStructType,
    FailedPredicate(core::RcValue),
    MissingRoot(Label),
    Io(io::Error),
}

impl From<InternalError> for ParseError {
    fn from(src: InternalError) -> ParseError {
        ParseError::Internal(src)
    }
}

impl From<io::Error> for ParseError {
    fn from(src: io::Error) -> ParseError {
        ParseError::Io(src)
    }
}

/// A stack of references that we still need to parse
type PendingOffsets = Vec<(u64, core::RcType)>;

/// Values returned as a result of parsing a binary format
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Pos(u64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    S8(i8),
    S16(i16),
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
    Struct(Vec<(Label, Value)>),
    Array(Vec<Value>),
}

impl<'a> From<&'a Value> for core::Term {
    fn from(src: &'a Value) -> core::Term {
        use syntax::core::{Literal, RcTerm, Term};
        use syntax::{FloatFormat, IntFormat};

        match *src {
            Value::Pos(value) => Term::Literal(Literal::Pos(value)),
            Value::U8(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::U16(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::U32(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::U64(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::S8(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::S16(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::S32(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::S64(value) => Term::Literal(Literal::Int(value.into(), IntFormat::Dec)),
            Value::F32(value) => Term::Literal(Literal::F32(value.into(), FloatFormat::Dec)),
            Value::F64(value) => Term::Literal(Literal::F64(value.into(), FloatFormat::Dec)),
            Value::Struct(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref val)| (label.clone(), RcTerm::from(Term::from(val))))
                    .collect();

                Term::Struct(fields)
            },
            Value::Array(ref elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| RcTerm::from(Term::from(elem)))
                    .collect();

                Term::Array(elems)
            },
        }
    }
}

pub fn parse_module<T>(
    context: &Context,
    root: &Label,
    module: &core::Module,
    bytes: &mut io::Cursor<T>,
) -> Result<im::HashMap<u64, Value>, ParseError>
where
    io::Cursor<T>: io::Read + io::Seek,
{
    let mut context = context.clone();
    let mut pending = PendingOffsets::new();

    for (label, Binder(free_var), Embed(definition)) in module.items.clone().unnest() {
        if label == *root {
            let root_value = match definition {
                core::Definition::Alias { ref term, .. } => {
                    let term = nf_term(&context, term)?;
                    parse_term(&context, &mut pending, &term, bytes)?
                },
                core::Definition::StructType { ref scope } => {
                    let (params, fields_scope) = scope.clone().unbind();

                    if !params.unsafe_patterns.is_empty() {
                        // TODO: more error info?
                        return Err(ParseError::ParametrizedStructType);
                    }

                    let (fields, ()) = fields_scope.unbind();
                    let fields = fields.clone().unnest();
                    let mappings = Vec::with_capacity(fields.len());

                    parse_struct(&context, &mut pending, fields, mappings, bytes)?
                },
            };

            // A dump of the previously parsed values
            let mut parsed_values = im::HashMap::new();
            // A cache of the core types we've looked at through offsets
            let mut parsed_tys = im::HashMap::<u64, core::RcValue>::new();

            // Add root definition
            parsed_values.insert(0, root_value);

            // Follow pending offsets until exhausted ヾ(｡ꏿ﹏ꏿ)ﾉﾞ
            while let Some((pos, ty)) = pending.pop() {
                use im::hashmap::Entry;
                use moniker::BoundTerm;

                match parsed_values.entry(pos) {
                    // This position has not yet been parsed!
                    Entry::Vacant(parsed_entry) => {
                        bytes.set_position(pos); // FIXME: Bounds check?
                        let value = parse_term(&mut context, &mut pending, &ty, bytes)?;
                        parsed_entry.insert(value);
                        parsed_tys.insert(pos, ty);
                    },
                    // Was already parsed!
                    Entry::Occupied(_) => {
                        // It's ok to refer to the same region of memory from
                        // two locations in the same file if the types match
                        let parsed_ty = parsed_tys.get(&pos).expect("expected entry").clone();
                        if !core::Type::term_eq(&parsed_ty, &ty.inner) {
                            return Err(ParseError::OffsetPointedToDifferentTypes(ty, parsed_ty));
                        }
                    },
                }
            }

            return Ok(parsed_values);
        } else {
            context.insert_definition(
                free_var.clone(),
                match definition {
                    core::Definition::Alias { ref term, .. } => Definition::Alias(term.clone()),
                    core::Definition::StructType { ref scope } => {
                        Definition::StructType(scope.clone())
                    },
                },
            );
        }
    }

    Err(ParseError::MissingRoot(root.clone()))
}

fn parse_struct<T>(
    context: &Context,
    pending: &mut PendingOffsets,
    fields: Vec<(Label, Binder<String>, Embed<core::RcTerm>)>,
    mut mappings: Vec<(FreeVar<String>, core::RcTerm)>,
    bytes: &mut io::Cursor<T>,
) -> Result<Value, ParseError>
where
    io::Cursor<T>: io::Read + io::Seek,
{
    let fields = fields
        .into_iter()
        .map(|(label, binder, Embed(ann))| {
            let ann = nf_term(context, &ann.substs(&mappings))?;
            let ann_value = parse_term(context, pending, &ann, bytes)?;
            mappings.push((
                binder.0.clone(),
                core::RcTerm::from(core::Term::from(&ann_value)),
            ));

            Ok((label.clone(), ann_value))
        })
        .collect::<Result<_, ParseError>>()?;

    Ok(Value::Struct(fields))
}

fn queue_offset(
    pending: &mut PendingOffsets,
    offset_pos: u64,
    ty: &core::RcType,
) -> Result<Value, ParseError> {
    pending.push((offset_pos, ty.clone()));
    Ok(Value::Pos(offset_pos))
}

fn parse_term<T>(
    context: &Context,
    pending: &mut PendingOffsets,
    ty: &core::RcType,
    bytes: &mut io::Cursor<T>,
) -> Result<Value, ParseError>
where
    io::Cursor<T>: io::Read + io::Seek,
{
    use byteorder::{BigEndian as Be, LittleEndian as Le, ReadBytesExt};
    use moniker::BoundTerm;
    use num_traits::ToPrimitive;

    match **ty {
        // Parse builtin types
        _ if core::RcValue::term_eq(ty, context.pos()) => Ok(Value::Pos(bytes.position())),
        _ if core::RcValue::term_eq(ty, context.u8()) => Ok(Value::U8(bytes.read_u8()?)),
        _ if core::RcValue::term_eq(ty, context.u16le()) => Ok(Value::U16(bytes.read_u16::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.u16be()) => Ok(Value::U16(bytes.read_u16::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.u32le()) => Ok(Value::U32(bytes.read_u32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.u32be()) => Ok(Value::U32(bytes.read_u32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.u64le()) => Ok(Value::U64(bytes.read_u64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.u64be()) => Ok(Value::U64(bytes.read_u64::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.s8()) => Ok(Value::S8(bytes.read_i8()?)),
        _ if core::RcValue::term_eq(ty, context.s16le()) => Ok(Value::S16(bytes.read_i16::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.s16be()) => Ok(Value::S16(bytes.read_i16::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.s32le()) => Ok(Value::S32(bytes.read_i32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.s32be()) => Ok(Value::S32(bytes.read_i32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.s64le()) => Ok(Value::S64(bytes.read_i64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.s64be()) => Ok(Value::S64(bytes.read_i64::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.f32le()) => Ok(Value::F32(bytes.read_f32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.f32be()) => Ok(Value::F32(bytes.read_f32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, context.f64le()) => Ok(Value::F64(bytes.read_f64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, context.f64be()) => Ok(Value::F64(bytes.read_f64::<Be>()?)),

        core::Value::Refinement(ref scope) => {
            let ((Binder(free_var), Embed(ann)), pred) = scope.clone().unbind();
            let ann_value = parse_term(context, pending, &ann, bytes)?;
            let pred_value = {
                let ann_value = core::RcTerm::from(core::Term::from(&ann_value));
                nf_term(context, &pred.substs(&[(free_var, ann_value)]))?
            };

            match *pred_value.inner {
                core::Value::Literal(core::Literal::Bool(true)) => Ok(ann_value),
                core::Value::Literal(core::Literal::Bool(false)) => {
                    Err(ParseError::FailedPredicate(pred.clone()))
                },
                _ => unimplemented!("unexpected value: {}", pred_value),
            }
        },

        // Invalid parse types
        core::Value::Universe(_)
        | core::Value::IntType(_, _)
        | core::Value::Literal(_)
        | core::Value::Pi(_)
        | core::Value::Lam(_)
        | core::Value::Struct(_)
        | core::Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),

        #[cfg_attr(rustfmt, rustfmt_skip)]
        core::Value::Neutral(ref neutral, ref spine) => {
            // Parse offsets
            if let Some((pos, ty)) = context.offset8(ty) { return queue_offset(pending, pos + bytes.read_u8()? as u64, ty); }
            if let Some((pos, ty)) = context.offset16le(ty) { return queue_offset(pending, pos + bytes.read_u16::<Le>()? as u64, ty); }
            if let Some((pos, ty)) = context.offset16be(ty) { return queue_offset(pending, pos + bytes.read_u16::<Be>()? as u64, ty); }
            if let Some((pos, ty)) = context.offset32le(ty) { return queue_offset(pending, pos + bytes.read_u32::<Le>()? as u64, ty); }
            if let Some((pos, ty)) = context.offset32be(ty) { return queue_offset(pending, pos + bytes.read_u32::<Be>()? as u64, ty); }
            if let Some((pos, ty)) = context.offset64le(ty) { return queue_offset(pending, pos + bytes.read_u64::<Le>()? as u64, ty); }
            if let Some((pos, ty)) = context.offset64be(ty) { return queue_offset(pending, pos + bytes.read_u64::<Be>()? as u64, ty); }
            if let Some((pos, offset, ty)) = context.offset_pos(ty) {
                let offset_pos = pos + offset.to_u64().unwrap(); // FIXME
                pending.push((offset_pos, ty.clone()));
                return Ok(Value::Pos(offset_pos));
            }

            // Reserved things
            if let Some(elem_ty) = context.reserved(ty) {
                parse_term(context, pending, elem_ty, bytes)?;
                return Ok(Value::Struct(Vec::new()));
            }

            // Parse arrays
            if let Some((len, elem_ty)) = context.array(ty) {
                return Ok(Value::Array(
                    (0..len.to_usize().unwrap()) // FIXME
                        .map(|_| parse_term(context, pending, elem_ty, bytes))
                        .collect::<Result<_, _>>()?,
                ));
            }

            match **neutral {
                core::Neutral::Head(core::Head::Var(Var::Free(ref free_var))) => {
                    // Follow definitions
                    match context.get_definition(free_var) {
                        Some(&Definition::StructType(ref scope)) => {
                            let (params, fields_scope) = scope.clone().unbind();
                            let (fields, ()) = fields_scope.unbind();
                            let params = params.unnest();
                            let fields = fields.unnest();

                            if params.len() != spine.len() {
                                unimplemented!();
                            }

                            let mut mappings = Vec::with_capacity(params.len() + fields.len());

                            for (&(Binder(ref free_var), _), arg) in
                                Iterator::zip(params.iter(), spine.iter())
                            {
                                let arg = arg.substs(&mappings);
                                mappings.push((free_var.clone(), arg));
                            }

                            parse_struct(context, pending, fields, mappings, bytes)
                        },
                        // FIXME: follow alias?
                        Some(&Definition::Alias(_)) => Err(ParseError::InvalidType(ty.clone())),
                        // Definition not found
                        None => Err(ParseError::InvalidType(ty.clone())),
                    }
                },
                // Invalid parse types
                core::Neutral::Head(_) | core::Neutral::Proj(_, _) | core::Neutral::Match(_, _) => {
                    Err(ParseError::InvalidType(ty.clone()))
                },
            }
        },
    }
}
