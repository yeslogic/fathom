use moniker::{Binder, Embed, FreeVar, Var};
use std::io;

use semantics::{nf_term, Definition, DefinitionEnv, InternalError};
use syntax::core;
use syntax::Label;

#[derive(Debug)]
pub enum ParseError {
    InvalidType(core::RcType),
    Internal(InternalError),
    BadArrayIndex(core::RcValue),
    MissingRoot(Label),
    ParametrizedStructType,
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

pub fn parse_module<Env, T>(
    env: &Env,
    root: &Label,
    module: &core::Module,
    bytes: &mut io::Cursor<T>,
) -> Result<Value, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
{
    let mut env = env.clone();

    for (label, Binder(free_var), Embed(definition)) in module.items.clone().unnest() {
        if label == *root {
            match definition {
                core::Definition::Alias { ref term, .. } => {
                    let term = nf_term(&env, term)?;
                    return parse_term(&env, &term, bytes);
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

                    return parse_struct(&env, fields, mappings, bytes);
                },
            }
        } else {
            env.insert_definition(
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

pub fn parse_struct<Env, T>(
    env: &Env,
    fields: Vec<(Label, Binder<String>, Embed<core::RcTerm>)>,
    mut mappings: Vec<(FreeVar<String>, core::RcTerm)>,
    bytes: &mut io::Cursor<T>,
) -> Result<Value, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
{
    let fields = fields
        .into_iter()
        .map(|(label, binder, Embed(ann))| {
            let ann = nf_term(env, &ann.substs(&mappings))?;
            let ann_value = parse_term(env, &ann, bytes)?;
            mappings.push((
                binder.0.clone(),
                core::RcTerm::from(core::Term::from(&ann_value)),
            ));

            Ok((label.clone(), ann_value))
        })
        .collect::<Result<_, ParseError>>()?;

    Ok(Value::Struct(fields))
}

pub fn parse_term<Env, T>(
    env: &Env,
    ty: &core::RcType,
    bytes: &mut io::Cursor<T>,
) -> Result<Value, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
{
    use byteorder::{BigEndian as Be, LittleEndian as Le, ReadBytesExt};
    use moniker::BoundTerm;
    use num_traits::ToPrimitive;

    match **ty {
        _ if core::RcValue::term_eq(ty, env.pos()) => Ok(Value::Pos(bytes.position())),
        _ if core::RcValue::term_eq(ty, env.u8()) => Ok(Value::U8(bytes.read_u8()?)),
        _ if core::RcValue::term_eq(ty, env.u16le()) => Ok(Value::U16(bytes.read_u16::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.u16be()) => Ok(Value::U16(bytes.read_u16::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.u32le()) => Ok(Value::U32(bytes.read_u32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.u32be()) => Ok(Value::U32(bytes.read_u32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.u64le()) => Ok(Value::U64(bytes.read_u64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.u64be()) => Ok(Value::U64(bytes.read_u64::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.s8()) => Ok(Value::S8(bytes.read_i8()?)),
        _ if core::RcValue::term_eq(ty, env.s16le()) => Ok(Value::S16(bytes.read_i16::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.s16be()) => Ok(Value::S16(bytes.read_i16::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.s32le()) => Ok(Value::S32(bytes.read_i32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.s32be()) => Ok(Value::S32(bytes.read_i32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.s64le()) => Ok(Value::S64(bytes.read_i64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.s64be()) => Ok(Value::S64(bytes.read_i64::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.f32le()) => Ok(Value::F32(bytes.read_f32::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.f32be()) => Ok(Value::F32(bytes.read_f32::<Be>()?)),
        _ if core::RcValue::term_eq(ty, env.f64le()) => Ok(Value::F64(bytes.read_f64::<Le>()?)),
        _ if core::RcValue::term_eq(ty, env.f64be()) => Ok(Value::F64(bytes.read_f64::<Be>()?)),

        core::Value::Universe(_)
        | core::Value::IntType(_, _)
        | core::Value::Literal(_)
        | core::Value::Pi(_)
        | core::Value::Lam(_)
        | core::Value::Struct(_)
        | core::Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),

        #[cfg_attr(rustfmt, rustfmt_skip)]
        core::Value::Neutral(ref neutral, ref spine) => {
            if let Some((pos, _)) = env.offset8(ty) { return Ok(Value::Pos(pos + bytes.read_u8()? as u64)); }
            if let Some((pos, _)) = env.offset16le(ty) { return Ok(Value::Pos(pos + bytes.read_u16::<Le>()? as u64)); }
            if let Some((pos, _)) = env.offset16be(ty) { return Ok(Value::Pos(pos + bytes.read_u16::<Be>()? as u64)); }
            if let Some((pos, _)) = env.offset32le(ty) { return Ok(Value::Pos(pos + bytes.read_u32::<Le>()? as u64)); }
            if let Some((pos, _)) = env.offset32be(ty) { return Ok(Value::Pos(pos + bytes.read_u32::<Be>()? as u64)); }
            if let Some((pos, _)) = env.offset64le(ty) { return Ok(Value::Pos(pos + bytes.read_u64::<Le>()? as u64)); }
            if let Some((pos, _)) = env.offset64be(ty) { return Ok(Value::Pos(pos + bytes.read_u64::<Be>()? as u64)); }
            if let Some((len, elem_ty)) = env.array(ty) {
                return Ok(Value::Array(
                    (0..len.to_usize().unwrap()) // FIXME
                        .map(|_| parse_term(env, elem_ty, bytes))
                        .collect::<Result<_, _>>()?,
                ));
            }

            match **neutral {
                core::Neutral::Head(core::Head::Var(Var::Free(ref free_var))) => {
                    match env.get_definition(free_var) {
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

                            parse_struct(env, fields, mappings, bytes)
                        },
                        // FIXME: follow alias?
                        None | Some(&Definition::Alias(_)) => {
                            Err(ParseError::InvalidType(ty.clone()))
                        },
                    }
                },
                core::Neutral::Head(_) | core::Neutral::Proj(_, _) | core::Neutral::Match(_, _) => {
                    Err(ParseError::InvalidType(ty.clone()))
                },
            }
        },
    }
}
