use moniker::{Binder, Embed, Nest};
use std::io;

use semantics::{nf_term, DefinitionEnv, InternalError};
use syntax::core::{self, Definition, Item, Module, RcTerm, RcType, RcValue, Term, Value};
use syntax::Label;

#[derive(Debug)]
pub enum ParseError {
    InvalidType(RcType),
    Internal(InternalError),
    BadArrayIndex(RcValue),
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

pub fn parse_module<Env, R>(
    env: &Env,
    root: &Label,
    module: &Module,
    bytes: &mut R,
) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    R: io::Read + io::Seek,
{
    let mut env = env.clone();

    for item in &module.items {
        match *item {
            Item::Declaration { .. } => {},
            Item::Definition {
                ref label,
                ref definition,
                ..
            }
                if label == root =>
            {
                match *definition {
                    core::Definition::Alias(ref term) => {
                        let term = nf_term(&env, term)?;
                        return parse_term(&env, &term, bytes);
                    },
                    core::Definition::StructType(ref fields) => {
                        return parse_struct(&env, fields, bytes)
                    },
                }
            },
            Item::Definition {
                binder: Binder(ref free_var),
                ref definition,
                ..
            } => env.insert_definition(
                free_var.clone(),
                match *definition {
                    core::Definition::Alias(ref term) => Definition::Alias(term.clone()),
                    core::Definition::StructType(ref scope) => {
                        Definition::StructType(scope.clone())
                    },
                },
            ),
        }
    }

    Err(ParseError::MissingRoot(root.clone()))
}

pub fn parse_struct<Env, R>(
    env: &Env,
    fields: &Nest<(Label, Binder<String>, Embed<RcTerm>)>,
    bytes: &mut R,
) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    R: io::Read + io::Seek,
{
    let fields = fields.clone().unnest();

    let mut mappings = Vec::with_capacity(fields.len());
    let fields = fields
        .into_iter()
        .map(|(label, binder, Embed(ann))| {
            let ann = nf_term(env, &ann.substs(&mappings))?;
            let ann_value = parse_term(env, &ann, bytes)?;
            mappings.push((binder.0.clone(), RcTerm::from(Term::from(&*ann_value))));

            Ok((label.clone(), ann_value))
        }).collect::<Result<_, ParseError>>()?;

    Ok(RcValue::from(Value::Struct(fields)))
}

pub fn parse_term<Env, R>(env: &Env, ty: &RcType, bytes: &mut R) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    R: io::Read + io::Seek,
{
    use byteorder::{BigEndian as Be, LittleEndian as Le, ReadBytesExt};
    use num_traits::ToPrimitive;

    use syntax::core::Literal::*;
    use syntax::core::Value::Literal;
    use syntax::{FloatFormat, IntFormat};

    #[cfg_attr(rustfmt, rustfmt_skip)]
    match **ty {
        Value::Universe(_)
        | Value::IntType(_, _)
        | Value::Literal(_)
        | Value::Pi(_)
        | Value::Lam(_)
        | Value::Struct(_)
        | Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),

        _ if ty == env.u8() => Ok(RcValue::from(Literal(Int(bytes.read_u8()?.into(), IntFormat::Dec)))),
        _ if ty == env.u16le() => Ok(RcValue::from(Literal(Int(bytes.read_u16::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.u16be() => Ok(RcValue::from(Literal(Int(bytes.read_u16::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.u32le() => Ok(RcValue::from(Literal(Int(bytes.read_u32::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.u32be() => Ok(RcValue::from(Literal(Int(bytes.read_u32::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.u64le() => Ok(RcValue::from(Literal(Int(bytes.read_u64::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.u64be() => Ok(RcValue::from(Literal(Int(bytes.read_u64::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s8() => Ok(RcValue::from(Literal(Int(bytes.read_i8()?.into(), IntFormat::Dec)))),
        _ if ty == env.s16le() => Ok(RcValue::from(Literal(Int(bytes.read_i16::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s16be() => Ok(RcValue::from(Literal(Int(bytes.read_i16::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s32le() => Ok(RcValue::from(Literal(Int(bytes.read_i32::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s32be() => Ok(RcValue::from(Literal(Int(bytes.read_i32::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s64le() => Ok(RcValue::from(Literal(Int(bytes.read_i64::<Le>()?.into(), IntFormat::Dec)))),
        _ if ty == env.s64be() => Ok(RcValue::from(Literal(Int(bytes.read_i64::<Be>()?.into(), IntFormat::Dec)))),
        _ if ty == env.f32le() => Ok(RcValue::from(Literal(F32(bytes.read_f32::<Le>()?, FloatFormat::Dec)))),
        _ if ty == env.f32be() => Ok(RcValue::from(Literal(F32(bytes.read_f32::<Be>()?, FloatFormat::Dec)))),
        _ if ty == env.f64le() => Ok(RcValue::from(Literal(F64(bytes.read_f64::<Le>()?, FloatFormat::Dec)))),
        _ if ty == env.f64be() => Ok(RcValue::from(Literal(F64(bytes.read_f64::<Be>()?, FloatFormat::Dec)))),

        _ => match env.array(ty) {
            Some((len, elem_ty)) => Ok(RcValue::from(Value::Array(
                (0..len.to_usize().unwrap()) // FIXME
                    .map(|_| parse_term(env, elem_ty, bytes))
                    .collect::<Result<_, _>>()?,
            ))),
            None => Err(ParseError::InvalidType(ty.clone())),
        },
    }
}
