use moniker::{Binder, Embed, FreeVar, Var};
use std::io;

use semantics::{nf_term, DefinitionEnv, InternalError};
use syntax::core::{Definition, Head, Item, Module, Neutral, RcTerm, RcType, RcValue, Term, Value};
use syntax::Label;

#[derive(Debug)]
pub enum ParseError {
    InvalidType(RcType),
    Internal(InternalError),
    BadArrayIndex(RcValue),
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

pub fn parse_module<Env, T>(
    env: &Env,
    root: &Label,
    module: &Module,
    bytes: &mut io::Cursor<T>,
) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
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
                    Definition::Alias(ref term) => {
                        let term = nf_term(&env, term)?;
                        return parse_term(&env, &term, bytes);
                    },
                    Definition::StructType(ref scope) => {
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
            }
            Item::Definition {
                binder: Binder(ref free_var),
                ref definition,
                ..
            } => env.insert_definition(
                free_var.clone(),
                match *definition {
                    Definition::Alias(ref term) => Definition::Alias(term.clone()),
                    Definition::StructType(ref scope) => Definition::StructType(scope.clone()),
                },
            ),
        }
    }

    Err(ParseError::MissingRoot(root.clone()))
}

pub fn parse_struct<Env, T>(
    env: &Env,
    fields: Vec<(Label, Binder<String>, Embed<RcTerm>)>,
    mut mappings: Vec<(FreeVar<String>, RcTerm)>,
    bytes: &mut io::Cursor<T>,
) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
{
    let fields = fields
        .into_iter()
        .map(|(label, binder, Embed(ann))| {
            let ann = nf_term(env, &ann.substs(&mappings))?;
            let ann_value = parse_term(env, &ann, bytes)?;
            mappings.push((binder.0.clone(), RcTerm::from(Term::from(&*ann_value))));

            Ok((label.clone(), ann_value))
        })
        .collect::<Result<_, ParseError>>()?;

    Ok(RcValue::from(Value::Struct(fields)))
}

pub fn parse_term<Env, T>(
    env: &Env,
    ty: &RcType,
    bytes: &mut io::Cursor<T>,
) -> Result<RcValue, ParseError>
where
    Env: DefinitionEnv,
    io::Cursor<T>: io::Read + io::Seek,
{
    use byteorder::{BigEndian as Be, LittleEndian as Le, ReadBytesExt};
    use moniker::BoundTerm;
    use num_traits::ToPrimitive;

    use syntax::core::Literal::*;
    use syntax::core::Value::Literal;
    use syntax::{FloatFormat, IntFormat};

    #[cfg_attr(rustfmt, rustfmt_skip)]
    match **ty {
        _ if RcValue::term_eq(ty, env.pos()) => Ok(RcValue::from(Literal(Pos(bytes.position())))),
        _ if RcValue::term_eq(ty, env.u8()) => Ok(RcValue::from(Literal(Int(bytes.read_u8()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u16le()) => Ok(RcValue::from(Literal(Int(bytes.read_u16::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u16be()) => Ok(RcValue::from(Literal(Int(bytes.read_u16::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u32le()) => Ok(RcValue::from(Literal(Int(bytes.read_u32::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u32be()) => Ok(RcValue::from(Literal(Int(bytes.read_u32::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u64le()) => Ok(RcValue::from(Literal(Int(bytes.read_u64::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.u64be()) => Ok(RcValue::from(Literal(Int(bytes.read_u64::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s8()) => Ok(RcValue::from(Literal(Int(bytes.read_i8()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s16le()) => Ok(RcValue::from(Literal(Int(bytes.read_i16::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s16be()) => Ok(RcValue::from(Literal(Int(bytes.read_i16::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s32le()) => Ok(RcValue::from(Literal(Int(bytes.read_i32::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s32be()) => Ok(RcValue::from(Literal(Int(bytes.read_i32::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s64le()) => Ok(RcValue::from(Literal(Int(bytes.read_i64::<Le>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.s64be()) => Ok(RcValue::from(Literal(Int(bytes.read_i64::<Be>()?.into(), IntFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.f32le()) => Ok(RcValue::from(Literal(F32(bytes.read_f32::<Le>()?, FloatFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.f32be()) => Ok(RcValue::from(Literal(F32(bytes.read_f32::<Be>()?, FloatFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.f64le()) => Ok(RcValue::from(Literal(F64(bytes.read_f64::<Le>()?, FloatFormat::Dec)))),
        _ if RcValue::term_eq(ty, env.f64be()) => Ok(RcValue::from(Literal(F64(bytes.read_f64::<Be>()?, FloatFormat::Dec)))),

        Value::Universe(_)
        | Value::IntType(_, _)
        | Value::Literal(_)
        | Value::Pi(_)
        | Value::Lam(_)
        | Value::Struct(_)
        | Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),

        Value::Neutral(ref neutral, ref spine) => match env.array(ty) {
            Some((len, elem_ty)) => Ok(RcValue::from(Value::Array(
                (0..len.to_usize().unwrap()) // FIXME
                    .map(|_| parse_term(env, elem_ty, bytes))
                    .collect::<Result<_, _>>()?,
            ))),
            None => match **neutral {
                Neutral::Head(Head::Var(Var::Free(ref free_var))) => {
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
                Neutral::Head(_) | Neutral::Proj(_, _) | Neutral::Match(_, _) => {
                    Err(ParseError::InvalidType(ty.clone()))
                },
            }
        },
    }
}
