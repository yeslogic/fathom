use moniker::{Binder, Embed, Nest};
use std::io;

use semantics::{nf_term, DefinitionEnv, InternalError};
use syntax::core::{
    self, Definition, Head, Item, Literal, Module, Neutral, RcTerm, RcType, RcValue, Term, Value,
};
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

    match **ty {
        Value::Universe(_)
        | Value::IntType(_, _)
        | Value::Literal(_)
        | Value::Pi(_)
        | Value::Lam(_)
        | Value::Struct(_)
        | Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),
        Value::Neutral(ref neutral, ref spine) => match **neutral {
            Neutral::Head(Head::Global(ref n)) => {
                if spine.len() == 0 {
                    Ok(RcValue::from(Value::Literal(match n.as_str() {
                        "U8" => Literal::Int(bytes.read_u8()?.into()),
                        "U16Le" => Literal::Int(bytes.read_u16::<Le>()?.into()),
                        "U16Be" => Literal::Int(bytes.read_u16::<Be>()?.into()),
                        "U32Le" => Literal::Int(bytes.read_u32::<Le>()?.into()),
                        "U32Be" => Literal::Int(bytes.read_u32::<Be>()?.into()),
                        "U64Le" => Literal::Int(bytes.read_u64::<Le>()?.into()),
                        "U64Be" => Literal::Int(bytes.read_u64::<Be>()?.into()),
                        "S8" => Literal::Int(bytes.read_i8()?.into()),
                        "S16Le" => Literal::Int(bytes.read_i16::<Le>()?.into()),
                        "S16Be" => Literal::Int(bytes.read_i16::<Be>()?.into()),
                        "S32Le" => Literal::Int(bytes.read_i32::<Le>()?.into()),
                        "S32Be" => Literal::Int(bytes.read_i32::<Be>()?.into()),
                        "S64Le" => Literal::Int(bytes.read_i64::<Le>()?.into()),
                        "S64Be" => Literal::Int(bytes.read_i64::<Be>()?.into()),
                        "F32Le" => Literal::F32(bytes.read_f32::<Le>()?),
                        "F32Be" => Literal::F32(bytes.read_f32::<Be>()?),
                        "F64Le" => Literal::F64(bytes.read_f64::<Le>()?),
                        "F64Be" => Literal::F64(bytes.read_f64::<Be>()?),
                        _ => return Err(ParseError::InvalidType(ty.clone())),
                    })))
                } else if spine.len() == 2 && *n == "Array" {
                    let len = &spine[0];
                    let elem_ty = &spine[1];
                    match **len {
                        Value::Literal(Literal::Int(ref len)) => Ok(RcValue::from(Value::Array(
                            (0..len.to_usize().unwrap()) // FIXME
                                .map(|_| parse_term(env, elem_ty, bytes))
                                .collect::<Result<_, _>>()?,
                        ))),
                        _ => Err(ParseError::BadArrayIndex(len.clone())),
                    }
                } else {
                    Err(ParseError::InvalidType(ty.clone()))
                }
            },
            Neutral::Head(Head::Var(ref var)) => Err(InternalError::UnexpectedBoundVar {
                span: None,
                var: var.clone(),
            }.into()),
            Neutral::Head(Head::Extern(_, _))
            | Neutral::If(_, _, _)
            | Neutral::Proj(_, _)
            | Neutral::Case(_, _) => Err(ParseError::InvalidType(ty.clone())),
        },
    }
}
