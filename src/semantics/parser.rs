use moniker::{Embed, FreeVar, Scope, Var};
use std::io;

use syntax::context::Context;
use syntax::core::{Head, Literal, Neutral, RcTerm, RcType, RcValue, Term, Value};

use super::{normalize, InternalError};

#[derive(Debug)]
pub enum ParseError {
    InvalidType(RcType),
    Internal(InternalError),
    BadArrayIndex(RcValue),
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

pub fn parse<R>(context: &Context, ty: &RcType, bytes: &mut R) -> Result<RcValue, ParseError>
where
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
        | Value::Record(_)
        | Value::RecordEmpty
        | Value::Array(_) => Err(ParseError::InvalidType(ty.clone())),
        Value::RecordType(ref scope) => {
            let ((label, Embed(ann)), body) = scope.clone().unbind();

            let ann_value = parse(context, &ann, bytes)?;
            let body = body.substs(&[((label.0).0.clone(), RcTerm::from(Term::from(&*ann_value)))]);
            let body = normalize(context, &body)?;
            let body_value = parse(context, &body, bytes)?;

            Ok(RcValue::from(Value::Record(Scope::new(
                (label, Embed(ann_value)),
                body_value,
            ))))
        },
        Value::RecordTypeEmpty => Ok(RcValue::from(Value::RecordEmpty)),
        Value::Neutral(ref neutral, ref spine) => match **neutral {
            Neutral::Head(Head::Var(Var::Free(ref n))) => {
                if spine.len() == 0 {
                    #[cfg_attr(rustfmt, rustfmt_skip)]
                    match () {
                        () if *n == FreeVar::user("U8") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u8()?.into())))),
                        () if *n == FreeVar::user("U16Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u16::<Le>()?.into())))),
                        () if *n == FreeVar::user("U16Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u16::<Be>()?.into())))),
                        () if *n == FreeVar::user("U32Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u32::<Le>()?.into())))),
                        () if *n == FreeVar::user("U32Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u32::<Be>()?.into())))),
                        () if *n == FreeVar::user("U64Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u64::<Le>()?.into())))),
                        () if *n == FreeVar::user("U64Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_u64::<Be>()?.into())))),
                        () if *n == FreeVar::user("S8") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i8()?.into())))),
                        () if *n == FreeVar::user("S16Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i16::<Le>()?.into())))),
                        () if *n == FreeVar::user("S16Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i16::<Be>()?.into())))),
                        () if *n == FreeVar::user("S32Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i32::<Le>()?.into())))),
                        () if *n == FreeVar::user("S32Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i32::<Be>()?.into())))),
                        () if *n == FreeVar::user("S64Le") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i64::<Le>()?.into())))),
                        () if *n == FreeVar::user("S64Be") => Ok(RcValue::from(Value::Literal(Literal::Int(bytes.read_i64::<Be>()?.into())))),
                        () if *n == FreeVar::user("F32Le") => Ok(RcValue::from(Value::Literal(Literal::F32(bytes.read_f32::<Le>()?)))),
                        () if *n == FreeVar::user("F32Be") => Ok(RcValue::from(Value::Literal(Literal::F32(bytes.read_f32::<Be>()?)))),
                        () if *n == FreeVar::user("F64Le") => Ok(RcValue::from(Value::Literal(Literal::F64(bytes.read_f64::<Le>()?)))),
                        () if *n == FreeVar::user("F64Be") => Ok(RcValue::from(Value::Literal(Literal::F64(bytes.read_f64::<Be>()?)))),
                        _ => Err(ParseError::InvalidType(ty.clone())),
                    }
                } else if spine.len() == 2 && *n == FreeVar::user("Array") {
                    let len = &spine[0];
                    let elem_ty = &spine[1];
                    match **len {
                        Value::Literal(Literal::Int(ref len)) => Ok(RcValue::from(Value::Array(
                            (0..len.to_usize().unwrap()) // FIXME
                                .map(|_| parse(context, elem_ty, bytes))
                                .collect::<Result<_, _>>()?,
                        ))),
                        _ => Err(ParseError::BadArrayIndex(len.clone())),
                    }
                } else {
                    Err(ParseError::InvalidType(ty.clone()))
                }
            },
            Neutral::Head(Head::Var(ref var)) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: None,
                var: var.clone(),
            }.into()),
            Neutral::If(_, _, _) | Neutral::Proj(_, _) | Neutral::Case(_, _) => {
                Err(ParseError::InvalidType(ty.clone()))
            },
        },
    }
}
