use nameless::{self, Embed, Name, Var};
use std::io;
use std::rc::Rc;

use syntax::context::Context;
use syntax::core::{Head, Literal, Neutral, Term, Type, Value};

use super::{normalize, subst, InternalError};

#[derive(Debug)]
pub enum ParseError {
    InvalidType(Rc<Type>),
    Internal(InternalError),
    BadArrayIndex(Rc<Value>),
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

pub fn parse<R>(context: &Context, ty: &Rc<Type>, bytes: &mut R) -> Result<Rc<Value>, ParseError>
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
            let ((label, Embed(ann)), body) = nameless::unbind(scope.clone());

            let ann_value = parse(context, &ann, bytes)?;
            let body = subst(
                &body,
                &[(label.0.clone(), Rc::new(Term::from(&*ann_value)))],
            );
            let body = normalize(context, &body)?;
            let body_value = parse(context, &body, bytes)?;

            Ok(Rc::new(Value::Record(nameless::bind(
                (label, Embed(ann_value)),
                body_value,
            ))))
        },
        Value::RecordTypeEmpty => Ok(Rc::new(Value::RecordEmpty)),
        Value::Neutral(ref neutral) => match **neutral {
            #[cfg_attr(rustfmt, rustfmt_skip)]
            Neutral::App(Head::Var(Var::Free(ref n)), ref spine) => match spine[..] {
                [] if *n == Name::user("U8") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u8()?.into())))),
                [] if *n == Name::user("U16Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u16::<Le>()?.into())))),
                [] if *n == Name::user("U16Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u16::<Be>()?.into())))),
                [] if *n == Name::user("U32Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u32::<Le>()?.into())))),
                [] if *n == Name::user("U32Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u32::<Be>()?.into())))),
                [] if *n == Name::user("U64Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u64::<Le>()?.into())))),
                [] if *n == Name::user("U64Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_u64::<Be>()?.into())))),
                [] if *n == Name::user("S8") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i8()?.into())))),
                [] if *n == Name::user("S16Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i16::<Le>()?.into())))),
                [] if *n == Name::user("S16Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i16::<Be>()?.into())))),
                [] if *n == Name::user("S32Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i32::<Le>()?.into())))),
                [] if *n == Name::user("S32Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i32::<Be>()?.into())))),
                [] if *n == Name::user("S64Le") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i64::<Le>()?.into())))),
                [] if *n == Name::user("S64Be") => Ok(Rc::new(Value::Literal(Literal::Int(bytes.read_i64::<Be>()?.into())))),
                [] if *n == Name::user("F32Le") => Ok(Rc::new(Value::Literal(Literal::F32(bytes.read_f32::<Le>()?)))),
                [] if *n == Name::user("F32Be") => Ok(Rc::new(Value::Literal(Literal::F32(bytes.read_f32::<Be>()?)))),
                [] if *n == Name::user("F64Le") => Ok(Rc::new(Value::Literal(Literal::F64(bytes.read_f64::<Le>()?)))),
                [] if *n == Name::user("F64Be") => Ok(Rc::new(Value::Literal(Literal::F64(bytes.read_f64::<Be>()?)))),
                [ref len, ref elem_ty] if *n == Name::user("Array") => match **len {
                    Value::Literal(Literal::Int(ref len)) => Ok(Rc::new(Value::Array(
                        (0..len.to_usize().unwrap()) // FIXME
                            .map(|_| parse(context, elem_ty, bytes))
                            .collect::<Result<_, _>>()?,
                    ))),
                    _ => Err(ParseError::BadArrayIndex(len.clone())),
                },
                _ => Err(ParseError::InvalidType(ty.clone())),
            },
            Neutral::App(Head::Var(Var::Bound(ref name, index)), _) => {
                Err(InternalError::UnsubstitutedDebruijnIndex {
                    span: None,
                    name: name.clone(),
                    index,
                }.into())
            },
            Neutral::If(_, _, _, _) | Neutral::Proj(_, _, _) => {
                Err(ParseError::InvalidType(ty.clone()))
            },
        },
    }
}
