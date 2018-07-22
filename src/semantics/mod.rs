//! The semantics of the language
//!
//! Here we define the rules of normalization, type checking, and type inference.
//!
//! For more information, check out the theory appendix of the DDL book.

use codespan::ByteSpan;
use moniker::{BoundPattern, BoundTerm, Embed, FreeVar, Nest, Scope, Var};
use num_traits::ToPrimitive;
use std::rc::Rc;

use syntax::context::Context;
use syntax::core::{
    Definition, Head, Literal, Module, Neutral, RcTerm, RcType, RcValue, Term, Type, Value,
};
use syntax::raw;
use syntax::translation::Resugar;
use syntax::{Label, Level};

mod errors;
pub mod parser;
#[cfg(test)]
mod tests;

pub use self::errors::{InternalError, TypeError};

/// Type check and elaborate a module
pub fn check_module(raw_module: &raw::Module) -> Result<Module, TypeError> {
    let mut context = Context::default();
    let definitions = raw_module
        .definitions
        .clone()
        .unnest()
        .into_iter()
        .map(|(name, Embed(raw_definition))| {
            let (term, ann) = match *raw_definition.ann.inner {
                // We don't have a type annotation available to us! Instead we will
                // attempt to infer it based on the body of the definition
                raw::Term::Hole(_) => infer(&context, &raw_definition.term)?,
                // We have a type annotation! Elaborate it, then normalize it, then
                // check that it matches the body of the definition
                _ => {
                    let (ann, _) = infer(&context, &raw_definition.ann)?;
                    let ann = normalize(&context, &ann)?;
                    let term = check(&context, &raw_definition.term, &ann)?;
                    (term, ann)
                },
            };

            // Add the definition to the context
            context = context.define_term(name.clone(), ann.clone(), term.clone());

            Ok((name, Embed(Definition { term, ann })))
        })
        .collect::<Result<_, TypeError>>()?;

    Ok(Module {
        name: raw_module.name.clone(),
        definitions: Nest::new(definitions),
    })
}

/// Reduce a term to its normal form
pub fn normalize(context: &Context, term: &RcTerm) -> Result<RcValue, InternalError> {
    use syntax::context::Definition;

    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => normalize(context, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::IntType(ref min, ref max) => {
            let min = match *min {
                None => None,
                Some(ref x) => Some(normalize(context, x)?),
            };

            let max = match *max {
                None => None,
                Some(ref x) => Some(normalize(context, x)?),
            };

            Ok(RcValue::from(Value::IntType(min, max)))
        },

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var) => match *var {
            Var::Free(ref name) => match context.lookup_definition(name) {
                Some(Definition::Term(term)) => normalize(context, &term),
                Some(Definition::Prim(_)) | None => Ok(RcValue::from(Value::from(var.clone()))),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(index, ref hint) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: None,
                index,
                hint: hint.clone(),
            }),
        },

        // E-PI
        Term::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(RcValue::from(Value::Pi(Scope::new(
                (name, Embed(ann)),
                body,
            ))))
        },

        // E-LAM
        Term::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(RcValue::from(Value::Lam(Scope::new(
                (name, Embed(ann)),
                body,
            ))))
        },

        // E-APP
        Term::App(ref expr, ref arg) => {
            let mut value_expr = normalize(context, expr)?;

            match Rc::make_mut(&mut value_expr.inner) {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((name, Embed(_)), body) = scope.clone().unbind();
                    normalize(context, &body.substs(&[(name, arg.clone())]))
                },
                Value::Neutral(ref mut neutral) => {
                    let arg = normalize(context, arg)?;

                    // Update the spine in place, if possible
                    match *Rc::make_mut(&mut neutral.inner) {
                        Neutral::App(Head::Var(Var::Free(ref name)), ref mut spine) => {
                            spine.push(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully normalized
                            if let Some(Definition::Prim(prim)) = context.lookup_definition(name) {
                                if prim.arity == spine.len() && spine.iter().all(|arg| arg.is_nf())
                                {
                                    return Ok((prim.fun)(spine).unwrap());
                                }
                            }
                        },
                        Neutral::App(_, ref mut spine)
                        | Neutral::If(_, _, _, ref mut spine)
                        | Neutral::Proj(_, _, ref mut spine) => spine.push(arg),
                    }

                    Ok(RcValue::from(Value::Neutral(neutral.clone())))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction),
            }
        },

        // E-IF, E-IF-TRUE, E-IF-FALSE
        Term::If(ref cond, ref if_true, ref if_false) => {
            let value_cond = normalize(context, cond)?;

            match *value_cond {
                Value::Literal(Literal::Bool(true)) => normalize(context, if_true),
                Value::Literal(Literal::Bool(false)) => normalize(context, if_false),
                Value::Neutral(ref cond) => Ok(RcValue::from(Value::from(Neutral::If(
                    cond.clone(),
                    normalize(context, if_true)?,
                    normalize(context, if_false)?,
                    vec![],
                )))),
                _ => Err(InternalError::ExpectedBoolExpr),
            }
        },

        // E-RECORD-TYPE
        Term::RecordType(ref scope) => {
            let ((label, Embed(ann)), body) = scope.clone().unbind();
            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(Value::RecordType(Scope::new((label, Embed(ann)), body)).into())
        },

        // E-EMPTY-RECORD-TYPE
        Term::RecordTypeEmpty => Ok(RcValue::from(Value::RecordTypeEmpty)),

        // E-RECORD
        Term::Record(ref scope) => {
            let ((label, Embed(term)), body) = scope.clone().unbind();
            let value = normalize(context, &term)?;
            let body = normalize(context, &body)?;

            Ok(Value::Record(Scope::new((label, Embed(value)), body)).into())
        },

        // E-EMPTY-RECORD
        Term::RecordEmpty => Ok(RcValue::from(Value::RecordEmpty)),

        // E-PROJ
        Term::Proj(ref expr, ref label) => match *normalize(context, expr)? {
            Value::Neutral(ref neutral) => Ok(RcValue::from(Value::from(Neutral::Proj(
                neutral.clone(),
                label.clone(),
                vec![],
            )))),
            ref expr => match expr.lookup_record(label) {
                Some(value) => Ok(value.clone()),
                None => Err(InternalError::ProjectedOnNonExistentField {
                    label: label.clone(),
                }),
            },
        },

        Term::Array(ref elems) => Ok(RcValue::from(Value::Array(
            elems
                .iter()
                .map(|elem| normalize(context, elem))
                .collect::<Result<_, _>>()?,
        ))),
    }
}

fn is_name(ty: &Type, name: &str) -> bool {
    if let Value::Neutral(ref neutral) = *ty {
        if let Neutral::App(Head::Var(Var::Free(ref n)), ref spine) = **neutral {
            return FreeVar::user(name) == *n && spine.is_empty();
        }
    }
    false
}

/// Check that `ty1` is a subtype of `ty2`
pub fn is_subtype(ty1: &RcType, ty2: &RcType) -> bool {
    use num_bigint::BigInt;
    use std::{i16, i32, i64, u16, u32, u64};

    fn int_ty<T: Into<BigInt>>(min: Option<T>, max: Option<T>) -> RcValue {
        RcValue::from(Value::IntType(
            min.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
            max.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
        ))
    }

    match (&*ty1.inner, &*ty2.inner) {
        (&Value::IntType(ref min1, ref max1), &Value::IntType(ref min2, ref max2)) => {
            let in_min_bound = match (min1, min2) {
                (None, None) => true,     // -∞ <= -∞
                (Some(_), None) => true,  //  n <= -∞
                (None, Some(_)) => false, // -∞ <=  n
                (Some(ref min1), Some(ref min2)) => match (&*min1.inner, &*min2.inner) {
                    (
                        Value::Literal(Literal::Int(ref min1)),
                        Value::Literal(Literal::Int(ref min2)),
                    ) => min1 >= min2,
                    _ => Value::term_eq(min1, min2), // Fallback to alpha-equality
                },
            };

            let in_max_bound = match (max1, max2) {
                (None, None) => true,     // +∞ <= +∞
                (Some(_), None) => true,  //  n <= +∞
                (None, Some(_)) => false, // +∞ <=  n
                (Some(ref max1), Some(ref max2)) => match (&*max1.inner, &*max2.inner) {
                    (
                        Value::Literal(Literal::Int(ref max1)),
                        Value::Literal(Literal::Int(ref max2)),
                    ) => max1 <= max2,
                    _ => Value::term_eq(max1, max2), // Fallback to alpha-equality
                },
            };

            in_min_bound && in_max_bound
        },

        (t1, _) if is_name(t1, "U16Le") => is_subtype(&int_ty(Some(u16::MIN), Some(u16::MAX)), ty2),
        (t1, _) if is_name(t1, "U32Le") => is_subtype(&int_ty(Some(u32::MIN), Some(u32::MAX)), ty2),
        (t1, _) if is_name(t1, "U64Le") => is_subtype(&int_ty(Some(u64::MIN), Some(u64::MAX)), ty2),
        (t1, _) if is_name(t1, "S16Le") => is_subtype(&int_ty(Some(i16::MIN), Some(i16::MAX)), ty2),
        (t1, _) if is_name(t1, "S32Le") => is_subtype(&int_ty(Some(i32::MIN), Some(i32::MAX)), ty2),
        (t1, _) if is_name(t1, "S64Le") => is_subtype(&int_ty(Some(i64::MIN), Some(i64::MAX)), ty2),
        (t1, t2) if is_name(t1, "F32Le") && is_name(t2, "F32") => true,
        (t1, t2) if is_name(t1, "F64Le") && is_name(t2, "F64") => true,
        (t1, _) if is_name(t1, "U16Be") => is_subtype(&int_ty(Some(u16::MIN), Some(u16::MAX)), ty2),
        (t1, _) if is_name(t1, "U32Be") => is_subtype(&int_ty(Some(u32::MIN), Some(u32::MAX)), ty2),
        (t1, _) if is_name(t1, "U64Be") => is_subtype(&int_ty(Some(u64::MIN), Some(u64::MAX)), ty2),
        (t1, _) if is_name(t1, "S16Be") => is_subtype(&int_ty(Some(i16::MIN), Some(i16::MAX)), ty2),
        (t1, _) if is_name(t1, "S32Be") => is_subtype(&int_ty(Some(i32::MIN), Some(i32::MAX)), ty2),
        (t1, _) if is_name(t1, "S64Be") => is_subtype(&int_ty(Some(i64::MIN), Some(i64::MAX)), ty2),
        (t1, t2) if is_name(t1, "F32Be") && is_name(t2, "F32") => true,
        (t1, t2) if is_name(t1, "F64Be") && is_name(t2, "F64") => true,

        // Fallback to alpha-equality
        _ => Type::term_eq(ty1, ty2),
    }
}

/// Type checking of terms
pub fn check(
    context: &Context,
    raw_term: &raw::RcTerm,
    expected_ty: &RcType,
) -> Result<RcTerm, TypeError> {
    match (&*raw_term.inner, &*expected_ty.inner) {
        (&raw::Term::Literal(_, raw::Literal::Int(_)), Value::IntType(_, _)) => {
            // Fallthrough to subtyping! We'll be checking that `{= val} <: {min .. max}`
        },
        (&raw::Term::Literal(span, ref raw_literal), ty) => {
            let literal = match *raw_literal {
                raw::Literal::String(ref val) if is_name(ty, "String") => {
                    Literal::String(val.clone())
                },
                raw::Literal::Char(val) if is_name(ty, "Char") => Literal::Char(val),

                // FIXME: overflow?
                raw::Literal::Int(ref val) if is_name(ty, "F32") => {
                    Literal::F32(val.to_f32().unwrap())
                },
                raw::Literal::Int(ref val) if is_name(ty, "F64") => {
                    Literal::F64(val.to_f64().unwrap())
                },
                raw::Literal::Float(val) if is_name(ty, "F32") => {
                    Literal::F32(val.to_f32().unwrap())
                },
                raw::Literal::Float(val) if is_name(ty, "F64") => Literal::F64(val),

                _ => {
                    return Err(TypeError::LiteralMismatch {
                        literal_span: span.0,
                        found: raw_literal.clone(),
                        expected: Box::new(expected_ty.resugar()),
                    });
                },
            };

            return Ok(RcTerm::from(Term::Literal(literal)));
        },

        // C-LAM
        (&raw::Term::Lam(_, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (pi_name, Embed(pi_ann)), pi_body) =
                Scope::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let raw::Term::Hole(_) = *lam_ann.inner {
                let lam_ann = RcTerm::from(Term::from(&*pi_ann));
                let lam_body = check(&context.claim(pi_name, pi_ann), &lam_body, &pi_body)?;
                let lam_scope = Scope::new((lam_name, Embed(lam_ann)), lam_body);

                return Ok(RcTerm::from(Term::Lam(lam_scope)));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and unbinding again at I-LAM
        },
        (&raw::Term::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: raw_term.span(),
                expected: Box::new(expected_ty.resugar()),
            });
        },

        // C-IF
        (&raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false), _) => {
            let bool_ty = RcValue::from(Value::from(Var::Free(FreeVar::user("Bool"))));
            let cond = check(context, raw_cond, &bool_ty)?;
            let if_true = check(context, raw_if_true, expected_ty)?;
            let if_false = check(context, raw_if_false, expected_ty)?;

            return Ok(RcTerm::from(Term::If(cond, if_true, if_false)));
        },

        // C-RECORD
        (&raw::Term::Record(span, ref scope), &Value::RecordType(ref ty_scope)) => {
            let ((label, Embed(raw_expr)), raw_body, (ty_label, Embed(ann)), ty_body) =
                Scope::unbind2(scope.clone(), ty_scope.clone());

            if Label::pattern_eq(&label, &ty_label) {
                let expr = check(context, &raw_expr, &ann)?;
                let ty_body =
                    normalize(context, &ty_body.substs(&[(label.0.clone(), expr.clone())]))?;
                let body = check(context, &raw_body, &ty_body)?;

                return Ok(RcTerm::from(Term::Record(Scope::new(
                    (label, Embed(expr)),
                    body,
                ))));
            } else {
                return Err(TypeError::LabelMismatch {
                    span: span.0,
                    found: label,
                    expected: ty_label,
                });
            }
        },

        (&raw::Term::Array(span, ref elems), ty) => match ty.free_app() {
            Some((name, [ref len, ref elem_ty])) if *name == FreeVar::user("Array") => {
                if let Value::Literal(Literal::Int(ref len)) = **len {
                    if *len != elems.len().into() {
                        return Err(TypeError::ArrayLengthMismatch {
                            span: span.0,
                            found_len: elems.len(),
                            expected_len: len.clone(),
                        });
                    }
                }

                return Ok(RcTerm::from(Term::Array(
                    elems
                        .iter()
                        .map(|elem| check(context, elem, elem_ty))
                        .collect::<Result<_, _>>()?,
                )));
            },
            Some(_) | None => unimplemented!(),
        },

        (&raw::Term::Hole(span), _) => {
            return Err(TypeError::UnableToElaborateHole {
                span: span.0,
                expected: Some(Box::new(expected_ty.resugar())),
            });
        },

        _ => {},
    }

    // C-CONV
    let (term, inferred_ty) = infer(context, raw_term)?;
    if is_subtype(&inferred_ty, expected_ty) {
        Ok(term)
    } else {
        Err(TypeError::Mismatch {
            span: raw_term.span(),
            found: Box::new(inferred_ty.resugar()),
            expected: Box::new(expected_ty.resugar()),
        })
    }
}

/// Type inference of terms
pub fn infer(context: &Context, raw_term: &raw::RcTerm) -> Result<(RcTerm, RcType), TypeError> {
    use std::cmp;

    /// Ensures that the given term is a universe, returning the level of that
    /// universe and its elaborated form.
    fn infer_universe(
        context: &Context,
        raw_term: &raw::RcTerm,
    ) -> Result<(RcTerm, Level), TypeError> {
        let (term, ty) = infer(context, raw_term)?;
        match *ty {
            Value::Universe(level) => Ok((term, level)),
            _ => Err(TypeError::ExpectedUniverse {
                span: raw_term.span(),
                found: Box::new(ty.resugar()),
            }),
        }
    }

    match *raw_term.inner {
        //  I-ANN
        raw::Term::Ann(_, ref raw_expr, ref raw_ty) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = normalize(context, &ty)?;
            let expr = check(context, raw_expr, &value_ty)?;

            Ok((RcTerm::from(Term::Ann(expr, ty)), value_ty))
        },

        // I-TYPE
        raw::Term::Universe(_, level) => Ok((
            RcTerm::from(Term::Universe(level)),
            RcValue::from(Value::Universe(level.succ())),
        )),

        raw::Term::Hole(span) => Err(TypeError::UnableToElaborateHole {
            span: span.0,
            expected: None,
        }),

        raw::Term::IntType(_, ref min, ref max) => {
            let min = match *min {
                None => None,
                Some(ref min) => Some(check(
                    context,
                    min,
                    &RcValue::from(Value::IntType(None, None)),
                )?),
            };

            let max = match *max {
                None => None,
                Some(ref max) => Some(check(
                    context,
                    max,
                    &RcValue::from(Value::IntType(None, None)),
                )?),
            };

            Ok((
                RcTerm::from(Term::IntType(min, max)),
                RcValue::from(Value::Universe(Level(0))),
            ))
        },

        raw::Term::Literal(span, ref raw_literal) => match *raw_literal {
            raw::Literal::String(ref value) => Ok((
                RcTerm::from(Term::Literal(Literal::String(value.clone()))),
                RcValue::from(Value::from(Var::Free(FreeVar::user("String")))),
            )),
            raw::Literal::Char(value) => Ok((
                RcTerm::from(Term::Literal(Literal::Char(value))),
                RcValue::from(Value::from(Var::Free(FreeVar::user("Char")))),
            )),
            raw::Literal::Int(ref value) => {
                Ok((RcTerm::from(Term::Literal(Literal::Int(value.clone()))), {
                    let value = RcValue::from(Value::Literal(Literal::Int(value.clone())));
                    RcValue::from(Value::IntType(Some(value.clone()), Some(value)))
                }))
            },
            raw::Literal::Float(_) => Err(TypeError::AmbiguousFloatLiteral { span: span.0 }),
        },

        // I-VAR
        raw::Term::Var(span, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_claim(name) {
                Some(ty) => Ok((RcTerm::from(Term::Var(var.clone())), ty.clone())),
                None => Err(TypeError::UndefinedName {
                    var_span: span.0,
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(index, ref hint) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: Some(raw_term.span()),
                index,
                hint: hint.clone(),
            }.into()),
        },

        // I-PI
        raw::Term::Pi(_, ref raw_scope) => {
            let ((name, Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(name.clone(), ann), &raw_body)?
            };

            Ok((
                RcTerm::from(Term::Pi(Scope::new((name, Embed(ann)), body))),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        // I-LAM
        raw::Term::Lam(_, ref raw_scope) => {
            let ((name, Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check for holes before entering to ensure we get a nice error
            if let raw::Term::Hole(_) = *raw_ann {
                return Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: name.clone(),
                });
            }

            let (lam_ann, _) = infer_universe(context, &raw_ann)?;
            let pi_ann = normalize(context, &lam_ann)?;
            let (lam_body, pi_body) =
                infer(&context.claim(name.clone(), pi_ann.clone()), &raw_body)?;

            let lam_param = (name.clone(), Embed(lam_ann));
            let pi_param = (name.clone(), Embed(pi_ann));

            Ok((
                RcTerm::from(Term::Lam(Scope::new(lam_param, lam_body))),
                RcValue::from(Value::Pi(Scope::new(pi_param, pi_body))),
            ))
        },

        // I-IF
        raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false) => {
            let bool_ty = RcValue::from(Value::from(Var::Free(FreeVar::user("Bool"))));
            let cond = check(context, raw_cond, &bool_ty)?;
            let (if_true, ty) = infer(context, raw_if_true)?;
            let if_false = check(context, raw_if_false, &ty)?;

            Ok((RcTerm::from(Term::If(cond, if_true, if_false)), ty))
        },

        // I-APP
        raw::Term::App(ref raw_expr, ref raw_arg) => {
            let (expr, expr_ty) = infer(context, raw_expr)?;

            match *expr_ty {
                Value::Pi(ref scope) => {
                    let ((name, Embed(ann)), body) = scope.clone().unbind();

                    let arg = check(context, raw_arg, &ann)?;
                    let body = normalize(context, &body.substs(&[(name, arg.clone())]))?;

                    Ok((RcTerm::from(Term::App(expr, arg)), body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: raw_expr.span(),
                    arg_span: raw_arg.span(),
                    found: Box::new(expr_ty.resugar()),
                }),
            }
        },

        // I-RECORD-TYPE
        raw::Term::RecordType(_, ref raw_scope) => {
            let ((label, Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check that rest of record is well-formed?
            // Might be able to skip that for now, because there's no way to
            // express ill-formed records in the concrete syntax...

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(label.0.clone(), ann), &raw_body)?
            };

            let scope = Scope::new((label, Embed(ann)), body);

            Ok((
                RcTerm::from(Term::RecordType(scope)),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        raw::Term::Record(span, _) => Err(TypeError::AmbiguousRecord { span: span.0 }),

        // I-EMPTY-RECORD-TYPE
        raw::Term::RecordTypeEmpty(_) => Ok((
            RcTerm::from(Term::RecordTypeEmpty),
            RcValue::from(Value::Universe(Level(0))),
        )),

        // I-EMPTY-RECORD
        raw::Term::RecordEmpty(_) => Ok((
            RcTerm::from(Term::RecordEmpty),
            RcValue::from(Value::RecordTypeEmpty),
        )),

        // I-PROJ
        raw::Term::Proj(_, ref expr, label_span, ref label) => {
            let (expr, ty) = infer(context, expr)?;

            match ty.lookup_record_ty(label) {
                Some(field_ty) => {
                    let mappings = field_substs(&expr, &label, &ty);
                    Ok((
                        RcTerm::from(Term::Proj(expr, label.clone())),
                        normalize(context, &field_ty.substs(&mappings))?,
                    ))
                },
                None => Err(TypeError::NoFieldInType {
                    label_span: label_span.0,
                    expected_label: label.clone(),
                    found: Box::new(ty.resugar()),
                }),
            }
        },

        raw::Term::Array(span, _) => Err(TypeError::AmbiguousArrayLiteral { span: span.0 }),
    }
}

fn field_substs(
    expr: &RcTerm,
    label: &Label<String>,
    ty: &RcType,
) -> Vec<(FreeVar<String>, RcTerm)> {
    let mut substs = vec![];
    let mut current_scope = ty.record_ty();

    while let Some(scope) = current_scope {
        let ((current_label, Embed(_)), body) = scope.unbind();

        if Label::pattern_eq(&current_label, &label) {
            break;
        }

        let proj = RcTerm::from(Term::Proj(expr.clone(), current_label.clone()));

        substs.push((current_label.0, proj));
        current_scope = body.record_ty();
    }

    substs
}
