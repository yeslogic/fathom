use moniker::{Binder, Embed, FreeVar, Scope, Var};

use syntax::core::{Head, Neutral, Pattern, RcNeutral, RcPattern, RcTerm, RcValue, Term, Value};

use semantics::errors::InternalError;
use semantics::{Context, Definition};

/// Reduce a term to its normal form
pub fn nf_term(context: &Context, term: &RcTerm) -> Result<RcValue, InternalError> {
    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => nf_term(context, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::IntType(ref min, ref max) => {
            let min = match *min {
                None => None,
                Some(ref x) => Some(nf_term(context, x)?),
            };

            let max = match *max {
                None => None,
                Some(ref x) => Some(nf_term(context, x)?),
            };

            Ok(RcValue::from(Value::IntType(min, max)))
        },

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var) => match *var {
            Var::Free(ref name) => match context.get_definition(name) {
                Some(&Definition::Alias(ref term)) => nf_term(context, term),
                Some(&Definition::StructType(_)) | None => {
                    Ok(RcValue::from(Value::from(var.clone())))
                },
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                span: None,
                var: var.clone(),
            }),
        },

        Term::Extern(ref name) => Ok(RcValue::from(Value::from(Neutral::Head(Head::Extern(
            name.clone(),
        ))))),

        // E-PI
        Term::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Pi(Scope::new(
                (name, Embed(nf_term(context, &ann)?)),
                nf_term(context, &body)?,
            ))))
        },

        // E-LAM
        Term::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Lam(Scope::new(
                (name, Embed(nf_term(context, &ann)?)),
                nf_term(context, &body)?,
            ))))
        },

        // E-APP
        Term::App(ref head, ref arg) => {
            match *nf_term(context, head)?.inner {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((Binder(free_var), Embed(_)), body) = scope.clone().unbind();
                    nf_term(context, &body.substs(&[(free_var, arg.clone())]))
                },
                Value::Neutral(ref neutral, ref spine) => {
                    let arg = nf_term(context, arg)?;
                    let mut spine = spine.clone();

                    match *neutral.inner {
                        Neutral::Head(Head::Extern(ref name)) => {
                            spine.push(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully nfd
                            if let Some(prim) = context.get_extern_definition(name) {
                                if prim.arity == spine.len() && spine.iter().all(|arg| arg.is_nf())
                                {
                                    match (prim.interpretation)(spine) {
                                        Ok(value) => return Ok(value),
                                        Err(()) => unimplemented!("proper error"),
                                    }
                                }
                            }
                        },
                        Neutral::Head(Head::Var(_))
                        | Neutral::Proj(_, _)
                        | Neutral::Match(_, _) => spine.push(arg),
                    }

                    Ok(RcValue::from(Value::Neutral(neutral.clone(), spine)))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction),
            }
        },

        // E-STRUCT, E-EMPTY-STRUCT
        Term::Struct(ref fields) => {
            let fields = fields
                .iter()
                .map(|&(ref label, ref term)| Ok((label.clone(), nf_term(context, &term)?)))
                .collect::<Result<_, _>>()?;

            Ok(RcValue::from(Value::Struct(fields)))
        },

        // E-PROJ
        Term::Proj(ref expr, ref label) => {
            match *nf_term(context, expr)? {
                Value::Neutral(ref neutral, ref spine) => {
                    return Ok(RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Proj(neutral.clone(), label.clone())),
                        spine.clone(),
                    )));
                },
                Value::Struct(ref fields) => {
                    for &(ref current_label, ref current_expr) in fields {
                        if current_label == label {
                            return Ok(current_expr.clone());
                        }
                    }
                },
                _ => {},
            }

            Err(InternalError::ProjectedOnNonExistentField {
                label: label.clone(),
            })
        },

        // E-CASE
        Term::Match(ref head, ref clauses) => {
            let head = nf_term(context, head)?;

            if let Value::Neutral(ref neutral, ref spine) = *head {
                Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::Match(
                        neutral.clone(),
                        clauses
                            .iter()
                            .map(|clause| {
                                let (pattern, body) = clause.clone().unbind();
                                Ok(Scope::new(pattern, nf_term(context, &body)?))
                            })
                            .collect::<Result<_, _>>()?,
                    )),
                    spine.clone(),
                )))
            } else {
                for clause in clauses {
                    let (pattern, body) = clause.clone().unbind();
                    if let Some(mappings) = match_value(context, &pattern, &head)? {
                        let mappings = mappings
                            .into_iter()
                            .map(|(free_var, value)| (free_var, RcTerm::from(&*value.inner)))
                            .collect::<Vec<_>>();
                        return nf_term(context, &body.substs(&mappings));
                    }
                }
                Err(InternalError::NoPatternsApplicable)
            }
        },

        // E-ARRAY
        Term::Array(ref elems) => Ok(RcValue::from(Value::Array(
            elems
                .iter()
                .map(|elem| nf_term(context, elem))
                .collect::<Result<_, _>>()?,
        ))),
    }
}

/// If the pattern matches the value, this function returns the substitutions
/// needed to apply the pattern to some body expression
pub fn match_value(
    context: &Context,
    pattern: &RcPattern,
    value: &RcValue,
) -> Result<Option<Vec<(FreeVar<String>, RcValue)>>, InternalError> {
    match (&*pattern.inner, &*value.inner) {
        (&Pattern::Binder(Binder(ref free_var)), _) => {
            Ok(Some(vec![(free_var.clone(), value.clone())]))
        },
        (&Pattern::Var(Embed(Var::Free(ref free_var))), _) => match context
            .get_definition(free_var)
            .and_then(|definition| match definition {
                Definition::Alias(ref term) => Some(nf_term(context, term)),
                Definition::StructType(_) => None,
            }) {
            Some(Ok(ref term)) if term == value => Ok(Some(vec![])),
            Some(Ok(_)) | None => Ok(None),
            Some(Err(err)) => Err(err),
        },
        (&Pattern::Literal(ref pattern_lit), &Value::Literal(ref value_lit))
            if pattern_lit == value_lit =>
        {
            Ok(Some(vec![]))
        },
        (_, _) => Ok(None),
    }
}
