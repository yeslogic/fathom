use codespan::{ByteOffset, ByteSpan};
use codespan_reporting::{Diagnostic, Label as DiagnosticLabel};
use im::HashMap;
use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};

use syntax::concrete;
use syntax::raw;
use syntax::{Label, Level};

#[cfg(test)]
mod tests;

/// The environment used when desugaring from the concrete to raw syntax
#[derive(Debug, Clone)]
pub struct DesugarEnv {
    /// An environment that maps strings to unique free variables
    ///
    /// This is a persistent map so that we can create new environments as we enter
    /// new scopes, allowing us to properly model variable shadowing.
    ///
    /// If we arrive at a variable that has not already been assigned a free name,
    /// we assume that it is a global name.
    locals: HashMap<String, FreeVar<String>>,
}

impl DesugarEnv {
    pub fn new(mappings: HashMap<String, FreeVar<String>>) -> DesugarEnv {
        DesugarEnv { locals: mappings }
    }

    pub fn on_item(&mut self, name: &str) -> Binder<String> {
        if let Some(free_var) = self.locals.get(name) {
            return Binder(free_var.clone());
        }
        Binder(self.on_binding(name))
    }

    pub fn on_binding(&mut self, name: &str) -> FreeVar<String> {
        let name = name.to_owned();
        let free_var = FreeVar::fresh_named(name.clone());
        self.locals.insert(name, free_var.clone());
        free_var
    }

    pub fn on_name(&self, span: ByteSpan, name: &str) -> raw::RcTerm {
        raw::RcTerm::from(raw::Term::Var(
            span,
            Var::Free(match self.locals.get(name) {
                None => FreeVar::fresh_named(name),
                Some(free_var) => free_var.clone(),
            }),
        ))
    }
}

/// An error produced during resugaring
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum DesugarError {
    #[fail(
        display = "Name had more than one declaration associated with it: `{}`",
        name
    )]
    DuplicateDeclarations {
        original_span: ByteSpan,
        duplicate_span: ByteSpan,
        name: String,
    },
    #[fail(display = "Declaration followed definition: `{}`", name)]
    DeclarationFollowedDefinition {
        definition_span: ByteSpan,
        declaration_span: ByteSpan,
        name: String,
    },
    #[fail(
        display = "Name had more than one definition associated with it: `{}`",
        name
    )]
    DuplicateDefinitions {
        original_span: ByteSpan,
        duplicate_span: ByteSpan,
        name: String,
    },
}

impl DesugarError {
    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            DesugarError::DuplicateDeclarations {
                original_span,
                duplicate_span,
                ref name,
            } => Diagnostic::new_error(format!(
                "name had more than one declaration associated with it `{}`",
                name,
            ))
            .with_label(
                DiagnosticLabel::new_primary(duplicate_span)
                    .with_message("the duplicated declaration"),
            )
            .with_label(
                DiagnosticLabel::new_secondary(original_span)
                    .with_message("the original declaration"),
            ),
            DesugarError::DeclarationFollowedDefinition {
                definition_span,
                declaration_span,
                name: _,
            } => Diagnostic::new_error(format!("declarations cannot follow definitions"))
                .with_label(
                    DiagnosticLabel::new_primary(declaration_span).with_message("the declaration"),
                )
                .with_label(
                    DiagnosticLabel::new_secondary(definition_span)
                        .with_message("the original definition"),
                ),
            DesugarError::DuplicateDefinitions {
                original_span,
                duplicate_span,
                ref name,
            } => Diagnostic::new_error(format!(
                "name had more than one definition associated with it `{}`",
                name,
            ))
            .with_label(
                DiagnosticLabel::new_primary(duplicate_span)
                    .with_message("the duplicated definition"),
            )
            .with_label(
                DiagnosticLabel::new_secondary(original_span)
                    .with_message("the original definition"),
            ),
        }
    }
}

/// Translate something to the corresponding core representation
pub trait Desugar<T> {
    fn desugar(&self, env: &DesugarEnv) -> Result<T, DesugarError>;
}

/// Convert a sugary pi type from something like:
///
/// ```text
/// (a b : t1) (c : t2) -> t3
/// ```
///
/// To a bunch of nested pi types like:
///
/// ```text
/// (a : t1) -> (b : t1) -> (c : t2) -> t3
/// ```
fn desugar_pi(
    env: &DesugarEnv,
    param_groups: &[concrete::PiParamGroup],
    body: &concrete::Term,
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = raw::RcTerm::from(ann.desugar(&env)?);
        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    Ok(params
        .into_iter()
        .rev()
        .fold(body.desugar(&env)?, |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Pi(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        }))
}

/// Convert a sugary lambda from something like:
///
/// ```text
/// \(a b : t1) c (d : t2) => t3
/// ```
///
/// To a bunch of nested lambdas like:
///
/// ```text
/// \(a : t1) => \(b : t1) => \c => \(d : t2) => t3
/// ```
fn desugar_lam(
    env: &DesugarEnv,
    param_groups: &[concrete::LamParamGroup],
    return_ann: Option<&concrete::Term>,
    body: &concrete::Term,
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = match *ann {
            None => raw::RcTerm::from(raw::Term::Hole(ByteSpan::default())),
            Some(ref ann) => ann.desugar(&env)?,
        };

        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    let body = match return_ann {
        None => body.desugar(&env)?,
        Some(ann) => raw::RcTerm::from(raw::Term::Ann(body.desugar(&env)?, ann.desugar(&env)?)),
    };

    Ok(params
        .into_iter()
        .rev()
        .fold(body, |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Lam(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        }))
}

fn desugar_struct(
    env: &DesugarEnv,
    span: ByteSpan,
    fields: &[concrete::StructField],
) -> Result<raw::RcTerm, DesugarError> {
    use syntax::concrete::StructField;

    let fields = fields
        .iter()
        .map(|field| match field {
            StructField::Punned {
                label: (_, ref name),
            } => {
                let var = env.on_name(span, name);
                Ok((Label(name.clone()), var))
            },
            StructField::Explicit {
                label: (_, ref name),
                ref term,
            } => {
                let term = term.desugar(&env)?;
                Ok((Label(name.clone()), term))
            },
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(raw::RcTerm::from(raw::Term::Struct(span, fields)))
}

fn desugar_items(
    env: &mut DesugarEnv,
    concrete_items: &[concrete::Item],
) -> Result<Nest<(Label, Binder<String>, Embed<raw::Definition>)>, DesugarError> {
    use im::HashMap;

    #[derive(Clone)]
    pub enum ForwardDecl {
        Pending(ByteSpan, raw::RcTerm),
        Defined(ByteSpan),
    }

    // Declarations that may be waiting to be defined
    let mut forward_declarations = HashMap::new();
    // The elaborated items, pre-allocated to improve performance
    let mut items = Vec::with_capacity(concrete_items.len());
    let hole = raw::RcTerm::from(raw::Term::Hole(ByteSpan::default()));

    // Iterate through the items in the module, checking each in turn
    for concrete_item in concrete_items {
        match *concrete_item {
            concrete::Item::Import { .. } => unimplemented!("import declarations"),

            concrete::Item::Declaration {
                name: (start, ref name),
                ref ann,
            } => {
                let binder = env.on_item(name);
                let name_span = ByteSpan::from_offset(start, ByteOffset::from_str(name));

                // Ensure that this declaration has not already been seen
                match forward_declarations.get(&binder) {
                    // There's already a definition associated with this name -
                    // we can't add a new declaration for it!
                    Some(&ForwardDecl::Defined(definition_span)) => {
                        return Err(DesugarError::DeclarationFollowedDefinition {
                            definition_span,
                            declaration_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // There's a declaration  for this name already pending - we
                    // can't add a new one!
                    Some(&ForwardDecl::Pending(original_span, _)) => {
                        return Err(DesugarError::DuplicateDeclarations {
                            original_span,
                            duplicate_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // No previous declaration for this name was seen, so we can
                    // go-ahead and type check, elaborate, and then add it to
                    // the context
                    None => {},
                }

                // Remember the declaration for when we get to a subsequent definition
                let declaration = ForwardDecl::Pending(name_span, ann.desugar(&env)?);
                forward_declarations.insert(binder.clone(), declaration);
            },

            concrete::Item::Definition(concrete::Definition::Alias {
                name: (start, ref name),
                ref params,
                ref return_ann,
                ref term,
            }) => {
                let binder = env.on_item(name);
                let name_span = ByteSpan::from_offset(start, ByteOffset::from_str(name));
                let term = desugar_lam(env, params, return_ann.as_ref().map(<_>::as_ref), term)?;
                let ty = match forward_declarations.get(&binder).cloned() {
                    // This declaration was already given a definition, so this
                    // is an error!
                    //
                    // NOTE: Some languages (eg. Haskell, Agda, Idris, and
                    // Erlang) turn duplicate definitions into case matches.
                    // Languages like Elm don't. What should we do here?
                    Some(ForwardDecl::Defined(original_span)) => {
                        return Err(DesugarError::DuplicateDefinitions {
                            original_span,
                            duplicate_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // We found a prior declaration, so we'll use it as a basis
                    // for checking the definition
                    Some(ForwardDecl::Pending(_, ty)) => ty.clone(),
                    // No prior declaration was found, so use a hole instead
                    None => hole.clone(),
                };

                // We must not remove this from the list of pending
                // declarations, lest we encounter another declaration or
                // definition of the same name later on!
                forward_declarations.insert(binder.clone(), ForwardDecl::Defined(name_span));
                // Add the definition to the elaborated items
                items.push((
                    Label(name.clone()),
                    binder,
                    Embed(raw::Definition::Alias { term, ty }),
                ));
            },

            concrete::Item::Definition(concrete::Definition::StructType {
                span,
                name: (start, ref name),
                ref params,
                ref fields,
            }) => {
                let binder = env.on_item(name);
                let name_span = ByteSpan::from_offset(start, ByteOffset::from_str(name));

                let scope = {
                    let mut env = env.clone();

                    let params = params
                        .iter()
                        .map(|&(ref name, ref ann)| {
                            let ann = ann.desugar(&env)?;
                            let binder = env.on_binding(name);
                            Ok((Binder(binder), Embed(ann)))
                        })
                        .collect::<Result<_, _>>()?;

                    let fields = fields
                        .iter()
                        .map(|field| {
                            let (_, ref label) = field.label;
                            let ann = field.ann.desugar(&env)?;
                            let free_var = env.on_binding(label);
                            Ok((Label(label.clone()), Binder(free_var), Embed(ann)))
                        })
                        .collect::<Result<_, _>>()?;

                    Scope::new(Nest::new(params), Scope::new(Nest::new(fields), ()))
                };

                match forward_declarations.get(&binder).cloned() {
                    // This declaration was already given a definition, so this
                    // is an error!
                    Some(ForwardDecl::Defined(original_span)) => {
                        return Err(DesugarError::DuplicateDefinitions {
                            original_span,
                            duplicate_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // We found a prior declaration, so we'll use it as a basis
                    // for checking the definition
                    Some(ForwardDecl::Pending(_, _)) => {
                        unimplemented!("forward struct definitions")
                    },
                    // No prior declaration was found
                    None => {},
                };

                // We must not remove this from the list of pending
                // declarations, lest we encounter another declaration or
                // definition of the same name later on!
                forward_declarations.insert(binder.clone(), ForwardDecl::Defined(name_span));
                // Add the definition to the elaborated items
                items.push((
                    Label(name.clone()),
                    binder,
                    Embed(raw::Definition::StructType { span, scope }),
                ));
            },

            concrete::Item::Error(_) => unimplemented!("error recovery"),
        }
    }

    Ok(Nest::new(items))
}

impl Desugar<raw::Module> for concrete::Module {
    fn desugar(&self, env: &DesugarEnv) -> Result<raw::Module, DesugarError> {
        let mut env = env.clone();
        match *self {
            concrete::Module::Valid {
                name: (_, ref name),
                items: ref concrete_items,
            } => Ok(raw::Module {
                name: name.clone(),
                items: desugar_items(&mut env, concrete_items)?,
            }),
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<raw::Literal> for concrete::Literal {
    fn desugar(&self, _: &DesugarEnv) -> Result<raw::Literal, DesugarError> {
        match *self {
            concrete::Literal::String(span, ref val) => Ok(raw::Literal::String(span, val.clone())),
            concrete::Literal::Char(span, val) => Ok(raw::Literal::Char(span, val)),
            concrete::Literal::Int(span, ref val, format) => {
                Ok(raw::Literal::Int(span, val.clone(), format))
            },
            concrete::Literal::Float(span, val, format) => {
                Ok(raw::Literal::Float(span, val, format))
            },
        }
    }
}

impl Desugar<(raw::RcPattern, DesugarEnv)> for concrete::Pattern {
    fn desugar(&self, env: &DesugarEnv) -> Result<(raw::RcPattern, DesugarEnv), DesugarError> {
        let span = self.span();
        match *self {
            concrete::Pattern::Parens(_, ref pattern) => pattern.desugar(env),
            concrete::Pattern::Ann(ref pattern, ref ty) => {
                let ty = ty.desugar(env)?;
                let (pattern, env) = pattern.desugar(env)?;
                let ann_pattern = raw::RcPattern::from(raw::Pattern::Ann(pattern, Embed(ty)));

                Ok((ann_pattern, env))
            },
            concrete::Pattern::Name(_, ref name) => match env.locals.get(name) {
                Some(free_var) => {
                    let var = Var::Free(free_var.clone());
                    let pattern = raw::RcPattern::from(raw::Pattern::Var(span, Embed(var)));

                    Ok((pattern, env.clone()))
                },
                None => {
                    let mut env = env.clone();
                    let free_var = env.on_binding(name);
                    let binder = Binder(free_var);
                    let pattern = raw::RcPattern::from(raw::Pattern::Binder(span, binder));

                    Ok((pattern, env))
                },
            },
            concrete::Pattern::Literal(ref literal) => Ok((
                raw::RcPattern::from(raw::Pattern::Literal(literal.desugar(env)?)),
                env.clone(),
            )),
            concrete::Pattern::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<raw::RcTerm> for concrete::Term {
    fn desugar(&self, env: &DesugarEnv) -> Result<raw::RcTerm, DesugarError> {
        let span = self.span();
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(env),
            concrete::Term::Ann(ref expr, ref ty) => Ok(raw::RcTerm::from(raw::Term::Ann(
                expr.desugar(env)?,
                ty.desugar(env)?,
            ))),
            concrete::Term::Universe(_, level) => Ok(raw::RcTerm::from(raw::Term::Universe(
                span,
                Level(level.unwrap_or(0)),
            ))),
            concrete::Term::IntTypeSingleton(_, ref value) => {
                let value = value.desugar(env)?;
                Ok(raw::RcTerm::from(raw::Term::IntType(
                    span,
                    Some(value.clone()),
                    Some(value),
                )))
            },
            concrete::Term::IntType(_, ref min, ref max) => {
                Ok(raw::RcTerm::from(raw::Term::IntType(
                    span,
                    match min {
                        None => None,
                        Some(x) => Some(x.desugar(env)?),
                    },
                    match max {
                        None => None,
                        Some(x) => Some(x.desugar(env)?),
                    },
                )))
            },
            concrete::Term::Extern(_, name_span, ref name) => Ok(raw::RcTerm::from(
                raw::Term::Extern(span, name_span, name.clone()),
            )),
            concrete::Term::Literal(ref literal) => {
                Ok(raw::RcTerm::from(raw::Term::Literal(literal.desugar(env)?)))
            },
            concrete::Term::Array(_, ref elems) => Ok(raw::RcTerm::from(raw::Term::Array(
                span,
                elems
                    .iter()
                    .map(|elem| elem.desugar(env))
                    .collect::<Result<_, _>>()?,
            ))),
            concrete::Term::Hole(_) => Ok(raw::RcTerm::from(raw::Term::Hole(span))),
            concrete::Term::Name(_, ref name) => Ok(env.on_name(span, name)),
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(env, params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(env, params, None, body),
            concrete::Term::Arrow(ref ann, ref body) => Ok(raw::RcTerm::from(raw::Term::Pi(
                span,
                Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(ann.desugar(env)?)),
                    body.desugar(env)?,
                ),
            ))),
            concrete::Term::App(ref head, ref args) => {
                args.iter().fold(head.desugar(env), |acc, arg| {
                    Ok(raw::RcTerm::from(raw::Term::App(acc?, arg.desugar(env)?)))
                })
            },
            concrete::Term::If(_, ref cond, ref if_true, ref if_false) => {
                let bool_pattern = |name: &str| {
                    raw::RcPattern::from(raw::Pattern::Var(
                        ByteSpan::default(),
                        Embed(Var::Free(match env.locals.get(name) {
                            Some(free_var) => free_var.clone(),
                            None => FreeVar::fresh_named("oops"),
                        })),
                    ))
                };

                Ok(raw::RcTerm::from(raw::Term::Match(
                    span,
                    cond.desugar(env)?,
                    vec![
                        Scope::new(bool_pattern("true"), if_true.desugar(&env)?),
                        Scope::new(bool_pattern("false"), if_false.desugar(&env)?),
                    ],
                )))
            },
            concrete::Term::Match(span, ref head, ref clauses) => {
                Ok(raw::RcTerm::from(raw::Term::Match(
                    span,
                    head.desugar(env)?,
                    clauses
                        .iter()
                        .map(|(pattern, term)| {
                            let (pattern, env) = pattern.desugar(env)?;
                            Ok(Scope::new(pattern, term.desugar(&env)?))
                        })
                        .collect::<Result<_, _>>()?,
                )))
            },
            concrete::Term::Struct(span, ref fields) => desugar_struct(env, span, fields),
            concrete::Term::Proj(ref tm, label_start, ref label) => {
                Ok(raw::RcTerm::from(raw::Term::Proj(
                    span,
                    tm.desugar(env)?,
                    ByteSpan::from_offset(label_start, ByteOffset::from_str(label)),
                    Label(label.clone()),
                )))
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}
