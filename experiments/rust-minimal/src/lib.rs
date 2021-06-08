//! Experimenting with a simple implementation of type theory elaboration
//!
//! Based on [Andras Korvacs' example type checker][02-typecheck-closures-debruijn].
//! We adapt it to Rust, using arenas for allocating source terms, and
//! reference-counting in values.
//!
//! [02-typecheck-closures-debruijn]: https://github.com/AndrasKovacs/elaboration-zoo/tree/master/02-typecheck-closures-debruijn

// TODO:
//
// - language features
//   - [x] let expressions
//   - [x] dependent functions
//   - [ ] dependent records
//   - [ ] top-level items
//   - [ ] recursive definitions
//   - [ ] binary format descriptions
//     - [ ] error formats
//     - [ ] map formats
//     - [ ] pure formats
//     - [ ] bind formats
// - implementation
//   - [x] command line interface
//   - [x] parser
//   - [ ] source location tracking
//   - [x] string interning
//   - [x] arena allocation
//   - [x] normalisation-by-evaluation
//   - [x] elaborator
//     - [ ] error recovery
//   - [ ] distiller
//   - [ ] pretty printing
//   - [ ] integration tests

/// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub type StringInterner = string_interner::StringInterner<
    StringId,
    string_interner::backend::BucketBackend<StringId>,
    std::hash::BuildHasherDefault<fxhash::FxHasher32>,
>;

/// Core language.
pub mod core {
    use crate::StringId;

    /// A [de Bruijn index][de-bruijn-index] in the current [environment].
    ///
    /// De Bruijn indices describe an occurrence of a variable in terms of the
    /// number of binders between the occurrence and its associated binder.
    /// For example:
    ///
    /// | Representation    | Example (S combinator)  |
    /// | ----------------- | ----------------------- |
    /// | Named             | `λx. λy. λz. x z (y z)` |
    /// | De Bruijn indices | `λ_. λ_. λ_. 2 0 (1 0)` |
    ///
    /// This is a helpful representation because it allows us to easily compare
    /// terms for equivalence based on their binding structure without maintaining a
    /// list of name substitutions. For example we want `λx. x` to be the same as
    /// `λy. y`. With de Bruijn indices these would both be described as `λ 0`.
    ///
    /// [environment]: `Env`
    /// [de-bruijn-index]: https://en.wikipedia.org/wiki/De_Bruijn_index
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct LocalVar(u16);

    pub fn local_vars() -> impl Iterator<Item = LocalVar> {
        (0..).map(LocalVar)
    }

    /// A de Bruijn level in the current [environment].
    ///
    /// This describes an occurrence of a variable by counting the binders inwards
    /// from the top of the term until the occurrence is reached. For example:
    ///
    /// | Representation    | Example (S combinator)  |
    /// | ----------------- | ----------------------- |
    /// | Named             | `λx. λy. λz. x z (y z)` |
    /// | De Bruijn levels  | `λ_. λ_. λ_. 0 2 (1 2)` |
    ///
    /// Levels are used in [values][semantics::Value] because they are not context-
    /// dependent (this is in contrast to [indices][LocalVar]). Because of this,
    /// we're able to sidestep the need for expensive variable shifting in the
    /// semantics. More information can be found in Soham Chowdhury's blog post,
    /// “[Real-world type theory I: untyped normalisation by evaluation for λ-calculus][untyped-nbe-for-lc]”.
    ///
    /// [environment]: `Env`
    /// [untyped-nbe-for-lc]: https://colimit.net/posts/normalisation-by-evaluation/
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct GlobalVar(u16);

    /// Length of the environment.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct EnvLen(u16);

    impl EnvLen {
        pub fn local_to_global(self, local: LocalVar) -> Option<GlobalVar> {
            Some(GlobalVar(self.0.checked_sub(local.0)?.checked_sub(1)?))
        }

        pub fn global_to_local(self, global: GlobalVar) -> Option<LocalVar> {
            Some(LocalVar(self.0.checked_sub(global.0)?.checked_sub(1)?))
        }

        pub fn next_global(self) -> GlobalVar {
            GlobalVar(self.0)
        }

        pub fn add_param(self) -> EnvLen {
            EnvLen(self.0 + 1) // FIXME: check overflow?
        }
    }

    /// A generic environment
    #[derive(Clone)]
    pub struct Env<Entry> {
        /// The entries in the environment.
        ///
        /// An `rpds::Vector` is used instead of an `im::Vector` as it's a bit
        /// more compact, which is important as we tend to clone environments
        /// often, and they contribute to the size of values.
        entries: rpds::VectorSync<Entry>,
    }

    impl<Entry> Env<Entry> {
        pub fn new() -> Env<Entry> {
            Env {
                entries: rpds::Vector::new_sync(),
            }
        }

        pub fn len(&self) -> EnvLen {
            EnvLen(self.entries.len() as u16)
        }

        pub fn get(&self, local: LocalVar) -> Option<&Entry> {
            let global_var = self.len().local_to_global(local)?;
            self.entries.get(global_var.0 as usize)
        }

        pub fn push_clone(&self, entry: Entry) -> Env<Entry> {
            assert!(self.entries.len() < u16::MAX as usize);
            Env {
                entries: self.entries.push_back(entry),
            }
        }

        pub fn push(&mut self, entry: Entry) {
            assert!(self.entries.len() < u16::MAX as usize);
            self.entries.push_back_mut(entry);
        }

        pub fn pop(&mut self) {
            self.entries.drop_last_mut();
        }
    }

    pub type TermRef<'arena> = &'arena Term<'arena>;

    /// Core language terms.
    pub enum Term<'arena> {
        /// Variable occurrences.
        Var(LocalVar),
        /// Let expressions.
        Let(StringId, TermRef<'arena>, TermRef<'arena>, TermRef<'arena>),
        /// The type of types.
        Universe,
        /// Dependent function types.
        ///
        /// Also known as: pi types, dependent product types.
        FunType(StringId, TermRef<'arena>, TermRef<'arena>),
        /// Function introductions.
        ///
        /// Also known as: lambda expressions, anonymous functions.
        FunIntro(StringId, TermRef<'arena>),
        /// Function eliminations.
        ///
        /// Also known as: function applications.
        FunElim(TermRef<'arena>, TermRef<'arena>),
        // RecordType(&'arena [StringId], &'arena [Term<'arena>]),
        // RecordIntro(&'arena [StringId], &'arena [Term<'arena>]),
        // RecordElim(TermRef<'arena>, StringId),
    }

    /// The semantics of the core language, implemented through the use of
    /// normalization-by-evaluation.
    pub mod semantics {
        use std::sync::Arc;

        use typed_arena::Arena;

        use crate::core::{Env, EnvLen, GlobalVar, Term, TermRef};
        use crate::StringId;

        pub type ValueEnv<'arena> = Env<Arc<Value<'arena>>>;

        /// Values in weak-head-normal form.
        #[derive(Clone)]
        pub enum Value<'arena> {
            /// A value whose computation has stopped as a result of trying to
            /// [evaluate][`eval`] an open [term][`Term`].
            Stuck(GlobalVar, Vec<Elim<'arena>>),
            /// Universes.
            Universe,
            /// Dependent function types.
            FunType(StringId, Arc<Value<'arena>>, Closure<'arena>),
            /// Function introductions.
            FunIntro(StringId, Closure<'arena>),
            // RecordType(&'arena [StringId], Telescope<'arena>),
            // RecordIntro(&'arena [StringId], Telescope<'arena>),
        }

        /// A pending elimination to be reduced if the [head][`Head`] of a
        /// [stuck value][`Value::Stuck`] becomes known.
        #[derive(Clone)]
        pub enum Elim<'arena> {
            /// Function eliminations.
            Fun(Arc<Value<'arena>>),
            // Record(StringId),
        }

        /// A closure is a term and a captured environment that will be later
        /// evaluated
        /// that will be later evaluated in the presence of an input expression.
        #[derive(Clone)]
        pub struct Closure<'arena> {
            /// Captured environment.
            env: ValueEnv<'arena>,
            /// The body expression.
            ///
            /// This can be evaluated using the captured environment with an
            /// expression pushed onto it.
            body_expr: TermRef<'arena>,
        }

        impl<'arena> Closure<'arena> {
            pub fn new(env: ValueEnv<'arena>, body_expr: TermRef<'arena>) -> Closure<'arena> {
                Closure { env, body_expr }
            }

            /// Apply an input to the closure.
            pub fn apply(
                &self,
                input_expr: Arc<Value<'arena>>,
            ) -> Result<Arc<Value<'arena>>, EvalError> {
                eval(&mut self.env.push_clone(input_expr), self.body_expr)
            }
        }

        // TODO: include stack trace(??)
        #[derive(Clone, Debug)]
        pub enum EvalError {
            MisboundLocal,
            InvalidFunctionElimHead,
        }

        pub fn normalise<'in_arena, 'out_arena>(
            arena: &'out_arena Arena<Term<'out_arena>>,
            env: &mut ValueEnv<'in_arena>,
            term: &Term<'in_arena>,
        ) -> Result<Term<'out_arena>, EvalError> {
            readback(arena, env.len(), eval(env, term)?.as_ref())
        }

        pub fn eval<'arena>(
            env: &mut ValueEnv<'arena>,
            term: &Term<'arena>,
        ) -> Result<Arc<Value<'arena>>, EvalError> {
            match term {
                Term::Var(local) => match env.get(*local) {
                    Some(value) => Ok(value.clone()),
                    None => Err(EvalError::MisboundLocal),
                },
                Term::Let(_, _, expr, body_expr) => {
                    let expr = eval(env, expr)?;
                    env.push(expr);
                    let body_expr = eval(env, body_expr);
                    env.pop();
                    body_expr
                }
                Term::Universe => Ok(Arc::new(Value::Universe)),
                Term::FunType(name, input_type, output_type) => {
                    let input_type = eval(env, input_type)?;
                    let output_type = Closure::new(env.clone(), output_type);
                    Ok(Arc::new(Value::FunType(*name, input_type, output_type)))
                }
                Term::FunIntro(name, output_expr) => {
                    let output_expr = Closure::new(env.clone(), output_expr);
                    Ok(Arc::new(Value::FunIntro(*name, output_expr)))
                }
                Term::FunElim(head_expr, input_expr) => {
                    let head_expr = eval(env, head_expr)?;
                    let input_expr = eval(env, input_expr)?;
                    fun_elim(head_expr, input_expr)
                }
            }
        }

        fn fun_elim<'arena>(
            mut head_expr: Arc<Value<'arena>>,
            input_expr: Arc<Value<'arena>>,
        ) -> Result<Arc<Value<'arena>>, EvalError> {
            match Arc::make_mut(&mut head_expr) {
                Value::FunIntro(_, output_expr) => output_expr.apply(input_expr),
                Value::Stuck(_, elims) => {
                    elims.push(Elim::Fun(input_expr));
                    Ok(head_expr)
                }
                _ => Err(EvalError::InvalidFunctionElimHead),
            }
        }

        /// Read a [value][`Value`] back into a [term][`Term`].
        pub fn readback<'in_arena, 'out_arena>(
            arena: &'out_arena Arena<Term<'out_arena>>,
            env_len: EnvLen,
            value: &Value<'in_arena>,
        ) -> Result<Term<'out_arena>, EvalError> {
            match value {
                Value::Stuck(global, elims) => {
                    let mut head_expr = Term::Var(env_len.global_to_local(*global).unwrap()); // FIXME: Unwrap
                    for elim in elims {
                        head_expr = match elim {
                            Elim::Fun(input_expr) => {
                                let input_expr = readback(arena, env_len, input_expr)?;
                                Term::FunElim(arena.alloc(head_expr), arena.alloc(input_expr))
                            }
                        };
                    }
                    Ok(head_expr)
                }
                Value::Universe => Ok(Term::Universe),
                Value::FunType(name, input_type, output_type) => {
                    let input_type = readback(arena, env_len, input_type)?;
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let output_type = output_type.apply(var)?;
                    let output_type = readback(arena, env_len.add_param(), &output_type)?;

                    Ok(Term::FunType(
                        *name,
                        arena.alloc(input_type),
                        arena.alloc(output_type),
                    ))
                }
                Value::FunIntro(name, output_expr) => {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let output_expr = output_expr.apply(var)?;
                    let output_expr = readback(arena, env_len.add_param(), &output_expr)?;

                    Ok(Term::FunIntro(*name, arena.alloc(output_expr)))
                }
            }
        }

        /// Check that one value is [computationally equal] to another value.
        ///
        /// This is sometimes referred to as 'conversion checking', or checking
        /// for 'definitional equality'.
        ///
        /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
        pub fn is_equal(
            env_len: EnvLen,
            value0: &Arc<Value<'_>>,
            value1: &Arc<Value<'_>>,
        ) -> Result<bool, EvalError> {
            match (value0.as_ref(), value1.as_ref()) {
                (Value::Stuck(global0, elims0), Value::Stuck(global1, elims1)) => {
                    if global0 != global1 || elims0.len() != elims1.len() {
                        return Ok(false);
                    }
                    for (elim0, elim1) in Iterator::zip(elims0.iter(), elims1.iter()) {
                        match (elim0, elim1) {
                            (Elim::Fun(input_expr0), Elim::Fun(input_expr1))
                                if is_equal(env_len, input_expr0, input_expr1)? => {}
                            (_, _) => return Ok(false),
                        }
                    }
                    Ok(true)
                }
                (Value::Universe, Value::Universe) => Ok(true),
                (
                    Value::FunType(_, input_type0, output_type0),
                    Value::FunType(_, input_type1, output_type1),
                ) => Ok(is_equal(env_len, input_type0, input_type1)? && {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let output_type0 = output_type0.apply(var.clone())?;
                    let output_type1 = output_type1.apply(var)?;

                    is_equal(env_len.add_param(), &output_type0, &output_type1)?
                }),
                (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let output_expr0 = output_expr0.apply(var.clone())?;
                    let output_expr1 = output_expr1.apply(var)?;

                    is_equal(env_len.add_param(), &output_expr0, &output_expr1)
                }

                // Eta-conversion
                (Value::FunIntro(_, output_expr), _) => {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let value0 = output_expr.apply(var.clone())?;
                    let value1 = fun_elim(value1.clone(), var)?;

                    is_equal(env_len.add_param(), &value0, &value1)
                }
                (_, Value::FunIntro(_, output_expr)) => {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let value0 = fun_elim(value0.clone(), var.clone())?;
                    let value1 = output_expr.apply(var)?;

                    is_equal(env_len.add_param(), &value0, &value1)
                }

                (_, _) => Ok(false),
            }
        }
    }
}

/// Surface language.
pub mod surface {
    use lalrpop_util::lalrpop_mod;
    use typed_arena::Arena;

    use crate::{StringId, StringInterner};

    pub mod lexer {
        use logos::Logos;

        #[derive(Clone, Debug, Logos)]
        pub enum Token<'source> {
            #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
            Ident(&'source str),

            #[token("fun")]
            KeywordFun,
            #[token("let")]
            KeywordLet,
            #[token("in")]
            KeywordIn,

            #[token(":")]
            Colon,
            #[token("=")]
            Equals,
            #[token("=>")]
            EqualsGreater,
            #[token("->")]
            HyphenGreater,

            #[token("(")]
            OpenParen,
            #[token(")")]
            CloseParen,

            #[error]
            #[regex(r"\p{Whitespace}", logos::skip)]
            #[regex(r"//(.*)\n", logos::skip)]
            Error,
        }

        pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

        pub fn tokens<'source>(
            source: &'source str,
        ) -> impl 'source + Iterator<Item = Result<Spanned<Token<'source>, usize>, ()>> {
            Token::lexer(source)
                .spanned()
                .map(|(token, range)| match token {
                    Token::Error => Err(()),
                    token => Ok((range.start, token, range.end)),
                })
        }
    }

    lalrpop_mod!(grammar);

    pub type TermRef<'arena> = &'arena Term<'arena>;

    pub enum Term<'arena> {
        Var(StringId),
        Let(StringId, TermRef<'arena>, TermRef<'arena>, TermRef<'arena>),
        Universe,
        FunType(StringId, TermRef<'arena>, TermRef<'arena>),
        FunIntro(StringId, TermRef<'arena>),
        FunElim(TermRef<'arena>, TermRef<'arena>),
        // RecordType(&'arena [(StringId, TermRef<'arena>)])
        // RecordTerm(&'arena [(StringId, TermRef<'arena>)])
        // RecordElim(TermRef<'arena>, StringId)
    }

    // TODO: Convert to an internal error message
    pub type ParseError<'source> = lalrpop_util::ParseError<usize, lexer::Token<'source>, ()>;

    impl<'arena> Term<'arena> {
        /// Parse a term from the `source` string, interning strings to the
        /// supplied `interner` and allocating nodes to the `arena`.
        pub fn parse<'source>(
            interner: &mut StringInterner,
            arena: &'arena Arena<Term<'arena>>,
            source: &'source str,
        ) -> Result<Term<'arena>, ParseError<'source>> {
            grammar::TermParser::new().parse(interner, arena, lexer::tokens(source))
        }
    }

    // TODO: pretty print terms
}

/// Bidirectional elaboration of the surface language into the core language.
pub mod elaboration {
    use std::sync::Arc;
    use typed_arena::Arena;

    use crate::core::semantics::{self, Value, ValueEnv};
    use crate::{core, surface, StringId};

    /// Elaboration context.
    pub struct Context<'arena> {
        /// Arena used for storing elaborated terms.
        arena: &'arena Arena<core::Term<'arena>>,
        /// Name environment.
        name_env: Vec<StringId>,
        /// Type environment.
        type_env: Vec<Arc<Value<'arena>>>,
        /// An environment of evaluated expressions.
        expr_env: ValueEnv<'arena>,
        /// Diagnostic messages encountered during elaboration.
        messages: Vec<String>,
    }

    impl<'arena> Context<'arena> {
        /// Construct a new elaboration context, backed by the supplied arena.
        pub fn new(arena: &'arena Arena<core::Term<'arena>>) -> Context<'arena> {
            Context {
                arena,
                name_env: Vec::new(),
                type_env: Vec::new(),
                expr_env: ValueEnv::new(),
                messages: Vec::new(),
            }
        }

        fn get_binding(&self, name: StringId) -> Option<(core::LocalVar, &Arc<Value<'arena>>)> {
            let mut bindings = itertools::izip!(
                core::local_vars(),
                self.name_env.iter().rev(),
                self.type_env.iter().rev(),
            );

            bindings.find_map(|(local_var, n, r#type)| match name == *n {
                true => Some((local_var, r#type)),
                false => None,
            })
        }

        fn push_binding(
            &mut self,
            name: StringId,
            value: Arc<Value<'arena>>,
            r#type: Arc<Value<'arena>>,
        ) {
            self.name_env.push(name);
            self.type_env.push(r#type);
            self.expr_env.push(value);
        }

        fn push_param(&mut self, name: StringId, r#type: Arc<Value<'arena>>) -> Arc<Value<'arena>> {
            let value = Arc::new(Value::Stuck(self.expr_env.len().next_global(), Vec::new()));
            self.push_binding(name, value.clone(), r#type);
            value
        }

        fn pop_binding(&mut self) {
            self.name_env.pop();
            self.type_env.pop();
            self.expr_env.pop();
        }

        fn push_message(&mut self, message: impl Into<String>) {
            self.messages.push(message.into());
        }

        pub fn drain_messages<'this>(&'this mut self) -> impl 'this + Iterator<Item = String> {
            self.messages.drain(..)
        }

        pub fn normalize<'out_arena>(
            &mut self,
            arena: &'out_arena Arena<core::Term<'out_arena>>,
            term: &core::Term<'arena>,
        ) -> Option<core::Term<'out_arena>> {
            semantics::normalise(arena, &mut self.expr_env, term).ok() // FIXME: record error
        }

        fn eval(&mut self, term: &core::Term<'arena>) -> Option<Arc<Value<'arena>>> {
            semantics::eval(&mut self.expr_env, term).ok() // FIXME: record error
        }

        fn is_equal(&mut self, value0: &Arc<Value<'_>>, value1: &Arc<Value<'_>>) -> Option<bool> {
            semantics::is_equal(self.expr_env.len(), value0, value1).ok() // FIXME: record error
        }

        fn apply_closure(
            &mut self,
            closure: &semantics::Closure<'arena>,
            input_expr: Arc<Value<'arena>>,
        ) -> Option<Arc<Value<'arena>>> {
            closure.apply(input_expr).ok() // FIXME: record error
        }

        /// Check that a surface term conforms to the given type.
        ///
        /// Returns the elaborated term in the core language.
        pub fn check(
            &mut self,
            surface_term: surface::TermRef<'_>,
            expected_type: &Arc<Value<'arena>>,
        ) -> Option<core::Term<'arena>> {
            match (surface_term, expected_type.as_ref()) {
                (surface::Term::Let(name, def_type, def_expr, body_expr), _) => {
                    let def_type = self.check(def_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                    let def_type_value = self.eval(&def_type)?;

                    let def_expr = self.check(def_expr, &def_type_value)?;
                    let def_expr_value = self.eval(&def_expr)?;

                    self.push_binding(*name, def_expr_value, def_type_value);
                    let body_expr = self.check(body_expr, expected_type);
                    self.pop_binding();

                    body_expr.map(|body_expr| {
                        core::Term::Let(
                            *name,
                            self.arena.alloc(def_expr),
                            self.arena.alloc(def_type),
                            self.arena.alloc(body_expr),
                        )
                    })
                }
                (
                    surface::Term::FunIntro(name, output_expr),
                    Value::FunType(_, input_type, output_type),
                ) => {
                    let input_expr = self.push_param(*name, input_type.clone());
                    let output_expr = {
                        let output_type = self.apply_closure(output_type, input_expr);
                        output_type.and_then(|output_type| self.check(output_expr, &output_type))
                    };
                    self.pop_binding();

                    output_expr.map(|output_expr| {
                        core::Term::FunIntro(*name, self.arena.alloc(output_expr))
                    })
                }
                (_, _) => {
                    let (core_term, synth_type) = self.synth(surface_term)?;

                    if self.is_equal(&synth_type, expected_type)? {
                        Some(core_term)
                    } else {
                        self.push_message("error: type mismatch");
                        None
                    }
                }
            }
        }

        /// Synthesize the type of the given surface term.
        ///
        /// Returns the elaborated term in the core language and its type.
        pub fn synth(
            &mut self,
            surface_term: surface::TermRef<'_>,
        ) -> Option<(core::Term<'arena>, Arc<Value<'arena>>)> {
            match surface_term {
                surface::Term::Var(var_name) => match self.get_binding(*var_name) {
                    Some((local_var, r#type)) => Some((core::Term::Var(local_var), r#type.clone())),
                    None => {
                        self.push_message("error: unknown variable");
                        None
                    }
                },
                surface::Term::Let(name, def_type, def_expr, body_expr) => {
                    let def_type = self.check(def_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                    let def_type_value = self.eval(&def_type)?;

                    let def_expr = self.check(def_expr, &def_type_value)?;
                    let def_expr_value = self.eval(&def_expr)?;

                    self.push_binding(*name, def_expr_value, def_type_value);
                    let body_expr = self.synth(body_expr);
                    self.pop_binding();

                    body_expr.map(|(body_expr, body_type)| {
                        let let_expr = core::Term::Let(
                            *name,
                            self.arena.alloc(def_expr),
                            self.arena.alloc(def_type),
                            self.arena.alloc(body_expr),
                        );

                        (let_expr, body_type)
                    })
                }
                surface::Term::Universe => Some((core::Term::Universe, Arc::new(Value::Universe))),
                surface::Term::FunType(name, input_type, output_type) => {
                    let input_type = self.check(input_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                    let input_type_value = self.eval(&input_type)?;

                    self.push_param(*name, input_type_value);
                    let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                    self.pop_binding();

                    output_type.map(|output_type| {
                        let fun_type = core::Term::FunType(
                            *name,
                            self.arena.alloc(input_type),
                            self.arena.alloc(output_type),
                        );

                        (fun_type, Arc::new(Value::Universe))
                    })
                }
                surface::Term::FunIntro(_, _) => {
                    self.push_message("error: ambiguous function introduction");
                    None
                }
                surface::Term::FunElim(head_expr, input_expr) => {
                    let (head_expr, head_type) = self.synth(head_expr)?;
                    match head_type.as_ref() {
                        Value::FunType(_, input_type, output_type) => {
                            let input_expr = self.check(input_expr, input_type)?;
                            let input_expr_value = self.eval(&input_expr)?;

                            let output_type = self.apply_closure(output_type, input_expr_value)?;

                            let fun_elim = core::Term::FunElim(
                                self.arena.alloc(head_expr),
                                self.arena.alloc(input_expr),
                            );

                            Some((fun_elim, output_type))
                        }
                        _ => {
                            self.push_message("error: expected a function type");
                            None
                        }
                    }
                }
            }
        }
    }
}

/// Distillation of the core language into the surface language.
pub mod distillation {
    use typed_arena::Arena;

    use crate::{core, surface, StringId};

    /// Distillation context.
    pub struct Context<'arena> {
        arena: &'arena Arena<surface::Term<'arena>>,
        names: Vec<StringId>,
    }

    impl<'arena> Context<'arena> {
        /// Construct a new distillation context.
        pub fn new(arena: &'arena Arena<surface::Term<'arena>>) -> Context<'arena> {
            Context {
                arena,
                names: Vec::new(),
            }
        }

        fn get_name(&self, local_var: core::LocalVar) -> StringId {
            // *self.names.get(todo!()).unwrap()
            todo!()
        }

        fn push_binding(&mut self, name: StringId) -> StringId {
            // TODO: choose optimal name
            self.names.push(name);
            name
        }

        fn pop_binding(&mut self) {
            self.names.pop();
        }

        pub fn check(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
            match core_term {
                core::Term::Let(name, def_type, def_expr, body_expr) => {
                    let def_type = self.synth(def_type);
                    let def_expr = self.synth(def_expr);

                    let name = self.push_binding(*name);
                    let body_expr = self.check(body_expr);
                    self.pop_binding();

                    surface::Term::Let(
                        name,
                        self.arena.alloc(def_type),
                        self.arena.alloc(def_expr),
                        self.arena.alloc(body_expr),
                    )
                }
                core::Term::FunIntro(name, output_expr) => {
                    let name = self.push_binding(*name);
                    let output_expr = self.check(output_expr);
                    self.pop_binding();

                    surface::Term::FunIntro(name, self.arena.alloc(output_expr))
                }
                _ => self.synth(core_term),
            }
        }

        pub fn synth(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
            match core_term {
                core::Term::Var(local_var) => surface::Term::Var(self.get_name(*local_var)),
                core::Term::Let(name, def_type, def_expr, body_expr) => {
                    let def_type = self.synth(def_type);
                    let def_expr = self.synth(def_expr);

                    let name = self.push_binding(*name);
                    let body_expr = self.synth(body_expr);
                    self.pop_binding();

                    surface::Term::Let(
                        name,
                        self.arena.alloc(def_type),
                        self.arena.alloc(def_expr),
                        self.arena.alloc(body_expr),
                    )
                }
                core::Term::Universe => surface::Term::Universe,
                core::Term::FunType(name, input_type, output_type) => {
                    let input_type = self.check(input_type);

                    let name = self.push_binding(*name);
                    let output_type = self.check(output_type);
                    self.pop_binding();

                    surface::Term::FunType(
                        name,
                        self.arena.alloc(input_type),
                        self.arena.alloc(output_type),
                    )
                }
                core::Term::FunIntro(name, output_expr) => {
                    let name = self.push_binding(*name);
                    let output_expr = self.synth(output_expr);
                    self.pop_binding();

                    surface::Term::FunIntro(name, self.arena.alloc(output_expr))
                }
                core::Term::FunElim(head_expr, input_expr) => {
                    let head_expr = self.synth(head_expr);
                    let input_expr = self.synth(input_expr);

                    surface::Term::FunElim(
                        self.arena.alloc(head_expr),
                        self.arena.alloc(input_expr),
                    )
                }
            }
        }
    }
}
