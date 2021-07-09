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
//   - [x] holes
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
//   - [x] pretty printing
//   - [x] source location tracking
//   - [x] string interning
//   - [x] arena allocation
//   - [x] normalisation-by-evaluation
//   - [x] elaborator
//     - [x] error recovery
//     - [ ] unification
//   - [x] distiller
//     - [ ] improve binder names
//     - [ ] improve meta names
//   - [ ] codespan diagnostics
//   - [ ] integration tests

/// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub type StringInterner = string_interner::StringInterner<
    StringId,
    string_interner::backend::BucketBackend<StringId>,
    std::hash::BuildHasherDefault<fxhash::FxHasher32>,
>;

pub type BytePos = usize;

#[derive(Debug, Copy, Clone)]
pub struct ByteRange {
    start: BytePos,
    end: BytePos,
}

impl ByteRange {
    pub const fn new(start: BytePos, end: BytePos) -> ByteRange {
        ByteRange { start, end }
    }

    pub const fn start(&self) -> BytePos {
        self.start
    }

    pub const fn end(&self) -> BytePos {
        self.start
    }
}

/// Core language.
pub mod core {
    use crate::StringId;

    /// Underlying variable representation.
    type RawVar = u16;

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
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct LocalVar(RawVar);

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
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct GlobalVar(RawVar);

    /// The length of an environment.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct EnvLen(RawVar);

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

    /// A uniquely owned environment.
    #[derive(Debug, Clone)]
    pub struct UniqueEnv<Entry> {
        entries: Vec<Entry>,
    }

    impl<Entry> UniqueEnv<Entry> {
        /// Construct a new, empty environment.
        pub fn new() -> UniqueEnv<Entry> {
            UniqueEnv {
                entries: Vec::new(),
            }
        }

        /// The length of the environment.
        pub fn len(&self) -> EnvLen {
            EnvLen(self.entries.len() as RawVar)
        }

        /// Lookup an entry in the environment.
        pub fn get(&self, local: LocalVar) -> Option<&Entry> {
            let global_var = self.len().local_to_global(local)?;
            self.entries.get(usize::from(global_var.0))
        }

        /// Push an entry onto the environment.
        pub fn push(&mut self, entry: Entry) {
            assert!(self.entries.len() < usize::from(u16::MAX));
            self.entries.push(entry);
        }

        /// Pop an entry off the environment.
        pub fn pop(&mut self) {
            self.entries.pop();
        }

        /// Iterate over the elements in the environment.
        pub fn iter<'this>(&'this self) -> impl 'this + DoubleEndedIterator<Item = &'this Entry> {
            self.entries.iter()
        }
    }

    /// A persistent environment with structural sharing.
    #[derive(Debug, Clone)]
    pub struct SharedEnv<Entry> {
        // An `rpds::Vector` is used instead of an `im::Vector` as it's a bit
        // more compact. We assume this is important because we tend to clone
        // environments often, and they contribute to the overall size of values.
        //
        // TODO: validate these assumptions by benchmarking
        //       against the following internal representations:
        //
        // - `Vec<_>`
        // - `im::Vector<_>`
        // - `Arc<im::Vector<_>>`
        entries: rpds::VectorSync<Entry>,
    }

    impl<Entry> SharedEnv<Entry> {
        /// Construct a new, empty environment.
        pub fn new() -> SharedEnv<Entry> {
            SharedEnv {
                entries: rpds::Vector::new_sync(),
            }
        }

        /// The length of the environment.
        pub fn len(&self) -> EnvLen {
            EnvLen(self.entries.len() as u16)
        }

        /// Lookup an entry in the environment.
        pub fn get(&self, local: LocalVar) -> Option<&Entry> {
            let global_var = self.len().local_to_global(local)?;
            self.entries.get(usize::from(global_var.0))
        }

        /// Push an entry onto a clone of the environment.
        pub fn push_clone(&self, entry: Entry) -> SharedEnv<Entry> {
            assert!(self.entries.len() < usize::from(u16::MAX));
            SharedEnv {
                entries: self.entries.push_back(entry),
            }
        }

        /// Push an entry onto the environment.
        pub fn push(&mut self, entry: Entry) {
            assert!(self.entries.len() < usize::from(u16::MAX));
            self.entries.push_back_mut(entry);
        }

        /// Pop an entry off the environment.
        pub fn pop(&mut self) {
            self.entries.drop_last_mut();
        }
    }

    /// Core language terms.
    #[derive(Debug, Clone)]
    pub enum Term<'arena> {
        /// Bound variable occurrences.
        BoundVar(LocalVar),
        /// Unification variable occurrences.
        ///
        /// Also known as: metavariables.
        UnificationVar(GlobalVar),
        /// Annotated expressions.
        Ann(&'arena Term<'arena>, &'arena Term<'arena>),
        /// Let expressions.
        Let(StringId, &'arena Term<'arena>, &'arena Term<'arena>),
        /// The type of types.
        Universe,
        /// Dependent function types.
        ///
        /// Also known as: pi types, dependent product types.
        FunType(StringId, &'arena Term<'arena>, &'arena Term<'arena>),
        /// Function introductions.
        ///
        /// Also known as: lambda expressions, anonymous functions.
        FunIntro(StringId, &'arena Term<'arena>),
        /// Function eliminations.
        ///
        /// Also known as: function applications.
        FunElim(&'arena Term<'arena>, &'arena Term<'arena>),
        // RecordType(&'arena [StringId], &'arena [Term<'arena>]),
        // RecordIntro(&'arena [StringId], &'arena [Term<'arena>]),
        // RecordElim(&'arena Term<'arena>, StringId),
        /// Reported errors.
        ReportedError,
    }

    /// Arena for storing data related to [`Term`]s.
    pub struct Arena<'arena> {
        terms: typed_arena::Arena<Term<'arena>>,
    }

    impl<'arena> Arena<'arena> {
        pub fn new() -> Arena<'arena> {
            Arena {
                terms: typed_arena::Arena::new(),
            }
        }

        pub fn alloc_term(&self, term: Term<'arena>) -> &mut Term<'arena> {
            self.terms.alloc(term)
        }
    }

    /// The semantics of the core language, implemented using
    /// _normalization by evaluation_.
    pub mod semantics {
        use std::sync::Arc;

        use crate::core::{Arena, EnvLen, GlobalVar, SharedEnv, Term};
        use crate::StringId;

        /// Values in weak-head-normal form.
        #[derive(Debug, Clone)]
        pub enum Value<'arena> {
            /// A value whose computation has stopped as a result of trying to
            /// [evaluate][`eval`] an open [term][`Term`].
            Stuck(Head, Vec<Elim<'arena>>),
            /// Universes.
            Universe,
            /// Dependent function types.
            FunType(StringId, Arc<Value<'arena>>, Closure<'arena>),
            /// Function introductions.
            FunIntro(StringId, Closure<'arena>),
            // RecordType(&'arena [StringId], Telescope<'arena>),
            // RecordIntro(&'arena [StringId], Vec<Arc<Value<'arena>>>),
        }

        impl<'arena> Value<'arena> {
            pub fn bound_var(global: GlobalVar) -> Value<'arena> {
                Value::Stuck(Head::BoundVar(global), Vec::new())
            }

            pub fn problem_var(global: GlobalVar) -> Value<'arena> {
                Value::Stuck(Head::UnificationVar(global), Vec::new())
            }

            pub fn reported_error() -> Value<'arena> {
                Value::Stuck(Head::ReportedError, Vec::new())
            }
        }

        /// The head of a [stuck value][`Value::Stuck`].
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Head {
            BoundVar(GlobalVar),
            UnificationVar(GlobalVar),
            ReportedError,
        }

        /// A pending elimination to be reduced if the [head][`Head`] of a
        /// [stuck value][`Value::Stuck`] becomes known.
        #[derive(Debug, Clone)]
        pub enum Elim<'arena> {
            /// Function eliminations.
            Fun(Arc<Value<'arena>>),
            // Record(StringId),
        }

        /// A closure is a term that can later be instantiated with a value.
        #[derive(Debug, Clone)]
        pub struct Closure<'arena> {
            env: SharedEnv<Arc<Value<'arena>>>,
            term: &'arena Term<'arena>,
        }

        impl<'arena> Closure<'arena> {
            pub fn new(
                env: SharedEnv<Arc<Value<'arena>>>,
                term: &'arena Term<'arena>,
            ) -> Closure<'arena> {
                Closure { env, term }
            }

            /// Instantiate the closure with a value.
            pub fn apply(
                &self,
                value: Arc<Value<'arena>>,
            ) -> Result<Arc<Value<'arena>>, EvalError> {
                eval(&mut self.env.push_clone(value), self.term)
            }
        }

        // TODO: include stack trace(??)
        #[derive(Clone, Debug)]
        pub enum EvalError {
            MisboundLocal,
            InvalidFunctionElimHead,
        }

        pub fn normalise<'in_arena, 'out_arena>(
            arena: &'out_arena Arena<'out_arena>,
            env: &mut SharedEnv<Arc<Value<'in_arena>>>,
            term: &Term<'in_arena>,
        ) -> Result<Term<'out_arena>, EvalError> {
            readback(arena, env.len(), eval(env, term)?.as_ref())
        }

        pub fn eval<'arena>(
            env: &mut SharedEnv<Arc<Value<'arena>>>,
            term: &Term<'arena>,
        ) -> Result<Arc<Value<'arena>>, EvalError> {
            match term {
                Term::BoundVar(var) => match env.get(*var) {
                    Some(value) => Ok(value.clone()),
                    None => Err(EvalError::MisboundLocal),
                },
                Term::UnificationVar(var) => Ok(Arc::new(Value::problem_var(*var))), // TODO: lookup in solution environment
                Term::Ann(expr, _) => eval(env, expr),
                Term::Let(_, def_expr, output_expr) => {
                    let def_expr = eval(env, def_expr)?;
                    env.push(def_expr);
                    let output_expr = eval(env, output_expr);
                    env.pop();
                    output_expr
                }
                Term::Universe => Ok(Arc::new(Value::Universe)),
                Term::FunType(input_name, input_type, output_type) => {
                    let input_type = eval(env, input_type)?;
                    let output_type = Closure::new(env.clone(), output_type);
                    Ok(Arc::new(Value::FunType(
                        *input_name,
                        input_type,
                        output_type,
                    )))
                }
                Term::FunIntro(input_name, output_expr) => {
                    let output_expr = Closure::new(env.clone(), output_expr);
                    Ok(Arc::new(Value::FunIntro(*input_name, output_expr)))
                }
                Term::FunElim(head_expr, input_expr) => {
                    let head_expr = eval(env, head_expr)?;
                    let input_expr = eval(env, input_expr)?;
                    fun_elim(head_expr, input_expr)
                }
                Term::ReportedError => Ok(Arc::new(Value::reported_error())),
            }
        }

        /// Apply a function elimination to an expression, performing
        /// [beta-reduction] if possible.
        ///
        /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
        fn fun_elim<'arena>(
            mut head_expr: Arc<Value<'arena>>,
            input_expr: Arc<Value<'arena>>,
        ) -> Result<Arc<Value<'arena>>, EvalError> {
            match Arc::make_mut(&mut head_expr) {
                // Beta-reduction
                Value::FunIntro(_, output_expr) => output_expr.apply(input_expr),
                // The computation is stuck, preventing further reduction
                Value::Stuck(_, spine) => {
                    spine.push(Elim::Fun(input_expr));
                    Ok(head_expr)
                }
                _ => Err(EvalError::InvalidFunctionElimHead),
            }
        }

        /// Read a [value][`Value`] back into a [term][`Term`].
        pub fn readback<'in_arena, 'out_arena>(
            arena: &'out_arena Arena<'out_arena>,
            env_len: EnvLen,
            value: &Value<'in_arena>,
        ) -> Result<Term<'out_arena>, EvalError> {
            match value {
                Value::Stuck(head, spine) => {
                    let mut head_expr = match head {
                        // FIXME: Unwrap
                        Head::BoundVar(var) => {
                            Term::BoundVar(env_len.global_to_local(*var).unwrap())
                        }
                        Head::UnificationVar(var) => Term::UnificationVar(*var),
                        Head::ReportedError => Term::ReportedError,
                    };

                    for elim in spine {
                        head_expr = match elim {
                            Elim::Fun(input_expr) => {
                                let input_expr = readback(arena, env_len, input_expr)?;
                                Term::FunElim(
                                    arena.alloc_term(head_expr),
                                    arena.alloc_term(input_expr),
                                )
                            }
                        };
                    }

                    Ok(head_expr)
                }
                Value::Universe => Ok(Term::Universe),
                Value::FunType(input_name, input_type, output_type) => {
                    let input_type = readback(arena, env_len, input_type)?;
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
                    let output_type = output_type.apply(var)?;
                    let output_type = readback(arena, env_len.add_param(), &output_type)?;

                    Ok(Term::FunType(
                        *input_name,
                        arena.alloc_term(input_type),
                        arena.alloc_term(output_type),
                    ))
                }
                Value::FunIntro(input_name, output_expr) => {
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
                    let output_expr = output_expr.apply(var)?;
                    let output_expr = readback(arena, env_len.add_param(), &output_expr)?;

                    Ok(Term::FunIntro(*input_name, arena.alloc_term(output_expr)))
                }
            }
        }

        /// Check that one value is [computationally equal] to another value.
        ///
        /// This is sometimes referred to as 'conversion checking', or checking
        /// for 'definitional equality'.
        ///
        /// We perform [eta-conversion] here, if possible.
        ///
        /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
        /// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
        pub fn is_equal(
            env_len: EnvLen,
            value0: &Arc<Value<'_>>,
            value1: &Arc<Value<'_>>,
        ) -> Result<bool, EvalError> {
            match (value0.as_ref(), value1.as_ref()) {
                // `ReportedError`s result from errors that have already been
                // reported, so we say that they are equal to any other value to
                // prevent them from triggering more errors.
                (Value::Stuck(Head::ReportedError, _), _)
                | (_, Value::Stuck(Head::ReportedError, _)) => Ok(true),

                (Value::Stuck(var0, spine0), Value::Stuck(var1, spine1)) => {
                    if var0 != var1 || spine0.len() != spine1.len() {
                        return Ok(false);
                    }
                    for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
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
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
                    let output_type0 = output_type0.apply(var.clone())?;
                    let output_type1 = output_type1.apply(var)?;

                    is_equal(env_len.add_param(), &output_type0, &output_type1)?
                }),
                (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
                    let output_expr0 = output_expr0.apply(var.clone())?;
                    let output_expr1 = output_expr1.apply(var)?;

                    is_equal(env_len.add_param(), &output_expr0, &output_expr1)
                }

                // Eta-conversion
                (Value::FunIntro(_, output_expr), _) => {
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
                    let value0 = output_expr.apply(var.clone())?;
                    let value1 = fun_elim(value1.clone(), var)?;

                    is_equal(env_len.add_param(), &value0, &value1)
                }
                (_, Value::FunIntro(_, output_expr)) => {
                    let var = Arc::new(Value::bound_var(env_len.next_global()));
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

    use crate::{BytePos, ByteRange, StringId, StringInterner};

    // FIXME: lexer should be private! LALRPOP's exports are somewhat broken though.
    //        See: https://github.com/lalrpop/lalrpop/pull/584#issuecomment-856731852
    pub(crate) mod lexer {
        use logos::Logos;

        #[derive(Clone, Debug, Logos)]
        pub enum Token<'source> {
            #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
            Ident(&'source str),

            #[token("fun")]
            KeywordFun,
            #[token("let")]
            KeywordLet,
            #[token("Type")]
            KeywordType,

            #[token(":")]
            Colon,
            #[token("=")]
            Equals,
            #[token("=>")]
            EqualsGreater,
            #[token("->")]
            HyphenGreater,
            #[token("?")]
            QuestionMark,
            #[token(";")]
            Semicolon,

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

    // TODO: Convert to an internal error message
    pub type ParseError<'source> = lalrpop_util::ParseError<usize, lexer::Token<'source>, ()>;

    /// Surface terms.
    #[derive(Debug, Clone)]
    pub enum Term<'arena> {
        Name(ByteRange, StringId),
        Hole(ByteRange, Option<StringId>),
        Ann(&'arena Term<'arena>, &'arena Term<'arena>),
        Let(
            BytePos,
            (ByteRange, StringId),
            Option<&'arena Term<'arena>>,
            &'arena Term<'arena>,
            &'arena Term<'arena>,
        ),
        Universe(ByteRange),
        FunType(
            BytePos,
            (ByteRange, StringId),
            &'arena Term<'arena>,
            &'arena Term<'arena>,
        ),
        FunIntro(BytePos, (ByteRange, StringId), &'arena Term<'arena>),
        FunElim(&'arena Term<'arena>, &'arena Term<'arena>),
        // RecordType(&'arena [(StringId, &'arena Term<'arena>)])
        // RecordTerm(&'arena [(StringId, &'arena Term<'arena>)])
        // RecordElim(&'arena Term<'arena>, StringId)
    }

    impl<'arena> Term<'arena> {
        /// Parse a term from the `source` string, interning strings to the
        /// supplied `interner` and allocating nodes to the `arena`.
        pub fn parse<'source>(
            interner: &mut StringInterner,
            arena: &'arena Arena<'arena>,
            source: &'source str,
        ) -> Result<Term<'arena>, ParseError<'source>> {
            grammar::TermParser::new().parse(interner, arena, lexer::tokens(source))
        }

        pub fn range(&self) -> ByteRange {
            ByteRange::new(self.start(), self.end())
        }

        fn start(&self) -> BytePos {
            match self {
                Term::Name(range, _) => range.start(),
                Term::Hole(range, _) => range.start(),
                Term::Ann(expr, _) => expr.start(),
                Term::Let(start, _, _, _, _) => *start,
                Term::Universe(range) => range.start(),
                Term::FunType(start, _, _, _) => *start,
                Term::FunIntro(start, _, _) => *start,
                Term::FunElim(head_expr, _) => head_expr.start(),
            }
        }

        fn end(&self) -> BytePos {
            match self {
                Term::Name(range, _) => range.end(),
                Term::Hole(range, _) => range.end(),
                Term::Ann(expr, _) => expr.end(),
                Term::Let(_, _, _, _, output_expr) => output_expr.end(),
                Term::Universe(range) => range.end(),
                Term::FunType(_, _, _, output_type) => output_type.end(),
                Term::FunIntro(_, _, output_expr) => output_expr.end(),
                Term::FunElim(_, input_expr) => input_expr.end(),
            }
        }
    }

    /// Arena for storing data related to [`Term`]s.
    pub struct Arena<'arena> {
        terms: typed_arena::Arena<Term<'arena>>,
    }

    impl<'arena> Arena<'arena> {
        pub fn new() -> Arena<'arena> {
            Arena {
                terms: typed_arena::Arena::new(),
            }
        }

        pub fn alloc_term(&self, term: Term<'arena>) -> &mut Term<'arena> {
            self.terms.alloc(term)
        }
    }

    pub mod pretty {
        use pretty::{DocAllocator, DocBuilder};

        use crate::surface::Term;
        use crate::{StringId, StringInterner};

        /// Term precedences
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum Prec {
            Top = 0,
            Let,
            Fun,
            App,
            Atomic,
        }

        pub struct Context<'doc, D> {
            interner: &'doc StringInterner,
            alloc: &'doc D,
        }

        impl<'doc, D> std::ops::Deref for Context<'doc, D> {
            type Target = &'doc D;

            // FIXME: `&&'doc D` is a bit ick... egh
            fn deref(&self) -> &&'doc D {
                &self.alloc
            }
        }

        impl<'doc, D> Context<'doc, D>
        where
            D: DocAllocator<'doc>,
        {
            pub fn new(interner: &'doc StringInterner, alloc: &'doc D) -> Context<'doc, D> {
                Context { interner, alloc }
            }

            pub fn name(&self, name: StringId) -> DocBuilder<'doc, D> {
                self.text(self.interner.resolve(name).unwrap_or("<ERROR>"))
            }

            pub fn ann(&self, expr: &Term<'_>, r#type: &Term<'_>) -> DocBuilder<'doc, D> {
                self.concat([
                    self.concat([
                        self.term_prec(Prec::Let, &expr),
                        self.space(),
                        self.text(":"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Top, &r#type),
                ])
            }

            pub fn paren(&self, wrap: bool, doc: DocBuilder<'doc, D>) -> DocBuilder<'doc, D> {
                if wrap {
                    self.concat([self.text("("), doc, self.text(")")])
                } else {
                    doc
                }
            }

            pub fn term(&self, term: &Term<'_>) -> DocBuilder<'doc, D> {
                self.term_prec(Prec::Top, term)
            }

            pub fn term_prec(&self, prec: Prec, term: &Term<'_>) -> DocBuilder<'doc, D> {
                // FIXME: indentation and grouping

                match term {
                    Term::Name(_, name) => self.name(*name),
                    Term::Hole(_, None) => self.text("?"),
                    Term::Hole(_, Some(name)) => self.concat([self.text("?"), self.name(*name)]),
                    Term::Ann(expr, r#type) => self.ann(expr, r#type),
                    Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => self.paren(
                        prec > Prec::Let,
                        self.concat([
                            self.concat([
                                self.text("let"),
                                self.space(),
                                self.name(*def_name),
                                self.space(),
                                match def_type {
                                    None => self.nil(),
                                    Some(def_type) => self.concat([
                                        self.text(":"),
                                        self.softline(),
                                        self.term_prec(Prec::Fun, def_type),
                                        self.space(),
                                    ]),
                                },
                                self.text("="),
                                self.softline(),
                                self.term_prec(Prec::Let, def_expr),
                                self.text(";"),
                            ])
                            .group(),
                            self.line(),
                            self.term_prec(Prec::Let, output_expr),
                        ]),
                    ),
                    Term::Universe(_) => self.text("Type"),
                    Term::FunType(_, (_, input_name), input_type, output_type) => self.paren(
                        prec > Prec::Fun,
                        self.concat([
                            self.concat([
                                self.text("fun"),
                                self.space(),
                                self.text("("),
                                self.name(*input_name),
                                self.space(),
                                self.text(":"),
                                self.softline(),
                                self.term_prec(Prec::Top, input_type),
                                self.text(")"),
                                self.space(),
                                self.text("->"),
                            ])
                            .group(),
                            self.softline(),
                            self.term_prec(Prec::Let, output_type),
                        ]),
                    ),
                    Term::FunIntro(_, (_, input_name), output_expr) => self.paren(
                        prec > Prec::Fun,
                        self.concat([
                            self.concat([
                                self.text("fun"),
                                self.space(),
                                self.name(*input_name),
                                self.space(),
                                self.text("=>"),
                            ])
                            .group(),
                            self.space(),
                            self.term_prec(Prec::Let, output_expr),
                        ]),
                    ),
                    Term::FunElim(head_expr, input_expr) => self.paren(
                        prec > Prec::App,
                        self.concat([
                            self.term_prec(Prec::App, head_expr),
                            self.space(),
                            self.term_prec(Prec::Atomic, input_expr),
                        ]),
                    ),
                }
            }
        }
    }

    /// Bidirectional elaboration of the surface language into the core language.
    pub mod elaboration {
        use std::sync::Arc;

        use crate::core::semantics::{self, Value};
        use crate::{core, surface, ByteRange, StringId};

        /// Elaboration context.
        pub struct Context<'arena> {
            /// Arena used for storing elaborated terms.
            arena: &'arena core::Arena<'arena>,
            /// Bound variable names
            bound_names: core::UniqueEnv<StringId>,
            /// Types of bound variables
            bound_types: core::UniqueEnv<Arc<Value<'arena>>>,
            /// Expressions associated with bound variables (used during evaluation).
            bound_exprs: core::SharedEnv<Arc<Value<'arena>>>,
            /// Unification variable names (added by named holes).
            unification_names: core::UniqueEnv<Option<StringId>>,
            /// Unification variable solutions.
            unification_solutions: core::UniqueEnv<Option<Arc<Value<'arena>>>>,
            /// Diagnostic messages encountered during elaboration.
            messages: Vec<(ByteRange, String)>,
        }

        impl<'arena> Context<'arena> {
            /// Construct a new elaboration context, backed by the supplied arena.
            pub fn new(arena: &'arena core::Arena<'arena>) -> Context<'arena> {
                Context {
                    arena,
                    bound_names: core::UniqueEnv::new(),
                    bound_types: core::UniqueEnv::new(),
                    bound_exprs: core::SharedEnv::new(),
                    unification_names: core::UniqueEnv::new(),
                    unification_solutions: core::UniqueEnv::new(),
                    messages: Vec::new(),
                }
            }

            fn get_binding(&self, name: StringId) -> Option<(core::LocalVar, &Arc<Value<'arena>>)> {
                let bindings = Iterator::zip(core::local_vars(), self.bound_types.iter().rev());

                Iterator::zip(self.bound_names.iter().rev(), bindings)
                    .find_map(|(n, binding)| (name == *n).then(|| binding))
            }

            /// Push a binding onto the context.
            fn push_binding(
                &mut self,
                name: StringId,
                expr: Arc<Value<'arena>>,
                r#type: Arc<Value<'arena>>,
            ) {
                self.bound_names.push(name);
                self.bound_types.push(r#type);
                self.bound_exprs.push(expr);
            }

            /// Push an abstract binding onto the context.
            fn push_param(
                &mut self,
                name: StringId,
                r#type: Arc<Value<'arena>>,
            ) -> Arc<Value<'arena>> {
                // Create a variable that refers to itself (once pushed onto the context).
                let expr = Arc::new(Value::bound_var(self.bound_exprs.len().next_global()));
                self.push_binding(name, expr.clone(), r#type);
                expr
            }

            /// Pop a binding off the context.
            fn pop_binding(&mut self) {
                self.bound_names.pop();
                self.bound_types.pop();
                self.bound_exprs.pop();
            }

            /// Push a fresh unification problem onto the context.
            fn push_unification_problem(&mut self, name: Option<StringId>) -> core::Term<'arena> {
                // TODO: check that hole name is not already in use
                let fresh_var = self.unification_solutions.len().next_global();
                self.unification_names.push(name);
                self.unification_solutions.push(None);
                core::Term::UnificationVar(fresh_var)
            }

            fn push_message(&mut self, range: ByteRange, message: impl Into<String>) {
                self.messages.push((range, message.into()));
            }

            pub fn drain_messages<'this>(
                &'this mut self,
            ) -> impl 'this + Iterator<Item = (ByteRange, String)> {
                self.messages.drain(..)
            }

            pub fn normalize<'out_arena>(
                &mut self,
                arena: &'out_arena core::Arena<'out_arena>,
                term: &core::Term<'arena>,
            ) -> core::Term<'out_arena> {
                semantics::normalise(arena, &mut self.bound_exprs, term)
                    .unwrap_or_else(|_| todo!("report error"))
            }

            pub fn eval(&mut self, term: &core::Term<'arena>) -> Arc<Value<'arena>> {
                semantics::eval(&mut self.bound_exprs, term)
                    .unwrap_or_else(|_| todo!("report error"))
            }

            pub fn readback<'out_arena>(
                &mut self,
                arena: &'out_arena core::Arena<'out_arena>,
                value: &Arc<Value<'arena>>,
            ) -> core::Term<'out_arena> {
                semantics::readback(arena, self.bound_exprs.len(), value)
                    .unwrap_or_else(|_| todo!("report error"))
            }

            fn is_equal(&mut self, value0: &Arc<Value<'_>>, value1: &Arc<Value<'_>>) -> bool {
                semantics::is_equal(self.bound_exprs.len(), value0, value1)
                    .unwrap_or_else(|_| todo!("report error"))
            }

            fn apply_closure(
                &mut self,
                closure: &semantics::Closure<'arena>,
                input_expr: Arc<Value<'arena>>,
            ) -> Arc<Value<'arena>> {
                closure
                    .apply(input_expr)
                    .unwrap_or_else(|_| todo!("report error"))
            }

            /// Check that a surface term conforms to the given type.
            ///
            /// Returns the elaborated term in the core language.
            pub fn check(
                &mut self,
                surface_term: &surface::Term<'_>,
                expected_type: &Arc<Value<'arena>>,
            ) -> core::Term<'arena> {
                match (surface_term, expected_type.as_ref()) {
                    (surface::Term::Let(_, (_, def_name), def_type, def_expr, output_expr), _) => {
                        let (def_expr, def_type_value) = match def_type {
                            None => self.synth(def_expr),
                            Some(def_type) => {
                                let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                                let def_type_value = self.eval(&def_type);

                                let def_expr = self.check(def_expr, &def_type_value);
                                let def_expr = core::Term::Ann(
                                    self.arena.alloc_term(def_expr),
                                    self.arena.alloc_term(def_type),
                                );

                                (def_expr, def_type_value)
                            }
                        };

                        let def_expr_value = self.eval(&def_expr);

                        self.push_binding(*def_name, def_expr_value, def_type_value);
                        let output_expr = self.check(output_expr, expected_type);
                        self.pop_binding();

                        core::Term::Let(
                            *def_name,
                            self.arena.alloc_term(def_expr),
                            self.arena.alloc_term(output_expr),
                        )
                    }
                    (
                        surface::Term::FunIntro(_, (_, input_name), output_expr),
                        Value::FunType(_, input_type, output_type),
                    ) => {
                        let input_expr = self.push_param(*input_name, input_type.clone());
                        let output_type = self.apply_closure(output_type, input_expr);
                        let output_expr = self.check(output_expr, &output_type);
                        self.pop_binding();

                        core::Term::FunIntro(*input_name, self.arena.alloc_term(output_expr))
                    }
                    (_, _) => {
                        let (core_term, synth_type) = self.synth(surface_term);

                        if self.is_equal(&synth_type, expected_type) {
                            core_term
                        } else {
                            self.push_message(surface_term.range(), "error: type mismatch");
                            core::Term::ReportedError
                        }
                    }
                }
            }

            /// Synthesize the type of the given surface term.
            ///
            /// Returns the elaborated term in the core language and its type.
            pub fn synth(
                &mut self,
                surface_term: &surface::Term<'_>,
            ) -> (core::Term<'arena>, Arc<Value<'arena>>) {
                match surface_term {
                    surface::Term::Name(_, name) => match self.get_binding(*name) {
                        Some((local_var, r#type)) => {
                            (core::Term::BoundVar(local_var), r#type.clone())
                        }
                        None => {
                            self.push_message(surface_term.range(), "error: unknown variable");
                            let r#type = self.push_unification_problem(None);
                            (core::Term::ReportedError, self.eval(&r#type))
                        }
                    },
                    surface::Term::Hole(_, name) => {
                        let r#type = self.push_unification_problem(None);
                        let expr = self.push_unification_problem(*name);
                        (expr, self.eval(&r#type))
                    }
                    surface::Term::Ann(expr, r#type) => {
                        let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let type_value = self.eval(&r#type);
                        let expr = self.check(expr, &type_value);

                        let ann_expr = core::Term::Ann(
                            self.arena.alloc_term(expr),
                            self.arena.alloc_term(r#type),
                        );

                        (ann_expr, type_value)
                    }
                    surface::Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => {
                        let (def_expr, def_type_value) = match def_type {
                            None => self.synth(def_expr),
                            Some(def_type) => {
                                let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                                let def_type_value = self.eval(&def_type);

                                let def_expr = self.check(def_expr, &def_type_value);
                                let def_expr = core::Term::Ann(
                                    self.arena.alloc_term(def_expr),
                                    self.arena.alloc_term(def_type),
                                );

                                (def_expr, def_type_value)
                            }
                        };

                        let def_expr_value = self.eval(&def_expr);

                        self.push_binding(*def_name, def_expr_value, def_type_value);
                        let (output_expr, output_type) = self.synth(output_expr);
                        self.pop_binding();

                        let let_expr = core::Term::Let(
                            *def_name,
                            self.arena.alloc_term(def_expr),
                            self.arena.alloc_term(output_expr),
                        );

                        (let_expr, output_type)
                    }
                    surface::Term::Universe(_) => (core::Term::Universe, Arc::new(Value::Universe)),
                    surface::Term::FunType(_, (_, input_name), input_type, output_type) => {
                        let input_type = self.check(input_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let input_type_value = self.eval(&input_type);

                        self.push_param(*input_name, input_type_value);
                        let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        self.pop_binding();

                        let fun_type = core::Term::FunType(
                            *input_name,
                            self.arena.alloc_term(input_type),
                            self.arena.alloc_term(output_type),
                        );

                        (fun_type, Arc::new(Value::Universe))
                    }
                    surface::Term::FunIntro(_, _, _) => {
                        self.push_message(
                            surface_term.range(),
                            "error: ambiguous function introduction",
                        );
                        let r#type = self.push_unification_problem(None);
                        (core::Term::ReportedError, self.eval(&r#type))
                    }
                    surface::Term::FunElim(head_expr, input_expr) => {
                        let (head_expr, head_type) = self.synth(head_expr);
                        match head_type.as_ref() {
                            Value::FunType(_, input_type, output_type) => {
                                let input_expr = self.check(input_expr, input_type);
                                let input_expr_value = self.eval(&input_expr);

                                let output_type = self.apply_closure(output_type, input_expr_value);

                                let fun_elim = core::Term::FunElim(
                                    self.arena.alloc_term(head_expr),
                                    self.arena.alloc_term(input_expr),
                                );

                                (fun_elim, output_type)
                            }
                            _ => {
                                self.push_message(
                                    surface_term.range(),
                                    "error: argument to applied non-function",
                                );
                                let r#type = self.push_unification_problem(None);
                                (core::Term::ReportedError, self.eval(&r#type))
                            }
                        }
                    }
                }
            }
        }
    }

    /// Bidirectional distillation of the core language into the surface language.
    pub mod distillation {
        use crate::{core, surface, BytePos, ByteRange, StringId};

        const PLACEHOLDER_POS: BytePos = 0;
        const PLACEHOLDER_RANGE: ByteRange = ByteRange::new(PLACEHOLDER_POS, PLACEHOLDER_POS);

        /// Distillation context.
        pub struct Context<'arena> {
            /// Arena for storing distilled terms.
            arena: &'arena surface::Arena<'arena>,
            /// Name environment.
            names: core::UniqueEnv<StringId>,
        }

        impl<'arena> Context<'arena> {
            /// Construct a new distillation context.
            pub fn new(arena: &'arena surface::Arena<'arena>) -> Context<'arena> {
                Context {
                    arena,
                    names: core::UniqueEnv::new(),
                }
            }

            fn get_name(&self, var: core::LocalVar) -> Option<StringId> {
                self.names.get(var).copied()
            }

            fn push_binding(&mut self, name: StringId) -> StringId {
                self.names.push(name); // TODO: ensure we chose a correctly bound name
                name
            }

            fn pop_binding(&mut self) {
                self.names.pop();
            }

            /// Distill a core term into a surface term, in a 'checkable' context.
            pub fn check(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
                match core_term {
                    core::Term::Ann(expr, _) => {
                        // Avoid adding extraneous type annotations!
                        self.check(expr)
                    }
                    core::Term::Let(def_name, def_expr, output_expr) => {
                        let (def_expr, def_type) = match self.synth(def_expr) {
                            surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                            expr => (self.arena.alloc_term(expr) as &_, None),
                        };

                        let def_name = self.push_binding(*def_name);
                        let output_expr = self.check(output_expr);
                        self.pop_binding();

                        surface::Term::Let(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, def_name),
                            def_type,
                            def_expr,
                            self.arena.alloc_term(output_expr),
                        )
                    }
                    core::Term::FunIntro(input_name, output_expr) => {
                        let input_name = self.push_binding(*input_name);
                        let output_expr = self.check(output_expr);
                        self.pop_binding();

                        surface::Term::FunIntro(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, input_name),
                            self.arena.alloc_term(output_expr),
                        )
                    }
                    _ => self.synth(core_term),
                }
            }

            /// Distill a core term into a surface term, in a 'synthesizable' context.
            pub fn synth(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
                match core_term {
                    core::Term::BoundVar(var) => match self.get_name(*var) {
                        Some(name) => surface::Term::Name(PLACEHOLDER_RANGE, name),
                        None => todo!("misbound variable"), // TODO: error?
                    },
                    core::Term::UnificationVar(_) => {
                        let name = None; // TODO: lookup in unification name environment
                        surface::Term::Hole(PLACEHOLDER_RANGE, name)
                    }
                    core::Term::Ann(expr, r#type) => {
                        let r#type = self.synth(r#type);
                        let expr = self.check(expr);

                        surface::Term::Ann(
                            self.arena.alloc_term(expr),
                            self.arena.alloc_term(r#type),
                        )
                    }
                    core::Term::Let(def_name, def_expr, output_expr) => {
                        let (def_expr, def_type) = match self.synth(def_expr) {
                            surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                            expr => (self.arena.alloc_term(expr) as &_, None),
                        };

                        let def_name = self.push_binding(*def_name);
                        let output_expr = self.synth(output_expr);
                        self.pop_binding();

                        surface::Term::Let(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, def_name),
                            def_type,
                            def_expr,
                            self.arena.alloc_term(output_expr),
                        )
                    }
                    core::Term::Universe => surface::Term::Universe(PLACEHOLDER_RANGE),
                    core::Term::FunType(input_name, input_type, output_type) => {
                        let input_type = self.check(input_type);

                        let input_name = self.push_binding(*input_name);
                        let output_type = self.check(output_type);
                        self.pop_binding();

                        surface::Term::FunType(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, input_name),
                            self.arena.alloc_term(input_type),
                            self.arena.alloc_term(output_type),
                        )
                    }
                    core::Term::FunIntro(input_name, output_expr) => {
                        let input_name = self.push_binding(*input_name);
                        let output_expr = self.synth(output_expr);
                        self.pop_binding();

                        surface::Term::FunIntro(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, input_name),
                            self.arena.alloc_term(output_expr),
                        )
                    }
                    core::Term::FunElim(head_expr, input_expr) => {
                        let head_expr = self.synth(head_expr);
                        let input_expr = self.synth(input_expr);

                        surface::Term::FunElim(
                            self.arena.alloc_term(head_expr),
                            self.arena.alloc_term(input_expr),
                        )
                    }
                    // NOTE: Not sure if this is a great approach!
                    core::Term::ReportedError => surface::Term::Hole(PLACEHOLDER_RANGE, None),
                }
            }
        }
    }
}
