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
//   - [ ] holes
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
//     - [ ] error recovery
//   - [x] distiller
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
            self.entries.get(global_var.0 as usize)
        }

        /// Push an entry onto the environment.
        pub fn push(&mut self, entry: Entry) {
            assert!(self.entries.len() < u16::MAX as usize);
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
            self.entries.get(global_var.0 as usize)
        }

        /// Push an entry onto a clone of the environment.
        pub fn push_clone(&self, entry: Entry) -> SharedEnv<Entry> {
            assert!(self.entries.len() < u16::MAX as usize);
            SharedEnv {
                entries: self.entries.push_back(entry),
            }
        }

        /// Push an entry onto the environment.
        pub fn push(&mut self, entry: Entry) {
            assert!(self.entries.len() < u16::MAX as usize);
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
        /// Variable occurrences.
        Var(LocalVar),
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
            Stuck(GlobalVar, Vec<Elim<'arena>>),
            /// Universes.
            Universe,
            /// Dependent function types.
            FunType(StringId, Arc<Value<'arena>>, Closure<'arena>),
            /// Function introductions.
            FunIntro(StringId, Closure<'arena>),
            // RecordType(&'arena [StringId], Telescope<'arena>),
            // RecordIntro(&'arena [StringId], Vec<Arc<Value<'arena>>>),
        }

        /// A pending elimination to be reduced if the [head][`Head`] of a
        /// [stuck value][`Value::Stuck`] becomes known.
        #[derive(Debug, Clone)]
        pub enum Elim<'arena> {
            /// Function eliminations.
            Fun(Arc<Value<'arena>>),
            // Record(StringId),
        }

        /// A closure is a term and a captured environment that will be later
        /// evaluated
        /// that will be later evaluated in the presence of an input expression.
        #[derive(Debug, Clone)]
        pub struct Closure<'arena> {
            /// Captured environment.
            env: SharedEnv<Arc<Value<'arena>>>,
            /// The body expression.
            ///
            /// This can be evaluated using the captured environment with an
            /// expression pushed onto it.
            body_expr: &'arena Term<'arena>,
        }

        impl<'arena> Closure<'arena> {
            pub fn new(
                env: SharedEnv<Arc<Value<'arena>>>,
                body_expr: &'arena Term<'arena>,
            ) -> Closure<'arena> {
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
                Term::Var(local) => match env.get(*local) {
                    Some(value) => Ok(value.clone()),
                    None => Err(EvalError::MisboundLocal),
                },
                Term::Ann(expr, _) => eval(env, expr),
                Term::Let(_, def_expr, body_expr) => {
                    let def_expr = eval(env, def_expr)?;
                    env.push(def_expr);
                    let body_expr = eval(env, body_expr);
                    env.pop();
                    body_expr
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
                Value::Stuck(_, elims) => {
                    elims.push(Elim::Fun(input_expr));
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
                Value::Stuck(global, elims) => {
                    let mut head_expr = Term::Var(env_len.global_to_local(*global).unwrap()); // FIXME: Unwrap
                    for elim in elims {
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
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
                    let output_type = output_type.apply(var)?;
                    let output_type = readback(arena, env_len.add_param(), &output_type)?;

                    Ok(Term::FunType(
                        *input_name,
                        arena.alloc_term(input_type),
                        arena.alloc_term(output_type),
                    ))
                }
                Value::FunIntro(input_name, output_expr) => {
                    let var = Arc::new(Value::Stuck(env_len.next_global(), Vec::new()));
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
        Var(ByteRange, StringId),
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
                Term::Var(range, _) => range.start(),
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
                Term::Var(range, _) => range.end(),
                Term::Ann(expr, _) => expr.end(),
                Term::Let(_, _, _, _, output_type) => output_type.end(),
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
                    Term::Var(_, name) => self.name(*name),
                    Term::Ann(expr, r#type) => self.ann(expr, r#type),
                    Term::Let(_, (_, def_name), def_type, def_expr, body_expr) => self.paren(
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
                            self.term_prec(Prec::Let, body_expr),
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
            /// Name environment.
            name_env: core::UniqueEnv<StringId>,
            /// Type environment.
            type_env: core::UniqueEnv<Arc<Value<'arena>>>,
            /// An environment of evaluated expressions.
            expr_env: core::SharedEnv<Arc<Value<'arena>>>,
            /// Diagnostic messages encountered during elaboration.
            messages: Vec<(ByteRange, String)>,
        }

        impl<'arena> Context<'arena> {
            /// Construct a new elaboration context, backed by the supplied arena.
            pub fn new(arena: &'arena core::Arena<'arena>) -> Context<'arena> {
                Context {
                    arena,
                    name_env: core::UniqueEnv::new(),
                    type_env: core::UniqueEnv::new(),
                    expr_env: core::SharedEnv::new(),
                    messages: Vec::new(),
                }
            }

            fn get_binding(&self, name: StringId) -> Option<(core::LocalVar, &Arc<Value<'arena>>)> {
                let bindings = Iterator::zip(core::local_vars(), self.type_env.iter().rev());

                Iterator::zip(self.name_env.iter().rev(), bindings)
                    .find_map(|(n, binding)| (name == *n).then(|| binding))
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

            fn push_param(
                &mut self,
                name: StringId,
                r#type: Arc<Value<'arena>>,
            ) -> Arc<Value<'arena>> {
                let value = Arc::new(Value::Stuck(self.expr_env.len().next_global(), Vec::new()));
                self.push_binding(name, value.clone(), r#type);
                value
            }

            fn pop_binding(&mut self) {
                self.name_env.pop();
                self.type_env.pop();
                self.expr_env.pop();
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
            ) -> Option<core::Term<'out_arena>> {
                semantics::normalise(arena, &mut self.expr_env, term).ok() // FIXME: record error
            }

            pub fn eval(&mut self, term: &core::Term<'arena>) -> Option<Arc<Value<'arena>>> {
                semantics::eval(&mut self.expr_env, term).ok() // FIXME: record error
            }

            pub fn readback<'out_arena>(
                &mut self,
                arena: &'out_arena core::Arena<'out_arena>,
                value: &Arc<Value<'arena>>,
            ) -> Option<core::Term<'out_arena>> {
                semantics::readback(arena, self.expr_env.len(), value).ok() // FIXME: record error
            }

            fn is_equal(
                &mut self,
                value0: &Arc<Value<'_>>,
                value1: &Arc<Value<'_>>,
            ) -> Option<bool> {
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
                surface_term: &surface::Term<'_>,
                expected_type: &Arc<Value<'arena>>,
            ) -> Option<core::Term<'arena>> {
                match (surface_term, expected_type.as_ref()) {
                    (surface::Term::Let(_, (_, def_name), def_type, def_expr, body_expr), _) => {
                        let (def_expr, def_type_value) = match def_type {
                            None => self.synth(def_expr)?,
                            Some(def_type) => {
                                let def_type = self.check(def_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                                let def_type_value = self.eval(&def_type)?;

                                let def_expr = self.check(def_expr, &def_type_value)?;
                                let def_expr = core::Term::Ann(
                                    self.arena.alloc_term(def_expr),
                                    self.arena.alloc_term(def_type),
                                );

                                (def_expr, def_type_value)
                            }
                        };

                        let def_expr_value = self.eval(&def_expr)?;

                        self.push_binding(*def_name, def_expr_value, def_type_value);
                        let body_expr = self.check(body_expr, expected_type);
                        self.pop_binding();

                        body_expr.map(|body_expr| {
                            core::Term::Let(
                                *def_name,
                                self.arena.alloc_term(def_expr),
                                self.arena.alloc_term(body_expr),
                            )
                        })
                    }
                    (
                        surface::Term::FunIntro(_, (_, input_name), output_expr),
                        Value::FunType(_, input_type, output_type),
                    ) => {
                        let input_expr = self.push_param(*input_name, input_type.clone());
                        let output_expr = self
                            .apply_closure(output_type, input_expr)
                            .and_then(|output_type| self.check(output_expr, &output_type));
                        self.pop_binding();

                        output_expr.map(|output_expr| {
                            core::Term::FunIntro(*input_name, self.arena.alloc_term(output_expr))
                        })
                    }
                    (_, _) => {
                        let (core_term, synth_type) = self.synth(surface_term)?;

                        if self.is_equal(&synth_type, expected_type)? {
                            Some(core_term)
                        } else {
                            self.push_message(surface_term.range(), "error: type mismatch");
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
                surface_term: &surface::Term<'_>,
            ) -> Option<(core::Term<'arena>, Arc<Value<'arena>>)> {
                match surface_term {
                    surface::Term::Var(_, var_name) => match self.get_binding(*var_name) {
                        Some((local_var, r#type)) => {
                            Some((core::Term::Var(local_var), r#type.clone()))
                        }
                        None => {
                            self.push_message(surface_term.range(), "error: unknown variable");
                            None
                        }
                    },
                    surface::Term::Ann(expr, r#type) => {
                        let r#type = self.check(r#type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                        let type_value = self.eval(&r#type)?;

                        let expr = self.check(expr, &type_value)?;

                        Some((
                            core::Term::Ann(
                                self.arena.alloc_term(expr),
                                self.arena.alloc_term(r#type),
                            ),
                            type_value,
                        ))
                    }
                    surface::Term::Let(_, (_, def_name), def_type, def_expr, body_expr) => {
                        let (def_expr, def_type_value) = match def_type {
                            None => self.synth(def_expr)?,
                            Some(def_type) => {
                                let def_type = self.check(def_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                                let def_type_value = self.eval(&def_type)?;

                                let def_expr = self.check(def_expr, &def_type_value)?;
                                let def_expr = core::Term::Ann(
                                    self.arena.alloc_term(def_expr),
                                    self.arena.alloc_term(def_type),
                                );

                                (def_expr, def_type_value)
                            }
                        };

                        let def_expr_value = self.eval(&def_expr)?;

                        self.push_binding(*def_name, def_expr_value, def_type_value);
                        let body_expr = self.synth(body_expr);
                        self.pop_binding();

                        body_expr.map(|(body_expr, body_type)| {
                            let let_expr = core::Term::Let(
                                *def_name,
                                self.arena.alloc_term(def_expr),
                                self.arena.alloc_term(body_expr),
                            );

                            (let_expr, body_type)
                        })
                    }
                    surface::Term::Universe(_) => {
                        Some((core::Term::Universe, Arc::new(Value::Universe)))
                    }
                    surface::Term::FunType(_, (_, input_name), input_type, output_type) => {
                        let input_type = self.check(input_type, &Arc::new(Value::Universe))?; // FIXME: avoid temporary Arc
                        let input_type_value = self.eval(&input_type)?;

                        self.push_param(*input_name, input_type_value);
                        let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        self.pop_binding();

                        output_type.map(|output_type| {
                            let fun_type = core::Term::FunType(
                                *input_name,
                                self.arena.alloc_term(input_type),
                                self.arena.alloc_term(output_type),
                            );

                            (fun_type, Arc::new(Value::Universe))
                        })
                    }
                    surface::Term::FunIntro(_, _, _) => {
                        self.push_message(
                            surface_term.range(),
                            "error: ambiguous function introduction",
                        );
                        None
                    }
                    surface::Term::FunElim(head_expr, input_expr) => {
                        let (head_expr, head_type) = self.synth(head_expr)?;
                        match head_type.as_ref() {
                            Value::FunType(_, input_type, output_type) => {
                                let input_expr = self.check(input_expr, input_type)?;
                                let input_expr_value = self.eval(&input_expr)?;

                                let output_type =
                                    self.apply_closure(output_type, input_expr_value)?;

                                let fun_elim = core::Term::FunElim(
                                    self.arena.alloc_term(head_expr),
                                    self.arena.alloc_term(input_expr),
                                );

                                Some((fun_elim, output_type))
                            }
                            _ => {
                                self.push_message(
                                    surface_term.range(),
                                    "error: argument to applied non-function",
                                );
                                None
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

            fn get_name(&self, local_var: core::LocalVar) -> Option<StringId> {
                self.names.get(local_var).copied()
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
                    core::Term::Let(def_name, def_expr, body_expr) => {
                        let (def_expr, def_type) = match self.synth(def_expr) {
                            surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                            expr => (self.arena.alloc_term(expr) as &_, None),
                        };

                        let def_name = self.push_binding(*def_name);
                        let body_expr = self.check(body_expr);
                        self.pop_binding();

                        surface::Term::Let(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, def_name),
                            def_type,
                            def_expr,
                            self.arena.alloc_term(body_expr),
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
                    core::Term::Var(local_var) => match self.get_name(*local_var) {
                        Some(name) => surface::Term::Var(PLACEHOLDER_RANGE, name),
                        None => todo!("misbound variable"), // TODO: error?
                    },
                    core::Term::Ann(expr, r#type) => {
                        let r#type = self.synth(r#type);
                        let expr = self.check(expr);

                        surface::Term::Ann(
                            self.arena.alloc_term(expr),
                            self.arena.alloc_term(r#type),
                        )
                    }
                    core::Term::Let(def_name, def_expr, body_expr) => {
                        let (def_expr, def_type) = match self.synth(def_expr) {
                            surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                            expr => (self.arena.alloc_term(expr) as &_, None),
                        };

                        let def_name = self.push_binding(*def_name);
                        let body_expr = self.synth(body_expr);
                        self.pop_binding();

                        surface::Term::Let(
                            PLACEHOLDER_POS,
                            (PLACEHOLDER_RANGE, def_name),
                            def_type,
                            def_expr,
                            self.arena.alloc_term(body_expr),
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
                }
            }
        }
    }
}
