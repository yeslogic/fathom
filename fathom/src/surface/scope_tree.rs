//! A ScopeTree is a data structure that mirrors `surface::Term`, but with only
//! information relevant to name resolution. The intention is that all
//! name/scope handing can be performed in a single pass, rather than having to
//! be performed during elaboration, evaluation etc.
//!
//! The ScopeTree data structure conceptually is a tree, just like
//! `surface::Term`. A `Scope` node represents a subexpression with 0 or more
//! new bindings visible, and inherits the bindings from the its ancestors. Each
//! `surface::Term` has an associated `Scope` node.
//!
//! Name resolution is performed by looking up a `surface::Term::Name`'s
//! corresponding `Scope` node, and then querying the Scope's names. If the name
//! is not found, query the Scope's ancestors upto the root Scope.
//!
//! The lookup algorithm requires being able to go from a Scope node to it's
//! parent, so the the ScopeTree data structure is structured opposite to how a
//! tree data strcuture is normally represented in Rust: each node stores its
//! parent (if any), rather than its children.
//!
//! # Bibliography
//! Shamelessly stolen from `rust-analyzer`: https://github.com/rust-lang/rust-analyzer/blob/c468e39b5a371f1a85dfe5ebece82caad0796c0a/crates/hir-def/src/body/scope.rs#L1

use fxhash::FxHashMap;
use index_vec::IndexVec;

use crate::by_addr::ByAddr;
use crate::env::UniqueEnv;
use crate::source::ByteRange;
use crate::surface::{FormatField, Pattern, Term};
use crate::symbol::Symbol;

type TermAddr<'a> = ByAddr<'a, Term<'a, ByteRange>>;

index_vec::define_index_type! {
    pub struct ScopeId = u32;
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    parent: Option<ScopeId>,
    names: UniqueEnv<Option<Symbol>>,
    infos: UniqueEnv<LocalInfo>,
}

#[derive(Debug, Clone)]
pub enum LocalInfo {
    Def,
    Param,
}

#[derive(Debug, Clone)]
pub struct ScopeTree<'surface> {
    scopes: IndexVec<ScopeId, Scope>,
    scope_of_term: FxHashMap<TermAddr<'surface>, ScopeId>,
}

pub struct ScopeTreeBuilder<'surface> {
    scopes: IndexVec<ScopeId, Scope>,
    current_scope: ScopeId,
    scope_of_term: FxHashMap<TermAddr<'surface>, ScopeId>,
}

impl<'surface> ScopeTreeBuilder<'surface> {
    pub fn new() -> Self {
        let mut scopes = IndexVec::with_capacity(1);
        let current_scope = scopes.push(Scope::default());

        Self {
            scopes,
            current_scope,
            scope_of_term: FxHashMap::default(),
        }
    }

    pub fn finish(self) -> ScopeTree<'surface> {
        ScopeTree {
            scopes: self.scopes,
            scope_of_term: self.scope_of_term,
        }
    }

    fn push_scope(&mut self) {
        let parent_scope = self.current_scope;
        self.current_scope = self.scopes.push(Scope {
            parent: Some(parent_scope),
            ..Scope::default()
        });
    }

    fn with_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        let old_scope = self.current_scope;
        let ret = f(self);
        self.current_scope = old_scope;
        ret
    }

    fn in_child_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.with_scope(|this| {
            this.push_scope();
            f(this)
        })
    }

    fn push_local(&mut self, name: Option<Symbol>, info: LocalInfo) {
        self.scopes[self.current_scope].names.push(name);
        self.scopes[self.current_scope].infos.push(info);
    }

    fn push_def(&mut self, pat: &'surface Pattern<ByteRange>) {
        match pat {
            Pattern::Name(_, name) => self.push_local(Some(*name), LocalInfo::Def),
            Pattern::Placeholder(_)
            | Pattern::StringLiteral(_, _)
            | Pattern::NumberLiteral(_, _)
            | Pattern::BooleanLiteral(_, _) => self.push_local(None, LocalInfo::Def),
        }
    }

    fn push_param(&mut self, pat: &'surface Pattern<ByteRange>) {
        match pat {
            Pattern::Name(_, name) => self.push_local(Some(*name), LocalInfo::Param),
            Pattern::Placeholder(_)
            | Pattern::StringLiteral(_, _)
            | Pattern::NumberLiteral(_, _)
            | Pattern::BooleanLiteral(_, _) => self.push_local(None, LocalInfo::Param),
        }
    }

    pub fn term(&mut self, term: &'surface Term<'surface, ByteRange>) {
        self.scope_of_term.insert(ByAddr(term), self.current_scope);

        match term {
            Term::Let(_, pat, r#type, expr, body) => {
                if let Some(r#type) = r#type {
                    self.term(r#type);
                }
                self.term(expr);
                self.in_child_scope(|this| {
                    this.push_def(pat);
                    this.term(body);
                });
            }
            Term::Match(_, scrut, branches) => {
                self.term(scrut);
                for (pat, term) in branches.iter() {
                    self.in_child_scope(|this| {
                        this.push_def(pat);
                        this.term(term);
                    });
                }
            }
            Term::Arrow(_, _, r#type, body) => {
                self.term(r#type);
                self.in_child_scope(|this| {
                    this.push_local(None, LocalInfo::Param);
                    this.term(body);
                });
            }
            Term::FunType(_, params, body) | Term::FunLiteral(_, params, body) => {
                self.with_scope(|this| {
                    for param in params.iter() {
                        if let Some(r#type) = param.r#type.as_ref() {
                            this.term(r#type);
                        }
                        this.push_scope();
                        this.push_param(&param.pattern);
                    }
                    this.term(body)
                })
            }
            Term::RecordType(_, fields) => self.with_scope(|this| {
                for field in fields.iter() {
                    this.term(&field.r#type);
                    this.push_scope();
                    this.push_local(Some(field.label.1), LocalInfo::Param);
                }
            }),
            Term::FormatRecord(_, fields) => self.format_fields(fields),
            Term::FormatOverlap(_, fields) => self.format_fields(fields),
            Term::FormatCond(_, (_, name), format, pred) => {
                self.term(format);
                self.in_child_scope(|this| {
                    this.push_local(Some(*name), LocalInfo::Param);
                    this.term(pred);
                })
            }

            _ => term.walk_child_terms(|term| self.term(term)),
        }
    }

    fn format_fields(&mut self, fields: &'surface [FormatField<'surface, ByteRange>]) {
        self.with_scope(|this| {
            for field in fields.iter() {
                match field {
                    FormatField::Format {
                        label: (_, name),
                        format,
                        pred,
                    } => {
                        this.term(format);
                        this.push_scope();
                        this.push_local(Some(*name), LocalInfo::Param);
                        if let Some(pred) = pred {
                            this.term(pred)
                        }
                    }
                    FormatField::Computed {
                        label: (_, name),
                        r#type,
                        expr,
                    } => {
                        if let Some(r#type) = r#type {
                            this.term(r#type)
                        }
                        this.term(expr);
                        this.push_scope();
                        this.push_local(Some(*name), LocalInfo::Param);
                    }
                }
            }
        })
    }
}
