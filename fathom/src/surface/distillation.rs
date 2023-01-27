//! Bidirectional distillation of the core language into the surface language.

use std::cell::RefCell;

use scoped_arena::Scope;

use crate::alloc::SliceVec;
use crate::core;
use crate::core::{Const, Plicity, UIntStyle};
use crate::env::{self, EnvLen, Index, Level, UniqueEnv};
use crate::source::{Span, StringId, StringInterner};
use crate::surface::elaboration::MetaSource;
use crate::surface::{
    Arg, BinOp, ExprField, FormatField, Item, ItemDef, Module, Param, Pattern, Term, TypeField,
};

/// Term precedences
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Top = 0,
    Let,
    Fun,
    Eq,
    Cmp,
    Mul,
    Add,
    App,
    Proj,
    Atomic,
}

/// Distillation context.
pub struct Context<'interner, 'arena, 'env> {
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Item name environment.
    item_names: &'env UniqueEnv<StringId>,
    /// Local name environment.
    local_names: &'env mut UniqueEnv<Option<StringId>>,
    /// Metavariable sources.
    meta_sources: &'env UniqueEnv<MetaSource>,
}

impl<'interner, 'arena, 'env> Context<'interner, 'arena, 'env> {
    /// Construct a new distillation context.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        item_names: &'env UniqueEnv<StringId>,
        local_names: &'env mut UniqueEnv<Option<StringId>>,
        meta_sources: &'env UniqueEnv<MetaSource>,
    ) -> Context<'interner, 'arena, 'env> {
        Context {
            interner,
            scope,
            item_names,
            local_names,
            meta_sources,
        }
    }

    fn is_bound(&self, name: StringId) -> bool {
        (self.local_names.iter()).any(|local_name| *local_name == Some(name))
            || self.item_names.iter().any(|item_name| *item_name == name)
    }

    fn local_len(&mut self) -> EnvLen {
        self.local_names.len()
    }

    fn get_item_name(&self, var: Level) -> Option<StringId> {
        self.item_names.get_level(var).copied()
    }

    fn get_local_name(&self, var: Index) -> Option<StringId> {
        self.local_names.get_index(var).copied().flatten()
    }

    fn push_local(&mut self, name: Option<StringId>) -> Option<StringId> {
        // TODO: avoid globals
        // TODO: ensure we chose a correctly bound name
        self.local_names.push(name);
        name
    }

    fn pop_local(&mut self) {
        self.local_names.pop();
    }

    fn truncate_local(&mut self, len: EnvLen) {
        self.local_names.truncate(len);
    }

    fn get_hole_name(&self, var: Level) -> Option<StringId> {
        match self.meta_sources.get_level(var)? {
            MetaSource::HoleExpr(_, name) => Some(*name),
            _ => Some(self.interner.borrow_mut().get_or_intern(var.to_string())),
        }
    }

    /// Generate a fresh name that is not currently bound in the context
    fn gen_fresh_name(&mut self) -> StringId {
        let mut counter = 0;
        loop {
            let name = self.interner.borrow_mut().get_alphabetic_name(counter);
            match self.is_bound(name) {
                true => counter += 1,
                false => return name,
            }
        }
    }

    /// Replace `name` with a fresh name if it is `_` and occurs in `body`
    fn freshen_name(&mut self, name: Option<StringId>, body: &core::Term<'_>) -> Option<StringId> {
        match name {
            Some(name) => Some(name),
            None => body
                .binds_local(Index::last())
                .then(|| self.gen_fresh_name()),
        }
    }

    fn check_number_literal_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
    ) -> Term<'arena, ()> {
        let string = style.format(number);
        let number = self.interner.borrow_mut().get_or_intern(string);
        Term::NumberLiteral((), number)
    }

    pub fn distill_module(mut self, core_module: &core::Module<'_>) -> Module<'arena, ()> {
        let scope = self.scope;

        let items = core_module.items.iter().map(|item| match item {
            core::Item::Def {
                label,
                r#type,
                expr,
            } => {
                let r#type = scope.to_scope(self.check_prec(Prec::Top, r#type));
                let expr = scope.to_scope(self.check_prec(Prec::Let, expr));

                Item::Def(ItemDef {
                    range: (),
                    label: ((), *label),
                    params: &[],
                    r#type: Some(r#type),
                    expr,
                })
            }
        });

        Module {
            items: scope.to_scope_from_iter(items),
        }
    }

    fn check_number_literal<T: std::fmt::Display>(&mut self, number: T) -> Term<'arena, ()> {
        let number = self.interner.borrow_mut().get_or_intern(number.to_string());
        Term::NumberLiteral((), number)
    }

    fn check_boolean_pattern(&mut self, boolean: bool) -> Pattern<()> {
        let name = match boolean {
            true => self.interner.borrow_mut().get_or_intern("true"),
            false => self.interner.borrow_mut().get_or_intern("false"),
        };
        Pattern::Name((), name)
    }

    fn check_number_pattern<T: std::fmt::Display>(&mut self, number: T) -> Pattern<()> {
        let number = self.interner.borrow_mut().get_or_intern(number.to_string());
        Pattern::NumberLiteral((), number)
    }

    fn check_number_pattern_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
    ) -> Pattern<()> {
        // TODO: Share with check_number_literal_styled
        let string = style.format(number);
        let number = self.interner.borrow_mut().get_or_intern(string);
        Pattern::NumberLiteral((), number)
    }

    fn check_constant_pattern(&mut self, r#const: &Const) -> Pattern<()> {
        match r#const {
            Const::Bool(boolean) => self.check_boolean_pattern(*boolean),
            Const::U8(number, style) => self.check_number_pattern_styled(number, *style),
            Const::U16(number, style) => self.check_number_pattern_styled(number, *style),
            Const::U32(number, style) => self.check_number_pattern_styled(number, *style),
            Const::U64(number, style) => self.check_number_pattern_styled(number, *style),
            Const::S8(number) => self.check_number_pattern(number),
            Const::S16(number) => self.check_number_pattern(number),
            Const::S32(number) => self.check_number_pattern(number),
            Const::S64(number) => self.check_number_pattern(number),
            Const::F32(number) => self.check_number_pattern(number),
            Const::F64(number) => self.check_number_pattern(number),
            Const::Pos(number) => self.check_number_pattern(number),
            Const::Ref(number) => self.check_number_pattern(number),
        }
    }

    fn synth_prim(&mut self, prim: core::Prim) -> Term<'arena, ()> {
        // FIXME: Check if shadowed
        let name = self.interner.borrow_mut().get_or_intern(prim.name());
        Term::Name((), name)
    }

    fn synth_number_literal<T: std::fmt::Display>(
        &mut self,
        prec: Prec,
        number: T,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal(number);
        let r#type = self.synth_prim(prim_type);

        self.paren(
            prec > Prec::Top,
            Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type)),
        )
    }

    fn synth_number_literal_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        prec: Prec,
        number: T,
        style: UIntStyle,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal_styled(number, style);
        let r#type = self.synth_prim(prim_type);

        self.paren(
            prec > Prec::Top,
            Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type)),
        )
    }

    fn check_dependent_tuple(
        &mut self,
        labels: &[StringId],
        exprs: &[core::Term<'_>],
    ) -> Term<'arena, ()> {
        self.local_names.reserve(labels.len());
        let initial_local_len = self.local_len();
        let exprs = (self.scope).to_scope_from_iter(
            Iterator::zip(labels.iter(), exprs.iter()).map(|(label, expr)| {
                let expr = self.check_prec(Prec::Top, expr);
                self.push_local(Some(*label));
                expr
            }),
        );
        self.truncate_local(initial_local_len);
        Term::Tuple((), exprs)
    }

    /// Wrap a term in parens.
    fn paren(&self, wrap: bool, term: Term<'arena, ()>) -> Term<'arena, ()> {
        if wrap {
            Term::Paren((), self.scope.to_scope(term))
        } else {
            term
        }
    }

    /// Distill a core term into a surface term, in a 'checkable' context.
    pub fn check(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        self.check_prec(Prec::Top, core_term)
    }

    fn check_prec(&mut self, prec: Prec, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        match core_term {
            core::Term::Ann(_span, expr, _) => {
                // Avoid adding extraneous type annotations!
                self.check_prec(prec, expr)
            }
            core::Term::Let(_span, def_name, def_type, def_expr, body_expr) => {
                let def_type = self.check_prec(Prec::Top, def_type);
                let def_expr = self.check_prec(Prec::Let, def_expr);

                let def_name = self.freshen_name(*def_name, body_expr);
                let def_name = self.push_local(def_name);
                let def_pattern = name_to_pattern(def_name);
                let body_expr = self.check_prec(Prec::Let, body_expr);
                self.pop_local();

                self.paren(
                    prec > Prec::Let,
                    Term::Let(
                        (),
                        def_pattern,
                        Some(self.scope.to_scope(def_type)),
                        self.scope.to_scope(def_expr),
                        self.scope.to_scope(body_expr),
                    ),
                )
            }
            core::Term::FunLit(..) => {
                let initial_local_len = self.local_len();
                let mut params = Vec::new();
                let mut body_expr = core_term;
                while let core::Term::FunLit(_, plicity, param_name, next_body_expr) = body_expr {
                    let param_name = self.freshen_name(*param_name, next_body_expr);
                    params.push((*plicity, self.push_local(param_name)));
                    body_expr = next_body_expr;
                }

                let body_expr = self.check_prec(Prec::Let, body_expr);
                self.truncate_local(initial_local_len);

                let params = params.into_iter().map(|(plicity, name)| Param {
                    plicity,
                    pattern: name_to_pattern(name),
                    r#type: None,
                });

                self.paren(
                    prec > Prec::Fun,
                    Term::FunLiteral(
                        (),
                        self.scope.to_scope_from_iter(params),
                        self.scope.to_scope(body_expr),
                    ),
                )
            }
            core::Term::RecordType(_, labels, types)
                if is_tuple_type(&mut self.interner.borrow_mut(), labels, types) =>
            {
                self.check_dependent_tuple(labels, types)
            }
            core::Term::FormatRecord(_, labels, formats)
                if is_tuple_type(&mut self.interner.borrow_mut(), labels, formats) =>
            {
                self.check_dependent_tuple(labels, formats)
            }
            core::Term::RecordLit(_, labels, exprs)
                if self.interner.borrow_mut().is_tuple_labels(labels) =>
            {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.check_prec(Prec::Top, expr));
                Term::Tuple((), scope.to_scope_from_iter(exprs))
            }
            core::Term::RecordLit(_, labels, exprs) => {
                let scope = self.scope;
                let expr_fields =
                    Iterator::zip(labels.iter(), exprs.iter()).map(|(label, expr)| ExprField {
                        label: ((), *label),
                        expr: self.check_prec(Prec::Top, expr),
                    });

                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::ArrayLit(_span, elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs
                    .iter()
                    .map(|elem_exprs| self.check_prec(Prec::Top, elem_exprs));

                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }
            core::Term::ConstLit(_span, r#const) => match r#const {
                core::Const::Bool(boolean) => Term::BooleanLiteral((), *boolean),
                core::Const::U8(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U16(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U32(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U64(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::S8(number) => self.check_number_literal(number),
                core::Const::S16(number) => self.check_number_literal(number),
                core::Const::S32(number) => self.check_number_literal(number),
                core::Const::S64(number) => self.check_number_literal(number),
                core::Const::F32(number) => self.check_number_literal(number),
                core::Const::F64(number) => self.check_number_literal(number),
                core::Const::Pos(number) => self.check_number_literal(number),
                core::Const::Ref(number) => self.check_number_literal(number),
            },
            core::Term::ConstMatch(_span, head_expr, branches, default_branch) => {
                if let Some((then_expr, else_expr)) = match_if_then_else(branches, *default_branch)
                {
                    let cond_expr = self.check_prec(Prec::Fun, head_expr);
                    let then_expr = self.check_prec(Prec::Let, then_expr);
                    let else_expr = self.check_prec(Prec::Let, else_expr);
                    return self.paren(
                        prec > Prec::Let,
                        Term::If(
                            (),
                            self.scope.to_scope(cond_expr),
                            self.scope.to_scope(then_expr),
                            self.scope.to_scope(else_expr),
                        ),
                    );
                }

                let head_expr = self.synth_prec(Prec::Proj, head_expr);
                match default_branch {
                    Some((default_name, default_expr)) => {
                        let default_branch = {
                            let name = self.freshen_name(*default_name, default_expr);
                            let name = self.push_local(name);
                            let default_expr = self.check_prec(Prec::Top, default_expr);
                            self.pop_local();

                            (name_to_pattern(name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, body_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let body_expr = self.check_prec(Prec::Top, body_expr);
                                        (pattern, body_expr)
                                    })
                                    .chain(std::iter::once(default_branch)),
                            ),
                        )
                    }
                    None => Term::Match(
                        (),
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope_from_iter(branches.iter().map(
                            |(r#const, body_expr)| {
                                let pattern = self.check_constant_pattern(r#const);
                                let body_expr = self.check_prec(Prec::Top, body_expr);
                                (pattern, body_expr)
                            },
                        )),
                    ),
                }
            }

            _ => self.synth_prec(prec, core_term),
        }
    }

    /// Distill a core term into a surface term, in a 'synthesizable' context.
    pub fn synth(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        self.synth_prec(Prec::Top, core_term)
    }

    fn synth_prec(&mut self, prec: Prec, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        match core_term {
            core::Term::ItemVar(_span, var) => match self.get_item_name(*var) {
                Some(name) => Term::Name((), name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::LocalVar(_span, var) => match self.get_local_name(*var) {
                Some(name) => Term::Name((), name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::MetaVar(_span, var) => match self.get_hole_name(*var) {
                Some(name) => Term::Hole((), name),
                None => Term::Placeholder(()),
            },
            core::Term::InsertedMeta(span, var, local_infos) => {
                let head_expr = self.synth_prec(Prec::Top, &core::Term::MetaVar(*span, *var));
                let num_params = local_infos
                    .iter()
                    .filter(|info| matches!(info, core::LocalInfo::Param))
                    .count();

                if num_params == 0 {
                    head_expr
                } else {
                    let head_expr = self.scope.to_scope(head_expr);
                    let mut args = SliceVec::new(self.scope, num_params);

                    for (var, info) in Iterator::zip(env::levels(), local_infos.iter()) {
                        match info {
                            core::LocalInfo::Def => {}
                            core::LocalInfo::Param => {
                                let var = self.local_len().level_to_index(var).unwrap();
                                args.push(Arg {
                                    plicity: Plicity::Explicit,
                                    term: self.check_prec(
                                        Prec::Top,
                                        &core::Term::LocalVar(Span::Empty, var),
                                    ),
                                });
                            }
                        }
                    }

                    self.paren(prec > Prec::App, Term::App((), head_expr, args.into()))
                }
            }
            core::Term::Ann(_span, expr, r#type) => {
                let expr = self.check_prec(Prec::Let, expr);
                let r#type = self.check_prec(Prec::Top, r#type);

                self.paren(
                    prec > Prec::Top,
                    Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type)),
                )
            }
            core::Term::Let(_span, def_name, def_type, def_expr, body_expr) => {
                let def_type = self.check_prec(Prec::Top, def_type);
                let def_expr = self.check_prec(Prec::Let, def_expr);

                let def_name = self.freshen_name(*def_name, body_expr);
                let def_name = self.push_local(def_name);
                let body_expr = self.synth_prec(Prec::Let, body_expr);
                self.pop_local();

                self.paren(
                    prec > Prec::Let,
                    Term::Let(
                        (),
                        name_to_pattern(def_name),
                        Some(self.scope.to_scope(def_type)),
                        self.scope.to_scope(def_expr),
                        self.scope.to_scope(body_expr),
                    ),
                )
            }
            core::Term::Universe(_span) => Term::Universe(()),

            core::Term::FunType(..) => {
                let initial_local_len = self.local_len();

                let mut params = Vec::new();
                let mut body_type = core_term;

                let body_type = loop {
                    match body_type {
                        // Use an explicit parameter if it is referenced in the body
                        core::Term::FunType(_, plicity, param_name, param_type, next_body_type)
                            if next_body_type.binds_local(Index::last()) =>
                        {
                            let param_type = self.check_prec(Prec::Top, param_type);
                            let param_name = self.freshen_name(*param_name, next_body_type);
                            let param_name = self.push_local(param_name);
                            params.push(Param {
                                plicity: *plicity,
                                pattern: name_to_pattern(param_name),
                                r#type: Some(param_type),
                            });
                            body_type = next_body_type;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        core::Term::FunType(_, plicity, _, param_type, body_type) => {
                            let param_type = self.check_prec(Prec::App, param_type);

                            self.push_local(None);
                            let body_type = self.check_prec(Prec::Fun, body_type);
                            self.pop_local();

                            break Term::Arrow(
                                (),
                                *plicity,
                                self.scope.to_scope(param_type),
                                self.scope.to_scope(body_type),
                            );
                        }
                        body_type => break self.check_prec(Prec::Fun, body_type),
                    }
                };

                self.truncate_local(initial_local_len);

                self.paren(
                    prec > Prec::Fun,
                    if params.is_empty() {
                        body_type
                    } else {
                        Term::FunType(
                            (),
                            self.scope.to_scope_from_iter(params),
                            self.scope.to_scope(body_type),
                        )
                    },
                )
            }
            core::Term::FunLit(..) => {
                let initial_local_len = self.local_len();
                let mut params = Vec::new();
                let mut body_expr = core_term;

                while let core::Term::FunLit(_, plicity, param_name, next_body_expr) = body_expr {
                    let param_name = self.freshen_name(*param_name, next_body_expr);
                    params.push((*plicity, self.push_local(param_name)));
                    body_expr = next_body_expr;
                }

                let body_expr = self.synth_prec(Prec::Let, body_expr);
                self.truncate_local(initial_local_len);

                let params = params.into_iter().map(|(plicity, name)| Param {
                    plicity,
                    pattern: name_to_pattern(name),
                    r#type: None,
                });

                self.paren(
                    prec > Prec::Fun,
                    Term::FunLiteral(
                        (),
                        self.scope.to_scope_from_iter(params),
                        self.scope.to_scope(body_expr),
                    ),
                )
            }
            core::Term::FunApp(..) => {
                let mut head_expr = core_term;
                let mut args = Vec::new();

                // Collect a spine of arguments in reverse order
                while let core::Term::FunApp(_, plicity, next_head_expr, arg_expr) = *head_expr {
                    head_expr = next_head_expr;
                    args.push((plicity, arg_expr));
                }

                // Distill appropriate primitives to binary operator expressions
                if let (core::Term::Prim(_, prim), [(_, rhs), (_, lhs)]) = (head_expr, &args[..]) {
                    if let Some(op) = prim_to_bin_op(prim) {
                        let lhs = (self.scope).to_scope(self.synth_prec(op.lhs_prec(), lhs));
                        let rhs = (self.scope).to_scope(self.synth_prec(op.rhs_prec(), rhs));
                        return self.paren(prec > op.precedence(), Term::BinOp((), lhs, op, rhs));
                    }
                }

                let head_expr = self.scope.to_scope(self.synth_prec(Prec::Proj, head_expr));
                let args = self.scope.to_scope_from_iter(args.into_iter().rev().map(
                    |(plicity, arg_expr)| Arg {
                        plicity,
                        term: self.check_prec(Prec::Proj, arg_expr),
                    },
                ));

                // Otherwise distill to a function application
                self.paren(prec > Prec::App, Term::App((), head_expr, args))
            }

            core::Term::RecordType(_, labels, types)
                if is_tuple_type(&mut self.interner.borrow_mut(), labels, types) =>
            {
                let tuple = self.check_dependent_tuple(labels, types);
                Term::Ann((), self.scope.to_scope(tuple), &Term::Universe(()))
            }
            core::Term::FormatRecord(_span, labels, formats)
                if is_tuple_type(&mut self.interner.borrow_mut(), labels, formats) =>
            {
                self.check_dependent_tuple(labels, formats)
            }
            core::Term::RecordType(_span, labels, types) => {
                self.local_names.reserve(labels.len());
                let initial_local_len = self.local_len();
                let type_fields = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), types.iter()).map(|(label, r#type)| {
                        let r#type = self.check_prec(Prec::Top, r#type);
                        self.push_local(Some(*label));
                        TypeField {
                            label: ((), *label), // TODO: range from span
                            r#type,
                        }
                    }),
                );
                self.truncate_local(initial_local_len);

                Term::RecordType((), type_fields)
            }
            core::Term::RecordLit(_, labels, exprs)
                if self.interner.borrow_mut().is_tuple_labels(labels) =>
            {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.synth_prec(Prec::Top, expr));
                Term::Tuple((), scope.to_scope_from_iter(exprs))
            }
            core::Term::RecordLit(_span, labels, exprs) => {
                let scope = self.scope;
                let expr_fields =
                    Iterator::zip(labels.iter(), exprs.iter()).map(|(label, expr)| ExprField {
                        label: ((), *label),
                        expr: self.synth_prec(Prec::Top, expr),
                    });

                // TODO: type annotations?
                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::RecordProj(_, mut head_expr, label) => {
                let mut labels = vec![((), *label)];

                while let core::Term::RecordProj(_, next_head_expr, label) = head_expr {
                    head_expr = next_head_expr;
                    labels.push(((), *label));
                }

                let head_expr = self.synth_prec(Prec::Atomic, head_expr);
                Term::Proj(
                    (),
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope_from_iter(labels.into_iter().rev()),
                )
            }
            core::Term::ArrayLit(_span, elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs
                    .iter()
                    .map(|elem_exprs| self.check_prec(Prec::Top, elem_exprs));

                // FIXME: Type annotations
                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }

            core::Term::FormatRecord(_span, labels, formats) => {
                Term::FormatRecord((), self.synth_format_fields(labels, formats))
            }
            core::Term::FormatCond(_span, label, format, cond) => {
                let format = self.check_prec(Prec::Top, format);
                self.push_local(Some(*label));
                let cond = self.check_prec(Prec::Top, cond);
                self.pop_local();
                Term::FormatCond(
                    (),
                    ((), *label),
                    self.scope.to_scope(format),
                    self.scope.to_scope(cond),
                )
            }
            core::Term::FormatOverlap(_span, labels, formats) => {
                Term::FormatOverlap((), self.synth_format_fields(labels, formats))
            }
            core::Term::Prim(_span, prim) => self.synth_prim(*prim),
            core::Term::ConstLit(_span, r#const) => match r#const {
                core::Const::Bool(boolean) => Term::BooleanLiteral((), *boolean),
                core::Const::U8(number, style) => self.synth_number_literal_styled(
                    prec,
                    number,
                    *style,
                    core::UintType::U8.into(),
                ),
                core::Const::U16(number, style) => self.synth_number_literal_styled(
                    prec,
                    number,
                    *style,
                    core::UintType::U16.into(),
                ),
                core::Const::U32(number, style) => self.synth_number_literal_styled(
                    prec,
                    number,
                    *style,
                    core::UintType::U32.into(),
                ),
                core::Const::U64(number, style) => self.synth_number_literal_styled(
                    prec,
                    number,
                    *style,
                    core::UintType::U64.into(),
                ),
                core::Const::S8(number) => {
                    self.synth_number_literal(prec, number, core::SintType::S8.into())
                }
                core::Const::S16(number) => {
                    self.synth_number_literal(prec, number, core::SintType::S16.into())
                }
                core::Const::S32(number) => {
                    self.synth_number_literal(prec, number, core::SintType::S32.into())
                }
                core::Const::S64(number) => {
                    self.synth_number_literal(prec, number, core::SintType::S64.into())
                }
                core::Const::F32(number) => {
                    self.synth_number_literal(prec, number, core::FloatType::F32.into())
                }
                core::Const::F64(number) => {
                    self.synth_number_literal(prec, number, core::FloatType::F64.into())
                }
                core::Const::Pos(number) => {
                    self.synth_number_literal(prec, number, core::Prim::PosType)
                }
                core::Const::Ref(number) => {
                    self.synth_number_literal(prec, number, core::Prim::RefType)
                }
            },
            core::Term::ConstMatch(_span, head_expr, branches, default_expr) => {
                if let Some((then_expr, else_expr)) = match_if_then_else(branches, *default_expr) {
                    let cond_expr = self.check_prec(Prec::Fun, head_expr);
                    let then_expr = self.synth_prec(Prec::Let, then_expr);
                    let else_expr = self.synth_prec(Prec::Let, else_expr);
                    return Term::If(
                        (),
                        self.scope.to_scope(cond_expr),
                        self.scope.to_scope(then_expr),
                        self.scope.to_scope(else_expr),
                    );
                }

                let head_expr = self.synth_prec(Prec::Proj, head_expr);
                match default_expr {
                    Some((default_name, default_expr)) => {
                        let default_branch = {
                            let name = self.freshen_name(*default_name, default_expr);
                            let name = self.push_local(name);
                            let default_expr = self.synth_prec(Prec::Top, default_expr);
                            self.pop_local();

                            (name_to_pattern(name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, body_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let body_expr = self.synth_prec(Prec::Top, body_expr);
                                        (pattern, body_expr)
                                    })
                                    .chain(std::iter::once(default_branch)),
                            ),
                        )
                    }
                    None => Term::Match(
                        (),
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope_from_iter(branches.iter().map(
                            |(r#const, body_expr)| {
                                let pattern = self.check_constant_pattern(r#const);
                                let body_expr = self.synth_prec(Prec::Top, body_expr);
                                (pattern, body_expr)
                            },
                        )),
                    ),
                }
            }
        }
    }

    fn synth_format_fields(
        &mut self,
        labels: &[StringId],
        core_formats: &[core::Term<'_>],
    ) -> &'arena [FormatField<'arena, ()>] {
        use crate::core::Prim::FormatSucceed;

        self.local_names.reserve(labels.len());
        let initial_local_len = self.local_len();
        let core_fields = Iterator::zip(labels.iter().copied(), core_formats.iter());
        let format_fields =
            (self.scope).to_scope_from_iter(core_fields.map(|(label, format)| match format {
                // Distill succeed formats back to computed formats
                core::Term::FunApp(
                    ..,
                    core::Term::FunApp(.., core::Term::Prim(_prim_span, FormatSucceed), r#type),
                    expr,
                ) => {
                    let r#type = self.check_prec(Prec::Top, r#type);
                    let expr = self.check_prec(Prec::Top, expr);
                    self.push_local(Some(label));

                    FormatField::Computed {
                        label: ((), label),
                        r#type: Some(r#type),
                        expr,
                    }
                }
                // Use field refinements when `format` is a conditional format
                // that binds the same name as the current field label.
                core::Term::FormatCond(_span, name, format, pred) if label == *name => {
                    let format = self.check_prec(Prec::Top, format);
                    self.push_local(Some(label));
                    let pred = self.check_prec(Prec::Top, pred);

                    FormatField::Format {
                        label: ((), label),
                        format,
                        pred: Some(pred),
                    }
                }
                // Otherwise stick with a regular format field...
                format => {
                    let format = self.check_prec(Prec::Top, format);
                    self.push_local(Some(label));

                    FormatField::Format {
                        label: ((), label),
                        format,
                        pred: None,
                    }
                }
            }));
        self.truncate_local(initial_local_len);

        format_fields
    }
}

fn name_to_pattern(name: Option<StringId>) -> Pattern<()> {
    match name {
        Some(name) => Pattern::Name((), name),
        None => Pattern::Placeholder(()),
    }
}

fn match_if_then_else<'arena>(
    branches: &'arena [(Const, core::Term<'arena>)],
    default_branch: Option<(Option<StringId>, &'arena core::Term<'arena>)>,
) -> Option<(&'arena core::Term<'arena>, &'arena core::Term<'arena>)> {
    match (branches, default_branch) {
        ([(Const::Bool(false), else_expr), (Const::Bool(true), then_expr)], None)
        // TODO: Normalize boolean branches when elaborating patterns
        | ([(Const::Bool(true), then_expr)], Some((_, else_expr)))
        | ([(Const::Bool(false), else_expr)], Some((_, then_expr))) => Some((then_expr, else_expr)),
        _ => None,
    }
}

/// Returns true if `labels` is a sequence of tuple labels (`_0`, `_1`, ...),
/// and a telescope of `types` contains independent entries.
fn is_tuple_type(
    interner: &mut StringInterner,
    labels: &[StringId],
    types: &[core::Term<'_>],
) -> bool {
    interner.is_tuple_labels(labels)
        // For each type in the telescope, ensure that the subsequent types in
        // the telescope do not depend on the current field.
        && (1..=types.len()).all(|index| {
            Iterator::zip(types[index..].iter(), env::indices())
                .all(|(expr, var)| !expr.binds_local(var))
        })
}

fn prim_to_bin_op(prim: &core::Prim) -> Option<BinOp<()>> {
    use crate::core::Prim::*;

    match prim {
        PosAdd(_) | IntAdd(_) => Some(BinOp::Add(())),
        IntSub(_) => Some(BinOp::Sub(())),
        IntMul(_) => Some(BinOp::Mul(())),
        IntDiv(_) => Some(BinOp::Div(())),

        BoolEq | IntEq(_) => Some(BinOp::Eq(())),
        BoolNeq | IntNeq(_) => Some(BinOp::Neq(())),

        IntLt(_) => Some(BinOp::Lt(())),
        IntLte(_) => Some(BinOp::Lte(())),
        IntGt(_) => Some(BinOp::Gt(())),
        IntGte(_) => Some(BinOp::Gte(())),

        _ => None,
    }
}

impl<Range> BinOp<Range> {
    fn precedence(&self) -> Prec {
        self.precedence_impl().1
    }

    fn lhs_prec(&self) -> Prec {
        self.precedence_impl().0
    }

    fn rhs_prec(&self) -> Prec {
        self.precedence_impl().2
    }

    /// Returns the precedence of this operator and its operands
    ///
    /// (lhs, op, rhs)
    fn precedence_impl(&self) -> (Prec, Prec, Prec) {
        match self {
            BinOp::Eq(_) | BinOp::Neq(_) => (Prec::Cmp, Prec::Eq, Prec::Eq),
            BinOp::Lt(_) | BinOp::Lte(_) | BinOp::Gt(_) | BinOp::Gte(_) => {
                (Prec::Add, Prec::Cmp, Prec::Cmp)
            }
            BinOp::Add(_) | BinOp::Sub(_) => (Prec::Mul, Prec::Add, Prec::Add),
            BinOp::Mul(_) | BinOp::Div(_) => (Prec::App, Prec::Mul, Prec::Mul),
        }
    }
}
