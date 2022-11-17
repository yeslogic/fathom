//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;
use std::cell::RefCell;

use crate::alloc::SliceVec;
use crate::core;
use crate::core::{Const, UIntStyle};
use crate::env::{self, EnvLen, Index, Level, UniqueEnv};
use crate::source::{Span, StringId, StringInterner};
use crate::surface::elaboration::MetaSource;
use crate::surface::{
    BinOp, ExprField, FormatField, Item, ItemDef, Module, Pattern, Term, TypeField,
};

/// Distillation context.
pub struct Context<'interner, 'arena, 'env> {
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Item name environment.
    item_names: &'env mut UniqueEnv<StringId>,
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
        item_names: &'env mut UniqueEnv<StringId>,
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

    fn local_len(&mut self) -> EnvLen {
        self.local_names.len()
    }

    fn get_item_name(&self, var: Level) -> Option<StringId> {
        self.item_names.get_level(var).copied()
    }

    fn push_item(&mut self, name: StringId) {
        self.item_names.push(name);
    }

    fn get_local_name(&self, var: Index) -> Option<StringId> {
        self.local_names.get_index(var).copied().flatten()
    }

    fn push_local(&mut self, name: Option<StringId>) -> StringId {
        let name = name.unwrap_or_else(|| {
            self.interner.borrow_mut().get_or_intern_static("_") // TODO: choose a better name?
        });

        // TODO: avoid globals
        // TODO: ensure we chose a correctly bound name
        self.local_names.push(Some(name));
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
            _ => None,
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
                let r#type = scope.to_scope(self.synth(r#type));
                let expr = scope.to_scope(self.check(expr));
                self.push_item(*label);

                Item::Def(ItemDef {
                    range: (),
                    label: ((), *label),
                    patterns: &[],
                    type_: Some(r#type),
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

    fn check_constant_pattern(&mut self, r#const: &core::Const) -> Pattern<()> {
        match r#const {
            core::Const::Bool(boolean) => self.check_boolean_pattern(*boolean),
            core::Const::U8(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U16(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U32(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U64(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::S8(number) => self.check_number_pattern(number),
            core::Const::S16(number) => self.check_number_pattern(number),
            core::Const::S32(number) => self.check_number_pattern(number),
            core::Const::S64(number) => self.check_number_pattern(number),
            core::Const::F32(number) => self.check_number_pattern(number),
            core::Const::F64(number) => self.check_number_pattern(number),
            core::Const::Pos(number) => self.check_number_pattern(number),
            core::Const::Ref(number) => self.check_number_pattern(number),
        }
    }

    fn synth_prim(&mut self, prim: core::Prim) -> Term<'arena, ()> {
        // FIXME: Check if shadowed
        let name = self.interner.borrow_mut().get_or_intern_static(prim.name());
        Term::Name((), name)
    }

    fn synth_number_literal<T: std::fmt::Display>(
        &mut self,
        number: T,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal(number);
        let r#type = self.synth_prim(prim_type);

        Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type))
    }

    fn synth_number_literal_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal_styled(number, style);
        let r#type = self.synth_prim(prim_type);

        Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type))
    }

    /// Distill a core term into a surface term, in a 'checkable' context.
    pub fn check(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        match core_term {
            core::Term::Ann(_span, expr, _) => {
                // Avoid adding extraneous type annotations!
                self.check(expr)
            }
            core::Term::Let(_span, def_name, def_type, def_expr, body_expr) => {
                let def_type = self.check(def_type);
                let def_expr = self.check(def_expr);

                let def_name = self.push_local(*def_name);
                let body_expr = self.check(body_expr);
                self.pop_local();

                Term::Let(
                    (),
                    Pattern::Name((), def_name),
                    Some(self.scope.to_scope(def_type)),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                )
            }
            core::Term::FunLit(_, param_name, mut body_expr) => {
                let initial_local_len = self.local_len();
                let mut param_names = vec![self.push_local(*param_name)];
                while let core::Term::FunLit(_, param_name, next_body_expr) = body_expr {
                    param_names.push(self.push_local(*param_name));
                    body_expr = next_body_expr;
                }

                let body_expr = self.check(body_expr);
                self.truncate_local(initial_local_len);

                let patterns = param_names
                    .into_iter()
                    .map(|name| (Pattern::Name((), name), None));

                Term::FunLiteral(
                    (),
                    self.scope.to_scope_from_iter(patterns),
                    self.scope.to_scope(body_expr),
                )
            }
            core::Term::RecordType(_span, labels, exprs)
                if is_tuple_type(labels, exprs, &self.interner.borrow()) =>
            {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.check(expr));
                Term::Tuple((), scope.to_scope_from_iter(exprs))
            }
            core::Term::RecordLit(_span, labels, exprs)
                if is_tuple_expr(labels, &self.interner.borrow()) =>
            {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.check(expr));
                Term::Tuple((), scope.to_scope_from_iter(exprs))
            }
            core::Term::RecordLit(_span, labels, exprs) => {
                let scope = self.scope;
                let expr_fields =
                    Iterator::zip(labels.iter(), exprs.iter()).map(|(label, expr)| ExprField {
                        label: ((), *label),
                        expr: self.check(expr),
                    });

                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::ArrayLit(_span, elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs.iter().map(|elem_exprs| self.check(elem_exprs));

                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }
            core::Term::FormatRecord(_span, labels, formats)
                if is_tuple_type(labels, formats, &self.interner.borrow()) =>
            {
                let scope = self.scope;
                let formats = formats.iter().map(|format| self.check(format));
                Term::Tuple((), scope.to_scope_from_iter(formats))
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
            core::Term::ConstMatch(_span, head_expr, branches, default_expr) => {
                if let Some((then_expr, else_expr)) = match_if_then_else(branches, *default_expr) {
                    let cond_expr = self.check(head_expr);
                    let then_expr = self.check(then_expr);
                    let else_expr = self.check(else_expr);
                    return Term::If(
                        (),
                        self.scope.to_scope(cond_expr),
                        self.scope.to_scope(then_expr),
                        self.scope.to_scope(else_expr),
                    );
                }

                let head_expr = self.synth(head_expr);
                match default_expr {
                    Some(default_expr) => {
                        let default_branch = {
                            let name = self.push_local(None);
                            let default_expr = self.check(default_expr);
                            self.pop_local();

                            (Pattern::Name((), name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, body_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let body_expr = self.check(body_expr);
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
                                let body_expr = self.check(body_expr);
                                (pattern, body_expr)
                            },
                        )),
                    ),
                }
            }

            _ => self.synth(core_term),
        }
    }

    /// Distill a core term into a surface term, in a 'synthesizable' context.
    pub fn synth(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
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
                let head_expr = self.synth(&core::Term::MetaVar(*span, *var));
                let num_params = local_infos
                    .iter()
                    .filter(|info| matches!(info, core::LocalInfo::Param))
                    .count();

                if num_params == 0 {
                    head_expr
                } else {
                    let head_expr = self.scope.to_scope(head_expr);
                    let mut arg_exprs = SliceVec::new(self.scope, num_params);

                    for (var, info) in Iterator::zip(env::levels(), local_infos.iter()) {
                        match info {
                            core::LocalInfo::Def => {}
                            core::LocalInfo::Param => {
                                let var = self.local_len().level_to_index(var).unwrap();
                                arg_exprs.push(self.check(&core::Term::LocalVar(Span::Empty, var)));
                            }
                        }
                    }

                    Term::App((), head_expr, arg_exprs.into())
                }
            }
            core::Term::Ann(_span, expr, r#type) => {
                let r#type = self.check(r#type);
                let expr = self.check(expr);

                Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type))
            }
            core::Term::Let(_span, def_name, def_type, def_expr, body_expr) => {
                let def_type = self.check(def_type);
                let def_expr = self.check(def_expr);

                let def_name = self.push_local(*def_name);
                let body_expr = self.synth(body_expr);
                self.pop_local();

                Term::Let(
                    (),
                    Pattern::Name((), def_name),
                    Some(self.scope.to_scope(def_type)),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                )
            }
            core::Term::Universe(_span) => Term::Universe(()),

            core::Term::FunType(..) => {
                let initial_local_len = self.local_len();

                let mut patterns = Vec::new();
                let mut body_type = core_term;

                let body_type = loop {
                    match body_type {
                        // Use an explicit parameter if it is referenced in the body
                        core::Term::FunType(_, param_name, param_type, next_body_type)
                            if next_body_type.binds_local(Index::last()) =>
                        {
                            let param_type = self.check(param_type);
                            let param_name = self.push_local(*param_name);
                            patterns.push((
                                Pattern::Name((), param_name),
                                Some(self.scope.to_scope(param_type) as &_),
                            ));
                            body_type = next_body_type;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        core::Term::FunType(_, _, param_type, body_type) => {
                            let param_type = self.check(param_type);

                            self.push_local(None);
                            let body_type = self.check(body_type);
                            self.pop_local();

                            break Term::Arrow(
                                (),
                                self.scope.to_scope(param_type),
                                self.scope.to_scope(body_type),
                            );
                        }
                        body_type => break self.check(body_type),
                    }
                };

                self.truncate_local(initial_local_len);

                if patterns.is_empty() {
                    body_type
                } else {
                    Term::FunType(
                        (),
                        self.scope.to_scope_from_iter(patterns),
                        self.scope.to_scope(body_type),
                    )
                }
            }

            core::Term::FunLit(..) => {
                let initial_local_len = self.local_len();
                let mut param_names = Vec::new();
                let mut body_expr = core_term;

                while let core::Term::FunLit(_, param_name, next_body_expr) = body_expr {
                    param_names.push(self.push_local(*param_name));
                    body_expr = next_body_expr;
                }

                let body_expr = self.synth(body_expr);
                self.truncate_local(initial_local_len);

                let patterns = param_names
                    .into_iter()
                    .map(|name| (Pattern::Name((), name), None));

                Term::FunLiteral(
                    (),
                    self.scope.to_scope_from_iter(patterns),
                    self.scope.to_scope(body_expr),
                )
            }
            core::Term::FunApp(_, mut head_expr, arg_expr) => match head_expr {
                core::Term::FunApp(_, core::Term::Prim(_, prim), lhs)
                    if prim_to_bin_op(prim).is_some() =>
                {
                    // unwrap is safe due to is_some check above
                    self.synth_bin_op(lhs, arg_expr, prim_to_bin_op(prim).unwrap())
                }
                _ => {
                    let mut arg_exprs = vec![self.check(arg_expr)];

                    while let core::Term::FunApp(_, next_head_expr, arg_expr) = head_expr {
                        head_expr = next_head_expr;
                        arg_exprs.push(self.check(arg_expr));
                    }

                    let head_expr = self.synth(head_expr);

                    Term::App(
                        (),
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope_from_iter(arg_exprs.into_iter().rev()),
                    )
                }
            },
            core::Term::RecordType(_span, labels, exprs)
                if is_tuple_type(labels, exprs, &self.interner.borrow()) =>
            {
                let initial_local_len = self.local_len();
                let types = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), exprs.iter()).map(|(label, r#type)| {
                        let r#type = self.check(r#type);
                        self.push_local(Some(*label));
                        r#type
                    }),
                );
                self.truncate_local(initial_local_len);
                Term::Ann(
                    (),
                    self.scope.to_scope(Term::Tuple((), types)),
                    &Term::Universe(()),
                )
            }
            core::Term::RecordType(_span, labels, types) => {
                let initial_local_len = self.local_len();
                let type_fields = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), types.iter()).map(|(label, r#type)| {
                        let r#type = self.check(r#type);
                        self.push_local(Some(*label));
                        TypeField {
                            label: ((), *label), // TODO: range from span
                            type_: r#type,
                        }
                    }),
                );
                self.truncate_local(initial_local_len);

                Term::RecordType((), type_fields)
            }
            core::Term::RecordLit(_span, labels, exprs)
                if is_tuple_expr(labels, &self.interner.borrow()) =>
            {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.synth(expr));
                Term::Tuple((), scope.to_scope_from_iter(exprs))
            }
            core::Term::RecordLit(_span, labels, exprs) => {
                let scope = self.scope;
                let expr_fields =
                    Iterator::zip(labels.iter(), exprs.iter()).map(|(label, expr)| ExprField {
                        label: ((), *label),
                        expr: self.synth(expr),
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

                let head_expr = self.synth(head_expr);
                Term::Proj(
                    (),
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope_from_iter(labels.into_iter().rev()),
                )
            }
            core::Term::ArrayLit(_span, elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs.iter().map(|elem_exprs| self.check(elem_exprs));

                // FIXME: Type annotations
                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }
            core::Term::FormatRecord(_span, labels, formats)
                if is_tuple_type(labels, formats, &self.interner.borrow()) =>
            {
                let scope = self.scope;
                let formats = formats.iter().map(|format| self.synth(format));
                Term::Tuple((), scope.to_scope_from_iter(formats))
            }
            core::Term::FormatRecord(_span, labels, formats) => {
                Term::FormatRecord((), self.synth_format_fields(labels, formats))
            }
            core::Term::FormatCond(_span, label, format, cond) => {
                let format = self.check(format);
                self.push_local(Some(*label));
                let cond = self.check(cond);
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
                core::Const::U8(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U8Type)
                }
                core::Const::U16(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U16Type)
                }
                core::Const::U32(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U32Type)
                }
                core::Const::U64(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U64Type)
                }
                core::Const::S8(number) => self.synth_number_literal(number, core::Prim::S8Type),
                core::Const::S16(number) => self.synth_number_literal(number, core::Prim::S16Type),
                core::Const::S32(number) => self.synth_number_literal(number, core::Prim::S32Type),
                core::Const::S64(number) => self.synth_number_literal(number, core::Prim::S64Type),
                core::Const::F32(number) => self.synth_number_literal(number, core::Prim::F32Type),
                core::Const::F64(number) => self.synth_number_literal(number, core::Prim::F64Type),
                core::Const::Pos(number) => self.synth_number_literal(number, core::Prim::PosType),
                core::Const::Ref(number) => self.synth_number_literal(number, core::Prim::RefType),
            },
            core::Term::ConstMatch(_span, head_expr, branches, default_expr) => {
                if let Some((then_expr, else_expr)) = match_if_then_else(branches, *default_expr) {
                    let cond_expr = self.check(head_expr);
                    let then_expr = self.synth(then_expr);
                    let else_expr = self.synth(else_expr);
                    return Term::If(
                        (),
                        self.scope.to_scope(cond_expr),
                        self.scope.to_scope(then_expr),
                        self.scope.to_scope(else_expr),
                    );
                }

                let head_expr = self.synth(head_expr);
                match default_expr {
                    Some(default_expr) => {
                        let default_branch = {
                            let name = self.push_local(None);
                            let default_expr = self.synth(default_expr);
                            self.pop_local();

                            (Pattern::Name((), name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, body_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let body_expr = self.synth(body_expr);
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
                                let body_expr = self.synth(body_expr);
                                (pattern, body_expr)
                            },
                        )),
                    ),
                }
            }
        }
    }

    fn synth_bin_op(
        &mut self,
        lhs: &core::Term<'_>,
        rhs: &core::Term<'_>,
        op: BinOp<()>,
    ) -> Term<'arena, ()> {
        let lhs = self.synth(lhs);
        let rhs = self.synth(rhs);
        Term::BinOp(
            (),
            self.scope.to_scope(lhs),
            op,
            self.scope.to_scope(self.scope.to_scope(rhs)),
        )
    }

    fn synth_format_fields(
        &mut self,
        labels: &[StringId],
        core_formats: &[core::Term<'_>],
    ) -> &'arena [FormatField<'arena, ()>] {
        use crate::core::Prim::FormatSucceed;

        let initial_local_len = self.local_len();
        let core_fields = Iterator::zip(labels.iter().copied(), core_formats.iter());
        let format_fields =
            (self.scope).to_scope_from_iter(core_fields.map(|(label, format)| match format {
                // Distill succeed formats back to computed formats
                core::Term::FunApp(
                    _,
                    core::Term::FunApp(_span, core::Term::Prim(_prim_span, FormatSucceed), r#type),
                    expr,
                ) => {
                    let r#type = self.check(r#type);
                    let expr = self.check(expr);
                    self.push_local(Some(label));

                    FormatField::Computed {
                        label: ((), label),
                        type_: Some(r#type),
                        expr,
                    }
                }
                // Use field refinements when `format` is a conditional format
                // that binds the same name as the current field label.
                core::Term::FormatCond(_span, name, format, pred) if label == *name => {
                    let format = self.check(format);
                    self.push_local(Some(label));
                    let pred = self.check(pred);

                    FormatField::Format {
                        label: ((), label),
                        format,
                        pred: Some(pred),
                    }
                }
                // Otherwise stick with a regular format field...
                format => {
                    let format = self.check(format);
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

fn match_if_then_else<'arena>(
    branches: &'arena [(Const, core::Term<'arena>)],
    default_expr: Option<&'arena core::Term<'arena>>,
) -> Option<(&'arena core::Term<'arena>, &'arena core::Term<'arena>)> {
    match (branches, default_expr) {
        ([(Const::Bool(false), else_expr), (Const::Bool(true), then_expr)], None)
        // TODO: Normalise boolean branches when elaborating patterns
        | ([(Const::Bool(true), then_expr)], Some(else_expr))
        | ([(Const::Bool(false), else_expr)], Some(then_expr)) => Some((then_expr, else_expr)),
        _ => None,
    }
}

// Return true if `labels` are all tuple labels
fn is_tuple_expr(labels: &[StringId], interner: &StringInterner) -> bool {
    labels
        .iter()
        .enumerate()
        .all(|(idx, label)| interner.resolve(*label) == Some(&format!("_{}", idx)))
}

// Return true if `labels` are all tuple labels, and `exprs` do not depend on any of the expressions bound by `labels`
fn is_tuple_type(labels: &[StringId], exprs: &[core::Term<'_>], interner: &StringInterner) -> bool {
    let suffixes = (1..=exprs.len()).rev().map(move |idx| &exprs[idx..]);
    labels
        .iter()
        .zip(suffixes)
        .enumerate()
        .all(|(idx, (label, suffix))| {
            interner.resolve(*label) == Some(&format!("_{}", idx))
                && suffix
                    .iter()
                    .zip(env::indices())
                    .all(|(expr, idx)| !expr.binds_local(idx))
        })
}

fn prim_to_bin_op(prim: &core::Prim) -> Option<BinOp<()>> {
    use crate::core::Prim::*;

    match prim {
        U8Mul | U16Mul | U32Mul | U64Mul | S8Mul | S16Mul | S32Mul | S64Mul => Some(BinOp::Mul(())),
        U8Div | U16Div | U32Div | U64Div | S8Div | S16Div | S32Div | S64Div => Some(BinOp::Div(())),
        U8Add | U16Add | U32Add | U64Add | S8Add | S16Add | S32Add | S64Add | PosAddU8
        | PosAddU16 | PosAddU32 | PosAddU64 => Some(BinOp::Add(())),
        U8Sub | U16Sub | U32Sub | U64Sub | S8Sub | S16Sub | S32Sub | S64Sub => Some(BinOp::Sub(())),
        BoolEq | U8Eq | U16Eq | U32Eq | U64Eq | S8Eq | S16Eq | S32Eq | S64Eq => Some(BinOp::Eq(())),
        BoolNeq | U8Neq | U16Neq | U32Neq | U64Neq | S8Neq | S16Neq | S32Neq | S64Neq => {
            Some(BinOp::Neq(()))
        }
        U8Lt | U16Lt | U32Lt | U64Lt | S8Lt | S16Lt | S32Lt | S64Lt => Some(BinOp::Lt(())),
        U8Lte | U16Lte | U32Lte | U64Lte | S8Lte | S16Lte | S32Lte | S64Lte => Some(BinOp::Lte(())),
        U8Gt | U16Gt | U32Gt | U64Gt | S8Gt | S16Gt | S32Gt | S64Gt => Some(BinOp::Gt(())),
        U8Gte | U16Gte | U32Gte | U64Gte | S8Gte | S16Gte | S32Gte | S64Gte => Some(BinOp::Gte(())),

        _ => None,
    }
}
