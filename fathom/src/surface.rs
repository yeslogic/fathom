//! Surface language.

use std::cell::RefCell;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fxhash::{FxHashMap, FxHashSet};
use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;

use crate::source::{ByteRange, FileId};
use crate::{StringId, StringInterner};

lalrpop_mod!(grammar, "/surface/grammar.rs");
mod lexer;
pub mod pretty;

pub mod distillation;
pub mod elaboration;

/// Modules, consisting of a sequence of top-level items.
#[derive(Debug, Clone)]
pub struct Module<'arena, Range> {
    items: &'arena [Item<'arena, Range>],
}

impl<'arena> Module<'arena, ByteRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse<'source>(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        file_id: FileId,
        source: &'source str,
    ) -> (Module<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(file_id, source);
        let term = grammar::ModuleParser::new()
            .parse(interner, scope, &mut messages, file_id, tokens)
            .unwrap_or_else(|error| {
                messages.push(ParseMessage::from_lalrpop(file_id, error));
                Module { items: &[] }
            });

        (term, messages)
    }

    pub fn elaboration_order(&self) -> Result<Vec<usize>, Vec<ModuleOrderMessage>> {
        let item_names = self.item_names();
        let item_deps = self.item_dependencies(&item_names);

        let mut context = ModuleOrderContext::new();
        context.determine_order(&self.items, &item_names, &item_deps);

        if context.messages.is_empty() {
            Ok(context.output)
        } else {
            Err(context.messages)
        }
    }

    fn item_names(&self) -> FxHashMap<StringId, usize> {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(i, item)| match item {
                Item::Definition {
                    label: (_, label), ..
                } => Some((*label, i)),
                Item::ReportedError(_) => None,
            })
            .collect()
    }

    fn item_dependencies(&self, item_names: &FxHashMap<StringId, usize>) -> Vec<Vec<StringId>> {
        let mut local_names = Vec::new();
        self.items
            .iter()
            .map(|item| item_dependencies(item, &item_names, &mut local_names))
            .collect()
    }
}

struct ModuleOrderContext {
    output: Vec<usize>,
    visited: FxHashSet<StringId>,
    stack: Vec<StringId>,
    messages: Vec<ModuleOrderMessage>,
}

impl ModuleOrderContext {
    fn new() -> Self {
        ModuleOrderContext {
            output: Vec::new(),
            visited: FxHashSet::default(),
            stack: Vec::new(),
            messages: Vec::new(),
        }
    }

    fn determine_order(
        &mut self,
        items: &[Item<'_, ByteRange>],
        item_names: &FxHashMap<StringId, usize>,
        dependencies: &[Vec<StringId>],
    ) {
        for item in items {
            match item {
                Item::Definition {
                    label: (_, label), ..
                } => {
                    self.visit_item(*label, item_names, dependencies);
                    self.stack.clear();
                }
                Item::ReportedError(_) => {}
            }
        }
    }

    fn visit_item(
        &mut self,
        name: StringId,
        item_names: &FxHashMap<StringId, usize>,
        dependencies: &[Vec<StringId>],
    ) {
        if self.visited.contains(&name) {
            return;
        }

        if self.stack.contains(&name) {
            self.stack.push(name);
            self.messages.push(ModuleOrderMessage::CycleDetected {
                names: self.stack.clone(),
            });
            return;
        }

        match item_names.get(&name) {
            Some(index) => {
                let index = *index;
                self.stack.push(name);
                let deps = &dependencies[index];
                for dep in deps {
                    self.visit_item(*dep, item_names, dependencies);
                }
                self.stack.pop();
                self.visited.insert(name);
                self.output.push(index);
            }
            // not a module item
            None => {}
        }
    }
}

fn item_dependencies(
    item: &Item<'_, ByteRange>,
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
) -> Vec<StringId> {
    let mut deps = Vec::new();
    match item {
        Item::Definition {
            label: _,
            type_,
            expr,
        } => {
            if let Some(r#type) = type_ {
                term_deps(r#type, item_names, local_names, &mut deps);
            }
            term_deps(expr, item_names, local_names, &mut deps);
        }
        Item::ReportedError(_) => {}
    }
    deps
}

fn term_deps(
    term: &Term<ByteRange>,
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
    deps: &mut Vec<StringId>,
) {
    match term {
        Term::Name(_, name) => {
            if local_names.iter().rev().any(|local| name == local) {
                // local binding, do nothing
            } else if item_names.contains_key(name) {
                if deps.last() != Some(name) {
                    // Only push if it's not a duplicate of the last item. This is a basic way
                    // to reduce the number of duplicate dependencies that are pushed.
                    deps.push(*name);
                }
            }
        }
        Term::Ann(_, expr, r#type) => {
            term_deps(expr, item_names, local_names, deps);
            term_deps(r#type, item_names, local_names, deps);
        }
        Term::Let(_, pattern, r#type, def_expr, body_expr) => {
            push_pattern(pattern, local_names);
            if let Some(r#type) = r#type {
                term_deps(r#type, item_names, local_names, deps);
            }
            term_deps(def_expr, item_names, local_names, deps);
            term_deps(body_expr, item_names, local_names, deps);
            pop_pattern(pattern, local_names);
        }
        Term::Match(_, scrutinee, equations) => {
            let initial_locals_names_len = local_names.len();
            term_deps(scrutinee, item_names, local_names, deps);
            for (pattern, body) in *equations {
                push_pattern(pattern, local_names);
                term_deps(body, item_names, local_names, deps);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::Arrow(_, input_type, output_type) => {
            term_deps(input_type, item_names, local_names, deps);
            term_deps(output_type, item_names, local_names, deps);
        }
        Term::FunType(_, input_pattern, input_type, output_type) => {
            push_pattern(input_pattern, local_names);
            if let Some(input_type) = input_type {
                term_deps(input_type, item_names, local_names, deps);
            }
            term_deps(output_type, item_names, local_names, deps);
            pop_pattern(input_pattern, local_names);
        }
        Term::FunLiteral(_, input_param, input_type, output_type) => {
            push_pattern(input_param, local_names);
            if let Some(input_type) = input_type {
                term_deps(input_type, item_names, local_names, deps);
            }
            term_deps(output_type, item_names, local_names, deps);
            pop_pattern(input_param, local_names);
        }
        Term::App(_, head_expr, input_expr) => {
            term_deps(head_expr, item_names, local_names, deps);
            term_deps(input_expr, item_names, local_names, deps);
        }
        Term::RecordType(_, type_fields) => {
            let initial_locals_names_len = local_names.len();
            for type_field in *type_fields {
                term_deps(&type_field.type_, item_names, local_names, deps);
                local_names.push(type_field.label.1);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::RecordLiteral(_, expr_fields) => {
            let initial_locals_names_len = local_names.len();
            for expr_field in *expr_fields {
                term_deps(&expr_field.expr, item_names, local_names, deps);
                local_names.push(expr_field.label.1);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::Proj(_, head_expr, _) => {
            term_deps(head_expr, item_names, local_names, deps);
        }
        Term::ArrayLiteral(_, terms) => {
            for term in *terms {
                term_deps(term, item_names, local_names, deps);
            }
        }
        Term::FormatRecord(_, fields) => {
            field_deps(fields, item_names, local_names, deps);
        }
        Term::FormatOverlap(_, format_fields) => {
            field_deps(format_fields, item_names, local_names, deps);
        }
        Term::FormatCond(_, (_, name), format, cond) => {
            local_names.push(*name);
            term_deps(format, item_names, local_names, deps);
            term_deps(cond, item_names, local_names, deps);
            local_names.pop();
        }
        Term::Hole(_, _)
        | Term::Placeholder(_)
        | Term::Universe(_)
        | Term::UnitLiteral(_)
        | Term::StringLiteral(_, _)
        | Term::NumberLiteral(_, _)
        | Term::BooleanLiteral(_, _)
        | Term::ReportedError(_) => {}
    }
}

fn field_deps(
    fields: &[FormatField<ByteRange>],
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
    deps: &mut Vec<StringId>,
) {
    let initial_locals_names_len = local_names.len();
    for field in fields {
        match field {
            FormatField::Format {
                label: (_, label),
                format,
                ..
            } => {
                term_deps(format, item_names, local_names, deps);
                local_names.push(*label)
            }
            FormatField::Computed {
                label: (_, label),
                type_,
                expr,
            } => {
                if let Some(r#type) = type_ {
                    term_deps(r#type, item_names, local_names, deps);
                }
                term_deps(expr, item_names, local_names, deps);
                local_names.push(*label)
            }
        }
    }
    local_names.truncate(initial_locals_names_len);
}

fn push_pattern(pattern: &Pattern<ByteRange>, local_names: &mut Vec<StringId>) {
    match pattern {
        Pattern::Name(_, name) => local_names.push(*name),
        Pattern::Placeholder(_) => {}
        Pattern::StringLiteral(_, _) => {}
        Pattern::NumberLiteral(_, _) => {}
        Pattern::BooleanLiteral(_, _) => {}
    }
}

fn pop_pattern(pattern: &Pattern<ByteRange>, local_names: &mut Vec<StringId>) {
    match pattern {
        Pattern::Name(_, _) => {
            local_names.pop();
        }
        Pattern::Placeholder(_) => {}
        Pattern::StringLiteral(_, _) => {}
        Pattern::NumberLiteral(_, _) => {}
        Pattern::BooleanLiteral(_, _) => {}
    }
}

/// Top-level items.
#[derive(Debug, Clone)]
pub enum Item<'arena, Range> {
    /// Top-level definitions
    Definition {
        /// The label that identifies this definition
        label: (Range, StringId),
        /// An optional type annotation for the defined expression
        // FIXME: raw identifiers in LALRPOP grammars https://github.com/lalrpop/lalrpop/issues/613
        type_: Option<&'arena Term<'arena, Range>>,
        /// The defined expression
        expr: &'arena Term<'arena, Range>,
    },
    /// Reported error sentinel
    ReportedError(Range),
}

/// Surface patterns.
#[derive(Debug, Clone)]
pub enum Pattern<Range> {
    /// Named patterns, eg. `x`, `true`, `false`
    Name(Range, StringId),
    /// Placeholder patterns, eg. `_`
    Placeholder(Range),
    /// String literal patterns, eg. `"htmx"`
    ///
    /// As with [term literals][Term::StringLiteral], these will be parsed fully
    /// during [elaboration].
    StringLiteral(Range, StringId),
    /// Number literal patterns, eg. `1`, `0x00FF`
    ///
    /// As with [term literals][Term::NumberLiteral], these will be parsed fully
    /// during [elaboration].
    NumberLiteral(Range, StringId),
    /// Boolean literal patterns
    BooleanLiteral(Range, bool),
    // TODO: Record literal patterns
    // RecordLiteral(Range, &'arena [((ByteRange, StringId), Pattern<'arena, Range>)]),
}

impl<Range: Clone> Pattern<Range> {
    fn range(&self) -> Range {
        match self {
            Pattern::Name(range, _)
            | Pattern::Placeholder(range)
            | Pattern::StringLiteral(range, _)
            | Pattern::NumberLiteral(range, _)
            | Pattern::BooleanLiteral(range, _) => range.clone(),
        }
    }
}

/// Surface terms.
#[derive(Debug, Clone)]
pub enum Term<'arena, Range> {
    /// Named patterns.
    Name(Range, StringId),
    /// Hole expressions.
    Hole(Range, StringId),
    /// Placeholder expressions.
    Placeholder(Range),
    /// Annotated expressions.
    Ann(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Let expressions.
    Let(
        Range,
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Match expressions
    Match(
        Range,
        &'arena Term<'arena, Range>,
        &'arena [(Pattern<Range>, Term<'arena, Range>)],
    ),
    /// The type of types.
    Universe(Range),
    /// Arrow types.
    Arrow(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Dependent function types.
    FunType(
        Range,
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
    ),
    /// Function literals.
    FunLiteral(
        Range,
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
    ),
    /// Applications.
    App(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Dependent record types.
    RecordType(Range, &'arena [TypeField<'arena, Range>]),
    /// Record literals.
    RecordLiteral(Range, &'arena [ExprField<'arena, Range>]),
    /// Unit literals.
    UnitLiteral(Range),
    /// Projections.
    Proj(Range, &'arena Term<'arena, Range>, (Range, StringId)),
    /// Array literals.
    ArrayLiteral(Range, &'arena [Term<'arena, Range>]),
    /// String literal.
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
    StringLiteral(Range, StringId),
    /// Number literals.
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
    NumberLiteral(Range, StringId),
    /// Boolean literals.
    BooleanLiteral(Range, bool),
    /// Record format.
    FormatRecord(Range, &'arena [FormatField<'arena, Range>]),
    /// Overlap format.
    FormatOverlap(Range, &'arena [FormatField<'arena, Range>]),
    /// Conditional format.
    FormatCond(
        Range,
        (Range, StringId),
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Reported error sentinel.
    ReportedError(Range),
}

impl<'arena, Range: Clone> Term<'arena, Range> {
    /// Get the source range of the term.
    pub fn range(&self) -> Range {
        match self {
            Term::Name(range, _)
            | Term::Hole(range, _)
            | Term::Placeholder(range)
            | Term::Ann(range, _, _)
            | Term::Let(range, _, _, _, _)
            | Term::Match(range, _, _)
            | Term::Universe(range)
            | Term::Arrow(range, _, _)
            | Term::FunType(range, _, _, _)
            | Term::FunLiteral(range, _, _, _)
            | Term::App(range, _, _)
            | Term::RecordType(range, _)
            | Term::RecordLiteral(range, _)
            | Term::UnitLiteral(range)
            | Term::Proj(range, _, _)
            | Term::ArrayLiteral(range, _)
            | Term::StringLiteral(range, _)
            | Term::NumberLiteral(range, _)
            | Term::BooleanLiteral(range, _)
            | Term::FormatRecord(range, _)
            | Term::FormatCond(range, _, _, _)
            | Term::FormatOverlap(range, _)
            | Term::ReportedError(range) => range.clone(),
        }
    }
}

impl<'arena> Term<'arena, ByteRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse<'source>(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        file_id: FileId,
        source: &'source str,
    ) -> (Term<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(file_id, source);
        let term = grammar::TermParser::new()
            .parse(interner, scope, &mut messages, file_id, tokens)
            .unwrap_or_else(|error| {
                let message = ParseMessage::from_lalrpop(file_id, error);
                let range = message.range();
                messages.push(message);
                Term::ReportedError(range)
            });

        (term, messages)
    }
}

/// A field declaration in a record and offset format
#[derive(Debug, Clone)]
pub enum FormatField<'arena, Range> {
    /// Regular format field
    Format {
        /// Label identifying the field
        label: (Range, StringId),
        /// The format that this field will be parsed with
        format: Term<'arena, Range>,
        /// An optional predicate that refines the format field
        pred: Option<Term<'arena, Range>>,
    },
    /// Computed format field
    Computed {
        /// Label identifying the field
        label: (Range, StringId),
        /// Optional type annotation
        // FIXME: raw identifiers in LALRPOP grammars https://github.com/lalrpop/lalrpop/issues/613
        type_: Option<Term<'arena, Range>>,
        /// The expression that this field compute
        expr: Term<'arena, Range>,
    },
}

/// A field declaration in a record type
#[derive(Debug, Clone)]
pub struct TypeField<'arena, Range> {
    /// Label identifying the field
    label: (Range, StringId),
    /// The type that is expected for this field
    // FIXME: raw identifiers in LALRPOP grammars https://github.com/lalrpop/lalrpop/issues/613
    type_: Term<'arena, Range>,
}

/// A field definition in a record literal
#[derive(Debug, Clone)]
pub struct ExprField<'arena, Range> {
    /// Label identifying the field
    label: (Range, StringId),
    /// The expression that this field will store
    expr: Term<'arena, Range>,
}

/// Messages produced during parsing
#[derive(Clone, Debug)]
pub enum ParseMessage {
    Lexer(lexer::Error),
    InvalidToken {
        range: ByteRange,
    },
    UnrecognizedEof {
        range: ByteRange,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        range: ByteRange,
        token: &'static str,
        expected: Vec<String>,
    },
    ExtraToken {
        range: ByteRange,
        token: &'static str,
    },
}

impl ParseMessage {
    pub fn range(&self) -> ByteRange {
        match self {
            ParseMessage::Lexer(error) => error.range(),
            ParseMessage::InvalidToken { range }
            | ParseMessage::UnrecognizedEof { range, .. }
            | ParseMessage::UnrecognizedToken { range, .. }
            | ParseMessage::ExtraToken { range, .. } => *range,
        }
    }

    fn from_lalrpop(file_id: FileId, error: LalrpopParseError<'_>) -> ParseMessage {
        match error {
            LalrpopParseError::InvalidToken { location } => ParseMessage::InvalidToken {
                range: ByteRange::new(file_id, location, location),
            },
            LalrpopParseError::UnrecognizedEOF { location, expected } => {
                ParseMessage::UnrecognizedEof {
                    range: ByteRange::new(file_id, location, location),
                    expected, // TODO: convert to descriptions?
                }
            }
            LalrpopParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => ParseMessage::UnrecognizedToken {
                range: ByteRange::new(file_id, start, end),
                token: token.description(),
                expected,
            },
            LalrpopParseError::ExtraToken {
                token: (start, token, end),
            } => ParseMessage::ExtraToken {
                range: ByteRange::new(file_id, start, end),
                token: token.description(),
            },
            LalrpopParseError::User { error } => ParseMessage::Lexer(error),
        }
    }

    fn from_lalrpop_recovery(file_id: FileId, error: LalrpopErrorRecovery<'_>) -> ParseMessage {
        // TODO: make use of use `error.dropped_tokens` in error reporting?
        ParseMessage::from_lalrpop(file_id, error.error)
    }

    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        let primary_label = |range: &ByteRange| Label::primary(range.file_id(), *range);

        match self {
            ParseMessage::Lexer(error) => error.to_diagnostic(),
            ParseMessage::InvalidToken { range } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![primary_label(range)]),
            ParseMessage::UnrecognizedEof { range, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    primary_label(range).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::UnrecognizedToken {
                range,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(vec![primary_label(range).with_message("unexpected token")])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::ExtraToken { range, token } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(vec![primary_label(range).with_message("extra token")]),
        }
    }
}

type LalrpopParseError<'source> =
    lalrpop_util::ParseError<usize, lexer::Token<'source>, lexer::Error>;

type LalrpopErrorRecovery<'source> =
    lalrpop_util::ErrorRecovery<usize, lexer::Token<'source>, lexer::Error>;

fn format_expected(expected: &[impl std::fmt::Display]) -> Option<String> {
    use itertools::Itertools;

    expected.split_last().map(|items| match items {
        (last, []) => format!("expected {}", last),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
}

#[derive(Clone, Debug)]
pub enum ModuleOrderMessage {
    CycleDetected { names: Vec<StringId> },
}

impl ModuleOrderMessage {
    pub fn to_diagnostic(&self, interner: &RefCell<StringInterner>) -> Diagnostic<FileId> {
        match self {
            ModuleOrderMessage::CycleDetected { names } => {
                let interner = interner.borrow();
                let names: Vec<_> = names
                    .iter()
                    .map(|id| interner.resolve(*id).unwrap())
                    .collect();
                let cycle = names.join(" → ");
                Diagnostic::error()
                    .with_message("cycle detected")
                    .with_notes(vec![cycle])
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_drop() {
        assert!(!std::mem::needs_drop::<Term<'_, ()>>());
        assert!(!std::mem::needs_drop::<Term<'_, StringId>>());
        assert!(!std::mem::needs_drop::<Pattern<StringId>>());
    }
}
