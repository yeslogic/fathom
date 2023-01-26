// TODO: avoid trailing whitespace

use std::cell::RefCell;

use self::printer::Printer;
use super::lexer::is_keyword;
use crate::source::{StringId, StringInterner};
use crate::surface::*;

mod printer;

pub struct Context<'interner> {
    pub printer: Printer,
    interner: &'interner RefCell<StringInterner>,
}

const INDENT: usize = 4;

impl<'interner> Context<'interner> {
    pub fn new(target_width: usize, interner: &'interner RefCell<StringInterner>) -> Self {
        Self {
            printer: Printer::new(target_width),
            interner,
        }
    }

    pub fn flush(&mut self) -> String {
        self.printer.flush()
    }

    pub fn text(&mut self, text: impl AsRef<str>) {
        self.printer.string(text.as_ref())
    }

    fn symbol(&mut self, symbol: StringId) {
        match self.interner.borrow().resolve(symbol) {
            Some(text) => self.text(text),
            None => self.text("#error"),
        }
    }

    fn ident(&mut self, name: StringId) {
        match self.interner.borrow().resolve(name) {
            Some(name) if is_keyword(name) => self.text(format!("r#{name}")),
            Some(name) => self.text(name),
            None => self.text("#error"),
        }
    }

    pub fn module<Range>(&mut self, module: &Module<'_, Range>) {
        for item in module.items {
            self.item(item);
            self.printer.hardline();
        }
    }

    fn item<Range>(&mut self, item: &Item<'_, Range>) {
        match item {
            Item::ReportedError(_) => self.printer.string("#error"),
            Item::Def(def) => {
                self.printer.open_group();
                self.printer.string("def ");
                match def.r#type {
                    None => {
                        self.ident(def.label.1);
                        self.params(def.params);
                        self.printer.string(" ");
                    }
                    Some(r#type) => {
                        self.printer.open_group();
                        self.ident(def.label.1);
                        self.params(def.params);
                        self.printer.string(" : ");
                        self.printer.close_group();
                        self.printer.softline();
                        self.term(r#type)
                    }
                }
                self.text(" = ");
                self.printer.softline();
                self.term(def.expr);
                self.text(";");
                self.printer.close_group();
            }
        }
    }

    fn pattern<Range>(&mut self, pattern: &Pattern<Range>) {
        match pattern {
            Pattern::Placeholder(_) => self.text("_"),
            Pattern::Name(_, name) => self.ident(*name),
            Pattern::StringLiteral(_, symbol) => {
                self.text("\"");
                self.symbol(*symbol);
                self.text("\"");
            }
            Pattern::NumberLiteral(_, symbol) => self.symbol(*symbol),
            Pattern::BooleanLiteral(_, true) => self.printer.string("true"),
            Pattern::BooleanLiteral(_, false) => self.printer.string("false"),
        }
    }

    fn ann_pattern<Range>(&mut self, pattern: &Pattern<Range>, r#type: Option<&Term<'_, Range>>) {
        match r#type {
            None => self.pattern(pattern),
            Some(r#type) => {
                self.printer.open_group();
                self.pattern(pattern);
                self.text(" : ");
                self.printer.close_group();
                self.printer.softline();
                self.term(r#type);
            }
        }
    }

    fn plicity(&mut self, plicity: Plicity) {
        match plicity {
            Plicity::Explicit => {}
            Plicity::Implicit => self.text("@"),
        }
    }

    fn param<Range>(&mut self, param: &Param<'_, Range>) {
        match &param.r#type {
            None => {
                self.plicity(param.plicity);
                self.pattern(&param.pattern);
            }
            Some(r#type) => {
                self.text("(");
                self.printer.open_group();
                self.plicity(param.plicity);
                self.pattern(&param.pattern);
                self.text(" : ");
                self.printer.close_group();
                self.printer.softline();
                self.term(r#type);
                self.text(")");
            }
        }
    }

    fn params<Range>(&mut self, params: &[Param<'_, Range>]) {
        for param in params {
            self.text(" ");
            self.param(param);
        }
    }

    fn arg<Range>(&mut self, arg: &Arg<'_, Range>) {
        self.plicity(arg.plicity);
        self.term(&arg.term);
    }

    pub fn term<Range>(&mut self, term: &Term<'_, Range>) {
        match term {
            Term::Paren(_, term) => {
                self.text("(");
                self.term(term);
                self.text(")");
            }
            Term::Name(_, name) => self.ident(*name),
            Term::Hole(_, name) => {
                self.text("?");
                self.symbol(*name);
            }
            Term::Placeholder(_) => self.text("_"),
            Term::Ann(_, term, r#type) => {
                self.printer.open_group();
                self.term(term);
                self.text(" : ");
                self.printer.softline();
                self.term(r#type);
                self.printer.close_group();
            }
            Term::Let(_, pattern, r#type, expr, body) => {
                self.printer.open_group();
                self.text("let ");
                self.ann_pattern(pattern, *r#type);
                self.text(" = ");
                self.printer.softline();
                self.term(expr);
                self.text(";");
                self.printer.close_group();
                self.printer.hardline();
                self.term(body);
            }
            Term::If(_, cond, then, mut r#else) => {
                let mut branches = Vec::new();

                while let Term::If(_, cond, then, next_else) = r#else {
                    branches.push((*cond, *then));
                    r#else = next_else;
                }

                if branches.is_empty() {
                    self.printer.open_group();

                    self.text("if ");
                    self.term(cond);

                    self.printer.consistent_line();
                    self.printer.indent(INDENT);
                    self.text("then ");
                    self.term(then);
                    self.printer.outdent(INDENT);

                    self.printer.consistent_line();
                    self.printer.indent(INDENT);
                    self.text("else ");
                    self.term(r#else);
                    self.printer.outdent(INDENT);

                    self.printer.close_group();
                } else {
                    self.printer.open_group();

                    self.text("if ");
                    self.term(cond);
                    self.printer.softline();

                    self.printer.consistent_line();
                    self.printer.indent(INDENT);
                    self.text("then ");
                    self.term(then);
                    self.printer.outdent(INDENT);

                    for (cond, then) in branches {
                        self.printer.consistent_line();
                        self.printer.indent(INDENT);
                        self.text("else if ");
                        self.term(cond);
                        self.text(" then ");
                        self.term(then);
                        self.printer.outdent(INDENT);
                    }

                    self.printer.consistent_line();
                    self.printer.indent(INDENT);
                    self.text("else ");
                    self.term(r#else);
                    self.printer.outdent(INDENT);

                    self.printer.close_group();
                }
            }
            Term::Match(_, scrut, branches) => {
                self.text("match ");
                self.term(scrut);
                self.text(" {");
                self.printer.open_group();
                self.printer.indent(INDENT);
                self.printer.consistent_line();
                if let Some(((pattern, expr), branches)) = branches.split_first() {
                    self.pattern(pattern);
                    self.text(" => ");
                    self.term(expr);
                    for (pattern, expr) in branches.iter() {
                        self.text(",");
                        self.printer.consistent_line();
                        self.pattern(pattern);
                        self.text(" => ");
                        self.term(expr);
                    }
                }
                self.printer.consistent_line();
                self.printer.outdent(INDENT);
                self.printer.close_group();
                self.text("}");
            }
            Term::Universe(_) => self.text("Type"),
            Term::Arrow(_, plicity, r#type, body) => {
                self.plicity(*plicity);
                self.term(r#type);
                self.printer.softline();
                self.text(" -> ");
                self.printer.softline();
                self.term(body);
            }
            Term::FunType(_, params, body) => {
                self.printer.open_group();
                self.text("fun");
                self.params(params);
                self.text(" -> ");
                self.printer.close_group();
                self.term(body);
            }
            Term::FunLiteral(_, params, body) => {
                self.printer.open_group();
                self.text("fun");
                self.params(params);
                self.text(" => ");
                self.printer.close_group();
                self.term(body);
            }
            Term::App(_, fun, args) => {
                self.term(fun);
                for arg in args.iter() {
                    self.text(" ");
                    self.arg(arg);
                }
            }
            Term::RecordType(_, fields) => {
                self.printer.open_group();
                self.text("{");
                self.printer.consistent_line();
                self.printer.indent(INDENT);
                if let Some((field, fields)) = fields.split_first() {
                    self.ident(field.label.1);
                    self.text(" : ");
                    self.term(&field.r#type);

                    for field in fields.iter() {
                        self.text(",");
                        self.printer.consistent_line();
                        self.ident(field.label.1);
                        self.text(" : ");
                        self.term(&field.r#type);
                    }
                }
                self.printer.outdent(INDENT);
                self.printer.consistent_line();
                self.text("}");
                self.printer.close_group();
            }
            Term::RecordLiteral(_, fields) => {
                self.printer.open_group();
                self.text("{");
                self.printer.consistent_line();
                self.printer.indent(INDENT);
                if let Some((field, fields)) = fields.split_first() {
                    self.ident(field.label.1);
                    self.text(" = ");
                    self.term(&field.expr);

                    for field in fields.iter() {
                        self.text(",");
                        self.printer.consistent_line();
                        self.ident(field.label.1);
                        self.text(" = ");
                        self.term(&field.expr);
                    }
                }
                self.printer.outdent(INDENT);
                self.printer.consistent_line();
                self.text("}");
                self.printer.close_group();
            }
            Term::Tuple(_, terms) if terms.len() == 1 => {
                self.text("(");
                self.term(&terms[0]);
                self.text(",)");
            }
            Term::Tuple(_, terms) => {
                self.printer.open_group();
                self.text("(");
                self.printer.indent(INDENT);
                if let Some((term, terms)) = terms.split_first() {
                    self.term(term);
                    for term in terms {
                        self.text(",");
                        self.printer.consistent_line();
                        self.term(term);
                    }
                }
                self.printer.outdent(INDENT);
                self.text(")");
                self.printer.close_group();
            }
            Term::Proj(_, head, labels) => {
                self.term(head);
                for label in labels.iter() {
                    self.text(".");
                    self.ident(label.1);
                }
            }
            Term::ArrayLiteral(_, terms) => {
                self.printer.open_group();
                self.text("[");
                self.printer.indent(INDENT);
                if let Some((term, terms)) = terms.split_first() {
                    self.term(term);
                    for term in terms {
                        self.text(",");
                        self.printer.consistent_line();
                        self.term(term);
                    }
                }
                self.printer.outdent(INDENT);
                self.text("]");
                self.printer.close_group();
            }
            Term::StringLiteral(_, symbol) => {
                self.text("\"");
                self.symbol(*symbol);
                self.text("\"");
            }
            Term::NumberLiteral(_, symbol) => self.symbol(*symbol),
            Term::BooleanLiteral(_, true) => self.text("true"),
            Term::BooleanLiteral(_, false) => self.text("false"),
            Term::FormatRecord(_, fields) => {
                self.printer.open_group();
                self.text("{");
                self.printer.consistent_line();
                self.printer.indent(INDENT);
                if let Some((field, fields)) = fields.split_first() {
                    self.format_field(field);
                    for field in fields.iter() {
                        self.text(",");
                        self.printer.consistent_line();
                        self.format_field(field);
                    }
                }
                self.printer.outdent(INDENT);
                self.printer.consistent_line();
                self.text("}");
                self.printer.close_group();
            }
            Term::FormatOverlap(_, fields) => {
                self.printer.open_group();
                self.text("overlap {");
                self.printer.consistent_line();
                self.printer.indent(INDENT);
                if let Some((field, fields)) = fields.split_first() {
                    self.format_field(field);
                    for field in fields.iter() {
                        self.text(",");
                        self.printer.consistent_line();
                        self.format_field(field);
                    }
                }
                self.printer.outdent(INDENT);
                self.printer.consistent_line();
                self.text("}");
                self.printer.close_group();
            }
            Term::FormatCond(_, label, format, cond) => {
                self.text("{");
                self.ident(label.1);
                self.text(" <- ");
                self.term(format);
                self.text(" | ");
                self.term(cond);
                self.text(" }");
            }
            Term::BinOp(_, lhs, op, rhs) => {
                self.term(lhs);
                self.text(" ");
                self.text(op.as_str());
                self.text(" ");
                self.term(rhs);
            }
            Term::ReportedError(_) => self.text("#error"),
        }
    }

    fn format_field<Range>(&mut self, field: &FormatField<'_, Range>) {
        match field {
            FormatField::Format {
                label,
                format,
                pred,
            } => {
                self.ident(label.1);
                self.text(" <- ");
                self.term(format);
                match pred {
                    None => {}
                    Some(pred) => {
                        self.text(" where ");
                        self.term(pred);
                    }
                }
            }
            FormatField::Computed {
                label,
                r#type,
                expr,
            } => {
                self.text("let ");
                self.ident(label.1);
                match r#type {
                    None => {}
                    Some(r#type) => {
                        self.text(" : ");
                        self.term(r#type);
                    }
                }
                self.text(" = ");
                self.term(expr);
            }
        }
    }
}
