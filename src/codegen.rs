use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use std::fmt;

use ir::owned::ast::{Definition, Expr, Field, ParseExpr, Path, Program, RcType, RepeatBound, Type};
use ir::owned::ast::{Binop, Const, Unop};

pub struct LowerProgram<'a>(pub &'a Program<String>);

impl<'a> fmt::Display for LowerProgram<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let DocBuilder(_, ref doc) = lower_program(&BoxAllocator, self.0);
        doc.render_fmt(f.width().unwrap_or(MAX_WIDTH), f)
    }
}

const INDENT_WIDTH: usize = 4;
const MAX_WIDTH: usize = 100;

fn lower_program<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    program: &'a Program<String>,
) -> DocBuilder<'doc, A> {
    doc.nil()
        .append(doc.text("use std::io;").append(doc.newline()))
        .append(doc.text("use std::io::prelude::*;").append(doc.newline()))
        .append(doc.newline())
        .append(
            doc.intersperse(
                program
                    .defs
                    .iter()
                    .map(|(path, definition)| lower_definition(doc, path, definition)),
                doc.newline().append(doc.newline()),
            ),
        )
        .append(doc.newline())
}

fn lower_definition<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    definition: &'a Definition<String>,
) -> DocBuilder<'doc, A> {
    match *definition {
        Definition::Alias(ref ty) => lower_alias(doc, path, ty),
        Definition::Struct(ref fields, ref parse_expr) => match *parse_expr {
            None => lower_struct(doc, path, fields),
            Some(ref parse_expr) => lower_struct(doc, path, fields)
                .append(doc.newline())
                .append(doc.newline())
                .append(lower_read_impl(doc, path, parse_expr)),
        },
        Definition::Union(ref variants, ref parse_expr) => match *parse_expr {
            None => lower_union(doc, path, variants),
            Some(ref parse_expr) => lower_union(doc, path, variants)
                .append(doc.newline())
                .append(doc.newline())
                .append(lower_read_impl(doc, path, parse_expr)),
        },
    }
}

fn lower_alias<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    ty: &'a Type<String>,
) -> DocBuilder<'doc, A> {
    doc.text("pub type")
        .append(doc.space())
        .append(doc.as_string(path))
        .append(doc.space())
        .append(doc.text("="))
        .append(doc.space())
        .append(lower_ty(doc, ty))
        .append(doc.text(";"))
        .group()
}

fn lower_struct<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    fields: &'a [Field<String, RcType<String>>],
) -> DocBuilder<'doc, A> {
    doc.text("pub struct")
        .append(doc.space())
        .append(doc.as_string(path))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(doc.intersperse(
                    fields.iter().map(|field| {
                        doc.as_string(&field.name)
                            .append(doc.text(":"))
                            .append(doc.space())
                            .append(lower_ty(doc, &field.value))
                            .append(doc.text(","))
                            .group()
                    }),
                    doc.newline(),
                ))
                .nest(INDENT_WIDTH)
                .append(doc.newline()),
        )
        .append(doc.text("}"))
}

fn lower_union<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    variants: &'a [Field<String, RcType<String>>],
) -> DocBuilder<'doc, A> {
    doc.text("pub enum")
        .append(doc.space())
        .append(doc.as_string(path))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(doc.intersperse(
                    variants.iter().map(|variant| {
                        // FIXME: Case conversion
                        doc.as_string(&variant.name)
                            .append(doc.text("("))
                            .append(lower_ty(doc, &variant.value))
                            .append(doc.text("),"))
                            .group()
                    }),
                    doc.newline(),
                ))
                .nest(INDENT_WIDTH)
                .append(doc.newline()),
        )
        .append(doc.text("}"))
}

fn lower_read_impl<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    parse_expr: &'a ParseExpr<String>,
) -> DocBuilder<'doc, A> {
    doc.text("impl")
        .append(doc.space())
        .append(doc.as_string(path))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(
                    doc.text("fn read<R: Read>(reader: &mut R) -> io::Result<")
                        .append(doc.as_string(path))
                        .append(doc.text(">"))
                        .append(doc.space())
                        .append(doc.text("{"))
                        .group(),
                )
                .append(
                    doc.newline()
                        .append(lower_parse_expr(doc, parse_expr))
                        .nest(INDENT_WIDTH),
                )
                .append(doc.newline())
                .append(doc.text("}"))
                .nest(INDENT_WIDTH)
                .append(doc.newline()),
        )
        .append(doc.text("}"))
}

fn lower_ty<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    ty: &'a Type<String>,
) -> DocBuilder<'doc, A> {
    match *ty {
        Type::Path(ref path) => doc.as_string(path),
        Type::Array(ref ty) => doc.text("Vec<")
            .append(lower_ty(doc, ty))
            .append(doc.text(">"))
            .group(),
        // FIXME: Implement this!
        Type::Arrow(_, _) => unimplemented!(),
        Type::U8 => doc.text("u8"),
        Type::Int => doc.text("i64"),
        Type::Bool => doc.text("bool"),
    }
}

fn lower_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    parse_expr: &'a ParseExpr<String>,
) -> DocBuilder<'doc, A> {
    match *parse_expr {
        // FIXME: Implement this!
        ParseExpr::Var(_) => doc.as_string("unimplemented!()"),
        ParseExpr::U8 => doc.as_string("buf.read_u8()?"),
        ParseExpr::Ident(ref name) => doc.as_string(name).append(doc.as_string("::read(buf)?")),
        ParseExpr::Repeat(ref parse_expr, RepeatBound::Exact(ref size_expr)) => doc.text("{")
            .append(
                doc.space()
                    .append(doc.text("(0.."))
                    .append(lower_expr(doc, size_expr))
                    .append(doc.text(").map(|_| "))
                    .append(lower_parse_expr(doc, parse_expr))
                    .append(doc.text(").collect::<Result<_, _>>()?")),
            )
            .append(doc.space())
            .append(doc.text("}")),
        ParseExpr::Assert(ref parse_expr, ref pred_expr) => doc.text("{")
            .append(
                doc.newline()
                    .append(
                        // FIXME: Hygiene!
                        doc.text("let __value = ")
                            .append(lower_parse_expr(doc, parse_expr))
                            .append(doc.text("?;"))
                            .group(),
                    )
                    .append(doc.newline())
                    .append(
                        doc.text("if !(")
                            .append(lower_expr(doc, pred_expr))
                            .append(doc.text(")(__value) {"))
                            .group(),
                    )
                    .append(
                        doc.newline()
                            .append(doc.text(
                                r#"return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid binary data"));"#,
                            ))
                            .nest(INDENT_WIDTH),
                    )
                    .append(doc.newline())
                    .append(doc.text("}"))
                    .append(doc.newline())
                    .append(doc.text("__value")),
            )
            .append(doc.newline())
            .append(doc.text("}")),
        // FIXME: Implement this!
        ParseExpr::Sequence(_, _) => doc.as_string("unimplemented!()"),
        // FIXME: Implement this!
        ParseExpr::Choice(_) => doc.as_string("unimplemented!()"),
    }
}

fn lower_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    expr: &'a Expr<String>,
) -> DocBuilder<'doc, A> {
    match *expr {
        Expr::Const(Const::Bool(value)) => doc.as_string(value),
        Expr::Const(Const::U8(value)) => doc.as_string(value),
        Expr::Const(Const::Int(value)) => doc.as_string(value),
        // FIXME: Implement this!
        _ => doc.as_string("unimplemented!()"),
    }
}
