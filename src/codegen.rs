use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use std::fmt;

use name::Named;
use ir::ast::{Definition, Expr, Field, ParseExpr, Path, Program, RepeatBound, Type};
use ir::ast::{RcParseExpr, RcType};
use ir::ast::{Binop, Const, Unop};
use ir::ast::{BinaryTypeConst, IntSuffix, TypeConst};
use ir::ast::{FloatType, SignedType, UnsignedType};
use var::Var;

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
        .append(doc.text("extern crate ddl_util;").append(doc.newline()))
        .append(doc.newline())
        .append(doc.text("use std::io;").append(doc.newline()))
        .append(doc.text("use std::io::prelude::*;").append(doc.newline()))
        .append(doc.newline())
        .append({
            let defs = program.defs.iter();
            doc.intersperse(
                defs.map(|(path, definition)| lower_definition(doc, path, definition)),
                doc.newline().append(doc.newline()),
            )
        })
        .append(doc.newline())
}

fn lower_definition<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    definition: &'a Definition<String>,
) -> DocBuilder<'doc, A> {
    match *definition {
        Definition::Alias(ref comment, ref ty) => {
            lower_doc_comment(doc, comment).append(lower_alias(doc, path, ty))
        }
        Definition::Struct(ref comment, ref fields, ref parse_expr) => match *parse_expr {
            None => lower_doc_comment(doc, comment).append(lower_struct(doc, path, fields)),
            Some(ref parse_expr) => lower_doc_comment(doc, comment)
                .append(lower_struct(doc, path, fields))
                .append(doc.newline())
                .append(doc.newline())
                .append(lower_read_impl(doc, path, parse_expr)),
        },
        Definition::Union(ref comment, ref variants, ref parse_expr) => match *parse_expr {
            None => lower_doc_comment(doc, comment).append(lower_union(doc, path, variants)),
            Some(ref parse_expr) => lower_doc_comment(doc, comment)
                .append(lower_union(doc, path, variants))
                .append(doc.newline())
                .append(doc.newline())
                .append(lower_read_impl(doc, path, parse_expr)),
        },
    }
}

fn lower_doc_comment<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    comment: &'a str,
) -> DocBuilder<'doc, A> {
    if comment.is_empty() {
        doc.nil()
    } else {
        doc.concat(comment.lines().map(|line| match line {
            "" => doc.text("///").append(doc.newline()),
            _ => doc.text(format!("/// {}", line)).append(doc.newline()),
        }))
    }
}

fn lower_alias<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    path: &'a Path<String>,
    ty: &'a Type<String>,
) -> DocBuilder<'doc, A> {
    doc.text("pub type")
        .append(doc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(doc.text(path.to_camel_case()))
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
    doc.text("#[derive(Debug, Clone)]")
        .append(doc.newline())
        .append(doc.text("pub struct"))
        .append(doc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(doc.text(path.to_camel_case()))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(doc.intersperse(
                    fields.iter().map(|field| {
                        lower_doc_comment(doc, &field.doc)
                            .append(doc.text("pub"))
                            .append(doc.space())
                            .append(doc.as_string(&field.name))
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
    use heck::CamelCase;

    doc.text("pub enum")
        .append(doc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(doc.text(path.to_camel_case()))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(doc.intersperse(
                    variants.iter().map(|variant| {
                        lower_doc_comment(doc, &variant.doc)
                            // FIXME: this will break if there is already another
                            // variant in the enum that uses the pascalised identifier
                            .append(doc.text(variant.name.to_camel_case()))
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
        .append(doc.text(path.to_camel_case()))
        .append(doc.space())
        .append(doc.text("{"))
        .group()
        .append(
            doc.newline()
                .append(
                    doc.text("pub fn read<R: Read>(reader: &mut R) -> io::Result<")
                        .append(doc.text(path.to_camel_case()))
                        .append(doc.text(">"))
                        .append(doc.space())
                        .append(doc.text("{"))
                        .group(),
                )
                .append(
                    doc.newline()
                        .append(lower_parse_expr(doc, Prec::Block, parse_expr))
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
        Type::Path(ref path) => doc.text(path.to_camel_case()),
        Type::Array(ref ty) => doc.text("Vec<")
            .append(lower_ty(doc, ty))
            .append(doc.text(">"))
            .group(),
        // FIXME: Implement this!
        Type::Arrow(_, _) => unimplemented!(),
        Type::Const(ty_const) => lower_ty_const(doc, ty_const),
    }
}

fn lower_float_ty(ty: FloatType) -> &'static str {
    match ty {
        FloatType::F32 => "f32",
        FloatType::F64 => "f64",
    }
}

fn lower_signed_ty(ty: SignedType) -> &'static str {
    match ty {
        SignedType::I8 => "i8",
        SignedType::I16 => "i16",
        SignedType::I24 => "i32",
        SignedType::I32 => "i32",
        SignedType::I64 => "i64",
    }
}

fn lower_unsigned_ty(ty: UnsignedType) -> &'static str {
    match ty {
        UnsignedType::U8 => "u8",
        UnsignedType::U16 => "u16",
        UnsignedType::U24 => "u32",
        UnsignedType::U32 => "u32",
        UnsignedType::U64 => "u64",
    }
}

fn lower_ty_const<'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    ty_const: TypeConst,
) -> DocBuilder<'doc, A> {
    match ty_const {
        TypeConst::Bool => doc.text("bool"),
        TypeConst::Float(ty) => doc.text(lower_float_ty(ty)),
        TypeConst::Signed(ty) => doc.text(lower_signed_ty(ty)),
        TypeConst::Unsigned(ty) => doc.text(lower_unsigned_ty(ty)),
    }
}

#[derive(Copy, Clone, Debug)]
enum Prec {
    Block,
    Expr,
}

fn lower_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    prec: Prec,
    parse_expr: &'a ParseExpr<String>,
) -> DocBuilder<'doc, A> {
    match *parse_expr {
        ParseExpr::Var(Var::Free(_)) => unimplemented!(),
        ParseExpr::Var(Var::Bound(Named(ref name, _))) => lower_named_parse_expr(doc, name),
        ParseExpr::Const(ty_const) => lower_parse_ty_const(doc, ty_const),
        ParseExpr::Repeat(ref parse_expr, ref repeat_bound) => {
            lower_repeat_parse_expr(doc, prec, parse_expr, repeat_bound)
        }
        ParseExpr::Assert(ref parse_expr, ref pred) => {
            lower_assert_parse_expr(doc, prec, parse_expr, pred)
        }
        ParseExpr::Sequence(ref parse_exprs, ref expr) => {
            lower_sequence_parse_expr(doc, prec, parse_exprs, expr)
        }
        ParseExpr::Choice(ref parse_exprs) => lower_choice_parse_expr(doc, parse_exprs),
        ParseExpr::Compute(ref expr) => doc.text("Ok::<_, io::Error>(")
            .append(lower_expr(doc, expr))
            .append(")"),
        ParseExpr::Apply(ref fn_expr, ref parse_expr) => lower_expr(doc, fn_expr)
            .append(doc.text("("))
            .append(lower_parse_expr(doc, Prec::Expr, parse_expr))
            .append(doc.text(")")),
    }
}

fn lower_named_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    name: &'a str,
) -> DocBuilder<'doc, A> {
    doc.as_string(name).append(doc.as_string("::read(reader)"))
}

fn lower_parse_ty_const<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    ty_const: BinaryTypeConst,
) -> DocBuilder<'doc, A> {
    doc.text(match ty_const {
        BinaryTypeConst::U8 => "ddl_util::from_u8(reader)",
        BinaryTypeConst::I8 => "ddl_util::from_i8(reader)",
        BinaryTypeConst::U16Le => "ddl_util::from_u16le(reader)",
        BinaryTypeConst::U24Le => "ddl_util::from_u24le(reader)",
        BinaryTypeConst::U32Le => "ddl_util::from_u32le(reader)",
        BinaryTypeConst::U64Le => "ddl_util::from_u64le(reader)",
        BinaryTypeConst::I16Le => "ddl_util::from_i16le(reader)",
        BinaryTypeConst::I24Le => "ddl_util::from_i24le(reader)",
        BinaryTypeConst::I32Le => "ddl_util::from_i32le(reader)",
        BinaryTypeConst::I64Le => "ddl_util::from_i64le(reader)",
        BinaryTypeConst::F32Le => "ddl_util::from_f32le(reader)",
        BinaryTypeConst::F64Le => "ddl_util::from_f64le(reader)",
        BinaryTypeConst::U16Be => "ddl_util::from_u16be(reader)",
        BinaryTypeConst::U24Be => "ddl_util::from_u24be(reader)",
        BinaryTypeConst::U32Be => "ddl_util::from_u32be(reader)",
        BinaryTypeConst::U64Be => "ddl_util::from_u64be(reader)",
        BinaryTypeConst::I16Be => "ddl_util::from_i16be(reader)",
        BinaryTypeConst::I24Be => "ddl_util::from_i24be(reader)",
        BinaryTypeConst::I32Be => "ddl_util::from_i32be(reader)",
        BinaryTypeConst::I64Be => "ddl_util::from_i64be(reader)",
        BinaryTypeConst::F32Be => "ddl_util::from_f32be(reader)",
        BinaryTypeConst::F64Be => "ddl_util::from_f64be(reader)",
    })
}

fn lower_repeat_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    prec: Prec,
    parse_expr: &'a ParseExpr<String>,
    repeat_bound: &'a RepeatBound<String>,
) -> DocBuilder<'doc, A> {
    match *repeat_bound {
        RepeatBound::Exact(ref size_expr) => {
            let inner_parser = doc.text("(0..")
                .append(lower_expr(doc, size_expr))
                .append(doc.text(")"))
                .group()
                .append(
                    doc.text(".map(|_| ")
                        .append(lower_parse_expr(doc, Prec::Expr, parse_expr))
                        .append(doc.text(")"))
                        .group(),
                )
                .append(doc.text(".collect::<Result<_, _>>()"));

            match prec {
                Prec::Block | Prec::Expr => inner_parser,
            }
        }
    }
}

fn lower_assert_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    prec: Prec,
    parse_expr: &'a ParseExpr<String>,
    pred_expr: &'a Expr<String>,
) -> DocBuilder<'doc, A> {
    let inner_parser = doc.newline()
        .append(
            // FIXME: Hygiene!
            doc.text("let __value = ")
                .append(lower_parse_expr(doc, prec, parse_expr))
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
                .append(
                    doc.text("return")
                        .append(doc.space())
                        .append(invalid_data_err(doc))
                        .group(),
                )
                .nest(INDENT_WIDTH),
        )
        .append(doc.newline())
        .append(doc.text("}"))
        .append(doc.newline())
        .append(doc.text("Ok::<_, io::Error>(__value)"));

    match prec {
        Prec::Block => inner_parser,
        Prec::Expr => doc.text("{")
            .append(inner_parser.nest(INDENT_WIDTH))
            .append(doc.newline())
            .append(doc.text("}")),
    }
}

fn lower_sequence_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    prec: Prec,
    parse_exprs: &'a [Named<String, RcParseExpr<String>>],
    expr: &'a Expr<String>,
) -> DocBuilder<'doc, A> {
    let inner_parser = doc.concat(parse_exprs.iter().map(|&Named(ref name, ref parse_expr)| {
        doc.text("let")
            .append(doc.space())
            .append(doc.as_string(name))
            .append(doc.space())
            .append(doc.text("="))
            .group()
            .append(doc.space())
            .append(lower_parse_expr(doc, Prec::Expr, parse_expr))
            .append(doc.text("?;"))
            .group()
            .append(doc.newline())
    })).append(
            doc.text("Ok::<_, io::Error>(")
                .append(lower_expr(doc, expr))
                .append(")"),
        );

    match prec {
        Prec::Block => inner_parser,
        Prec::Expr => doc.text("{")
            .append(doc.newline().append(inner_parser).nest(INDENT_WIDTH))
            .append(doc.newline())
            .append(doc.text("}")),
    }
}

fn lower_choice_parse_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    parse_exprs: &'a [RcParseExpr<String>],
) -> DocBuilder<'doc, A> {
    if parse_exprs.is_empty() {
        invalid_data_err(doc)
    } else {
        doc.intersperse(
            parse_exprs.iter().map(|parse_expr| {
                doc.text("if let Ok(__value) =")
                    .append(doc.space())
                    .append(lower_parse_expr(doc, Prec::Expr, parse_expr))
                    .append(doc.text("{"))
                    .append(doc.newline().append(doc.text("__value").nest(INDENT_WIDTH)))
                    .append(doc.newline())
                    .append(doc.text("}"))
            }),
            doc.space().append(doc.text("else")).append(doc.space()),
        ).append(
                doc.space()
                    .append(doc.text("else"))
                    .append(doc.space())
                    .append(doc.text("{"))
                    .append(
                        doc.newline()
                            .append(invalid_data_err(doc))
                            .nest(INDENT_WIDTH),
                    )
                    .append(doc.newline())
                    .append(doc.text("}")),
            )
    }
}

fn lower_expr<'doc, 'a: 'doc, A: DocAllocator<'doc>>(
    doc: &'doc A,
    expr: &'a Expr<String>,
) -> DocBuilder<'doc, A> {
    match *expr {
        // FIXME: Hygiene!
        Expr::Const(Const::Bool(value)) => doc.as_string(value),
        Expr::Const(Const::Int(value, IntSuffix::Signed(suffix))) => doc.as_string(value)
            .append(doc.text(lower_signed_ty(suffix))),
        Expr::Const(Const::Int(value, IntSuffix::Unsigned(suffix))) => doc.as_string(value)
            .append(doc.text(lower_unsigned_ty(suffix))),
        Expr::Const(Const::Float(value, suffix)) => doc.as_string(value)
            .append(doc.text(lower_float_ty(suffix))),
        // FXIME: Hygiene!
        Expr::Prim(name, _) => doc.text("ddl_util::").append(doc.as_string(name)),
        Expr::Var(Var::Free(_)) => unimplemented!(),
        Expr::Var(Var::Bound(Named(ref name, _))) => doc.as_string(name),
        Expr::Unop(op, ref expr) => {
            let op_str = match op {
                Unop::Neg => "-",
                Unop::Not => "!",
            };

            // Let's not worry about being pretty with operator precedence here
            // Just chuck redundant parens around all the things... ¯\_(ツ)_/¯
            doc.text("(")
                .append(doc.text(op_str))
                .append(lower_expr(doc, expr))
                .append(doc.text(")"))
                .group()
        }
        Expr::Binop(op, ref lhs, ref rhs) => {
            let op_str = match op {
                Binop::Or => "||",
                Binop::And => "&",
                Binop::Eq => "==",
                Binop::Ne => "!=",
                Binop::Le => "<=",
                Binop::Lt => "<",
                Binop::Gt => ">",
                Binop::Ge => ">=",
                Binop::Add => "+",
                Binop::Sub => "-",
                Binop::Mul => "*",
                Binop::Div => "/",
            };

            // Let's not worry about being pretty with operator precedence here
            // Just chuck redundant parens around all the things... ¯\_(ツ)_/¯
            doc.text("(")
                .append(lower_expr(doc, lhs))
                .append(doc.space())
                .append(doc.text(op_str))
                .append(doc.space())
                .append(lower_expr(doc, rhs))
                .append(doc.text(")"))
                .group()
        }
        Expr::Struct(ref path, ref fields) => doc.text(path.to_camel_case())
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
                                .append(lower_expr(doc, &field.value))
                                .append(doc.text(","))
                                .group()
                        }),
                        doc.newline(),
                    ))
                    .nest(INDENT_WIDTH)
                    .append(doc.newline()),
            )
            .append(doc.text("}")),
        Expr::Proj(ref expr, ref field_name) => lower_expr(doc, expr)
            .append(doc.text("."))
            .append(doc.as_string(field_name)),
        Expr::Intro(ref path, ref variant_name, ref expr) => doc.text(path.to_camel_case())
            .append(doc.text("::"))
            .append(doc.as_string(variant_name))
            .append(doc.text("("))
            .append(lower_expr(doc, expr))
            .append(doc.text(")")),
        Expr::Subscript(ref expr, ref index) => lower_expr(doc, expr)
            .append(doc.text("["))
            .append(
                lower_expr(doc, index)
                    .append(doc.space())
                    .append(doc.text("as usize")),
            )
            .append(doc.text("]")),
        Expr::Cast(ref expr, ref ty) => lower_expr(doc, expr)
            .append(doc.space())
            .append(doc.text("as"))
            .append(doc.space())
            .append(lower_ty(doc, ty))
            .group(),
        Expr::Abs(ref params, ref body_expr) => doc.text("|")
            .append(doc.intersperse(
                params.iter().map(|&Named(ref name, ref ty)| {
                    doc.as_string(name)
                        .append(doc.text(":"))
                        .append(doc.space())
                        .append(lower_ty(doc, ty))
                }),
                doc.text(","),
            ))
            .append(doc.text("|"))
            .append(doc.space())
            .append(lower_expr(doc, body_expr).nest(INDENT_WIDTH))
            .group(),
        Expr::App(ref fn_expr, ref arg_exprs) => {
            let arg_exprs = arg_exprs.iter().map(|arg_expr| lower_expr(doc, arg_expr));

            lower_expr(doc, fn_expr)
                .append(doc.text("("))
                .append(doc.intersperse(arg_exprs, doc.text(",")))
                .append(doc.text(")"))
        }
    }
}

fn invalid_data_err<'doc, A: DocAllocator<'doc>>(doc: &'doc A) -> DocBuilder<'doc, A> {
    doc.text("Err::<_, io::Error>(")
        .append(
            doc.text("io::Error::new(")
                .append(
                    doc.text("io::ErrorKind::InvalidData,")
                        .append(doc.space())
                        .append(doc.text(r#""Invalid binary data""#))
                        .nest(INDENT_WIDTH),
                )
                .nest(INDENT_WIDTH)
                .append(doc.text(")")),
        )
        .append(doc.text(")"))
        .group()
}
