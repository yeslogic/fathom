use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use std::fmt;

use heck::CamelCase;
use name::{Ident, Name, Named};
use ir::ast::{Definition, Expr, Field, Item, Module, ParseExpr, Path, RepeatBound, Type};
use ir::ast::{RcExpr, RcParseExpr, RcType};
use ir::ast::{Binop, Const, Unop};
use ir::ast::{BinaryTypeConst, HostTypeConst, IntSuffix};
use ir::ast::{FloatType, SignedType, UnsignedType};
use var::Var;

pub struct LowerModule<'a>(pub &'a Module);

impl<'a> fmt::Display for LowerModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let DocBuilder(_, ref alloc) = lower_module(&BoxAllocator, self.0);
        alloc.render_fmt(f.width().unwrap_or(MAX_WIDTH), f)
    }
}

const INDENT_WIDTH: usize = 4;
const MAX_WIDTH: usize = 100;

fn lower_module<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    module: &'a Module,
) -> DocBuilder<'alloc, A> {
    let version_comment = format!(
        "// auto-generated: \"{} {}\"",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    );

    alloc
        .nil()
        .append(alloc.text(version_comment).append(alloc.newline()))
        .append(alloc.newline())
        .append(alloc.text("extern crate ddl_util;").append(alloc.newline()))
        .append(alloc.newline())
        .append(lower_import(alloc, "self::ddl_util::FromBinary"))
        .append(lower_import(alloc, "std::io"))
        .append(lower_import(alloc, "std::io::prelude::*"))
        .append(alloc.newline())
        .append({
            let defs = module.definitions.iter();
            alloc.intersperse(
                defs.map(|definition| lower_definition(alloc, definition)),
                alloc.newline().append(alloc.newline()),
            )
        })
        .append(alloc.newline())
}

fn lower_import<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    import: &'a str,
) -> DocBuilder<'alloc, A> {
    alloc
        .text("use")
        .append(alloc.space())
        .append(alloc.text(import))
        .append(alloc.text(";"))
        .group()
        .append(alloc.newline())
}

fn lower_definition<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    definition: &'a Definition,
) -> DocBuilder<'alloc, A> {
    lower_doc_comment(alloc, &definition.doc).append(match definition.item {
        Item::Alias(ref ty) => lower_alias(alloc, &definition.path, &definition.params, ty),
        Item::Struct(ref fields, ref parse_expr) => {
            let item = lower_struct(alloc, &definition.path, &definition.params, fields);

            match *parse_expr {
                None => item,
                Some(ref parse_expr) => item.append(alloc.newline())
                    .append(alloc.newline())
                    .append(lower_from_binary_impl(
                        alloc,
                        &definition.path,
                        &definition.params,
                        parse_expr,
                    )),
            }
        }
        Item::Union(ref variants, ref parse_expr) => {
            let item = lower_union(alloc, &definition.path, &definition.params, variants);

            match *parse_expr {
                None => item,
                Some(ref parse_expr) => item.append(alloc.newline())
                    .append(alloc.newline())
                    .append(lower_from_binary_impl(
                        alloc,
                        &definition.path,
                        &definition.params,
                        parse_expr,
                    )),
            }
        }
    })
}

fn lower_doc_comment<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    doc: &'a str,
) -> DocBuilder<'alloc, A> {
    if doc.is_empty() {
        alloc.nil()
    } else {
        alloc.concat(doc.lines().map(|line| match line {
            "" => alloc.text("///").append(alloc.newline()),
            _ => alloc.text(format!("/// {}", line)).append(alloc.newline()),
        }))
    }
}

fn lower_alias<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    path: &'a Path,
    params: &'a [Name],
    ty: &'a RcType,
) -> DocBuilder<'alloc, A> {
    alloc.text("pub type")
        .append(alloc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(alloc.text(path.to_camel_case()))
        .append(lower_intro_ty_params(alloc, params))
        .append(alloc.space())
        .append(alloc.text("="))
        .append(alloc.space())
        .append(lower_ty(alloc, ty))
        .append(alloc.text(";"))
        .group()
}

fn lower_struct<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    path: &'a Path,
    params: &'a [Name],
    fields: &'a [Field<RcType>],
) -> DocBuilder<'alloc, A> {
    alloc.text("#[derive(Debug, Clone)]")
        .append(alloc.newline())
        .append(alloc.text("pub struct"))
        .append(alloc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(alloc.text(path.to_camel_case()))
        .append(lower_intro_ty_params(alloc, params))
        .append(alloc.space())
        .append(alloc.text("{"))
        .group()
        .append(
            alloc.newline()
                .append(alloc.intersperse(
                    fields.iter().map(|field| {
                        lower_doc_comment(alloc, &field.doc)
                            .append(alloc.text("pub"))
                            .append(alloc.space())
                            .append(alloc.as_string(&field.name))
                            .append(alloc.text(":"))
                            .append(alloc.space())
                            .append(lower_ty(alloc, &field.value))
                            .append(alloc.text(","))
                            .group()
                    }),
                    alloc.newline(),
                ))
                .nest(INDENT_WIDTH)
                .append(alloc.newline()),
        )
        .append(alloc.text("}"))
}

fn lower_union<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    path: &'a Path,
    params: &'a [Name],
    variants: &'a [Field<RcType>],
) -> DocBuilder<'alloc, A> {
    use heck::CamelCase;

    alloc.text("#[derive(Debug, Clone)]")
        .append(alloc.newline())
        .append(alloc.text("pub enum"))
        .append(alloc.space())
        // FIXME: this will break if there is already a definition in scope
        // that uses the pascalised identifier
        .append(alloc.text(path.to_camel_case()))
        .append(lower_intro_ty_params(alloc, params))
        .append(alloc.space())
        .append(alloc.text("{"))
        .group()
        .append(
            alloc.newline()
                .append(alloc.intersperse(
                    variants.iter().map(|variant| {
                        lower_doc_comment(alloc, &variant.doc)
                            // FIXME: this will break if there is already another
                            // variant in the enum that uses the pascalised identifier
                            .append(alloc.text(variant.name.0.to_camel_case()))
                            .append(alloc.text("("))
                            .append(lower_ty(alloc, &variant.value))
                            .append(alloc.text("),"))
                            .group()
                    }),
                    alloc.newline(),
                ))
                .nest(INDENT_WIDTH)
                .append(alloc.newline()),
        )
        .append(alloc.text("}"))
}

fn lower_from_binary_impl<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    path: &'a Path,
    params: &'a [Name],
    parse_expr: &'a RcParseExpr,
) -> DocBuilder<'alloc, A> {
    let base_header = alloc
        .text("impl")
        .append(lower_intro_ty_params(alloc, params))
        .append(alloc.space())
        .append(alloc.text("FromBinary"))
        .append(alloc.space())
        .append(alloc.text("for"))
        .append(alloc.space())
        .append(alloc.text(path.to_camel_case()))
        .append(lower_intro_ty_params(alloc, params))
        .append(alloc.space());

    let header = if params.is_empty() {
        base_header.append(alloc.text("{")).group()
    } else {
        base_header
            .append(alloc.text("where"))
            .group()
            .append(alloc.newline())
            .append(alloc.concat(params.iter().map(|param| {
                alloc
                    .as_string(param)
                    .append(alloc.text(":"))
                    .append(alloc.space())
                    .append(alloc.text("FromBinary,"))
                    .group()
                    .append(alloc.newline())
            })))
            .append(alloc.text("{"))
    };

    header
        .append(
            alloc
                .newline()
                .append(
                    alloc
                        .text("fn from_binary<R: Read>(reader: &mut R) -> io::Result<")
                        .append(alloc.text(path.to_camel_case()))
                        .append(lower_intro_ty_params(alloc, params))
                        .append(alloc.text(">"))
                        .append(alloc.space())
                        .append(alloc.text("{"))
                        .group(),
                )
                .append(
                    alloc
                        .newline()
                        .append(lower_parse_expr(alloc, Prec::Block, parse_expr))
                        .nest(INDENT_WIDTH),
                )
                .append(alloc.newline())
                .append(alloc.text("}"))
                .nest(INDENT_WIDTH)
                .append(alloc.newline()),
        )
        .append(alloc.text("}"))
}

fn lower_intro_ty_params<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    params: &'a [Name],
) -> DocBuilder<'alloc, A> {
    if params.is_empty() {
        alloc.nil()
    } else {
        alloc
            .text("<")
            .append(alloc.intersperse(
                params.iter().map(|param| alloc.as_string(param)),
                alloc.text(",").append(alloc.space()),
            ))
            .append(alloc.text(">"))
    }
}

fn lower_ty<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    ty: &'a RcType,
) -> DocBuilder<'alloc, A> {
    match *ty.inner {
        Type::Path(ref path, ref args) if args.is_empty() => alloc.text(path.to_camel_case()),
        Type::Path(ref path, ref args) => alloc
            .text(path.to_camel_case())
            .append(alloc.text("<"))
            .append(alloc.intersperse(
                args.iter().map(|arg| lower_ty(alloc, arg)),
                alloc.text(",").append(alloc.space()),
            ))
            .append(alloc.text(">")),
        Type::Array(ref ty) => alloc
            .text("Vec<")
            .append(lower_ty(alloc, ty))
            .append(alloc.text(">"))
            .group(),
        // FIXME: Implement this!
        Type::Arrow(_, _) => unimplemented!(),
        Type::Const(ty_const) => lower_ty_const(alloc, ty_const),
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

fn lower_ty_const<'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    ty_const: HostTypeConst,
) -> DocBuilder<'alloc, A> {
    match ty_const {
        HostTypeConst::Unit => alloc.text("()"),
        HostTypeConst::Bottom => alloc.text("ddl_util::Never"),
        HostTypeConst::Bool => alloc.text("bool"),
        HostTypeConst::Float(ty) => alloc.text(lower_float_ty(ty)),
        HostTypeConst::Signed(ty) => alloc.text(lower_signed_ty(ty)),
        HostTypeConst::Unsigned(ty) => alloc.text(lower_unsigned_ty(ty)),
    }
}

#[derive(Copy, Clone, Debug)]
enum Prec {
    Block,
    Expr,
}

fn lower_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    prec: Prec,
    parse_expr: &'a RcParseExpr,
) -> DocBuilder<'alloc, A> {
    match *parse_expr.inner {
        ParseExpr::Var(Var::Free(_)) => unimplemented!(),
        ParseExpr::Var(Var::Bound(Named(ref name, _))) => lower_named_parse_expr(alloc, name),
        ParseExpr::Const(ty_const) => lower_parse_ty_const(alloc, ty_const),
        ParseExpr::Repeat(ref parse_expr, ref repeat_bound) => {
            lower_repeat_parse_expr(alloc, prec, parse_expr, repeat_bound)
        }
        ParseExpr::Assert(ref parse_expr, ref pred) => {
            lower_assert_parse_expr(alloc, prec, parse_expr, pred)
        }
        ParseExpr::Sequence(ref parse_exprs, ref expr) => {
            lower_sequence_parse_expr(alloc, prec, parse_exprs, expr)
        }
        ParseExpr::Cond(ref options) => lower_cond_parse_expr(alloc, options),
        ParseExpr::Apply(ref fn_expr, ref parse_expr) => alloc
            .text("Ok::<_, io::Error>(")
            .append(
                alloc
                    .newline()
                    .append(alloc.text("("))
                    .append(lower_expr(alloc, Prec::Block, fn_expr))
                    .append(alloc.text(")"))
                    .append(alloc.text("("))
                    .append(lower_parse_expr(alloc, Prec::Expr, parse_expr))
                    .append(alloc.text("?)"))
                    .nest(INDENT_WIDTH)
                    .append(alloc.newline()),
            )
            .append(")")
            .group(),
    }
}

fn lower_named_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    name: &'a Name,
) -> DocBuilder<'alloc, A> {
    let name = match *name {
        Name::Abstract => unimplemented!(),
        Name::User(Ident(ref ident)) => ident.to_camel_case(),
    };

    alloc.text(name).append(alloc.text("::from_binary(reader)"))
}

fn lower_parse_ty_const<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    ty_const: BinaryTypeConst,
) -> DocBuilder<'alloc, A> {
    use ir::ast::Endianness as E;

    alloc.text(match ty_const {
        BinaryTypeConst::Empty => "ddl_util::empty()",
        BinaryTypeConst::Error => "ddl_util::error()",
        BinaryTypeConst::U8 => "ddl_util::from_u8(reader)",
        BinaryTypeConst::I8 => "ddl_util::from_i8(reader)",
        BinaryTypeConst::U16(E::Little) => "ddl_util::from_u16le(reader)",
        BinaryTypeConst::U24(E::Little) => "ddl_util::from_u24le(reader)",
        BinaryTypeConst::U32(E::Little) => "ddl_util::from_u32le(reader)",
        BinaryTypeConst::U64(E::Little) => "ddl_util::from_u64le(reader)",
        BinaryTypeConst::I16(E::Little) => "ddl_util::from_i16le(reader)",
        BinaryTypeConst::I24(E::Little) => "ddl_util::from_i24le(reader)",
        BinaryTypeConst::I32(E::Little) => "ddl_util::from_i32le(reader)",
        BinaryTypeConst::I64(E::Little) => "ddl_util::from_i64le(reader)",
        BinaryTypeConst::F32(E::Little) => "ddl_util::from_f32le(reader)",
        BinaryTypeConst::F64(E::Little) => "ddl_util::from_f64le(reader)",
        BinaryTypeConst::U16(E::Big) => "ddl_util::from_u16be(reader)",
        BinaryTypeConst::U24(E::Big) => "ddl_util::from_u24be(reader)",
        BinaryTypeConst::U32(E::Big) => "ddl_util::from_u32be(reader)",
        BinaryTypeConst::U64(E::Big) => "ddl_util::from_u64be(reader)",
        BinaryTypeConst::I16(E::Big) => "ddl_util::from_i16be(reader)",
        BinaryTypeConst::I24(E::Big) => "ddl_util::from_i24be(reader)",
        BinaryTypeConst::I32(E::Big) => "ddl_util::from_i32be(reader)",
        BinaryTypeConst::I64(E::Big) => "ddl_util::from_i64be(reader)",
        BinaryTypeConst::F32(E::Big) => "ddl_util::from_f32be(reader)",
        BinaryTypeConst::F64(E::Big) => "ddl_util::from_f64be(reader)",
    })
}

fn lower_repeat_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    prec: Prec,
    parse_expr: &'a RcParseExpr,
    repeat_bound: &'a RepeatBound,
) -> DocBuilder<'alloc, A> {
    match *repeat_bound {
        RepeatBound::Exact(ref size_expr) => {
            let inner_parser = alloc
                .text("ddl_util::from_array(")
                .append(
                    alloc
                        .text("0..")
                        .append(lower_expr(alloc, Prec::Block, size_expr))
                        .append(alloc.text(","))
                        .group(),
                )
                .append(alloc.space())
                .append(
                    alloc
                        .text("||")
                        .append(alloc.space())
                        .append(lower_parse_expr(alloc, Prec::Expr, parse_expr))
                        .group(),
                )
                .append(alloc.text(")"))
                .group();

            match prec {
                Prec::Block | Prec::Expr => inner_parser,
            }
        }
    }
}

fn lower_assert_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    prec: Prec,
    parse_expr: &'a RcParseExpr,
    pred_expr: &'a RcExpr,
) -> DocBuilder<'alloc, A> {
    let pred = lower_expr(alloc, Prec::Block, pred_expr).append(alloc.text(")(__value)"));
    let if_true = alloc.newline().append(alloc.text("Ok(__value)"));
    let if_false = alloc.newline().append(
        alloc
            .text("return")
            .append(alloc.space())
            .append(alloc.text("ddl_util::error()"))
            .group(),
    );

    lower_parse_expr(alloc, prec, parse_expr).append(
        // FIXME: Hygiene!
        alloc
            .text(".and_then(|__value| {")
            .append(alloc.newline())
            .append(
                alloc
                    .text("if")
                    .append(alloc.space())
                    .append(alloc.text("("))
                    .append(pred)
                    .append(alloc.space())
                    .append(alloc.text("{"))
                    .group()
                    .append(if_true.nest(INDENT_WIDTH))
                    .append(alloc.newline())
                    .append(alloc.text("} else {"))
                    .append(if_false.nest(INDENT_WIDTH))
                    .append(alloc.newline())
                    .append(alloc.text("}"))
                    .nest(INDENT_WIDTH)
                    .append(alloc.newline()),
            )
            .append(alloc.text("})")),
    )
}

fn lower_sequence_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    prec: Prec,
    parse_exprs: &'a [Named<Ident, RcParseExpr>],
    expr: &'a RcExpr,
) -> DocBuilder<'alloc, A> {
    let inner_parser = alloc
        .concat(parse_exprs.iter().map(|&Named(ref name, ref parse_expr)| {
            alloc
                .text("let")
                .append(alloc.space())
                .append(alloc.as_string(name))
                .append(alloc.space())
                .append(alloc.text("="))
                .group()
                .append(alloc.space())
                .append(lower_parse_expr(alloc, Prec::Expr, parse_expr))
                .append(alloc.text("?;"))
                .group()
                .append(alloc.newline())
        }))
        .append(
            alloc
                .text("Ok::<_, io::Error>(")
                .append(lower_expr(alloc, Prec::Block, expr))
                .append(")"),
        );

    match prec {
        Prec::Block => inner_parser,
        Prec::Expr => alloc
            .text("{")
            .append(alloc.newline().append(inner_parser).nest(INDENT_WIDTH))
            .append(alloc.newline())
            .append(alloc.text("}")),
    }
}

fn lower_cond_parse_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    options: &'a [(RcExpr, RcParseExpr)],
) -> DocBuilder<'alloc, A> {
    if options.is_empty() {
        alloc.text("ddl_util::error()")
    } else {
        alloc
            .intersperse(
                options.iter().map(|option| {
                    alloc
                        .text("if")
                        .append(alloc.space())
                        .append(lower_expr(alloc, Prec::Block, &option.0))
                        .append(alloc.space())
                        .append(alloc.text("{"))
                        .group()
                        .append(
                            alloc
                                .newline()
                                .append(lower_parse_expr(alloc, Prec::Block, &option.1))
                                .nest(INDENT_WIDTH),
                        )
                        .append(alloc.newline())
                        .append(alloc.text("}"))
                }),
                alloc
                    .space()
                    .append(alloc.text("else"))
                    .append(alloc.space()),
            )
            .append(
                alloc
                    .space()
                    .append(alloc.text("else"))
                    .append(alloc.space())
                    .append(alloc.text("{"))
                    .group()
                    .append(
                        alloc
                            .newline()
                            .append(alloc.text("ddl_util::error()"))
                            .nest(INDENT_WIDTH),
                    )
                    .append(alloc.newline())
                    .append(alloc.text("}")),
            )
    }
}

fn lower_expr<'alloc, 'a: 'alloc, A: DocAllocator<'alloc>>(
    alloc: &'alloc A,
    prec: Prec,
    expr: &'a RcExpr,
) -> DocBuilder<'alloc, A> {
    let mut is_atomic = false;

    let inner = match *expr.inner {
        Expr::Ann(ref expr, _) => {
            // TODO: use type annotation
            lower_expr(alloc, prec, expr)
        }

        Expr::Const(c) => {
            is_atomic = true;

            match c {
                Const::Bool(value) => alloc.as_string(value),
                Const::Int(value, suffix) => {
                    alloc.as_string(value).append(alloc.text(match suffix {
                        IntSuffix::Signed(suffix) => lower_signed_ty(suffix),
                        IntSuffix::Unsigned(suffix) => lower_unsigned_ty(suffix),
                    }))
                }
                Const::Float(value, suffix) => alloc
                    .as_string(value)
                    .append(alloc.text(lower_float_ty(suffix))),
            }
        }

        // FIXME: Hygiene!
        Expr::Var(Var::Free(_)) => unimplemented!(),
        Expr::Var(Var::Bound(Named(ref name, _))) => {
            is_atomic = true;
            alloc.as_string(name)
        }

        Expr::Lam(ref params, ref body_expr) => alloc
            .text("|")
            .append(alloc.intersperse(
                params.iter().map(|&Named(ref name, ref ty)| {
                    alloc
                        .as_string(name)
                        .append(alloc.text(":"))
                        .append(alloc.space())
                        .append(lower_ty(alloc, ty))
                }),
                alloc.text(","),
            ))
            .append(alloc.text("|"))
            .append(alloc.space())
            .append(lower_expr(alloc, Prec::Block, body_expr).nest(INDENT_WIDTH))
            .group(),

        Expr::App(ref fn_expr, ref arg_exprs) => {
            let arg_exprs = arg_exprs
                .iter()
                .map(|arg_expr| lower_expr(alloc, Prec::Block, arg_expr));

            lower_expr(alloc, Prec::Block, fn_expr)
                .append(alloc.text("("))
                .append(alloc.intersperse(arg_exprs, alloc.text(",")))
                .append(alloc.text(")"))
        }

        Expr::Unop(op, ref expr) => {
            let op_str = match op {
                Unop::Neg => "-",
                Unop::Not => "!",
            };

            // Let's not worry about being pretty with operator precedence here
            // Just chuck redundant parens around all the things... ¯\_(ツ)_/¯
            alloc
                .text(op_str)
                .append(lower_expr(alloc, Prec::Expr, expr))
        }

        Expr::Binop(op, ref lhs, ref rhs) => {
            let op_str = match op {
                Binop::Or => "||",
                Binop::And => "&&",
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
            lower_expr(alloc, Prec::Expr, lhs)
                .append(alloc.space())
                .append(alloc.text(op_str))
                .append(alloc.space())
                .append(lower_expr(alloc, Prec::Expr, rhs))
        }

        Expr::Array(ref elems) => alloc
            .text("[")
            .append(
                alloc.intersperse(
                    elems
                        .iter()
                        .map(|elem| lower_expr(alloc, Prec::Block, elem)),
                    alloc.text(","),
                ),
            )
            .append(alloc.text("]")),

        Expr::Struct(ref path, ref fields) => alloc
            .text(path.to_camel_case())
            .append(alloc.space())
            .append(alloc.text("{"))
            .group()
            .append(
                alloc
                    .newline()
                    .append(alloc.intersperse(
                        fields.iter().map(|field| {
                            alloc
                                .as_string(&field.name)
                                .append(alloc.text(":"))
                                .append(alloc.space())
                                .append(lower_expr(alloc, Prec::Block, &field.value))
                                .append(alloc.text(","))
                                .group()
                        }),
                        alloc.newline(),
                    ))
                    .nest(INDENT_WIDTH)
                    .append(alloc.newline()),
            )
            .append(alloc.text("}")),

        Expr::Proj(ref expr, ref field_name) => {
            is_atomic = true;

            lower_expr(alloc, Prec::Block, expr)
                .append(alloc.text("."))
                .append(alloc.as_string(field_name))
        }

        Expr::Intro(ref path, ref variant_name, ref expr) => alloc
            .text(path.to_camel_case())
            .append(alloc.text("::"))
            .append(alloc.as_string(variant_name.0.to_camel_case()))
            .append(alloc.text("("))
            .append(lower_expr(alloc, Prec::Block, expr))
            .append(alloc.text(")")),

        Expr::Subscript(ref expr, ref index) => lower_expr(alloc, Prec::Block, expr)
            .append(alloc.text("["))
            .append(
                lower_expr(alloc, Prec::Block, index)
                    .append(alloc.space())
                    .append(alloc.text("as usize")),
            )
            .append(alloc.text("]")),

        Expr::Cast(ref expr, ref ty) => {
            is_atomic = true;

            lower_expr(alloc, Prec::Block, expr)
                .append(alloc.space())
                .append(alloc.text("as"))
                .append(alloc.space())
                .append(lower_ty(alloc, ty))
                .group()
        }
    };

    match (is_atomic, prec) {
        (true, _) | (_, Prec::Block) => inner,
        (_, Prec::Expr) => alloc
            .text("(")
            .append(inner)
            .append(alloc.text(")"))
            .group(),
    }
}
