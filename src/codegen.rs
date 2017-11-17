use std::fmt;

use ir::owned::ast::{Definition, Expr, ParseExpr, Program, RepeatBound, Type};
use ir::owned::ast::{Binop, Const, Unop};

pub struct LowerProgram<'a>(pub &'a Program<String>);

impl<'a> fmt::Display for LowerProgram<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "use std::io;")?;
        writeln!(f, "use std::io::prelude::*;")?;
        writeln!(f)?;

        for (path, definition) in &self.0.defs {
            match *definition {
                Definition::Alias(ref ty) => {
                    writeln!(f, "pub type {} = {};", path, LowerType(ty))?;
                    writeln!(f)?;
                }
                Definition::Struct(ref fields, ref parse_expr) => {
                    writeln!(f, "pub struct {} {{", path)?;
                    for field in fields {
                        writeln!(f, "    {}: {},", field.name, LowerType(&field.value))?;
                    }
                    writeln!(f, "}}")?;
                    writeln!(f)?;

                    if let Some(ref parse_expr) = *parse_expr {
                        writeln!(f, "impl {} {{", path)?;
                        writeln!(
                            f,
                            "    fn read<R: Read>(reader: &mut R) -> io::Result<{}> {{",
                            path
                        )?;
                        writeln!(f, "{}", LowerParseExpr(parse_expr, 2))?;
                        writeln!(f, "    }}")?;
                        writeln!(f, "}}")?;
                        writeln!(f)?;
                    }
                }
                Definition::Union(ref variants, ref parse_expr) => {
                    writeln!(f, "pub enum {} {{", path)?;
                    for variant in variants {
                        writeln!(f, "    {}({}),", variant.name, LowerType(&variant.value))?;
                    }
                    writeln!(f, "}}")?;
                    writeln!(f)?;

                    if let Some(ref parse_expr) = *parse_expr {
                        writeln!(f, "impl {} {{", path)?;
                        writeln!(
                            f,
                            "    fn read<R: Read>(reader: &mut R) -> io::Result<{}> {{",
                            path
                        )?;
                        writeln!(f, "{}", LowerParseExpr(parse_expr, 2))?;
                        writeln!(f, "    }}")?;
                        writeln!(f, "}}")?;
                        writeln!(f)?;
                    }
                }
            }
        }
        Ok(())
    }
}

struct LowerType<'a>(pub &'a Type<String>);

impl<'a> fmt::Display for LowerType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            Type::Path(ref path) => write!(f, "{}", path),
            Type::Array(ref ty) => write!(f, "Vec<{}>", LowerType(ty)),
            Type::Arrow(_, _) => unimplemented!(),
            Type::U8 => write!(f, "u8"),
            Type::Int => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

struct LowerParseExpr<'a>(pub &'a ParseExpr<String>, pub usize);

impl<'a> fmt::Display for LowerParseExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let write_indent = |f: &mut fmt::Formatter| {
            for _ in 0..(self.1 * 4) {
                write!(f, " ")?;
            }
            Ok(())
        };

        match *self.0 {
            // ParseExpr::Var() => unimplemented!(),
            ParseExpr::U8 => {
                write_indent(f)?;
                write!(f, "buf.read_u8()?")?;
            },
            ParseExpr::Ident(ref name) => {
                write_indent(f)?;
                write!(f, "{}::read(buf)?", name)?;
            },
            ParseExpr::Repeat(ref parse_expr, RepeatBound::Exact(ref size_expr)) => {
                write_indent(f)?;
                writeln!(f, "{{")?;
                write_indent(f)?;
                writeln!(f, "    (0..{})", LowerExpr(size_expr))?;
                write_indent(f)?;
                writeln!(
                    f,
                    "        .map(|_| {})",
                    LowerParseExpr(parse_expr, self.1 + 3)
                )?;
                write_indent(f)?;
                writeln!(f, "        .collect::<Result<_, _>>()?")?;
                write_indent(f)?;
                writeln!(f, "}}")?;
            }
            ParseExpr::Assert(ref parse_expr, ref pred_expr) => {
                write_indent(f)?;
                writeln!(f, "{{")?;
                write_indent(f)?;
                writeln!(f, "    let __value = {}?;", LowerParseExpr(parse_expr, self.1 + 1))?;
                write_indent(f)?;
                writeln!(f, "    if !({})(__value) {{", LowerExpr(pred_expr))?;
                write_indent(f)?;
                writeln!(f, "         return Err(io::Error::new(io::ErrorKind::InvalidData, \"Invalid binary data\"));")?;
                write_indent(f)?;
                writeln!(f, "    }}")?;
                write_indent(f)?;
                writeln!(f, "    __value")?;
                write_indent(f)?;
                writeln!(f, "}}")?;
            },
            // ParseExpr::Sequence() => unimplemented!(),
            // ParseExpr::Choice() => unimplemented!(),
            _ => {
                write_indent(f)?;
                write!(f, "unimplemented!()")?
            }
        }
        Ok(())
    }
}

struct LowerExpr<'a>(pub &'a Expr<String>);

impl<'a> fmt::Display for LowerExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            Expr::Const(Const::Bool(value)) => write!(f, "{}", value),
            Expr::Const(Const::U8(value)) => write!(f, "{}", value),
            Expr::Const(Const::Int(value)) => write!(f, "{}", value),
            _ => write!(f, "unimplemented!()"),
        }
    }
}
