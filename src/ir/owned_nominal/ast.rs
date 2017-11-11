use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use ir::owned;
use ir::owned::ast::{RcParseExpr, RepeatBound};
use name::{Name, Named};
use syntax::ast::{host, Field};
use syntax::ast::host::{Binop, Const, Unop};
use var::Var;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<N> {
    pub defs: BTreeMap<Path<N>, Definition<N>>,
}

impl<N: Name> Program<N> {
    pub fn new() -> Program<N> {
        Program {
            defs: BTreeMap::new(),
        }
    }

    pub fn define<P: Into<Path<N>>>(&mut self, path: P, def: Definition<N>) {
        let path = path.into();
        assert!(
            !self.defs.contains_key(&path),
            "Found duplicate top level definition for {}",
            path
        );

        self.defs.insert(path, def);
    }

    pub fn define_alias<P: Into<Path<N>>>(&mut self, path: P, ty: RcType<N>) {
        self.define(path, Definition::Alias(ty));
    }

    pub fn define_struct<P: Into<Path<N>>>(
        &mut self,
        path: P,
        fields: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N, RcExpr<N>>>,
    ) {
        self.define(path, Definition::Struct(fields, parser));
    }

    pub fn define_union<P: Into<Path<N>>>(
        &mut self,
        path: P,
        variants: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N, RcExpr<N>>>,
    ) {
        self.define(path, Definition::Union(variants, parser));
    }
}

/// A fully qualified path to a type definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path<N> {
    /// The base definition name from the source AST
    pub base: N,
    /// The path through a structural type in the source AST
    pub children: Vec<N>,
}

impl<N: Name> Path<N> {
    pub fn new(base: N) -> Path<N> {
        Path {
            base,
            children: vec![],
        }
    }

    pub fn append_child<N1: Into<N>>(&self, name: N1) -> Path<N> {
        let mut path = self.clone();
        path.children.push(name.into());
        path
    }
}

impl<N: fmt::Display> fmt::Display for Path<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for child in &self.children {
            write!(f, "::{}", child)?;
        }
        Ok(())
    }
}

impl<'a, N: From<&'a str>> From<&'a str> for Path<N> {
    fn from(src: &'a str) -> Path<N> {
        let mut parts = src.split("::").map(N::from);

        let base = parts.next().unwrap();
        let children = parts.collect();

        Path { base, children }
    }
}

/// Top level type definitions
///
/// The names of these are declared when they are stored in the `Program` struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition<N> {
    /// Type alias
    Alias(RcType<N>),
    /// Struct definition
    Struct(Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N, RcExpr<N>>>),
    /// Union type definition
    Union(Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N, RcExpr<N>>>),
}

/// Structural types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A fully qualified path to a type definition
    Path(Path<N>),
    /// Array types. These are usually available in languages as primitives,
    /// so there is no need to generate new types for these
    Array(RcType<N>),
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    pub fn path<P: Into<Path<N>>>(path: P) -> Type<N> {
        Type::Path(path.into())
    }

    pub fn array<T1: Into<RcType<N>>>(elem_ty: T1) -> Type<N> {
        Type::Array(elem_ty.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<N> {
    Const(Const),
    Prim(&'static str, RcType<N>),
    Var(Var<N>),
    Unop(Unop, RcExpr<N>),
    Binop(Binop, RcExpr<N>, RcExpr<N>),
    Struct(Path<N>, Vec<Field<N, RcExpr<N>>>),
    Proj(RcExpr<N>, N),
    Intro(Path<N>, N, RcExpr<N>),
    Subscript(RcExpr<N>, RcExpr<N>),
    Abs(Named<N, RcType<N>>, RcExpr<N>),
    App(RcExpr<N>, RcExpr<N>),
}

pub type RcExpr<N> = Rc<Expr<N>>;

// Lowering

impl<'a> From<&'a owned::ast::Program<String>> for Program<String> {
    fn from(src: &'a owned::ast::Program<String>) -> Program<String> {
        let mut program = Program::new();

        for def in &src.defs {
            // Begin tracking the path of this definition from the root name of the
            // source definition. This will be appended to in order to provide a
            // fully qualified path through the type definitions, eg:
            // `Foo::field::Entry::Variant2::...`
            let path = Path::new(def.name.clone());

            match *def.ty {
                // Structs and unions that are defined at the top level should
                // get the best names, closest to what the author of the data
                // definition intended!
                host::Type::Struct(ref fields) => {
                    let lowered_fields = lower_row(&mut program, &path, fields);
                    let lowered_parse_expr = lower_parse_expr(&path, &def.parser);
                    program.define_struct(path, lowered_fields, Some(lowered_parse_expr));
                }
                host::Type::Union(ref variants) => {
                    let lowered_variants = lower_row(&mut program, &path, variants);
                    let lowered_parse_expr = lower_parse_expr(&path, &def.parser);
                    program.define_union(path, lowered_variants, Some(lowered_parse_expr));
                }
                // Everything else should be an alias
                _ => {
                    let ty = lower_ty(&mut program, &path, &def.ty);
                    program.define_alias(path, ty);
                }
            }
        }

        program
    }
}

fn lower_row(
    program: &mut Program<String>,
    path: &Path<String>,
    fields: &[Field<String, host::RcType<String>>],
) -> Vec<Field<String, RcType<String>>> {
    fields
        .iter()
        .map(|field| {
            let field_path = path.append_child(field.name.clone());
            let ty = lower_ty(program, &field_path, &field.value);

            Field::new(field.name.clone(), ty)
        })
        .collect()
}

fn lower_ty(
    program: &mut Program<String>,
    path: &Path<String>,
    ty: &host::RcType<String>,
) -> RcType<String> {
    use name::Named;

    match **ty {
        host::Type::Var(Var::Bound(Named(ref name, _))) => Rc::new(Type::path(name.as_str())),
        host::Type::Var(Var::Free(_)) => unimplemented!(),
        host::Type::Const(_) => unimplemented!(),
        host::Type::Arrow(_, _) => unimplemented!(),
        host::Type::Array(ref elem_ty) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_ty(program, &elem_path, elem_ty);

            Rc::new(Type::array(elem_ty))
        }
        host::Type::Union(ref variants) => {
            let variants = lower_row(program, path, variants);
            program.define_union(path.clone(), variants, None);

            Rc::new(Type::path(path.clone()))
        }
        host::Type::Struct(ref fields) => {
            let fields = lower_row(program, path, fields);
            program.define_struct(path.clone(), fields, None);

            Rc::new(Type::path(path.clone()))
        }
        host::Type::Abs(_, _) => unimplemented!(),
        host::Type::App(_, _) => unimplemented!(),
    }
}

fn lower_parse_expr(
    path: &Path<String>,
    parser: &RcParseExpr<String>,
) -> RcParseExpr<String, RcExpr<String>> {
    use ir::owned::ast::ParseExpr;

    Rc::new(match **parser {
        ParseExpr::Var(ref var) => ParseExpr::Var(var.clone()),
        ParseExpr::U8 => unimplemented!(),
        ParseExpr::Ident(ref name) => ParseExpr::Ident(name.clone()),
        ParseExpr::Repeat(ref parser, ref bound) => {
            let elem_path = path.append_child("Elem");

            ParseExpr::Repeat(
                lower_parse_expr(&elem_path, parser),
                lower_repeat_bound(path, bound),
            )
        }
        ParseExpr::Assert(ref parser, ref pred_expr) => {
            ParseExpr::Assert(lower_parse_expr(path, parser), lower_expr(path, pred_expr))
        }
        ParseExpr::Sequence(ref parsers, ref expr) => ParseExpr::Sequence(
            parsers
                .iter()
                .map(|&Named(ref name, ref parser)| {
                    Named(
                        name.clone(),
                        lower_parse_expr(&path.append_child(name.clone()), parser),
                    )
                })
                .collect(),
            lower_expr(path, expr),
        ),
        ParseExpr::Choice(_) => unimplemented!(),
    })
}

fn lower_repeat_bound(
    path: &Path<String>,
    bound: &RepeatBound<host::RcExpr<String>>,
) -> RepeatBound<RcExpr<String>> {
    match *bound {
        RepeatBound::Exact(ref expr) => RepeatBound::Exact(lower_expr(path, expr)),
    }
}

fn lower_expr(path: &Path<String>, expr: &host::RcExpr<String>) -> RcExpr<String> {
    Rc::new(match **expr {
        host::Expr::Const(_, c) => Expr::Const(c),
        host::Expr::Prim(name, ref ty) => Expr::Prim(name, unimplemented!()),
        host::Expr::Var(_, ref var) => Expr::Var(var.clone()),
        host::Expr::Unop(_, op, ref expr) => Expr::Unop(op, lower_expr(path, expr)),
        host::Expr::Binop(_, op, ref lhs, ref rhs) => {
            Expr::Binop(op, lower_expr(path, lhs), lower_expr(path, rhs))
        }
        host::Expr::Struct(ref fields) => unimplemented!(),
        host::Expr::Proj(_, ref expr, ref field_name) => {
            Expr::Proj(lower_expr(path, expr), field_name.clone())
        }
        host::Expr::Intro(_, _, _, _) => unimplemented!(),
        host::Expr::Subscript(_, _, _) => unimplemented!(),
        host::Expr::Abs(_, _, _) => unimplemented!(),
        host::Expr::App(_, _, _) => unimplemented!(),
    })
}
