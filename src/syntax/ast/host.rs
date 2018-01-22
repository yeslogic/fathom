//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Ident, Named, OwnedIdent};
use source::Span;
use syntax::ast::{self, Field, Substitutions};
use parser::ast::host::Expr as ParseExpr;
use var::{ScopeIndex, Var};

/// Kinds of host type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    ///
    /// For now we only allow type arguments of kind `Type`. We represent this
    /// as an arity count
    Arrow { arity: u32 },
}

impl Kind {
    /// Kind of type functions
    pub fn arrow(arity: u32) -> Kind {
        Kind::Arrow { arity }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatType {
    /// IEE-754 32-bit float
    F32,
    /// IEE-754 64-bit float
    F64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SignedType {
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 24-bit integer
    I24,
    /// Signed 32-bit integer
    I32,
    /// Signed 64-bit integer
    I64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnsignedType {
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 24-bit integer
    U24,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
}

/// A type constant in the host language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Unit
    Unit,
    /// Bottom
    Bottom,
    /// Boolean
    Bool,
    /// Float
    Float(FloatType),
    /// Signed Integers
    Signed(SignedType),
    /// Unsigned Integers
    Unsigned(UnsignedType),
}

/// A host type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A type variable: eg. `T`
    Var(Var),
    /// A type constant
    Const(TypeConst),
    /// Arrow type: eg. `(T, ..) -> U`
    Arrow(Vec<RcType>, RcType),
    /// An array, eg. `[T]`
    Array(RcType),
    /// A union of types: eg. `union { variant : T, ... }`
    Union(Vec<Field<RcType>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<RcType>>),
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Lam(Vec<Named<OwnedIdent, ()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(RcType, Vec<RcType>),
}

#[derive(Clone, PartialEq)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcType {
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn lam<T1>(params: Vec<Named<OwnedIdent, ()>>, body_ty: T1) -> RcType
    where
        T1: Into<RcType>,
    {
        let mut body_ty = body_ty.into();

        {
            let param_names = params.iter().map(|param| &*param.0).collect::<Vec<_>>();
            body_ty.abstract_names(&param_names[..]);
        }

        Type::Lam(params, body_ty).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::Union(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.repr().clone(),
            },
            Type::Var(Var::Bound(_)) | Type::Const(_) => return,
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.substitute(substs);
                }
                ret_ty.substitute(substs);

                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.substitute(substs);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.substitute(substs);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.substitute(substs);
                }
                return;
            }
            Type::Lam(_, ref mut body_ty) => {
                body_ty.substitute(substs);
                return;
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.substitute(substs);

                for arg_ty in arg_tys {
                    arg_ty.substitute(substs);
                }

                return;
            }
        };
    }

    pub fn abstract_names_at(&mut self, names: &[&Ident], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::Var(ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.abstract_names_at(names, scope);
                }
                ret_ty.abstract_names_at(names, scope);
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.abstract_names_at(names, scope);
            }
            Type::Union(ref mut variants) => for variant in variants {
                variant.value.abstract_names_at(names, scope);
            },
            Type::Struct(ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
            Type::Lam(_, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
                }
            }
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables in `names` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_names(&mut self, names: &[&Ident]) {
        self.abstract_names_at(names, ScopeIndex(0))
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::Var(Var::Bound(_)) | Type::Var(Var::Free(_)) | Type::Const(_) => return,
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.instantiate_at(scope, tys);
                }

                ret_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Lam(_, ref mut ty) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::App(ref mut ty, ref mut arg_tys) => {
                ty.instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    arg_ty.instantiate_at(scope, tys);
                }

                return;
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntSuffix {
    Signed(SignedType),
    Unsigned(UnsignedType),
}

#[derive(Copy, Clone, PartialEq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0u8`, `1i64`, `2i16`, ...
    Int(u64, IntSuffix),
    /// A floating point constant: eg. `0f32`, `1.32f64`, ...
    Float(f64, FloatType),
}

impl Const {
    pub fn ty_const_of(self) -> TypeConst {
        match self {
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(_, IntSuffix::Unsigned(suffix)) => TypeConst::Unsigned(suffix),
            Const::Int(_, IntSuffix::Signed(suffix)) => TypeConst::Signed(suffix),
            Const::Float(_, suffix) => TypeConst::Float(suffix),
        }
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::Bool(value) => write!(f, "Bool({:?})", value),
            Const::Int(value, suffix) => write!(f, "Int({:?}, {:?})", value, suffix),
            Const::Float(value, suffix) => write!(f, "Float({:?}, {:?})", value, suffix),
        }
    }
}

/// An unary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Unop {
    /// Not: eg. `!x`
    Not,
    /// Negation: eg. `-x`
    Neg,
}

/// A binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binop {
    /// Disjunction: eg. `x | y`
    Or,
    /// Conjunction: eg. `x & y`
    And,
    /// Equality: eg. `x == y`
    Eq,
    /// Inequality: eg. `x != y`
    Ne,
    /// Less than or equal: eg. `x <= y`
    Le,
    /// Less than: eg. `x < y`
    Lt,
    /// Greater than: eg. `x > y`
    Gt,
    /// Greater than or equal: eg. `x >= y`
    Ge,
    /// Addition: eg. `x + y`
    Add,
    /// Subtraction: eg. `x - y`
    Sub,
    /// Multiplication: eg. `x * y`
    Mul,
    /// Division: eg. `x / y`
    Div,
}

/// Checkable host expressions
#[derive(Debug, Clone, PartialEq)]
pub enum CExpr {
    /// Variant introduction, eg: `.variant1 x`
    Intro(Span, OwnedIdent, RcCExpr),
    /// Array literals. eg: `[1, 2, 3]`
    Array(Span, Vec<RcCExpr>),
    /// Inferred expressions
    Inf(RcIExpr),
}

#[derive(Clone, PartialEq)]
pub struct RcCExpr {
    pub inner: Rc<CExpr>,
}

impl From<CExpr> for RcCExpr {
    fn from(src: CExpr) -> RcCExpr {
        RcCExpr {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcCExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcCExpr {
    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        match *Rc::make_mut(&mut self.inner) {
            CExpr::Intro(_, _, ref mut expr) => {
                expr.substitute(substs);
            }
            CExpr::Array(_, ref mut elems) => for elem in elems {
                elem.substitute(substs);
            },
            CExpr::Inf(ref mut iexpr) => {
                iexpr.substitute(substs);
            }
        }
    }

    pub fn abstract_names_at(&mut self, names: &[&Ident], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            CExpr::Intro(_, _, ref mut expr) => {
                expr.abstract_names_at(names, scope);
            }
            CExpr::Array(_, ref mut elems) => for elem in elems {
                elem.abstract_names_at(names, scope);
            },
            CExpr::Inf(ref mut iexpr) => {
                iexpr.abstract_names_at(names, scope);
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&Ident]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }

    pub fn from_parse(src: &ParseExpr) -> Result<RcCExpr, ()> {
        match *src {
            ParseExpr::Array(span, ref elems) => {
                let elems = elems
                    .iter()
                    .map(RcCExpr::from_parse)
                    .collect::<Result<_, _>>()?;

                Ok(CExpr::Array(span, elems).into())
            }
            _ => Ok(CExpr::Inf(RcIExpr::from_parse(src)?).into()),
        }
    }
}

/// Inferrable host expressions
#[derive(Debug, Clone, PartialEq)]
pub enum IExpr {
    /// An expression annotated by a type, ie. `x : u32`
    Ann(Span, RcCExpr, RcType),
    /// A constant value
    Const(Span, Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, Var),
    /// An unary operator expression
    Unop(Span, Unop, RcIExpr),
    /// A binary operator expression
    Binop(Span, Binop, RcIExpr, RcIExpr),
    /// A struct initialization expression
    Struct(Vec<Field<RcIExpr>>),
    /// Field projection, eg: `x.field`
    Proj(Span, RcIExpr, OwnedIdent),
    /// Array index, eg: `x[i]`
    Subscript(Span, RcIExpr, RcIExpr),
    /// Cast expression, eg: `x as u32`
    Cast(Span, RcIExpr, RcType),
    /// Lambda abstraction, eg: `\(x : T, ..) -> x`
    Lam(Span, Vec<Named<OwnedIdent, RcType>>, RcIExpr),
    /// Application, eg: `f(x, ..)`
    App(Span, RcIExpr, Vec<RcCExpr>),
}

#[derive(Clone, PartialEq)]
pub struct RcIExpr {
    pub inner: Rc<IExpr>,
}

impl From<IExpr> for RcIExpr {
    fn from(src: IExpr) -> RcIExpr {
        RcIExpr {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcIExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcIExpr {
    /// Lambda abstraction, eg: `\(x : T, ..) -> x`
    pub fn lam<E1>(span: Span, params: Vec<Named<OwnedIdent, RcType>>, body_expr: E1) -> RcIExpr
    where
        E1: Into<RcIExpr>,
    {
        let mut body_expr = body_expr.into();

        {
            let param_names = params.iter().map(|param| &*param.0).collect::<Vec<_>>();
            body_expr.abstract_names(&param_names[..]);
        }

        IExpr::Lam(span, params, body_expr).into()
    }

    /// Attempt to lookup the value of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &Ident) -> Option<&RcIExpr> {
        match *self.inner {
            IExpr::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        match *Rc::make_mut(&mut self.inner) {
            IExpr::Ann(_, ref mut expr, ref mut ty) => {
                expr.substitute(substs);
                ty.substitute(substs);
            }
            IExpr::Var(_, Var::Free(ref name)) => match substs.get(name) {
                None => {}
                Some(ty) => panic!("Expected to substitute an expression, but found {:?}", ty),
            },
            IExpr::Var(_, Var::Bound(_)) | IExpr::Const(_, _) => {}
            IExpr::Unop(_, _, ref mut expr) | IExpr::Proj(_, ref mut expr, _) => {
                expr.substitute(substs);
            }
            IExpr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.substitute(substs);
                rhs_expr.substitute(substs);
            }
            IExpr::Struct(ref mut fields) => for field in fields {
                field.value.substitute(substs);
            },
            IExpr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                array_expr.substitute(substs);
                index_expr.substitute(substs);
            }
            IExpr::Cast(_, ref mut src_expr, ref mut dst_ty) => {
                src_expr.substitute(substs);
                dst_ty.substitute(substs);
            }
            IExpr::Lam(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    arg_ty.substitute(substs);
                }

                body_expr.substitute(substs);
            }
            IExpr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                fn_expr.substitute(substs);

                for arg_expr in arg_exprs {
                    arg_expr.substitute(substs);
                }
            }
        }
    }

    pub fn abstract_names_at(&mut self, names: &[&Ident], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            IExpr::Ann(_, ref mut expr, ref mut ty) => {
                expr.abstract_names_at(names, scope);
                ty.abstract_names_at(names, scope);
            }
            IExpr::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            IExpr::Const(_, _) => {}
            IExpr::Unop(_, _, ref mut expr) | IExpr::Proj(_, ref mut expr, _) => {
                expr.abstract_names_at(names, scope);
            }
            IExpr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.abstract_names_at(names, scope);
                rhs_expr.abstract_names_at(names, scope);
            }
            IExpr::Struct(ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
            IExpr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                array_expr.abstract_names_at(names, scope);
                index_expr.abstract_names_at(names, scope);
            }
            IExpr::Cast(_, ref mut src_expr, ref mut dst_ty) => {
                src_expr.abstract_names_at(names, scope);
                dst_ty.abstract_names_at(names, scope);
            }
            IExpr::Lam(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    arg_ty.abstract_names_at(names, scope);
                }

                body_expr.abstract_names_at(names, scope.succ());
            }
            IExpr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                fn_expr.abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    arg_expr.abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&Ident]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }

    pub fn from_parse(src: &ParseExpr) -> Result<RcIExpr, ()> {
        match *src {
            ParseExpr::Const(span, c) => Ok(IExpr::Const(span, c).into()),
            ParseExpr::Ann(span, ref expr, ty) => {
                let expr = RcCExpr::from_parse(&**expr)?;
                let ty = Type::Const(ty).into();

                Ok(IExpr::Ann(span, expr, ty).into())
            }
            ParseExpr::Var(span, name) => Ok(IExpr::Var(span, Var::free(name)).into()),
            ParseExpr::Unop(span, op, ref expr) => {
                let expr = RcIExpr::from_parse(&**expr)?;

                Ok(IExpr::Unop(span, op, expr).into())
            }
            ParseExpr::Binop(span, op, ref lhs_expr, ref rhs_expr) => {
                let lhs_expr = RcIExpr::from_parse(&**lhs_expr)?;
                let rhs_expr = RcIExpr::from_parse(&**rhs_expr)?;

                Ok(IExpr::Binop(span, op, lhs_expr, rhs_expr).into())
            }
            ParseExpr::Array(_, _) => Err(()),
            ParseExpr::Proj(span, ref struct_expr, field_name) => {
                let struct_expr = RcIExpr::from_parse(&**struct_expr)?;
                let field_name = String::from(field_name);

                Ok(IExpr::Proj(span, struct_expr, OwnedIdent::from(field_name)).into())
            }
            ParseExpr::Subscript(span, ref array_expr, ref index_expr) => {
                let array_expr = RcIExpr::from_parse(&**array_expr)?;
                let index_expr = RcIExpr::from_parse(&**index_expr)?;

                Ok(IExpr::Subscript(span, array_expr, index_expr).into())
            }
            ParseExpr::Cast(span, ref expr, ty) => {
                let expr = RcIExpr::from_parse(&**expr)?;
                let ty = Type::Const(ty).into();

                Ok(IExpr::Cast(span, expr, ty).into())
            }
        }
    }
}
