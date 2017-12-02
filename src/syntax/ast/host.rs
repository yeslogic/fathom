//! The syntax of our data description language

use ramp::Int;
use std::cmp::Ordering;
use std::rc::Rc;

use name::Named;
use source::Span;
use syntax::ast::{self, Field, Substitutions};
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

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0u8`, `1i64`, `2i16`, `123` ...
    Int(u64, Option<IntType>),
    /// A floating point constant: eg. `0f32`, `1.32f64`, ...
    Float(f64, FloatType),
}

impl Const {
    pub fn ty_const_of(&self) -> TypeConst {
        match *self {
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(x, ref suffix) => {
                TypeConst::Int(suffix.as_ref().cloned().unwrap_or(IntType::range(x, x)))
            }
            Const::Float(_, suffix) => TypeConst::Float(suffix),
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

/// A host expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A constant value
    Const(Span, Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, Var),
    /// An unary operator expression
    Unop(Span, Unop, RcExpr),
    /// A binary operator expression
    Binop(Span, Binop, RcExpr, RcExpr),
    /// A struct initialization expression
    Struct(Vec<Field<RcExpr>>),
    /// Field projection, eg: `x.field`
    Proj(Span, RcExpr, String),
    /// Variant introduction, eg: `.variant1 x : union { variant1 : T }`
    ///
    /// We require a type annotation because we don't have inference
    /// implemented in the type checker yet.
    //
    // TODO: add type inference to remove the need for this annotation
    Intro(Span, String, RcExpr, RcType),
    /// Array index, eg: `x[i]`
    Subscript(Span, RcExpr, RcExpr),
    /// Cast expression, eg: `x as u32`
    Cast(Span, RcExpr, RcType),
    /// Abstraction, eg: `\(x : T, ..) -> x`
    Abs(Span, Vec<Named<RcType>>, RcExpr),
    /// Application, eg: `f(x, ..)`
    App(Span, RcExpr, Vec<RcExpr>),
}

pub type RcExpr = Rc<Expr>;

impl Expr {
    /// Abstraction, eg: `\(x : T, ..) -> x`
    pub fn abs<E1>(span: Span, params: Vec<Named<RcType>>, body_expr: E1) -> Expr
    where
        E1: Into<RcExpr>,
    {
        let mut body_expr = body_expr.into();

        {
            let param_names = params.iter().map(|param| &*param.0).collect::<Vec<_>>();
            Rc::make_mut(&mut body_expr).abstract_names(&param_names[..]);
        }

        Expr::Abs(span, params, body_expr)
    }

    /// Attempt to lookup the value of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &str) -> Option<&RcExpr> {
        match *self {
            Expr::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        match *self {
            Expr::Var(_, Var::Free(ref name)) => match substs.get(name) {
                None => {}
                Some(ty) => panic!("Expected to substitute an expression, but found {:?}", ty),
            },
            Expr::Var(_, Var::Bound(_)) | Expr::Const(_, _) => {}
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                Rc::make_mut(expr).substitute(substs);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                Rc::make_mut(expr).substitute(substs);
                Rc::make_mut(ty).substitute(substs);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                Rc::make_mut(lhs_expr).substitute(substs);
                Rc::make_mut(rhs_expr).substitute(substs);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).substitute(substs);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                Rc::make_mut(array_expr).substitute(substs);
                Rc::make_mut(index_expr).substitute(substs);
            }
            Expr::Cast(_, ref mut src_expr, ref mut dst_ty) => {
                Rc::make_mut(src_expr).substitute(substs);
                Rc::make_mut(dst_ty).substitute(substs);
            }
            Expr::Abs(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    Rc::make_mut(arg_ty).substitute(substs);
                }

                Rc::make_mut(body_expr).substitute(substs);
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                Rc::make_mut(fn_expr).substitute(substs);

                for arg_expr in arg_exprs {
                    Rc::make_mut(arg_expr).substitute(substs);
                }
            }
        }
    }

    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *self {
            Expr::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Expr::Const(_, _) => {}
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
                Rc::make_mut(ty).abstract_names_at(names, scope);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                Rc::make_mut(lhs_expr).abstract_names_at(names, scope);
                Rc::make_mut(rhs_expr).abstract_names_at(names, scope);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                Rc::make_mut(array_expr).abstract_names_at(names, scope);
                Rc::make_mut(index_expr).abstract_names_at(names, scope);
            }
            Expr::Cast(_, ref mut src_expr, _) => {
                Rc::make_mut(src_expr).abstract_names_at(names, scope);
                // TODO: abstract dst_ty???
            }
            Expr::Abs(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    Rc::make_mut(arg_ty).abstract_names_at(names, scope);
                }

                Rc::make_mut(body_expr).abstract_names_at(names, scope.succ());
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                Rc::make_mut(fn_expr).abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    Rc::make_mut(arg_expr).abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatType {
    /// IEE-754 32-bit float
    F32,
    /// IEE-754 64-bit float
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntType {
    min: Int,
    max: Int,
}

impl IntType {
    pub fn range<Min: Into<Int>, Max: Into<Int>>(min: Min, max: Max) -> IntType {
        let min = min.into();
        let max = max.into();

        if min < max {
            IntType { min, max }
        } else {
            IntType { min, max }
        }
    }

    pub fn min(&self) -> &Int {
        &self.min
    }

    pub fn max(&self) -> &Int {
        &self.max
    }

    pub fn i8() -> IntType {
        IntType::range(::std::i8::MIN, ::std::i8::MAX)
    }

    pub fn i16() -> IntType {
        IntType::range(::std::i16::MIN, ::std::i16::MAX)
    }

    pub fn i24() -> IntType {
        // From https://en.wikipedia.org/wiki/24-bit
        IntType::range(-8_388_608, 8_388_607)
    }

    pub fn i32() -> IntType {
        IntType::range(::std::i32::MIN, ::std::i32::MAX)
    }

    pub fn i64() -> IntType {
        IntType::range(::std::i64::MIN, ::std::i64::MAX)
    }

    pub fn u8() -> IntType {
        IntType::range(::std::u8::MIN, ::std::u8::MAX)
    }

    pub fn u16() -> IntType {
        IntType::range(::std::u16::MIN, ::std::u16::MAX)
    }

    pub fn u24() -> IntType {
        IntType::range(::std::u32::MIN, 0xFFFFFF)
    }

    pub fn u32() -> IntType {
        IntType::range(::std::u32::MIN, ::std::u32::MAX)
    }

    pub fn u64() -> IntType {
        IntType::range(::std::u64::MIN, ::std::u64::MAX)
    }

    pub fn is_nonnegative(&self) -> bool {
        self.min() >= &Int::zero()
    }
}

impl PartialOrd for IntType {
    fn partial_cmp(&self, other: &IntType) -> Option<Ordering> {
        match (Int::cmp(self.min(), other.min()), Int::cmp(self.max(), other.max())) {
            // Equal ranges
            //
            // +-----self-----+
            // +-----other----+
            (Ordering::Equal, Ordering::Equal) => Some(Ordering::Equal),

            // Disjoint ranges
            //
            // +-----self-----?
            //                    ?-----other-----+
            (Ordering::Less, Ordering::Less) |
            //                    ?-----self-----+
            // +-----other-----?
            (Ordering::Greater, Ordering::Greater) => None,

            //       +-----self-----+
            // +-----------other-----------+
            (Ordering::Greater, Ordering::Less) |
            //       +-----self-----+
            // +-----------other----+
            (Ordering::Greater, Ordering::Equal) |
            // +-----self-----+
            // +-----other-----------+
            (Ordering::Equal, Ordering::Less) => Some(Ordering::Less),

            // +-----------self-----------+
            //       +-----other-----+
            (Ordering::Less, Ordering::Greater) |
            // +-----------self------+
            //       +-----other-----+
            (Ordering::Less, Ordering::Equal) |
            // +-----self-----------+
            // +-----other-----+
            (Ordering::Equal, Ordering::Greater) => Some(Ordering::Greater),
        }
    }
}

/// A type constant in the host language
#[derive(Debug, Clone, PartialEq, Eq)]
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
    Int(IntType),
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
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Abs(Vec<Named<()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(RcType, Vec<RcType>),
}

pub type RcType = Rc<Type>;

impl Type {
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn abs<T1>(param_names: &[&str], body_ty: T1) -> Type
    where
        T1: Into<RcType>,
    {
        let params = param_names
            .iter()
            .map(|&name| Named(String::from(name), ()))
            .collect();

        let mut body_ty = body_ty.into();
        Rc::make_mut(&mut body_ty).abstract_names(param_names);

        Type::Abs(params, body_ty)
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &str) -> Option<&RcType> {
        match *self {
            Type::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &str) -> Option<&RcType> {
        match *self {
            Type::Union(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        *self = match *self {
            Type::Var(Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => (*ty.repr()).clone(),
            },
            Type::Var(Var::Bound(_)) | Type::Const(_) => return,
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    Rc::make_mut(param_ty).substitute(substs);
                }
                Rc::make_mut(ret_ty).substitute(substs);

                return;
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).substitute(substs);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    Rc::make_mut(&mut variant.value).substitute(substs);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    Rc::make_mut(&mut field.value).substitute(substs);
                }
                return;
            }
            Type::Abs(_, ref mut body_ty) => {
                Rc::make_mut(body_ty).substitute(substs);
                return;
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                Rc::make_mut(fn_ty).substitute(substs);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).substitute(substs);
                }

                return;
            }
        };
    }

    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *self {
            Type::Var(ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    Rc::make_mut(param_ty).abstract_names_at(names, scope);
                }
                Rc::make_mut(ret_ty).abstract_names_at(names, scope);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).abstract_names_at(names, scope);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).abstract_names_at(names, scope);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope);
            },
            Type::Abs(_, ref mut body_ty) => {
                Rc::make_mut(body_ty).abstract_names_at(names, scope.succ());
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                Rc::make_mut(fn_ty).abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).abstract_names_at(names, scope);
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
    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0))
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        match *self {
            Type::Var(Var::Bound(Named(_, var))) => if var.scope == scope {
                *self = (*tys[var.binding.0 as usize]).clone();
            },
            Type::Var(Var::Free(_)) | Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    Rc::make_mut(param_ty).instantiate_at(scope, tys);
                }

                Rc::make_mut(ret_ty).instantiate_at(scope, tys);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).instantiate_at(scope, tys);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).instantiate_at(scope, tys);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).instantiate_at(scope, tys);
            },
            Type::Abs(_, ref mut ty) => {
                Rc::make_mut(ty).instantiate_at(scope.succ(), tys);
            }
            Type::App(ref mut ty, ref mut arg_tys) => {
                Rc::make_mut(ty).instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).instantiate_at(scope, tys);
                }
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }
}
