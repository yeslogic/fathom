use quote::Tokens;
use std::io::Write;

use Env;
use ast::Type;

/// Compile the symbols in the environment to a Rust parser
#[allow(unused_variables)]
pub fn compile<W: Write>(env: &Env, writer: &mut W) {
    unimplemented!()
}

#[allow(dead_code)]
fn compile_tokens<W: Write>(env: &Env) -> Tokens {
    let mut tokens = Tokens::new();

    tokens.append("\n");

    for (_, _) in env.tys() {
        unimplemented!()
    }

    tokens
}

#[allow(dead_code)]
fn compile_ty(ty: &Type) -> Tokens {
    use ast::{Endianness, TypeConst};

    match *ty {
        Type::Const(TypeConst::Bool) => quote! { bool },
        Type::Const(TypeConst::U(1, Endianness::Target)) => quote! { u8 },
        Type::Const(TypeConst::U(2, Endianness::Target)) => quote! { u16 },
        Type::Const(TypeConst::U(4, Endianness::Target)) => quote! { u32 },
        Type::Const(TypeConst::U(8, Endianness::Target)) => quote! { u64 },
        Type::Const(TypeConst::I(1, Endianness::Target)) => quote! { i8 },
        Type::Const(TypeConst::I(2, Endianness::Target)) => quote! { i16 },
        Type::Const(TypeConst::I(4, Endianness::Target)) => quote! { i32 },
        Type::Const(TypeConst::I(8, Endianness::Target)) => quote! { i64 },
        Type::Const(TypeConst::F(4, Endianness::Target)) => quote! { f32 },
        Type::Const(TypeConst::F(8, Endianness::Target)) => quote! { f64 },
        Type::Const(ty_const) => unimplemented!("{:?} not yet handled", ty_const),
        Type::Var(_, _) => unimplemented!(),
        Type::Struct(_, _) => {
            // If exists in compiled environment:
            //  Use name
            // Else:
            //  Schedule compilation
            unimplemented!()
        }
        Type::Array(_, _, _) => {
            // If depedent and at end of struct:
            //  Return slice
            // Else:
            //  Return iterator
            unimplemented!()
        }
        Type::Union(_, _) => {
            // Struct with method accessors that return `Option<_>` to access variants
            unimplemented!()
        }
        Type::Where(_, _, _, _) => unimplemented!(),
    }
}
