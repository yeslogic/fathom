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
    use ast::Endianness;

    match *ty {
        Type::Bool => quote! { bool },
        Type::UInt(1, Endianness::Target) => quote! { u8 },
        Type::UInt(2, Endianness::Target) => quote! { u16 },
        Type::UInt(4, Endianness::Target) => quote! { u32 },
        Type::UInt(8, Endianness::Target) => quote! { u64 },
        Type::SInt(1, Endianness::Target) => quote! { i8 },
        Type::SInt(2, Endianness::Target) => quote! { i16 },
        Type::SInt(4, Endianness::Target) => quote! { i32 },
        Type::SInt(8, Endianness::Target) => quote! { i64 },
        Type::Float(4, Endianness::Target) => quote! { f32 },
        Type::Float(8, Endianness::Target) => quote! { f64 },
        Type::SingletonInt(_) |
        Type::RangedInt(_, _) |
        Type::UInt(_, _) |
        Type::SInt(_, _) |
        Type::Float(_, _) => unimplemented!("{:?} not yet handled", ty),
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
