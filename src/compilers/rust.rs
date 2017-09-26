use quote::Tokens;
use std::io::Write;

use Env;
use ast::{Endianness, Field, Type};

/// Compile the symbols in the environment to a Rust parser
#[allow(unused_variables)]
pub fn compile<W: Write>(env: &Env, writer: &mut W) {
    let _ = compile_tokens(env);
    unimplemented!()
}

fn compile_tokens(env: &Env) -> Tokens {
    let mut tokens = Tokens::new();

    for (name, ty) in env.tys() {
        tokens.append("\n");

        // Will need to prioritize these identifiers over, say, generated names

        match *ty {
            Type::Struct(_, ref fields) => tokens.append(compile_struct(&name, fields)),
            ref ty => tokens.append(compile_alias(&name, ty)),
        }
    }

    tokens
}

fn compile_struct(name: &str, fields: &[Field]) -> Tokens {
    fn compile_struct_field(field: &Field) -> Tokens {
        let field_name = &field.name;
        let field_ty = compile_ty(&field.ty);

        quote! {
            #field_name: #field_ty,
        }
    }

    fn compile_struct_field_getter(field: &Field) -> Tokens {
        let field_name = &field.name;

        let (getter_ty, getter_body) = match field.ty {
            Type::Bool => (quote! { bool }, quote! { self.#field_name }),

            // 1-byte data types don't need any conversions applied
            Type::UInt(1, _) => (quote! { u8 }, quote! { self.#field_name }),
            Type::SInt(1, _) => (quote! { i8 }, quote! { self.#field_name }),

            // Target endianness requires no conversions either!
            Type::UInt(2, Endianness::Target) => (quote! { u16 }, quote! { self.#field_name }),
            Type::UInt(4, Endianness::Target) => (quote! { u32 }, quote! { self.#field_name }),
            Type::UInt(8, Endianness::Target) => (quote! { u64 }, quote! { self.#field_name }),
            Type::SInt(2, Endianness::Target) => (quote! { i16 }, quote! { self.#field_name }),
            Type::SInt(4, Endianness::Target) => (quote! { i32 }, quote! { self.#field_name }),
            Type::SInt(8, Endianness::Target) => (quote! { i64 }, quote! { self.#field_name }),
            Type::Float(4, Endianness::Target) => (quote! { f32 }, quote! { self.#field_name }),
            Type::Float(8, Endianness::Target) => (quote! { f64 }, quote! { self.#field_name }),

            Type::UInt(bytes, e) => {
                let from_source = match e {
                    Endianness::Big => "from_be",
                    Endianness::Little => "from_le",
                    _ => unreachable!(),
                };
                let field_name = field.name.as_str();
                let field_ret_ty = match bytes {
                    2 => "u16",
                    4 => "u32",
                    8 => "u64",
                    _ => unimplemented!(),
                };

                (
                    quote! { #field_ret_ty },
                    quote! { #field_ret_ty::#from_source(self.#field_name) },
                )
            }
            Type::SInt(bytes, e) => {
                let from_source = match e {
                    Endianness::Big => "from_be",
                    Endianness::Little => "from_le",
                    _ => unreachable!(),
                };
                let field_name = field.name.as_str();
                let field_ret_ty = match bytes {
                    2 => "i16",
                    4 => "i32",
                    8 => "i64",
                    _ => unimplemented!(),
                };

                (
                    quote! { #field_ret_ty },
                    quote! { #field_ret_ty::#from_source(self.#field_name) },
                )
            }
            Type::Float(bytes, e) => {
                let from_source = match e {
                    Endianness::Big => "from_be",
                    Endianness::Little => "from_le",
                    _ => unreachable!(),
                };
                let field_name = field.name.as_str();
                let (bits_ty, field_ret_ty) = match bytes {
                    4 => ("u32", "f32"),
                    8 => ("u64", "f64"),
                    _ => unimplemented!(),
                };

                (
                    quote! { #field_ret_ty },
                    quote! { #field_ret_ty::from_bits(#bits_ty::#from_source(self.#field_name)) },
                )
            }

            _ => unimplemented!(),
        };

        quote! {
            pub fn #field_name(&self) -> #getter_ty {
                #getter_body
            }
        }
    }

    let field_defs = fields.iter().map(compile_struct_field).collect::<Vec<_>>();

    let getter_defs = fields
        .iter()
        .map(compile_struct_field_getter)
        .collect::<Vec<_>>();

    quote! {
        pub struct #name { #field_defs }
        impl #name { #getter_defs }
    }
}

fn compile_alias(name: &str, ty: &Type) -> Tokens {
    let ty = compile_ty(ty);

    quote! {
        pub type #name = #ty;
    }
}

fn compile_ty(ty: &Type) -> Tokens {
    match *ty {
        Type::Bool => quote! { bool },

        // 1-byte data types don't need any conversions applied
        Type::UInt(1, _) => quote! { u8 },
        Type::SInt(1, _) => quote! { i8 },

        // Target endianness requires no conversions either!
        Type::UInt(2, Endianness::Target) => quote! { u16 },
        Type::UInt(4, Endianness::Target) => quote! { u32 },
        Type::UInt(8, Endianness::Target) => quote! { u64 },
        Type::SInt(2, Endianness::Target) => quote! { i16 },
        Type::SInt(4, Endianness::Target) => quote! { i32 },
        Type::SInt(8, Endianness::Target) => quote! { i64 },
        Type::Float(4, Endianness::Target) => quote! { f32 },
        Type::Float(8, Endianness::Target) => quote! { f64 },

        Type::UInt(_, _) |
        Type::SInt(_, _) |
        Type::Float(_, _) => unimplemented!("{:?} not yet handled outside of structs", ty),

        Type::RangedInt(_, _) => {
            // Sould never happen!
            unimplemented!("{:?} not yet handled", ty)
        }

        Type::Var(_, _) => {
            // find symbol in env
            unimplemented!("{:?} not yet handled", ty)
        }
        Type::Struct(_, _) => {
            // If exists in compiled environment:
            //    Use name
            // Else:
            //    Schedule struct compilation
            unimplemented!("{:?} not yet handled", ty)
        }
        Type::Array(_, _, _) => {
            // If depedent and at end of struct:
            //    Return slice
            // Else:
            //    Return iterator
            //    Schedule iterator struct compilation
            unimplemented!("{:?} not yet handled", ty)
        }
        Type::Union(_, _) => {
            // Struct with method accessors that return `Option<_>` to access variants
            unimplemented!("{:?} not yet handled", ty)
        }
        Type::Where(_, _, _, _) => unimplemented!("{:?} not yet handled", ty),
    }
}
