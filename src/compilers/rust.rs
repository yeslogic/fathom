use inflector::cases::pascalcase;
use quote::Tokens;
use std::collections::{HashSet, VecDeque};
use std::io::Write;

use Env;
use ast::{Endianness, Field, Type};

/// Compile the symbols in the environment to a Rust parser
#[allow(unused_variables)]
pub fn compile<W: Write>(env: &Env, writer: &mut W) {
    let _ = CompilerEnv::new().compile(env);
    unimplemented!()
}

/// For generating passable names for anonymous types
//
// TODO: More heuristics? Consider the path through a data structure?
enum NameHint {
    FieldName(String),
    AliasName(String),
}

enum CompileJob {
    Struct {
        name: String,
        fields: Vec<Field<Type>>,
    },
    Union { name: String, elems: Vec<Type> },
}

struct CompilerEnv {
    top_level_names: HashSet<String>,
    pending_jobs: VecDeque<CompileJob>,
}

impl CompilerEnv {
    fn new() -> CompilerEnv {
        CompilerEnv {
            top_level_names: HashSet::new(),
            pending_jobs: VecDeque::new(),
        }
    }

    fn reserve_top_level_name(&mut self, name: &str) {
        assert!(self.top_level_names.insert(name.into()));
    }

    fn gen_ty_name(&mut self, name: NameHint) -> String {
        let name = match name {
            NameHint::FieldName(ref name) => pascalcase::to_pascal_case(name),
            NameHint::AliasName(ref name) => pascalcase::to_pascal_case(name),
        };

        // FIXME: fallback if name is taken!
        self.reserve_top_level_name(&name);

        name
    }

    fn compile(mut self, env: &Env) -> Tokens {
        let mut tokens = Tokens::new();

        // Compile user-defined top level symbols
        for (name, ty) in env.tys() {
            tokens.append("\n");

            // Will need to prioritize these identifiers over, say, generated names
            self.reserve_top_level_name(name);

            tokens.append(match *ty {
                Type::Struct(_, ref fields) => self.compile_struct(&name, fields),
                ref ty => self.compile_alias(&name, ty),
            });
        }

        // Process pending codegen tasks
        while let Some(job) = self.pending_jobs.pop_front() {
            tokens.append(match job {
                CompileJob::Struct { name, fields } => self.compile_struct(&name, &fields),
                CompileJob::Union { name, elems } => self.compile_union(&name, &elems),
            });
        }

        tokens
    }

    fn compile_alias(&mut self, name: &str, ty: &Type) -> Tokens {
        let ty = self.compile_ty(NameHint::AliasName(name.to_owned()), ty);

        quote! {
            pub type #name = #ty;
        }
    }

    fn compile_struct(&mut self, name: &str, fields: &[Field]) -> Tokens {
        fn compile_struct_field(cenv: &mut CompilerEnv, field: &Field) -> Tokens {
            let field_name = &field.name;
            let field_ty = cenv.compile_ty(NameHint::FieldName(field_name.to_owned()), &field.ty);

            quote! {
                #field_name: #field_ty,
            }
        }

        fn compile_struct_field_getter(_: &mut CompilerEnv, field: &Field) -> Tokens {
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
                        quote! {
                            #field_ret_ty::from_bits(#bits_ty::#from_source(self.#field_name))
                        },
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

        let field_defs = fields
            .iter()
            .map(|field| compile_struct_field(self, field))
            .collect::<Vec<_>>();

        let getter_defs = fields
            .iter()
            .map(|field| compile_struct_field_getter(self, field))
            .collect::<Vec<_>>();

        quote! {
            pub struct #name {
                #field_defs
            }

            impl #name {
                #getter_defs
            }
        }
    }

    fn compile_union(&mut self, _name: &str, _elems: &[Type]) -> Tokens {
        // Struct with method accessors that return `Option<_>` to access variants
        unimplemented!()
    }

    fn compile_ty(&mut self, name: NameHint, ty: &Type) -> Tokens {
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

            Type::UInt(_, _) | Type::SInt(_, _) | Type::Float(_, _) => {
                unimplemented!("{:?} not yet handled outside of structs", ty)
            }

            Type::RangedInt(_, _) => {
                // Sould never happen!
                unimplemented!("{:?} not yet handled", ty)
            }

            Type::Var(_, _) => {
                // find symbol in env
                unimplemented!("{:?} not yet handled", ty)
            }
            Type::Struct(_, ref fields) => {
                let name = self.gen_ty_name(name);
                let tokens = quote! { #name };

                self.pending_jobs.push_back(CompileJob::Struct {
                    name,
                    fields: fields.clone(),
                });

                tokens
            }
            Type::Array(_, _, _) => {
                // If depedent and at end of struct:
                //    Return slice
                // Else:
                //    Return iterator
                //    Schedule iterator struct compilation
                unimplemented!("{:?} not yet handled", ty)
            }
            Type::Union(_, ref elems) => {
                let name = self.gen_ty_name(name);
                let tokens = quote! { #name };

                self.pending_jobs.push_back(CompileJob::Union {
                    name,
                    elems: elems.clone(),
                });

                tokens
            }
            Type::Where(_, _, _, _) => unimplemented!("{:?} not yet handled", ty),
        }
    }
}
