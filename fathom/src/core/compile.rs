use std::cell::RefCell;
use std::fmt::Debug;

use heck::ToPascalCase;

use crate::core::{self, Prim, Term};
use crate::env::{EnvLen, UniqueEnv};
use crate::{StringId, StringInterner};

pub struct Context<'env, 'interner> {
    compile_env: &'env mut CompileEnv,
    interner: &'interner RefCell<StringInterner>,
}

#[derive(Debug, Clone)]
enum Type {
    Bool,

    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    /// A vector: array with unknown length
    Vec(Box<Type>),
    /// Array with a known length
    Array(usize, Box<Type>),

    ReadU8,
    ReadU16Be,
    ReadU16Le,
    ReadU32Be,
    ReadU32Le,
    ReadU64Be,
    ReadU64Le,
    ReadI8,
    ReadI16Be,
    ReadI16Le,
    ReadI32Be,
    ReadI32Le,
    ReadI64Be,
    ReadI64Le,
    ReadF32Be,
    ReadF32Le,
    ReadF64Be,
    ReadF64Le,

    Todo,
}

/// The definition of a struct
#[derive(Debug)]
struct Struct {
    // name of the struct
    name: StringId,
    fields: Vec<(StringId, Type)>,
}

#[derive(Debug)]
struct DecodeFn {
    struct_name: StringId,
    exprs: Vec<DecodeExpr>,
}

#[derive(Debug)]
enum DecodeExpr {
    /// name, fields
    Struct {
        // name of the struct
        name: StringId,
        // Parse a type into a named variable
        parse_fields: Vec<(StringId, ParseExpr)>,
        // How to initialise the fields of the struct using the variables
        fields: Vec<(StringId, ParseExpr)>,
    },
}

#[derive(Debug)]
enum ParseExpr {
    // Parse a const type
    Const(Type),
    // A reference to a variable
    Var(StringId),
}

pub struct CompileEnv {
    /// Names of variables.
    names: UniqueEnv<Option<StringId>>,
    /// Types of variables.
    types: UniqueEnv<Type>,
}

#[derive(Debug)]
pub struct Module {
    items: Vec<Item>,
}

#[derive(Debug)]
enum Item {
    Struct(Struct),
    ReadFn(DecodeFn),
}

impl<'interner> CompileEnv {
    pub fn new() -> Self {
        CompileEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
        }
    }

    pub fn default(interner: &'interner RefCell<StringInterner>) -> Self {
        let mut env = CompileEnvBuilder::new(interner);

        env.define_prim("void", Type::Todo); // Perhaps these have no name, and names are used for variables?
        env.define_prim("bool", Type::Bool);
        env.define_prim("u8", Type::U8);
        env.define_prim("u16", Type::U16);
        env.define_prim("u32", Type::U32);
        env.define_prim("u64", Type::U64);
        env.define_prim("i8", Type::I8);
        env.define_prim("i16", Type::I16);
        env.define_prim("i32", Type::I32);
        env.define_prim("i64", Type::I64);
        env.define_prim("f32", Type::Todo);
        env.define_prim("f64", Type::Todo);
        env.define_prim("Option", Type::Todo);
        env.define_prim("array", Type::Todo);
        env.define_prim("[u8]", Type::Array(0, Box::new(Type::U8)));
        env.define_prim("[u16]", Type::Array(0, Box::new(Type::U16)));
        env.define_prim("[u32]", Type::Array(0, Box::new(Type::U32)));
        env.define_prim("[u64]", Type::Array(0, Box::new(Type::U64)));
        env.define_prim("pos", Type::Todo);
        env.define_prim("ref", Type::Todo);
        env.define_prim("format", Type::Todo);

        env.define_prim("FormatU8", Type::ReadU8);
        env.define_prim("FormatU16Be", Type::ReadU16Be);
        env.define_prim("FormatU16Le", Type::ReadU16Le);
        env.define_prim("FormatU32Be", Type::ReadU32Be);
        env.define_prim("FormatU32Le", Type::ReadU32Le);
        env.define_prim("FormatU64Be", Type::ReadU64Be);
        env.define_prim("FormatU64Le", Type::ReadU64Le);
        env.define_prim("FormatS8", Type::ReadI8);
        env.define_prim("FormatS16Be", Type::ReadI16Be);
        env.define_prim("FormatS16Le", Type::ReadI16Le);
        env.define_prim("FormatS32Be", Type::ReadI32Be);
        env.define_prim("FormatS32Le", Type::ReadI32Le);
        env.define_prim("FormatS64Be", Type::ReadI64Be);
        env.define_prim("FormatS64Le", Type::ReadI64Le);
        env.define_prim("FormatF32Be", Type::ReadF32Be);
        env.define_prim("FormatF32Le", Type::ReadF32Le);
        env.define_prim("FormatF64Be", Type::ReadF64Be);
        env.define_prim("FormatF64Le", Type::ReadF64Le);

        // FIXME: These are all just todos for now but need to be present so the number of entries
        // in the env matches in order for LocalVars to index properly.
        env.define_prim("FormatArray8", Type::Todo);
        env.define_prim("FormatArray16", Type::Todo);
        env.define_prim("FormatArray32", Type::Todo);
        env.define_prim("FormatArray64", Type::Todo);
        env.define_prim("FormatRepeatUntilEnd", Type::Todo);
        env.define_prim("FormatLimit8", Type::Todo);
        env.define_prim("FormatLimit16", Type::Todo);
        env.define_prim("FormatLimit32", Type::Todo);
        env.define_prim("FormatLimit64", Type::Todo);
        env.define_prim("FormatLink", Type::Todo);
        env.define_prim("FormatDeref", Type::Todo);
        env.define_prim("FormatStreamPos", Type::Todo);
        env.define_prim("FormatSucceed", Type::Todo);
        env.define_prim("FormatFail", Type::Todo);
        env.define_prim("FormatUnwrap", Type::Todo);
        env.define_prim("FormatRepr", Type::Todo);

        env.define_prim("BoolEq", Type::Todo);
        env.define_prim("BoolNeq", Type::Todo);
        env.define_prim("BoolNot", Type::Todo);
        env.define_prim("BoolAnd", Type::Todo);
        env.define_prim("BoolOr", Type::Todo);
        env.define_prim("BoolXor", Type::Todo);

        env.define_prim("U8Eq", Type::Todo);
        env.define_prim("U8Neq", Type::Todo);
        env.define_prim("U8Lt", Type::Todo);
        env.define_prim("U8Gt", Type::Todo);
        env.define_prim("U8Lte", Type::Todo);
        env.define_prim("U8Gte", Type::Todo);
        env.define_prim("U8Add", Type::Todo);
        env.define_prim("U8Sub", Type::Todo);
        env.define_prim("U8Mul", Type::Todo);
        env.define_prim("U8Div", Type::Todo);
        env.define_prim("U8Not", Type::Todo);
        env.define_prim("U8Shl", Type::Todo);
        env.define_prim("U8Shr", Type::Todo);
        env.define_prim("U8And", Type::Todo);
        env.define_prim("U8Or", Type::Todo);
        env.define_prim("U8Xor", Type::Todo);

        env.define_prim("U16Eq", Type::Todo);
        env.define_prim("U16Neq", Type::Todo);
        env.define_prim("U16Lt", Type::Todo);
        env.define_prim("U16Gt", Type::Todo);
        env.define_prim("U16Lte", Type::Todo);
        env.define_prim("U16Gte", Type::Todo);
        env.define_prim("U16Add", Type::Todo);
        env.define_prim("U16Sub", Type::Todo);
        env.define_prim("U16Mul", Type::Todo);
        env.define_prim("U16Div", Type::Todo);
        env.define_prim("U16Not", Type::Todo);
        env.define_prim("U16Shl", Type::Todo);
        env.define_prim("U16Shr", Type::Todo);
        env.define_prim("U16And", Type::Todo);
        env.define_prim("U16Or", Type::Todo);
        env.define_prim("U16Xor", Type::Todo);

        env.define_prim("U32Eq", Type::Todo);
        env.define_prim("U32Neq", Type::Todo);
        env.define_prim("U32Lt", Type::Todo);
        env.define_prim("U32Gt", Type::Todo);
        env.define_prim("U32Lte", Type::Todo);
        env.define_prim("U32Gte", Type::Todo);
        env.define_prim("U32Add", Type::Todo);
        env.define_prim("U32Sub", Type::Todo);
        env.define_prim("U32Mul", Type::Todo);
        env.define_prim("U32Div", Type::Todo);
        env.define_prim("U32Not", Type::Todo);
        env.define_prim("U32Shl", Type::Todo);
        env.define_prim("U32Shr", Type::Todo);
        env.define_prim("U32And", Type::Todo);
        env.define_prim("U32Or", Type::Todo);
        env.define_prim("U32Xor", Type::Todo);

        env.define_prim("U64Eq", Type::Todo);
        env.define_prim("U64Neq", Type::Todo);
        env.define_prim("U64Lt", Type::Todo);
        env.define_prim("U64Gt", Type::Todo);
        env.define_prim("U64Lte", Type::Todo);
        env.define_prim("U64Gte", Type::Todo);
        env.define_prim("U64Add", Type::Todo);
        env.define_prim("U64Sub", Type::Todo);
        env.define_prim("U64Mul", Type::Todo);
        env.define_prim("U64Div", Type::Todo);
        env.define_prim("U64Not", Type::Todo);
        env.define_prim("U64Shl", Type::Todo);
        env.define_prim("U64Shr", Type::Todo);
        env.define_prim("U64And", Type::Todo);
        env.define_prim("U64Or", Type::Todo);
        env.define_prim("U64Xor", Type::Todo);

        env.define_prim("S8Eq", Type::Todo);
        env.define_prim("S8Neq", Type::Todo);
        env.define_prim("S8Lt", Type::Todo);
        env.define_prim("S8Gt", Type::Todo);
        env.define_prim("S8Lte", Type::Todo);
        env.define_prim("S8Gte", Type::Todo);
        env.define_prim("S8Neg", Type::Todo);
        env.define_prim("S8Add", Type::Todo);
        env.define_prim("S8Sub", Type::Todo);
        env.define_prim("S8Mul", Type::Todo);
        env.define_prim("S8Div", Type::Todo);
        env.define_prim("S8Abs", Type::Todo);
        env.define_prim("S8UAbs", Type::Todo);

        env.define_prim("S16Eq", Type::Todo);
        env.define_prim("S16Neq", Type::Todo);
        env.define_prim("S16Lt", Type::Todo);
        env.define_prim("S16Gt", Type::Todo);
        env.define_prim("S16Lte", Type::Todo);
        env.define_prim("S16Gte", Type::Todo);
        env.define_prim("S16Neg", Type::Todo);
        env.define_prim("S16Add", Type::Todo);
        env.define_prim("S16Sub", Type::Todo);
        env.define_prim("S16Mul", Type::Todo);
        env.define_prim("S16Div", Type::Todo);
        env.define_prim("S16Abs", Type::Todo);
        env.define_prim("S16UAbs", Type::Todo);

        env.define_prim("S32Eq", Type::Todo);
        env.define_prim("S32Neq", Type::Todo);
        env.define_prim("S32Lt", Type::Todo);
        env.define_prim("S32Gt", Type::Todo);
        env.define_prim("S32Lte", Type::Todo);
        env.define_prim("S32Gte", Type::Todo);
        env.define_prim("S32Neg", Type::Todo);
        env.define_prim("S32Add", Type::Todo);
        env.define_prim("S32Sub", Type::Todo);
        env.define_prim("S32Mul", Type::Todo);
        env.define_prim("S32Div", Type::Todo);
        env.define_prim("S32Abs", Type::Todo);
        env.define_prim("S32UAbs", Type::Todo);

        env.define_prim("S64Eq", Type::Todo);
        env.define_prim("S64Neq", Type::Todo);
        env.define_prim("S64Lt", Type::Todo);
        env.define_prim("S64Gt", Type::Todo);
        env.define_prim("S64Lte", Type::Todo);
        env.define_prim("S64Gte", Type::Todo);
        env.define_prim("S64Neg", Type::Todo);
        env.define_prim("S64Add", Type::Todo);
        env.define_prim("S64Sub", Type::Todo);
        env.define_prim("S64Mul", Type::Todo);
        env.define_prim("S64Div", Type::Todo);
        env.define_prim("S64Abs", Type::Todo);
        env.define_prim("S64UAbs", Type::Todo);

        env.define_prim("OptionSome", Type::Todo);
        env.define_prim("OptionNone", Type::Todo);
        env.define_prim("OptionFold", Type::Todo);

        env.define_prim("Array8Find", Type::Todo);
        env.define_prim("Array16Find", Type::Todo);
        env.define_prim("Array32Find", Type::Todo);
        env.define_prim("Array64Find", Type::Todo);

        env.define_prim("Array8Index", Type::Todo);
        env.define_prim("Array16Index", Type::Todo);
        env.define_prim("Array32Index", Type::Todo);
        env.define_prim("Array64Index", Type::Todo);

        env.define_prim("PosAddU8", Type::Todo);
        env.define_prim("PosAddU16", Type::Todo);
        env.define_prim("PosAddU32", Type::Todo);
        env.define_prim("PosAddU64", Type::Todo);

        env.build()
    }

    /// Get the length of the environment.
    fn len(&self) -> EnvLen {
        self.names.len()
    }

    /// Truncate the environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
    }

    /// Push a definition onto the context.
    fn push_def(&mut self, name: Option<StringId>, r#type: Type) {
        self.names.push(name);
        self.types.push(r#type);
    }
}

pub struct CompileEnvBuilder<'interner> {
    env: CompileEnv,
    interner: &'interner RefCell<StringInterner>,
}

impl<'interner> CompileEnvBuilder<'interner> {
    fn new(interner: &'interner RefCell<StringInterner>) -> Self {
        CompileEnvBuilder {
            env: CompileEnv::new(),
            interner,
        }
    }

    // TODO: Why Option?
    fn name(&self, name: &'static str) -> Option<StringId> {
        Some(self.interner.borrow_mut().get_or_intern_static(name))
    }

    fn define_prim(&mut self, name: &'static str, ty: Type) {
        self.env.push_def(self.name(name), ty);
    }

    fn build(self) -> CompileEnv {
        self.env
    }
}

impl<'arena, 'env, 'data, 'interner> Context<'env, 'interner> {
    pub fn new(
        compile_env: &'env mut CompileEnv,
        interner: &'interner RefCell<StringInterner>,
    ) -> Context<'env, 'interner> {
        Context {
            compile_env,
            interner,
        }
    }

    pub fn compile_module(&mut self, module: &core::Module<'arena>) -> Result<Module, ()> {
        let mut m = Module { items: Vec::new() };
        for item in module.items {
            self.compile_item(&mut m, item)?;
            dbg!(&m);
        }
        Ok(m)
    }

    fn compile_item(&mut self, module: &mut Module, item: &core::Item<'arena>) -> Result<(), ()> {
        match item {
            core::Item::Def {
                label,
                r#type: _type,
                expr,
            } => {
                dbg!(expr);

                let name = self
                    .interner
                    .borrow()
                    .resolve(*label)
                    .map(|name| name.to_pascal_case())
                    .expect("missing string");
                let name = self.interner.borrow_mut().get_or_intern(name);
                self.compile_def(module, name, expr)
            }
        }
    }

    fn compile_def(
        &mut self,
        module: &mut Module,
        label: StringId,
        expr: &Term<'arena>,
    ) -> Result<(), ()> {
        match expr {
            Term::ItemVar(_, _) => unimplemented! {},
            Term::LocalVar(_, _) => unimplemented! {},
            Term::MetaVar(_, _) => unimplemented! {},
            Term::InsertedMeta(_, _, _) => unimplemented! {},
            Term::Ann(_, _, _) => unimplemented! {},
            Term::Let(_, _, _, _, _) => unimplemented! {},
            Term::Universe(_) => unimplemented! {},
            Term::FunType(_, _, _, _) => unimplemented! {},
            Term::FunLit(_, _, _) => unimplemented! {},
            Term::FunApp(_, _, _) => unimplemented! {},
            Term::RecordType(_, _, _) => unimplemented! {},
            Term::RecordLit(_, _, _) => unimplemented! {},
            Term::RecordProj(_, _, _) => unimplemented! {},
            Term::ArrayLit(_, _) => unimplemented! {},
            Term::FormatRecord(_, labels, formats) => {
                let mut fields = Vec::with_capacity(labels.len());

                let initial_env_len = self.compile_env.len();
                for (label, format) in labels.iter().copied().zip(formats.iter()) {
                    let ty = self.compile_rep(format);
                    fields.push((label, ty.clone())); // FIXME: clone
                    self.compile_env.push_def(Some(label), ty);
                }
                self.compile_env.truncate(initial_env_len);
                let r#struct = Struct {
                    name: label,
                    fields,
                };
                dbg!(&r#struct);
                module.items.push(Item::Struct(r#struct));

                // Now generate the read function
                let read_fn = self.compile_decode(label, expr);
                dbg!(&read_fn);
                module.items.push(Item::ReadFn(read_fn));
            }
            Term::FormatCond(_, _, _, _) => unimplemented! {},
            Term::FormatOverlap(_, _, _) => unimplemented! {},
            Term::Prim(_, _) => unimplemented! {},
            Term::ConstLit(_, _) => unimplemented! {},
            Term::ConstMatch(_, _, _, _) => unimplemented! {},
        }

        Ok(())
    }

    fn compile_rep(&mut self, format: &Term<'arena>) -> Type {
        // We already have `format_repr` but that operates on Values...
        match format {
            Term::ItemVar(_, _) => todo! {},

            Term::LocalVar(_, var) => {
                let ty = self
                    .compile_env
                    .types
                    .get_index(*var)
                    .expect("invalid rigid var")
                    .clone();

                // Get the repr for this
                match ty {
                    // Type::Bool => {}
                    // Type::U8 => {}
                    // Type::U16 => {}
                    // Type::U32 => {}
                    // Type::U64 => {}
                    // Type::I8 => {}
                    // Type::I16 => {}
                    // Type::I32 => {}
                    // Type::I64 => {}
                    // Type::Vec(_) => {}
                    // Type::Array(_, _) => {}
                    Type::ReadU8 => Type::U8,
                    Type::ReadU16Be => Type::U16,
                    Type::ReadU16Le => Type::U16,
                    Type::ReadU32Be => Type::U32,
                    Type::ReadU32Le => Type::U32,
                    Type::ReadU64Be => Type::U64,
                    Type::ReadU64Le => Type::U64,
                    Type::ReadI8 => Type::I8,
                    Type::ReadI16Be => Type::I16,
                    Type::ReadI16Le => Type::I16,
                    Type::ReadI32Be => Type::I32,
                    Type::ReadI32Le => Type::I32,
                    Type::ReadI64Be => Type::I64,
                    Type::ReadI64Le => Type::I64,
                    // Type::ReadF32Be => Type::F32,
                    // Type::ReadF32Le => Type::F32,
                    // Type::ReadF64Be => Type::F64,
                    // Type::ReadF64Le => Type::F64,
                    // Type::Todo => {}
                    _ => todo!(),
                }
            }
            Term::MetaVar(_, _) => todo! {},
            Term::InsertedMeta(_, _, _) => todo! {},
            Term::Ann(_, _, _) => todo! {},
            Term::Let(_, _, _, _, _) => todo! {},
            Term::Universe(_) => todo! {},
            Term::FunType(_, _, _, _) => todo! {},
            Term::FunLit(_, _, _) => todo! {},
            Term::FunApp(
                _,
                Term::FunApp(_, Term::Prim(_, Prim::FormatArray32), _len),
                ele_format,
            ) => {
                // How to compile function application?
                // In the simple case we're calling a primitive function, so know how to compile
                // that..?
                // let len = self.compile_rep(len); // ignore len for now as we might not know it
                // at this point
                let item_type = self.compile_rep(ele_format);
                // dbg!(("array32", len));
                Type::Vec(Box::new(item_type))
            }
            Term::FunApp(_, _head, _input) => todo!("fun app"),
            Term::RecordType(_, _, _) => todo! {},
            Term::RecordLit(_, _, _) => todo! {},
            Term::RecordProj(_, _, _) => todo! {},
            Term::ArrayLit(_, _) => todo! {},
            Term::FormatRecord(_, _, _) => todo! {},
            Term::FormatCond(_, _, _, _) => todo! {},
            Term::FormatOverlap(_, _, _) => todo! {},
            Term::Prim(_, prim) => Self::compile_prim(prim),
            Term::ConstLit(_, _) => todo! {},
            Term::ConstMatch(_, _, _, _) => todo! {},
        }
    }

    fn compile_prim(prim: &Prim) -> Type {
        match prim {
            Prim::FormatU8 => Type::U8,
            Prim::FormatU16Be | Prim::FormatU16Le => Type::U16,
            Prim::FormatU32Be | Prim::FormatU32Le => Type::U32,
            Prim::FormatU64Be | Prim::FormatU64Le => Type::U64,
            Prim::FormatS8 => Type::I8,
            Prim::FormatS16Be | Prim::FormatS16Le => Type::I16,
            Prim::FormatS32Be | Prim::FormatS32Le => Type::I32,
            Prim::FormatS64Be | Prim::FormatS64Le => Type::I64,
            _ => todo! {},
        }
    }

    fn compile_decode(&mut self, name: StringId, format: &Term<'arena>) -> DecodeFn {
        match format {
            Term::FormatRecord(_, labels, formats) => {
                // For each field, put the name and type in the environment and generate a reader for it
                // Then when encountering a variable we need to be able to lookup in that environment
                // using the variable
                let initial_env_len = self.compile_env.len();
                let mut fields = Vec::with_capacity(labels.len());
                let parse_fields: Vec<(StringId, ParseExpr)> = labels
                    .iter()
                    .copied()
                    .zip(formats.iter())
                    .map(|(label, format)| {
                        let ty = self.read_format(format);
                        self.compile_env.push_def(Some(label), ty.clone()); // FIXME: clone
                        (label, ParseExpr::Const(ty))
                    })
                    .collect();

                for name in labels.iter() {
                    fields.push((*name, ParseExpr::Var(*name)));
                }

                self.compile_env.truncate(initial_env_len);

                // Create DecodeExpr::Struct
                let st = DecodeExpr::Struct {
                    // name of the struct
                    name,
                    // Parse a type into a named variable
                    parse_fields,
                    // How to initialise the fields of the struct using the variables
                    fields,
                };
                DecodeFn {
                    struct_name: name,
                    exprs: vec![st],
                }
            }
            _ => unreachable!("can only compile decode for format records"),
        }
    }

    fn read_format(&mut self, format: &Term<'arena>) -> Type {
        match format {
            Term::LocalVar(_, var) => self
                .compile_env
                .types
                .get_index(*var)
                .expect("invalid rigid var")
                .clone(),
            Term::Prim(_, prim) => Self::read_prim(prim),
            _ => unimplemented!("format: {:?}", format),
        }
    }

    fn read_prim(prim: &Prim) -> Type {
        match prim {
            Prim::FormatU8 => Type::ReadU8,
            Prim::FormatU16Be => Type::ReadU16Be,
            Prim::FormatU16Le => Type::ReadU16Le,
            Prim::FormatU32Be => Type::ReadF32Be,
            Prim::FormatU32Le => Type::ReadF32Le,
            Prim::FormatU64Be => Type::ReadF64Be,
            Prim::FormatU64Le => Type::ReadF64Le,
            _ => todo! {},
        }
    }
}

pub mod rust {
    use std::cell::RefCell;

    use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
    use scoped_arena::Scope;

    use crate::core::compile::{DecodeExpr, DecodeFn, Item, Module, ParseExpr, Type};
    use crate::{StringId, StringInterner};

    const INDENT: isize = 4;

    pub struct Context<'interner, 'arena> {
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    }

    impl<'interner, 'arena> Context<'interner, 'arena> {
        pub fn new(
            interner: &'interner RefCell<StringInterner>,
            scope: &'arena Scope<'arena>,
        ) -> Context<'interner, 'arena> {
            Context { interner, scope }
        }

        fn string_id(&'arena self, name: StringId) -> DocBuilder<'arena, Self> {
            match self.interner.borrow().resolve(name) {
                Some(name) => self.text(name.to_owned()),
                None => self.text("#error"),
            }
        }

        pub fn module(&'arena self, module: &Module) -> DocBuilder<'arena, Self> {
            self.concat([
                self.imports(),
                self.hardline(),
                self.intersperse(
                    module.items.iter().map(|item| self.item(item)),
                    self.hardline(),
                ),
            ])
        }

        fn item(&'arena self, item: &Item) -> DocBuilder<'arena, Self> {
            match item {
                Item::Struct(r#struct) => self.sequence(
                    self.concat([
                        self.text("struct"),
                        self.space(),
                        self.string_id(r#struct.name),
                        self.space(),
                        self.text("{"),
                    ]),
                    r#struct.fields.iter().map(|(name, ty)| {
                        self.concat([
                            self.string_id(*name),
                            self.text(":"),
                            self.space(),
                            self.ty_prec((), ty),
                        ])
                    }),
                    self.text(","),
                    self.concat([self.text("}"), self.hardline()]),
                ),
                Item::ReadFn(readfn) => {
                    let docs = vec![
                        self.concat([self.text("type HostType = Self;"), self.hardline()]),
                        self.read_fn(readfn),
                    ];

                    self.concat([
                        self.text("impl<'a> ReadBinary<'a> for"),
                        self.space(),
                        self.string_id(readfn.struct_name),
                        self.space(),
                        self.text("{"),
                        self.hardline().nest(INDENT),
                        self.intersperse(docs, self.hardline()).nest(INDENT),
                        self.hardline(),
                        self.text("}"),
                    ])
                }
            }
        }

        fn imports(&'arena self) -> DocBuilder<'arena, Self> {
            self.concat([
                self.text("use fathom_runtime::error::{ParseError};"),
                self.hardline(),
                self.text("use fathom_runtime::read::{ReadCtxt, ReadBinary};"),
                self.hardline(),
            ])
        }

        fn read_fn(&'arena self, readfn: &DecodeFn) -> DocBuilder<'arena, Self> {
            self.concat([
                self.text("fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {"),
                self.hardline().nest(INDENT),
                self.intersperse(
                    readfn.exprs.iter().map(|expr| self.expr(expr)),
                    self.concat([self.text(";"), self.hardline()]),
                )
                .nest(INDENT),
                self.hardline(),
                self.text("}"),
            ])
        }

        fn ty_prec(
            &'arena self,
            _prec: (), // TODO: Is this needed?
            ty: &Type,
        ) -> DocBuilder<'arena, Self> {
            match ty {
                Type::U8 => self.text("u8"),
                Type::U16 => self.text("u16"),
                Type::U32 => self.text("u32"),
                Type::U64 => self.text("u64"),
                Type::I8 => self.text("i8"),
                Type::I16 => self.text("i16"),
                Type::I32 => self.text("i32"),
                Type::I64 => self.text("i64"),
                Type::Vec(t) => self.concat([
                    // TODO: Use sequence?
                    self.text("Vec<"),
                    self.ty_prec((), t),
                    self.text(">"),
                ]),
                Type::Array(len, t) => self.concat([
                    // TODO: Use sequence?
                    self.text("["),
                    self.ty_prec((), t),
                    self.text(";"),
                    self.space(),
                    self.text(len.to_string()), // FIXME: right way to do this?
                    self.text("]"),
                ]),
                Type::Bool => self.text("bool"),
                Type::ReadU8 => self.text("ctxt.read_u8()"),
                Type::ReadU16Be => self.text("ctxt.read_u16be()"),
                Type::ReadU32Be => self.text("ctxt.read_u32be()"),
                Type::ReadU64Be => self.text("ctxt.read_u64be()"),
                Type::ReadU16Le => self.text("ctxt.read_u16le()"),
                Type::ReadU32Le => self.text("ctxt.read_u32le()"),
                Type::ReadU64Le => self.text("ctxt.read_u64le()"),
                Type::Todo => self.text("todo!()"),
                _ => unimplemented!(),
            }
        }

        fn expr(&'arena self, expr: &DecodeExpr) -> DocBuilder<'arena, Self> {
            match expr {
                DecodeExpr::Struct {
                    name,
                    parse_fields,
                    fields,
                } => self.concat([
                    self.parse_fields(parse_fields),
                    self.hardline(),
                    self.construct_struct(*name, fields),
                ]),
            }
        }

        fn parse_fields(
            &'arena self,
            fields: &[(StringId, ParseExpr)],
        ) -> DocBuilder<'arena, Self> {
            self.concat(fields.iter().map(|(name, field)| {
                self.concat([
                    self.text("let"),
                    self.space(),
                    self.string_id(*name),
                    self.space(),
                    self.text("="),
                    self.space(),
                    self.parse_expr(field),
                    self.text("?;"),
                    self.hardline(),
                ])
            }))
        }

        fn construct_struct(
            &'arena self,
            name: StringId,
            fields: &[(StringId, ParseExpr)],
        ) -> DocBuilder<'arena, Self> {
            self.sequence(
                self.concat([
                    self.text("Ok("),
                    self.string_id(name),
                    self.space(),
                    self.text("{"),
                ]),
                fields.iter().map(|(name, field)| {
                    self.concat([
                        self.string_id(*name),
                        self.text(":"),
                        self.space(),
                        self.parse_expr(field),
                    ])
                }),
                self.text(","),
                self.text("})"),
            )
        }

        fn parse_expr(&'arena self, expr: &ParseExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ParseExpr::Const(ty) => self.type_const(ty),
                ParseExpr::Var(name) => self.string_id(*name),
            }
        }

        fn type_const(&'arena self, ty_const: &Type) -> DocBuilder<'arena, Self> {
            match ty_const {
                Type::ReadU8 => self.text("ctxt.read_u8()"),
                Type::ReadU16Be => self.text("ctxt.read_u16be()"),
                Type::ReadU32Be => self.text("ctxt.read_u32be()"),
                Type::ReadU64Be => self.text("ctxt.read_u64be()"),
                Type::ReadU16Le => self.text("ctxt.read_u16le()"),
                Type::ReadU32Le => self.text("ctxt.read_u32le()"),
                Type::ReadU64Le => self.text("ctxt.read_u64le()"),
                _ => unimplemented!(),
            }
        }

        /// Pretty prints a delimited sequence of documents with a trailing
        /// separator if it is formatted over multiple lines.
        pub fn sequence(
            &'arena self,
            start_delim: DocBuilder<'arena, Self>,
            docs: impl ExactSizeIterator<Item = DocBuilder<'arena, Self>> + Clone,
            separator: DocBuilder<'arena, Self>,
            end_delim: DocBuilder<'arena, Self>,
        ) -> DocBuilder<'arena, Self> {
            if docs.len() == 0 {
                self.concat([start_delim, end_delim])
            } else {
                DocBuilder::flat_alt(
                    self.concat([
                        start_delim.clone(),
                        self.concat(
                            docs.clone()
                                .map(|doc| self.concat([self.hardline(), doc, separator.clone()])),
                        )
                        .nest(INDENT),
                        self.hardline(),
                        end_delim.clone(),
                    ]),
                    self.concat([
                        start_delim,
                        self.space(),
                        self.intersperse(docs, self.concat([separator, self.space()])),
                        self.space(),
                        end_delim,
                    ]),
                )
                .group()
            }
        }
    }

    impl<'interner, 'arena, A: 'arena> DocAllocator<'arena, A> for Context<'interner, 'arena> {
        type Doc = RefDoc<'arena, A>;

        #[inline]
        fn alloc(&'arena self, doc: Doc<'arena, Self::Doc, A>) -> Self::Doc {
            // Based on the `DocAllocator` implementation for `pretty::Arena`
            RefDoc(match doc {
                // Return 'static references for common variants to avoid some allocations
                Doc::Nil => &Doc::Nil,
                Doc::Hardline => &Doc::Hardline,
                Doc::Fail => &Doc::Fail,
                // space()
                Doc::BorrowedText(" ") => &Doc::BorrowedText(" "),
                // line()
                Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::BorrowedText(" "))) => {
                    &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::BorrowedText(" ")))
                }
                // line_()
                Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)) => {
                    &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::Nil))
                }
                // softline()
                Doc::Group(RefDoc(Doc::FlatAlt(
                    RefDoc(Doc::Hardline),
                    RefDoc(Doc::BorrowedText(" ")),
                ))) => &Doc::Group(RefDoc(&Doc::FlatAlt(
                    RefDoc(&Doc::Hardline),
                    RefDoc(&Doc::BorrowedText(" ")),
                ))),
                // softline_()
                Doc::Group(RefDoc(Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)))) => {
                    &Doc::Group(RefDoc(&Doc::FlatAlt(
                        RefDoc(&Doc::Hardline),
                        RefDoc(&Doc::Nil),
                    )))
                }

                // Language tokens
                Doc::BorrowedText("fun") => &Doc::BorrowedText("fun"),
                Doc::BorrowedText("let") => &Doc::BorrowedText("let"),
                Doc::BorrowedText("overlap") => &Doc::BorrowedText("overlap"),
                Doc::BorrowedText("Type") => &Doc::BorrowedText("Type"),
                Doc::BorrowedText("where") => &Doc::BorrowedText("where"),
                Doc::BorrowedText(":") => &Doc::BorrowedText(":"),
                Doc::BorrowedText(",") => &Doc::BorrowedText(","),
                Doc::BorrowedText("=") => &Doc::BorrowedText("="),
                Doc::BorrowedText("=>") => &Doc::BorrowedText("=>"),
                Doc::BorrowedText(".") => &Doc::BorrowedText("."),
                Doc::BorrowedText("->") => &Doc::BorrowedText("->"),
                Doc::BorrowedText("<-") => &Doc::BorrowedText("<-"),
                Doc::BorrowedText(";") => &Doc::BorrowedText(";"),
                Doc::BorrowedText("_") => &Doc::BorrowedText("_"),
                Doc::BorrowedText("{") => &Doc::BorrowedText("{"),
                Doc::BorrowedText("}") => &Doc::BorrowedText("}"),
                Doc::BorrowedText("[") => &Doc::BorrowedText("["),
                Doc::BorrowedText("]") => &Doc::BorrowedText("]"),
                Doc::BorrowedText("(") => &Doc::BorrowedText("("),
                Doc::BorrowedText(")") => &Doc::BorrowedText(")"),

                _ => self.scope.to_scope(doc),
            })
        }

        fn alloc_column_fn(
            &'arena self,
            f: impl 'arena + Fn(usize) -> Self::Doc,
        ) -> <Self::Doc as DocPtr<'arena, A>>::ColumnFn {
            self.scope.to_scope(f)
        }

        fn alloc_width_fn(
            &'arena self,
            f: impl 'arena + Fn(isize) -> Self::Doc,
        ) -> <Self::Doc as DocPtr<'arena, A>>::WidthFn {
            self.scope.to_scope(f)
        }
    }
}
