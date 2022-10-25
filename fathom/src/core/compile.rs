use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::Debug;

use heck::ToPascalCase;

use crate::core::{self, Const, Prim, Term};
use crate::env::{EnvLen, Index, UniqueEnv};
use crate::{StringId, StringInterner};

pub struct Context<'env, 'interner> {
    compile_env: &'env mut CompileEnv,
    /// Names of top-level items.
    items: UniqueEnv<StringId>,
    interner: &'interner RefCell<StringInterner>,
}

pub struct CompileEnv {
    /// Names of variables.
    names: UniqueEnv<Option<StringId>>,
    /// Types of variables.
    types: UniqueEnv<Type>,
}

pub struct CompileEnvBuilder<'interner> {
    env: CompileEnv,
    interner: &'interner RefCell<StringInterner>,
}

#[derive(Debug, Clone)]
enum Type {
    Bool,
    Const(Const),

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

    ReadArray8(Box<ParseExpr>),
    ReadArray16(Box<ParseExpr>),
    ReadArray32(Box<ParseExpr>),
    ReadArray64(Box<ParseExpr>),

    // len, elem
    DoReadArray8(Box<ParseExpr>, Box<ParseExpr>),
    DoReadArray16(Box<ParseExpr>, Box<ParseExpr>),
    DoReadArray32(Box<ParseExpr>, Box<ParseExpr>),
    DoReadArray64(Box<ParseExpr>, Box<ParseExpr>),

    // I don't like this...
    PrimReadArray8,
    PrimReadArray16,
    PrimReadArray32,
    PrimReadArray64,

    Todo,
}

/// The definition of a struct
#[derive(Debug)]
struct Struct {
    // name of the struct
    name: StringId,
    fields: Vec<(StringId, Type)>,
    borrows_data: bool,
    fixed_size: Option<usize>,
}

#[derive(Debug)]
struct ReadStruct {
    name: StringId,
    borrows_data: bool,
    fixed_size: Option<usize>,
}

#[derive(Debug)]
struct ReadFn {
    r#struct: ReadStruct,
    exprs: Vec<ReadExpr>,
}

#[derive(Debug)]
enum ReadExpr {
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

#[derive(Debug, Clone)]
enum ParseExpr {
    // Parse a const type
    Const(Type),
    // A reference to a variable
    Var(StringId),
}

impl ParseExpr {
    pub(crate) fn repr(&self) -> ParseExpr {
        match self {
            ParseExpr::Const(ty) => match ty {
                Type::ReadU8 => ParseExpr::Const(Type::U8),
                Type::ReadU16Be => ParseExpr::Const(Type::U16),
                Type::ReadU16Le => ParseExpr::Const(Type::U16),
                Type::ReadU32Be => ParseExpr::Const(Type::U32),
                Type::ReadU32Le => ParseExpr::Const(Type::U32),
                Type::ReadU64Be => ParseExpr::Const(Type::U64),
                Type::ReadU64Le => ParseExpr::Const(Type::U64),
                Type::ReadI8 => ParseExpr::Const(Type::I8),
                Type::ReadI16Be => ParseExpr::Const(Type::I16),
                Type::ReadI16Le => ParseExpr::Const(Type::I16),
                Type::ReadI32Be => ParseExpr::Const(Type::I32),
                Type::ReadI32Le => ParseExpr::Const(Type::I32),
                Type::ReadI64Be => ParseExpr::Const(Type::I64),
                Type::ReadI64Le => ParseExpr::Const(Type::I64),
                // Type::ReadF32Be => Type::F32,
                // Type::ReadF32Le => Type::F32,
                // Type::ReadF64Be => Type::F64,
                // Type::ReadF64Le => Type::F64,
                // Type::ReadArray8(_) => {}
                // Type::ReadArray16(_) => {}
                // Type::ReadArray32(_) => {}
                // Type::ReadArray64(_) => {}
                // Type::DoReadArray8(_, _) => {}
                // Type::DoReadArray16(_, _) => {}
                // Type::DoReadArray32(_, _) => {}
                // Type::DoReadArray64(_, _) => {}
                // Type::PrimReadArray8 => {}
                // Type::PrimReadArray16 => {}
                // Type::PrimReadArray32 => {}
                // Type::PrimReadArray64 => {}
                Type::Todo => todo! {},
                _ => unimplemented!("unexpected type"),
            },
            ParseExpr::Var(_) => todo!("can we get here?"),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    items: Vec<Item>,
}

#[derive(Debug)]
enum Item {
    Struct(Struct),
    ReadFn(ReadFn),
    Var(StringId),
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

        env.define_prim(Some("void"), Type::Todo); // Perhaps these have no name, and names are used for variables?
        env.define_prim(None, Type::Bool);
        env.define_prim(None, Type::U8);
        env.define_prim(None, Type::U16);
        env.define_prim(None, Type::U32);
        env.define_prim(None, Type::U64);
        env.define_prim(None, Type::I8);
        env.define_prim(None, Type::I16);
        env.define_prim(None, Type::I32);
        env.define_prim(None, Type::I64);
        env.define_prim(Some("f32"), Type::Todo);
        env.define_prim(Some("f64"), Type::Todo);
        env.define_prim(Some("Option"), Type::Todo);
        env.define_prim(Some("array"), Type::Todo);
        env.define_prim(None, Type::Array(0, Box::new(Type::U8)));
        env.define_prim(None, Type::Array(0, Box::new(Type::U16)));
        env.define_prim(None, Type::Array(0, Box::new(Type::U32)));
        env.define_prim(None, Type::Array(0, Box::new(Type::U64)));
        env.define_prim(Some("pos"), Type::Todo);
        env.define_prim(Some("ref"), Type::Todo);
        env.define_prim(Some("format"), Type::Todo);

        env.define_prim(None, Type::ReadU8);
        env.define_prim(None, Type::ReadU16Be);
        env.define_prim(None, Type::ReadU16Le);
        env.define_prim(None, Type::ReadU32Be);
        env.define_prim(None, Type::ReadU32Le);
        env.define_prim(None, Type::ReadU64Be);
        env.define_prim(None, Type::ReadU64Le);
        env.define_prim(None, Type::ReadI8);
        env.define_prim(None, Type::ReadI16Be);
        env.define_prim(None, Type::ReadI16Le);
        env.define_prim(None, Type::ReadI32Be);
        env.define_prim(None, Type::ReadI32Le);
        env.define_prim(None, Type::ReadI64Be);
        env.define_prim(None, Type::ReadI64Le);
        env.define_prim(None, Type::ReadF32Be);
        env.define_prim(None, Type::ReadF32Le);
        env.define_prim(None, Type::ReadF64Be);
        env.define_prim(None, Type::ReadF64Le);
        env.define_prim(None, Type::PrimReadArray8);
        env.define_prim(None, Type::PrimReadArray16);
        env.define_prim(None, Type::PrimReadArray32);
        env.define_prim(None, Type::PrimReadArray64);

        // FIXME: These are all just todos for now but need to be present so the number of entries
        // in the env matches in order for LocalVars to index properly.
        env.define_prim(Some("FormatRepeatUntilEnd"), Type::Todo);
        env.define_prim(Some("FormatLimit8"), Type::Todo);
        env.define_prim(Some("FormatLimit16"), Type::Todo);
        env.define_prim(Some("FormatLimit32"), Type::Todo);
        env.define_prim(Some("FormatLimit64"), Type::Todo);
        env.define_prim(Some("FormatLink"), Type::Todo);
        env.define_prim(Some("FormatDeref"), Type::Todo);
        env.define_prim(Some("FormatStreamPos"), Type::Todo);
        env.define_prim(Some("FormatSucceed"), Type::Todo);
        env.define_prim(Some("FormatFail"), Type::Todo);
        env.define_prim(Some("FormatUnwrap"), Type::Todo);
        env.define_prim(Some("FormatRepr"), Type::Todo);

        env.define_prim(Some("BoolEq"), Type::Todo);
        env.define_prim(Some("BoolNeq"), Type::Todo);
        env.define_prim(Some("BoolNot"), Type::Todo);
        env.define_prim(Some("BoolAnd"), Type::Todo);
        env.define_prim(Some("BoolOr"), Type::Todo);
        env.define_prim(Some("BoolXor"), Type::Todo);

        env.define_prim(Some("U8Eq"), Type::Todo);
        env.define_prim(Some("U8Neq"), Type::Todo);
        env.define_prim(Some("U8Lt"), Type::Todo);
        env.define_prim(Some("U8Gt"), Type::Todo);
        env.define_prim(Some("U8Lte"), Type::Todo);
        env.define_prim(Some("U8Gte"), Type::Todo);
        env.define_prim(Some("U8Add"), Type::Todo);
        env.define_prim(Some("U8Sub"), Type::Todo);
        env.define_prim(Some("U8Mul"), Type::Todo);
        env.define_prim(Some("U8Div"), Type::Todo);
        env.define_prim(Some("U8Not"), Type::Todo);
        env.define_prim(Some("U8Shl"), Type::Todo);
        env.define_prim(Some("U8Shr"), Type::Todo);
        env.define_prim(Some("U8And"), Type::Todo);
        env.define_prim(Some("U8Or"), Type::Todo);
        env.define_prim(Some("U8Xor"), Type::Todo);

        env.define_prim(Some("U16Eq"), Type::Todo);
        env.define_prim(Some("U16Neq"), Type::Todo);
        env.define_prim(Some("U16Lt"), Type::Todo);
        env.define_prim(Some("U16Gt"), Type::Todo);
        env.define_prim(Some("U16Lte"), Type::Todo);
        env.define_prim(Some("U16Gte"), Type::Todo);
        env.define_prim(Some("U16Add"), Type::Todo);
        env.define_prim(Some("U16Sub"), Type::Todo);
        env.define_prim(Some("U16Mul"), Type::Todo);
        env.define_prim(Some("U16Div"), Type::Todo);
        env.define_prim(Some("U16Not"), Type::Todo);
        env.define_prim(Some("U16Shl"), Type::Todo);
        env.define_prim(Some("U16Shr"), Type::Todo);
        env.define_prim(Some("U16And"), Type::Todo);
        env.define_prim(Some("U16Or"), Type::Todo);
        env.define_prim(Some("U16Xor"), Type::Todo);

        env.define_prim(Some("U32Eq"), Type::Todo);
        env.define_prim(Some("U32Neq"), Type::Todo);
        env.define_prim(Some("U32Lt"), Type::Todo);
        env.define_prim(Some("U32Gt"), Type::Todo);
        env.define_prim(Some("U32Lte"), Type::Todo);
        env.define_prim(Some("U32Gte"), Type::Todo);
        env.define_prim(Some("U32Add"), Type::Todo);
        env.define_prim(Some("U32Sub"), Type::Todo);
        env.define_prim(Some("U32Mul"), Type::Todo);
        env.define_prim(Some("U32Div"), Type::Todo);
        env.define_prim(Some("U32Not"), Type::Todo);
        env.define_prim(Some("U32Shl"), Type::Todo);
        env.define_prim(Some("U32Shr"), Type::Todo);
        env.define_prim(Some("U32And"), Type::Todo);
        env.define_prim(Some("U32Or"), Type::Todo);
        env.define_prim(Some("U32Xor"), Type::Todo);

        env.define_prim(Some("U64Eq"), Type::Todo);
        env.define_prim(Some("U64Neq"), Type::Todo);
        env.define_prim(Some("U64Lt"), Type::Todo);
        env.define_prim(Some("U64Gt"), Type::Todo);
        env.define_prim(Some("U64Lte"), Type::Todo);
        env.define_prim(Some("U64Gte"), Type::Todo);
        env.define_prim(Some("U64Add"), Type::Todo);
        env.define_prim(Some("U64Sub"), Type::Todo);
        env.define_prim(Some("U64Mul"), Type::Todo);
        env.define_prim(Some("U64Div"), Type::Todo);
        env.define_prim(Some("U64Not"), Type::Todo);
        env.define_prim(Some("U64Shl"), Type::Todo);
        env.define_prim(Some("U64Shr"), Type::Todo);
        env.define_prim(Some("U64And"), Type::Todo);
        env.define_prim(Some("U64Or"), Type::Todo);
        env.define_prim(Some("U64Xor"), Type::Todo);

        env.define_prim(Some("S8Eq"), Type::Todo);
        env.define_prim(Some("S8Neq"), Type::Todo);
        env.define_prim(Some("S8Lt"), Type::Todo);
        env.define_prim(Some("S8Gt"), Type::Todo);
        env.define_prim(Some("S8Lte"), Type::Todo);
        env.define_prim(Some("S8Gte"), Type::Todo);
        env.define_prim(Some("S8Neg"), Type::Todo);
        env.define_prim(Some("S8Add"), Type::Todo);
        env.define_prim(Some("S8Sub"), Type::Todo);
        env.define_prim(Some("S8Mul"), Type::Todo);
        env.define_prim(Some("S8Div"), Type::Todo);
        env.define_prim(Some("S8Abs"), Type::Todo);
        env.define_prim(Some("S8UAbs"), Type::Todo);

        env.define_prim(Some("S16Eq"), Type::Todo);
        env.define_prim(Some("S16Neq"), Type::Todo);
        env.define_prim(Some("S16Lt"), Type::Todo);
        env.define_prim(Some("S16Gt"), Type::Todo);
        env.define_prim(Some("S16Lte"), Type::Todo);
        env.define_prim(Some("S16Gte"), Type::Todo);
        env.define_prim(Some("S16Neg"), Type::Todo);
        env.define_prim(Some("S16Add"), Type::Todo);
        env.define_prim(Some("S16Sub"), Type::Todo);
        env.define_prim(Some("S16Mul"), Type::Todo);
        env.define_prim(Some("S16Div"), Type::Todo);
        env.define_prim(Some("S16Abs"), Type::Todo);
        env.define_prim(Some("S16UAbs"), Type::Todo);

        env.define_prim(Some("S32Eq"), Type::Todo);
        env.define_prim(Some("S32Neq"), Type::Todo);
        env.define_prim(Some("S32Lt"), Type::Todo);
        env.define_prim(Some("S32Gt"), Type::Todo);
        env.define_prim(Some("S32Lte"), Type::Todo);
        env.define_prim(Some("S32Gte"), Type::Todo);
        env.define_prim(Some("S32Neg"), Type::Todo);
        env.define_prim(Some("S32Add"), Type::Todo);
        env.define_prim(Some("S32Sub"), Type::Todo);
        env.define_prim(Some("S32Mul"), Type::Todo);
        env.define_prim(Some("S32Div"), Type::Todo);
        env.define_prim(Some("S32Abs"), Type::Todo);
        env.define_prim(Some("S32UAbs"), Type::Todo);

        env.define_prim(Some("S64Eq"), Type::Todo);
        env.define_prim(Some("S64Neq"), Type::Todo);
        env.define_prim(Some("S64Lt"), Type::Todo);
        env.define_prim(Some("S64Gt"), Type::Todo);
        env.define_prim(Some("S64Lte"), Type::Todo);
        env.define_prim(Some("S64Gte"), Type::Todo);
        env.define_prim(Some("S64Neg"), Type::Todo);
        env.define_prim(Some("S64Add"), Type::Todo);
        env.define_prim(Some("S64Sub"), Type::Todo);
        env.define_prim(Some("S64Mul"), Type::Todo);
        env.define_prim(Some("S64Div"), Type::Todo);
        env.define_prim(Some("S64Abs"), Type::Todo);
        env.define_prim(Some("S64UAbs"), Type::Todo);

        env.define_prim(Some("OptionSome"), Type::Todo);
        env.define_prim(Some("OptionNone"), Type::Todo);
        env.define_prim(Some("OptionFold"), Type::Todo);

        env.define_prim(Some("Array8Find"), Type::Todo);
        env.define_prim(Some("Array16Find"), Type::Todo);
        env.define_prim(Some("Array32Find"), Type::Todo);
        env.define_prim(Some("Array64Find"), Type::Todo);

        env.define_prim(Some("Array8Index"), Type::Todo);
        env.define_prim(Some("Array16Index"), Type::Todo);
        env.define_prim(Some("Array32Index"), Type::Todo);
        env.define_prim(Some("Array64Index"), Type::Todo);

        env.define_prim(Some("PosAddU8"), Type::Todo);
        env.define_prim(Some("PosAddU16"), Type::Todo);
        env.define_prim(Some("PosAddU32"), Type::Todo);
        env.define_prim(Some("PosAddU64"), Type::Todo);

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

    fn define_prim(&mut self, name: Option<&'static str>, ty: Type) {
        self.env.push_def(name.and_then(|s| self.name(s)), ty);
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
            items: UniqueEnv::new(),
            interner,
        }
    }

    pub fn compile_module(&mut self, module: &core::Module<'arena>) -> Result<Module, ()> {
        let mut m = Module { items: Vec::new() };
        for item in module.items {
            let name = self.compile_item(&mut m, item)?;
            self.items.push(name);
        }
        Ok(m)
    }

    fn compile_item(
        &mut self,
        module: &mut Module,
        item: &core::Item<'arena>,
    ) -> Result<StringId, ()> {
        match item {
            core::Item::Def {
                label,
                r#type: _type,
                expr,
            } => {
                let name = self
                    .interner
                    .borrow()
                    .resolve(*label)
                    .map(|name| name.to_pascal_case())
                    .expect("missing string");
                let name = self.interner.borrow_mut().get_or_intern(name);
                self.compile_def(module, name, expr)?;
                Ok(name)
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
            Term::ItemVar(_, _) => {}
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
                let mut borrows_data = false;
                for (label, format) in labels.iter().copied().zip(formats.iter()) {
                    let ty = self.compile_rep(format);
                    fields.push((label, ty.clone())); // FIXME: clone
                    borrows_data |= ty_borrows_data(&ty);
                    self.compile_env.push_def(Some(label), ty);
                }
                self.compile_env.truncate(initial_env_len);
                let r#struct = Struct {
                    name: label,
                    fields,
                    borrows_data,
                    fixed_size: self.fixed_size(expr),
                };
                dbg!(&r#struct);

                // Now generate the read function
                let read_fn = self.compile_read(&r#struct, expr);
                dbg!(&read_fn);
                module.items.push(Item::Struct(r#struct));
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

                    // (Prim::FormatArray32, [Elim::FunApp(len), Elim::FunApp(elem)]) => {
                    //     Value::prim(Prim::Array32Type, [len.clone(), self.format_repr(elem)])
                    // }
                    // Type::ReadArray32(len, ele) => Type::Array(len as usize, ele),

                    // Type::Todo => {}
                    _ => todo!("compile rep: {:?}", ty),
                }
            }
            Term::MetaVar(_, _) => todo! {},
            Term::InsertedMeta(_, _, _) => todo! {},
            Term::Ann(_, _, _) => todo! {},
            Term::Let(_, _, _, _, _) => todo! {},
            Term::Universe(_) => todo! {},
            Term::FunType(_, _, _, _) => todo! {},
            Term::FunLit(_, _, _) => todo! {},
            // Term::FunApp(
            //     _,
            //     Term::FunApp(_, head, input),
            //     ele_format,
            // ) => {
            //     dbg!(head);
            //     dbg!(input);
            //     dbg!(ele_format);
            //
            //     // How to compile function application?
            //     // In the simple case we're calling a primitive function, so know how to compile
            //     // that..?
            //     // let len = self.compile_rep(len); // ignore len for now as we might not know it
            //     // at this point
            //     let item_type = self.compile_rep(ele_format);
            //     // dbg!(("array32", len));
            //     Type::Vec(Box::new(item_type))
            // }
            Term::FunApp(_, head, input) => {
                let mut args = vec![*input];
                self.compile_fun_app(head, &mut args).expect("not prim")
            }
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

    fn compile_fun_app<'a>(
        &self,
        head: &'a Term<'arena>,
        args: &mut Vec<&'a Term<'arena>>,
    ) -> Result<Type, ()> {
        match head {
            Term::LocalVar(_, index) => {
                let ty = self
                    .compile_env
                    .types
                    .get_index(*index)
                    .expect("invalid rigid var")
                    .clone();
                dbg!(&ty);

                let args = self.compile_args(args);

                match (ty, args.as_slice()) {
                    (Type::PrimReadArray8, [_, ele]) => Ok(Type::ReadArray8(Box::new(ele.clone()))),
                    (Type::PrimReadArray16, [_, ele]) => {
                        Ok(Type::ReadArray16(Box::new(ele.clone())))
                    }
                    (Type::PrimReadArray32, [_, ele]) => {
                        Ok(Type::ReadArray32(Box::new(ele.clone())))
                    }
                    (Type::PrimReadArray64, [_, ele]) => {
                        Ok(Type::ReadArray64(Box::new(ele.clone())))
                    }
                    (Type::Todo, _) => todo!(),
                    (Type::Const(c), []) => todo!("const {:?}", c),
                    otherwise => panic!("invalid fun app {:?}", otherwise),
                }
            }
            Term::FunApp(_, head, input) => {
                args.push(input);
                self.compile_fun_app(head, args)
            }
            Term::Prim(_, prim) => {
                match prim {
                    // Prim::VoidType => unimplemented!(),
                    // Prim::BoolType => Type::Bool,
                    // Prim::U8Type => unimplemented!(),
                    // Prim::U16Type => unimplemented!(),
                    // Prim::U32Type => unimplemented!(),
                    // Prim::U64Type => unimplemented!(),
                    // Prim::S8Type => unimplemented!(),
                    // Prim::S16Type => unimplemented!(),
                    // Prim::S32Type => unimplemented!(),
                    // Prim::S64Type => unimplemented!(),
                    // Prim::F32Type => unimplemented!(),
                    // Prim::F64Type => unimplemented!(),
                    // Prim::OptionType => unimplemented!(),
                    // Prim::ArrayType => unimplemented!(),
                    // Prim::Array8Type => unimplemented!(),
                    // Prim::Array16Type => unimplemented!(),
                    // Prim::Array32Type => unimplemented!(),
                    // Prim::Array64Type => unimplemented!(),
                    // Prim::PosType => unimplemented!(),
                    // Prim::RefType => unimplemented!(),
                    Prim::FormatType => unimplemented!(),
                    Prim::FormatU8 => unimplemented!(),
                    Prim::FormatU16Be => unimplemented!(),
                    Prim::FormatU16Le => unimplemented!(),
                    Prim::FormatU32Be => unimplemented!(),
                    Prim::FormatU32Le => unimplemented!(),
                    Prim::FormatU64Be => unimplemented!(),
                    Prim::FormatU64Le => unimplemented!(),
                    Prim::FormatS8 => unimplemented!(),
                    Prim::FormatS16Be => unimplemented!(),
                    Prim::FormatS16Le => unimplemented!(),
                    Prim::FormatS32Be => unimplemented!(),
                    Prim::FormatS32Le => unimplemented!(),
                    Prim::FormatS64Be => unimplemented!(),
                    Prim::FormatS64Le => unimplemented!(),
                    Prim::FormatF32Be => unimplemented!(),
                    Prim::FormatF32Le => unimplemented!(),
                    Prim::FormatF64Be => unimplemented!(),
                    Prim::FormatF64Le => unimplemented!(),
                    Prim::FormatArray8 => unimplemented!(),
                    Prim::FormatArray16 => unimplemented!(),
                    // TODO: deal with args
                    Prim::FormatArray32 => unimplemented!(),
                    Prim::FormatArray64 => unimplemented!(),
                    // Prim::FormatRepeatUntilEnd => unimplemented!(),
                    // Prim::FormatLimit8 => unimplemented!(),
                    // Prim::FormatLimit16 => unimplemented!(),
                    // Prim::FormatLimit32 => unimplemented!(),
                    // Prim::FormatLimit64 => unimplemented!(),
                    // Prim::FormatStreamPos => unimplemented!(),
                    // Prim::FormatLink => unimplemented!(),
                    // Prim::FormatDeref => unimplemented!(),
                    // Prim::FormatSucceed => unimplemented!(),
                    // Prim::FormatFail => unimplemented!(),
                    // Prim::FormatUnwrap => unimplemented!(),
                    // Prim::FormatRepr => unimplemented!(),
                    // Prim::ReportedError => unimplemented!(),
                    // Prim::BoolEq => unimplemented!(),
                    // Prim::BoolNeq => unimplemented!(),
                    // Prim::BoolNot => unimplemented!(),
                    // Prim::BoolAnd => unimplemented!(),
                    // Prim::BoolOr => unimplemented!(),
                    // Prim::BoolXor => unimplemented!(),
                    // Prim::U8Eq => unimplemented!(),
                    // Prim::U8Neq => unimplemented!(),
                    // Prim::U8Gt => unimplemented!(),
                    // Prim::U8Lt => unimplemented!(),
                    // Prim::U8Gte => unimplemented!(),
                    // Prim::U8Lte => unimplemented!(),
                    // Prim::U8Add => unimplemented!(),
                    // Prim::U8Sub => unimplemented!(),
                    // Prim::U8Mul => unimplemented!(),
                    // Prim::U8Div => unimplemented!(),
                    // Prim::U8Not => unimplemented!(),
                    // Prim::U8Shl => unimplemented!(),
                    // Prim::U8Shr => unimplemented!(),
                    // Prim::U8And => unimplemented!(),
                    // Prim::U8Or => unimplemented!(),
                    // Prim::U8Xor => unimplemented!(),
                    // Prim::U16Eq => unimplemented!(),
                    // Prim::U16Neq => unimplemented!(),
                    // Prim::U16Gt => unimplemented!(),
                    // Prim::U16Lt => unimplemented!(),
                    // Prim::U16Gte => unimplemented!(),
                    // Prim::U16Lte => unimplemented!(),
                    // Prim::U16Add => unimplemented!(),
                    // Prim::U16Sub => unimplemented!(),
                    // Prim::U16Mul => unimplemented!(),
                    // Prim::U16Div => unimplemented!(),
                    // Prim::U16Not => unimplemented!(),
                    // Prim::U16Shl => unimplemented!(),
                    // Prim::U16Shr => unimplemented!(),
                    // Prim::U16And => unimplemented!(),
                    // Prim::U16Or => unimplemented!(),
                    // Prim::U16Xor => unimplemented!(),
                    // Prim::U32Eq => unimplemented!(),
                    // Prim::U32Neq => unimplemented!(),
                    // Prim::U32Gt => unimplemented!(),
                    // Prim::U32Lt => unimplemented!(),
                    // Prim::U32Gte => unimplemented!(),
                    // Prim::U32Lte => unimplemented!(),
                    // Prim::U32Add => unimplemented!(),
                    // Prim::U32Sub => unimplemented!(),
                    // Prim::U32Mul => unimplemented!(),
                    // Prim::U32Div => unimplemented!(),
                    // Prim::U32Not => unimplemented!(),
                    // Prim::U32Shl => unimplemented!(),
                    // Prim::U32Shr => unimplemented!(),
                    // Prim::U32And => unimplemented!(),
                    // Prim::U32Or => unimplemented!(),
                    // Prim::U32Xor => unimplemented!(),
                    // Prim::U64Eq => unimplemented!(),
                    // Prim::U64Neq => unimplemented!(),
                    // Prim::U64Gt => unimplemented!(),
                    // Prim::U64Lt => unimplemented!(),
                    // Prim::U64Gte => unimplemented!(),
                    // Prim::U64Lte => unimplemented!(),
                    // Prim::U64Add => unimplemented!(),
                    // Prim::U64Sub => unimplemented!(),
                    // Prim::U64Mul => unimplemented!(),
                    // Prim::U64Div => unimplemented!(),
                    // Prim::U64Not => unimplemented!(),
                    // Prim::U64Shl => unimplemented!(),
                    // Prim::U64Shr => unimplemented!(),
                    // Prim::U64And => unimplemented!(),
                    // Prim::U64Or => unimplemented!(),
                    // Prim::U64Xor => unimplemented!(),
                    // Prim::S8Eq => unimplemented!(),
                    // Prim::S8Neq => unimplemented!(),
                    // Prim::S8Gt => unimplemented!(),
                    // Prim::S8Lt => unimplemented!(),
                    // Prim::S8Gte => unimplemented!(),
                    // Prim::S8Lte => unimplemented!(),
                    // Prim::S8Neg => unimplemented!(),
                    // Prim::S8Add => unimplemented!(),
                    // Prim::S8Sub => unimplemented!(),
                    // Prim::S8Mul => unimplemented!(),
                    // Prim::S8Div => unimplemented!(),
                    // Prim::S8Abs => unimplemented!(),
                    // Prim::S8UAbs => unimplemented!(),
                    // Prim::S16Eq => unimplemented!(),
                    // Prim::S16Neq => unimplemented!(),
                    // Prim::S16Gt => unimplemented!(),
                    // Prim::S16Lt => unimplemented!(),
                    // Prim::S16Gte => unimplemented!(),
                    // Prim::S16Lte => unimplemented!(),
                    // Prim::S16Neg => unimplemented!(),
                    // Prim::S16Add => unimplemented!(),
                    // Prim::S16Sub => unimplemented!(),
                    // Prim::S16Mul => unimplemented!(),
                    // Prim::S16Div => unimplemented!(),
                    // Prim::S16Abs => unimplemented!(),
                    // Prim::S16UAbs => unimplemented!(),
                    // Prim::S32Eq => unimplemented!(),
                    // Prim::S32Neq => unimplemented!(),
                    // Prim::S32Gt => unimplemented!(),
                    // Prim::S32Lt => unimplemented!(),
                    // Prim::S32Gte => unimplemented!(),
                    // Prim::S32Lte => unimplemented!(),
                    // Prim::S32Neg => unimplemented!(),
                    // Prim::S32Add => unimplemented!(),
                    // Prim::S32Sub => unimplemented!(),
                    // Prim::S32Mul => unimplemented!(),
                    // Prim::S32Div => unimplemented!(),
                    // Prim::S32Abs => unimplemented!(),
                    // Prim::S32UAbs => unimplemented!(),
                    // Prim::S64Eq => unimplemented!(),
                    // Prim::S64Neq => unimplemented!(),
                    // Prim::S64Gt => unimplemented!(),
                    // Prim::S64Lt => unimplemented!(),
                    // Prim::S64Gte => unimplemented!(),
                    // Prim::S64Lte => unimplemented!(),
                    // Prim::S64Neg => unimplemented!(),
                    // Prim::S64Add => unimplemented!(),
                    // Prim::S64Sub => unimplemented!(),
                    // Prim::S64Mul => unimplemented!(),
                    // Prim::S64Div => unimplemented!(),
                    // Prim::S64Abs => unimplemented!(),
                    // Prim::S64UAbs => unimplemented!(),
                    // Prim::OptionSome => unimplemented!(),
                    // Prim::OptionNone => unimplemented!(),
                    // Prim::OptionFold => unimplemented!(),
                    // Prim::Array8Find => unimplemented!(),
                    // Prim::Array16Find => unimplemented!(),
                    // Prim::Array32Find => unimplemented!(),
                    // Prim::Array64Find => unimplemented!(),
                    // Prim::Array8Index => unimplemented!(),
                    // Prim::Array16Index => unimplemented!(),
                    // Prim::Array32Index => unimplemented!(),
                    // Prim::Array64Index => unimplemented!(),
                    // Prim::PosAddU8 => unimplemented!(),
                    // Prim::PosAddU16 => unimplemented!(),
                    // Prim::PosAddU32 => unimplemented!(),
                    // Prim::PosAddU64 => unimplemented!(),
                    _ => unimplemented!(),
                }
            }
            _ => Err(()),
        }
        // todo!()
    }

    // TODO: Rename prim_repr or something
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

    // Generate the read function for a format
    fn compile_read(&mut self, r#struct: &Struct, format: &Term<'arena>) -> ReadFn {
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

                let name = r#struct.name;
                let read_struct = ReadStruct {
                    name,
                    borrows_data: r#struct.borrows_data,
                    fixed_size: r#struct.fixed_size,
                };
                let st = ReadExpr::Struct {
                    // name of the struct
                    name,
                    // Parse a type into a named variable
                    parse_fields,
                    // How to initialise the fields of the struct using the variables
                    fields,
                };
                ReadFn {
                    r#struct: read_struct,
                    exprs: vec![st],
                }
            }
            _ => unreachable!("can only compile read for format records"),
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
            Term::FunApp(_, head, input) => {
                let mut args = vec![*input];
                self.read_fun_app(head, &mut args)
                    .expect("read_fun_app failed")
            }
            _ => unimplemented!("format: {:?}", format),
        }
    }

    fn read_fun_app<'a>(
        &self,
        head: &'a Term<'arena>,
        args: &mut Vec<&'a Term<'arena>>,
    ) -> Result<Type, ()> {
        match head {
            Term::LocalVar(_, index) => {
                let ty = self
                    .compile_env
                    .types
                    .get_index(*index)
                    .expect("invalid rigid var")
                    .clone();
                let args = self.compile_args(args);

                match (ty, args.as_slice()) {
                    (Type::PrimReadArray8, [len, ele]) => Ok(Type::DoReadArray8(
                        Box::new(len.clone()),
                        Box::new(ele.clone()),
                    )),
                    (Type::PrimReadArray16, [len, ele]) => Ok(Type::DoReadArray16(
                        Box::new(len.clone()),
                        Box::new(ele.clone()),
                    )),
                    (Type::PrimReadArray32, [len, ele]) => Ok(Type::DoReadArray32(
                        Box::new(len.clone()),
                        Box::new(ele.clone()),
                    )),
                    (Type::PrimReadArray64, [len, ele]) => Ok(Type::DoReadArray64(
                        Box::new(len.clone()),
                        Box::new(ele.clone()),
                    )),
                    (Type::Todo, _) => todo!(),
                    (otherwise, ele) => panic!("invalid fun app {:?}, {:?}", otherwise, ele),
                }
            }
            Term::FunApp(_, head, input) => {
                args.push(input);
                self.read_fun_app(head, args)
            }
            Term::Prim(_, prim) => {
                match prim {
                    // Prim::VoidType => unimplemented!(),
                    // Prim::BoolType => Type::Bool,
                    // Prim::U8Type => unimplemented!(),
                    // Prim::U16Type => unimplemented!(),
                    // Prim::U32Type => unimplemented!(),
                    // Prim::U64Type => unimplemented!(),
                    // Prim::S8Type => unimplemented!(),
                    // Prim::S16Type => unimplemented!(),
                    // Prim::S32Type => unimplemented!(),
                    // Prim::S64Type => unimplemented!(),
                    // Prim::F32Type => unimplemented!(),
                    // Prim::F64Type => unimplemented!(),
                    // Prim::OptionType => unimplemented!(),
                    // Prim::ArrayType => unimplemented!(),
                    // Prim::Array8Type => unimplemented!(),
                    // Prim::Array16Type => unimplemented!(),
                    // Prim::Array32Type => unimplemented!(),
                    // Prim::Array64Type => unimplemented!(),
                    // Prim::PosType => unimplemented!(),
                    // Prim::RefType => unimplemented!(),
                    Prim::FormatType => unimplemented!(),
                    Prim::FormatU8 => unimplemented!(),
                    Prim::FormatU16Be => unimplemented!(),
                    Prim::FormatU16Le => unimplemented!(),
                    Prim::FormatU32Be => unimplemented!(),
                    Prim::FormatU32Le => unimplemented!(),
                    Prim::FormatU64Be => unimplemented!(),
                    Prim::FormatU64Le => unimplemented!(),
                    Prim::FormatS8 => unimplemented!(),
                    Prim::FormatS16Be => unimplemented!(),
                    Prim::FormatS16Le => unimplemented!(),
                    Prim::FormatS32Be => unimplemented!(),
                    Prim::FormatS32Le => unimplemented!(),
                    Prim::FormatS64Be => unimplemented!(),
                    Prim::FormatS64Le => unimplemented!(),
                    Prim::FormatF32Be => unimplemented!(),
                    Prim::FormatF32Le => unimplemented!(),
                    Prim::FormatF64Be => unimplemented!(),
                    Prim::FormatF64Le => unimplemented!(),
                    Prim::FormatArray8 => unimplemented!(),
                    Prim::FormatArray16 => unimplemented!(),
                    // TODO: deal with args
                    Prim::FormatArray32 => unimplemented!(),
                    Prim::FormatArray64 => unimplemented!(),
                    // Prim::FormatRepeatUntilEnd => unimplemented!(),
                    // Prim::FormatLimit8 => unimplemented!(),
                    // Prim::FormatLimit16 => unimplemented!(),
                    // Prim::FormatLimit32 => unimplemented!(),
                    // Prim::FormatLimit64 => unimplemented!(),
                    // Prim::FormatStreamPos => unimplemented!(),
                    // Prim::FormatLink => unimplemented!(),
                    // Prim::FormatDeref => unimplemented!(),
                    // Prim::FormatSucceed => unimplemented!(),
                    // Prim::FormatFail => unimplemented!(),
                    // Prim::FormatUnwrap => unimplemented!(),
                    // Prim::FormatRepr => unimplemented!(),
                    // Prim::ReportedError => unimplemented!(),
                    // Prim::BoolEq => unimplemented!(),
                    // Prim::BoolNeq => unimplemented!(),
                    // Prim::BoolNot => unimplemented!(),
                    // Prim::BoolAnd => unimplemented!(),
                    // Prim::BoolOr => unimplemented!(),
                    // Prim::BoolXor => unimplemented!(),
                    // Prim::U8Eq => unimplemented!(),
                    // Prim::U8Neq => unimplemented!(),
                    // Prim::U8Gt => unimplemented!(),
                    // Prim::U8Lt => unimplemented!(),
                    // Prim::U8Gte => unimplemented!(),
                    // Prim::U8Lte => unimplemented!(),
                    // Prim::U8Add => unimplemented!(),
                    // Prim::U8Sub => unimplemented!(),
                    // Prim::U8Mul => unimplemented!(),
                    // Prim::U8Div => unimplemented!(),
                    // Prim::U8Not => unimplemented!(),
                    // Prim::U8Shl => unimplemented!(),
                    // Prim::U8Shr => unimplemented!(),
                    // Prim::U8And => unimplemented!(),
                    // Prim::U8Or => unimplemented!(),
                    // Prim::U8Xor => unimplemented!(),
                    // Prim::U16Eq => unimplemented!(),
                    // Prim::U16Neq => unimplemented!(),
                    // Prim::U16Gt => unimplemented!(),
                    // Prim::U16Lt => unimplemented!(),
                    // Prim::U16Gte => unimplemented!(),
                    // Prim::U16Lte => unimplemented!(),
                    // Prim::U16Add => unimplemented!(),
                    // Prim::U16Sub => unimplemented!(),
                    // Prim::U16Mul => unimplemented!(),
                    // Prim::U16Div => unimplemented!(),
                    // Prim::U16Not => unimplemented!(),
                    // Prim::U16Shl => unimplemented!(),
                    // Prim::U16Shr => unimplemented!(),
                    // Prim::U16And => unimplemented!(),
                    // Prim::U16Or => unimplemented!(),
                    // Prim::U16Xor => unimplemented!(),
                    // Prim::U32Eq => unimplemented!(),
                    // Prim::U32Neq => unimplemented!(),
                    // Prim::U32Gt => unimplemented!(),
                    // Prim::U32Lt => unimplemented!(),
                    // Prim::U32Gte => unimplemented!(),
                    // Prim::U32Lte => unimplemented!(),
                    // Prim::U32Add => unimplemented!(),
                    // Prim::U32Sub => unimplemented!(),
                    // Prim::U32Mul => unimplemented!(),
                    // Prim::U32Div => unimplemented!(),
                    // Prim::U32Not => unimplemented!(),
                    // Prim::U32Shl => unimplemented!(),
                    // Prim::U32Shr => unimplemented!(),
                    // Prim::U32And => unimplemented!(),
                    // Prim::U32Or => unimplemented!(),
                    // Prim::U32Xor => unimplemented!(),
                    // Prim::U64Eq => unimplemented!(),
                    // Prim::U64Neq => unimplemented!(),
                    // Prim::U64Gt => unimplemented!(),
                    // Prim::U64Lt => unimplemented!(),
                    // Prim::U64Gte => unimplemented!(),
                    // Prim::U64Lte => unimplemented!(),
                    // Prim::U64Add => unimplemented!(),
                    // Prim::U64Sub => unimplemented!(),
                    // Prim::U64Mul => unimplemented!(),
                    // Prim::U64Div => unimplemented!(),
                    // Prim::U64Not => unimplemented!(),
                    // Prim::U64Shl => unimplemented!(),
                    // Prim::U64Shr => unimplemented!(),
                    // Prim::U64And => unimplemented!(),
                    // Prim::U64Or => unimplemented!(),
                    // Prim::U64Xor => unimplemented!(),
                    // Prim::S8Eq => unimplemented!(),
                    // Prim::S8Neq => unimplemented!(),
                    // Prim::S8Gt => unimplemented!(),
                    // Prim::S8Lt => unimplemented!(),
                    // Prim::S8Gte => unimplemented!(),
                    // Prim::S8Lte => unimplemented!(),
                    // Prim::S8Neg => unimplemented!(),
                    // Prim::S8Add => unimplemented!(),
                    // Prim::S8Sub => unimplemented!(),
                    // Prim::S8Mul => unimplemented!(),
                    // Prim::S8Div => unimplemented!(),
                    // Prim::S8Abs => unimplemented!(),
                    // Prim::S8UAbs => unimplemented!(),
                    // Prim::S16Eq => unimplemented!(),
                    // Prim::S16Neq => unimplemented!(),
                    // Prim::S16Gt => unimplemented!(),
                    // Prim::S16Lt => unimplemented!(),
                    // Prim::S16Gte => unimplemented!(),
                    // Prim::S16Lte => unimplemented!(),
                    // Prim::S16Neg => unimplemented!(),
                    // Prim::S16Add => unimplemented!(),
                    // Prim::S16Sub => unimplemented!(),
                    // Prim::S16Mul => unimplemented!(),
                    // Prim::S16Div => unimplemented!(),
                    // Prim::S16Abs => unimplemented!(),
                    // Prim::S16UAbs => unimplemented!(),
                    // Prim::S32Eq => unimplemented!(),
                    // Prim::S32Neq => unimplemented!(),
                    // Prim::S32Gt => unimplemented!(),
                    // Prim::S32Lt => unimplemented!(),
                    // Prim::S32Gte => unimplemented!(),
                    // Prim::S32Lte => unimplemented!(),
                    // Prim::S32Neg => unimplemented!(),
                    // Prim::S32Add => unimplemented!(),
                    // Prim::S32Sub => unimplemented!(),
                    // Prim::S32Mul => unimplemented!(),
                    // Prim::S32Div => unimplemented!(),
                    // Prim::S32Abs => unimplemented!(),
                    // Prim::S32UAbs => unimplemented!(),
                    // Prim::S64Eq => unimplemented!(),
                    // Prim::S64Neq => unimplemented!(),
                    // Prim::S64Gt => unimplemented!(),
                    // Prim::S64Lt => unimplemented!(),
                    // Prim::S64Gte => unimplemented!(),
                    // Prim::S64Lte => unimplemented!(),
                    // Prim::S64Neg => unimplemented!(),
                    // Prim::S64Add => unimplemented!(),
                    // Prim::S64Sub => unimplemented!(),
                    // Prim::S64Mul => unimplemented!(),
                    // Prim::S64Div => unimplemented!(),
                    // Prim::S64Abs => unimplemented!(),
                    // Prim::S64UAbs => unimplemented!(),
                    // Prim::OptionSome => unimplemented!(),
                    // Prim::OptionNone => unimplemented!(),
                    // Prim::OptionFold => unimplemented!(),
                    // Prim::Array8Find => unimplemented!(),
                    // Prim::Array16Find => unimplemented!(),
                    // Prim::Array32Find => unimplemented!(),
                    // Prim::Array64Find => unimplemented!(),
                    // Prim::Array8Index => unimplemented!(),
                    // Prim::Array16Index => unimplemented!(),
                    // Prim::Array32Index => unimplemented!(),
                    // Prim::Array64Index => unimplemented!(),
                    // Prim::PosAddU8 => unimplemented!(),
                    // Prim::PosAddU16 => unimplemented!(),
                    // Prim::PosAddU32 => unimplemented!(),
                    // Prim::PosAddU64 => unimplemented!(),
                    _ => unimplemented!(),
                }
            }
            _ => Err(()),
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
    fn compile_args(&self, args: &[&Term]) -> Vec<ParseExpr> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args.into_iter().rev() {
            let ty = match *arg {
                Term::ItemVar(_, var) => {
                    let name = self.items.get_level(*var).expect("missing item");
                    ParseExpr::Var(*name)
                }
                Term::LocalVar(_, index) => self
                    .compile_env
                    .names
                    .get_index(*index)
                    .expect("invalid local var")
                    .map(|name| ParseExpr::Var(name))
                    .unwrap_or_else(|| {
                        ParseExpr::Const(
                            self.compile_env
                                .types
                                .get_index(*index)
                                .expect("invalid local var")
                                .clone(),
                        )
                    }),
                // Term::MetaVar(_, _) => {}
                // Term::InsertedMeta(_, _, _) => {}
                // Term::Ann(_, _, _) => {}
                // Term::Let(_, _, _, _, _) => {}
                // Term::Universe(_) => {}
                // Term::FunType(_, _, _, _) => {}
                // Term::FunLit(_, _, _) => {}
                // Term::FunApp(_, _, _) => {}
                // Term::RecordType(_, _, _) => {}
                // Term::RecordLit(_, _, _) => {}
                // Term::RecordProj(_, _, _) => {}
                // Term::ArrayLit(_, _) => {}
                // Term::FormatRecord(_, _, _) => {}
                // Term::FormatCond(_, _, _, _) => {}
                // Term::FormatOverlap(_, _, _) => {}
                // Term::Prim(_, _) => {}
                Term::ConstLit(_, val) => ParseExpr::Const(Type::Const(*val)),
                // Term::ConstMatch(_, _, _, _) => {}
                otherwise => todo!("compile arg {:?}", otherwise),
            };
            out.push(ty);
        }
        out
    }

    fn fixed_size(&mut self, format: &Term<'arena>) -> Option<usize> {
        match format {
            Term::ItemVar(_, _) => unimplemented! {},
            Term::LocalVar(_, var) => {
                let ty = self
                    .compile_env
                    .types
                    .get_index(*var)
                    .expect("invalid rigid var")
                    .clone();

                // Get the size for this
                match ty {
                    Type::ReadU8 => Some(1),
                    Type::ReadU16Be => Some(2),
                    Type::ReadU16Le => Some(2),
                    Type::ReadU32Be => Some(4),
                    Type::ReadU32Le => Some(4),
                    Type::ReadU64Be => Some(8),
                    Type::ReadU64Le => Some(8),
                    Type::ReadI8 => Some(1),
                    Type::ReadI16Be => Some(2),
                    Type::ReadI16Le => Some(2),
                    Type::ReadI32Be => Some(4),
                    Type::ReadI32Le => Some(4),
                    Type::ReadI64Be => Some(8),
                    Type::ReadI64Le => Some(8),
                    Type::ReadF32Be => Some(4),
                    Type::ReadF32Le => Some(4),
                    Type::ReadF64Be => Some(8),
                    Type::ReadF64Le => Some(8),

                    Type::Todo => None,
                    _ => todo!("fixed size: {:?}", ty),
                }
            }
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
                let mut size = 0;
                for field in *formats {
                    size += self.fixed_size(field)?;
                }
                Some(size)
            }
            Term::FormatCond(_, _, _, _) => unimplemented! {},
            Term::FormatOverlap(_, _, _) => unimplemented! {},
            Term::Prim(_, _) => unimplemented! {},
            Term::ConstLit(_, _) => unimplemented! {},
            Term::ConstMatch(_, _, _, _) => unimplemented! {},
        }
    }
}

fn ty_borrows_data(ty: &Type) -> bool {
    match ty {
        Type::ReadArray8(_)
        | Type::ReadArray16(_)
        | Type::ReadArray32(_)
        | Type::ReadArray64(_) => true,
        _ => false,
    }
}

pub mod rust {
    use std::cell::RefCell;

    use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
    use scoped_arena::Scope;

    use crate::core::compile::{Item, Module, ParseExpr, ReadExpr, ReadFn, Type};
    use crate::core::Const;
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
                Item::Var(name) => self.string_id(*name),
                Item::Struct(r#struct) => self.sequence(
                    self.concat([
                        self.text("struct"),
                        self.space(),
                        self.string_id(r#struct.name),
                        if r#struct.borrows_data {
                            self.text("<'a>")
                        } else {
                            self.text("") // FIXME: better way to do this?
                        },
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
                    if readfn.r#struct.fixed_size.is_some() {
                        self.read_from_impl(readfn)
                    } else {
                        self.read_binary_impl(readfn)
                    }
                }
            }
        }

        fn read_from_impl(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
            // Collect the read type of each field
            let docs = vec![
                self.concat([
                    self.text("type ReadType = "),
                    self.read_types(readfn),
                    self.text(";"),
                    self.hardline(),
                ]),
                self.read_from_fn(readfn),
            ];
            self.concat([
                self.text("impl<'a> ReadFrom<'a> for"),
                self.space(),
                self.string_id(readfn.r#struct.name),
                if readfn.r#struct.borrows_data {
                    self.text("<'a>")
                } else {
                    self.text("") // FIXME: better way to do this?
                },
                self.space(),
                self.text("{"),
                self.hardline().nest(INDENT),
                self.intersperse(docs, self.hardline()).nest(INDENT),
                self.hardline(),
                self.text("}"),
            ])
        }

        fn read_binary_impl(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
            let docs = vec![
                self.concat([self.text("type HostType = Self;"), self.hardline()]),
                self.read_fn(readfn),
            ];

            self.concat([
                self.text("impl<'a> ReadBinary<'a> for"),
                self.space(),
                self.string_id(readfn.r#struct.name),
                if readfn.r#struct.borrows_data {
                    self.text("<'a>")
                } else {
                    self.text("") // FIXME: better way to do this?
                },
                self.space(),
                self.text("{"),
                self.hardline().nest(INDENT),
                self.intersperse(docs, self.hardline()).nest(INDENT),
                self.hardline(),
                self.text("}"),
            ])
        }

        fn imports(&'arena self) -> DocBuilder<'arena, Self> {
            self.concat([
                self.text("use fathom_runtime::prelude::*;"),
                self.hardline(),
            ])
        }

        fn read_fn(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
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

        fn read_from_fn(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
            let args = self.read_types_repr(readfn);
            self.concat([
                self.text("fn from(args: "),
                args,
                self.text(") -> Self {"),
                self.hardline().nest(INDENT),
                self.intersperse(
                    readfn.exprs.iter().map(|expr| self.from_expr(expr)),
                    self.concat([self.text(";"), self.hardline()]),
                )
                .nest(INDENT),
                self.hardline(),
                self.text("}"),
            ])
        }

        fn read_types(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
            self.concat([
                self.text("("),
                self.intersperse(
                    readfn.exprs.iter().map(|expr| self.read_type(expr)),
                    self.text(", "),
                ),
                self.text(")"),
            ])
        }

        fn read_type(&'arena self, expr: &ReadExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ReadExpr::Struct {
                    name,
                    parse_fields,
                    fields,
                } => self.intersperse(
                    parse_fields
                        .iter()
                        .map(|(_name, field)| self.parse_expr_prec(field)),
                    self.text(", "),
                ),
            }
        }

        fn read_types_repr(&'arena self, readfn: &ReadFn) -> DocBuilder<'arena, Self> {
            self.concat([
                self.text("("),
                self.intersperse(
                    readfn.exprs.iter().map(|expr| self.read_type_repr(expr)),
                    self.text(", "),
                ),
                self.text(")"),
            ])
        }

        fn read_type_repr(&'arena self, expr: &ReadExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ReadExpr::Struct {
                    name,
                    parse_fields,
                    fields,
                } => self.intersperse(
                    parse_fields
                        .iter()
                        .map(|(_name, field)| self.parse_expr_prec(&field.repr())),
                    self.text(", "),
                ),
            }
        }

        // This is used for defining variables and struct fields
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
                Type::ReadU8 => self.text("U8"),
                Type::ReadU16Be => self.text("U16Be"),
                Type::ReadU32Be => self.text("U32Be"),
                Type::ReadU64Be => self.text("U64Be"),
                Type::ReadU16Le => self.text("U32Le"),
                Type::ReadU32Le => self.text("U32Le"),
                Type::ReadU64Le => self.text("U64Le"),
                Type::ReadArray8(ele)
                | Type::ReadArray16(ele)
                | Type::ReadArray32(ele)
                | Type::ReadArray64(ele) => self.concat([
                    self.text("ReadArray<'a, "),
                    self.parse_expr_prec(ele),
                    self.text(">"),
                ]),
                Type::Todo => self.text("todo!()"),
                otherwise => unimplemented!("ty_prec {:?}", otherwise),
            }
        }

        fn expr(&'arena self, expr: &ReadExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ReadExpr::Struct {
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

        fn from_expr(&'arena self, expr: &ReadExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ReadExpr::Struct {
                    name,
                    parse_fields,
                    fields,
                } => self.concat([self.construct_struct_from(*name, fields)]),
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
                    self.text(";"),
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

        fn construct_struct_from(
            &'arena self,
            name: StringId,
            fields: &[(StringId, ParseExpr)],
        ) -> DocBuilder<'arena, Self> {
            self.sequence(
                self.concat([self.string_id(name), self.space(), self.text("{")]),
                fields.iter().enumerate().map(|(i, (name, field))| {
                    self.concat([
                        self.string_id(*name),
                        self.text(":"),
                        self.space(),
                        self.text("args."),
                        self.text(i.to_string()),
                    ])
                }),
                self.text(","),
                self.text("}"),
            )
        }

        fn parse_expr(&'arena self, expr: &ParseExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ParseExpr::Const(ty) => self.type_const(ty),
                ParseExpr::Var(name) => self.string_id(*name),
            }
        }

        fn parse_expr_prec(&'arena self, expr: &ParseExpr) -> DocBuilder<'arena, Self> {
            match expr {
                ParseExpr::Const(ty) => self.ty_prec((), ty),
                ParseExpr::Var(name) => self.string_id(*name),
            }
        }

        fn type_const(&'arena self, ty_const: &Type) -> DocBuilder<'arena, Self> {
            match ty_const {
                Type::ReadU8 => self.text("ctxt.read_u8()?"),
                Type::ReadU16Be => self.text("ctxt.read_u16be()?"),
                Type::ReadU32Be => self.text("ctxt.read_u32be()?"),
                Type::ReadU64Be => self.text("ctxt.read_u64be()?"),
                Type::ReadU16Le => self.text("ctxt.read_u16le()?"),
                Type::ReadU32Le => self.text("ctxt.read_u32le()?"),
                Type::ReadU64Le => self.text("ctxt.read_u64le()?"),
                Type::DoReadArray8(len, ele)
                | Type::DoReadArray16(len, ele)
                | Type::DoReadArray32(len, ele)
                | Type::DoReadArray64(len, ele) => self.concat([
                    self.text("ctxt.read_array::<"),
                    self.parse_expr_prec(ele),
                    self.text(">("),
                    self.parse_expr(len),
                    self.text(" as usize /* FIXME: cast */)?"),
                ]),
                Type::Const(Const::U8(val, style)) => self.text(style.format(val)),
                Type::Const(Const::U16(val, style)) => self.text(style.format(val)),
                Type::Const(Const::U32(val, style)) => self.text(style.format(val)),
                Type::Const(Const::U64(val, style)) => self.text(style.format(val)),
                otherwise => unimplemented!("type_const {:?}", otherwise),
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
