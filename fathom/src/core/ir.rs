use crate::core::binary::BufferError;
use crate::source::StringId;

type Pos = u64;

#[derive(Debug, Clone, Copy)]
pub enum Const {
    U8(u8),
    U16(u16),
    S16(i16),
    U32(u32),
    F32(f32),
    Pos(Pos),
}

// TODO: better name
pub(crate) mod host {
    use super::{Const, StringId};

    #[derive(Debug, Clone)]
    pub enum Type {
        /// A built in type
        Prim(Prim),
        /// A custom type that lives in the context
        CustomType(usize),
    }

    // A host side custom type
    #[derive(Debug, Clone)]
    pub enum CustomType {
        Record(Record),
        Enum(Enum),
    }

    #[derive(Debug, Clone)]
    pub struct Record {
        pub name: StringId,
        pub fields: Vec<Field>,
    }

    // a match that can yield different types compiles into an enum
    #[derive(Debug, Clone)]
    pub struct Enum {
        pub name: StringId,
        pub variants: Vec<Variant>,
    }

    #[derive(Debug, Clone)]
    pub struct Variant {
        pub name: StringId,
        pub data: Option<Type>,
    }

    #[derive(Debug, Clone)]
    pub struct Field {
        pub name: StringId,
        pub host_type: Type, // in theory there could also be write: WriteExpr
    }

    /// Primitive types
    #[derive(Debug, Clone)]
    pub enum Prim {
        S16,
        U8,
        U16,
        U32,
        F32,
        Pos,
        // An array with length and element type
        // The length can be another item or a const
        Array(Expr, Box<Type>), // Perhaps array with const length and array with non-const length should be separated
    }

    // Primitive functions... is it sensible to separate these from Prim?
    #[derive(Debug, Clone)]
    pub enum PrimFn {
        U16Sub,
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
        /// Item in the context
        Item(usize),
        Const(Const),
        // Prim(Prim),
        PrimFn(PrimFn),

        /// A function literal
        Func(Function),
        // TODO: Does there need to be a counterpart to this in the `format` module?
        /// Uncurried function application (fn, arguments)
        App(Box<Expr>, Vec<Expr>),
    }

    #[derive(Debug, Clone)]
    pub struct Function {
        /// Argument names
        arguments: Vec<StringId>,
        /// Body expression
        body: Box<Expr>,
    }
}

// TODO: better name
pub(crate) mod format {
    use super::{host, Const, Pos, StringId};

    #[derive(Debug, Clone)]
    pub struct Module {
        pub definitions: Vec<Def>,
    }

    #[derive(Debug, Clone)]
    pub struct Def {
        pub name: StringId,
        pub expr: Item,
    }

    #[derive(Debug, Clone)]
    pub enum Item {
        Format(Format),
        /// A let expression
        ///
        /// name, def expression, body expression
        Let(StringId, Box<Item>, Box<Item>),
    }

    #[derive(Debug, Clone)]
    pub struct Format {
        pub params: Vec<Param>,
        pub fields: Vec<Field>,
    }

    #[derive(Debug, Clone)]
    pub struct Param {
        pub name: StringId,
        pub host_type: host::Type,
    }

    #[derive(Debug, Clone)]
    pub struct Field {
        pub name: StringId,
        pub host_type: host::Type,
        pub read: ReadExpr,
    }

    #[derive(Debug, Clone)]
    pub struct ReadFn {
        pub exprs: Vec<ReadExpr>,
    }

    #[derive(Debug, Clone)]
    pub enum ReadExpr {
        Prim(ReadPrim),
        /// A custom type that lives in the context
        CustomType(usize),
        /// A match expression, cond, branches, default?
        Match(host::Expr, Vec<Branch>),
    }

    #[derive(Debug, Clone)]
    pub struct Branch {
        pub pattern: Pattern,
        pub expr: Box<ReadExpr>,
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Var(StringId), // may want to use something better than strings here
        Const(Const),
    }

    #[derive(Debug, Clone)]
    pub enum ReadPrim {
        S16Be,
        S16Le,
        U8,
        U16Be,
        U16Le,
        U32Be,
        U32Le,
        F32Le,
        Array(host::Expr, Box<ReadExpr>),
        StreamPos,
    }

    // impl Format {
    //     fn repr(&self) -> host::CustomType {
    //         let fields = self
    //             .fields
    //             .iter()
    //             .map(|field| host::Field {
    //                 name: field.name,
    //                 host_type: field.host_type.clone(),
    //             })
    //             .collect();
    //         host::CustomType::Record(host::Record {
    //             name: self.name,
    //             fields,
    //         })
    //     }
    // }
}

pub(crate) mod interpret {
    use super::*;
    use crate::core::binary;
    use format::*;
    use host::Expr;
    use string_interner::Symbol;

    use crate::core::binary::{Buffer, BufferError, BufferReader, ReadError};
    use crate::source::StringInterner;

    pub struct Interpreter<'data, 'interner> {
        buffer: Buffer<'data>,
        interner: &'interner StringInterner,
        item_env: Vec<(StringId, Val)>,
        defs: Vec<Def>,
    }

    #[derive(Debug, Clone)]
    pub enum Val {
        Record(RecordVal),
        Const(Const),
        Vec(Vec<Val>),
    }

    #[derive(Debug, Clone)]
    pub struct RecordVal {
        pub name: StringId,
        pub fields: Vec<FieldVal>,
    }

    #[derive(Debug, Clone)]
    pub struct FieldVal {
        pub name: StringId,
        pub value: Box<Val>,
    }

    // FIXME: Is this even needed?
    // I.e. can it just parse into the value of the selected variant
    // would we ever have variants without data?
    #[derive(Debug, Clone)]
    pub struct EnumVal {
        pub tag: u8,
        pub value: Val,
    }

    #[derive(Debug)]
    pub enum Error {
        UnknownItem,
        BadIndex,
        BufferError(BufferError),
        /// Value exceeds the range of the target type
        OutOfRange,
    }

    impl<'data, 'interner> Interpreter<'data, 'interner> {
        pub fn new(buffer: Buffer<'data>, interner: &'interner StringInterner) -> Self {
            Interpreter {
                buffer,
                interner,
                item_env: Vec::new(),
                defs: Vec::new(),
            }
        }

        pub fn interpret(&mut self, module: &Module) -> Result<Val, Error> {
            // Hmm so the definitions need to be evaluated first so that they are present in the
            // item env (since main refers to them by index).
            self.defs = module.definitions.clone(); // FIXME

            println!("Symbols:");
            let mut i = 0;
            while let Some(s) = self.interner.resolve(StringId::try_from_usize(i).unwrap()) {
                println!("- {}: '{}'", i, s);
                i += 1;
            }

            // find the "main" def, which is the entry point
            // TODO: Perhaps it should actually be fished out of the defs?
            let main_sym = self.interner.get("main").expect("no main symbol");
            let main = module
                .definitions
                .iter()
                .find(|def| def.name == main_sym)
                .expect("unable to find main item in module");

            let mut reader = self.buffer.reader();
            self.interpret_item(Some(main.name), &main.expr, &mut reader)
        }

        fn interpret_item(
            &mut self,
            name: Option<StringId>,
            item: &Item,
            reader: &mut BufferReader,
        ) -> Result<Val, Error> {
            match item {
                Item::Format(Format {
                    params: _params,
                    fields,
                }) => {
                    // TODO: handle params
                    let len_before = self.item_env.len();
                    let mut ffields = Vec::new();
                    for field in fields {
                        let val = self.read(&field.read, reader)?;
                        self.item_env.push((field.name, val.clone()));
                        ffields.push(FieldVal {
                            name: field.name,
                            value: Box::new(val),
                        });
                    }
                    self.item_env.truncate(len_before);
                    let record = RecordVal {
                        name: name.expect("no name"),
                        fields: ffields,
                    };

                    Ok(Val::Record(record))
                }
                Item::Let(name, def_expression, body_expression) => {
                    // eval the def_expression, then add the result to the env and eval body_expression
                    let len_before = self.item_env.len();
                    let val = self.interpret_item(None, def_expression, reader)?;
                    self.item_env.push((*name, val));
                    let val = self.interpret_item(None, body_expression, reader)?;
                    // pop the def expression afterwards
                    self.item_env.truncate(len_before);
                    Ok(val)
                }
            }
        }

        fn read(&mut self, expr: &ReadExpr, reader: &mut BufferReader) -> Result<Val, Error> {
            let val = match expr {
                ReadExpr::Prim(ReadPrim::S16Be) => {
                    binary::read_s16be(reader).map(|v| Val::Const(Const::S16(v)))?
                }
                ReadExpr::Prim(ReadPrim::S16Le) => {
                    binary::read_s16le(reader).map(|v| Val::Const(Const::S16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U8) => {
                    binary::read_u8(reader).map(|v| Val::Const(Const::U8(v)))?
                }
                ReadExpr::Prim(ReadPrim::U16Be) => {
                    binary::read_u16be(reader).map(|v| Val::Const(Const::U16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U16Le) => {
                    binary::read_u16le(reader).map(|v| Val::Const(Const::U16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U32Be) => {
                    binary::read_u32be(reader).map(|v| Val::Const(Const::U32(v)))?
                }
                ReadExpr::Prim(ReadPrim::U32Le) => {
                    binary::read_u32le(reader).map(|v| Val::Const(Const::U32(v)))?
                }
                ReadExpr::Prim(ReadPrim::F32Le) => {
                    binary::read_f32le(reader).map(|v| Val::Const(Const::F32(v)))?
                }
                ReadExpr::Prim(ReadPrim::Array(len_expr, read_item)) => {
                    // Need to run the len_expr, then read that many items
                    let len: usize = self.eval(len_expr)?.try_into()?;
                    let items = (0..len)
                        .map(|_| self.read(read_item, reader))
                        .collect::<Result<Vec<_>, _>>()?;
                    Val::Vec(items)
                }
                ReadExpr::Prim(ReadPrim::StreamPos) => {
                    let pos = reader.relative_offset();
                    Val::Const(Const::Pos(pos.try_into().map_err(|_| Error::OutOfRange)?))
                }
                ReadExpr::CustomType(i) => {
                    let def = self.defs.get(*i).unwrap().clone(); // FIXME clone
                    self.interpret_item(Some(def.name), &def.expr, reader)?
                }
                ReadExpr::Match(_, _) => todo! {},
            };
            Ok(val)
        }

        fn eval(&self, expr: &Expr) -> Result<Val, Error> {
            match expr {
                Expr::Item(index) => {
                    let item = self
                        .item_env
                        .get(*index)
                        .map(|pair| pair.1.clone())
                        .ok_or(Error::UnknownItem)?;
                    Ok(item)
                }
                Expr::Const(constant) => Ok(Val::Const(*constant)),
                Expr::PrimFn(_) => todo! {},
                Expr::Func(_) => todo! {},
                Expr::App(_, _) => todo! {},
            }
        }
    }

    impl From<BufferError> for Error {
        fn from(err: BufferError) -> Self {
            Error::BufferError(err)
        }
    }

    impl TryFrom<Val> for usize {
        type Error = Error;

        fn try_from(value: Val) -> Result<Self, Self::Error> {
            match value {
                Val::Const(Const::U8(val)) => Ok(usize::from(val)),
                Val::Const(Const::U16(val)) => Ok(usize::from(val)),
                Val::Const(Const::U32(val)) => usize::try_from(val).map_err(|_| Error::BadIndex),
                Val::Const(Const::S16(_))
                | Val::Const(Const::F32(_))
                | Val::Const(Const::Pos(_))
                | Val::Record(_)
                | Val::Vec(_) => Err(Error::BadIndex),
            }
        }
    }
}

pub(crate) mod compile {
    use super::*;
    use crate::core::binary;
    use format::*;
    use host::Expr;
    use string_interner::Symbol;

    use crate::core::binary::{Buffer, BufferError, BufferReader, ReadError};
    use crate::source::StringInterner;

    pub struct Compiler<'interner> {
        interner: &'interner mut StringInterner,
        item_env: Vec<(StringId, Val)>,
        defs: Vec<Def>,

        type_env: TypeEnv,
    }

    #[derive(Debug, Clone)]
    pub enum Val {
        Record(RecordVal),
        Const(Const),
        Vec(Vec<Val>),
    }

    #[derive(Debug, Clone)]
    pub struct RecordVal {
        pub name: StringId,
        pub fields: Vec<FieldVal>,
    }

    #[derive(Debug, Clone)]
    pub struct FieldVal {
        pub name: StringId,
        pub value: Box<Val>,
    }

    // FIXME: Is this even needed?
    // I.e. can it just parse into the value of the selected variant
    // would we ever have variants without data?
    #[derive(Debug, Clone)]
    pub struct EnumVal {
        pub tag: u8,
        pub value: Val,
    }

    struct Struct {
        name: StringId,
        fields: Vec<(StringId, TypeId)>,
    }

    struct TypeId(u16);

    /* So types get added to the type env */
    /* Where do functions live? */

    // Naive context for storing types
    struct TypeEnv {
        types: Vec<(StringId, Type)>,
    }

    impl TypeEnv {
        fn new(interner: &mut StringInterner) -> Self {
            let mut env = TypeEnv { types: Vec::new() };

            // Pre-load prims into env
            env.store(interner.get_or_intern("u8"), Type::Prim(host::Prim::U8));
            env.store(interner.get_or_intern("u16"), Type::Prim(host::Prim::U16));
            env.store(interner.get_or_intern("u32"), Type::Prim(host::Prim::U32));
            env.store(interner.get_or_intern("f32"), Type::Prim(host::Prim::F32));
            env.store(interner.get_or_intern("Pos"), Type::Prim(host::Prim::Pos));
            // TODO Array

            for name in [
                "S16Be", "S16Le", "U8", "U16Be", "U16Le", "U32Be", "U32Le", "F32Le",
            ] {
                let name = interner.get_or_intern(name);
                env.store(
                    name,
                    Type::Struct(Struct {
                        name,
                        fields: Vec::new(),
                    }),
                );
            }

            env
        }

        fn store(&mut self, name: StringId, ty: Type) -> TypeId {
            let id = self.types.len();
            self.types.push((name, ty));
            TypeId(id as u16)
        }

        fn find(&self, name: StringId) -> Option<&Type> {
            self.types
                .iter()
                .find_map(|(nm, ty)| if *nm == name { Some(ty) } else { None })
        }

        fn get(&self, id: TypeId) -> Option<&Type> {
            self.types.get(usize::from(id.0)).map(|pair| &pair.1)
        }
    }

    enum Type {
        Struct(Struct),
        Prim(host::Prim),
    }

    #[derive(Debug)]
    pub enum Error {
        UnknownItem,
        BadIndex,
        BufferError(BufferError),
        /// Value exceeds the range of the target type
        OutOfRange,
    }

    impl<'interner> Compiler<'interner> {
        pub fn new(interner: &'interner mut StringInterner) -> Self {
            let type_env = TypeEnv::new(interner);
            Compiler {
                interner,
                item_env: Vec::new(),
                defs: Vec::new(),
                type_env,
            }
        }

        pub fn compile(&mut self, module: &Module) -> Result<Val, Error> {
            // Hmm so the definitions need to be evaluated first so that they are present in the
            // item env (since main refers to them by index).
            self.defs = module.definitions.clone(); // FIXME

            println!("Symbols:");
            let mut i = 0;
            while let Some(s) = self.interner.resolve(StringId::try_from_usize(i).unwrap()) {
                println!("- {}: '{}'", i, s);
                i += 1;
            }

            // find the "main" def, which is the entry point
            // TODO: Perhaps it should actually be fished out of the defs?
            let main_sym = self.interner.get("main").expect("no main symbol");
            let main = module
                .definitions
                .iter()
                .find(|def| def.name == main_sym)
                .expect("unable to find main item in module");

            self.compile_item(Some(main.name), &main.expr)
        }

        fn compile_item(&mut self, name: Option<StringId>, item: &Item) -> Result<Val, Error> {
            match item {
                Item::Format(Format { params, fields }) => {
                    if params.is_empty() {
                        // Generate Format impl
                        // TODO: Need to be able to generate code like this:
                        /*
                        struct MyFormat {
                            data: Vec<U8::Repr>,
                        }

                        impl MyFormat {
                            #[requires(data.len() == len)]
                            pub fn new(len: u32, data: Vec<F>) -> MyFormat<F> {
                                MyFormat { len, data }
                            }

                            pub data(&self) -> &[u32] {
                                &self.data
                            }
                        }

                        impl DepFormat for MyFormat {
                            type Arg = u32;
                            type Repr = MyFormat;

                            fn decode_dep<'data>(len: u32, buf: &mut Buffer<'data>) -> Result<MyFormat, Error> {
                                let data = RepeatLen::<F>::decode_dep(len as usize)?;
                                Ok(MyFormat { data })
                            }

                            fn encode_dep<'data>(&self, len: u32, buf: &mut Buffer<'data>) -> Result<(), Error> {
                                RepeatLen::<F>::encode_dep(self.data, self.len, buf)
                            }
                        }
                         */

                        // Do we structure this in a form that implies some things or do we have
                        // a mechanism to parse that more accurately?

                        /*
                        Concepts:

                        - structs (record style)
                            - Possibly with type parameters
                        - impl blocks, with and without traits and associated types
                        - functions
                          - expressions: don't think we really want to be able to describe
                            arbitrary expressions

                         */
                        // let mut ffields = Vec::new();
                        for field in fields {
                            // Need to resolve the type id for each field host type
                            let host_type_id = match &field.host_type {
                                host::Type::Prim(prim) => {
                                    let name = match prim {
                                        host::Prim::U8 => self.interner.get_or_intern("u8"),
                                        host::Prim::S16 => self.interner.get_or_intern("i16"),
                                        host::Prim::U16 => self.interner.get_or_intern("u16"),
                                        host::Prim::U32 => self.interner.get_or_intern("u32"),
                                        host::Prim::F32 => self.interner.get_or_intern("f32"),
                                        host::Prim::Pos => self.interner.get_or_intern("Pos"),
                                        host::Prim::Array(_, _) => {
                                            todo!()
                                        }
                                    };
                                    self.type_env.find(name)
                                    // ffields.push()
                                }
                                host::Type::CustomType(_) => {
                                    todo!()
                                }
                            };
                        }

                        todo!()
                    } else {
                        // Generate FormatDep impl
                        todo!()
                    }

                    // // TODO: handle params
                    // let len_before = self.item_env.len();
                    // let mut ffields = Vec::new();
                    // for field in fields {
                    //     let val = self.read(&field.read, reader)?;
                    //     self.item_env.push((field.name, val.clone()));
                    //     ffields.push(FieldVal {
                    //         name: field.name,
                    //         value: Box::new(val),
                    //     });
                    // }
                    // self.item_env.truncate(len_before);
                    // let record = RecordVal {
                    //     name: name.expect("no name"),
                    //     fields: ffields,
                    // };
                    //
                    // Ok(Val::Record(record))
                }
                Item::Let(name, def_expression, body_expression) => {
                    // eval the def_expression, then add the result to the env and eval body_expression
                    let len_before = self.item_env.len();
                    let val = self.compile_item(None, def_expression)?;
                    self.item_env.push((*name, val));
                    let val = self.compile_item(None, body_expression)?;
                    // pop the def expression afterwards
                    self.item_env.truncate(len_before);
                    Ok(val)
                }
            }
        }

        fn read(&mut self, expr: &ReadExpr, reader: &mut BufferReader) -> Result<Val, Error> {
            let val = match expr {
                ReadExpr::Prim(ReadPrim::S16Be) => {
                    binary::read_s16be(reader).map(|v| Val::Const(Const::S16(v)))?
                }
                ReadExpr::Prim(ReadPrim::S16Le) => {
                    binary::read_s16le(reader).map(|v| Val::Const(Const::S16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U8) => {
                    binary::read_u8(reader).map(|v| Val::Const(Const::U8(v)))?
                }
                ReadExpr::Prim(ReadPrim::U16Be) => {
                    binary::read_u16be(reader).map(|v| Val::Const(Const::U16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U16Le) => {
                    binary::read_u16le(reader).map(|v| Val::Const(Const::U16(v)))?
                }
                ReadExpr::Prim(ReadPrim::U32Be) => {
                    binary::read_u32be(reader).map(|v| Val::Const(Const::U32(v)))?
                }
                ReadExpr::Prim(ReadPrim::U32Le) => {
                    binary::read_u32le(reader).map(|v| Val::Const(Const::U32(v)))?
                }
                ReadExpr::Prim(ReadPrim::F32Le) => {
                    binary::read_f32le(reader).map(|v| Val::Const(Const::F32(v)))?
                }
                ReadExpr::Prim(ReadPrim::Array(len_expr, read_item)) => {
                    // Need to run the len_expr, then read that many items
                    let len: usize = self.eval(len_expr)?.try_into()?;
                    let items = (0..len)
                        .map(|_| self.read(read_item, reader))
                        .collect::<Result<Vec<_>, _>>()?;
                    Val::Vec(items)
                }
                ReadExpr::Prim(ReadPrim::StreamPos) => {
                    let pos = reader.relative_offset();
                    Val::Const(Const::Pos(pos.try_into().map_err(|_| Error::OutOfRange)?))
                }
                ReadExpr::CustomType(i) => {
                    let def = self.defs.get(*i).unwrap().clone(); // FIXME clone
                    self.compile_item(Some(def.name), &def.expr)?
                }
                ReadExpr::Match(_, _) => todo! {},
            };
            Ok(val)
        }

        fn eval(&self, expr: &Expr) -> Result<Val, Error> {
            match expr {
                Expr::Item(index) => {
                    let item = self
                        .item_env
                        .get(*index)
                        .map(|pair| pair.1.clone())
                        .ok_or(Error::UnknownItem)?;
                    Ok(item)
                }
                Expr::Const(constant) => Ok(Val::Const(*constant)),
                Expr::PrimFn(_) => todo! {},
                Expr::Func(_) => todo! {},
                Expr::App(_, _) => todo! {},
            }
        }
    }

    impl From<BufferError> for Error {
        fn from(err: BufferError) -> Self {
            Error::BufferError(err)
        }
    }

    impl TryFrom<Val> for usize {
        type Error = Error;

        fn try_from(value: Val) -> Result<Self, Self::Error> {
            match value {
                Val::Const(Const::U8(val)) => Ok(usize::from(val)),
                Val::Const(Const::U16(val)) => Ok(usize::from(val)),
                Val::Const(Const::U32(val)) => usize::try_from(val).map_err(|_| Error::BadIndex),
                Val::Const(Const::S16(_))
                | Val::Const(Const::F32(_))
                | Val::Const(Const::Pos(_))
                | Val::Record(_)
                | Val::Vec(_) => Err(Error::BadIndex),
            }
        }
    }
}

/// What the generated code will use to parse data
mod runtime {
    use crate::core::binary;
    use crate::core::binary::{BufferError, BufferReader as Buffer};

    struct S16Be;
    struct S16Le;
    struct U8;
    struct U16Be;
    struct U16Le;
    struct U32Be;
    struct U32Le;
    struct F32Le;

    #[derive(Debug)]
    pub enum Error {
        BufferError(BufferError),
    }

    pub trait Format: Sized {
        type Repr;

        fn decode<'data>(buf: &mut Buffer<'data>) -> Result<Self::Repr, Error>;

        // TODO: encode
        // fn encode<'data>(&self, buf: &mut Buffer<'data>) -> Result<(), Error>;
    }

    pub trait FormatDep: Sized {
        type Repr;
        type Arg;

        fn decode<'data>(buf: &mut Buffer<'data>, arg: Self::Arg) -> Result<Self, Error>;

        // TODO: encode
        // fn encode<'data>(&self, buf: &mut Buffer<'data>) -> Result<(), Error>;
    }

    macro_rules! impl_prim_format {
        ($read:ident, $T:ty, $repr:ty) => {
            impl Format for $T {
                type Repr = $repr;

                fn decode<'data>(buf: &mut Buffer<'data>) -> Result<Self::Repr, Error> {
                    binary::$read(buf).map_err(Error::from)
                }
            }
        };
    }

    impl_prim_format!(read_s16be, S16Be, i16);
    impl_prim_format!(read_s16le, S16Le, i16);
    impl_prim_format!(read_u8, U8, u8);
    impl_prim_format!(read_u16be, U16Be, u16);
    impl_prim_format!(read_u16le, U16Le, u16);
    impl_prim_format!(read_u32be, U32Be, u32);
    impl_prim_format!(read_u32le, U32Le, u32);
    impl_prim_format!(read_f32le, F32Le, f32);

    impl From<BufferError> for Error {
        fn from(err: BufferError) -> Self {
            Error::BufferError(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::StringInterner;
    use format::*;
    use host::Expr;

    #[test]
    fn test_f2dot14() {
        let mut interner = StringInterner::new();
        // pub struct F2Dot14(u16);
        // impl<'a> ReadFrom<'a> for F2Dot14 {
        //     type ReadType = U16Be;
        //     fn from(value: u16) -> Self {
        //         F2Dot14(value)
        //     }
        // }
        //
        // def f2dot14 : Format = s16be;
        let format = Format {
            params: vec![],
            fields: vec![Field {
                name: interner.get_or_intern("0"),
                host_type: host::Type::Prim(host::Prim::U16),
                read: ReadExpr::Prim(ReadPrim::U16Be),
            }],
        };

        let item = Item::Format(format);
        let def = Def {
            name: interner.get_or_intern("f2dot14"),
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_table_record() {
        let mut interner = StringInterner::new();
        /*

        def table_record = {
            /// Table identifier.
            table_id <- tag,
            /// CheckSum for this table.
            ///
            /// ## References
            ///
            /// - [Microsoft's OpenType Spec: Calculating Checksums](https://docs.microsoft.com/en-us/typography/opentype/spec/otff#calculating-checksums)
            checksum <- u32be,
            /// Offset from the beginning of the TrueType font file.
            offset <- u32be,
            /// Length of this table.
            length <- u32be,
        };

        pub struct TableRecord {
            pub table_tag: u32,
            pub checksum: u32,
            pub offset: u32,
            pub length: u32,
        }

        impl<'a> ReadFrom<'a> for TableRecord {
            type ReadType = ((U32Be, U32Be), (U32Be, U32Be));
            fn from(((table_tag, checksum), (offset, length)): ((u32, u32), (u32, u32))) -> Self {
                TableRecord {
                    table_tag,
                    checksum,
                    offset,
                    length,
                }
            }
        }
         */

        // In order to be able to generate a ReadFrom impl we need to know/generate the ReadType as well as the host representation of that
        // Each field will eventually need a name too. Although in this case the tuple fields can be accessed by index
        let format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("table_tag"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("checksum"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("offset"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("length"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
            ],
        };

        let item = Item::Format(format);
        let def = Def {
            name: interner.get_or_intern("table_record"),
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn offset_table() {
        let mut interner = StringInterner::new();
        /*

        pub struct OffsetTable<'a> {
            pub sfnt_version: u32,
            pub search_range: u16,
            pub entry_selector: u16,
            pub range_shift: u16,
            pub table_records: ReadArray<'a, TableRecord>,
        }

        impl<'a> ReadBinary<'a> for OffsetTable<'a> {
            type host::HostType = Self;

            fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
                let sfnt_version = ctxt.read_u32be()?;
                match sfnt_version {
                    TTF_MAGIC | CFF_MAGIC => {
                        let num_tables = ctxt.read_u16be()?;
                        let search_range = ctxt.read_u16be()?;
                        let entry_selector = ctxt.read_u16be()?;
                        let range_shift = ctxt.read_u16be()?;
                        let table_records = ctxt.read_array::<TableRecord>(usize::from(num_tables))?;
                        Ok(OffsetTable {
                            sfnt_version,
                            search_range,
                            entry_selector,
                            range_shift,
                            table_records,
                        })
                    }
                    _ => Err(ParseError::BadVersion),
                }
            }
        }
         */

        // Assume table_record above is in the context at index 0

        let offset_table = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("num_tables"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("sfnt_version"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("search_range"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("entry_selector"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("range_shift"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: interner.get_or_intern("table_records"),
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the host env
                        // Or is it just referring to the format?
                        Box::new(host::Type::CustomType(0)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the "format" env
                        Box::new(ReadExpr::CustomType(0)),
                    )),
                },
            ],
        };

        let item = Item::Format(offset_table);
        let def = Def {
            name: interner.get_or_intern("offset_table"),
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_match_and_fn_app() {
        let mut interner = StringInterner::new();
        /*

        // cut-down kern version 0 sub-table
        def subtable0 = {
            length <- u16be,
            format <- u8,
            data <- match format {
                0 => subtable_format0,
                // 2 => limit16 (u16_sub length 8) (subtable_format2 table_start),
                // Unsupported format, read the raw bytes so that we stay synchronised with other sub-tables
                _ => array16 (u16_sub length 6) u8,
            }
        };

        def subtable_format0 = {
            example <- u16be
        };
         */
        let u16_sub_length_6 = Expr::App(
            Box::new(Expr::PrimFn(host::PrimFn::U16Sub)),
            vec![Expr::Item(0 /* length */), Expr::Const(Const::U16(6))],
        );
        // represents the item storing the value resulting from evaluating the expression
        let u16_sub_length_6_item = 6;
        let host_type_8 = host::CustomType::Enum(host::Enum {
            name: interner.get_or_intern("subtable0_data"),
            variants: vec![
                host::Variant {
                    name: interner.get_or_intern("0"), // how to name variants?
                    data: Some(host::Type::CustomType(1 /* subtable_format0 */)),
                },
                host::Variant {
                    name: interner.get_or_intern("default"),
                    data: Some(host::Type::Prim(host::Prim::Array(
                        u16_sub_length_6,
                        Box::new(host::Type::Prim(host::Prim::U8)),
                    ))),
                },
            ],
        });
        let format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("format"),
                    host_type: host::Type::Prim(host::Prim::U8),
                    read: ReadExpr::Prim(ReadPrim::U8),
                },
                Field {
                    name: interner.get_or_intern("data"),
                    host_type: host::Type::CustomType(8 /* host_type_8 in context */),
                    read: ReadExpr::Match(
                        Expr::Item(1), /* format */
                        vec![
                            Branch {
                                pattern: Pattern::Const(Const::U8(0)),
                                expr: Box::new(ReadExpr::CustomType(1 /* subtable_format0 */)),
                            },
                            Branch {
                                pattern: Pattern::Var(interner.get_or_intern("_")),
                                expr: Box::new(ReadExpr::Prim(ReadPrim::Array(
                                    Expr::Item(u16_sub_length_6_item),
                                    Box::new(ReadExpr::Prim(ReadPrim::U8)),
                                ))),
                            },
                        ],
                    ),
                },
            ],
        };

        let item = Item::Format(format);
        let def = Def {
            name: interner.get_or_intern("subtable0"),
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_let() {
        let mut interner = StringInterner::new();
        /*

        def subtable_format0 = (
            let kerning_pair = {
                left <- u16be,
                right <- u16be,
            };

            {
                num_pairs <- u16be,
                pairs <- array16 num_pairs kerning_pair,
            }
        );
         */

        let kerning_pair = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("left"),
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
                Field {
                    name: interner.get_or_intern("right"),
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
            ],
        };

        let format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("num_pairs"),
                    host_type: host::Type::Prim(host::Prim::U8),
                    read: ReadExpr::Prim(ReadPrim::U8),
                },
                Field {
                    name: interner.get_or_intern("pairs"),
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Item(3 /* num_pairs */),
                        Box::new(host::Type::CustomType(0)), // kerning_pair
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Item(3 /* num_pairs */),
                        // This one is referring to a custom type in the "format" env
                        Box::new(ReadExpr::CustomType(0)), // kerning_pair
                    )),
                },
            ],
        };

        let item = Item::Let(
            interner.get_or_intern("kerning_pair"),
            Box::new(Item::Format(kerning_pair)),
            Box::new(Item::Format(format)),
        );
        let def = Def {
            name: interner.get_or_intern("subtable_format0"),
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_pos() {
        let mut interner = StringInterner::new();
        /*
        def kerning_pair = {
            left <- stream_pos,
            right <- u16be,
        };
         */

        let kerning_pair = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("left"),
                    host_type: host::Type::Prim(host::Prim::Pos),
                    read: ReadExpr::Prim(ReadPrim::StreamPos),
                },
                Field {
                    name: interner.get_or_intern("right"),
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
            ],
        };
    }

    #[test]
    fn test_format_params() {
        let mut interner = StringInterner::new();
        /*

        def offset16 = fun (base : Pos) => fun (format : Format) => {
            offset <- u16be,
            link <- match offset {
                0 => empty,
                _ => link (pos_add_u16 base offset) format, // TODO: Use an option type?
            },
        };


        def example = fun (table_start : Pos) => {
               /// Offset from beginning of this subtable to right-hand class table.
               right_class_table <- offset16 table_start u32be,
        };
        */

        // How should offset16 be represented
        // let format = Format {
        //     params: vec![Param {
        //         name: 3,
        //         host_type: host::Type::Prim(host::Prim::Pos),
        //     }],
        //     fields: vec![Field {
        //         name: 1, // right_class_table
        //         host_type: host::Type::Prim(host::Prim::U16),
        //         read: ReadExpr::Prim(ReadPrim::U16Be),
        //     }],
        // };
        //
        // let item = Item::Format(format);
        // let def = Def {
        //     name: 5,
        //     expr: item,
        // };
        // let module = Module {
        //     definitions: vec![def],
        // };
    }

    #[test]
    fn test_stl() {
        let mut interner = StringInterner::new();
        /*
        def vec3d = {
            x <- f32le,
            y <- f32le,
            z <- f32le,
        };

        def triangle = {
            normal <- vec3d,
            vertices <- array8 3 vec3d,
            attribute_byte_count <- u16le,
        };

        def main = {
            header <- array8 80 u8,
            triangle_count <- u32le,
            triangles <- array32 triangle_count triangle,
        };
        */
        let vec3d_format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("x"),
                    host_type: host::Type::Prim(host::Prim::F32),
                    read: ReadExpr::Prim(ReadPrim::F32Le),
                },
                Field {
                    name: interner.get_or_intern("y"),
                    host_type: host::Type::Prim(host::Prim::F32),
                    read: ReadExpr::Prim(ReadPrim::F32Le),
                },
                Field {
                    name: interner.get_or_intern("z"),
                    host_type: host::Type::Prim(host::Prim::F32),
                    read: ReadExpr::Prim(ReadPrim::F32Le),
                },
            ],
        };

        let triangle_format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("normal"),
                    host_type: host::Type::CustomType(0), // vec3d
                    read: ReadExpr::CustomType(0),
                },
                Field {
                    name: interner.get_or_intern("vertices"),
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Const(Const::U8(3)),
                        Box::new(host::Type::CustomType(0)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Const(Const::U8(3)),
                        Box::new(ReadExpr::CustomType(0)),
                    )),
                },
                Field {
                    name: interner.get_or_intern("attribute_byte_count"),
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Le),
                },
            ],
        };

        let main_format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: interner.get_or_intern("header"),
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Const(Const::U8(80)),
                        Box::new(host::Type::CustomType(0)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Const(Const::U8(80)),
                        Box::new(ReadExpr::CustomType(0)),
                    )),
                },
                Field {
                    name: interner.get_or_intern("triangle_count"),
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Le),
                },
                Field {
                    name: interner.get_or_intern("triangles"),
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Item(4 /* triangle_count */),
                        Box::new(host::Type::CustomType(1)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Item(4 /* triangle_count */),
                        Box::new(ReadExpr::CustomType(1)),
                    )),
                },
            ],
        };

        let module = Module {
            definitions: vec![
                Def {
                    name: interner.get_or_intern("vec3d"),
                    expr: Item::Format(vec3d_format),
                },
                Def {
                    name: interner.get_or_intern("triangle"),
                    expr: Item::Format(triangle_format),
                },
                Def {
                    name: interner.get_or_intern("main"),
                    expr: Item::Format(main_format),
                },
            ],
        };
    }
}
