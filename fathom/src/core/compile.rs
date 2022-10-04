use std::fmt::Debug;

use crate::core::semantics::{self, ArcValue};
use crate::core::{Prim, Term};
use crate::env::{EnvLen, SharedEnv, SliceEnv, UniqueEnv};
use crate::StringId;

pub struct Context<'arena, 'env> {
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    rigid_exprs: &'env SharedEnv<ArcValue<'arena>>,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    compile_env: &'env mut CompileEnv,
}

#[derive(Debug)]
enum Type {
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
}

/// The definition of a struct
#[derive(Debug)]
struct Struct {
    // TODO: Add name?
    fields: Vec<(StringId, Type)>,
}

#[derive(Debug)]
struct DecodeFn {
    exprs: Vec<DecodeExpr>,
}

#[derive(Debug)]
enum DecodeExpr {
    /// name, fields
    Struct {
        // name of the struct
        name: String,
        // Parse a type into a named variable
        parse_fields: Vec<(StringId, ParseExpr)>,
        // How to initialise the fields of the struct using the variables
        fields: Vec<ParseExpr>,
    },
}

#[derive(Debug)]
enum ParseExpr {
    // Parse a const type
    Const(TypeConst),
    // A reference to a variable
    Var(usize),
}

#[derive(Debug)]
enum TypeConst {
    U8,
    U16Be,
    U32Be,
    U64Be,
    U16Le,
    U32Le,
    U64Le,
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

impl CompileEnv {
    pub fn new() -> Self {
        CompileEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
        }
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
}

impl<'arena, 'env, 'data> Context<'arena, 'env> {
    pub fn new(
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        rigid_exprs: &'env SharedEnv<ArcValue<'arena>>,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
        compile_env: &'env mut CompileEnv,
    ) -> Context<'arena, 'env> {
        Context {
            item_exprs,
            rigid_exprs,
            flexible_exprs,
            compile_env,
        }
    }

    fn elim_env(&self) -> semantics::ElimEnv<'arena, 'env> {
        semantics::ElimEnv::new(self.item_exprs, self.flexible_exprs)
    }

    fn conversion_env(&self) -> semantics::ConversionEnv<'arena, 'env> {
        semantics::ConversionEnv::new(self.item_exprs, EnvLen::new(), self.flexible_exprs)
    }

    pub fn compile_format(
        &mut self,
        format: &Term<'arena>,
        // TODO: Return a Rust module containing all the compiled elements?
    ) -> Result<Module, ()> {
        let mut items = Vec::new();
        match format {
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
                for (label, format) in labels.iter().copied().zip(formats.iter()) {
                    let ty = self.compile_rep(format);
                    fields.push((label, ty));
                }
                let r#struct = Struct { fields };
                dbg!(&r#struct);
                items.push(Item::Struct(r#struct));

                // Now generate the read function
                let read_fn = self.compile_decode(format);
                dbg!(&read_fn);
                items.push(Item::ReadFn(read_fn));
            }
            Term::FormatCond(_, _, _, _) => unimplemented! {},
            Term::FormatOverlap(_, _, _) => unimplemented! {},
            Term::Prim(_, _) => unimplemented! {},
            Term::ConstLit(_, _) => unimplemented! {},
            Term::ConstMatch(_, _, _, _) => unimplemented! {},
        }

        Ok(Module { items })
    }

    fn compile_rep(&mut self, format: &Term<'arena>) -> Type {
        // We already have `format_repr` but that operates on Values...

        match format {
            Term::ItemVar(_, _) => todo! {},
            Term::LocalVar(_, var) => {
                // TODO: How to resolve var?
                match self.rigid_exprs.get_index(*var) {
                    Some(value) => dbg!(value),
                    None => panic!("invalid rigid var"),
                };
                todo!("rigid var");
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
                Term::FunApp(_, Term::Prim(_, Prim::FormatArray32), len),
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
            Term::FunApp(_, head, input) => todo!("fun app"),
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

    fn compile_decode(&mut self, format: &Term<'arena>) -> DecodeFn {
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
                    .map(|(label, format)| (label, self.read_format(format)))
                    .collect();
                // for (label, format) in  {
                //     self.read_format(format, &mut parse_fields);
                //
                //     // Generate var decl
                //     // For each field create a variable in the environment and initialise it with
                //     // a read of the format
                //
                //     // Generate struct initialisation
                // }
                self.compile_env.truncate(initial_env_len);

                for (i, _) in parse_fields.iter().enumerate() {
                    fields.push(ParseExpr::Var(i));
                }

                // Create DecodeExpr::Struct
                let st = DecodeExpr::Struct {
                    // name of the struct
                    name: String::from("TodoName"),
                    // Parse a type into a named variable
                    parse_fields,
                    // How to initialise the fields of the struct using the variables
                    fields,
                };
                DecodeFn { exprs: vec![st] }
            }
            _ => unreachable!("can only compile decode for format records"),
        }
    }

    fn read_format(&mut self, format: &Term<'arena>) -> ParseExpr {
        match format {
            // Term::RigidVar(_, var) => {
            //     exprs.push(ParseExpr::Var(*var));
            // }
            Term::Prim(_, prim) => Self::read_prim(prim),
            _ => unreachable!("format: {:?}", format),
        }
    }

    fn read_prim(prim: &Prim) -> ParseExpr {
        match prim {
            Prim::FormatU8 => ParseExpr::Const(TypeConst::U8),
            Prim::FormatU16Be => ParseExpr::Const(TypeConst::U16Be),
            Prim::FormatU16Le => ParseExpr::Const(TypeConst::U16Le),
            Prim::FormatU32Be => ParseExpr::Const(TypeConst::U32Be),
            Prim::FormatU32Le => ParseExpr::Const(TypeConst::U32Le),
            Prim::FormatU64Be => ParseExpr::Const(TypeConst::U64Be),
            Prim::FormatU64Le => ParseExpr::Const(TypeConst::U64Le),
            _ => todo! {},
        }
    }
}

pub mod rust {
    use std::cell::RefCell;

    use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
    use scoped_arena::Scope;

    use crate::core::compile::{DecodeExpr, Item, Module, ParseExpr, Type, TypeConst};
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
            self.intersperse(
                module.items.iter().map(|item| self.item(item)),
                self.hardline(),
            )
        }

        fn item(&'arena self, item: &Item) -> DocBuilder<'arena, Self> {
            match item {
                Item::Struct(r#struct) => self.sequence(
                    self.concat([
                        self.text("struct"),
                        self.space(),
                        self.text("TodoStructName"),
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
                    self.text("}"),
                ),
                Item::ReadFn(readfn) => self.sequence(
                    self.concat([
                        self.text("impl<'a> ReadBinary<'a> for"),
                        self.space(),
                        self.text("TodoStruct"),
                        self.space(),
                        self.text("{"),
                        self.hardline(),
                        self.text("type HostType = Self;"),
                        self.hardline(),
                        self.text("pub fn read() -> Result<Self::HostType, ParseError> {"),
                    ]),
                    readfn.exprs.iter().map(|expr| self.expr(expr)),
                    self.text(";"),
                    self.text("}"),
                ),
            }
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
            }
        }

        fn expr(&'arena self, expr: &DecodeExpr) -> DocBuilder<'arena, Self> {
            match expr {
                DecodeExpr::Struct {
                    name,
                    parse_fields,
                    fields,
                } => {
                    self.concat([
                        self.parse_fields(parse_fields),
                        self.hardline(),
                        self.construct_struct(/*name,*/ fields),
                    ])
                }
            }
        }

        fn parse_fields(
            &'arena self,
            fields: &[(StringId, ParseExpr)],
        ) -> DocBuilder<'arena, Self> {
            self.sequence(
                self.text(""),
                fields.iter().map(|(name, field)| {
                    self.concat([
                        self.text("let"),
                        self.space(),
                        self.string_id(*name),
                        self.space(),
                        self.text("="),
                        self.space(),
                        self.parse_expr(field),
                        self.text("?"),
                    ])
                }),
                self.concat([self.text(";"), self.hardline()]),
                self.text(""),
            )
        }

        fn construct_struct(
            &'arena self,
            /*name: StringId,*/ fields: &[ParseExpr],
        ) -> DocBuilder<'arena, Self> {
            self.sequence(
                self.concat([
                    self.text("Ok("),
                    self.text(/*name*/ "TodoStruct"),
                    self.space(),
                    self.text("{"),
                ]),
                fields.iter().enumerate().map(|(i, field)| {
                    self.concat([
                        self.text(format!("todo_field_{}:", i)),
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
                ParseExpr::Const(r#const) => self.type_const(r#const),
                ParseExpr::Var(var) => self.text("todo_var"),
            }
        }

        fn type_const(&'arena self, ty_const: &TypeConst) -> DocBuilder<'arena, Self> {
            match ty_const {
                TypeConst::U8 => self.text("read_u8()"),
                TypeConst::U16Be => self.text("read_u16be()"),
                TypeConst::U32Be => self.text("read_u32be()"),
                TypeConst::U64Be => self.text("read_u64be()"),
                TypeConst::U16Le => self.text("read_u16le()"),
                TypeConst::U32Le => self.text("read_u32le()"),
                TypeConst::U64Le => self.text("read_u64le()"),
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
