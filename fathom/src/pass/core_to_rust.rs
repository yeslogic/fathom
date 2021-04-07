use itertools::Itertools;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Write};

use crate::lang::core;

struct Namespace {}

impl Namespace {
    fn new() -> Namespace {
        Namespace {}
    }

    fn fresh_upper_camel_case<'src>(&mut self, src: impl Into<Cow<'src, str>>) -> String {
        src.into().into() // FIXME
    }

    fn fresh_upper_snake_case<'src>(&mut self, src: impl Into<Cow<'src, str>>) -> String {
        src.into().into() // FIXME
    }

    fn fresh_lower_snake_case<'src>(&mut self, src: impl Into<Cow<'src, str>>) -> String {
        src.into().into() // FIXME
    }
}

struct TypeItem {
    name: String,
    type_params: Vec<String>,
}

struct FormatItem {
    name: String,
    type_params: Vec<String>,
    repr: String,
    read: String,
    write: String,
}

struct ConstantItem {
    name: String,
    r#type: Type,
}

enum Item {
    Sort,
    Type(TypeItem),
    Format(FormatItem),
    Constant(ConstantItem),
}

enum Term {
    Expr(Expr, Type),
    Type(Type),
    Format(Format),
    Sort,
}

type Expr = String;

type Type = String;

struct Format {
    repr: Type,
    read: Expr,
    write: Expr,
}

pub struct Context {
    type_namespace: Namespace,
    expr_namespace: Namespace,
    items: HashMap<String, Item>,
    locals: core::Locals<()>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            type_namespace: Namespace::new(),
            expr_namespace: Namespace::new(),
            items: HashMap::new(),
            locals: core::Locals::new(),
        }
    }

    pub fn emit_module(
        &mut self,
        writer: &mut impl Write,
        module: &core::Module,
    ) -> io::Result<()> {
        for line in module.doc.iter() {
            writeln!(writer, "//!{}", line)?;
        }
        if !module.doc.is_empty() {
            writeln!(writer)?;
        }

        for item in &module.items {
            self.emit_item(writer, item)?;
            writeln!(writer)?;
        }
        Ok(())
    }

    fn emit_item(&mut self, writer: &mut impl Write, item: &core::Item) -> io::Result<()> {
        match &item.data {
            core::ItemData::Constant(constant) => self.emit_constant(writer, constant),
            core::ItemData::StructType(struct_type) => self.emit_struct_type(writer, struct_type),
            core::ItemData::StructFormat(struct_format) => {
                self.emit_struct_format(writer, struct_format)
            }
        }
    }

    fn emit_constant(
        &mut self,
        writer: &mut impl Write,
        constant: &core::Constant,
    ) -> io::Result<()> {
        let item = match self.compile_term(&constant.term) {
            Term::Expr(expr, r#type) => {
                let name = self.expr_namespace.fresh_upper_snake_case(&constant.name);

                for line in constant.doc.iter() {
                    writeln!(writer, "///{}", line)?;
                }
                writeln!(
                    writer,
                    "pub const {name}: {expr} = {type};",
                    name = name,
                    expr = expr,
                    type = r#type,
                )?;

                Item::Constant(ConstantItem { name, r#type })
            }
            Term::Type(r#type) => {
                let name = self.type_namespace.fresh_upper_camel_case(&constant.name);

                for line in constant.doc.iter() {
                    writeln!(writer, "///{}", line)?;
                }
                writeln!(
                    writer,
                    "pub type {name} = {type};", // TODO: type params
                    name = name,
                    type = r#type,
                )?;

                Item::Type(TypeItem {
                    name,
                    type_params: Vec::new(), // TODO: Type parameters
                })
            }
            Term::Format(format) => {
                let name = self.type_namespace.fresh_upper_camel_case(&constant.name);
                let read_name = self
                    .expr_namespace
                    .fresh_lower_snake_case(format!("read_{}", constant.name));
                let write_name = self
                    .expr_namespace
                    .fresh_lower_snake_case(format!("write_{}", constant.name));

                // Type alias
                for line in constant.doc.iter() {
                    writeln!(writer, "///{}", line)?;
                }
                writeln!(
                    writer,
                    "pub type {name} = {type};", // TODO: type params
                    name = name,
                    type = format.repr,
                )?;

                writeln!(writer)?;

                // Read function
                writeln!(writer, "#[inline]")?;
                writeln!(writer, "pub fn {}(...) -> Result<{}> {{", read_name, name,)?; // TODO: type params
                writeln!(writer, "    {}(...)", format.read)?;
                writeln!(writer, "}}")?;

                writeln!(writer)?;

                // Write function
                writeln!(writer, "#[inline]")?;
                writeln!(writer, "pub fn {}(...) -> Result<()> {{", write_name)?; // TODO: type params
                writeln!(writer, "    {}(...)", format.write)?;
                writeln!(writer, "}}")?;

                Item::Format(FormatItem {
                    name: name.clone(),
                    type_params: Vec::new(), // TODO: type parameters
                    repr: name,
                    read: read_name,
                    write: write_name,
                })
            }
            Term::Sort => Item::Sort,
        };

        self.items.insert(constant.name.clone(), item);

        Ok(())
    }

    fn emit_struct_type(
        &mut self,
        writer: &mut impl Write,
        struct_type: &core::StructType,
    ) -> io::Result<()> {
        let name = self
            .type_namespace
            .fresh_upper_camel_case(&struct_type.name);

        let mut fields = Vec::with_capacity(struct_type.fields.len());
        for field_declaration in struct_type.fields.iter() {
            match self.compile_term(&field_declaration.type_) {
                Term::Type(r#type) => fields.push((field_declaration.label.data.as_str(), r#type)),
                _ => panic!(),
            }
        }

        for line in struct_type.doc.iter() {
            writeln!(writer, "///{}", line)?;
        }
        writeln!(writer, "pub struct {} {{", name)?; // TODO: type params
        for (label, r#type) in &fields {
            writeln!(writer, "{} : {},", label, r#type)?;
        }
        writeln!(writer, "}}")?;

        let item = Item::Type(TypeItem {
            name,
            type_params: Vec::new(), // TODO: Type parameters
        });

        self.items.insert(struct_type.name.clone(), item);

        Ok(())
    }

    fn emit_struct_format(
        &mut self,
        writer: &mut impl Write,
        struct_format: &core::StructFormat,
    ) -> io::Result<()> {
        let name = self
            .type_namespace
            .fresh_upper_camel_case(&struct_format.name);
        let read_name = self
            .expr_namespace
            .fresh_lower_snake_case(format!("read_{}", struct_format.name));
        let write_name = self
            .expr_namespace
            .fresh_lower_snake_case(format!("write_{}", struct_format.name));

        let mut fields = Vec::with_capacity(struct_format.fields.len());
        for field_declaration in struct_format.fields.iter() {
            match self.compile_term(&field_declaration.type_) {
                Term::Format(format) => {
                    fields.push((field_declaration.label.data.as_str(), format));
                }
                _ => panic!(),
            }
        }

        // Repr struct declaration
        for line in struct_format.doc.iter() {
            writeln!(writer, "///{}", line)?;
        }
        writeln!(writer, "pub struct {} {{", name)?; // TODO: type params
        for (label, format) in &fields {
            writeln!(writer, "{} : {},", label, format.repr)?;
        }
        writeln!(writer, "}}")?;

        writeln!(writer)?;

        // Read function
        writeln!(writer, "pub fn {}(...) -> Result<{}> {{", read_name, name,)?; // TODO: type params
        {
            // Read fields
            for (label, format) in &fields {
                writeln!(writer, "let {} = {}(...)?;", label, format.read)?;
            }
            writeln!(writer)?;

            // Construct repr struct
            writeln!(
                writer,
                "    Ok({name} {{ {fields} }})",
                name = name,
                fields = fields.iter().map(|(label, _)| label).format(","),
            )?;
        }
        writeln!(writer, "}}")?;

        writeln!(writer)?;

        // Write function
        writeln!(writer, "pub fn {}(...) -> Result<()> {{", write_name)?; // TODO: type params
        {
            for (_, format) in &fields {
                writeln!(writer, "{}(...)?;", format.write)?;
            }
            writeln!(writer, "Ok(())")?;
        }
        writeln!(writer, "}}")?;

        let item = Item::Format(FormatItem {
            name: name.clone(),
            type_params: Vec::new(), // TODO: type parameters
            repr: name,
            read: read_name,
            write: write_name,
        });

        self.items.insert(struct_format.name.clone(), item);

        Ok(())
    }

    fn compile_term(&mut self, term: &core::Term) -> Term {
        match &term.data {
            core::TermData::Global(name) => todo!(),
            core::TermData::Item(name) => match self.items.get(name) {
                Some(Item::Constant(item)) => Term::Expr(item.name.clone(), item.r#type.clone()),
                Some(Item::Type(item)) => Term::Type(item.name.clone()),
                Some(Item::Format(item)) => todo!(),
                Some(Item::Sort) => Term::Sort,
                None => todo!(),
            },
            core::TermData::Local(local_index) => match self.locals.get(*local_index) {
                Some(_) => todo!(),
                None => todo!(),
            },
            core::TermData::Ann(term, r#type) => todo!(),
            core::TermData::Sort(_) => Term::Sort,
            core::TermData::FunctionType(param_type, body_type) => todo!(),
            core::TermData::FunctionElim(head_term, arg_term) => todo!(),
            core::TermData::StructTerm(_) => todo!(),
            core::TermData::StructElim(_, _) => todo!(),
            core::TermData::ArrayTerm(_) => todo!(),
            core::TermData::Primitive(_) => todo!(),
            core::TermData::BoolElim(_, _, _) => todo!(),
            core::TermData::IntElim(_, _, _) => todo!(),
            core::TermData::FormatType => todo!(),
            core::TermData::Repr => todo!(),
            core::TermData::Error => todo!(),
        }
    }
}
