#![allow(dead_code)]

use codespan::{CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use ddl::semantics::{self, Context};
use ddl::syntax::concrete;
use ddl::syntax::core::{RcTerm, RcType, RcValue};
use ddl::syntax::parse;
use ddl::syntax::translation::{Desugar, DesugarEnv};

pub fn parse_module(codemap: &mut CodeMap, src: &str) -> concrete::Module {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_module, errors) = parse::module(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_module
}

pub fn parse_term(codemap: &mut CodeMap, src: &str) -> concrete::Term {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_term
}

pub fn parse_infer_term(codemap: &mut CodeMap, context: &Context, src: &str) -> (RcTerm, RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(context.mappings()))
        .unwrap();
    match semantics::infer_term(context, &raw_term) {
        Ok((term, ty)) => (term, ty),
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

pub fn parse_nf_term(codemap: &mut CodeMap, context: &Context, src: &str) -> RcValue {
    let (term, _) = parse_infer_term(codemap, context, src);
    match semantics::nf_term(context, &term) {
        Ok(value) => value,
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("internal error!");
        },
    }
}

pub fn parse_check_term(codemap: &mut CodeMap, context: &Context, src: &str, expected: &RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(context.mappings()))
        .unwrap();
    match semantics::check_term(context, &raw_term, expected) {
        Ok(_) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}
