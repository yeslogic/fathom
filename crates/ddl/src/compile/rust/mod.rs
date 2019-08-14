use std::borrow::Cow;

pub mod emit;

/// Compiled Rust types.
#[derive(Debug, Clone)]
pub struct Type(pub Cow<'static, str>);

/// Compiled Rust type fields types.
#[derive(Debug, Clone)]
pub struct TypeField {
    doc: Arc<[String]>,
    name: String,
    format_ty: Type,
    host_ty: Type,
}
