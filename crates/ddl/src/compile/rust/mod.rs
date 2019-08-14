use std::borrow::Cow;

pub mod emit;

/// Compiled Rust types.
#[derive(Debug, Clone)]
pub struct Type(pub Cow<'static, str>);
