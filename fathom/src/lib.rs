#![doc = include_str!("../../README.md")]

// Supporting modules
mod alloc;
pub mod env;
pub mod source;

// Intermediate languages
pub mod core;
pub mod surface;

// Top level driver
mod driver;

pub const BUG_REPORT_URL: &str = concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new");

// Public exports
pub use driver::{Driver, Status};

/// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub type StringInterner = string_interner::StringInterner<
    string_interner::backend::BucketBackend<StringId>,
    std::hash::BuildHasherDefault<fxhash::FxHasher32>,
>;
