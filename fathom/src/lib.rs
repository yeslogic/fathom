#![doc = include_str!("../../README.md")]
#![allow(clippy::bool_assert_comparison)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::new_without_default)]

// Supporting modules
mod alloc;
pub mod env;
pub mod files;
pub mod source;
pub mod symbol;

// Intermediate languages
pub mod core;
pub mod surface;

// Top level driver
mod driver;

pub const BUG_REPORT_URL: &str = concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new");

// Public exports
pub use driver::{Driver, Status};
