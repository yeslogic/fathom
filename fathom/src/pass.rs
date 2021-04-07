//! Translation passes between the intermediate representations of the compiler.

pub mod surface_to_core;
pub mod surface_to_doc;
pub mod surface_to_pretty;

pub mod core_to_pretty;
pub mod core_to_rust;
pub mod core_to_surface;
