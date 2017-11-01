//! A simple, nominal intermediate representation that can be easily
//! translated to Rust or other languages with similar type systems
//! that require structs and tagged unions to be declared up front.
//!
//! For example, the following data description:
//!
//! ```plain
//! Baz = [
//!     struct { x : u8 },
//! ];
//!
//! Bar = Baz;
//!
//! Foo = struct {
//!     data0 : union { u8, Bar, struct {} },
//!     data1 : struct { inner: Bar },
//!     data2 : [struct{}],
//! };
//! ```
//!
//! will be lowered to:
//!
//! ```plain
//! alias Baz = [Baz::Elem];
//!
//! struct Baz::Elem { x : u8 };
//!
//! alias Bar = Baz;
//!
//! struct Foo {
//!     data0 : Foo::data0,
//!     data1 : Foo::data1,
//!     data2 : [Foo::data2::Entry],
//! };
//!
//! union Foo::data0 {
//!     u8,
//!     Bar,
//!     Foo::data0::Variant2,
//! };
//!
//! struct Foo::data1 {
//!     inner : Bar,
//! };
//!
//! struct Foo::data2::Entry {};
//! ```
//!
//! A further step will need to be added to convert the separated paths into
//! flat identifiers that do not clash with each other.

pub mod ast;
pub mod compile;
