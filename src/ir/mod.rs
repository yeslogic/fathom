//! Intermediate representations of our DDL
//!
//! For example, take the following data definition:
//!
//! ```plain
//! Pixel = {
//!     r : u8,
//!     g : u8,
//!     b : u8,
//!     a : u8,
//! };
//!
//! Bitmap = {
//!     magic : [u8; 8] where magic =>
//!         magic == [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00],
//!     extents : struct {
//!         width : u32be,
//!         height : u32be,
//!     },
//!     data : [Pixel; extents.width * extents.height],
//! };
//!
//! Bmp = Bitmap;
//! ```
//!
//! # Lowering
//!
//! The following lowering steps world take place:
//!
//! ## Owned IR
//!
//! This lowers the data definition into a combination of a representation
//! struct and a parser combinator expression:
//!
//! ```plain
//! Pixel = define {
//!     type = struct {
//!         r : u8,
//!         g : u8,
//!         b : u8,
//!         a : u8,
//!     };
//!
//!     parser =
//!         (r : u8)
//!         (g : u8)
//!         (b : u8)
//!         (a : u8)
//!             => struct { r, g, b, a };
//! };
//!
//! Bitmap = define {
//!     type = struct {
//!         magic : [u8; 8],
//!         extents : struct {
//!             width : u32be,
//!             height : u32be,
//!         },
//!         data : [Pixel],
//!     };
//!
//!     parser =
//!         (magic : assert(\magic ->
//!             magic == [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]))
//!         (extents :
//!             (width : u32be)
//!             (height : u32be)
//!                 => struct { width, height })
//!         (data : Pixel ** (extents.width * extents.height))
//!             => struct { magic, extents, height };
//! };
//!
//! Bmp = define {
//!     type = Bitmap;
//! };
//! ```
//!
//! ## Owned Nominal IR
//!
//! This lowers the owned IR into a nominal form, where structs and unions are
//! each given unique names:
//!
//! ```plain
//! define Pixel {
//!     struct {
//!         r : u8,
//!         g : u8,
//!         b : u8,
//!         a : u8,
//!     };
//!
//!     parser =
//!         (r : u8)
//!         (g : u8)
//!         (b : u8)
//!         (a : u8)
//!             => Pixel { r, g, b, a };
//! };
//!
//! define Bitmap::extents {
//!     struct {
//!        width : u32be,
//!        height : u32be,
//!     };
//! };
//!
//! define Bitmap {
//!     struct {
//!         magic : [u8; 8],
//!         extents : Bitmap::extents,
//!         data : [Pixel],
//!     };
//!
//!     parser =
//!         (magic : assert(\magic ->
//!             magic == [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]))
//!         (extents :
//!             (width : u32be)
//!             (height : u32be)
//!                 => Bitmap::extents { width, height })
//!         (data : Pixel ** (extents.width * extents.height))
//!             => Bitmap { magic, extents, height };
//! };
//!
//! define Bmp {
//!     alias = Bitmap;
//! };
//! ```
//!
//! From there we can finally lower to a nice Rust API:
//!
//! ```rust,ignore
//! extern crate byteorder;
//!
//! use byteorder::{BigEndian, ReadBytesExt};
//! use std::io;
//! use std::io::prelude::*;
//!
//! pub struct Pixel {
//!     pub r: u8,
//!     pub g: u8,
//!     pub b: u8,
//!     pub a: u8,
//! }
//!
//! impl Pixel {
//!     pub fn read<R: Read>(buf: &mut R) -> io::Result<Pixel> {
//!         let r = buf.read_u8()?;
//!         let g = buf.read_u8()?;
//!         let b = buf.read_u8()?;
//!         let a = buf.read_u8()?;
//!         Ok(Pixel { r, g, b, a })
//!     }
//! }
//!
//! pub struct BitmapExtents {
//!     pub width: u32,
//!     pub height: u32,
//! }
//!
//! pub struct Bitmap {
//!     pub magic: [u8; 8],
//!     pub extents: BitmapExtents,
//!     pub data: Vec<Pixel>,
//! }
//!
//! impl Bitmap {
//!     pub fn read<R: Read>(buf: &mut R) -> io::Result<Bitmap> {
//!         let magic = {
//!             let mut magic = [0; 8];
//!             buf.read_exact(&mut magic)?;
//!             if magic == [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00] {
//!                 return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid binary data"));
//!             }
//!             magic
//!         };
//!         let extents = {
//!             let width = buf.read_u32::<BigEndian>()?;
//!             let height = buf.read_u32::<BigEndian>()?;
//!             BitmapExtents { width, height }
//!         };
//!         let data = {
//!             (0..extents.width * extents.height)
//!                 .map(|_| Pixel::read(buf))
//!                 .collect::<Result<_, _>>()?
//!         };
//!         Ok(Bitmap { magic, extents, data })
//!     }
//! }
//!
//! pub type Bmp = Bitmap;
//! ```
//!
//! # Questions
//!
//! - Where will we uncurry functions and type constructors?
//! - What will a cursor-style API look like? I want to be able to jump into
//!   collections without parsing the entire thing, eg:
//!
//!   ```rust,ignore
//!   let bitmap = Birmap::new(data);
//!   bitmap.data().get(0);
//!   let data = bitmap.data().iter.collect::<Vec_>>();
//!   ```
//!
//! - How should we avoid naming conflicts during the lowering of the owned
//!   nominal IR to Rust?

pub mod nominal;
pub mod owned;
