extern crate byteorder;

use byteorder::{BigEndian, LittleEndian, ReadBytesExt};
use std::io;
use std::io::prelude::*;

#[inline]
pub fn from_u8<R: Read>(r: &mut R) -> io::Result<u8> {
    r.read_u8()
}

#[inline]
pub fn from_i8<R: Read>(r: &mut R) -> io::Result<i8> {
    r.read_i8()
}

#[inline]
pub fn from_u16le<R: Read>(r: &mut R) -> io::Result<u16> {
    r.read_u16::<LittleEndian>()
}

#[inline]
pub fn from_u32le<R: Read>(r: &mut R) -> io::Result<u32> {
    r.read_u32::<LittleEndian>()
}

#[inline]
pub fn from_u64le<R: Read>(r: &mut R) -> io::Result<u64> {
    r.read_u64::<LittleEndian>()
}

#[inline]
pub fn from_i16le<R: Read>(r: &mut R) -> io::Result<i16> {
    r.read_i16::<LittleEndian>()
}

#[inline]
pub fn from_i32le<R: Read>(r: &mut R) -> io::Result<i32> {
    r.read_i32::<LittleEndian>()
}

#[inline]
pub fn from_i64le<R: Read>(r: &mut R) -> io::Result<i64> {
    r.read_i64::<LittleEndian>()
}

#[inline]
pub fn from_f32le<R: Read>(r: &mut R) -> io::Result<f32> {
    r.read_f32::<LittleEndian>()
}

#[inline]
pub fn from_f64le<R: Read>(r: &mut R) -> io::Result<f64> {
    r.read_f64::<LittleEndian>()
}

#[inline]
pub fn from_u16be<R: Read>(r: &mut R) -> io::Result<u16> {
    r.read_u16::<BigEndian>()
}

#[inline]
pub fn from_u32be<R: Read>(r: &mut R) -> io::Result<u32> {
    r.read_u32::<BigEndian>()
}

#[inline]
pub fn from_u64be<R: Read>(r: &mut R) -> io::Result<u64> {
    r.read_u64::<BigEndian>()
}

#[inline]
pub fn from_i16be<R: Read>(r: &mut R) -> io::Result<i16> {
    r.read_i16::<BigEndian>()
}

#[inline]
pub fn from_i32be<R: Read>(r: &mut R) -> io::Result<i32> {
    r.read_i32::<BigEndian>()
}

#[inline]
pub fn from_i64be<R: Read>(r: &mut R) -> io::Result<i64> {
    r.read_i64::<BigEndian>()
}

#[inline]
pub fn from_f32be<R: Read>(r: &mut R) -> io::Result<f32> {
    r.read_f32::<BigEndian>()
}

#[inline]
pub fn from_f64be<R: Read>(r: &mut R) -> io::Result<f64> {
    r.read_f64::<BigEndian>()
}
