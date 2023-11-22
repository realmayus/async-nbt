use crate::{io::NbtIoError, NbtTag};
use std::{
    io::{Result},
    mem::ManuallyDrop,
    result::Result as StdResult,
    slice,
};
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};
use tokio::io::AsyncReadExt;

#[inline]
pub const fn id_for_tag(tag: Option<&NbtTag>) -> u8 {
    match tag {
        None => 0x0, // TAG_End
        Some(NbtTag::Byte(..)) => 0x1,
        Some(NbtTag::Short(..)) => 0x2,
        Some(NbtTag::Int(..)) => 0x3,
        Some(NbtTag::Long(..)) => 0x4,
        Some(NbtTag::Float(..)) => 0x5,
        Some(NbtTag::Double(..)) => 0x6,
        Some(NbtTag::ByteArray(..)) => 0x7,
        Some(NbtTag::String(..)) => 0x8,
        Some(NbtTag::List(..)) => 0x9,
        Some(NbtTag::Compound(..)) => 0xA,
        Some(NbtTag::IntArray(..)) => 0xB,
        Some(NbtTag::LongArray(..)) => 0xC,
    }
}

#[cfg(feature = "serde")]
#[inline]
pub async fn read_bool<R: AsyncRead + Unpin>(reader: &mut R) -> Result<bool> {
    Ok(read_u8(reader).await? != 0)
}

#[inline]
pub async fn read_u8<R: AsyncRead + Unpin>(reader: &mut R) -> Result<u8> {
    reader.read_u8().await
}

#[inline]
pub async fn read_i8<R: AsyncRead + Unpin>(reader: &mut R) -> Result<i8> {
    reader.read_i8().await
}

#[inline]
pub async fn read_i16<R: AsyncRead + Unpin>(reader: &mut R) -> Result<i16> {
    reader.read_i16().await
}

#[inline]
pub async fn read_u16<R: AsyncRead + Unpin>(reader: &mut R) -> Result<u16> {
    reader.read_u16().await
}

#[inline]
pub async fn read_i32<R: AsyncRead + Unpin>(reader: &mut R) -> Result<i32> {
    reader.read_i32().await
}

#[inline]
pub async fn read_i64<R: AsyncRead + Unpin>(reader: &mut R) -> Result<i64> {
    reader.read_i64().await
}

#[inline]
pub async fn read_f32<R: AsyncRead + Unpin>(reader: &mut R) -> Result<f32> {
    reader.read_f32().await
}

#[inline]
pub async fn read_f64<R: AsyncRead + Unpin>(reader: &mut R) -> Result<f64> {
    reader.read_f64().await
}

pub async fn read_string<R: AsyncRead + Unpin>(reader: &mut R) -> StdResult<String, NbtIoError> {
    let len = read_u16(reader).await? as usize;
    let mut bytes = vec![0; len];
    reader.read_exact(&mut bytes).await?;

    let java_decoded = match cesu8::from_java_cesu8(&bytes) {
        Ok(string) => string,
        Err(_) => return Err(NbtIoError::InvalidCesu8String),
    };

    Ok(java_decoded.into_owned())
}

#[cfg(feature = "serde")]
pub async fn read_string_into<'a, R: AsyncRead + Unpin>(
    reader: &mut R,
    dest: &'a mut Vec<u8>,
) -> StdResult<std::borrow::Cow<'a, str>, NbtIoError> {
    let len = read_u16(reader).await? as usize;
    dest.resize(len, 0);
    reader.read_exact(dest).await?;
    match cesu8::from_java_cesu8(dest) {
        Ok(string) => Ok(string),
        Err(_) => Err(NbtIoError::InvalidCesu8String),
    }
}

#[cfg(feature = "serde")]
#[inline]
pub async fn write_bool<W: AsyncWrite + Unpin>(writer: &mut W, value: bool) -> Result<()> {
    write_u8(writer, if value { 1 } else { 0 })
}

#[inline]
pub async fn write_u8<W: AsyncWrite + Unpin>(writer: &mut W, value: u8) -> Result<()> {
    writer.write_u8(value).await
}

#[inline]
pub async fn write_i8<W: AsyncWrite + Unpin>(writer: &mut W, value: i8) -> Result<()> {
    writer.write_i8(value).await
}

#[inline]
pub async fn write_i16<W: AsyncWrite + Unpin>(writer: &mut W, value: i16) -> Result<()> {
    writer.write_i16(value).await
}

#[inline]
pub async fn write_u16<W: AsyncWrite + Unpin>(writer: &mut W, value: u16) -> Result<()> {
    writer.write_u16(value).await
}

#[inline]
pub async fn write_i32<W: AsyncWrite + Unpin>(writer: &mut W, value: i32) -> Result<()> {
    writer.write_i32(value).await
}

#[inline]
pub async fn write_i64<W: AsyncWrite + Unpin>(writer: &mut W, value: i64) -> Result<()> {
    writer.write_i64(value).await
}

#[inline]
pub async fn write_f32<W: AsyncWrite + Unpin>(writer: &mut W, value: f32) -> Result<()> {
    writer.write_f32(value).await
}

#[inline]
pub async fn write_f64<W: AsyncWrite + Unpin>(writer: &mut W, value: f64) -> Result<()> {
    writer.write_f64(value).await
}

pub async fn write_string<W: AsyncWrite + Unpin>(writer: &mut W, string: &str) -> Result<()> {
    let mod_utf8 = cesu8::to_java_cesu8(string);
    write_u16(writer, mod_utf8.len() as u16).await?;
    writer.write_all(&mod_utf8).await
}

pub fn size_string(string: &str) -> usize {
    let mod_utf8 = cesu8::to_java_cesu8(string);
    2 + mod_utf8.len()
}

#[inline]
pub fn cast_byte_buf_to_signed(buf: Vec<u8>) -> Vec<i8> {
    let mut me = ManuallyDrop::new(buf);
    // Pointer cast is valid because i8 and u8 have the same layout
    let ptr = me.as_mut_ptr() as *mut i8;
    let length = me.len();
    let capacity = me.capacity();

    // Safety
    // * `ptr` was allocated by a Vec
    // * i8 has the same size and alignment as u8
    // * `length` and `capacity` came from a valid Vec
    unsafe { Vec::from_raw_parts(ptr, length, capacity) }
}

#[inline]
pub fn cast_byte_buf_to_unsigned(buf: Vec<i8>) -> Vec<u8> {
    let mut me = ManuallyDrop::new(buf);
    // Pointer cast is valid because i8 and u8 have the same layout
    let ptr = me.as_mut_ptr() as *mut u8;
    let length = me.len();
    let capacity = me.capacity();

    // Safety
    // * `ptr` was allocated by a Vec
    // * u8 has the same size and alignment as i8
    // * `length` and `capacity` came from a valid Vec
    unsafe { Vec::from_raw_parts(ptr, length, capacity) }
}

// Currently unused, but might be used later
#[inline]
#[allow(dead_code)]
pub fn cast_bytes_to_signed(bytes: &[u8]) -> &[i8] {
    let data = bytes.as_ptr() as *const i8;
    let len = bytes.len();

    // Safety
    // * `data` is valid for len * 1 bytes
    //     * The entire memory range of `data` is contained in a single
    //       allocated object since it came from a valid slice
    //     * `data` is non-null and aligned correctly for u8 (and thus i8)
    // * `data` points to exactly `len` consecutive bytes
    // * The constructed reference adopts the lifetime of the provided reference
    // * `len` <= isize::MAX because `len` came from a valid slice
    unsafe { slice::from_raw_parts(data, len) }
}

#[inline]
pub fn cast_bytes_to_unsigned(bytes: &[i8]) -> &[u8] {
    let data = bytes.as_ptr() as *const u8;
    let len = bytes.len();

    // Safety
    // * `data` is valid for len * 1 bytes
    //     * The entire memory range of `data` is contained in a single
    //       allocated object since it came from a valid slice
    //     * `data` is non-null and aligned correctly for i8 (and thus u8)
    // * `data` points to exactly `len` consecutive bytes
    // * The constructed reference adopts the lifetime of the provided reference
    // * `len` <= isize::MAX because `len` came from a valid slice
    unsafe { slice::from_raw_parts(data, len) }
}

#[inline]
pub async fn read_i32_array<R: AsyncRead + Unpin>(reader: &mut R, len: usize) -> Result<Vec<i32>> {
    // read an i32 array from `reader` with length `len`
    let mut array = Vec::with_capacity(len);
    for _ in 0..len {
        array.push(read_i32(reader).await?);
    }
    Ok(array)
}

#[inline]
pub async fn read_i64_array<R: AsyncRead + Unpin>(reader: &mut R, len: usize) -> Result<Vec<i64>> {
    // read an i32 array from `reader` with length `len`
    let mut array = Vec::with_capacity(len);
    for _ in 0..len {
        array.push(read_i64(reader).await?);
    }
    Ok(array)
}