use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    io::{self},
};

use async_recursion::async_recursion;
use flate2::Compression;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::{NbtCompound, NbtList, NbtTag, raw};

/// Describes the flavors of NBT data: uncompressed, Zlib compressed and Gz compressed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Flavor {
    /// Uncompressed NBT data.
    Uncompressed,
    /// Zlib compressed NBT data. When writing, the default compression level will be used.
    ZlibCompressed,
    /// Zlib compressed NBT data with the given compression level.
    ZlibCompressedWith(Compression),
    /// Gz compressed NBT data. When writing, the default compression level will be used.
    GzCompressed,
    /// Gz compressed NBT data with the given compression level.
    GzCompressedWith(Compression),
}

/// Reads the given flavor of NBT data from the given reader, returning the resulting NBT
/// compound and associated root name.
pub async fn read_nbt<R: AsyncRead + Unpin + Send>(
    reader: &mut R,
    flavor: Flavor,
    skip_root_name: bool,
) -> Result<(NbtCompound, Option<String>), NbtIoError> {
    match flavor {
        Flavor::Uncompressed => read_nbt_uncompressed(reader, skip_root_name).await,
        _ => Err(NbtIoError::Custom(Box::from("Only uncompressed NBT is supported at this time due to async limitations")))
        // Flavor::ZlibCompressed | Flavor::ZlibCompressedWith(_) =>
        //     read_nbt_uncompressed(&mut ZlibDecoder::new(reader), skip_root_name),
        // Flavor::GzCompressed | Flavor::GzCompressedWith(_) =>
        //     read_nbt_uncompressed(&mut GzDecoder::new(reader), skip_root_name),
    }
}

async fn read_nbt_uncompressed<R: AsyncRead + Unpin + Send>(reader: &mut R, skip_root_name: bool) -> Result<(NbtCompound, Option<String>), NbtIoError> {
    let root_id = raw::read_u8(reader).await?;
    if root_id != 0xA {
        return Err(NbtIoError::TagTypeMismatch {
            expected: 0xA,
            found: root_id,
        });
    }
    let root_name = if !skip_root_name {
        Some(raw::read_string(reader).await?)
    } else {
        None
    };

    match read_tag_body_const::<_, 0xA>(reader).await {
        Ok(NbtTag::Compound(compound)) => Ok((compound, root_name)),
        Err(e) => Err(e),
        _ => unreachable!(),
    }
}

async fn read_tag_body_dyn<R: AsyncRead + Unpin + Send>(reader: &mut R, tag_id: u8) -> Result<NbtTag, NbtIoError> {
    macro_rules! drive_reader {
        ($($id:literal)*) => {
            match tag_id {
                $( $id => read_tag_body_const::<_, $id>(reader).await, )*
                _ => Err(NbtIoError::InvalidTagId(tag_id))
            }
        };
    }

    drive_reader!(0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xA 0xB 0xC)
}

#[inline]
#[async_recursion]
async fn read_tag_body_const<R: AsyncRead + Unpin + Send, const TAG_ID: u8>(reader: &mut R) -> Result<NbtTag, NbtIoError> {
    let tag = match TAG_ID {
        0x1 => NbtTag::Byte(raw::read_i8(reader).await?),
        0x2 => NbtTag::Short(raw::read_i16(reader).await?),
        0x3 => NbtTag::Int(raw::read_i32(reader).await?),
        0x4 => NbtTag::Long(raw::read_i64(reader).await?),
        0x5 => NbtTag::Float(raw::read_f32(reader).await?),
        0x6 => NbtTag::Double(raw::read_f64(reader).await?),
        0x7 => {
            let len = raw::read_i32(reader).await? as usize;
            let mut array = Vec::with_capacity(len);
            for _ in 0..len {
                array.push(raw::read_i8(reader).await?);
            }
            NbtTag::ByteArray(array)
        }
        0x8 => NbtTag::String(raw::read_string(reader).await?),
        0x9 => {
            let tag_id = raw::read_u8(reader).await?;
            let len = raw::read_i32(reader).await? as usize;

            // Make sure we don't have a list of TAG_End unless it's empty or an invalid type
            if tag_id > 0xC || (tag_id == 0 && len > 0) {
                return Err(NbtIoError::InvalidTagId(tag_id));
            }

            if len == 0 {
                return Ok(NbtTag::List(NbtList::new()));
            }

            let mut list = NbtList::with_capacity(len);

            macro_rules! drive_reader {
                ($($id:literal)*) => {
                    match tag_id {
                        $(
                            $id => {
                                for _ in 0 .. len {
                                    list.push(read_tag_body_const::<_, $id>(reader).await?);
                                }
                            }
                        )*
                        _ => return Err(NbtIoError::InvalidTagId(tag_id))
                    }
                };
            }

            drive_reader!(0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xA 0xB 0xC);

            NbtTag::List(list)
        }
        0xA => {
            let mut compound = NbtCompound::new();
            let mut tag_id = raw::read_u8(reader).await?;

            // Read until TAG_End
            while tag_id != 0x0 {
                let name = raw::read_string(reader).await?;
                let tag = read_tag_body_dyn(reader, tag_id).await?;
                compound.insert(name, tag);
                tag_id = raw::read_u8(reader).await?;
            }

            NbtTag::Compound(compound)
        }
        0xB => {
            let len = raw::read_i32(reader).await? as usize;
            NbtTag::IntArray(raw::read_i32_array(reader, len).await?)
        }
        0xC => {
            let len = raw::read_i32(reader).await? as usize;
            NbtTag::LongArray(raw::read_i64_array(reader, len).await?)
        }
        _ => unreachable!("read_tag_body_const called with unchecked TAG_ID"),
    };

    Ok(tag)
}

pub fn size(root: &NbtCompound, flavor: Flavor, root_name: Option<String>) -> Result<(usize), NbtIoError> {
    match flavor {
        Flavor::Uncompressed => size_nbt_uncompressed(root, root_name),
        _ => Err(NbtIoError::Custom(Box::from("Only uncompressed NBT is supported at this time due to async limitations")))
    }
}

fn size_nbt_uncompressed(root: &NbtCompound, root_name: Option<String>) -> Result<(usize), NbtIoError> {
    let mut size = 1; // root id
    if let Some(root_name) = root_name {
        size += raw::size_string(root_name.as_str());
    }

    for (name, tag) in root.inner() {
        size += 1; // tag id
        raw::size_string(name);
        size_tag_body(tag);
    }

    size += 1; // TAG_End
    Ok(size)
}

fn size_tag_body(tag: &NbtTag) -> usize {
    match tag {
        &NbtTag::Byte(value) => 1,
        &NbtTag::Short(value) => 2,
        &NbtTag::Int(value) => 4,
        &NbtTag::Long(value) => 8,
        &NbtTag::Float(value) => 4,
        &NbtTag::Double(value) => 8,
        NbtTag::ByteArray(value) => {
            4 // length
                + 1 * value.len() // bytes
        }
        NbtTag::String(value) => raw::size_string(value),
        NbtTag::List(value) =>
        1 // list type
        + 4 // length
        + value.as_ref().iter().map(size_tag_body).sum::<usize>(),
        NbtTag::Compound(value) => {
            value.inner().iter().map(|(name, tag)| {
                1 // tag id
                    + raw::size_string(name) // name
                    + size_tag_body(tag) // tag body
            }).sum::<usize>() + 1 // TAG_End
        }
        NbtTag::IntArray(value) => {
            4 // length
                + 4 * value.len() // ints
        }
        NbtTag::LongArray(value) => {
            4 // length
                + 8 * value.len() // longs
        }
    }
}

/// Writes the given flavor of NBT data to the given writer. If no root name is provided, the string
/// is omitted entirely (this is required since 1.20.2).
pub async fn write_nbt<W: AsyncWrite + Unpin + Send>(
    writer: &mut W,
    root_name: Option<&str>,
    root: &NbtCompound,
    flavor: Flavor,
) -> Result<(), NbtIoError> {
    match flavor {
        Flavor::Uncompressed => {
            return write_nbt_uncompressed(writer, root_name, root).await;
        }
        // Flavor::ZlibCompressed => (2, Compression::default()),
        // Flavor::ZlibCompressedWith(compression) => (2, compression),
        // Flavor::GzCompressed => (1, Compression::default()),
        // Flavor::GzCompressedWith(compression) => (1, compression),
        _ => return Err(NbtIoError::Custom(Box::from("Only uncompressed NBT is supported at this time due to async limitations")))
    };

    // if mode == 1 {
    //     write_nbt_uncompressed(&mut GzEncoder::new(writer, compression), root_name, root)
    // } else {
    //     write_nbt_uncompressed(&mut ZlibEncoder::new(writer, compression), root_name, root)
    // }
}

/// Writes the given tag compound with the given name to the provided writer, writing only the raw
/// NBT data without any compression.
async fn write_nbt_uncompressed<W>(
    writer: &mut W,
    root_name: Option<&str>,
    root: &NbtCompound,
) -> Result<(), NbtIoError>
    where
        W: AsyncWrite + Unpin + Send,
{
    // Compound ID
    raw::write_u8(writer, 0xA).await?;
    if let Some(root_name) = root_name {
        raw::write_string(writer, root_name).await?;
    }
    for (name, tag) in root.inner() {
        raw::write_u8(writer, raw::id_for_tag(Some(tag))).await?;
        raw::write_string(writer, name).await?;
        write_tag_body(writer, tag).await?;
    }
    raw::write_u8(writer, raw::id_for_tag(None)).await?;
    Ok(())
}

#[async_recursion]
async fn write_tag_body<W: AsyncWrite + Unpin + Send>(writer: &mut W, tag: &NbtTag) -> Result<(), NbtIoError> {
    match tag {
        &NbtTag::Byte(value) => raw::write_i8(writer, value).await?,
        &NbtTag::Short(value) => raw::write_i16(writer, value).await?,
        &NbtTag::Int(value) => raw::write_i32(writer, value).await?,
        &NbtTag::Long(value) => raw::write_i64(writer, value).await?,
        &NbtTag::Float(value) => raw::write_f32(writer, value).await?,
        &NbtTag::Double(value) => raw::write_f64(writer, value).await?,
        NbtTag::ByteArray(value) => {
            raw::write_i32(writer, value.len() as i32).await?;
            for &byte in value.iter() {
                raw::write_i8(writer, byte).await?;
            }
        }
        NbtTag::String(value) => raw::write_string(writer, value).await?,
        NbtTag::List(value) =>
            if value.is_empty() {
                writer.write_all(&[raw::id_for_tag(None), 0, 0, 0, 0]).await?;
            } else {
                let list_type = raw::id_for_tag(Some(&value[0]));
                raw::write_u8(writer, list_type).await?;
                raw::write_i32(writer, value.len() as i32).await?;

                for sub_tag in value.as_ref() {
                    let tag_id = raw::id_for_tag(Some(sub_tag));
                    if tag_id != list_type {
                        return Err(NbtIoError::NonHomogenousList {
                            list_type,
                            encountered_type: tag_id,
                        });
                    }

                    write_tag_body(writer, sub_tag).await?;
                }
            },
        NbtTag::Compound(value) => {
            for (name, tag) in value.inner() {
                raw::write_u8(writer, raw::id_for_tag(Some(tag))).await?;
                raw::write_string(writer, name).await?;
                write_tag_body(writer, tag).await?;
            }

            // TAG_End
            raw::write_u8(writer, raw::id_for_tag(None)).await?;
        }
        NbtTag::IntArray(value) => {
            raw::write_i32(writer, value.len() as i32).await?;

            for &int in value.iter() {
                raw::write_i32(writer, int).await?;
            }
        }
        NbtTag::LongArray(value) => {
            raw::write_i32(writer, value.len() as i32).await?;

            for &long in value.iter() {
                raw::write_i64(writer, long).await?;
            }
        }
    }

    Ok(())
}

/// Describes an error which occurred during the reading or writing of NBT data.
#[derive(Debug)]
pub enum NbtIoError {
    /// A native I/O error.
    StdIo(io::Error),
    /// No root tag was found. All NBT data must start with a valid compound tag.
    MissingRootTag,
    /// A sequential data structure was found to be non-homogenous. All sequential structures
    /// in NBT data are homogenous.
    NonHomogenousList {
        /// The list type.
        list_type: u8,
        /// The encountered type.
        encountered_type: u8,
    },
    /// A type requested an option to be read from a list. Since options are indicated by the
    /// absence or presence of a tag, and since all sequential types are length-prefixed,
    /// options cannot exists within arrays in NBT data.
    OptionInList,
    /// A sequential type without a definite length was passed to a serializer.
    MissingLength,
    /// An invalid tag ID was encountered.
    InvalidTagId(u8),
    /// The first tag ID was expected, but the second was found.
    TagTypeMismatch {
        /// The expected ID.
        expected: u8,
        /// The found ID.
        found: u8,
    },
    /// A sequential type was expected, but another was found.
    ExpectedSeq,
    /// An enum representation was expected, but another was found.
    ExpectedEnum,
    /// An invalid map key was encountered.
    InvalidKey,
    /// An invalid enum variant was encountered.
    InvalidEnumVariant,
    /// An invalid cesu8 string was encountered.
    InvalidCesu8String,
    /// An unsupported type was passed to a serializer or queried from a deserializer.
    UnsupportedType(&'static str),
    /// A custom error message.
    Custom(Box<str>),
}

#[cfg(feature = "serde")]
impl serde::ser::Error for NbtIoError {
    fn custom<T>(msg: T) -> Self
        where T: Display {
        NbtIoError::Custom(msg.to_string().into_boxed_str())
    }
}

#[cfg(feature = "serde")]
impl serde::de::Error for NbtIoError {
    fn custom<T>(msg: T) -> Self
        where T: Display {
        NbtIoError::Custom(msg.to_string().into_boxed_str())
    }
}

impl From<io::Error> for NbtIoError {
    fn from(error: io::Error) -> Self {
        NbtIoError::StdIo(error)
    }
}

impl Display for NbtIoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NbtIoError::StdIo(error) => write!(f, "{}", error),
            NbtIoError::MissingRootTag =>
                write!(f, "NBT tree does not start with a valid root tag."),
            &NbtIoError::NonHomogenousList {
                list_type,
                encountered_type,
            } => write!(
                f,
                "Encountered non-homogenous list or sequential type: expected {:X} but found {:X}",
                list_type, encountered_type
            ),
            NbtIoError::OptionInList => write!(
                f,
                "Minecraft's NBT format cannot support options in sequential data structures"
            ),
            NbtIoError::MissingLength => write!(
                f,
                "Sequential types must have an initial computable length to be serializable"
            ),
            &NbtIoError::InvalidTagId(id) => write!(
                f,
                "Encountered invalid tag ID 0x{:X} during deserialization",
                id
            ),
            &NbtIoError::TagTypeMismatch { expected, found } => write!(
                f,
                "Tag type mismatch: expected 0x{:X} but found 0x{:X}",
                expected, found
            ),
            NbtIoError::ExpectedSeq => write!(f, "Expected sequential tag type (array)"),
            NbtIoError::ExpectedEnum => write!(
                f,
                "Encountered invalid enum representation in the NBT tag tree"
            ),
            NbtIoError::InvalidKey => write!(f, "Map keys must be a valid string"),
            NbtIoError::InvalidEnumVariant =>
                write!(f, "Encountered invalid enum variant while deserializing"),
            NbtIoError::InvalidCesu8String => write!(f, "Encountered invalid CESU8 string"),
            NbtIoError::UnsupportedType(ty) =>
                write!(f, "Type {} is not supported by Minecraft's NBT format", ty),
            NbtIoError::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl Error for NbtIoError {}
