use std::io::Cursor;

use async_nbt::io::{read_nbt, write_nbt, Flavor, NbtIoError};

const UNCOMPRESSED_NBT: &[u8] = include_bytes!("./assets/uncompressed_nbt.nbt");
const GZ_COMPRESSED_NBT: &[u8] = include_bytes!("./assets/gz_compressed_nbt.nbt");

#[tokio::main]
async fn main() -> Result<(), NbtIoError> {
    // You can read nbt from raw bytes
    let nbt = read_nbt(&mut Cursor::new(UNCOMPRESSED_NBT), Flavor::Uncompressed, false).await?.0;

    println!("uncompressed: {}", nbt);

    // by changing the Flavor you can read compressed nbt too
    let compressed_nbt = read_nbt(&mut Cursor::new(GZ_COMPRESSED_NBT), Flavor::GzCompressed, false).await?.0;

    println!("gz compressed: {}", compressed_nbt);

    let mut vec = Vec::new();

    // You can also write nbt to anything that implements std::io::Write
    write_nbt(&mut vec, None, &nbt, Flavor::Uncompressed).await?;

    println!("bytes: {:02X?}", vec);

    Ok(())
}
