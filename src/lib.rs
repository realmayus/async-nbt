#![warn(rust_2018_idioms, missing_debug_implementations, missing_docs)]

/*!
Provides support for encoding and decoding Minecraft's NBT format. This crate supports both
zlib and gz compression, and also provides tools for converting NBT data to stringified NBT
(SNBT) and vice versa.

# Basic Usage

The basic unit of NBT data is the [`NbtTag`]. Larger data structures are
represented through a tree of compounds (hash maps) and lists (vecs) of NBT tags.

## Creating NBT Data

```
# use async_nbt::*;
let mut compound = NbtCompound::new();
compound.insert("foo", 123);
compound.insert("bar", -3.6f32);

let mut list = NbtList::with_capacity(3);
(1i64..=3).for_each(|x| list.push(x));
compound.insert("list", list);

*compound.get_mut::<_, &mut i32>("foo").unwrap() += 1;

assert!(matches!(compound.get::<_, i32>("foo"), Ok(124)));
assert!(compound.get::<_, f64>("bar").is_err());
assert!(compound.get::<_, &NbtTag>("list").is_ok());
```

## Reading and Writing NBT

```
# use async_nbt::*;
use async_nbt::io::{self, Flavor};
use std::io::Cursor;

let mut compound = NbtCompound::new();
compound.insert("foo", 123);
compound.insert("bar", -3.6f32);

let mut binary: Vec<u8> = Vec::new();
io::write_nbt(&mut binary, Some("root-tag"), &compound, Flavor::Uncompressed);

let read_compound = io::read_nbt(&mut Cursor::new(binary), Flavor::Uncompressed, false).unwrap();
assert_eq!(read_compound.1, "root-tag"); // The root tag's name is generally unused
assert_eq!(read_compound.0, compound);
```

# Querying Tags

Generics are used to make the tag querying process as seamless as possible, however this
allows for two types of errors to occur: missing tags (invalid key or index), and tag type
mismatches. Thus, methods that would normally return an [`Option`](Option) in `std` collection
equivalents return a [`Result`](Result) in this crate.

An error converting NBT tags directly into unwrapped values via [`TryFrom`](std::convert::TryFrom)
and [`TryInto`](std::convert::TryInto) is represented by an [`NbtStructureError`](crate::NbtStructureError).
An error querying an [`NbtCompound`] or [`NbtList`] is represented by an [`NbtReprError`](crate::NbtReprError),
which is short for "NBT representation error." See the error's documentation for details.

```
# use async_nbt::*;
use std::convert::TryFrom;

let tag1: NbtTag = vec![1i8, 2, 3].into();
let tag2: NbtTag = "abcde".into();

assert_eq!(Vec::<i8>::try_from(tag1).unwrap(), vec![1i8, 2, 3]);
assert!(i16::try_from(tag2).is_err()); // Type mismatch
```

```
# use async_nbt::*;
let mut compound = NbtCompound::new();
compound.insert("foo", 123);
compound.insert("bar", -3.6f32);

assert!(compound.get::<_, i32>("fooz").is_err()); // Missing tag
assert!(compound.get::<_, i32>("bar").is_err()); // Type mismatch
```

# Collection Types and Iteration

The [`NbtCompound`] and [`NbtList`] types are wrappers around [`Map`](crate::Map)s
and [`Vec`](Vec)s respectively. Because [`NbtTag`]s obscure the type of data actually stored,
these wrappers provide utilities for unpacking tags into concrete types. If greater functionality
is required, then the internal collection managed by these wrappers can be accessed through
calls to `inner`, `inner_mut`, and/or `into_inner`.

## Lists

Minecraft's NBT specification currently has special tags for arrays (or [`Vec`](Vec)s in rust)
of `i8`, `i32`, and `i64`. Thus, vecs of these types can be directly converted into [`NbtTag`]s.
All other NBT-compatible types must be stored in an [`NbtList`].

Obtaining the aforementioned special list types can be done through a regular query.
```
# use async_nbt::*;
let mut compound = NbtCompound::new();
compound.insert("list", vec![10i32, 20, 30]);

compound.get_mut::<_, &mut [i32]>("list")
    .unwrap()
    .iter_mut()
    .for_each(|x| *x /= 10);

let list = compound.get::<_, &[i32]>("list");
assert!(list.is_ok());
assert_eq!(list.unwrap(), [1i32, 2, 3].as_ref());
```

Utility methods are provided for NBT lists to iterate over unpacked values. See
[`iter_map`](crate::NbtList::iter_map) and [`iter_mut_map`](crate::NbtList::iter_mut_map).
```
# use async_nbt::*;
let mut list = NbtList::new();
list.push("abc");
list.push("ijk");
list.push("xyz");

list.iter_mut_map::<&mut String>()
    .for_each(|s| s.unwrap().push('!'));

let mut iter = list.iter_map::<&str>();
assert!(matches!(iter.next(), Some(Ok("abc!"))));
assert!(matches!(iter.next(), Some(Ok("ijk!"))));
assert!(matches!(iter.next(), Some(Ok("xyz!"))));
assert!(matches!(iter.next(), None));
```

NBT lists can be created by cloning data from an iterator (or something which can be
converted into an iterator) via [`clone_from`](crate::NbtList::clone_from).
```
# use async_nbt::*;
let mut list1 = NbtList::new();
list1.push("abc");
list1.push("ijk");
list1.push("xyz");

let list2 = NbtList::clone_from(&["abc", "ijk", "xyz"]);

assert_eq!(list1, list2);
```

## Compounds

[`NbtCompound`]s have the same set of utility functions as [`NbtList`]s, except for the
obvious fact that compounds use string keys instead of indices. Similar to lists, compounds
have [`iter_map`](crate::NbtCompound::iter_map) and [`iter_mut_map`](crate::NbtCompound::iter_mut_map)
utility functions, as well as a [`clone_from`](crate::NbtCompound::clone_from) constructor.
See the documentation for more details.

# Stringified NBT (SNBT)

Minecraft also contains a string encoding of NBT data called SNBT. This encoding is basically an
extension of JSON with stricter types and looser rules regarding string quotation. See the
[`snbt`](crate::snbt) module documentation for more details.

```
# use async_nbt::*;
use async_nbt::snbt;

let tag: NbtTag = vec![10i8, 15, 20].into();
assert_eq!(tag.to_snbt(), "[B;10,15,20]");

let mut compound = NbtCompound::new();
compound.insert("short", -10i16);
compound.insert("string", "fizzbuzz");
compound.insert("array", vec![1i64, 1, 2, 3, 5]);

const SNBT: &str = "{short: -10s, string: fizzbuzz, array: [L; 1, 1, 2, 3, 5]}";

assert_eq!(compound, snbt::parse(SNBT).unwrap());
```

[`NbtCompound`]: crate::NbtCompound
[`NbtList`]: crate::NbtList
[`NbtRepr`]: crate::NbtRepr
[`NbtTag`]: crate::NbtTag
*/

/// Provides efficient serializer and deserializer implementations for arbitrary NBT tag trees. The
/// functions in this module should be used for serializing and deserializing [`NbtCompound`]s
/// over the utilities provided by serde.
///
/// [`NbtCompound`]: crate::NbtCompound
pub mod io;
mod raw;
mod repr;
/// When the `serde` feature is enabled, this module provides `Serializer` and `Deserializer`
/// implementations to link this crate into the serde data model.
///
/// # Example
///
/// ```
/// # extern crate serde;
/// # use serde::{Serialize, Deserialize};
/// use async_nbt::{
///     io::Flavor,
///     serde::{serialize, deserialize}
/// };
///
/// #[derive(Serialize, Deserialize, PartialEq, Debug)]
/// pub struct Entity {
///     name: String,
///     health: f32,
///     position: (f64, f64)
/// }
///
/// let entity = Entity {
///     name: "Ferris".to_owned(),
///     health: 100.0,
///     position: (1.0, -2.0)
/// };
///
/// let serialized: Vec<u8> = serialize(
///     &entity,
///     None, // Name of the root tag
///     Flavor::Uncompressed
/// ).unwrap();
/// let (deserialized, _root_name) = deserialize(
///     &serialized,
///     Flavor::Uncompressed
/// ).unwrap();
///
/// assert_eq!(entity, deserialized);
/// ```
///
/// # Arrays
///
/// By default, all sequential types (vectors, arrays, tuples, etc.) are serialized as tag lists.
/// If you wish to have a type serialized as a byte array, int array, or long array, you can
/// opt-into that by wrapping the type in [`Array`]. An example is shown below.
///
/// ```
/// # extern crate serde;
/// # use serde::{Serialize, Deserialize};
/// use async_nbt::{
///     compound,
///     io::{self, Flavor},
///     serde::{Array, serialize}
/// };
/// use std::io::Cursor;
///
/// #[derive(Serialize)]
/// struct Arrays {
///     int_array: Array<Vec<i32>>,
///     bytes: Array<[i8; 3]>
/// }
///
/// let arrays = Arrays {
///     int_array: Array::from(vec![1i32, -2i32, 3i32]),
///     bytes: Array::from([-32i8, -64, -128])
/// };
///
/// let repr = compound! {
///     "int_array": [I; 1, -2, 3],
///     "bytes": [B; -32, -64, -128]
/// };
///
/// let serialized: Vec<u8> = serialize(
///     &arrays,
///     None,
///     Flavor::Uncompressed
/// ).unwrap();
///
/// assert_eq!(
///     repr,
///     io::read_nbt(&mut Cursor::new(serialized), Flavor::Uncompressed, false).unwrap().0
/// );
/// ```
///
/// # Enum Representation
///
/// All enum types can be represented in NBT, however not all types are efficiently representable.
/// Unit variants are serialized according to their index in the enum, while all other variant
/// types are serialized as compounds. Although we do not currently support serializing unit
/// variants by name, we will likely add this feature in future versions, and already support
/// deserializing unit variants by name.
///
/// ```
/// # extern crate serde;
/// # use serde::{Serialize, Deserialize};
/// use async_nbt::{
///     compound,
///     io::{self, Flavor},
///     serde::serialize
/// };
/// use std::io::Cursor;
///
/// #[derive(Serialize)]
/// enum MyEnum {
///     Unit,
///     Newtype(i32),
///     Tuple(String, String),
///     Struct {
///         a: i32,
///         b: f32
///     }
/// }
///
/// #[derive(Serialize)]
/// struct AllVariants {
///     unit: MyEnum,
///     newtype: MyEnum,
///     tuple: MyEnum,
///     strukt: MyEnum
/// }
///
/// let data = AllVariants {
///     unit: MyEnum::Unit,
///     newtype: MyEnum::Newtype(10),
///     tuple: MyEnum::Tuple("foo".to_owned(), "bar".to_owned()),
///     strukt: MyEnum::Struct {
///         a: -1,
///         b: 4.669201
///     }
/// };
///
/// let repr = compound! {
///     "unit": 0i32,
///     "newtype": {
///         "Newtype": 10i32
///     },
///     "tuple": {
///         "Tuple": ["foo", "bar"]
///     },
///     "strukt": {
///         "Struct": {
///             "a": -1i32,
///             "b": 4.669201f32
///         }
///     }
/// };
///
/// let serialized: Vec<u8> = serialize(
///     &data,
///     None,
///     Flavor::Uncompressed
/// ).unwrap();
///
/// assert_eq!(
///     repr,
///     io::read_nbt(&mut Cursor::new(serialized), Flavor::Uncompressed, false).unwrap().0
/// );
/// ```
///
/// # Borrowing Data
///
/// With some inventive coding techniques, we were able to allow for borrowing data during
/// deserialization, for free, without significantly changing the original serde API. This feature
/// can only be taken advantage of by using [`deserialize_from_buffer`], which requires that the
/// input is in the form of a slice of uncompressed, binary NBT data.
///
/// Note that although you can borrow bytes for free, because NBT uses Java's CESU-8 encoding,
/// attempting to borrow a string may fail if it is not UTF-8, hence it is recommended to use
/// a [`Cow`] with the attribute `#[serde(borrow)]` in case the string needs to be re-encoded into
/// an owned value.
///
/// ```
/// # extern crate serde;
/// # use serde::Deserialize;
/// use async_nbt::{
///     compound,
///     io::{self, Flavor},
///     serde::deserialize_from_buffer
/// };
/// use std::{
///     borrow::Cow,
///     io::Cursor
/// };
///
/// #[derive(Deserialize, PartialEq, Debug)]
/// struct Borrowed<'a> {
///     bytes: &'a [u8], // Bytes must be borrowed as unsigned
///     #[serde(borrow)]
///     string: Cow<'a, str>
/// }
///
/// let repr = compound! {
///     "bytes": [B; 2, 3, 5, 7, 11],
///     "string": "this is utf-8"
/// };
///
/// let mut bin_nbt = Cursor::new(Vec::<u8>::new());
/// io::write_nbt(&mut bin_nbt, None, &repr, Flavor::Uncompressed).unwrap();
/// let bin_nbt = bin_nbt.into_inner();
///
/// const PRIMES: &[u8] = &[2, 3, 5, 7, 11];
///
/// assert_eq!(
///     deserialize_from_buffer::<Borrowed<'_>>(&bin_nbt).unwrap().0,
///     Borrowed {
///         bytes: PRIMES,
///         string: Cow::Borrowed("this is utf-8")
///     }
/// );
/// ```
///
/// [`Array`]: crate::serde::Array
/// [`Cow`]: std::borrow::Cow
/// [`deserialize_from_buffer`]: crate::serde::deserialize_from_buffer
#[cfg(feature = "serde")]
#[allow(missing_debug_implementations)]
pub mod serde;
mod tag;

/// Provides support for parsing stringified NBT data.
///
/// SNBT is essentially an extension of JSON. It uses the same overarching syntax with some changes
/// to enforce stronger types.
///
/// # Numbers
///
/// Numbers in SNBT generally have a single-character suffix specifying their type (with `i32` and
/// `f64` being exceptions). If a number without a decimal point is encountered without a type
/// specifier, then the parser assumes it is an int. Likewise, if a number with a decimal point
/// but no type specifier is encountered, then it is assumed to be a double. Note that the type
/// specifier for doubles (`D` or `d`) is optional, however the integer type specifier (`I` or `i`)
/// is reserved for arrays and cannot be appended to an integer. Examples are shown below:
///  - Byte (`i8`): `2B`, `-3b`
///  - Short (`i16`): `17S`, `-1024s`
///  - Int (`i32`): `123`
///  - Long (`i64`): `43046721L`
///  - Float (`f32`): `3.141F`, `0.0f`
///  - Double (`f64`): `18932.214`, `10.2D`
///
/// Booleans are encoded as bytes, so `0b` represents `false` and `1b` (or any non-zero byte value)
/// represents `true`.
///
/// # Strings
///
/// SNBT treats any sequence of unicode characters not representing another token to be a string. For this
/// reason, strings are not required to be in quotes, however they can optionally be enclosed
/// in either single or double quotes. In other words, `foo` is equivalent to `"foo"` and `'foo'`, and
/// `"\"quoted\""` is equivalent to `'"quoted"'`. Although Minecraft's parser discourages the use of
/// whitespace in SNBT, this parser implementation is fully capable of handling it.
/// The way whitespace is ignored is by trimming strings, so if leading
/// or trailing whitespace is required, then the string must be enclosed in quotes.
///
/// SNBT strings also support several escape sequences:
///  - `\'`, `\"`, `\\`: copies the second character verbatim
///  - `\n`: new line
///  - `\r`: carriage return
///  - `\t`: tab
///  - `\uXXXX`: an unsigned hex number representing a unicode character
///
/// These escape sequences are only applied if a string is surrounded by quotes. An unquoted escape
/// sequence will be taken verbatim.
///
/// # Arrays and Lists
///
/// There are three array types supported by Minecraft's NBT formal: byte arrays, int arrays, and
/// long arrays. To differentiate an [`NbtList`](crate::NbtList) from an array, arrays start with
/// its values' type specifier followed by a semicolon. For example, an empty int array is denoted
/// by `[I;]`, and an example of a long array is `[L; -1, -2, -3]`. It is not necessary to put a
/// type specifier on the elements of an array, however the array must be homogenously typed, meaning
/// each element is of the same type.
///
/// NBT lists also use the square bracket syntax, however they do not contain a type specifier.
/// For example, a list of strings may look like `[foo, bar, baz]`. NBT lists, even though they
/// theoretically can contain multiple different types, must also be homogenous. The parser will
/// throw an error if the list is non-homogenous. Note that it is necessary to include the type specifier
/// for each element in an NBT list where applicable.
///
/// # Compounds
///
/// All valid SNBT strings have a compound as the root tag. Compounds, like JSON objects, follow the
/// syntax of `{key: value, ...}`. Compounds must have every key be a string, but its values do not have
/// to be homogenously typed. Whitespace is allowed to make compounds more readable, however one should
/// refer to the section on strings to avoid unexpected elisions.
pub mod snbt;

pub use repr::*;
pub use tag::*;

/// A utility macro for constructing [`NbtCompound`]s.
///
/// With exceptions for arrays and compounds, all keys and values must be well-formed rust
/// expressions. The benefit of this is that local variables can be included in the generated
/// compound. If a local variable is of the type `Vec<T>`, then the generated code will try to
/// convert the variable to an [`NbtTag`], which will fail unless `Vec<T>` maps onto a specialized
/// array type. If it is desireable for the variable to serialize as a tag list, then it should be
/// wrapped in [`NbtList`]`::from`.
/// ```
/// # use async_nbt::{NbtCompound, NbtList, compound};
/// let product = 87235i32 * 932i32;
/// let byte_array = vec![0i8, 2, 4];
/// let tag_list = byte_array.clone();
///
/// let compound = compound! {
///     "product": product,
///     "foo": "bar",
///     "byte_array": byte_array,
///     "tag_list": NbtList::from(tag_list)
/// };
///
/// let mut manual_compound = NbtCompound::new();
/// manual_compound.insert("product", 81303020i32);
/// manual_compound.insert("foo", "bar");
/// manual_compound.insert("byte_array", vec![0i8, 2, 4]);
/// manual_compound.insert("tag_list", NbtList::from(vec![0i8, 2, 4]));
///
/// assert_eq!(compound, manual_compound);
/// ```
///
/// Similar to SNBT, constant specialized array types can be opted-into with a type specifier:
/// ```
/// # use async_nbt::{NbtTag, compound};
/// let compound = compound! {
///     "byte_array": [B; 1, 2, 3],
///     "int_array": [I; 4, 5, 6],
///     "long_array": [L; 7, 8, 9],
///     "tag_array": [10, 11, 12]
/// };
///
/// assert!(matches!(compound.get::<_, &NbtTag>("byte_array"), Ok(NbtTag::ByteArray(_))));
/// assert!(matches!(compound.get::<_, &NbtTag>("int_array"), Ok(NbtTag::IntArray(_))));
/// assert!(matches!(compound.get::<_, &NbtTag>("long_array"), Ok(NbtTag::LongArray(_))));
/// assert!(matches!(compound.get::<_, &NbtTag>("tag_array"), Ok(NbtTag::List(_))));
///
/// assert_eq!(
///     compound.get::<_, &[i64]>("long_array")
///         .unwrap()
///         .iter()
///         .copied()
///         .sum::<i64>(),
///     24
/// );
/// ```
///
/// Just like in JSON or SNBT, compounds are enclosed by braces:
/// ```
/// # use async_nbt::{NbtCompound, NbtList, compound};
/// let compound = compound! {
///     "nested": {
///         "a": [I;],
///         "b": []
///     }
/// };
///
/// let mut outer = NbtCompound::new();
/// let mut nested = NbtCompound::new();
/// nested.insert("a", Vec::<i32>::new());
/// nested.insert("b", NbtList::new());
/// outer.insert("nested", nested);
///
/// assert_eq!(compound, outer);
/// ```
///
/// [`NbtCompound`]: crate::NbtCompound
/// [`NbtTag`]: crate::NbtTag
/// [`NbtList`]: crate::NbtList
pub use quartz_nbt_macros::compound;
