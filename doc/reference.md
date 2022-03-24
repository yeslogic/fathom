# Language Reference

A reference-level documentation for the Fathom data description language.

This is intended as a user-facing language reference, _not_ a language
specification. A more precise specification of the lexical and abstract syntax,
elaboration, and core language is forthcoming.

## Contents

- [Structure](#structure)
  - [Names](#names)
  - [Let expressions](#let-expressions)
  - [Match expressions](#match-expressions)
  - [Holes](#holes)
  - [Placeholders](#placeholders)
  - [Annotated terms](#annotated-terms)
  - [Grouped terms](#grouped-terms)
- [Universes](#universes)
- [Formats](#formats)
  - [Format types](#format-types)
  - [Format representations](#format-representations)
  - [Record formats](#record-formats)
  - [Overlap formats](#overlap-formats)
  - [Number formats](#number-formats)
  - [Array formats](#array-formats)
  - [Link formats](#link-formats)
  - [Stream position formats](#stream-position-formats)
  - [Succeed format](#succeed-format)
  - [Fail format](#fail-format)
- [Functions](#functions)
  - [Function types](#function-types)
  - [Function literals](#function-literals)
  - [Function applications](#function-applications)
- [Records](#records)
  - [Record types](#record-types)
  - [Record literals](#record-literals)
  - [Record projections](#record-projections)
- [Numbers](#numbers)
  - [Number types](#number-types)
  - [Number literals](#array-literals)
  - [String literals](#string-literals)
  - [Number Operations](#number-operations)
- [Arrays](#arrays)
  - [Array types](#array-types)
  - [Array literals](#array-literals)
- [Positions](#positions)
- [References](#references)
- [Void](#void)

## Structure

This section descibes basic structural parts of Fathom.

### Names

Names start with a an ASCII letter (`a`..`z` or `A`..`Z`), and end with a
sequence of ASCII letters (`a`..`z` or `A`..`Z`), numbers (`0`..`9`), or
underscores (`_`).

During elaboration, names are resolved to variables bound by:

- [let expressions](#let-expressions)
- [function types](#function-types)
- [function literals](#function-literals)
- [record formats](#record-formats)
- [overlap formats](#overlap-formats)
- [record types](#record-types)

Depending on the binder, these variables might either be bound as definitions or
as parameters. If a variable is bound as a definition it reduces to a
definition during evaluation.

If no binding is found, names can refer to one of the built-in primitives:

- `Format`, `Repr`
- `u8`, `u16be`, `u16le`, `u32be`, `u32le`, `u64be`, `u64le`
- `s8`, `s16be`, `s16le`, `s32be`, `s32le`, `s64be`, `s64le`
- `f32be`, `f32le`, `f64be`, `f64le`
- `array8`, `array16`, `array32`, `array64`
- `link8`, `link16`, `link32`, `link64`
- `stream_pos`
- `succeed`, `fail`
- `U8`, `U16`, `U32`, `U64`, `S8`, `S16`, `S32`, `S64`, `F32`, `F64`
- `Array8`, `Array16`, `Array32`, `Array64`
- `Pos`, `Ref`
- `Void`

### Let expressions

Shared definitions can be described using let expressions.

A let expression is begun using the `let` keyword, followed by a pattern
binding. Any bindings introduced by the pattern are then bound as definitions
in the _body term_ that follows.

For example:

```fathom
let Point = { x : S32, y : S32 };
let origin : Point = { x = 0, y = 0 };

origin.x
```

### Match expressions

Branching can be achieved though the use of match expressions.

Match expressions begin with a `match` keyword, followed by a head expression,
and a sequence of zero-or-more equations. The equations consist of a pattern and
a body expression. Any bindings introduced by the pattern are then bound as
parameters in the body expression.

An error is reported if there are any cases missing in the pattern match.

For example:

```fathom
match x {
    1 => { one : {} },
    2 => { two : {} },
    n => { any : Array32 n {} },
}
```

### Placeholders

Placeholders are introduced with an underscore.

Unification will attempt to fill placeholders with a term, producing an error
if the solution is ambiguous, or could not be found.

For example:

```fathom
fun (A : _) -> A -> A
//       ▲
//       └─── will be filled with `Type`
```

### Holes

Holes have the same syntax as [names](#names), but with a question mark (`?`) at the
beginning.

They behave like placeholders, but they report the contents of the hole if a
solution is found. This can be useful if if you want to figure out what to fill
an expression with.

For example:

```fathom
fun (A : ?param_type) -> A -> A
//           ▲
//           └─── will report: `?param_type = Type`
```

### Annotated terms

Terms can be annotated with explicit types using a colon (`:`). This can be used
to help type inference if an expression is ambiguous.

For example:

```fathom
let Thing = { x : S32 };

{ x = 1 } : Thing
```

### Grouped terms

Parentheses can be used to explicitly group terms.

For example:

```fathom
Array32 len (Repr point)
```

## Universes

Fathom has a single universe of types, `Type`:

- `Type : Type`

## Formats

Formats form descriptions that can be used to drive the serialisation and
deserialisation of binary data.

### Format Types

The type of formats is formed using the `Format` primitive:

- `Format : Type`

### Format representations

Every binary format has a unique host representation, which is accessed via the
built-in `Repr` operator:

- `Repr : Format -> Type`

### Record formats

Record formats are sequences of formats that are parsed one after the other.
For example, a point format could be defined as:

```fathom
{
    x <- u32be,
    y <- u32be,
}
```

Subsequent fields of a record format can depend on the values parsed from
previous fields. This can be useful for defining more complicated,
data-dependent formats. For example:

```fathom
{
    len <- u32be,
    data <- array32 len { x <- u32be, y <- u32be },
    //               ▲
    //               └─── type of `len` is `Repr u32be`
}
```

Empty record formats must be checked in the presence of an annotation in order
to disambiguate them from unit record types and literals. For example:

```fathom
{} : Format
```

```fathom
let unit : Format =
    {};

...
```

No annotations needed in the following example, as the type can be inferred from
the type of `array8`:

```fathom
array8 3 {}
```

#### Representation of record formats

The [representation](#format-representations) of a record format is a [dependent
record type](#records), with the `Repr` operation applied to each of the field's
formats, preserving dependencies as required.

Some examples are as follows:

| format                                     | `Repr` format                          |
| ------------------------------------------ | -------------------------------------- |
| `{}`                                       | `{}`                                   |
| `{ x <- f32le, y <- f32le }`               | `{ x : F32, y : F32 }`                 |
| `{ len <- u16be, data <- array16 len s8 }` | `{ len : U16, data : Array16 len S8 }` |

### Overlap formats

Overlap formats are very similar to [record formats](#record-formats), only each
field is parsed over the same space in memory. This allows for formats to be
enriched with information that occurs later on in the stream:

```fathom
overlap {
  records0 : array16 len array_record0,
  records1 : array16 len (array_record0 records0),
}
```

#### Representation of overlap formats

Overlap formats are [represented](#format-representations) as [dependent record
types](#records) that preserve dependencies between the fields present in the
original format.

### Number formats

There are formats for unsigned integer, signed integer, and floating point
number types. Multi-byte numbers come in big endian and little endian flavours:

- `u8 : Format`
- `u16be : Format`
- `u16le : Format`
- `u32be : Format`
- `u32le : Format`
- `u64be : Format`
- `u64le : Format`
- `s8 : Format`
- `s16be : Format`
- `s16le : Format`
- `s32be : Format`
- `s32le : Format`
- `s64be : Format`
- `s64le : Format`
- `f32be : Format`
- `f32le : Format`
- `f64be : Format`
- `f64le : Format`

#### Representation of number formats

Number formats lose their endianness as they are interpreted as their
corresponding host representation:

| format            | `Repr` format |
| ----------------- | ------------- |
| `u8`              | `U8`          |
| `u16be`, `u16le`  | `U16`         |
| `u32be`, `u32le`  | `U32`         |
| `u64be`, `u64le`  | `U64`         |
| `s8`              | `S8`          |
| `s16be`, `s16le`  | `S16`         |
| `s32be`, `s32le`  | `S32`         |
| `s64be`, `s64le`  | `S64`         |
| `f32be`, `f32le`  | `F32`         |
| `f64be`, `f64le`  | `F64`         |

### Array formats

There are four array formats, corresponding to the four [array types](#arrays):

- `array8 : U8 -> Format -> Format`
- `array16 : U16 -> Format -> Format`
- `array32 : U32 -> Format -> Format`
- `array64 : U64 -> Format -> Format`

#### Representation of array formats

The [representation](#format-representations) of the array formats preserve the
lengths, and use the representation of the element formats as the element types
of the host array types.

| format                 | `Repr` format                       |
| ---------------------- | ----------------------------------- |
| `array8 len format`    | `Array8 len (Repr format)`          |
| `array16 len format`   | `Array16 len (Repr format)`         |
| `array32 len format`   | `Array32 len (Repr format)`         |
| `array64 len format`   | `Array64 len (Repr format)`         |

### Link formats

Link formats allow for references to other parts of a binary stream to be
registered during parsing. They take a base [position](#positions), an offset
from that position, and a format to expect at that position.

There is a different link type for each unsigned integer offset:

- `link8 : Pos -> U8 -> Format -> Format`
- `link16 : Pos -> U16 -> Format -> Format`
- `link32 : Pos -> U32 -> Format -> Format`
- `link64 : Pos -> U64 -> Format -> Format`

#### Representation of link formats

Links formats are [represented](#format-representations) as typed
[references](#references) to other parts of the binary stream.

| format                       | `Repr` format               |
| ---------------------------- | --------------------------- |
| `link8 pos offset format`    | `Ref (Repr format)`         |
| `link16 pos offset format`   | `Ref (Repr format)`         |
| `link32 pos offset format`   | `Ref (Repr format)`         |
| `link64 pos offset format`   | `Ref (Repr format)`         |

### Stream position formats

The stream position format is interpreted as the current stream position during
parsing:

- `stream_pos : Format`

#### Representation of stream position formats

| format       | `Repr` format |
| ------------ | ------------- |
| `stream_pos` | `Pos`         |

### Succeed format

The succeed format allows values to be embedded in the resulting parsed output.

- `succeed : fun (A : Type) -> A -> Format`

#### Representation of succeed format

| format        | `Repr` format |
| ------------- | ------------- |
| `succeed A a` | `A`           |

### Fail format

The fail format always results in a parse failure if it is encountered during
parsing.

- `fail : Format`

#### Representation of fail format

The fail format should never produce a term, so is represented with [void](#void).

| format | `Repr` format |
| ------ | ------------- |
| `fail` | `Void`        |

## Functions

Functions enable terms to be abstracted with parameters. As Fathom is a
dependently typed programming language, functions also play the role of
‘generics’, allowing types and terms to be parameterised by other types or
terms.

### Function types

Simple, non-dependent function types are formed using the arrow operator:

```fathom
A -> B
```

Dependent functions (where the output type depends on the applied argument) are
formed using the `fun` keyword, followed by a parameter pattern, then an arrow
`->` and finally a body type:

```fathom
fun (a : A) -> B a
```

For example, the type of the identity function is:

```fathom
fun (A : Type) -> A -> A
```

### Function literals

Function literals are constructed using the `fun` keyword, followed by a
parameter pattern, then a fat arrow `=>` and a body expression:

```fathom
fun a => a : Type -> Type
```

### Function applications

Functions are applied using juxtaposition:

```fathom
let id : fun (A : Type) -> A -> A =
    fun _ a => a;

id Type S32
```

The above could also be defined as:

```fathom
let id =
    fun (A : Type) => fun (a : A) => a;

id Type S32
```

## Records

Records are types formed out of a combination of other types, each type
associated with a corresponding parameter.

### Record types

Record types are formed as sequences of fields:

```fathom
{ x : F32, y : F32 }
```

The types of later fields and depend on previous fields:

```fathom
{
    len : U32,
    data : Array32 len S32,
    //        ▲
    //        └─── error: expected `Array 3 S32`, found `Array 2 S32`
}
```

### Record literals

Records literals consist of a sequence of field assignments. For example:

```fathom
let Point = { x : U32, y : U32 };

let origin : Point = {
    x = 0,
    y = 0,
};
```

When checking dependent record types, the values assigned to field expressions
will substituted into the types of subsequent fields. For example:

```fathom
let Data = {
    len : U32,
    data : Array32 len S32,
    //              ▲
    //              └─── this type depends on the value of the `len` field
};

{
    len = 3,
    data = [1, 2],
    //        ▲
    //        └─── error: expected `Array 3 S32`, found `Array 2 S32`
} : Data
```

### Record projections

The fields in a record can be accessed using the dot (`.`) operator,
followed by the desired field label. For example:

```fathom
let Color : Type = { r : U8, g : U8, b : U8 };
let yellow : Color = { r = 255, g = 255, b = 0 };

yellow.g
```

Record projections preserve data dependencies. For example:

```fathom
let Data = {
    len : U32,
    data : Array32 len S32,
};

let some-data : Data = {
    len = 3,
    data = [1, 2, 3];
};

some-data.data : Array3 some-data.len S32
//                                 ▲
//                                 └─── dependency on `len` is preserved
//                                      in type of the projected term
```

## Numbers

### Number types

- Unsigned integer types: `U8`, `U16`, `U32`, `U64`
- Signed integer types: `S8`, `S16`, `S32`, `S64`
- Floating point numbers: `F32`, `F64`

### Number literals

- `1 : U8`
- `42 : S32`
- `-42 : S32`

### String literals

- `"GSUB" : U16`

### Number operations

A number of operations are defined for the numeric types:

- `u8_add : U8 -> U8 -> U8`
- `u8_sub : U8 -> U8 -> U8`
- `u8_mul : U8 -> U8 -> U8`
- `u8_div : U8 -> U8 -> U8`
- `u8_not : U8 -> U8`
- `u8_shl : U8 -> U8 -> U8`
- `u8_shr : U8 -> U8 -> U8`
- `u8_and : U8 -> U8 -> U8`
- `u8_or : U8 -> U8 -> U8`
- `u8_xor : U8 -> U8 -> U8`
- `u16_add : U16 -> U16 -> U16`
- `u16_sub : U16 -> U16 -> U16`
- `u16_mul : U16 -> U16 -> U16`
- `u16_div : U16 -> U16 -> U16`
- `u16_not : U16 -> U16`
- `u16_shl : U16 -> U16 -> U16`
- `u16_shr : U16 -> U16 -> U16`
- `u16_and : U16 -> U16 -> U16`
- `u16_or : U16 -> U16 -> U16`
- `u16_xor : U16 -> U16 -> U16`
- `u32_add : U32 -> U32 -> U32`
- `u32_sub : U32 -> U32 -> U32`
- `u32_mul : U32 -> U32 -> U32`
- `u32_div : U32 -> U32 -> U32`
- `u32_not : U32 -> U32`
- `u32_shl : U32 -> U32 -> U32`
- `u32_shr : U32 -> U32 -> U32`
- `u32_and : U32 -> U32 -> U32`
- `u32_or : U32 -> U32 -> U32`
- `u32_xor : U32 -> U32 -> U32`
- `u64_add : U64 -> U64 -> U64`
- `u64_sub : U64 -> U64 -> U64`
- `u64_mul : U64 -> U64 -> U64`
- `u64_div : U64 -> U64 -> U64`
- `u64_not : U64 -> U64`
- `u64_shl : U64 -> U64 -> U64`
- `u64_shr : U64 -> U64 -> U64`
- `u64_and : U64 -> U64 -> U64`
- `u64_or : U64 -> U64 -> U64`
- `u64_xor : U64 -> U64 -> U64`
- `s8_neg : S8 -> S8`
- `s8_add : S8 -> S8 -> S8`
- `s8_sub : S8 -> S8 -> S8`
- `s8_mul : S8 -> S8 -> S8`
- `s8_div : S8 -> S8 -> S8`
- `s16_neg : S16 -> S16`
- `s16_add : S16 -> S16 -> S16`
- `s16_sub : S16 -> S16 -> S16`
- `s16_mul : S16 -> S16 -> S16`
- `s16_div : S16 -> S16 -> S16`
- `s32_neg : S32 -> S32`
- `s32_add : S32 -> S32 -> S32`
- `s32_sub : S32 -> S32 -> S32`
- `s32_mul : S32 -> S32 -> S32`
- `s32_div : S32 -> S32 -> S32`
- `s64_neg : S64 -> S64`
- `s64_add : S64 -> S64 -> S64`
- `s64_sub : S64 -> S64 -> S64`
- `s64_mul : S64 -> S64 -> S64`
- `s64_div : S64 -> S64 -> S64`

## Arrays

Arrays are sequences of elements up to a given length.

### Array types

Array types are formed with the following primitives:

- `Array8 : U8 -> Type -> Type`
- `Array16 : U16 -> Type -> Type`
- `Array32 : U32 -> Type -> Type`
- `Array64 : U64 -> Type -> Type`

For example, this is the type of an array of three signed 32-bit integers:

```fathom
Array8 3 S32
```

### Array literals

Arrays can be constructed as sequences of terms within square
brackets. For example:

```fathom
[] : Array32 0 S32
[3, 32, -6] : Array8 3 S32
```

## Positions

Stream positions are represented as an abstract datatype:

- `Pos : Type`

Positions are usually encountered as a result of parsing a [stream position
format](#stream-position-formats).

## References

References are like [stream positions](#positions), only they also have an
expected type given as well:

- `Ref : Type -> Type`

References are usually encountered as a result of parsing a [link
format](#link-formats).

## Void

The void type is be used to mark terms that must never be constructed:

- `Void : Type`
