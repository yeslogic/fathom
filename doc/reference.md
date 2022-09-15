# Language Reference

A reference-level documentation for the Fathom data description language.

This is intended as a user-facing language reference, _not_ a language
specification. A more precise specification of the lexical and abstract syntax,
elaboration, and core language is forthcoming.

## Contents

- [Modules](#items)
  - [Definitions](#definitions)
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
  - [Conditional formats](#conditional-formats)
  - [Overlap formats](#overlap-formats)
  - [Number formats](#number-formats)
  - [Array formats](#array-formats)
  - [Repeat formats](#repeat-formats)
  - [Limit formats](#limit-formats)
  - [Stream position formats](#stream-position-formats)
  - [Link formats](#link-formats)
  - [Deref formats](#deref-formats)
  - [Succeed format](#succeed-format)
  - [Fail format](#fail-format)
  - [Unwrap format](#unwrap-format)
- [Functions](#functions)
  - [Function types](#function-types)
  - [Function literals](#function-literals)
  - [Function applications](#function-applications)
- [Records](#records)
  - [Record types](#record-types)
  - [Record literals](#record-literals)
  - [Record projections](#record-projections)
- [Booleans](#booleans)
  - [Boolean operations](#boolean-operations)
- [Numbers](#numbers)
  - [Number types](#number-types)
  - [Number literals](#array-literals)
  - [String literals](#string-literals)
  - [Number operations](#number-operations)
- [Options](#options)
  - [Option operations](#option-operations)
- [Arrays](#arrays)
  - [Array types](#array-types)
  - [Array literals](#array-literals)
  - [Array operations](#array-operations)
- [Positions](#positions)
  - [Position types](#position-types)
  - [Position operations](#position-operations)
- [References](#references)
- [Void](#void)

## Modules

Fathom modules are made up of multiple top-level items

### Definitions

Top-level definitions are preceded by the `def` keyword:

```fathom
def point = {
  x <- u32be,
  y <- u32be,
};
```

Definitions can optionally have a type annotation given to them:

```fathom
def point : Format = {
  x <- u32be,
  y <- u32be,
};
```

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
- `Bool`, `U8`, `U16`, `U32`, `U64`, `S8`, `S16`, `S32`, `S64`, `F32`, `F64`
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
and a sequence of zero-or-more equations. The equations consist of a pattern
and a body expression separated by `=>`. Any bindings introduced by the pattern
are then bound as parameters in the body expression.

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

#### Empty record formats

Empty record formats must be checked in the presence of an annotation in order
to disambiguate them from unit record types and literals. For example:

```fathom
{} : Format
```

```fathom
let unit : Format =
    {};
⋮
```

No annotations needed in the following example, as the type can be inferred from
the type of `array8`:

```fathom
array8 3 {}
```

#### Field refinements

The parsed representations of fields can be refined with boolean predicates
using the `where` syntax. If the predicate evaluates to `false`, the rest of the
record format will fail to parse. For example:

```fathom
{
    magic <- u32be where u32_eq magic "icns",
    //                   ▲      ▲
    //                   │      └──── `magic` is bound as type `Repr u32be`
    //                   │
    //                   └──── `u32_eq magic "icns"` must be of type `Bool`
}
```

This can be thought of as a shorthand form of [conditional formats](#conditional-format),
allowing the field label to be reused as the name bound by the conditional
format. For example, the above format is equivalent to:

```fathom
{
    magic <- { magic <- u32be | u32_eq magic "icns" },
}
```

#### Computed fields

Sometimes it is useful to embed a pure computation (that does not perform any
parsing) in the middle of a record format, to be used in subsequent parts of the
record format. Fathom supports a `let` syntax for format fields to make this
more convenient:

```fathom
{
    /// 2 × seg_count.
    seg_count_x2 <- u16be,

    /// Number of contiguous ranges of character codes
    let seg_count = u16_div seg_count_x2 2,
    //  ▲
    //  └─── the value computed for `seg_count` will be
    //       available for use in subsequent fields

    ⋮

    start_code <- array16 seg_count u16be,
    id_delta <- array16 seg_count s16be,
    //                  ▲
    //                  └──── `seg_count` is used here
}
```

Computed fields can thought of as a shorthand form of [succeed formats](#succeed-format).
For example, the beginning of above format is equivalent to:

```fathom
{
    seg_count_x2 <- u16be,
    seg_count <- succeed U16 (u16_div seg_count_x2 2),

    ⋮
}
```

Type annotations are also supported on computed fields:

```fathom
{
    let x : U32 = 3,
}
```

#### Representation of record formats

The [representation](#format-representations) of a record format is a [dependent
record type](#records), with the `Repr` operation applied to each of the field's
formats, preserving dependencies as required:

| format                                   | `Repr` format                          |
| ---------------------------------------- | -------------------------------------- |
| `{}`                                     | `{}`                                   |
| `{ ..., l <- format, ... }`              | `{ ..., l : Repr format, ... }`        |
| `{ ..., l <- format where pred, ... }`   | `{ ..., l : Repr format, ... }`        |
| `{ ..., let l = a : A, ... }`            | `{ ..., l : A, ... }`                  |

Computed fields are included as fields in the representation of record formats
because they might appear in the representation types of subsequent fields.

Some some examples of record formats and their representations are:

| format                                            | `Repr` format                          |
| ------------------------------------------------- | -------------------------------------- |
| `{ x <- f32le, y <- f32le }`                      | `{ x : F32, y : F32 }`                 |
| `{ len <- u16be, data <- array16 len s8 }`        | `{ len : U16, data : Array16 len S8 }` |
| `{ magic <- u32be where u32_eq magic "icns" }`    | `{ magic : U32 }`                      |
| `{ let len = 4 : U32, data <- array32 len s8 }`   | `{ len : U32, data : Array32 len S8 }` |

### Conditional formats

Conditional formats are formats that that have their parsed representations
refined with boolean predicates. The format will only succeed if the predicate
evaluates to `true`.

```fathom
{ x <- format | pred x }
//              ▲    ▲
//              │    └─── `x` is bound as type `Repr format` in the predicate
//              │
//              └──── `pred x` must be of type `Bool`
```

This can be used to verify magic numbers or version expectations. For example:

```fathom
{ magic <- u64le | u64_eq magic 0x00ffffffffffff00 }
```

```fathom
{ version <- u16be | u16_lte version 2 }
```

#### Representation of conditional formats

The [representation](#format-representations) of a conditional format is the
same as the representation of the refined format. I.e.

| format                    | `Repr` format        |
| ------------------------- | -------------------- |
| `{ x <- format \| cond }` | `Repr format`        |

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

Overlap formats also support [field refinements](#field-refinements) and
[computed fields](#computed-fields), like in record formats.

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
of the host [array types](#array-types).

| format                 | `Repr` format                       |
| ---------------------- | ----------------------------------- |
| `array8 len format`    | `Array8 len (Repr format)`          |
| `array16 len format`   | `Array16 len (Repr format)`         |
| `array32 len format`   | `Array32 len (Repr format)`         |
| `array64 len format`   | `Array64 len (Repr format)`         |

### Repeat formats

The `repeat_until_end` format repeats parsing the given format until the end of
the current binary stream is reached:

- `repeat_until_end : Format -> Format`

#### Representation of repeat formats

Because the repeat format does not have a predefined length, it is
[represented](#format-representations) as a dynamically sized
[array type](#array-types):

| format                    | `Repr` format         |
| ------------------------- | --------------------- |
| `repeat_until_end format` | `Array (Repr format)` |

### Limit formats

Limit formats parse a format within a limited sub-stream of the binary data. The
stream will start from the [current stream position](#stream-position-formats)
and continue up to the given number of bytes.

- `limit8 : U8 -> Format -> Format`
- `limit16 : U16 -> Format -> Format`
- `limit32 : U32 -> Format -> Format`
- `limit64 : U64 -> Format -> Format`

#### Representation of limit formats

| format                    | `Repr` format       |
| ------------------------- | ------------------- |
| `limit8 length format`    | `Repr format`       |
| `limit16 length format`   | `Repr format`       |
| `limit32 length format`   | `Repr format`       |
| `limit64 length format`   | `Repr format`       |

### Stream position formats

The stream position format is interpreted as the current stream position during
parsing:

- `stream_pos : Format`

#### Representation of stream position formats

| format       | `Repr` format |
| ------------ | ------------- |
| `stream_pos` | `Pos`         |

### Link formats

Link formats allow for references to other parts of a binary stream to be
registered during parsing. They take a base [position](#positions) and a format
to expect at that position:

- `link : Pos -> Format -> Format`

#### Representation of link formats

Links formats are [represented](#format-representations) as typed
[references](#references) to other parts of the binary stream.

| format            | `Repr` format |
| ----------------- | ------------- |
| `link pos format` | `Ref format`  |

### Deref formats

Deref formats allow [references](#references) to other parts of the stream to be
included in resulting parsed output.

- `deref : fun (f : Format) -> Ref f -> Format`

#### Representation of deref formats

Dereferences are [represented](#format-representations) after parsing using the
representation of the referenced format.

| format             | `Repr` format |
| ------------------ | ------------- |
| `deref format ref` | `Repr format` |

### Succeed format

The succeed format consumes no input during parsing, allowing values to be
embedded in the resulting parsed output.

- `succeed : fun (A : Type) -> A -> Format`

#### Representation of succeed formats

| format        | `Repr` format |
| ------------- | ------------- |
| `succeed A a` | `A`           |

### Fail format

The fail format always results in a parse failure if it is encountered during
parsing.

- `fail : Format`

#### Representation of fail formats

The fail format should never produce a term, so is represented with [void](#void).

| format | `Repr` format |
| ------ | ------------- |
| `fail` | `Void`        |

### Unwrap format

The unwrap format consumes no input during parsing, succeeding with the data
contained in a the `some` case of an [option](#options), or otherwise causing a
parse failure.

- `unwrap : fun (A : Type) -> Option A -> Format`

#### Representation of unwrap formats

| format              | `Repr` format |
| ------------------- | ------------- |
| `unwrap A option_a` | `A`           |

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
    fun (A : Type) (a : A) => a;

id Type S32
```

## Records

Records are types formed out of a combination of other types, each type
associated with a corresponding parameter.

### Record types

Record types are formed as sequences of field declarations:

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

Records literals consist of a sequence of field definitions. For example:

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

## Booleans

The boolean type is: `Bool` with values `true` and `false`.

### Boolean operations

The following operations are defined for booleans:

- `bool_eq : Bool -> Bool -> Bool`
- `bool_neq : Bool -> Bool -> Bool`
- `bool_not : Bool -> Bool`
- `bool_and : Bool -> Bool -> Bool`
- `bool_or : Bool -> Bool -> Bool`
- `bool_xor : Bool -> Bool -> Bool`

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

A number of operations are defined for the numeric types. Some also have
infix operators as noted.

#### U8

| Operation                   | Operator |
|-----------------------------|:--------:|
| `u8_eq : U8 -> U8 -> Bool`  |   `==`   |
| `u8_neq : U8 -> U8 -> Bool` |   `!=`   |
| `u8_gt : U8 -> U8 -> Bool`  |   `>`    |
| `u8_lt : U8 -> U8 -> Bool`  |   `<`    |
| `u8_gte : U8 -> U8 -> Bool` |   `>=`   |
| `u8_lte : U8 -> U8 -> Bool` |   `<=`   |
| `u8_add : U8 -> U8 -> U8`   |   `+`    |
| `u8_sub : U8 -> U8 -> U8`   |   `-`    |
| `u8_mul : U8 -> U8 -> U8`   |   `*`    |
| `u8_div : U8 -> U8 -> U8`   |   `/`    |
| `u8_not : U8 -> U8`         |          |
| `u8_shl : U8 -> U8 -> U8`   |          |
| `u8_shr : U8 -> U8 -> U8`   |          |
| `u8_and : U8 -> U8 -> U8`   |          |
| `u8_or : U8 -> U8 -> U8`    |          |
| `u8_xor : U8 -> U8 -> U8`   |          |

#### U16

| Operation                      | Operator |
|--------------------------------|:--------:|
| `u16_eq : U16 -> U16 -> Bool`  |   `==`   |
| `u16_neq : U16 -> U16 -> Bool` |   `!=`   |
| `u16_gt : U16 -> U16 -> Bool`  |   `>`    |
| `u16_lt : U16 -> U16 -> Bool`  |   `<`    |
| `u16_gte : U16 -> U16 -> Bool` |   `>=`   |
| `u16_lte : U16 -> U16 -> Bool` |   `<=`   |
| `u16_add : U16 -> U16 -> U16`  |   `+`    |
| `u16_sub : U16 -> U16 -> U16`  |   `-`    |
| `u16_mul : U16 -> U16 -> U16`  |   `*`    |
| `u16_div : U16 -> U16 -> U16`  |   `/`    |
| `u16_not : U16 -> U16`         |          |
| `u16_shl : U16 -> U8 -> U16`   |          |
| `u16_shr : U16 -> U8 -> U16`   |          |
| `u16_and : U16 -> U16 -> U16`  |          |
| `u16_or : U16 -> U16 -> U16`   |          |
| `u16_xor : U16 -> U16 -> U16`  |          |

#### U32

| Operation                      | Operator |
|--------------------------------|:--------:|
| `u32_eq : U32 -> U32 -> Bool`  |   `==`   |
| `u32_neq : U32 -> U32 -> Bool` |   `!=`   |
| `u32_gt : U32 -> U32 -> Bool`  |   `>`    |
| `u32_lt : U32 -> U32 -> Bool`  |   `<`    |
| `u32_gte : U32 -> U32 -> Bool` |   `>=`   |
| `u32_lte : U32 -> U32 -> Bool` |   `<=`   |
| `u32_add : U32 -> U32 -> U32`  |   `+`    |
| `u32_sub : U32 -> U32 -> U32`  |   `-`    |
| `u32_mul : U32 -> U32 -> U32`  |   `*`    |
| `u32_div : U32 -> U32 -> U32`  |   `/`    |
| `u32_not : U32 -> U32`         |          |
| `u32_shl : U32 -> U8 -> U32`   |          |
| `u32_shr : U32 -> U8 -> U32`   |          |
| `u32_and : U32 -> U32 -> U32`  |          |
| `u32_or : U32 -> U32 -> U32`   |          |
| `u32_xor : U32 -> U32 -> U32`  |          |

#### U64

| Operation                      | Operator |
|--------------------------------|:--------:|
| `u64_eq : U64 -> U64 -> Bool`  |   `==`   |
| `u64_neq : U64 -> U64 -> Bool` |   `!=`   |
| `u64_gt : U64 -> U64 -> Bool`  |   `>`    |
| `u64_lt : U64 -> U64 -> Bool`  |   `<`    |
| `u64_gte : U64 -> U64 -> Bool` |   `>=`   |
| `u64_lte : U64 -> U64 -> Bool` |   `<=`   |
| `u64_add : U64 -> U64 -> U64`  |   `+`    |
| `u64_sub : U64 -> U64 -> U64`  |   `-`    |
| `u64_mul : U64 -> U64 -> U64`  |   `*`    |
| `u64_div : U64 -> U64 -> U64`  |   `/`    |
| `u64_not : U64 -> U64`         |          |
| `u64_shl : U64 -> U8 -> U64`   |          |
| `u64_shr : U64 -> U8 -> U64`   |          |
| `u64_and : U64 -> U64 -> U64`  |          |
| `u64_or : U64 -> U64 -> U64`   |          |
| `u64_xor : U64 -> U64 -> U64`  |          |

#### S8

| Operation                    | Operator |
|------------------------------|:--------:|
| `s8_eq : S8 -> S8 -> Bool`   |   `==`   |
| `s8_neq : S8 -> S8 -> Bool`  |   `!=`   |
| `s8_gt : S8 -> S8 -> Bool`   |   `>`    |
| `s8_lt : S8 -> S8 -> Bool`   |   `<`    |
| `s8_gte : S8 -> S8 -> Bool`  |   `>=`   |
| `s8_lte : S8 -> S8 -> Bool`  |   `<=`   |
| `s8_neg : S8 -> S8`          |          |
| `s8_add : S8 -> S8 -> S8`    |   `+`    |
| `s8_sub : S8 -> S8 -> S8`    |   `-`    |
| `s8_mul : S8 -> S8 -> S8`    |   `*`    |
| `s8_div : S8 -> S8 -> S8`    |   `/`    |
| `s8_abs : S8 -> S8`          |          |
| `s8_unsigned_abs : S8 -> U8` |          |

#### S16

| Operation                       | Operator |
|---------------------------------|:--------:|
| `s16_eq : S16 -> S16 -> Bool`   |   `==`   |
| `s16_neq : S16 -> S16 -> Bool`  |   `!=`   |
| `s16_gt : S16 -> S16 -> Bool`   |   `>`    |
| `s16_lt : S16 -> S16 -> Bool`   |   `<`    |
| `s16_gte : S16 -> S16 -> Bool`  |   `>=`   |
| `s16_lte : S16 -> S16 -> Bool`  |   `<=`   |
| `s16_neg : S16 -> S16`          |          |
| `s16_add : S16 -> S16 -> S16`   |   `+`    |
| `s16_sub : S16 -> S16 -> S16`   |   `-`    |
| `s16_mul : S16 -> S16 -> S16`   |   `*`    |
| `s16_div : S16 -> S16 -> S16`   |   `/`    |
| `s16_abs : S16 -> S16`          |          |
| `s16_unsigned_abs : S16 -> U16` |          |

#### S32

| Operation                       | Operator |
|---------------------------------|:--------:|
| `s32_eq : S32 -> S32 -> Bool`   |   `==`   |
| `s32_neq : S32 -> S32 -> Bool`  |   `!=`   |
| `s32_gt : S32 -> S32 -> Bool`   |   `>`    |
| `s32_lt : S32 -> S32 -> Bool`   |   `<`    |
| `s32_gte : S32 -> S32 -> Bool`  |   `>=`   |
| `s32_lte : S32 -> S32 -> Bool`  |   `<=`   |
| `s32_neg : S32 -> S32`          |          |
| `s32_add : S32 -> S32 -> S32`   |   `+`    |
| `s32_sub : S32 -> S32 -> S32`   |   `-`    |
| `s32_mul : S32 -> S32 -> S32`   |   `*`    |
| `s32_div : S32 -> S32 -> S32`   |   `/`    |
| `s32_abs : S32 -> S32`          |          |
| `s32_unsigned_abs : S32 -> U32` |          |

#### S64

| Operation                       | Operator |
|---------------------------------|:--------:|
| `s64_eq : S64 -> S64 -> Bool`   |   `==`   |
| `s64_neq : S64 -> S64 -> Bool`  |   `!=`   |
| `s64_gt : S64 -> S64 -> Bool`   |   `>`    |
| `s64_lt : S64 -> S64 -> Bool`   |   `<`    |
| `s64_gte : S64 -> S64 -> Bool`  |   `>=`   |
| `s64_lte : S64 -> S64 -> Bool`  |   `<=`   |
| `s64_neg : S64 -> S64`          |          |
| `s64_add : S64 -> S64 -> S64`   |   `+`    |
| `s64_sub : S64 -> S64 -> S64`   |   `-`    |
| `s64_mul : S64 -> S64 -> S64`   |   `*`    |
| `s64_div : S64 -> S64 -> S64`   |   `/`    |
| `s64_abs : S64 -> S64`          |          |
| `s64_unsigned_abs : S64 -> U64` |          |

## Options

Data that may not be present can be formed with the following primitive:

- `Option : Type -> Type`

Optional data can be introduced with the `some` or `none` primitives:

- `some : fun (A : Type) -> A -> Option A`
- `none : fun (A : Type) -> Option A`

### Option operations

The following operations are defined for option types:

- `option_fold : fun (A : Type) (B : Type) -> B -> (A -> B) -> Option A -> B`

## Arrays

Arrays are sequences of elements up to a given length.

### Array types

Arrays with dynamic lengths are formed with the following primitive:

- `Array : Type -> Type`

Fixed-length array types are formed with the following primitives:

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
[1, 2, 3] : Array S16
[] : Array32 0 S32
[3, 32, -6] : Array8 3 S32
```

The number of terms in an array literal must match the length parameter of a
fixed-length array type.

### Array operations

The following operations are defined on arrays:

#### find

`array*_find` takes a function that returns true or false. It applies this
function to each element of the array. If the function returns true, then
`array*_find` returns `some element`. If they all return false, it returns
`none`.

- `array8_find : fun (len : U8) (A : Type) -> (A -> Bool) -> Array8 len A -> Option A`
- `array16_find : fun (len : U16) (A : Type) -> (A -> Bool) -> Array16 len A -> Option A`
- `array32_find : fun (len : U32) (A : Type) -> (A -> Bool) -> Array32 len A -> Option A`
- `array64_find : fun (len : U64) (A : Type) -> (A -> Bool) -> Array64 len A -> Option A`

#### index

`array*_index` returns the item at the supplied index in the array. The
operation will not evaluate fully if the index is out of bounds. If this
happens when parsing a binary format, a parse failure will be triggered.

- `array8_index : fun (len : U8) (A : Type) (index : U8) -> Array8 len A -> A`
- `array16_index : fun (len : U16) (A : Type) (index : U16) -> Array16 len A -> A`
- `array32_index : fun (len : U32) (A : Type) (index : U32) -> Array32 len A -> A`
- `array64_index : fun (len : U64) (A : Type) (index : U64) -> Array64 len A -> A`

## Positions

Position types represent locations in the binary stream, relative to the
beginning of the binary stream.

### Position types

Stream positions are formed with the primitive:

- `Pos : Type`

Positions are usually encountered as a result of parsing a [stream position
format](#stream-position-formats).

### Position operations

A number of operations are defined for positions:

- `pos_add_u8 : Pos -> U8 -> Pos`
- `pos_add_u16 : Pos -> U16 -> Pos`
- `pos_add_u32 : Pos -> U32 -> Pos`
- `pos_add_u64 : Pos -> U64 -> Pos`

## References

References to other parts of the binary file are described with:

- `Ref : Format -> Type`

References are usually encountered as a result of parsing a [link format](#link-formats),
and can be dereferenced later on with a [deref format](#deref-formats).

## Void

The void type is be used to mark terms that must never be constructed:

- `Void : Type`
