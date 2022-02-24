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
  - [Record formats](#record-formats)
  - [Number formats](#number-formats)
  - [Array formats](#array-formats)
  - [Stream position formats](#stream-position-formats)
  - [Succeed format](#succeed-format)
  - [Fail format](#fail-format)
  - [Format representations](#format-representations)
- [Patterns](#patterns)
  - [Name patterns](#name-patterns)
  - [Placeholder patterns](#placeholder-patterns)
  - [Number literal patterns](#number-literal-patterns)
  - [String literal patterns](#string-literal-patterns)
  - [Annotated patterns](#annotated-patterns)
  - [Grouped patterns](#grouped-patterns)
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
- [Arrays](#arrays)
  - [Array types](#array-types)
  - [Array literals](#array-literals)
- [Positions](#positions)
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
- [sequence formats](#sequence-formats)
- [record types](#record-types)

Depending on the binder, these variables might either be bound as definitions or
as parameters. If a variable is bound as a definition it reduces to a
definition during evaluation.

If no binding is found, names can refer to one of the built-in primitives:

- `Format`
- `u8`, `u16be`, `u16le`, `u32be`, `u32le`, `u64be`, `u64le`
- `s8`, `s16be`, `s16le`, `s32be`, `s32le`, `s64be`, `s64le`
- `f32be`, `f32le`, `f64be`, `f64le`
- `array8`, `array16`, `array32`, `array64`
- `stream_pos`
- `succeed`, `fail`
- `Repr`
- `U8`, `U16`, `U32`, `U64`, `S8`, `S16`, `S32`, `S64`, `F32`, `F64`
- `Array8`, `Array16`, `Array32`, `Array64`
- `Pos`
- `Void`

### Let expressions

Shared definitions can be described using let expressions.

A let expression is begun using the `let` keyword, followed by a
[pattern](#patterns) binding. Any bindings introduced by the pattern are then
bound as definitions in the _body term_ that follows.

For example:

```fathom
let Point = { x : S32, y : S32 };
let origin : Point = { x = 0, y = 0 };

origin.x
```

### Match expressions

Branching can be achieved though the use of match expressions.

Match expressions begin with a `match` keyword, followed by a head expression,
and a sequence of zero-or-more equations. The equations consist of a
[pattern](#patterns) and a body expression. Any bindings introduced by the
pattern are then bound as parameters in the body expression.

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

### Record formats

```fathom
{
    x <- u32be,
    y <- u32be,
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

No annotations needed, as the type can be inferred from the type of `array8`:

```fathom
array8 3 {}
```

Subsequent fields of a record format can depend on the information parsed data
from previous fields. This can be useful for defining more complex,
data-dependent formats. For example:

```fathom
{
    len <- u32be,
    data <- array32 len { x <- u32be, y <- u32be },
    //               ▲
    //               └─── type of `len` is `Repr u32be`
}
```

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

### Array formats

There are four array formats, corresponding to the four array types:

- `array8 : U8 -> Format -> Format`
- `array16 : U16 -> Format -> Format`
- `array32 : U32 -> Format -> Format`
- `array64 : U64 -> Format -> Format`

### Stream position formats

The stream position format is interpreted as the current stream position during
parsing:

- `stream_pos : Format`

### Succeed format

The succeed format allows values to be embedded in the resulting parsed output.

- `succeed : fun (A : Type) -> A -> Format`

### Fail format

The fail format always results in a parse failure if it is encountered during
parsing.

- `fail : Format`

### Format representations

Every binary format has a unique host representation, which is accessed via the
built-in `Repr` operator:

- `Repr : Format -> Type`

#### Representations of record formats

The representation of a record format is a dependent record type, with the
`Repr` operation applied to each of the field's formats, preserving dependencies
as required.

Some examples are as follows:

| format                                     | `Repr` format                          |
| ------------------------------------------ | -------------------------------------- |
| `{}`                                       | `{}`                                   |
| `{ x <- f32le, y <- f32le }`               | `{ x : F32, y : F32 }`                 |
| `{ len <- u16be, data <- array16 len s8 }` | `{ len : U16, data : Array16 len S8 }` |

#### Representations of number formats

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

#### Representations of array formats

The representation of the array formats preserve the lengths, and use the
representation of the element formats as the element types of the host array
types.

| format                 | `Repr` format                       |
| ---------------------- | ----------------------------------- |
| `array8 len format`    | `Array8 len (Repr format)`          |
| `array16 len format`   | `Array16 len (Repr format)`         |
| `array32 len format`   | `Array32 len (Repr format)`         |
| `array64 len format`   | `Array64 len (Repr format)`         |

#### Representation of stream position formats

| format       | `Repr` format |
| ------------ | ------------- |
| `stream_pos` | `Pos`         |

#### Representation of succeed format

| format        | `Repr` format |
| ------------- | ------------- |
| `succeed A a` | `A`           |

#### Representation of fail format

| format | `Repr` format |
| ------ | ------------- |
| `fail` | `Void`        |

## Patterns

### Name patterns

> **TODO**: document name patterns

### Placeholder patterns

> **TODO**: document placeholder patterns

### Number literal patterns

> **TODO**: number literal patterns

### String literal patterns

> **TODO**: document string literal patterns

### Annotated patterns

> **TODO**: annotated patterns

### Grouped patterns

Parentheses can be used to group patterns.

## Functions

### Function types

> **TODO**: document non-dependent function types\
> **TODO**: document dependent function types

### Function literals

> **TODO**: document record literals

### Function applications

> **TODO**: document function applications

## Records

### Record types

> **TODO**: document non-dependent record types\
> **TODO**: document dependent record types

### Record literals

> **TODO**: document record literals

### Record projections

> **TODO**: document record projections

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

## Void

The void type is be used to mark terms that must never be constructed:

- `Void : Type`
