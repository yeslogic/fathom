# Binary format descriptions

Binary format descriptions allow us to describe binary formats in a way that is similar to datatype-generic programming.

Although appear reminiscent of types on a syntactic level,
they have no inhabitants and are better thought of as a builtin [inductive type].

[inductive type]: https://en.wikipedia.org/wiki/Inductive_type

## Formation

All binary format descriptions are elements of the primitive type `Format`.

`Format` is an element of the `Kind` sort:

```fathom
Format : Kind
```

## Representations of format descriptions

Each binary format description can be interpreted as a corresponding 'representation type'
that is intended to represent result of parsing a format description.
This can be accessed by using the built-in `repr : Format -> Type` function.
For example:

```fathom
repr U8             // evaluates to `Int`
repr I32Be          // evaluates to `Int`
repr { x : U32Be }  // evaluates to `{ x : Int } : Type`
```

## Introduction

### Unsigned integer formats

| Name    | Representation | Description                              |
| ------- | -------------- | ---------------------------------------- |
| `U8`    | `Int`          | 8-bit unsigned integers                  |
| `U16Le` | `Int`          | 16-bit unsigned integers (little endian) |
| `U16Be` | `Int`          | 16-bit unsigned integers (big endian)    |
| `U32Le` | `Int`          | 32-bit unsigned integers (little endian) |
| `U32Be` | `Int`          | 32-bit unsigned integers (big endian)    |
| `U64Le` | `Int`          | 64-bit unsigned integers (little endian) |
| `U64Be` | `Int`          | 64-bit unsigned integers (big endian)    |

> **TODO**: use refinement types for integer representations

### Signed integer formats

These are encoded using a [two's complement][twos-complement-wikipedia]
representation.

| Name    | Representation | Description                            |
| ------- | -------------- | -------------------------------------- |
| `S8`    | `Int`          | 8-bit signed integers                  |
| `S16Le` | `Int`          | 16-bit signed integers (little endian) |
| `S16Be` | `Int`          | 16-bit signed integers (big endian)    |
| `S32Le` | `Int`          | 32-bit signed integers (little endian) |
| `S32Be` | `Int`          | 32-bit signed integers (big endian)    |
| `S64Le` | `Int`          | 64-bit signed integers (little endian) |
| `S64Be` | `Int`          | 64-bit signed integers (big endian)    |

> **TODO**: use refinement types for integer representations

[twos-complement-wikipedia]: https://en.wikipedia.org/wiki/Two%27s_complement

### Floating point formats

These are encoded following the [IEEE Standard for Floating-Point Arithmetic
(IEEE 754)][ieee-754-wikipedia].

| Name    | Representation | Description                             |
| ------- | -------------- | --------------------------------------- |
| `F32Le` | `F32`          | single-precision floats (little endian) |
| `F32Be` | `F32`          | single-precision floats (big endian)    |
| `F64Le` | `F64`          | double-precision floats (little endian) |
| `F64Be` | `F64`          | double-precision floats (big endian)    |

[ieee-754-wikipedia]: https://en.wikipedia.org/wiki/IEEE_754

### Character formats

> **TODO**: add documentation

### String formats

> **TODO**: add documentation

### Array formats

An fixed-length array of a single format can be described using the `FormatArray` format:

```fathom
FormatArray : Int -> Format -> Format
```

Representation, assuming `len : Int` and `format : Format`:

```fathom
repr (FormatArray len format) == Array len (repr format)
```

### Position formats

The current position of the binary stream can be accessed using the `CurrentPos` format:

```fathom
CurrentPos : Format
```

Representation:

```fathom
repr CurrentPos == Pos
```

### Offset formats

> **TODO**: add documentation

### Record formats

> **TODO**: add documentation

### Enumeration formats

> **TODO**: add documentation
