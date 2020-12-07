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
repr U8             // normalizes to `Int`
repr I32Be          // normalizes to `Int`
repr { x : U32Be }  // normalizes to `{ x : Int } : Type`
```

The `repr` function will rarely appear in user-code,
but it may occasionally pop up in type errors when a `format` is unknown.
For example:

```fathom
struct Things (format : Format): Format = {
    thing : format,
    // `thing : repr format` is now in the typing context
    another_thing : FormatArray (thing.len) U32Be, // error: `len` is not a field of `repr format`
};
```

In the above example `format : Format` is unknown, and therefore its representation is also unknown.

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

A fixed-length array of a single format can be described using the `FormatArray` format:

```fathom
FormatArray : Int -> Format -> Format
```

Representation, assuming `len : Int` and `format : Format`:

```fathom
repr (FormatArray len format) // normalizes to `Array len (repr format)`
```

### Current position formats

The current position of the binary stream can be accessed using the `CurrentPos` format:

```fathom
CurrentPos : Format
```

Representation:

```fathom
repr CurrentPos         // normalizes to `Pos`
```

This evaluates to the current position of the binary parser during parsing.

### Link formats

Links allow binary formats to refer to other positions in the binary stream:

```fathom
Link : Pos -> Int -> Format -> Format
```

Representation, assuming `base : Pos`, `offset : Int`, `format : Format`:

```fathom
repr Link base offset format    // normalizes to `Pos`
```

This evaluates to `base` with `offset` added to it during parsing.

### Struct formats

Struct formats are mappings of field names to format descriptions.
When interpreted as a binary parser they are read in definition order:

```fathom
struct Point : Format {
    x : U32Be,
    y : U32Be,
}
```

The fields of struct formats are available for use in subsequent field descriptions.
This allows for data-dependent formats to be defined:

```fathom
struct MyArray : Format {
    len : U32Be,
    // `len : repr U32Be` is now available
    data : FormatArray len U32Be,
}
```

Representation:

```fathom
repr Point      // normalizes to `repr Point`
repr MyArray    // normalizes to `repr MyArray`
```

The representation of struct formats are struct types,
and so struct format representations be introduced with [struct terms]:

```fathom
struct { x = 2, y = 3 } : repr Point
struct { len = 2, data = [ 2, 3 ] } : repr MyArray
```

[struct terms]: ./structs.md#introduction

The representation of struct formats can also be eliminated with [field lookups]:

```fathom
struct Header : Format {
    count : U32Be,
}

struct TestFormat {
    header : Header,
    // `header : repr Header` is now available.
    // Because it is a structure type, we can eliminate it with `header.count`.
    data : FormatArray header.count U32Be,
}
```

[field lookups]: ./structs.md#elimination

### Enumeration formats

> **TODO**: add documentation
