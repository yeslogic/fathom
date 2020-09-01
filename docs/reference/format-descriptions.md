# Binary format descriptions

Binary format descriptions allow us to describe binary formats in a declarative way.
These can then be compiled to parser implementations.

All binary format descriptions are elements of the primitive type `Format`.
`Format` is an element of the `Kind` sort.

```fathom
Format : Kind
```

Although they seem to be types on a superficial, syntactic level,
they are better thought of as a primitive inductive datatype that is hard-coded into the host language.

Each binary format description can be interpreted as a corresponding 'representation type'
that is intended to represent result of parsing a format description.
This can be accessed by using the `repr` operator, for example:

```fathom
repr U8
```

## Numeric formats

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

## Character formats

> **TODO**: add documentation

## String formats

> **TODO**: add documentation

## Array formats

> **TODO**: add documentation

## Offset formats

> **TODO**: add documentation

## Record formats

> **TODO**: add documentation
