# Primitive Types

The data description language has a number of built-in types that can be used
to create more complicated format specifications.

## Unsigned integers

| Type | Description |
| ---- | ----------- |
| `U8` | 8-bit unsigned integers |
| `U16Le` | 16-bit unsigned integers (little endian) |
| `U16Be` | 16-bit unsigned integers (big endian) |
| `U32Le` | 32-bit unsigned integers (little endian) |
| `U32Be` | 32-bit unsigned integers (big endian) |
| `U64Le` | 64-bit unsigned integers (little endian) |
| `U64Be` | 64-bit unsigned integers (big endian) |

## Signed integers

These are encoded using a [two's complement][twos-complement-wikipedia]
representation.

| Type | Description |
| ---- | ----------- |
| `S8` | 8-bit signed integers |
| `S16Le` | 16-bit signed integers (little endian) |
| `S16Be` | 16-bit signed integers (big endian) |
| `S32Le` | 32-bit signed integers (little endian) |
| `S32Be` | 32-bit signed integers (big endian) |
| `S64Le` | 64-bit signed integers (little endian) |
| `S64Be` | 64-bit signed integers (big endian) |

[twos-complement-wikipedia]: https://en.wikipedia.org/wiki/Two%27s_complement

## Floating point numbers

These are encoded following the [IEEE Standard for Floating-Point Arithmetic
(IEEE 754)][ieee-754-wikipedia].

| Type | Description |
| ---- | ----------- |
| `F32Le` | single-precision floats (little endian) |
| `F32Be` | single-precision floats (big endian) |
| `F64Le` | double-precision floats (little endian) |
| `F64Be` | double-precision floats (big endian) |

[ieee-754-wikipedia]: https://en.wikipedia.org/wiki/IEEE_754
