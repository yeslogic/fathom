# Specification

## Contents

- [Expressions](#expressions)
  - [Integer expressions](#integer-expressions)
  - [Boolean expressions](#boolean-expressions)
- [Types](#types)
  - [Integer Types](#integer-types)
  - [Array Types](#array-types)
  - [Existential Types](#existential-types)
  - [Struct Types](#struct-types)
  - [Constrained Types](#constrained-types)
  - [Interpreted Types](#interpreted-types)
  - [Conditional Types](#conditional-types)
  - [Choice Types](#choice-types)
  - [Empty Type](#empty-type)
  - [Error Type](#error-type)
  - [End Type](#end-type)
  - [Repeating Types](#repeating-types)
  - [Links](#links)
  - [Type Declarations](#type-declarations)
  - [Distinguishable Types](#distinguishable-types)

## Expressions

### Integer expressions

- numbers: `3`, `-10`, `0xFF`
- identifiers: `count`, `hdrSize`
- arithmetic: `count - 1`, `hdrSize*4`
- indexing: `values[6]`, `offset[i+1]`
- properties: `struct.field`, `array.length`
- functions: `sizeof(type)`
- conditionals: `if x > 3 { x } else { 0 }`

FIXME bitwise arithmetic?
FIXME what about strings?
FIXME what about char arrays for tags, eg. 'OS/2' ?

### Boolean expressions

- equality: `x = y`, `x != y`
- relational: `x < y`, `x > y`, `x =< x`, `x >= y`
- conjunction: `a, b`
- disjunction: `a ; b`
- negation: `!a`
- `for i < count: offset[i] < offset[i+1]`
- `exists i < count: table_records[i].tag == 'hmtx'`

## Types

Types match sequences of bytes and describe their interpretation as values such as integers, arrays, and structs.

The size of a type refers to the length of the byte sequences that it matches, which can be fixed, variable, or unlimited.

- Fixed size types always match the same number of bytes and this number is known in advance, before any of the bytes are read.

- Variable size types can match a different number of bytes depending on the content of those bytes, for example a struct containing a length field and a variable length array.

- Unlimited size types can match any number of bytes and are only constrained by the number of bytes available.

The size of a type may also depend on values from the environment that need to be evaluated before the actual size can be determined.

### Integer Types

There are types for signed and unsigned integers of various sizes:

- `int8`, `int16`, `int32`, `int64`
- `byte`/`uint8`, `uint16`, `uint32`, `uint64`

The integer types always match if there are sufficient bytes available and any alignment constraint is met.

FIXME we could rename these to `u8`/`u16`/`u32` etc. like Rust to save typing if we think that's easier

FIXME we will need an option to specify whether integers are big endian or little endian; currently big endian is assumed just for compatibility with OpenType.

FIXME we will also need an option to specify the default alignment for integer fields and a way to override this for specific fields.

```
sizeof(int8) = 1
sizeof(int16) = 2
sizeof(int32) = 4
...
```

### Array Types

Arrays are a multiple of any fixed size type:

- `type[length]`
- `type[]` (see existential types)

The length can be any integer expression as long as it only depends on values located before the array.

Because the array element type has fixed size, the array itself will also have fixed size if the length is known in advance, or variable size if the length depends upon another value.

```
sizeof(type[length]) = sizeof(type) * length
```

### Existential Types

Existential types are parameterised by a variable of unknown value:

```
exists x: type
```

The value of this variable can be determined from constraints expressed on it elsewhere, such as a `@where` clause.

For arrays of unknown length there is a shorthand syntax where if the length is omitted it is treated as introducing a new unnamed variable:

```
type[]

exists x: type[x]
```

This allows the length of trailing arrays to be expressed simply:

```
struct MyStruct {
    length: uint32
    field1: uint32
    field2: uint32
    data: uint32[]
}
@where sizeof(MyStruct) = length
```

This is equivalent to:

```
struct MyStruct {
    length: uint32
    field1: uint32
    field2: uint32
    data: exists x: uint32[x]
}
@where sizeof(MyStruct) = length
```

And the constraint simplifies as follows:

```
sizeof(MyStruct) = length
sizeof(length) + sizeof(field1) + sizeof(field2) + sizeof(uint32)*x = length
(x + 3)*sizeof(uint32) = length
x = length/sizeof(uint32) - 3
```

### Struct Types

Structs are sequences of typed fields with unique names:

```
struct {
    num_tables: uint16
    tag: byte[4]
}
```

A struct has fixed size if all of its fields are fixed size, or variable size if it contains at least one variable sized field, or unknown size if its last field has unknown size. (Only the last field in a struct can have unknown size (FIXME really?)).

Fields can be referenced by name in expressions, for example to give the size of an array field later in the struct:

```
struct {
    count: uint32
    values: uint32[count]
}
```

Note the restriction on field ordering means the reverse would not be a valid struct type:

```
struct {
    values: uint32[count] // error!
    count: uint32
}
```

It would be impossible to locate the count field without looking past the array, but the array size depends on the count field, so this struct is impossible to process and is erroneous.

```
sizeof(struct {}) = 0
sizeof(struct {field: type | fields}) =
    sizeof(type) + sizeof(struct {fields})
```

### Constrained Types

Constrained types consist of a named value of a type followed by a `@where` clause with a boolean expression that constrains the value:

```
name: type @where expr
```

If the expression evaluates to false then the type will not match.

This can be used directly on struct fields:

```
version: uint32 @where version = 0x00010000

hdrSize: byte @where hdrSize >= 4
```

Or on any other types, such as array items:

```
data: (x: uint16 @where x > 0)[length]
```

Simple relational constraints can be represented using shorthand syntax without introducing a variable name:

```
uint32 = 0x00010000

uint8 >= 4
```

Where clauses can include multiple constraints with conjunctions using the comma operator:

```
struct {
    version: uint32
    hdrSize: uint32
}
@where version = 0x00010000, hdrSize >= 4
```

FIXME implies extra syntax sugar for unnamed struct field access

```
sizeof(name: type @where expr) = sizeof(type)
```

### Interpreted Types

Interpreted types have a value determined by an expression in terms of their original value:

```
name: type1 @as type2 = expr
```

For example, this can be used to interpret 24-bit integers as 32-bit:

```
x: uint8[3] @as uint32 = x[0] << 24 | x[1] << 16 | x[2]
```

```
sizeof(name: type1 @as type2) = sizeof(type1)
```

### Conditional Types

Conditional types depend on other values and are expressed using if-else and switch expressions:

```
if expr1 { type1 }
else if expr2 { type2 }
else { type3 }

switch {
    type1 when expr1
    type2 when expr2
    type3 otherwise
}
```

The final else can be omitted from an if-else expression, which is equivalent to matching the empty type:

```
if epxr1 { type1 }

if expr1 { type1 }
else { empty }
```

The case expressions in a switch must be mutually exclusive.

FIXME is the default otherwise case in switch expressions mandatory?

Example of an if-else expression:

```
if x = 1 { type1 }
else if y > 3 { type2 }
else { type3 }
```

This would not be a valid switch as the expressions are not mutually exclusive.

Example of a switch expression:

```
switch {
    type1 when x = 1
    type2 when x = 2
    type3 otherwise
}
```

Every switch can be trivially translated to an if-else:

```
if x = 1 { type1 }
else if x = 2 { type2 }
else { type3 }
```

As well as returning entire types, conditional expressions can also return fields to allow structs to optionally include fields depending on the value of other fields in the struct, for example:

```
struct {
    version: uint32
    @if version > 1 {
        extra: uint32
        more: uint32
    }
}
```

FIXME do we need to use `@if` and `@switch` with the `@` for clearer syntax?

A struct containing an `@if` rule cannot be fixed size.

FIXME what if the if only depends on a type argument?

```
sizeof(if X { type1 } else { type2 }) =
    if X sizeof(type1) else sizeof(type2)
```

### Choice Types

Choice types can match one of a set of type options:

```
choice {
    option { header1 }
    option { header2 }
}

header1: struct {
    type: byte = 1
    ...
}

header2: struct {
    type: byte = 2
    ...
}
```

It must be possible to distinguish the options in the choice by looking at the first field in each option, eg. if the first field in each option is constrained to have a different value. (See definition of distinguishable types below).

A choice has fixed size if all of its options have the same fixed size, or variable size if at least one of its options has variable size, or unknown size if at least one of its options has unknown size.

A choice type can be converted to a switch type.

### Empty Type

The `empty` type does not consume any bytes and thus always matches. It can be used in conditional expressions when something is optional:

```
@if version > 1 { uint32 }
@else { empty }
```

### Error Type

The `error` type never matches. It can be used in conditional expressions when something is mandatory:

```
@if version > 1 { uint32 }
@else { error }
```

FIXME do we ever actually need to use the error type given that we already have `@where` clauses?

### End Type

The end type does not consume any bytes but only matches if there are no available bytes remaining. It can be used to ensure that another type has consumed all available bytes:

```
struct {
    first: uint32
    second: uint32
    done: end
}
```

Nothing can follow an end value as it can never match.

FIXME there may be a better way of doing this, maybe in a `@where` clause

### Repeating Types

Repeating types are like arrays except the length may not be known in advance and the element type can be fixed or variable size:

```
repeat count type
```

The element type will be matched zero or more times until it cannot be matched or there are no further bytes available. A repeating type has unknown size.

The number of repetitions can be specified explicitly or constrained by introducing an integer variable representing the number:

```
repeat 10 type

repeat n type
@where n >= min, n =< max
```

A repeating type must be followed by another type. If the repeat is intended to consume all the available bytes then it can be followed by the end type.

FIXME must the type that follows the repeat be distinguishable from the type within the repeat?

```
sizeof(repeat count type) = sum of sizeof each type matched
```

### Links

Links create a reference from one value to another and are created with the `@link` directive:

```
@link name: pointer(base, offset) -> Type

@link name: slice(base, offset, length) -> Type
```

The name is optional and may be used to refer to the linked value.

Links are relative to a base, which can be one of these:

FIXME start of the file? end of the file?
FIXME relative to the current slice, if we are in one?
FIXME current field? other field in current struct?
FIXME the current struct itself?

A pointer link takes an integer offset which can be any arbitary integer expression and is applied to the base to find the location of the pointer. The specified type is then matched at this position; the type cannot have unknown size.

A slice link takes an offset and also an integer length expression which determines the length of the slice. The specified type can have an unknown size, in which case it may match the entire slice.

FIXME can types in slices have links that go outside the slice?

Here are is an example of a pointer link from a struct:

```
script_records[script_count]: struct {
    script_tag: uint32
    script_offset: uint16
    @link script: pointer(???, script) -> Script
}
```

It is also possible to create link arrays using a loop expression:

```
struct {
    num_fonts: uint32
    offset_tables[num_fonts]: uint32
    @link tables[num_fonts]:
        for i < num_fonts:
          pointer(???, offset_tables[i]) -> OffsetTable
}
```

### Type Declarations

Type declarations associate a name with a type:

```
SID := uint16

CharsetRange1 := struct {
    first: SID
    nLeft: byte
}
```

Type declarations can take arguments which are used in the definition of the type:

```
Charset0(nGlyphs:uint16) := struct {
    format: byte = 0
    glyph: SID[nGlyphs-1]
}
```

These arguments must be provided when the type is referenced in order to obtain a usable type.

Type declarations can be recursive:

```
String := struct {
    b: byte
    @if b != 0 {
        next: String
    }
}
```

However recursion must be guarded by at least one non-optional field occurring before any recursive mention of the same type.

### Distinguishable Types

Two types are distinguishable if it is possible to decide which one matches a given byte sequence just by looking at the first field in each type. For example, these two types are distinguishable:

```
struct {
    format: uint32 = 0
    data: uint32
}

struct {
    format: uint32 = 1
    data: byte[4]
}
```

Empty types and the "end" type are distinguishable with non-empty types, but not with each other.
