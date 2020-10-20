# Structs

Structs are labelled, heterogeneous collections of data.

## Formation

A record type can be formed with a sequence of unique, labelled types called _fields_.
Struct types may be dependent, with types of later fields depending on the value of previous fields.
For example:

```fathom
struct Point : Type {
    x : Int,
    y : Int,
}

struct MyArray : Type {
    len : Int,
    data : Array len Point,
    //           ^ The type of the `data` field depends on `len`
}
```

Struct types can only contain unique fields:

```fathom
struct Point : Type {
    x : Int,
    x : Int, // error!
}
```

Because the syntax is overloaded with [struct format descriptions],
we need to explicitly annotate struct types with `Type` when the usage is ambiguous:

```fathom
struct Point { // error!
    x : Int,
    y : Int,
}
```

[struct format descriptions]: ./format-descriptions.md#struct-formats

Struct types can only contain terms:

```fathom
struct Data : Type {
    type : Type, // error!
}
```

## Introduction

Inhabitants of struct types are known as 'struct terms'.
These can be introduced in with sequences of labelled terms:

```fathom
struct { x = 23, y = 42 } : Point
```

## Elimination

Struct terms can be eliminated using field lookups:

```fathom
point : Point = struct {
    x = 23,
    y = 42,
};

point.x // normalizes to `23`
```

When synthesizing the type of field lookups we need to be careful about data dependencies:

```fathom
foo (array : MyArray) =
    array.data; // type of body is `Array array.len Point`
```
