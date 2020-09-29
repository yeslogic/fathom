# Records

Records are labelled, heterogeneous collections of data.

## Formation

A record type can be formed with a sequence of unique, labelled types called _fields_.
Record types may be dependent, with types of later fields depending on the value of previous fields.
For example:

```fathom
Point : Type = {
    x : Int,
    y : Int,
};

MyArray : Type = {
    len : Int,
    data : Array len Point,
    //           ^ The type of the `data` field depends on `len`
};
```

Record types can only contain unique fields:

```fathom
Point : Type = {
    x : Int,
    x : Int, // error!
};
```

Because the syntax is overloaded with [record format descriptions],
we need to explicitly annotate records with `Type` when the usage is ambiguous:

```fathom
Point = { // error!
    x : Int,
    y : Int,
};
```

Records types can only contain terms:

```fathom
Data : Type = {
    type : Type, // error!
};
```

[record format descriptions]: ./format-descriptions.md#record-formats

## Introduction

```fathom
{ x = 23, y = 42 } : Point
```

## Elimination

> **TODO**: add documentation
