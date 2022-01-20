# Sequences

Some terms can be constructed using an ordered sequence of elements,
in a bracket-delimited, comma-separated list:

```fathom
[1, 2, 3]
```

At the moment only `Array`s are supported.

## Arrays

Arrays can be constructed using sequence terms:

```fathom
[1, 2, 3] : Array 3 Int;
```

An error is reported if the number of elements does not match the length
specified in the type:

```fathom
[1, 2, 3] : Array 0 Int; // error: Incorrect number of elements in array
```

## Ambiguous Sequences

Sequence terms must always have a type annotation, otherwise they are considered
to be ambiguous:

```fathom
const my_array_2 = [1, 2, 3]; // error: Ambiguous sequence
```
