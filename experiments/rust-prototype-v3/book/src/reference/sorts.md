# Sorts

Fathom has two sorts, `Type` and `Kind`, with a single axiom:

```fathom
Type : Kind
```

`Kind` has no type, so it may only ever appear in type annotations.
For example it cannot be used in top-level definitions:

```fathom
const MyKind = Kind; // error!
```
