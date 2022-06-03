# Further Reading and Related Work

We were originally inspired by [“The next 700 Data Description Languages”](https://doi.org/10.1145/1111037.1111039)
by Fisher et. al, but have since drawn more heavily on the data description
language defined in [“The power of Pi”](https://doi.org/10.1145/1411204.1411213)
by Oury and Swierstra.

The implementation is heavily based on the implementations in Andras Korvacs'
[Elaboration Zoo][elaboration-zoo]. We adapt them to more idiomatic Rust, using
arenas for allocating source terms, and reference-counting in values, and
reduce allocations/indirection where possible.

[elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/

## Format descriptions as universes

Our approach to format descriptions (inspired by “The power of Pi”) is closely
related to the idea of [_Tarski-style Universes_][tarski-universes]. In this
approach, universes _U_ are defined as a datatype, together with an _El_
operation that interprets these universes into actual types. In Fathom `Format`
can be seen as a universe of binary formats, and `Repr` interprets these formats
into representation types in the host language. We still contimue to use
[_Russell-style universes_][russell-universes] for describing types, however.

Below you can see a comparison between Tarski-style universes and `Format`s in
Fathom:

```text
                  A : U
────────      ────────────
 U type        El(A) type

 A : U    B : El(A) → U           A : U         B : El(A) → U
────────────────────────    ──────────────────────────────────────
      pi(A, B) : U           El(pi(A, B)) ≡ Π x : El(A). El(B(x))
```

```text
                    A : Format
─────────────     ──────────────
 Format type       Repr(A) type


 A : Format    B : Repr(A) → Format        A : Format         B : Repr(A) → Format
────────────────────────────────────    ──────────────────────────────────────────────
       pair(A, B) : Format               Repr(pair(A, B)) ≡ Σ x : Repr(A). Repr(B(x))
```

[tarski-universes]: https://ncatlab.org/homotopytypetheory/show/universe#Tarski
[russell-universes]: https://ncatlab.org/homotopytypetheory/show/universe#Russell

## Related projects

- [binary-data](https://github.com/dylan-lang/binary-data)
- [Daedalus](https://github.com/GaloisInc/daedalus)
- [Kaitai Struct](https://kaitai.io/)
- [Narcissus](https://github.com/mit-plv/fiat/tree/master/src/Narcissus)
- [PADS](http://www.padsproj.org/)
- [Wuffs](https://github.com/google/wuffs)

More can be be found in dloss' excellent [list of tools](https://github.com/dloss/binary-parsing)
for binary parsing.
