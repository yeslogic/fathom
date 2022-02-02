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

## Related projects

- [binary-data](https://github.com/dylan-lang/binary-data)
- [Daedalus](https://github.com/GaloisInc/daedalus)
- [Kaitai Struct](https://kaitai.io/)
- [Narcissus](https://github.com/mit-plv/fiat/tree/master/src/Narcissus)
- [PADS](http://www.padsproj.org/)
- [Wuffs](https://github.com/google/wuffs)

More can be be found in dloss' excellent [list of tools](https://github.com/dloss/binary-parsing)
for binary parsing.
