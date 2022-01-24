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
