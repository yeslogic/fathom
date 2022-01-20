# Fathom

[![Actions Status][actions-badge]][actions-url]
[![Matrix][matrix-badge]][matrix-lobby]
[![License][license-badge]][license-url]

[actions-badge]: https://github.com/yeslogic/fathom/workflows/ci/badge.svg
[actions-url]: https://github.com/yeslogic/fathom/actions
[matrix-badge]: https://img.shields.io/badge/chat-%23fathom--lang%3Amatrix.org-brightgreen
[matrix-lobby]: https://matrix.to/#/#fathom-lang:matrix.org
[license-badge]: https://img.shields.io/github/license/yeslogic/fathom
[license-url]: ./LICENSE

A language for specifying data-dependent binary data formats.

## Example

```text
let pixel = {
    red <- u8,
    green <- u8,
    blue <- u8,
};

let image = {
    width <- u16le,
    height <- u16le,
    pixels <- array16 (width * height) pixel,
};

image
```

## References

We were originally inspired by [“The next 700 Data Description Languages”](https://doi.org/10.1145/1111037.1111039)
by Fisher et. al, but have since drawn more heavily on the data description
language defined in [“The power of Pi”](https://doi.org/10.1145/1411204.1411213)
by Oury and Swierstra.

The implementation is heavily based on the implementations in Andras Korvacs'
[Elaboration Zoo][elaboration-zoo]. We adapt them to more idiomatic Rust, using
arenas for allocating source terms, and reference-counting in values, and
reduce allocations/indirection where possible.

[elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/

## Code of Conduct

Please note that this project is released with a [Code of Conduct](./CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

Fathom is licensed under the terms of the Apache License (Version 2.0).

See [LICENSE](./LICENSE) or <http://www.apache.org/licenses/LICENSE-2.0> for details.
