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

## Roadmap

- language features
  - [ ] top-level items
  - [ ] recursive definitions
  - [x] let expressions
  - [x] dependent funs
    - [ ] implicit parameters
  - [x] records
    - [x] non-dependent
    - [x] dependent
  - [x] holes
    - [x] named holes
  - [x] numeric types
  - [x] numeric literals
    - [x] decimal literals
    - [ ] non-decimal radixes
    - [ ] custom parsing
  - [x] array types
  - [x] array literals
  - [ ] binary format descriptions
    - [x] succcess formats
    - [x] failure formats
    - [ ] end-of-input formats
    - [x] record formats
      - [ ] conditional field sugar
      - [ ] skipped fields
    - [x] position formats
    - [ ] link formats formats
    - [ ] map formats
    - [x] numeric formats
    - [x] array formats
    - [ ] uniform-choice formats
    - [ ] choice formats
    - [ ] repeat formats
  - [ ] type refinements
  - [x] match expressions
    - [x] single-layer pattern matching
    - [ ] multi-layer pattern matching
    - [ ] dependent pattern matching
  - [ ] patterns
    - [x] wildcard patterns
    - [x] named patterns
    - [x] annotated patterns
    - [x] numeric literal patterns
    - [ ] record literal patterns
  - [ ] invertible format descriptions
- implementation
  - [x] command line interface
  - [x] parser
  - [ ] pretty printing
    - [x] surface language
    - [ ] core language
  - [ ] source locations
    - [x] surface language
    - [ ] core language
  - [x] string interning
  - [x] arena allocation
  - [ ] value interning (for commonly used values)
  - [x] normalisation-by-evaluation
    - [ ] stack traces
  - [x] elaborator
    - [x] error recovery
    - [x] unification
    - [ ] zonking
  - [x] distiller
    - [ ] improve binder names
    - [ ] improve hole names
  - [ ] core language validation
  - [x] binary format interpreter
    - [x] parser
    - [ ] pretty printer
  - [ ] compiler
  - [x] codespan diagnostics
    - [x] unification solutions
    - [ ] terms and types included in messages
  - [x] integration tests
    - [x] basic error code checks
    - [ ] snapshot testing
    - [ ] diagnostic expectations
    - [ ] binary parser tests
