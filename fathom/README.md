# Minimal language experiment

Experimenting with a simple implementation of type theory elaboration

Based on [Andras Korvacs' elaboration examples][elaboration-zoo]. We adapt them
to more idiomatic Rust, using arenas for allocating source terms, and
reference-counting in values, and reducinallocations/indirection where possible.

[elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/

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
