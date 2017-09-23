# DDL (placeholder name)

A declarative data definition language for formally specifying sound binary
data formats.

## Goals

Here is a list of lofty goals that we would like to guide our work:

- Specifications should be defined in declarative and human-readable, with
  embedded, domain-specific documentation alongside
- Specifications should be easily to map to efficient deserializers that are
  competitive with hand-written code
- The metalanguage should have well-specified static and runtime semantics
- The metalanguage should report bugs or gaps in specifications statically
- The metalanguage should be expressive enough to be able to define a wide
  variety of existing binary data formats, allowing for explicit and obvious
  escape hatches in the event of finding bugs or soundness holes
- The metalanguage and associated tooling should evolve to become a platform
  for designing sound binary formats from the start, rather than as just a way
  of defining deserializers of existing formats after-the-fact

## Development status

Early days. _Expect soundness bugs_! Currently we have two parallel
implementations, one written in Mercury, and one in Rust. The ultimate goal is
to replace the Mercury implementation with the Rust version, and use it to
generate a [zero-copy parser] for the [Open Type Font specification], but it
could be used more widely than that in the future.

[zero-copy parser]: https://github.com/yeslogic/allsorts
[Open Type Font specification]: https://www.microsoft.com/typography/otspec/otff.htm

## Inspiration and Prior Art

[dloss/binary-parsing](https://github.com/dloss/binary-parsing):
A comprehensive list of tools for and links about parsing binary data
structures, such as file formats, network protocols or bitstreams.

### Exisiting Tools

- [Kaitai Struct](http://kaitai.io):
  Uses YAML as a way of marking up the data definitions. Lots of nice
  examples, a nice graphvis output, and a nice web-based IDE. It lacks a
  formal foundation however, and YAML can be hard to read.
- [PADS](https://pads.cs.tufts.edu):
  We like the PADS approach, but it is geard more towards log files and
  misses things like pointer offsets which are needed to support many
  binary formats. We also believe that Rust is a better compilation target
  and that providing a binary parser language at the same level of quality
  as LALRPOP will be a great boost to the ecosystem.
- [Restructure](https://github.com/devongovett/restructure):
  A library for declaratively encode and decode binary data using a JavaScript
  EDSL. Used in [fontkit](https://github.com/devongovett/fontkit) for describing
  the Open Type Font specification. Shows promise as nice way to describe
  binary formats, but JS is not the most flexible host language.
- [Harfbuzz](https://github.com/behdad/harfbuzz) defines [a handy set of types
  and macros](https://github.com/behdad/harfbuzz/blob/master/src/hb-open-type-private.hh)
  that allows binary data to be cast in-place into declaratively descibed Open
  Type Font tables. Alas, it is in C++, and the API seems extremely prone to
  human-error. A Rust API would be much more robust in regards to upholding
  claimed invariants.
- [Nom](https://github.com/Geal/nom):
  A Rust parser combinator library geared towards describing binary
  formats. It is fast and zero copy, but we belive that the combinator
  approach is too low-level for formally describing format specifications.

### Research papers

- [PADS/ML: A Functional Data Description Language](https://www.cs.princeton.edu/~dpw/papers/padsml06.pdf)
- [DataScript](http://people.cs.vt.edu/%7Egback/papers/gback-datascript-gpce2002.pdf)
- [Packet Types: Abstract Specification of Network Protocol Messages](http://conferences.sigcomm.org/sigcomm/2000/conf/paper/sigcomm2000-9-2.pdf)
- [Nail: A Practical Tool for Parsing and Generating Data Formats](https://www.usenix.org/system/files/conference/osdi14/osdi14-paper-bangert.pdf)
- [Semantics and Algorithms for Data-dependent Grammars](https://www.cs.princeton.edu/~dpw/papers/ddg-tr.pdf)

### Planned features

- [x] External DSL
- [ ] Type system
  - [ ] Byte-aligned numeric types
    - [ ] Unsigned integers
      - [x] 1-byte (8-bit)
      - [x] 2-byte (16-bit)
      - [ ] 3-byte (24-bit)
      - [x] 4-byte (32-bit)
      - [x] 8-byte (64-bit)
      - [ ] n-byte
    - [ ] Two's complement signed integers
      - [x] 1-byte (8-bit)
      - [x] 2-byte (16-bit)
      - [ ] 3-byte (24-bit)
      - [x] 4-byte (32-bit)
      - [x] 8-byte (64-bit)
      - [ ] n-byte
    - [x] IEEE 754 Floating point numbers
      - [x] 32 bit
      - [x] 64 bit
  - [ ] Polymorphic types
    - [ ] Types depending on values
    - [x] Dependant arrays: eg. `Array(u8, 4)`
    - [ ] Parametrised Endianness: eg. `Body(E: Endianness) = ...`
  - [x] Dependant records: eg. `struct { len: u8, data: Array(u8, len) }`
  - [x] Unions: eg. `union { T1, T2 }`
  - [ ] Constraints `x where expr`
  - [ ] Pointer offsets
- [ ] Backends
  - [ ] Rust codegen:
    - [ ] zero copy
    - [ ] push/pull modes
  - [ ] Graphvis DOT output
  - [ ] HTML Formatted Specification
- [ ] Tooling
  - [ ] Pretty error messages
  - [ ] Textmate syntax highlighting
  - [ ] Language server (via Language Server Protocol)
  - [ ] Binary explorer tool
- [ ] Verification
  - [ ] Fuzz generated parsers
  - [ ] Formal specification

## License

The Rust code is distributed under the terms of the Apache License (Version 2.0).

The copyright to the Mercury implementation is currently reserved by YesLogic.

See [LICENSE](LICENSE) for details.
