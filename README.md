# DDL (placeholder name)

A declarative data definition language for formally specifying sound binary
data formats.

## Development status

Early days. Currently we have two parallel implementations, one written in
Mercury, and one in Rust. The ultimate goal is to replace the Mercury
implementation with the Rust version, and use it to generate a zero-copy
parser for the [Open Type Font specification], but it could be used more
widely than that in the future.

[Open Type Font specification]: https://www.microsoft.com/typography/otspec/otff.htm

### Planned features

- [x] External DSL
- [ ] Advanced type system
  - [ ] Polymorphic types
    - [ ] Types depending on values
    - [x] Dependant arrays: eg: `Array(u8, 4)`
    - [ ] Parametrised Endianness: eg: `Body(E: Endianness) = ...`
  - [x] Dependant records: eg: `{ len: u8, data: Array(u8, len) }`
  - [ ] Unions
  - [ ] Constraints
  - [ ] Pointer offsets
- [ ] Rust codegen:
  - [ ] zero copy
  - [ ] push/pull modes
- [ ] Library of example formats: demonstrate how the language can be used to describe common formats
- [ ] Specification generation: output readable format specifications
- [ ] Graphvis output
- [ ] Formal specification

## Inspiration

[dloss/binary-parsing](https://github.com/dloss/binary-parsing):
This is a handy list of generic tools for parsing binary data structures, such
as file formats, network protocols or bitstreams.

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

## License

The Rust code is distributed under the terms of the Apache License (Version 2.0).

The copyright to the Mercury implementation is currently reserved by YesLogic.

See [LICENSE](LICENSE) for details.
