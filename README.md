# DDL (placeholder name)

A declarative binary data definition language.

## References

- [PADS website](https://pads.cs.tufts.edu)
- [The next 700 data description languages](https://pdfs.semanticscholar.org/2f5f/097261fdb7b0922fb548b487be28613640d8.pdf)
- [Kaitai Struct](http://kaitai.io)

## Development status

Early days. Currently we have two parallel implementations, one written in
Mercury, and one in Rust. The ultimate goal is to replace the Mercury
implementation with the Rust version, and use it to generate a zero-copy
parser for the [Open Type Font specification], but it could be used more
widely than that in the future.

[Open Type Font specification]: https://www.microsoft.com/typography/otspec/otff.htm

## License

The Rust code is distributed under the terms of the Apache License (Version 2.0).

The copyright to the Mercury implementation is currently reserved by YesLogic.

See [LICENSE](LICENSE) for details.
