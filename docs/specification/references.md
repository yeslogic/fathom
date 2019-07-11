# References

A list of resources that were useful when designing the data description language.

## Academic papers

### Binary data descriptions

- Kathleen Fisher, Yitzhak Mandelbaum, and David Walker. 2006.
  The next 700 data description languages.
  In _Conference record of the 33rd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '06)_.
  ACM, New York, NY, USA, 2-15.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1111039)]
  [[PDF](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf)]
- Nicolas Oury and Wouter Swierstra. 2008.
  The power of Pi.
  In _Proceedings of the 13th ACM SIGPLAN international conference on Functional programming (ICFP '08)_.
  ACM, New York, NY, USA, 39-50.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1411213)]
  [[PDF](https://cs.ru.nl/~wouters/Publications/ThePowerOfPi.pdf)]

### Dependent type sytems

- Andres Löh, Conor McBride, and Wouter Swierstra. 2010.
  A Tutorial Implementation of a Dependently Typed Lambda Calculus.
  Fundam. Inf. 102, 2 (April 2010), 177-207.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1883637)]
  [[PDF](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)]
  [[SITE](https://www.andres-loeh.de/LambdaPi/)]
- Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, and Makoto Takeyama. 2009.
  A simple type-theoretic language: Mini-TT.
  In _Semantics to Computer Science: Essays in Honour of Gilles Kahn_.
  Cambridge University Press, 139-164.\
  [[PDF](http://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)]

## Language specifications

The following language specifications and language references were used as
inspiration when writing the specification of the data description language:

- [WebAssembly Specification](https://webassembly.github.io/spec/core/index.html)
- [Dhall Semantics](https://github.com/dhall-lang/dhall-lang/tree/master/standard)
- [Swift Language Reference](https://docs.swift.org/swift-book/ReferenceManual/AboutTheLanguageReference.html)
- [The Rust Reference](https://doc.rust-lang.org/reference/index.html)

## Existing Binary data description languages:

[dloss/binary-parsing](https://github.com/dloss/binary-parsing):
This is a a comprehensive list of tools for and links about parsing binary data
structures, such as file formats, network protocols or bitstreams.

- [Kaitai Struct](http://kaitai.io):
  Uses YAML as a way of marking up the data definitions. Lots of nice
  examples, a nice graphvis output, and a nice web-based IDE. It lacks a
  formal foundation however, and YAML can be hard to read.
- [IPADS/DDC](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf):
  We like the IPADS/DDC approach, but it is geard more towards log files and
  misses things like pointer offsets which are needed to support many
  binary formats.
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
  approach has drawbacks to do with being to tied to a specific programming
  language for specifications, and a specific operational semantics.
