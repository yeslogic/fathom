# Background Reading

A list of resources that were useful when designing Fathom.

## Academic papers

### Binary data descriptions

- Benjamin Delaware, Sorawit Suriyakarn, Clément Pit-Claudel, Qianchuan Ye, and Adam Chlipala. 2019.
  **Narcissus: Correct-by-Construction Derivation of Decoders and Encoders from Binary Formats**.
  In _Proceedings of the ACM on Programming Languages (ICFP '19)_.\
  [[ACM](https://dl.acm.org/citation.cfm?doid=3352468.3341686)]
  [[PDF](https://www.cs.purdue.edu/homes/bendy/Narcissus/narcissus.pdf)]
  [[SITE](https://www.cs.purdue.edu/homes/bendy/Narcissus/)]
  [[CODE](https://github.com/mit-plv/fiat/tree/narcissus-icfp2019)]
- Kathleen Fisher, Yitzhak Mandelbaum, and David Walker. 2006.
  **The next 700 data description languages**.
  In _Conference record of the 33rd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '06)_.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1111039)]
  [[PDF](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf)]
- Kathleen Fisher, Yitzhak Mandelbaum, and David Walker. 2007.
  **A Dual Semantics for the Data Description Calculus (Extended Abstract)**.\
  [[PDF](https://www.cs.princeton.edu/~dpw/papers/tfp07.pdf)]
- Hannes Mehnert, Andreas Bogk, 2007.
  **A domain-specific language for manipulation of binary data in Dylan**.
  In _Proceedings of the 2007 International Lisp Conference_.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1622148)]
  [[PDF](https://github.com/dylan-lang/binary-data/blob/master/documentation/a-DSL-for-manipulation-of-binary-data.pdf)]
- Nicolas Oury and Wouter Swierstra. 2008.
  **The power of Pi**.
  In _Proceedings of the 13th ACM SIGPLAN international conference on Functional programming (ICFP '08)_.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1411213)]
  [[PDF](https://cs.ru.nl/~wouters/Publications/ThePowerOfPi.pdf)]
- Tahina Ramananandro, Antoine Delignat-Lavaud, Cédric Fournet, Nikhil Swamy, Tej Chajed, Nadim Kobeissi, and Jonathan Protzenko. 2019.
  **EverParse: Verified Secure Zero-Copy Parsers for Authenticated Message Formats.**
  In _USENIX Security Symposium 2019_.\
  [[PDF](https://www.chajed.io/papers/everparse:usenix-sec2019.pdf)]
  [[CODE](https://github.com/project-everest/everparse/)]

### Dependent type systems

- Andres Löh, Conor McBride, and Wouter Swierstra. 2010.
  **A Tutorial Implementation of a Dependently Typed Lambda Calculus.**
  Fundam. Inf. 102, 2 (April 2010), 177-207.\
  [[ACM](https://dl.acm.org/citation.cfm?id=1883637)]
  [[PDF](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)]
  [[SITE](https://www.andres-loeh.de/LambdaPi/)]
- Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, and Makoto Takeyama. 2009.
  **A simple type-theoretic language: Mini-TT.**
  In _Semantics to Computer Science: Essays in Honour of Gilles Kahn_.
  Cambridge University Press, 139-164.\
  [[PDF](http://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)]

## Language specifications

The following language specifications and language references were used as
inspiration when writing the specification of Fathom:

- [WebAssembly Specification](https://webassembly.github.io/spec/core/index.html)
- [Dhall Semantics](https://github.com/dhall-lang/dhall-lang/tree/master/standard)
- [Swift Language Reference](https://docs.swift.org/swift-book/ReferenceManual/AboutTheLanguageReference.html)
- [The Rust Reference](https://doc.rust-lang.org/reference/index.html)

## Existing Binary data description languages:

[dloss/binary-parsing](https://github.com/dloss/binary-parsing):
This is a a comprehensive list of tools for and links about parsing binary data structures,
such as file formats, network protocols or bitstreams.

- [Kaitai Struct](http://kaitai.io):
  Uses YAML as a way of marking up the data definitions. Lots of nice examples,
  a nice graphvis output, and a nice web-based IDE. It lacks a formal foundation however,
  and YAML can be hard to read.
- [IPADS/DDC](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf):
  We like the IPADS/DDC approach, but it is geared more towards log files and misses features like pointer offsets
  which are needed to support many binary formats.
- [Restructure](https://github.com/devongovett/restructure):
  A library for declaratively encoding and decoding binary data using a JavaScript EDSL.
  Used in [fontkit](https://github.com/devongovett/fontkit) for describing the Open Type Font specification.
  Shows promise as nice way to describe binary formats, but JS is not the most flexible host language.
- [HarfBuzz](https://github.com/harfbuzz/harfbuzz) defines [a handy set of types and macros](https://github.com/harfbuzz/harfbuzz/blob/35218c488c3966aa6d459ec5a007a2b43208e97c/src/hb-machinery.hh)
  that allows binary data to be cast in-place into declaratively described Open Type Font tables.
  Alas, it is in C++, and the API seems extremely prone to human-error.
  A Rust API would be much more robust in regards to upholding claimed invariants.
- [Nom](https://github.com/Geal/nom):
  A Rust parser combinator library geared towards describing binary formats.
  It is fast and zero copy, but we believe that the combinator approach has drawbacks to do with
  being too tied to a single host language for specifications,
  and tied to a single operational semantics.
