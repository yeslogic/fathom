# DDL: Language Specification

Here we describe the syntax and semantics of the data description language in a
semi-formal way.

## Limitations

It is important to note that we do not claim that these semantics are sound,
but at the very least this specification could form the building blocks of
formally verified specification in the future.

## Sections

-   [Notation](./specification/notation.md):
    description of the notation used in the specification
-   [Primitives](./specification/primitives.md):
    primitive constants in the host language
-   [Core theory](./specification/core-theory.md):
    the core type theory of the language
-   [Concrete syntax](./specification/concrete-syntax.md):
    the concrete syntax of the language
-   [Elaboration](./specification/elaboration.md):
    elaboration of the concrete syntax into the core type theory
-   [Binary interpretation](./specification/binary-interpretation.md):
    describes how to interpret binary types as binary encoders and decoders
-   [References](./specification/references.md):
    list of references that proved useful when designing the data description language
