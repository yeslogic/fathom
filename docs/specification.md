# DDL: Language Specification

Here we describe the syntax and semantics of the data description language in a
semi-formal way.

## Limitations

It is important to note that we do not claim that these semantics are sound,
but at the very least this specification could form the building blocks of
formally verified specification in the future.

## Sections

-   [Core theory](./specification/core-theory):
    the core type theory of the language
-   [Concrete syntax](./specification/concrete-syntax):
    the concrete syntax of the language
-   [Elaboration](./specification/elaboration):
    elaboration of the concrete syntax into the core type theory
-   [Binary interpretation](./binary-interpretation):
    describes a how to interpret binary types as binary encoders and decoders

## How to read the specification

The following notation is used to specify the data description language.
The notation is reminiscent of what you might find in programming language and
type theory texts, but we have made an effort to take advantage of the increased
amount of space and styling options that we have on the web in order to make
it more readable.

### Terminal symbols

Terminal symbols are written in monospace font:

-   `{`
-   `struct`
-   `U32Be`

### Non-terminal symbols

Non-terminal symbols are written in italics:

-   _term_
-   _primitive-type_

We treat multiple instances of the same non-terminal in the same local scope
as referring to the same non-terminal.
We can distinguish between multiple non-terminals using subscripts:

-   _term_<sub>0</sub>
-   _term_<sub>1</sub>
-   &hellip;
-   _term_<sub>_n_</sub>

### Sequences

Sequences are ordered lists of elements

-   ε is the empty sequence
-   _elem_<sup>\*</sup> is a possibly empty sequence of _elem_
-   _elem_<sup>\+</sup> is a possibly non-empty sequence of _elem_
-   _elem_<sup>?</sup> is an optional occurrence of of _elem_
-   _elem_ ∈ _elems_ can be believed if _elem_ can be found within the sequence _elems_
-   _elem_ ∉ _elems_ can be believed if _elem_ cannot be found within the sequence _elems_

### Grammar productions

Productions are written as:

> <sub>Grammar:</sub>
>
> _symbol_ ::=\
> &emsp;|&ensp;_A_<sub>0</sub>\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;_A_<sub>_n_</sub>

### Judgment forms

_Forms of judgement_ are declared in the following way:

> <sub>Judgement form:</sub>
>
>  _item-context_ `⊢` _module_ `module`

### Inference rules

An _inference rule_ is described using the following notation:

> <sub>Inference rule:</sub>
>
> - premise<sub>0</sub>
> - &hellip;
> - premise<sub>_n_</sub>
> ----------------------------------------------------------------------------------------------
> - conclusion

Where the _premises_ and _conclusions_ follow pre-declared forms of judgement.
We can read this as saying that:
"if we believe in the premises, then it is also safe to believe in the conclusion".

### Records

Records are productions in the form:

> <sub>Grammar:</sub>
>
> _record_ ::=\
> &emsp;|&ensp;`{` _label_<sub>0</sub> _elem_<sub>0</sub>\
> &emsp;&emsp;`,` _label_<sub>1</sub> _elem_<sub>1</sub>\
> &emsp;&emsp;`,` &hellip;\
> &emsp;&emsp;`,` _label_<sub>_n_</sub> _elem_<sub>_n_</sub> \
> &emsp;&emsp;`}`

- _record_._label_<sub>_n_</sub> denotes the contents of _elem_<sub>_n_</sub> in _record_
- _record_, _label_<sub>_n_</sub> _elem_<sup>\*</sup> denotes the same _record_, but with _elem_<sup>\*</sup> prepended to the sequence _elem_<sub>_n_</sub>
