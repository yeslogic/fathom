# Notation

The following notation is used to specify the data description language.
The notation is reminiscent of what you might find in programming language and
type theory texts, but we have made an effort to take advantage of the increased
amount of space and styling options that we have on the web in order to make
it more readable.

## Terminal symbols

Terminal symbols are written in monospace font:

-   `{`
-   `struct`
-   `U32Be`

## Non-terminal symbols

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

## Sequences

Sequences are ordered lists of elements

-   ε is the empty sequence
-   _elem_<sup>_n_</sup> is a sequence of _n_ iterations of _elem_
-   _elem_<sup>_n_&hellip;</sup> is a sequence of _n_ ≤ ∞ iterations of _elem_
-   _elem_<sup>&hellip;_n_</sup> is a sequence of 0 ≤ _n_ iterations of _elem_
-   _elem_<sup>\*</sup> is a possibly empty sequence of iterations of _elem_ (short for _elem_<sup>0&hellip;</sup>)
-   _elem_<sup>\+</sup> is a possibly non-empty sequence of iterations of _elem_ (short for _elem_<sup>1&hellip;</sup>)
-   _elem_<sup>?</sup> is an optional occurrence of of _elem_ (short for _elem_<sup>&hellip;1</sup>)
-   _elem_ ∈ _elems_ can be believed if _elem_ can be found within the sequence _elems_
-   _elem_ ∉ _elems_ can be believed if _elem_ cannot be found within the sequence _elems_

## Grammar productions

Productions are written as:

> <sub>Grammar:</sub>
>
> _symbol_ ::=\
> &emsp;|&ensp;_A_<sub>0</sub>\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;_A_<sub>_n_</sub>

Macro productions are in the form:

> <sub>Grammar:</sub>
>
> symbol(_param_<sup>_n_</sup>) ::=\
> &emsp;|&ensp;_A_<sub>0</sub>\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;_A_<sub>_m_</sub>

Where the each of the symbols in _A_<sup>_m_</sup> may mention any parameter in
_param_<sup>_n_</sup>.

## Judgment forms

_Forms of judgement_ are declared in the following way:

> <sub>Judgement form:</sub>
>
>  _item-context_ `⊢` _module_ `module`

## Inference rules

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

## Records

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
