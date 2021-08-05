# Minimal language experiment

Experimenting with a simple implementation of type theory elaboration

Based on [Andras Korvacs' elaboration examples][elaboration-zoo]. We adapt them
to more idiomatic Rust, using arenas for allocating source terms, and
reference-counting in values, and reducinallocations/indirection where possible.

[elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/

## Surface language

_surface.term_ ::=\
&emsp;| _surface.let-term_\
&emsp;| _surface.let-term_ `:` _surface.term_

_surface.let-term_ ::=\
&emsp;| _surface.fun-term_\
&emsp;| `let` _name_ `:` _surface.fun-term_ `=` _surface.let-term_ `;` _surface.let-term_

_surface.fun-term_ ::=\
&emsp;| _surface.app-term_\
&emsp;| _surface.app-term_ "->" _surface.fun-term_\
&emsp;| `fun` `(` _name_ `:` _surface.term_ `)` `->` _surface.fun-term_\
&emsp;| `fun` _name_ `=>` _surface.fun-term_

_surface.app-term_ ::=\
&emsp;| _surface.atomic-term_\
&emsp;| _surface.app-term_ _surface.atomic-term_

_surface.atomic-term_ ::=\
&emsp;| _name_\
&emsp;| `_`\
&emsp;| `?` _name_\
&emsp;| `Type`\
&emsp;| `(` _surface.term_ `)`

## Core language

_core.index_ ::= `last` | `prev(`_core.index_`)`\
_core.level_ ::= `first` | `next(`_core.level_`)`

_core.term_ ::=\
&emsp;| `rigid-var(`_core.index_`)`\
&emsp;| `flexible-var(`_core.level_`)`\
&emsp;| `ann(`_core.term_`,` _core.term_`)`\
&emsp;| `let(`_name_`,` _core.term_`,` _core.term_`,` _core.term_`)`\
&emsp;| `universe`\
&emsp;| `function/type(`_name_`,` _core.term_`,` _core.term_`)`\
&emsp;| `function/intro(`_name_`,` _core.term_`)`\
&emsp;| `function/elim(`_core.term_`,` _core.term_`)`

_core.value_ ::=\
&emsp;| `stuck(`_core.head_`,` _core.elim_*`)`\
&emsp;| `universe`\
&emsp;| `fun/type(`_core.value_`,` _core.closure_`)`\
&emsp;| `fun/intro(`_core.closure_`)`

_core.head_ ::=\
&emsp;| `rigid-var(`_core.index_`)`\
&emsp;| `flexible-var(`_core.level_`)`

_core.elim_ ::=\
&emsp;| `function(`_core.value_`)`

_core.closure_ ::=\
&emsp;| `(`_core.env_`,` _core.term_`)`

_core.env_ ::=\
&emsp;| _core.env_`,` _core.value_\
&emsp;| `empty`

_core.context_ ::=\
&emsp;| _core.env_`,` _core.value_ `:` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document evaluation, readback, checking and synthesis

## Elaboration

_elab.context_ ::=\
&emsp;| _elab.context_`,` _name_ `:` _core.value_ `=` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document lookup, checking and synthesis

## Roadmap

- language features
  - [x] let expressions
  - [x] dependent functions
    - [ ] implicit parameters
  - [ ] dependent records
  - [x] holes
    - [x] named holes
  - [ ] top-level items
  - [ ] recursive definitions
  - [ ] binary format descriptions
    - [ ] error formats
    - [ ] map formats
    - [ ] pure formats
    - [ ] bind formats
- implementation
  - [x] command line interface
  - [x] parser
  - [x] pretty printing
  - [x] source location tracking
  - [x] string interning
  - [x] arena allocation
  - [ ] value interning (for commonly used values)
  - [x] normalisation-by-evaluation
  - [x] elaborator
    - [x] error recovery
    - [x] unification
    - [ ] zonking
  - [x] distiller
    - [ ] improve binder names
    - [ ] improve hole names
  - [ ] core language validation
  - [x] codespan diagnostics
    - [x] unification solutions
    - [ ] terms and types included in messages
  - [x] integration tests
    - [ ] snapshot testing
    - [ ] diagnostic expectations
