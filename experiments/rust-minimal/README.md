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
&emsp;| `let(`_core.term_`,` _core.term_`,` _core.term_`)`\
&emsp;| `universe`\
&emsp;| `fun/type(`_core.term_`,` _core.term_`)`\
&emsp;| `fun/intro(`_core.term_`)`\
&emsp;| `fun/elim(`_core.term_`,` _core.term_`)`

_core.value_ ::=\
&emsp;| `stuck(`_core.head_`,` _core.elim_*`)`\
&emsp;| `universe`\
&emsp;| `fun/type(`_core.value_`,` _core.closure_`)`\
&emsp;| `fun/intro(`_core.closure_`)`

_core.head_ ::=\
&emsp;| `rigid-var(`_core.index_`)`\
&emsp;| `flexible-var(`_core.level_`)`

_core.elim_ ::=\
&emsp;| `fun(`_core.value_`)`

_core.closure_ ::=\
&emsp;| `closure(`_core.env_`,` _core.term_`)`

_core.env_ ::=\
&emsp;| `extend(`_core.env_`,` _core.value_`)`\
&emsp;| `empty`

_core.context_ ::=\
&emsp;| _core.env_`,` _core.value_ `:` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document evaluation, quotation, checking and synthesis

| eval(_core.env_, _core.term_)                        | _core.value_
| ---------------------------------------------------- | -------------------------------------------
| eval(_env_, `rigid-var(`_var_`)`)                    | &hellip;
| eval(_env_, `flexible-var(`_var_`)`)                 | &hellip;
| eval(_env_, `ann(`_term_`,` \_`)`)                   | eval(_env_, _term_)
| eval(_env_, `let(`_def-expr_`,` \_`,` _out-expr_`)`) | eval(`extend(`_env_`,` eval(_env_, _def-expr_)`)`, _out-expr_)
| eval(_env_, `universe`)                              | `universe`
| eval(_env_, `fun/type(`_in-type_`,` _out-type_`)`)   | `fun/type(`eval(_env_, _in-type_)`,` `closure(extend(`_env_`,` &hellip;`),` _out-type_`))`
| eval(_env_, `fun/intro(`_out-expr_`)`)               | `fun/intro(closure(extend(`_env_`,` &hellip;`),` _out-expr_`))`
| eval(_env_, `fun/elim(`_head-expr_`,` _in-expr_`)`)  | fun-elim(eval(_env_, _head-expr_), eval(_env_, _in-expr_))

| fun-elim(_core.value_, _core.value_)              | _core.value_
| ------------------------------------------------- | -----------------------------------------
| fun-elim(`stuck(`_head_`,` _spine_`)`, _in-expr_) | &hellip;
| fun-elim(`fun/intro(`_out-expr_`)`, _in-expr_)    | &hellip;

## Elaboration

_elab.context_ ::=\
&emsp;| _elab.context_`,` _name_ `:` _core.value_ `=` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document lookup, checking and synthesis

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
    - [x] error formats
    - [x] record formats
      - [ ] conditional field sugar
      - [ ] skipped fields
    - [ ] map formats
    - [x] numeric formats
    - [x] array formats
  - [ ] type refinements
  - [ ] match expressions
  - [ ] patterns
    - [x] wildcard patterns
    - [x] named patterns
    - [ ] annotated patterns
    - [ ] numeric literal patterns
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
