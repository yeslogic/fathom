# Elaboration

This section describes how the concrete syntax of the data description language
is 'elaborated' into the core type theory.
The intention is that the resulting core terms and modules are well-formed with
respect to the validation rules described in the core type theory, but we make
no claims that this is _actually_ the case.

## Contents

- [Syntax](#syntax)
    - [Contexts](#contexts)
- [Rules](#rules)
    - [Checking](#checking)
    - [Synthesis](#synthesis)
    - [Structure type fields](#structure-type-fields)
    - [Modules](#modules)

## Syntax

### Contexts

Contexts are records that allow us to accumulate contextual information across
sequences of syntactic elements during elaboration.

> <sub>Grammar:</sub>
>
> _term-context_ ::=\
> &emsp;|&ensp;`{` `items` _concrete.ident_<sup>\*</sup> `}`
>
> _field-context_ ::=\
> &emsp;|&ensp;`{` `items` _concrete.ident_<sup>\*</sup>\
> &emsp;&emsp;`,` `fields` _concrete.ident_<sup>\*</sup>\
> &emsp;&emsp;`}`
>
> _item-context_ ::=\
> &emsp;|&ensp;`{` `items` _concrete.ident_<sup>\*</sup> `}`

_TODO: Well-formedness rules for contexts_

## Rules

### Checking

Checks the type of a term.

> <sub>Judgement form:</sub>
>
> _term-context_ ⊢ _term_ : _value_ check ↝ _core.term_

-   ...

    > <sub>Inference rule:</sub>
    >
    > - _term-context_ ⊢ _term_ : _value_ check ↝ _core.term_
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ `(` _term_ `)` : _value_ check ↝ _core.term_

-   ...

    > <sub>Inference rule:</sub>
    >
    > (if other rules are not applicable)
    >
    > - _term-context_ ⊢ _term_ synth ↝ _core.term_ : _value_<sub>0</sub>
    > - readback( _value_<sub>0</sub> ) = readback( _value_<sub>1</sub> )
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _term_ : _value_<sub>1</sub> check ↝ _core.term_

## Synthesis

> <sub>Judgement form:</sub>
>
> _term-context_ ⊢ _concrete.term_ synth ↝ _core.term_ : _core.value_

-   ...

    > <sub>Inference rule:</sub>
    >
    > - _term-context_ ⊢ _term_ synth ↝ _core.term_ : _value_
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ `(` _term_ `)` synth ↝ _core.term_ : _value_

-   Variables elaborate to item references of type _value_ if:

    -   (_concrete.ident_ `:` _value_) is an element of _term-context_.`items`

    > <sub>Inference rule:</sub>
    >
    > - (_concrete.ident_ `:` _value_) ∈ _term-context_.`items`
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _concrete.ident_ type synth ↝ `item` _concrete.ident_ : _value_

-   Variables elaborate to `Type` if:

    -   `Type` is not an element of _term-context_.`items`

    > <sub>Inference rule:</sub>
    >
    > - `alias` `Type` `:` \_ ∉ _term-context_.`items` ∨ `struct` `Type` ∉ _term-context_.`items`
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ `Type` synth ↝ `Type` : `Kind`

-   Annotated terms can be synthesized if:

    > <sub>Inference rule:</sub>
    >
    > - _term-context_ ⊢ _concrete.term_<sub>1</sub> synth ↝ _core.term_<sub>1</sub> : _sort_
    > - eval( _core.term_<sub>1</sub> ) = _core.value_
    > - _term-context_ ⊢ _concrete.term_<sub>0</sub> : _core.value_ check ↝ _core.term_<sub>0</sub>
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _concrete.term_<sub>0</sub> `:` _concrete.term_<sub>1</sub> synth\
    >   ↝ (_core.term_<sub>0</sub> `:` _core.term_<sub>1</sub>) : _core.value_

-   _TODO: Description of type synthesis for primitive types_

## Structure type fields

> <sub>Judgement form:</sub>
>
> _field-context_ ⊢ _concrete.struct-type-items_ struct ↝ _core.struct-type-items_

-   An empty sequence of fields can always elaborate to an empty sequence of fields.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _field-context_ ⊢ ε struct ↝ ε

-   _TODO: Description of structure type field elaboration_

    > <sub>Inference rule:</sub>
    >
    > - _concrete.ident_ ∉ _field-context_.`fields`
    > - `{` `items` _field-context_.`items` `}` ⊢ _concrete.term_ : `Type` check ↝ _core.term_
    > - _field-context_, `fields` _concrete.ident_ ⊢ _concrete.struct-type-fields_ struct
    >   ↝ _core.struct-type-fields_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _field-context_  ⊢ (_concrete.ident_ `:` _concrete.term_) _concrete.struct-type-fields_ struct\
    >   ↝ (_core.label_ `:` _core.term_) _core.struct-type-fields_

## Modules

> <sub>Judgement form:</sub>
>
> _item-context_ ⊢ _concrete.module_ module ↝ _core.module_

-   An empty sequence of items can always elaborate to an empty sequence of items.

    > <sub>Inference rule:</sub>
    >
    > ---
    > - _item-context_ ⊢ ε module ↝ ε

-   _TODO: Description of alias elaboration_

    > <sub>Inference rule:</sub>
    >
    > - (_concrete.ident_ `:` \_) ∉ _item-context_.`items`
    > - `{` `items` _item-context_.`items` `}` ⊢ _concrete.term_ synth ↝ _core.struct-type-fields_ : _core.value_
    > - _item-context_, `items` (_concrete.ident_ `:` _core.value_) ⊢ _concrete.items_ module ↝ _core.items_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (_concrete.ident_ `=` _concrete.term_ `;`) _concrete.items_ module\
    >   ↝ (_core.label_ `=` _concrete.term_ `;`) _core.items_


-   _TODO: Description of structure type elaboration_

    > <sub>Inference rule:</sub>
    >
    > - (_concrete.ident_ `:` \_) ∉ _item-context_.`items`
    > - `{` `items` _item-context_.`items` `,` `fields` ε `}` ⊢ _concrete.struct-type-fields_ struct
    >   ↝ _core.struct-type-fields_
    > - _item-context_, `items` (_concrete.ident_ `:` `Type`) ⊢ _concrete.items_ module ↝ _core.items_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (`struct` _concrete.ident_ `{` _concrete.struct-type-fields_ `}`) _concrete.items_ module\
    >   ↝ (`struct` _core.label_ `{` _core.struct-type-fields_ `}`) _core.items_
