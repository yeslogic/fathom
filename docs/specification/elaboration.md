# Elaboration

This section describes how the concrete syntax of the data description language
is 'elaborated' into the core type theory.
The intention is that the resulting core terms and modules are well-formed with
respect to the validation rules described in the core type theory, but we make
no claims that this is _actually_ the case.

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

## Types

> <sub>Judgement form:</sub>
>
> _term-context_ ⊢ _concrete.term_ type ↝ _core.term_

-   Variables elaborate to item references if:

    -   _concrete.ident_ is an element of _term-context_.`items`

    > <sub>Inference rule:</sub>
    >
    > - _concrete.ident_ ∈ _term-context_.`items`
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _concrete.ident_ type ↝ `item` _concrete.ident_

_TODO: Description of term elaboration for primitive types_

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
    > - `{` `items` _field-context_.`items` `}` ⊢ _concrete.term_ type ↝ _core.term_
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
    > - _concrete.ident_ ∉ _item-context_.`labels`
    > - ⊢ _concrete.term_ type ↝ _core.struct-type-fields_
    > - _item-context_, `labels` _concrete.ident_ ⊢ _concrete.items_ module ↝ _core.items_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (_concrete.ident_ `=` _concrete.term_ `;`) _concrete.items_ module\
    >   ↝ (_core.label_ `=` _concrete.term_ `;`) _core.items_


-   _TODO: Description of structure type elaboration_

    > <sub>Inference rule:</sub>
    >
    > - _concrete.ident_ ∉ _item-context_.`items`
    > - `{` `items` _item-context_.`items` `,` `fields` ε `}` ⊢ _concrete.struct-type-fields_ struct
    >   ↝ _core.struct-type-fields_
    > - _item-context_, `items` _concrete.ident_ ⊢ _concrete.items_ module ↝ _core.items_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (`struct` _concrete.ident_ `{` _concrete.struct-type-fields_ `}`) _concrete.items_ module\
    >   ↝ (`struct` _core.label_ `{` _core.struct-type-fields_ `}`) _core.items_
