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
> _field-context_ ::=\
> &emsp;|&ensp;`{` `labels` _concrete.ident_<sup>\*</sup> `}`
>
> _item-context_ ::=\
> &emsp;|&ensp;`{` `labels` _concrete.ident_<sup>\*</sup> `}`

_TODO: Well-formedness rules for contexts_

## Types

> <sub>Judgement form:</sub>
>
> ⊢ _concrete.term_ type ↝ _core.term_

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

-   _TODO: Description of struct item elaboration_

    > <sub>Inference rule:</sub>
    >
    > - _concrete.ident_ ∉ _field-context_.`labels`
    > - ⊢ _concrete.term_ type ↝ _core.term_
    > - _field-context_, `labels` _concrete.ident_ ⊢ _concrete.struct-type-fields_ struct
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

-   _TODO: Description of module elaboration_

    > <sub>Inference rule:</sub>
    >
    > - _concrete.ident_ ∉ _item-context_.`labels`
    > - `{` `labels` ε `}` ⊢ _concrete.struct-type-fields_ struct ↝ _core.struct-type-fields_
    > - _item-context_, `labels` _concrete.ident_ ⊢ _concrete.items_ module ↝ _core.items_
    > - _concrete.ident_ = _core.label_
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (`struct` _concrete.ident_ `{` _concrete.struct-type-fields_ `}`) _concrete.items_ module\
    >   ↝ (`struct` _core.label_ `{` _core.struct-type-fields_ `}`) _core.items_
