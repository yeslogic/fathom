# Core theory

The binary data description language can be represented in a small dependently
typed language. We describe this here.

## Syntax

### Labels

> <sub>Grammar:</sub>
>
> _label_ ::=\
> &emsp;|&ensp;(Any Unicode scalar value)<sup>+</sup>

### Primitive types

> <sub>Grammar:</sub>
>
> _primitive-type-unsigned_ ::=\
> &emsp;|&ensp;`U8`\
> &emsp;|&ensp;`U16Le`\
> &emsp;|&ensp;`U16Be`\
> &emsp;|&ensp;`U32Le`\
> &emsp;|&ensp;`U32Be`\
> &emsp;|&ensp;`U64Le`\
> &emsp;|&ensp;`U64Be`
>
> _primitive-type-signed_ ::=\
> &emsp;|&ensp;`S8`\
> &emsp;|&ensp;`S16Le`\
> &emsp;|&ensp;`S16Be`\
> &emsp;|&ensp;`S32Le`\
> &emsp;|&ensp;`S32Be`\
> &emsp;|&ensp;`S64Le`\
> &emsp;|&ensp;`S64Be`
>
> _primitive-type-float_ ::=\
> &emsp;|&ensp;`F32Le`\
> &emsp;|&ensp;`F32Be`\
> &emsp;|&ensp;`F64Le`\
> &emsp;|&ensp;`F64Be`
>
> _primitive-type_ ::=\
> &emsp;|&ensp;_primitive-type-unsigned_\
> &emsp;|&ensp;_primitive-type-signed_\
> &emsp;|&ensp;_primitive-type-float_

### Terms

> <sub>Grammar:</sub>
>
> _term_ ::=\
> &emsp;|&ensp;_primitive-type_

### Modules

> <sub>Grammar:</sub>
>
> _struct-type-field_ ::=\
> &emsp;|&ensp;_label_ `:` _term_
>
> _struct-type-fields_ ::=\
> &emsp;|&ensp;_struct-type-field_<sup>\*</sup>
>
> _item_ ::=\
> &emsp;|&ensp;`struct` _label_ `{` _struct-type-fields_ `}`
>
> _items_ ::=\
> &emsp;|&ensp;_item_<sup>\*</sup>
>
> _module_ ::=\
> &emsp;|&ensp;_items_

## Validation

The validation rules are a simpler form of the elaboration rules.
Here we are only interested in checking the validity of an existing core module.

### Contexts

Contexts are records that allow us to accumulate contextual information across
sequences of syntactic elements during validation.

> <sub>Grammar:</sub>
>
> _field-context_ ::=\
> &emsp;|&ensp;`{` `labels` _label_<sup>\*</sup> `}`
>
> _item-context_ ::=\
> &emsp;|&ensp;`{` `labels` _label_<sup>\*</sup> `}`

_TODO: Well-formedness rules for contexts_

### Types

Validates that a term is a well-formed type.

> <sub>Judgement form:</sub>
>
> `⊢` _term_ `type`

-   All primitive types are well-formed types.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - `⊢` _primitive-type_ `type`

### Structure type fields

Validates that a sequence of type field declarations is well-formed.

> <sub>Judgement form:</sub>
>
> _field-context_ `⊢` _struct-type-fields_ `struct`

-   An empty sequence of fields can always be used to create a well-formed struct.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _field-context_ `⊢` ε `struct`

-   A sequence of struct fields can be used to define a well-formed struct if:

    -   the _label_ of the field is not an element of _field-context_.`labels`
    -   the _term_ is a well-formed type
    -   the rest of the _struct-type-fields_ are well-formed when checked with the _label_ added to _field-context_.`labels`

    > <sub>Inference rule:</sub>
    >
    > - _label_ ∉ _field-context_.`labels`
    > - `⊢` _term_ `type`
    > - _field-context_, `labels` _label_ `⊢` _struct-type-fields_ `struct`
    > ----------------------------------------------------------------------------------------------
    > - _field-context_  `⊢` (_label_ `:` _term_) _struct-type-fields_ `struct`


### Modules

Validates that a module a well-formed.

> <sub>Judgement form:</sub>
>
> _item-context_ `⊢` _module_ `module`

-   An empty sequence of items is always a well-formed module.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ `⊢` ε `module`

-   A sequence of items with a struct type definition on top is well formed if:

    -   the _label_ of the struct is not an element of _item-context_.`labels`
    -   the _struct-type-fields_ are well-formed in an empty field context
    -   the rest of the _items_ are well-formed when checked with the _label_ added to _item-context_.`labels`

    > <sub>Inference rule:</sub>
    >
    > - _label_ ∉ _item-context_.`labels`
    > - `{` `labels` ε `}` `⊢` _struct-type-fields_ `struct`
    > - _item-context_, `labels` _label_ `⊢` _items_ `module`
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ `⊢` (`struct` _label_ `{` _struct-type-fields_ `}`) _items_ `module`
