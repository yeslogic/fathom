# Core theory

The binary data description language can be represented in a small dependently
typed language. We describe this here.

## Contents

- [Syntax](#syntax)
    - [Labels](#labels)
    - [Primitive types](#primitive-types)
    - [Sorts](#sorts)
    - [Terms](#terms)
    - [Values](#values)
    - [Modules](#modules)
    - [Contexts](#contexts)
- [Rules](#rules)
    - [Operational semantics](#operational-semantics)
        - [Evaluation](#evaluation)
        - [Readback](#readback)
    - [Typing](#typing)
        - [Checking](#checking)
        - [Synthesis](#synthesis)
        - [Structure type fields](#structure-type-fields)
        - [Modules](#modules)

## Syntax

### Labels

> <sub>Grammar:</sub>
>
> _name_ ::=\
> &emsp;|&ensp;(Any Unicode scalar value)<sup>+</sup>
>
> _label_ ::=\
> &emsp;|&ensp;_name_


### Primitive types

> <sub>Grammar:</sub>
>
> _primitive-type_ ::=\
> &emsp;|&ensp;`Bool`\
> &emsp;|&ensp;`Int`\
> &emsp;|&ensp;`F32`\
> &emsp;|&ensp;`F64`\
> &emsp;|&ensp;`U8`\
> &emsp;|&ensp;`U16Le`\
> &emsp;|&ensp;`U16Be`\
> &emsp;|&ensp;`U32Le`\
> &emsp;|&ensp;`U32Be`\
> &emsp;|&ensp;`U64Le`\
> &emsp;|&ensp;`U64Be`\
> &emsp;|&ensp;`S8`\
> &emsp;|&ensp;`S16Le`\
> &emsp;|&ensp;`S16Be`\
> &emsp;|&ensp;`S32Le`\
> &emsp;|&ensp;`S32Be`\
> &emsp;|&ensp;`S64Le`\
> &emsp;|&ensp;`S64Be`\
> &emsp;|&ensp;`F32Le`\
> &emsp;|&ensp;`F32Be`\
> &emsp;|&ensp;`F64Le`\
> &emsp;|&ensp;`F64Be`

### Sorts

> <sub>Grammar:</sub>
>
> _sort_ ::=\
> &emsp;|&ensp;`Kind`\
> &emsp;|&ensp;`Type`

### Terms

> <sub>Grammar:</sub>
>
> _term_ ::=\
> &emsp;|&ensp;`item` _name_\
> &emsp;|&ensp;_term_ `:` _term_\
> &emsp;|&ensp;_sort_\
> &emsp;|&ensp;_primitive-type_

### Values

> <sub>Grammar:</sub>
>
> _value_ ::=\
> &emsp;|&ensp;`item` _label_\
> &emsp;|&ensp;_sort_\
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
> &emsp;|&ensp;_label_ `=` _term_ `;`\
> &emsp;|&ensp;`struct` _label_ `{` _struct-type-fields_ `}`
>
> _items_ ::=\
> &emsp;|&ensp;_item_<sup>\*</sup>
>
> _module_ ::=\
> &emsp;|&ensp;_items_

### Contexts

Contexts are records that allow us to accumulate contextual information across
sequences of syntactic elements during validation.

> <sub>Grammar:</sub>
>
> _term-context_ ::=\
> &emsp;|&ensp;`{` `items` (_label_ : _value_)<sup>\*</sup> `}`
>
> _field-context_ ::=\
> &emsp;|&ensp;`{` `items` (_label_ : _value_)<sup>\*</sup>\
> &emsp;&emsp;`,` `fields` _label_<sup>\*</sup>\
> &emsp;&emsp;`}`
>
> _item-context_ ::=\
> &emsp;|&ensp;`{` `items` (_label_ : _value_)<sup>\*</sup> `}`

_TODO: Well-formedness rules for contexts_

## Rules

### Operational Semantics

#### Evaluation

| eval( _term_ ) | _value_ |
| - | - |
| eval( `item` _label_ ) | `item` _label_ <!-- TODO: Lookup entry --> |
| eval( _term_<sub>0</sub> `:` _term_<sub>1</sub> ) | eval( _term_<sub>0</sub> ) |
| eval( _sort_ ) | _sort_ |
| eval( _primitive-type_ ) | _primitive-type_ |

#### Readback

| readback( _value_ ) | _term_ |
| - | - |
| readback( `item` _label_ ) | `item` _label_ |
| readback( _sort_ ) | _sort_ |
| readback( _primitive-type_ ) | _primitive-type_ |

### Typing

The typing rules are a simpler form of the elaboration rules.
Here we are only interested in checking the validity of an existing core module.

#### Checking

Checks the type of a term.

> <sub>Judgement form:</sub>
>
> _term-context_ ⊢ _term_ : _value_ check

-   ...

    > <sub>Inference rule:</sub>
    >
    > (if other rules are not applicable)
    >
    > - _term-context_ ⊢ _term_ synth ↝ _value_<sub>0</sub>
    > - readback( _value_<sub>0</sub> ) = readback( _value_<sub>1</sub> )
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _term_ : _value_<sub>1</sub> check

#### Synthesis

Synthesizes the type of a term.

> <sub>Judgement form:</sub>
>
> _term-context_ ⊢ _term_ synth ↝ _value_

-   Item references can be synthesized to _value_ if:

    -   (_label_ `:` _value_) is an element of _term-context_.`items`

    > <sub>Inference rule:</sub>
    >
    > - (_label_ `:` _value) ∈ _term-context_.`items`
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ `item` synth ↝ _value_

-   `Type` synthesizes to `Kind`.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ `Type` synth ↝ `Kind`

-   Annotated terms can be synthesized if:

    - _term_<sub>1</sub> synthesizes to a _sort_
    - _term_<sub>0</sub> checks against the evaluation result of _term_<sub>1</sub>

    > <sub>Inference rule:</sub>
    >
    > - _term-context_ ⊢ _term_<sub>1</sub> synth ↝ _sort_
    > - eval( _term_<sub>1</sub> ) = _value_
    > - _term-context_ ⊢ _term_<sub>0</sub> : _value_ check
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _term_<sub>0</sub> `:` _term_<sub>1</sub> synth ↝ _value_

-   All primitive types synthesize to `Type`.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _term-context_ ⊢ _primitive-type_ synth ↝ `Type`

#### Structure type fields

Validates that a sequence of type field declarations is well-formed.

> <sub>Judgement form:</sub>
>
> _field-context_ ⊢ _struct-type-fields_ struct

-   An empty sequence of fields can always be used to create a well-formed struct.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _field-context_ ⊢ ε struct

-   A sequence of struct fields can be used to define a well-formed struct if:

    -   the _label_ of the field is not an element of _field-context_.`fields`
    -   the _term_ is a well-formed type
    -   the rest of the _struct-type-fields_ are well-formed when checked with the _label_ added to _field-context_.`fields`

    > <sub>Inference rule:</sub>
    >
    > - _label_ ∉ _field-context_.`fields`
    > - `{` `items` _item-context_.`items` `}` ⊢ _term_ : `Type` check
    > - _field-context_, `fields` _label_ ⊢ _struct-type-fields_ struct
    > ----------------------------------------------------------------------------------------------
    > - _field-context_  ⊢ (_label_ `:` _term_) _struct-type-fields_ struct


#### Modules

Validates that a module a well-formed.

> <sub>Judgement form:</sub>
>
> _item-context_ ⊢ _module_ module

-   An empty sequence of items is always a well-formed module.

    > <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ ε module

-   A sequence of items with an alias definition on top is well formed if:

    -   the _label_ of the alias is not an element of _item-context_.`labels`
    -   the _term_ is a well-formed type
    -   the rest of the _items_ are well-formed when checked with the _label_ added to _item-context_.`labels`

    > <sub>Inference rule:</sub>
    >
    > - (_label_ `:` \_) ∉ _item-context_.`items`
    > - `{` `items` _item-context_.`items` `}` ⊢ _term_ synth ↝ _value_
    > - _item-context_, `items` (_label_ `:` _value_) ⊢ _items_ module
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (_label_ `=` _term_ `;`) _items_ module

-   A sequence of items with a struct type definition on top is well formed if:

    -   the _label_ of the struct is not an element of _item-context_.`items`
    -   the _struct-type-fields_ are well-formed in an empty field context
    -   the rest of the _items_ are well-formed when checked with the _label_ added to _item-context_.`items`

    > <sub>Inference rule:</sub>
    >
    > - (_label_ `:` \_) ∉ _item-context_.`items`
    > - `{` `items` _item-context_.`items` `,` `fields` ε `}` ⊢ _struct-type-fields_ struct
    > - _item-context_, `items` (_label_ `:` `Type`) ⊢ _items_ module
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ (`struct` _label_ `{` _struct-type-fields_ `}`) _items_ module
