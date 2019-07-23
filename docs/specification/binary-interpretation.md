# Binary Interpretation

This section describes how binary data descriptions can be naively interpreted
as binary encoders or decoders. In an actual implementation we would partially
evaluate this (presumably by hand) into an efficient compiler implementation.

## Syntax

### Bits

A stream of [bits](https://en.wikipedia.org/wiki/Bit) is a sequence of `1`s and `0`s:

> <sub>Grammar:</sub>
>
> _bit_ ::=\
> &emsp;|&ensp;`0`\
> &emsp;|&ensp;`1`
>
> _bits_ ::=\
> &emsp;|&ensp;_bit_<sup>\*</sup>

A stream of bits can be made up of [binary words](https://en.wikipedia.org/wiki/Word_(computer_architecture)):

> <sub>Grammar:</sub>
>
> _word8_ ::=\
> &emsp;|&ensp;_bit_<sup>8</sup>
>
> _word16_ ::=\
> &emsp;|&ensp;_bit_<sup>16</sup>
>
> _word32_ ::=\
> &emsp;|&ensp;_bit_<sup>32</sup>
>
> _word64_ ::=\
> &emsp;|&ensp;_bit_<sup>64</sup>

### Terms

> <sub>Grammar:</sub>
>
> _struct-field_ ::=\
> &emsp;|&ensp;_label_ `=` _term_
>
> _struct-fields_ ::=\
> &emsp;|&ensp;_struct-field_<sup>\*</sup>
>
> _term_ ::=\
> &emsp;|&ensp;_primitive.int_\
> &emsp;|&ensp;_primitive.float_\
> &emsp;|&ensp;`struct` `{` _struct-fields_ `}`

### Contexts

Contexts are records that allow us to accumulate contextual information across
sequences of syntactic elements during encoding and decoding.

> <sub>Grammar:</sub>
>
> _item-context_ ::=\
> &emsp;|&ensp;`{` `items` _core.item_<sup>\*</sup> `}`

## Binary decoder interpretation

### Decoding Terms

> <sub>Judgement form:</sub>
>
> _item-context_ ⊢ _bits_<sub>0</sub> : _core.term_ ↝ _term_, _bits_<sub>1</sub>

-   We can parse an item into a term if:

    -   there is an alias definition corresponding to _core.label_ in _item-context_.`items`
    -   we can parse the aliased _core.term_ into a _term_

    >  <sub>Inference rule:</sub>
    >
    > - (_core.label_ `=` _core.term_`;`) ∈ _item-context_.`items`
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.term_ ↝ _term_ , _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : `item` _core.label_ ↝ _term_ , _bits_<sub>1</sub>

-   We can parse an item into a structure value if:

    -   there is a structure definition corresponding to _core.label_ in _item-context_.`items`
    -   we can parse the _struct-type-fields_ into _struct-fields_

    >  <sub>Inference rule:</sub>
    >
    > - `struct` _core.label_ `{` _core.struct-type-fields_ `}` ∈ _item-context_.`items`
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.struct-type-fields_ ↝ _struct-fields_ , _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : `item` _core.label_ ↝ `struct` `{` _struct-fields_ `}` , _bits_<sub>1</sub>

-   We can parse a `U8` if there is a _word8_ available at the beginning of the
    input stream.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _word8_ _bits_ : `U8` ↝ TODO , _bits_

-   We can parse a `S8` if there is a _word8_ available at the beginning of the
    input stream.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _word8_ _bits_ : `S8` ↝ TODO , _bits_

-   TODO: rules for remaining primitives:
    - `U16Le`, `U16Be`, `U32Le`, `U32Be`, `U64Le`, `U64Be`
    - `S16Le`, `S16Be`, `S32Le`, `S32Be`, `S64Le`, `S64Be`
    - `F32Le`, `F32Be`, `F64Le`, `F64Be`

### Decoding structure fields

> <sub>Judgement form:</sub>
>
> _item-context_ ⊢ _bits_<sub>0</sub> : _core.struct-type-fields_ ↝ _struct-fields_, _bits_<sub>1</sub>

-   Empty structures do not consume any input.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_ : ε ↝ ε , _bits_

-   If we see a field on top, parse the top field, then parse the rest of the
    fields.

    >  <sub>Inference rule:</sub>
    >
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.term_ ↝ _term_, _bits_<sub>1</sub>
    > - _item-context_ ⊢ _bits_<sub>1</sub> : _core.struct-type-fields_ ↝ _struct-fields_, _bits_<sub>2</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : (_core.label_ `:` _core.term_) _core.struct-type-fields_\
    >   ↝ (_core.label_ `:` _term_) _struct-fields_, _bits_<sub>2</sub>

### Decoding module-level items

This shows how we can decode an item in a module, given the _core.label_.

> <sub>Judgement form:</sub>
>
> ⊢ _bits_<sub>0</sub> : _core.module_ . _core.label_ ↝ _term_, _bits_<sub>1</sub>

-   If we see an alias at the top of a module with a label that matches the
    one we are looking for, then we parse the bits using the fie specified in
    the structure definition.

    >  <sub>Inference rule:</sub>
    >
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.term_ ↝ _term_, _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : (_core.label_ `=` _core.term_ `;`) _core.items_ . _core.label_
    >   ↝ _term_, _bits_<sub>1</sub>

-   If we see a structure type at the top of a module with a label that matches
    the one we are looking for, then we parse the bits using the fie specified
    in the structure definition.

    >  <sub>Inference rule:</sub>
    >
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.struct-type-fields_ ↝ _struct-fields_, _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : (`struct` _core.label_ `{` _core.struct-type-fields_ `}`) _core.items_ . _core.label_\
    >   ↝ `struct` `{` _struct-fields_ `}`, _bits_<sub>1</sub>

-   If we see an item at the top of a module with a label that does not match
    the one we are looking for, then we should look for the item in the rest
    of the module.

    >  <sub>Inference rule:</sub>
    >
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.items_ . _core.label_<sub>1</sub> ↝ _term_, _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : (_core.label_<sub>0</sub> `=` _core.term_ `;`)
    >   _core.items_ . _core.label_<sub>1</sub> ↝ _term_, _bits_<sub>1</sub>

    >  <sub>Inference rule:</sub>
    >
    > - _item-context_ ⊢ _bits_<sub>0</sub> : _core.items_ . _core.label_<sub>1</sub> ↝ _term_, _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - _item-context_ ⊢ _bits_<sub>0</sub> : (`struct` _core.label_<sub>0</sub> `{` _core.struct-type-fields_ `}`)
    >   _core.items_ . _core.label_<sub>1</sub> ↝ _term_, _bits_<sub>1</sub>

## Binary encoder interpretation

TODO
