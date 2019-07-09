# Binary Interpretation

This section describes how binary data descriptions can be naively interpreted
as binary encoders or decoders. In an actual implementation we would partially
evaluate this (presumably by hand) into an efficient compiler implementation.

## Syntax

### Bits

A stream of bits is a sequence of `1`s and `0`s:

> <sub>Grammar:</sub>
>
> _bit_ ::=\
> &emsp;|&ensp;`0`\
> &emsp;|&ensp;`1`
>
> _bits_ ::=\
> &emsp;|&ensp;_bit_<sup>\*</sup>

A stream of bits can be made up of sequences of bytes:

> <sub>Grammar:</sub>
>
> _byte1_ ::=\
> &emsp;|&ensp;_bit_<sup>8</sup>
>
> _byte2_ ::=\
> &emsp;|&ensp;_bit_<sup>16</sup>
>
> _byte4_ ::=\
> &emsp;|&ensp;_bit_<sup>32</sup>
>
> _byte8_ ::=\
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
> &emsp;|&ensp;_primitive.unsigned_\
> &emsp;|&ensp;_primitive.signed_\
> &emsp;|&ensp;_primitive.float_\
> &emsp;|&ensp;`struct` `{` _struct-fields_ `}`

## Binary decoder interpretation

### Decoding Terms

> <sub>Judgement form:</sub>
>
> `⊢` _bits_<sub>0</sub> `:` _core.term_ `~>` _term_ `,` _bits_<sub>1</sub>

-   We can parse a `U8` if there is a _byte1_ available at the beginning of the
    input stream.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > `⊢` _byte1_ _bits_ `:` `U8` `~>` ??? `,` _bits_

-   We can parse a `S8` if there is a _byte1_ available at the beginning of the
    input stream.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > `⊢` _byte1_ _bits_ `:` `S8` `~>` ??? `,` _bits_

-   TODO: rules for remaining primitives:
    - `U16Le`, `U16Be`, `U32Le`, `U32Be`, `U64Le`, `U64Be`
    - `S16Le`, `S16Be`, `S32Le`, `S32Be`, `S64Le`, `S64Be`
    - `F32Le`, `F32Be`, `F64Le`, `F64Be`

### Decoding structure fields

> <sub>Judgement form:</sub>
>
> `⊢` _bits_<sub>0</sub> `:` _core.struct-type-fields_ `~>` _struct-fields_ `,` _bits_<sub>1</sub>

-   Empty structures do not consume any input.

    >  <sub>Inference rule:</sub>
    >
    > ----------------------------------------------------------------------------------------------
    > - `⊢` _bits_ `:` ε `~>` ε `,` _bits_

-   If we see a field on top, parse the top field, then parse the rest of the
    fields.

    >  <sub>Inference rule:</sub>
    >
    > - `⊢` _bits_<sub>0</sub> `:` _core.term_ `~>` _term_ `,` _bits_<sub>1</sub>
    > - `⊢` _bits_<sub>1</sub> `:` _core.struct-type-fields_ `~>` _struct-fields_ `,` _bits_<sub>2</sub>
    > ----------------------------------------------------------------------------------------------
    > - `⊢` _bits_<sub>0</sub> `:` (_core.label_ `:` _core.term_) _core.struct-type-fields_
    >   `~>` (_core.label_ `:` _term_) _struct-fields_ `,` _bits_<sub>2</sub>

### Decoding module-level items

This shows how we can decode an item in a module, given the _core.label_.

> <sub>Judgement form:</sub>
>
> `⊢` _bits_<sub>0</sub> `:` _core.module_ `.` _core.label_ `~>` _term_ `,` _bits_<sub>1</sub>

-   If we see a structure at the top of a module with a label that does not
    match the one we are looking for, then we should look in the rest of the
    module.

    >  <sub>Inference rule:</sub>
    >
    > - `⊢` _bits_<sub>0</sub> `:` _core.items_ `.` _core.label_<sub>1</sub> `~>` _term_ `,` _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - `⊢` _bits_<sub>0</sub> `:` (`struct` _core.label_<sub>0</sub> `{` _core.struct-type-fields_ `}`)
    >   _core.items_ `.` _core.label_<sub>1</sub> `~>` _term_ `,` _bits_<sub>1</sub>

-   If we see a structure at the top of a module with a label that matches the
    one we are looking for, then we parse the bits using the fie specified in
    the structure definition.

    >  <sub>Inference rule:</sub>
    >
    > - `⊢` _bits_<sub>0</sub> `:` _core.struct-type-fields_ `~>` _struct-fields_ `,` _bits_<sub>1</sub>
    > ----------------------------------------------------------------------------------------------
    > - `⊢` _bits_<sub>0</sub> `:` (`struct` _core.label_ `{` _core.struct-type-fields_ `}`) _core.items_ `.` _core.label_\
    >   `~>` `struct` `{` _struct-fields_ `}` `,` _bits_<sub>1</sub>

## Binary encoder interpretation

TODO
