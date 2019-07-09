# Primitives

Here we describe primitive constants that are required to be supported by the
host language.

## Integers

Signed or unsigned numbers.

> <sub>Grammar:</sub>
>
> unsigned(_bits_) ::=\
> &emsp;|&ensp;0\
> &emsp;|&ensp;1\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;2<sup>_bits_</sup> - 1
>
> signed(_bits_) ::=\
> &emsp;|&ensp;-2<sup>_bits_ - 1</sup>\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;-1\
> &emsp;|&ensp;0\
> &emsp;|&ensp;1\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;2<sup>_bits_</sup> - 1

Unsigned and signed integers in the host language can be 8, 16, 32, or 64 bits
in width:

> <sub>Grammar:</sub>
>
> _unsigned_ ::=\
> &emsp;|&ensp;unsigned(8)\
> &emsp;|&ensp;unsigned(16)\
> &emsp;|&ensp;unsigned(32)\
> &emsp;|&ensp;unsigned(64)
>
> _signed_ ::=\
> &emsp;|&ensp;signed(8)\
> &emsp;|&ensp;signed(16)\
> &emsp;|&ensp;signed(32)\
> &emsp;|&ensp;signed(64)

## Floating-point

IEEE 754 floating point numbers:

> <sub>Grammar:</sub>
>
> float-normal(_bits_) ::=\
> &emsp;|&ensp;TODO
>
> float-subnormal(_bits_) ::=\
> &emsp;|&ensp;TODO
>
> float-nan(_bits_) ::=\
> &emsp;|&ensp;TODO
>
> float-magnitude(_bits_) ::=\
> &emsp;|&ensp;∞\
> &emsp;|&ensp;float-normal(_bits_)\
> &emsp;|&ensp;float-subnormal(_bits_)\
> &emsp;|&ensp;float-nan(_bits_)
>
> float-sign ::=\
> &emsp;|&ensp;+\
> &emsp;|&ensp;-
>
> float(_bits_) ::=\
> &emsp;|&ensp;float-sign float-magnitude(_bits_)

Floating point numbers in the host language can either be 32 or 64 bits in width:

> <sub>Grammar:</sub>
>
> _float_ ::=\
> &emsp;|&ensp;float(32)\
> &emsp;|&ensp;float(64)
