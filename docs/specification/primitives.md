# Primitives

Here we describe primitive constants that are required to be supported by the
host language.

## Integers

### Big integers

We use abstract integers in the host language:

> <sub>Grammar:</sub>
>
> _int_ ::=\
> &emsp;|&ensp;&hellip;\
> &emsp;|&ensp;-1\
> &emsp;|&ensp;0\
> &emsp;|&ensp;1\
> &emsp;|&ensp;&hellip;

Note: these integers may not map exactly to a host language's natively supported primitive types,
for example `i8` &hellip; `i128` and `u8` &hellip; `u128` in the case of Rust.
Binary data descriptions will be expected to constrain these to reasonable ranges
in order to ensure that they can be compiled to an efficient parser in the target language.

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
