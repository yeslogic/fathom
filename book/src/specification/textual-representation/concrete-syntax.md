# Concrete Syntax

The surface syntax matches on any _token_, filtering out _white-space_ in the
process.

## Terms

> <sub>Grammar:</sub>
>
> _term_ ::=\
> &emsp;|&ensp;_term-atomic_\
> &emsp;|&ensp;_term-atomic_ `:` _term_
>
> _term-atomic_ ::=\
> &emsp;|&ensp;`(` _term_ `)`\
> &emsp;|&ensp;_ident_

## Items

### Constant definitions

Constant definitions are used to give names to terms that can be later used in
other places in the binary description. For example:

```fathom
const Byte = U8;
```

Constants are bound using the `const` keyword followed by an
identifier and the term that they are assigned to, terminated by a
semicolon:

> <sub>Grammar:</sub>
>
> _constant-definition_ ::=\
> &emsp;|&ensp;_doc-comment_<sup>?</sup> `const` _ident_ `=` _term_ `;`

### Structure type definitions

Structure types are used to describe ordered sequences of binary data.
They are defined using the `struct` keyword, for example:

```fathom
struct Point3 {
    x : F32Be,
    y : F32Be,
    z : F32Be,
}
```

Structures are composite types that have a name and a list of fields. The
fields within a structure must have unique names.

> <sub>Grammar:</sub>
>
> _struct-type-field_ ::=\
> &emsp;|&ensp;_doc-comment_<sup>?</sup> _ident_ `:` _term_
>
> _struct-type-fields_ ::=\
> &emsp;|&ensp;(_struct-type-field_ `,`)<sup>\*</sup> _struct-type-field_<sup>?</sup>
>
> _struct-type-definition_ ::=\
> &emsp;|&ensp;_doc-comment_<sup>?</sup> `struct` _ident_ `{` _struct-type-fields_ `}`

## Modules

Modules are lists of zero-or-more definitions. Definitions within a module must have unique names.

> <sub>Grammar:</sub>
>
> _item_ ::=\
> &emsp;|&ensp;_constant-definition_\
> &emsp;|&ensp;_struct-type-definition_
>
> _module_ ::=\
> &emsp;|&ensp;_item_<sup>\*</sup>
