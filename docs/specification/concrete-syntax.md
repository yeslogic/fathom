# Concrete syntax

This section describes the concrete, textual representation of the data
description language. This how most users will interact with data descriptions.

## Lexical syntax

### Whitespace and comments

> <sub>Grammar:<sub>
>
> _horizontal-tab_ ::=\
> &emsp;|&ensp;U+0009
>
> _line-feed_ ::=\
> &emsp;|&ensp;U+000A
>
> _vertical-tab_ ::=\
> &emsp;|&ensp;U+000B
>
> _form-feed_ ::=\
> &emsp;|&ensp;U+000C
>
> _carriage-return_ ::=\
> &emsp;|&ensp;U+000D
>
> _next-line_ ::=\
> &emsp;|&ensp;U+0085
>
> _left-to-right-mark_ ::=\
> &emsp;|&ensp;U+200E
>
> _right-to-left-mark_ ::=\
> &emsp;|&ensp;U+200F
>
> _line-separator_ ::=\
> &emsp;|&ensp;U+2028
>
> _paragraph-separator_ ::=\
> &emsp;|&ensp;U+2029
>
> _line-break_ ::=\
> &emsp;|&ensp;_line-feed_\
> &emsp;|&ensp;_carriage-return_\
> &emsp;|&ensp;_carriage-return_ _line-feed_
>
> _comment-text_ ::=\
> &emsp;|&ensp;Any Unicode scalar value except _line-feed_ or _carriage-return_
>
> _comment_ ::=\
> &emsp;|&ensp;`//` (Any _comment-text_ not starting with `/`) _line-break_
>
> _doc-comment_ ::=\
> &emsp;|&ensp;`///` _comment-text_ _line-break_
>
> _white-space_ ::=\
> &emsp;|&ensp;_horizontal-tab_\
> &emsp;|&ensp;_comment_\
> &emsp;|&ensp;_vertical-tab_\
> &emsp;|&ensp;_form-feed_\
> &emsp;|&ensp;_line-break_\
> &emsp;|&ensp;_next-line_\
> &emsp;|&ensp;_left-to-right-mark_\
> &emsp;|&ensp;_right-to-left-mark_\
> &emsp;|&ensp;_line-separator_\
> &emsp;|&ensp;_paragraph-separator_

### Keywords and identifiers

> <sub>Grammar:<sub>
>
> _keyword_ ::=\
> &emsp;|&ensp; `struct`
>
> _ident-or-keyword_ ::=\
> &emsp;|&ensp;[`a`-`z` `A`-`Z` `_`] [`a`-`z` `A`-`Z` `0`-`9` `_`]<sup>\*</sup>
>
> _ident_ ::=\
> &emsp;|&ensp;Any _ident-or-keyword_ except _keyword_

### Punctuation

> <sub>Grammar:<sub>
>
> _punctuation_ ::=\
> &emsp;|&ensp;`{`\
> &emsp;|&ensp;`}`\
> &emsp;|&ensp;`:`\
> &emsp;|&ensp;`,`

### Tokens

> <sub>Grammar:<sub>
>
> _token_ ::=\
> &emsp;|&ensp;_white-space_\
> &emsp;|&ensp;_doc-comment_\
> &emsp;|&ensp;_keyword_\
> &emsp;|&ensp;_ident_\
> &emsp;|&ensp;_punctuation_

## Syntax

The concrete syntax matches on any _token_, filtering out _white-space_ in the
process.

### Terms

> <sub>Grammar:<sub>
>
> _term_ ::=\
> &emsp;|&ensp;_ident_

### Items

#### Structure type definitions

Structure types are used to describe ordered sequences of binary data.
They are defined using the `struct` keyword, for example:

```
struct Point3 {
    x : F32Be,
    y : F32Be,
    z : F32Be,
}
```

Structures are composite types that are have a name and a list of fields. The
fields within a structure must have unique names.

> <sub>Grammar:<sub>
>
> _struct-type-field_ ::=\
> &emsp;|&ensp;_doc-comment_<sup>?</sup> _ident_ `:` _term_
>
> _struct-type-fields_ ::=\
> &emsp;|&ensp;(_struct-type-field_ `,`)<sup>\*</sup> _struct-type-field_<sup>?</sup>
>
> _struct-type-definition_ ::=\
> &emsp;|&ensp;`struct` _ident_ `{` _struct-type-fields_ `}`

### Modules

Modules are lists of zero-or-more definitions. Definitions within a module must have unique names.

> <sub>Grammar:<sub>
>
> _item_ ::=\
> &emsp;|&ensp;_struct-type-definition_
>
> _module_ ::=\
> &emsp;|&ensp;_item_<sup>\*</sup>
