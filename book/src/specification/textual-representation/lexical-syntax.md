# Lexical Syntax

## Whitespace and comments

> <sub>Grammar:</sub>
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

## Keywords and identifiers

> <sub>Grammar:</sub>
>
> _keyword_ ::=\
> &emsp;|&ensp; `const`
> &emsp;|&ensp; `struct`
>
> _ident-or-keyword_ ::=\
> &emsp;|&ensp;\[`a`-`z` `A`-`Z` `_`\] \[`a`-`z` `A`-`Z` `0`-`9` `_`\]<sup>\*</sup>
>
> _ident_ ::=\
> &emsp;|&ensp;Any _ident-or-keyword_ except _keyword_

## Punctuation

> <sub>Grammar:</sub>
>
> _punctuation_ ::=\
> &emsp;|&ensp;`{`\
> &emsp;|&ensp;`}`\
> &emsp;|&ensp;`(`\
> &emsp;|&ensp;`)`\
> &emsp;|&ensp;`:`\
> &emsp;|&ensp;`,`\
> &emsp;|&ensp;`=`\
> &emsp;|&ensp;`;`

## Tokens

> <sub>Grammar:</sub>
>
> _token_ ::=\
> &emsp;|&ensp;_white-space_\
> &emsp;|&ensp;_doc-comment_\
> &emsp;|&ensp;_keyword_\
> &emsp;|&ensp;_ident_\
> &emsp;|&ensp;_punctuation_
