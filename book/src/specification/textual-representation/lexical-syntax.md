# Lexical Syntax

## Characters

The textual surface language assigns meaning to a source string,
which consists of a sequence of _Unicode scalar values_
(as defined in Section 3.4 of [the Unicode Standard](https://www.unicode.org/versions/latest/)),
terminated with a virtual end-of-file symbol, `"\0"`:

```term
unicode-scalar-value ::=
  | "\u{00}" ... "\u{D7FF}"
  | "\u{E000}" ... "\u{10FFF}"

source ::=
  | unicode-scalar-value* "\0"
```

For convenience, we define a number of special values within the above `unicode-scalar-value` definition:

```text
horizontal-tab        ::= "\u{0009}"
line-feed             ::= "\u{000A}"
vertical-tab          ::= "\u{000B}"
form-feed             ::= "\u{000C}"
carriage-return       ::= "\u{000D}"
next-line             ::= "\u{0085}"
left-to-right-mark    ::= "\u{200E}"
right-to-left-mark    ::= "\u{200F}"
line-separator        ::= "\u{2028}"
paragraph-separator   ::= "\u{2029}"
```

## Tokens

```term
token ::=
  | white-space
  | doc-comment
  | comment
  | keyword
  | name
  | numeric-literal
  | punctuation
```

## Whitespace

```term
line-break ::=
  | line-feed
  | carriage-return
  | carriage-return line-feed
  | "\0"

white-space ::=
  | horizontal-tab
  | comment
  | vertical-tab
  | form-feed
  | line-break
  | next-line
  | left-to-right-mark
  | right-to-left-mark
  | line-separator
  | paragraph-separator
```

## Comments

```term
comment-text  ::= unicode-scalar-value - line-break

comment       ::= "//" (comment-text - "/" comment-text*)? line-break
doc-comment   ::= "///" comment-text* line-break
```

## Keywords

```term
keyword ::=
  | "const"
  | "else"
  | "Format"
  | "Kind"
  | "if"
  | "match"
  | "repr"
  | "struct"
  | "Type"
```

## Names

```term
name-start       ::= "a" ... z" | "A" ... "Z" | "_"
name-continue    ::= "a" ... z" | "A" ... "Z" | "0" ... "9" | "_"

name ::=
  | (name-start name-continue*) - keyword
```

### Numeric literals

```text
sign           ::= "+" | "-"
digit-start    ::= "0" ... "9"
digit-continue ::= "a" ... "z" | "A" ... "Z" | "0" ... "9" | "."

numeric-literal ::=
  | sign? digit-start digit-continue*
```

## Punctuation

```term
delimiters ::=
  | "{"
  | "}"
  | "["
  | "]"
  | "("
  | ")"

symbol ::=
  | ":"
  | ","
  | "="
  | "=>"
  | "."
  | "->"
  | ";"

punctuation ::=
  | delimiter
  | symbol
```
