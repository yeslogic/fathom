# Concrete Syntax

The surface syntax matches on any `token`, filtering out `white-space` and
`comment`s in the process.

## Modules

Modules are made up of a list of zero-or-more items.

```text
module ::= item*
```

## Items

Items can be either constant definitions or structure definitions.

```text
item ::=
  | constant-definition
  | struct-type-definition
```

### Constant definitions

Constant definitions are used to give names to terms that can be later used in
other places in the binary description, for example:

```fathom
const Byte = U8;
```

Constants are bound using the `const` keyword followed by an
identifier and the term that they are assigned to, terminated by a
semicolon:

```text
constant-definition ::=
  | doc-comment* "const" name "=" term ";"
```

### Structure type definitions

Structure types are used to describe ordered sequences of binary data.
They are defined using the `struct` keyword, for example:

```fathom
struct Point3 : Format {
    x : F32Be,
    y : F32Be,
    z : F32Be,
}
```

Structures are composite types that have a name and a list of fields. The
fields within a structure must have unique names.

```text
struct-type-field ::=
  | doc-comment* name ":" term

struct-type-definition ::=
  | doc-comment* "struct" name (":" term)? "{" separated(struct-type-field) "}"
```

## Terms

```text
term ::=
  | arrow-term
  | arrow-term ":" term

arrow-term ::=
  | arrow-term
  | app-term "->" arrow-term

app-term ::=
  | atomic-term
  | atomic-term atomic-tem*

atomic-term ::=
  | "(" term ")"
  | name
  | "Type"
  | "Kind"
  | "repr"
  | "struct" "{" separated(name "=" term) "}"
  | atomic-term "." name
  | "[" separated(term) "]"
  | numeric-literal
  | "if" tern "{" term "}" "else" "{" term "}"
  | "match" term "{" separated(pattern "=>" term) "}"
  | "Format"
```

## Separated lists

Lists of zero-or-more elements with optional trailing separators.

```text
separated(element) ::=
    | (element ",")* element?
```
