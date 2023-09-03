# `fathom elab`

## Help information

Short help can be printed with `-h`

```console
$ fathom elab -h
Elaborate a Fathom module or term, printing the result to stdout

Usage: fathom elab [OPTIONS]

Options:
      --module <MODULE_FILE>  Path to a module to elaborate
      --term <TERM_FILE>      Path to a term to elaborate
      --allow-errors          Continue even if errors were encountered
      --pretty-core           Pretty print core module
  -h, --help                  Print help

```

Long help can be printed with `--help`

```console
$ fathom elab --help
Elaborate a Fathom module or term, printing the result to stdout

Usage: fathom elab [OPTIONS]

Options:
      --module <MODULE_FILE>  Path to a module to elaborate
      --term <TERM_FILE>      Path to a term to elaborate
      --allow-errors          Continue even if errors were encountered
      --pretty-core           Pretty print core module
  -h, --help                  Print help

```

## Usage examples

### Elaborating modules

Modules can be elaborated with `--module`

```console
$ fathom elab --module formats/object-id.fathom
def u24be : Format = repeat_len8 3 u8;
def main : Format = {
    timestamp <- u32be,
    random <- repeat_len8 5 u8,
    counter <- u24be,
};

```

### Elaborating terms

Terms can be elaborated with `--term`

```console
$ fathom elab --term tests/succeed/record-type/pair-dependent.fathom
{ A : Type, a : A } : Type

```

## Error cases

### Missing arguments

Either a `--module` or a `--term` must be provided

```console
$ fathom elab
? failed
error: the following required arguments were not provided:
  --module <MODULE_FILE>
  --term <TERM_FILE>

Usage: fathom elab --module <MODULE_FILE> --term <TERM_FILE>

For more information, try '--help'.

```

### Conflicting arguments

The `--module` and `--term` inputs conflict with each other

```console
$ fathom elab --module formats/object-id.fathom
>             --term tests/succeed/record-type/pair-dependent.fathom
? failed
error: the argument '--module <MODULE_FILE>' cannot be used with '--term <TERM_FILE>'

Usage: fathom elab --module <MODULE_FILE>

For more information, try '--help'.

```

### Missing files

The path supplied to `--term` must exist

```console
$ fathom elab --term does/not/exist.fathom
? failed
error: couldn't read `does/not/exist.fathom`: No such file or directory (os error 2)


```

The path supplied to `--module` must exist

```console
$ fathom elab --module does/not/exist.fathom
? failed
error: couldn't read `does/not/exist.fathom`: No such file or directory (os error 2)


```

### Type errors

Type errors will be reported during elaboration

```console
$ fathom elab --term tests/fail/elaboration/duplicate-field-labels/record-literal.fathom
? failed
error: duplicate labels found in record
  ┌─ tests/fail/elaboration/duplicate-field-labels/record-literal.fathom:3:23
  │
3 │ { x = Type, y = Type, x = Type }
  │ ----------------------^---------
  │ │                     │
  │ │                     duplicate field
  │ the record literal
  │
  = duplicate fields `x`


```

### Module item cycle

Cycles in a module are reported during elaboration

```console
$ fathom elab --module tests/fail/elaboration/item-cycle.fathom
? failed
error: cycle detected
 = first → second → third → first

error: cycle detected
 = a → b → c → d → b


```
