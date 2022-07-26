# `fathom-norm`

## Help information

Short help can be printed with `-h`

```console
$ fathom norm -h
fathom-norm 
Normalise a Fathom term, printing its normal form and type

USAGE:
    fathom norm [OPTIONS] --term <TERM_FILE>

OPTIONS:
        --term <TERM_FILE>    Path to a term to normalise
        --allow-errors        Continue even if errors were encountered
    -h, --help                Print help information

```

Long help can be printed with `--help`

```console
$ fathom norm --help
fathom-norm 
Normalise a Fathom term, printing its normal form and type

USAGE:
    fathom norm [OPTIONS] --term <TERM_FILE>

OPTIONS:
        --term <TERM_FILE>    Path to a term to normalise
        --allow-errors        Continue even if errors were encountered
    -h, --help                Print help information

```

## Usage examples

### Normalising terms

Terms can be normalised with `--term`

```console
$ fathom norm --term tests/succeed/fun-elim/ann-identity-poly-1.fathom
fun a => a : Type -> Type

```

## Error cases

### Missing arguments

At least a `--term` must be provided

```console
$ fathom norm
? failed
error: The following required arguments were not provided:
    --term <TERM_FILE>

USAGE:
    fathom norm [OPTIONS] --term <TERM_FILE>

For more information try --help

```

## Missing files

The path supplied to `--term` must exist

```console
$ fathom norm --term does/not/exist.fathom
? failed
error: couldn't read `does/not/exist.fathom`: No such file or directory (os error 2)


```

### Type errors

The term must be well-typed before normalisation

```console
$ fathom norm --term tests/fail/elaboration/duplicate-field-labels/record-literal.fathom
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
