# `fathom`

## Help information

Short help can be printed with `-h`

```console
$ fathom -h
A language for declaratively specifying binary data formats

Usage: fathom <COMMAND>

Commands:
  elab  Elaborate a Fathom module or term, printing the result to stdout
  norm  Normalize a Fathom term, printing its normal form and type
  data  Manipulate binary data based on a Fathom format
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version

```

Long help can be printed with `--help`

```console
$ fathom --help
A language for declaratively specifying binary data formats

Usage: fathom <COMMAND>

Commands:
  elab  Elaborate a Fathom module or term, printing the result to stdout
  norm  Normalize a Fathom term, printing its normal form and type
  data  Manipulate binary data based on a Fathom format
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version

```

## Missing subcommands

A subcommand must be provided to `fathom`

```console
$ fathom
? failed
A language for declaratively specifying binary data formats

Usage: fathom <COMMAND>

Commands:
  elab  Elaborate a Fathom module or term, printing the result to stdout
  norm  Normalize a Fathom term, printing its normal form and type
  data  Manipulate binary data based on a Fathom format
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version

```
