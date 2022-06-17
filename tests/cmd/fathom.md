# `fathom`

## Help information

Short help can be printed with `-h`

```console
$ fathom -h
fathom 0.1.0
YesLogic Pty. Ltd. <info@yeslogic.com>
A language for declaratively specifying binary data formats

USAGE:
    fathom <SUBCOMMAND>

OPTIONS:
    -h, --help       Print help information
    -V, --version    Print version information

SUBCOMMANDS:
    data    Manipulate binary data based on a Fathom format
    elab    Elaborate a Fathom module or term, printing the result to stdout
    help    Print this message or the help of the given subcommand(s)
    norm    Normalise a Fathom term, printing its normal form and type

```

Long help can be printed with `--help`

```console
$ fathom --help
fathom 0.1.0
YesLogic Pty. Ltd. <info@yeslogic.com>
A language for declaratively specifying binary data formats

USAGE:
    fathom <SUBCOMMAND>

OPTIONS:
    -h, --help       Print help information
    -V, --version    Print version information

SUBCOMMANDS:
    data    Manipulate binary data based on a Fathom format
    elab    Elaborate a Fathom module or term, printing the result to stdout
    help    Print this message or the help of the given subcommand(s)
    norm    Normalise a Fathom term, printing its normal form and type

```

## Missing subcommands

A subcommand must be provided to `fathom`

```console
$ fathom
? failed
fathom 0.1.0
YesLogic Pty. Ltd. <info@yeslogic.com>
A language for declaratively specifying binary data formats

USAGE:
    fathom <SUBCOMMAND>

OPTIONS:
    -h, --help       Print help information
    -V, --version    Print version information

SUBCOMMANDS:
    data    Manipulate binary data based on a Fathom format
    elab    Elaborate a Fathom module or term, printing the result to stdout
    help    Print this message or the help of the given subcommand(s)
    norm    Normalise a Fathom term, printing its normal form and type

```
