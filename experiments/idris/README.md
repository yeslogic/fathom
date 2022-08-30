# Core language experiments in Idris 2

Some sketches of Fathom’s core language using Idris as a logical framework.

> **Note:**
>
> Idris 2 does not yet support cumulatively or full totality checking, so the
> definitions here may depend on inconsistency. We also don’t aim to prove any
> properties of these definitions, this is more intended for experimentation.

## Development setup

Depends on the following:

- [Idris 2](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md)
- [rlwrap](https://github.com/hanslub42/rlwrap) (optional - as a workaround for
  [idris-lang/Idris2#54](https://github.com/idris-lang/Idris2/issues/54))

If you use NixPkgs the above is installed as part of the default development
shell in the [flake.nix](../../flake.nix) provided.

## Usage

```command
$ rlwrap idris2 --repl experiments/idris/fathom.ipkg
Main> :load "src/Playground.idr"
```
