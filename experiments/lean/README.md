# Language formalisation in Lean

This is incomplete, and mainly indended as an explorative tool to guide the Rust
implementation. It would be very nice to provide more guarentees in the future
that opur Rust implementation matches this formalisation, but that is out of
scope for now!

To work with the formalisations, you will need to [install Lean 3.3][install_lean].
Then run the following commands in the shell:

```sh
cd experiments/lean
leanpkg configure
leanpkg build
```

[install_lean]: https://leanprover.github.io/download/

## Note for VS Code users

Note that the VS Code extension doesn't work correctly with packages in
sub-directories at the time of writing, so you'll need to open a new editor
scoped to this directory in order to get the language server to work correctly.
