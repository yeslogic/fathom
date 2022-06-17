# Command line snapshot tests

These snapshot tests leverage [trycmd][trycmd-crate] test harness. More
information on writing and running these tests can be found by reading the
[trycmd docs][trycmd-docs]. If test snapshots need to be updated, rerunning the
tests with `TRYCMD=overwrite` will regenerate them:

```sh
TRYCMD=overwrite cargo test cli_tests
```

[trycmd-crate]: https://crates.io/crates/trycmd
[trycmd-docs]: https://docs.rs/trycmd/latest/trycmd
