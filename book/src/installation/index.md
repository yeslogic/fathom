# Installation

The DDL is written in [Rust][rust-site] and therefore needs to be compiled with
Cargo, because we don't yet offer prebuilt binaries. If you haven't already
installed Rust, please [install it][rust-install] now!

[rust-site]: https://www.rust-lang.org/
[rust-install]: https://www.rust-lang.org/downloads.html

## Cloning the source from Github

We've not yet published the DDL on [crates.io][crates-io], so you'll first need
to clone [the repository][ddl-repository] using git:

```sh
git clone https://github.com/yeslogic/ddl.git
cd ddl
```

[crates-io]: https://crates.io/
[ddl-repository]: https://github.com/yeslogic/ddl

## Running the REPL

After cloning, you can now run the [REPL][repl-wikipedia] using Cargo:

```sh
cargo run repl
```

You will now need to wait for cargo to download and build the dependencies, but
sooner or later the REPL will be ready for you to interact with!

[repl-wikipedia]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
