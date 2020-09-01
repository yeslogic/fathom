# Contributing to Fathom

## Code of Conduct

Please note that this project is released with a [Code of Conduct](./CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## Join the matrix room

Joining the matrix room at [#fathom-lang:matrix.org](https://app.element.io/#/room/#fathom-lang:matrix.org) is a good way to get in touch with the developers and community.

## Getting started with development

### Install Rust

We use [Rust](https://www.rust-lang.org/) as our implementation language.
You can learn more about programming in Rust by reading
[The Rust Programming Language](https://doc.rust-lang.org/book/).

For the best experience in working with Rust, you'll want to also install IDE
support for your editor of choice:

- [Rust Analyzer](https://rust-analyzer.github.io/) (for VS Code, Vim Emacs, etc.)
- [IntelliJ Rust](https://intellij-rust.github.io/) (for IntelliJ-based IDEs)

### Cloning the repository

Clone the Fathom repository using [Git](https://git-scm.com) by running the
following command in your terminal:

```sh
git clone git@github.com:yeslogic/fathom.git
```

### Running the tests

We run tests using continuous integration in order to reduce the risk of
regressions and bugs being introduced. The cargo tests can be run with the
following command from within the Fathom project directory:

```sh
cargo test
```

## Get up to speed with the theory

Watching the [talk on Fathom][compose-talk] at Compose Melbourne 2019 is a good
start if you want to get an initial understanding of our approach. It is
intended to be accessible to a wide range of backgrounds, no matter your
experience with type theory and programming language research.

Optionally you might be interested in some of the academic research we base our
work on. These can be found in the [list of references][references]. Our
approach is very inspired those described in “The next 700 hundred data
description languages” and “The power of pi”, so you might want to start with
those first!

If are new to reading programming language papers, Jeremy Siek's [Crash Course
on Notation in Programming Language Theory][crash-couse] can be a big help. Also
feel free to reach out to us on [Matrix](#join-the-matrix-room) for assistance!

[compose-talk]: https://www.youtube.com/watch?v=L9TubiWkBZ8
[references]: ./docs/specification/references.md
[crash-couse]: http://siek.blogspot.com.au/2012/07/crash-course-on-notation-in-programming.html
