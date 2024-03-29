# Contributing

## Code of Conduct

Please note that this project is released with a [Code of Conduct](./code-of-conduct.md).
By participating in this project you agree to abide by its terms.

## Matrix room

Joining the matrix room at [#fathom-lang:matrix.org][fathom-matrix] is a good way to get in touch with the developers and community.

[fathom-matrix]: https://app.element.io/#/room/#fathom-lang:matrix.org

## Prerequisites

We use [Rust][rust] as our implementation language, which can be installed using the [rustup] tool.

For the best experience in working with Rust we also recommend installing IDE support for your editor of choice:

- [Rust Analyzer][rust-analyzer] (for VS Code, Vim Emacs, etc.)
- [IntelliJ Rust][intellij-rust] (for IntelliJ-based IDEs)

You can learn more about programming in Rust by reading [The Rust Programming Language][rust-book].

[rust]: https://www.rust-lang.org/
[rustup]: https://rustup.rs/
[rust-analyzer]: https://rust-analyzer.github.io/
[intellij-rust]: https://intellij-rust.github.io/
[rust-book]: https://doc.rust-lang.org/book/

## Workflow

Follow these steps to contribute to the project:

1. Make a fork of the [Fathom repository][fathom-repo].
1. Within your fork, create a branch for your contribution. Use a meaningful name.
1. Create your contribution, meeting all [contribution quality standards](#quality-standards).
1. Ensure all the tests pass (`cargo test`).
1. [Create a pull request][create-a-pr] against the `master` branch of the repository.
1. Once the pull request is reviewed and CI passes, it will be merged.

[fathom-repo]: https://github.com/yeslogic/fathom
[create-a-pr]: https://help.github.com/articles/creating-a-pull-request-from-a-fork/

## Quality Standards

Most quality and style standards are checked automatically by the CI build.
Contributions should:

- Separate each **logical change** into its own commit.
- Include tests for any new functionality in your pull request.
- Document public functions.
- Format code with `cargo fmt`.
- Avoid adding `unsafe` code.
  If it is necessary, provide an explanatory comment on any `unsafe` block explaining its rationale and why it's safe.
- Add a descriptive message for each commit.
  Follow [these commit message guidelines][commit-messages].
- Document your pull requests.
  Include the reasoning behind each change, and the testing done.

[commit-messages]: https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

## Background

Watching the [talk on Fathom][compose-talk] at Compose Melbourne 2019 is a good start if you want to get an initial understanding of our approach.
It is intended to be accessible to a wide range of backgrounds,
no matter your experience with type theory and programming language research.

Optionally you might be interested in some of the academic research we base our work on.
These can be found in the [background reading][background].
Our approach is very inspired by those described in
“The next 700 data description languages” and “The power of pi”,
so you might want to start with those first!

If you are new to reading programming language papers,
Jeremy Siek's [Crash Course on Notation in Programming Language Theory][crash-couse] can be a big help.
Also feel free to reach out to us on [Matrix](#matrix-room) for assistance!

[compose-talk]: https://www.youtube.com/watch?v=L9TubiWkBZ8
[background]: ../background.md
[crash-couse]: http://siek.blogspot.com.au/2012/07/crash-course-on-notation-in-programming.html
