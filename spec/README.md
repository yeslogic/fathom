# ddl-spec

A specification of the binary DDL using [Makam](https://github.com/astampoulis/makam/).

## Getting started

We use [Yarn](https://yarnpkg.com/) to manage this project. Because of this you
won't need to install Makam globally. To fetch the dependencies, run the
following command in this directory:

```sh
yarn install
```

You can alternatively run this from another directory in this repository using:

```sh
yarn workspace spec install
```

### Available scripts

From this directory:

```sh
yarn test     # Run the tests
yarn repl     # Start the Makam REPL with the source files loaded
```

From another directory in this project:

```sh
yarn workspace spec test     # Run the tests
yarn workspace spec repl     # Start the Makam REPL with the source files loaded
```

## Roadmap

- [-] core language
    - [-] language feautures
        - [x] basic MLTT (without identity types)
        - [x] basic binary format descriptions
        - [ ] unions
        - [ ] refinement types
        - [ ] multi-stage programming
    - [x] normalization by evaluation
    - [-] typing rules
        - [-] declarative typing rules
        - [x] bidirectional type checking rules
    - [ ] binary format interpretation
- [ ] projections
    - [ ] surface-to-core
    - [ ] core-to-surface
    - [ ] core-to-stratified
    - [ ] stratified-to-unkinded
    - [ ] unkinded-to-uncurried
    - [ ] uncurried-to-rust
