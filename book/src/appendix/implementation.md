# Compiler Architecture

In order to create a separation of concerns, we break up our compiler into many
small stages, beginning with a source string, and ultimately ending up with
compiled machine code.

Below is a rough flow chart showing how source strings are currently lexed,
parsed, desugared, and type checked/elaborated:

```bob
         .------------.
         |   String   |
         '------------'
                |
      syntax::parse::lexer
                |
                v
  .-----------------------------.
  | syntax::parse::lexer::Token |
  '-----------------------------'
                |
     syntax::parse::grammar
                |
                v
    .------------------------.
    | syntax::concrete::Term |
    '------------------------'
                |
   syntax::translation::desugar
                |
                v
      .-------------------.
      | syntax::raw::Term |
      '-------------------'
                |                        .---------------------.
    semantics::{check,infer} <---------- | syntax::core::Value |
                |                        '---------------------'
                v                                    ^
      .--------------------.                         |
      | syntax::core::Term | - semantics::normalize -'
      '--------------------'
                |
                v
    TODO: compiler back end(s)
```

As you can see we have only built the front-end as of the time of writing. When
we begin to build out a [compiler back end](https://github.com/pikelet-lang/pikelet/issues/9),
more stages will be added after type checking and elaboration.

## Name binding

Name binding is a surprisingly challenging thing to implement in type checkers
and compilers. We use the [`moniker` crate](https://github.com/brendanzab/moniker)
for this. Unfortunately this uses a quite slow method of name binding, and could
result in performance blowouts in the future. This is something to keep an eye on!
