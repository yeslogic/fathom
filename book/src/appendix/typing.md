# Typing

## Contents

- [Introduction](#introduction)
- [Syntax](#syntax)
    - [Terms](#terms)
        - [Syntactic sugar](#syntactic-sugar)
        - [Field lookups](#field-lookups)
        - [Representation types](#representation-types)
    - [Contexts](#contexts)
- [Semantics](#semantics)
    - [Evaluation](#evaluation)
    - [Type checking](#type-checking)
    - [Type inference](#type-inference)
- [Related work](#related-work)
- [References](#references)

## Introduction

Here we invest some time towards defining the syntax and semantics for the core
type sytem of our data definition language. In order to do this we use a form of
‘computer science meta-notation’ that combines BNF-style syntax trees and
natural deduction to allow us to describe our system with reasonable brevity.
## Syntax

### Terms

We define a core dependently typed language with the addition of dependent pairs
and a separate universe for binary types:

\\[
\\newcommand{\rule}[3]{
    \dfrac{
        ~~#2~~
    }{
        ~~#3~~
    }
    \Tiny{\text{(#1)}}
}
\\
\\DeclareMathOperator{\FV}{FV}
\\
\\newcommand{\subst}[3]
    {#1 [#2 \rightarrow #3]
}
\\newcommand{\eval}[2]{
    #1 \Rightarrow #2
}
\\newcommand{\check}[3]{
    #1 \vdash #2 \uparrow #3
}
\\newcommand{\infer}[3]{
    #1 \vdash #2 \downarrow #3
}
\\
\\newcommand{\Arrow}[2]{
    #1 \rightarrow #2
}
\\newcommand{\Pair}[2]{
    #1 \times #2
}
\\newcommand{\pair}[2]{
    \langle #1,#2 \rangle
}
\\newcommand{\Host}{\mathsf{Host}}
\\newcommand{\Binary}{\mathsf{Binary}}
\\newcommand{\Kind}{\mathsf{Kind}}
\\newcommand{\Unit}{\mathsf{Unit}}
\\newcommand{\unit}{\langle\rangle}
\\
\begin{array}{rrll}
    e,\tau  & ::= & x                               & \text{variables} \\\\
            &   | & e : \tau                        & \text{term annotated with a type} \\\\
            &   | & \Arrow{(x:\tau_1)}{\tau_2}      & \text{dependent functions} \\\\
            &   | & \lambda x.e                     & \text{functions} \\\\
            &   | & e_1 e_2                         & \text{function application} \\\\
            &   | & \Pair{(x:\tau_1)}{\tau_2}       & \text{dependent pair type} \\\\
            &   | & \pair{x:e_1}{e_2}               & \text{dependent pairs} \\\\
            &   | & e.x                             & \text{field projection} \\\\
            &   | & \Host                           & \text{universe of host types} \\\\
            &   | & \Binary                         & \text{universe of binary descriptions} \\\\
            &   | & \Kind                           & \text{universe of universes} \\\\
            &   | & \Unit                           & \text{the unit type} \\\\
            &   | & \unit                           & \text{the element of the unit type} \\\\
\end{array}
\\]

#### Syntactic sugar

To lighten some of our syntactic overhead, we'll be using some syntactic sugar
for declaring non-dependent versions of some types. This will become inportant
once we come to our type checking rules because we would like to ensure our
\\(\Host\\) terms remain non-dependent.

\\[
\begin{array}{rrll}
    \Arrow{\tau_1}{\tau_2}      & := & \Arrow{(x:\tau_1)}{\tau_2}   & x \notin \FV(\tau_2) \\\\
    \Pair{\tau_1}{\tau_2}       & := & \Pair{(x:\tau_1)}{\tau_2}    & x \notin \FV(\tau_2) \\\\
    \pair{e_1}{e_2}             & := & \pair{x:e_1}{e_2}            & x \notin \FV(e_2) \\\\
\end{array}
\\]

#### Field lookups

Here we define field lookups at both the value and the type level:

\\[
\\DeclareMathOperator{\field}{field} \\
\\
\begin{array}{lll}
    \field(\langle x:e_1, e_2 \rangle, x) & = & e_1 \\\\
    \field(\langle y:e_1, e_2 \rangle, x) & = & \field(e_2, x), ~ \text{if} ~ y \ne x \\\\
    \\\\
    \field((x:\tau_1) \times \tau_2, x)   & = & \tau_1 \\\\
    \field((y:\tau_1) \times \tau_2, x)   & = & \field(\tau_2, x), ~ \text{if} ~ y \ne x \\\\
\end{array}
\\]
#### Representation types

\\[
\\DeclareMathOperator{\repr}{repr} \\
\\
\begin{array}{lll}
    \repr(x)                                & = & x \\\\
    \repr(e : \tau)                         & = & \repr(e) : \repr(\tau) \\\\
    \repr(\Pair{(x:\tau_1)}{\tau_2})        & = & \Pair{\repr(\tau_1)}{\repr(\tau_2)} \\\\
    \repr(\pair{x:e_1}{e_2})                & = & \pair{\repr(e_1)}{\repr(e_2)} \\\\
    \repr(\Unit)                            & = & \Unit \\\\
    \repr(\unit)                            & = & \unit \\\\
\end{array}
\\]

TODO

### Contexts

\\[
\begin{array}{rrll}
    \Gamma  & ::= & \cdot            & \text{the empty context} \\\\
            &   | & \Gamma,x:\tau    & \text{context extended with a type} \\\\
\end{array}
\\]

## Semantics

We define a bidirectional inference algorithm, using an approach pioneered by
Pierce, and since championed by McBride. We'll end up with the following
judgement forms for our syntax:

- \\(\eval{ e_1 }{ e_2 }\\): Evaluate an expression \\(e_1\\) to an expression \\(e_2\\)
- \\(\check{ \Gamma }{ e }{ \tau }\\): Check that the expression \\(e\\) has the type \\(\tau\\) in the context \\(\Gamma\\)
- \\(\infer{ \Gamma }{ e }{ \tau }\\): Infer that the expression \\(e\\) has the type \\(\tau\\) in the context \\(\Gamma\\)

Throughout the judgements we assume that variables are well-scoped. An actual
implementation would most likely use a locally nameless representation, but
for clarity we have chosen to omit this machinery from our typing rules.

### Evaluation

Since we are designing a dependent type system it is crucial that we first
define evaluation. This ensures that we reduce terms before we check them for
equivalence during type checking.

\\[
\boxed{
    \eval{ e_1 }{ e_2 }
}
\\]

TODO
### Type checking

Now we get to the main part of typechecking. We supply and expression \\(e\\)
and a type \\(\tau\\), and check to see if it meets any of the judgements in
the context \\(\Gamma\\). Note that we expect that the type \\(\tau\\) has been
previously evaluated before we start:

\\[
\boxed{
    \check{ \Gamma }{ e }{ \tau }
}
\\\\[2em]
\rule{C-LAMBDA}{
    \infer{ \Gamma,x:\tau_1 }{ e }{ \tau_2 }
}{
    \check{ \Gamma }{ \lambda x.e }{ \Arrow{(x:\tau_1)}{\tau_2} }
}
\\\\[2em]
\rule{C-UNIT-BINARY}{}{
    \check{ \Gamma }{ \Unit }{ \Binary }
}
\\\\[2em]
\rule{C-UNIT-HOST}{}{
    \check{ \Gamma }{ \Unit }{ \Host }
}
\\\\[2em]
\rule{C-CONV}{
    \infer{ \Gamma }{ e }{ \tau_2 }
    \qquad
    \tau_1 \equiv \tau_2
}{
    \check{ \Gamma }{ e }{ \tau_1 }
}
\\]

For concision, we allow lambdas to omit their type annotations, so they must be
checked contextually. Type checking lambdas that lack this type annotation is
easy, because we can pull it out of the input type, which we expect to be a
dependent function type.

We want to be able to use the same \\(\Unit\\) type for both binary
descriptions and host descriptions, so this means they must also be checked
contextually.

The flip between checking and inference also occurs here. We rely on alpha
equivalence check (\\(\tau_1 \equiv \tau_2\\)) to ensure that the expected type
\\(\tau_1\\) is equivalent to the inferred type \\(\tau_2\\). This could be
replaced with a subtyping check in the future.

### Type inference

\\[
\boxed{
    \infer{ \Gamma }{ e }{ \tau }
}
\\\\[2em]
\rule{I-VAR}{
    \Gamma (x) = \tau
}{
    \infer{ \Gamma }{ x }{ \tau }
}
\\\\[2em]
\rule{I-BINARY}{}{
    \infer{ \Gamma }{ \Binary }{ \Kind }
}
\\\\[2em]
\rule{I-HOST}{}{
    \infer{ \Gamma }{ \Host }{ \Kind }
}
\\\\[2em]
\rule{I-ANN-BINARY}{
    \check{ \Gamma }{ e }{ \tau }
    \qquad
    \eval{ \tau }{ \tau' }
    \qquad
    \check{ \Gamma }{ \tau' }{ \Binary }
}{
    \infer{ \Gamma }{ e : \tau }{ \tau' }
}
\\\\[2em]
\rule{I-ANN-HOST}{
    \check{ \Gamma }{ e }{ \tau }
    \qquad
    \eval{ \tau }{ \tau' }
    \qquad
    \check{ \Gamma }{ \tau' }{ \Host }
}{
    \infer{ \Gamma }{ e : \tau }{ \tau' }
}
\\\\[2em]
\rule{I-APP}{
    \check{ \Gamma }{ e_1 }{ \Arrow{(x:\tau_1)}{\tau_2} }
    \qquad
    \infer{ \Gamma }{ e_2 }{ \tau_1 }
    \qquad
    \eval{ \tau_2 }{ \tau_2' }
}{
    \infer{ \Gamma }{ e_1 e_2 }{ \subst{\tau_2'}{x}{e_2} }
}
\\\\[2em]
\rule{I-PI-BINARY}{
    \infer{ \Gamma }{ \tau_1 }{ \Binary }
    \qquad
    \eval{ \tau_1 }{ \tau_1' }
    \qquad
    \check{ \Gamma,x:\tau_1' }{ \tau_2 }{ \Binary }
}{
    \infer{ \Gamma }{ \Arrow{(x:\tau_1)}{\tau_2} }{ \Binary }
}
\\\\[2em]
\rule{I-PI-HOST}{
    \infer{ \Gamma }{ \tau_1 }{ \Host }
    \qquad
    \eval{ \tau_1 }{ \tau_1' }
    \qquad
    \check{ \Gamma,x:\tau_1' }{ \tau_2 }{ \Binary }
}{
    \infer{ \Gamma }{ \Arrow{(x:\tau_1)}{\tau_2} }{ \Binary }
}
\\\\[2em]
\rule{I-ARROW}{
    \infer{ \Gamma }{ \tau_1 }{ \Host }
    \qquad
    \check{ \Gamma }{ \tau_2 }{ \Host }
}{
    \infer{ \Gamma }{ \Arrow{\tau_1}{\tau_2} }{ \Host }
}
\\\\[2em]
\rule{I-SIGMA}{
    \infer{ \Gamma }{ \tau_1 }{ \Binary }
    \qquad
    \eval{ \tau_1 }{ \tau_1' }
    \qquad
    \check{ \Gamma, x:\tau_1' }{ \tau_2 }{ \Binary }
}{
    \infer{ \Gamma }{ \Pair{(x:\tau_1)}{\tau_2} }{ \Binary }
}
\\\\[2em]
\rule{I-PAIR}{
    \infer{ \Gamma }{ \tau_1 }{ \Host }
    \qquad
    \check{ \Gamma }{ \tau_2 }{ \Host }
}{
    \infer{ \Gamma }{ \Pair{\tau_1}{\tau_2} }{ \Host }
}
\\\\[2em]
\rule{I-INTRO-SIGMA}{
    \infer{ \Gamma }{ e_1 }{ \tau_1 }
    \qquad
    \check{ \Gamma }{ \tau_1 }{  \Binary }
    \qquad
    \infer{ \Gamma, x:\tau_1 }{ e_2 }{ \tau_2 }
}{
    \infer{ \Gamma }{ \pair{x:e_1}{e_2} }{ \Pair{(x:\tau_1)}{\tau_2} }
}
\\\\[2em]
\rule{I-INTRO-PAIR}{
    \infer{ \Gamma }{ e_1 }{ \tau_1 }
    \qquad
    \check{ \Gamma }{ \tau_1 }{  \Host }
    \qquad
    \infer{ \Gamma }{ e_2 }{ \tau_2 }
}{
    \infer{ \Gamma }{ \pair{x:e_1}{e_2} }{ \Pair{\tau_1}{\tau_2} }
}
\\\\[2em]
\rule{I-PROJ}{
    \infer{ \Gamma }{ e }{ \tau_1 }
    \qquad
    \eval{ \tau_1 }{ \tau_1' }
    \qquad
    \field(\tau_1',x) = \tau_2
}{
    \infer{ \Gamma }{ e.x }{ \tau_2 }
}
\\\\[2em]
\rule{I-INTRO-UNIT}{}{
    \infer{ \Gamma }{ \unit }{ \Unit }
}
\\\\[2em]
\\]

## Related work

TODO

- DDC/PADS
- The Power of Pi
- Generic Packet Descriptions: Verified Parsing and Pretty Printing of Low-Level Data

## References

- Charguéraud, Arthur (2011). “The Locally Nameless Representation”.
  In _Journal of Automated Reasoning (JAR)_.
  [[SITE][ln-site]]
  [[PAPER][ln-paper]]
- Christiansen, David Raymond (2013). “Bidirectional Typing Rules: A Tutorial”.
  [[PAPER][bidirectional-typing-paper]]
- Löh, Andres, McBride, Conor and Swierstra, Wouter (2009). “A tutorial
  implementation of a dependently typed lambda calculus”.
  [[SITE][lambdapi-site]]
  [[PAPER][lambdapi-paper]]
- Norell, Ulf (2007). “Towards a practical programming language based on
  dependent type theory”.
  [[PAPER][agda-paper]]

[ln-site]: http://www.chargueraud.org/softs/ln/
[ln-paper]: http://www.chargueraud.org/research/2009/ln/main.pdf
[bidirectional-typing-paper]: http://www.davidchristiansen.dk/tutorials/bidirectional.pdf
[lambdapi-site]: https://www.andres-loeh.de/LambdaPi/
[lambdapi-paper]: https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
[agda-paper]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
