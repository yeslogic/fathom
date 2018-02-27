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
\\DeclareMathOperator{\FV}{FV} \\
\\
\begin{array}{rrll}
    e,\tau  & ::= & x                               & \text{variables} \\\\
            &   | & e : \tau                        & \text{term annotated with a type} \\\\
            &   | & (x:\tau_1) \rightarrow \tau_2   & \text{dependent functions} \\\\
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
    \tau_1 \rightarrow \tau_2   & := & (x:\tau_1) \rightarrow \tau_2   & x \notin \FV(\tau_2) \\\\
    \Pair{\tau_1}{\tau_2}       & := & \Pair{(x:\tau_1)}{\tau_2}       & x \notin \FV(\tau_2) \\\\
    \pair{e_1}{e_2}             & := & \pair{x:e_1}{e_2}               & x \notin \FV(e_2) \\\\
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

- \\(e_1 \Rightarrow e_2\\): Evaluate an expression \\(e_1\\) to an expression \\(e_2\\)
- \\(\Gamma \vdash e \uparrow \tau\\): Check that the expression \\(e\\) has the type \\(\tau\\) in the context \\(\Gamma\\)
- \\(\Gamma \vdash e \downarrow \tau\\): Infer that the expression \\(e\\) has the type \\(\tau\\) in the context \\(\Gamma\\)

Throughout the judgements we assume that variables are well-scoped. An actual
implementation would most likely use a locally nameless representation, but
for clarity we have chosen to omit this machinery from our typing rules.

### Evaluation

Since we are designing a dependent type system it is crucial that we first
define evaluation. This ensures that we reduce terms before we check them for
equivalence during type checking.

\\[
\boxed{
    e_1 \Rightarrow e_2
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
    \Gamma \vdash e \uparrow \tau
}
\\\\[2em]
\rule{C-LAMBDA}{
    \Gamma,x:\tau_1 \vdash e \downarrow \tau_2
}{
    \Gamma \vdash \lambda x.e \uparrow (x:\tau_1) \rightarrow \tau_2
}
\\\\[2em]
\rule{C-UNIT-BINARY}{}{
    \Gamma \vdash \Unit \uparrow \Binary
}
\\\\[2em]
\rule{C-UNIT-HOST}{}{
    \Gamma \vdash \Unit \uparrow \Host
}
\\\\[2em]
\rule{C-CONV}{
    \Gamma \vdash e \downarrow \tau_2
    \qquad
    \tau_1 \equiv \tau_2
}{
    \Gamma \vdash e \uparrow \tau_1
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
    \Gamma \vdash e \downarrow \tau
}
\\\\[2em]
\rule{I-VAR}{
    \Gamma (x) = \tau
}{
    \Gamma \vdash x \downarrow \tau
}
\\\\[2em]
\rule{I-BINARY}{}{
    \Gamma \vdash \Binary \downarrow \Kind
}
\\\\[2em]
\rule{I-HOST}{}{
    \Gamma \vdash \Host \downarrow \Kind
}
\\\\[2em]
\rule{I-ANN-BINARY}{
    \Gamma \vdash e \downarrow \tau
    \qquad
    \tau \Rightarrow \tau'
    \qquad
    \Gamma \vdash \tau' \downarrow \Binary
}{
    \Gamma \vdash e : \tau \downarrow \tau'
}
\\\\[2em]
\rule{I-ANN-HOST}{
    \Gamma \vdash e \downarrow \tau
    \qquad
    \tau \Rightarrow \tau'
    \qquad
    \Gamma \vdash \tau' \downarrow \Host
}{
    \Gamma \vdash e : \tau \downarrow \tau'
}
\\\\[2em]
\rule{I-APP}{
    \Gamma \vdash e_1 \uparrow (x:\tau_1) \rightarrow \tau_2
    \qquad
    \Gamma \vdash e_2 \downarrow \tau_1
    \qquad
    \tau_2 \Rightarrow \tau_2'
}{
    \Gamma \vdash e_1 e_2 \downarrow \tau_2'[x \rightarrow e_2]
}
\\\\[2em]
\rule{I-PI-BINARY}{
    \Gamma \vdash \tau_1 \downarrow \Binary
    \qquad
    \tau_1 \Rightarrow \tau_1'
    \qquad
    \Gamma,x:\tau_1' \vdash \tau_2 \uparrow \Binary
}{
    \Gamma \vdash (x:\tau_1) \rightarrow \tau_2 \downarrow \Binary
}
\\\\[2em]
\rule{I-PI-HOST}{
    \Gamma \vdash \tau_1 \downarrow \Host
    \qquad
    \tau_1 \Rightarrow \tau_1'
    \qquad
    \Gamma,x:\tau_1' \vdash \tau_2 \uparrow \Binary
}{
    \Gamma \vdash (x:\tau_1) \rightarrow \tau_2 \downarrow \Binary
}
\\\\[2em]
\rule{I-ARROW}{
    \Gamma \vdash \tau_1 \downarrow \Host
    \qquad
    \Gamma \vdash \tau_2 \uparrow \Host
}{
    \Gamma \vdash \tau_1 \rightarrow \tau_2 \downarrow \Host
}
\\\\[2em]
\rule{I-SIGMA}{
    \Gamma \vdash \tau_1 \downarrow \Binary
    \qquad
    \tau_1 \Rightarrow \tau_1'
    \qquad
    \Gamma, x:\tau_1' \vdash \tau_2 \uparrow \Binary
}{
    \Gamma \vdash \Pair{(x:\tau_1)}{\tau_2} \downarrow \Binary
}
\\\\[2em]
\rule{I-PAIR}{
    \Gamma \vdash \tau_1 \downarrow \Host
    \qquad
    \Gamma \vdash \tau_2 \uparrow \Host
}{
    \Gamma \vdash \Pair{\tau_1}{\tau_2} \downarrow \Host
}
\\\\[2em]
\rule{I-INTRO-SIGMA}{
    \Gamma \vdash e_1 \downarrow \tau_1
    \qquad
    \Gamma \vdash \tau_1 \uparrow  \Binary
    \qquad
    \Gamma, x:\tau_1 \vdash e_2 \downarrow \tau_2
}{
    \Gamma \vdash \pair{x:e_1}{e_2} \downarrow \Pair{(x:\tau_1)}{\tau_2}
}
\\\\[2em]
\rule{I-INTRO-PAIR}{
    \Gamma \vdash e_1 \downarrow \tau_1
    \qquad
    \Gamma \vdash \tau_1 \uparrow  \Host
    \qquad
    \Gamma \vdash e_2 \downarrow \tau_2
}{
    \Gamma \vdash \pair{x:e_1}{e_2} \downarrow \Pair{\tau_1}{\tau_2}
}
\\\\[2em]
\rule{I-PROJ}{
    \Gamma \vdash e \downarrow \tau_1
    \qquad
    \tau_1 \Rightarrow \tau_1'
    \qquad
    \field(\tau_1',x) = \tau_2
}{
    \Gamma \vdash e.x \downarrow \tau_2
}
\\\\[2em]
\rule{I-INTRO-UNIT}{}{
    \Gamma \vdash \unit \downarrow \Unit
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
