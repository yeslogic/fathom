# Typing

## Contents

- [Introduction](#introduction)
- [Syntax](#syntax)
    - [Sorts and kinds](#sorts-and-kinds)
    - [Core terms](#core-terms)
    - [Contexts](#contexts)
    - [Syntactic sugar](#syntactic-sugar)
    - [Field lookups](#field-lookups)
    - [Representation types](#representation-types)
- [Semantics](#semantics)
    - [Normalization](#normalization)
    - [Type checking](#type-checking)
    - [Type synthesis](#type-synthesis)
    - [Parsing](#parsing)
- [Compilation](#compilation)
- [Related work](#related-work)
- [References](#references)

## Introduction

Here we invest some time towards defining the syntax and semantics for the core
type sytem of our data definition language. In order to do this we use a form of
‘computer science meta-notation’ that combines BNF-style syntax trees and
natural deduction to allow us to describe our system with reasonable brevity.
## Syntax

### Sorts and kinds

\\[
\\newcommand{\Host}{\mathsf{Host}}
\\newcommand{\Binary}{\mathsf{Binary}}
\\newcommand{\Kind}{\mathsf{Kind}}
\\
\\newcommand{\sort}{s}
\\newcommand{\kind}{k}
\\
\begin{array}{rrll}
    \sort           & ::= & \Kind                           & \text{universe of kinds} \\\\
    \\\\
    \kind           & ::= & \Host                           & \text{universe of host types} \\\\
                    &   | & \Binary                         & \text{universe of binary descriptions} \\\\
    \\\\
\end{array}
\\]

### Core terms

We define a dependently typed core language with the addition of dependent pairs
and a separate universe for binary types:

\\[
\\newcommand{\rule}[3]{ \dfrac{ ~~#2~~ }{ ~~#3~~ } & \Tiny{\text{(#1)}} }
\\newcommand{\change}[1]{ \bbox[lightgray]{#1} }
\\
\\DeclareMathOperator{\FV}{FV}
\\newcommand{\subst}[3]{ #1 [#2 \rightarrow #3] }
\\newcommand{\eval}[2]{ #1 \Rightarrow #2 }
\\newcommand{\check}[3]{ #1 \vdash #2 \uparrow #3 }
\\newcommand{\infer}[3]{ #1 \vdash #2 \downarrow #3 }
\\
% Type constructors
\\newcommand{\Arrow}[2]{ #1 \rightarrow #2 }
\\newcommand{\Pair}[2]{ #1 \times #2 }
\\newcommand{\Unit}{ \mathsf{Unit} }
\\newcommand{\Array}{ \mathsf{Array} }
\\newcommand{\List}{ \mathsf{List}}
\\
% Value constructors
\\newcommand{\pair}[2]{ \langle #1, ~ #2 \rangle }
\\newcommand{\unit}{ \langle\rangle }
\\
% Core metavariables
\\newcommand{\texpr}{t}
\\newcommand{\ttype}{T}
\\
\begin{array}{rrll}
    \texpr,\ttype   & ::= & x                               & \text{variables} \\\\
                    &   | & \sort                           & \text{sorts} \\\\
                    &   | & \kind                           & \text{kinds} \\\\
                    &   | & \texpr : \ttype                 & \text{term annotated with a type} \\\\
                    &   | & \Arrow{(x:\ttype_1)}{\ttype_2}  & \text{dependent function type} \\\\
                    &   | & \lambda x.\texpr                & \text{functions} \\\\
                    &   | & \texpr_1 ~ \texpr_2             & \text{function application} \\\\
                    &   | & \Pair{(x:\ttype_1)}{\ttype_2}   & \text{dependent pair type} \\\\
                    &   | & \pair{x=\texpr_1}{\texpr_2}     & \text{dependent pairs} \\\\
                    &   | & \texpr.x                        & \text{field projection} \\\\
                    &   | & \Unit                           & \text{the unit type} \\\\
                    &   | & \unit                           & \text{the element of the unit type} \\\\
                    &   | & \Array                          & \text{array type constructor} \\\\
                    &   | & \List                           & \text{list type constructor} \\\\
                    &   | & []                              & \text{the empty list} \\\\
                    &   | & \texpr_1 :: \texpr_2            & \text{list constructor} \\\\
                    &   | & \texpr_1[\texpr_2]              & \text{list subscript} \\\\
\end{array}
\\]

We assume that variables are well-scoped. An actual implementation would most
likely use a locally nameless representation, but for clarity we have chosen to
omit this machinery from our typing rules.

By convention we use lowercase metavariables for expression-like things, and
uppercase metavariables for type-like things. Note that we will not enforce
these conventions syntactically in our rules, they are only intended to aid
readability.

### Contexts

As we typecheck our language, we'll be passing over bindings like lambdas and
dependent pairs. Contexts allow us to keep track of the bound parameters and
fields, even though we don't know the exact values these will eventually take
during runtime.

\\[
\begin{array}{rrll}
    \Gamma  & ::= & \epsilon           & \text{the empty context} \\\\
            &   | & \Gamma,x:\ttype    & \text{context extended with a term} \\\\
\end{array}
\\]

### Syntactic sugar

To lighten some of our syntactic overhead, we'll be using some syntactic sugar
for declaring non-dependent versions of some types. This will become inportant
once we come to our type checking rules because we would like to ensure our
\\(\Host\\) terms remain non-dependent.

\\[
\begin{array}{rrll}
    \Arrow{\ttype_1}{\ttype_2}  & := & \Arrow{(x:\ttype_1)}{\ttype_2}   & x \notin \FV(\ttype_2) \\\\
    \Pair{\ttype_1}{\ttype_2}   & := & \Pair{(x:\ttype_1)}{\ttype_2}    & x \notin \FV(\ttype_2) \\\\
    \pair{\texpr_1}{\texpr_2}   & := & \pair{x=\texpr_1}{\texpr_2}      & x \notin \FV(\texpr_2) \\\\
\end{array}
\\]

### Field lookups

Here we define field lookups at both the type and the value level:

\\[
\\DeclareMathOperator{\field}{field} \\
\\
\begin{array}{lll}
    \field((x:\ttype_1) \times \ttype_2, x)   & = & \ttype_1 \\\\
    \field((y:\ttype_1) \times \ttype_2, x)   & = & \field(\ttype_2, x), ~\text{if}~ y \ne x \\\\
    \\\\
    \field(\langle x=\texpr_1, \texpr_2 \rangle, x) & = & \texpr_1 \\\\
    \field(\langle y=\texpr_1, \texpr_2 \rangle, x) & = & \field(\texpr_2, x), ~\text{if}~ y \ne x \\\\
    \\\\
\end{array}
\\]

### List operations

\\[
\\DeclareMathOperator{\index}{index} \\
\\
\begin{array}{lll}
    \index(\texpr_1::\texpr_2, ~0)      & = & \texpr_1 \\\\
    \index(\texpr_1::\texpr_2, ~n)      & = & \index(\texpr_2, ~n-1), ~\text{if}~ n \gt 0 \\\\
    \\\\
\end{array}
\\]

### Representation types

\\[
\\DeclareMathOperator{\repr}{repr} \\
\\
\begin{array}{lll}
    \repr(x)                                & = & x \\\\
    \repr(\texpr : \ttype)                  & = & \repr(\texpr) : \repr(\ttype) \\\\
    \repr(\Pair{(x:\ttype_1)}{\ttype_2})    & = & \Pair{\repr(\ttype_1)}{\repr(\ttype_2)} \\\\
    \repr(\pair{x=\texpr_1}{\texpr_2})      & = & \pair{\repr(\texpr_1)}{\repr(\texpr_2)} \\\\
    \repr(\Unit_{\Binary})                  & = & \Unit_{\Host} \\\\
    \repr(\unit)                            & = & \unit \\\\
    \repr(\Array ~ \ttype ~ \texpr)         & = & \List ~ \repr(\ttype) \\\\
    % could implement Arrays using a dependent struct to reify the length:
    % \pair{\mathsf{len}: \texpr}{\Array ~ \repr(\ttype) ~ \mathsf{len}} \\\\
\end{array}
\\]

TODO

## Semantics

We define a bidirectional type checking algorithm. We'll end up with the
following judgement forms for our syntax:

| name                              | notation                                   | inputs                                   | outputs       |
|-----------------------------------|--------------------------------------------|------------------------------------------|---------------|
| [normalization](#normalization)   | \\(\eval{ \texpr }{ \texpr' }\\)           | \\(\texpr\\)                             | \\(\texpr'\\) |
| [type checking](#type-checking)   | \\(\check{ \Gamma }{ \texpr }{ \ttype }\\) | \\(\Gamma\\), \\(\texpr\\), \\(\ttype\\) |               |
| [type synthesis](#type-synthesis) | \\(\infer{ \Gamma }{ \texpr }{ \ttype }\\) | \\(\Gamma\\), \\(\texpr\\)               | \\(\ttype\\)  |

### Normalization

Since we are designing a dependent type system it is crucial that we first
define normalization. This ensures that we reduce terms before we check them for
equivalence during type checking.

\\[
\boxed{
    \eval{ \texpr }{ \texpr' }
}
\\\\[2em]
\begin{array}{cl}
    \rule{E-VAR}{}{
        \eval{ x }{ x }
    }
    \\\\[2em]
    \rule{E-KIND}{}{
        \eval{ \Kind }{ \Kind }
    }
    \\\\[2em]
    \rule{E-HOST}{}{
        \eval{ \Host }{ \Host }
    }
    \\\\[2em]
    \rule{E-BINARY}{}{
        \eval{ \Binary }{ \Binary }
    }
    \\\\[2em]
    \rule{E-ANN}{
        \eval{ \texpr }{ \texpr' }
        \qquad
        \eval{ \ttype }{ \ttype' }
    }{
        \texpr' : \ttype'
    }
    \\\\[2em]
    \rule{E-PI}{
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \eval{ \ttype_2 }{ \ttype_2' }
    }{
        \eval{ \Arrow{(x:\ttype_1)}{\ttype_2} }{ \Arrow{(x:\ttype_1')}{\ttype_2'} }
    }
    \\\\[2em]
    \rule{E-LAMBDA}{
        \eval{ \texpr }{ \texpr' }
        \qquad
        \eval{ \ttype }{ \ttype' }
    }{
        \eval{ \lambda x.\texpr }{ \lambda x.\texpr' }
    }
    \\\\[2em]
    \rule{E-APP}{
        \eval{ \texpr_1 }{ \lambda x.\texpr_1' }
        \qquad
        \eval{ \subst{\texpr_1'}{x}{\texpr_2'} }{ \texpr_1'' }
    }{
        \eval{ \texpr_1 ~ \texpr_2 }{ \texpr_1'' }
    }
    \\\\[2em]
    \rule{E-SIGMA}{
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \eval{ \ttype_2 }{ \ttype_2' }
    }{
        \eval{ \Pair{(x:\ttype_1)}{\ttype_2} }{ \Pair{(x:\ttype_1')}{\ttype_2'} }
    }
    \\\\[2em]
    \rule{E-INTRO-SIGMA}{
        \eval{ \texpr_1 }{ \texpr_1' }
        \qquad
        \eval{ \texpr_2 }{ \texpr_2' }
    }{
        \eval{ \pair{x=\texpr_1}{\texpr_2} }{ \pair{x=\texpr_1'}{\texpr_2'} }
    }
    \\\\[2em]
    \rule{E-PROJ}{
        \eval{ \texpr_1 }{ \texpr_1' }
        \qquad
        \field(\texpr_1',x) = \texpr_2
    }{
        \eval{ \texpr_1.x }{ \texpr_2 }
    }
    \\\\[2em]
    \rule{E-UNIT}{}{
        \eval{ \Unit }{ \Unit }
    }
    \\\\[2em]
    \rule{E-INTRO-UNIT}{}{
        \eval{ \unit }{ \unit }
    }
    \\\\[2em]
    \rule{E-NIL}{}{
        \eval{ [] }{ [] }
    }
    \\\\[2em]
    \rule{E-CONS}{
        \eval{ \texpr_1 }{ \texpr_1' }
        \qquad
        \eval{ \texpr_2 }{ \texpr_2' }
    }{
        \eval{ \texpr_1 :: \texpr_2 }{ \texpr_1' :: \texpr_2' }
    }
    \\\\[2em]
    \rule{E-SUBSCRIPT}{
        \eval{ \texpr_1 }{ \texpr_1' }
        \qquad
        \eval{ \texpr_2 }{ \texpr_2' }
        \qquad
        \index(\texpr_1', \texpr_2') = \texpr_3
    }{
        \eval{ \texpr_1[\texpr_2] }{ \texpr_3 }
    }
    \\\\[2em]
\end{array}
\\]

### Type checking

Now we get to the main part of typechecking. We supply and expression \\(\texpr\\)
and a type \\(\ttype\\), and check to see if it meets any of the judgements in
the context \\(\Gamma\\). Note that we expect that the type \\(\ttype\\) has been
previously normalized before we start:

\\[
\boxed{
    \check{ \Gamma }{ \texpr }{ \ttype }
}
\\\\[2em]
\begin{array}{cl}
    \rule{C-LAMBDA}{
        \infer{ \Gamma,x:\ttype_1 }{ \texpr }{ \ttype_2 }
    }{
        \check{ \Gamma }{ \lambda x.\texpr }{ \Arrow{(x:\ttype_1)}{\ttype_2} }
    }
    \\\\[2em]
    \rule{C-UNIT}{}{
        \check{ \Gamma }{ \Unit }{ s }
    }
    \\\\[2em]
    \rule{C-INTRO-UNIT}{}{
        \infer{ \Gamma }{ \unit }{ \Unit }
    }
    \\\\[2em]
    \rule{C-NIL}{}{
        \check{ \Gamma }{ [] }{ \List ~ \ttype }
    }
    \\\\[2em]
    \rule{C-CONV}{
        \infer{ \Gamma }{ \texpr }{ \ttype_2 }
        \qquad
        \ttype_1 \equiv_{\alpha} \ttype_2
    }{
        \check{ \Gamma }{ \texpr }{ \ttype_1 }
    }
    \\\\[2em]
\end{array}
\\]

For concision, we allow lambdas to omit their type annotations, so they must be
checked contextually. Type checking lambdas that lack this type annotation is
easy, because we can pull it out of the input type, which we expect to be a
dependent function type.

We want to be able to use the same \\(\Unit\\) type for both binary
descriptions and host descriptions, so this means they must also be checked
contextually.

The flip between checking and synthesis also occurs here. We rely on alpha
equivalence check (\\(\ttype_1 \equiv_{\alpha} \ttype_2\\)) to ensure that the
expected type \\(\ttype_1\\) is equivalent to the inferred type \\(\ttype_2\\).
This could be replaced with a subtyping check in the future.

### Type synthesis

\\[
\boxed{
    \infer{ \Gamma }{ \texpr }{ \ttype }
}
\\\\[2em]
\begin{array}{cl}
    \rule{I-VAR}{
        x:\ttype \in \Gamma
    }{
        \infer{ \Gamma }{ x }{ \ttype }
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
        \check{ \Gamma }{ \ttype }{ \Binary }
        \qquad
        \eval{ \ttype }{ \ttype' }
        \qquad
        \check{ \Gamma }{ \texpr }{ \ttype' }
    }{
        \infer{ \Gamma }{ \texpr : \ttype }{ \ttype' }
    }
    \\\\[2em]
    \rule{I-ANN-HOST}{
        \check{ \Gamma }{ \ttype }{ \Host }
        \qquad
        \eval{ \ttype }{ \ttype' }
        \qquad
        \check{ \Gamma }{ \texpr }{ \ttype' }
    }{
        \infer{ \Gamma }{ \texpr : \ttype }{ \ttype' }
    }
    \\\\[2em]
    \rule{I-APP}{
        \infer{ \Gamma }{ \texpr_1 }{ \Arrow{(x:\ttype_1)}{\ttype_2} }
        \qquad
        \check{ \Gamma }{ \texpr_2 }{ \ttype_1 }
        \qquad
        \eval{ \subst{\ttype_2}{x}{\texpr_2} }{ \ttype_2' }
    }{
        \infer{ \Gamma }{ \texpr_1 ~ \texpr_2 }{ \ttype_2' }
    }
    \\\\[2em]
    \rule{I-PI-BINARY1}{
        \infer{ \Gamma }{ \ttype_1 }{ \Binary }
        \qquad
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \check{ \Gamma,x:\ttype_1' }{ \ttype_2 }{ \Binary }
    }{
        \infer{ \Gamma }{ \Arrow{(x:\ttype_1)}{\ttype_2} }{ \Binary }
    }
    \\\\[2em]
    \rule{I-PI-BINARY2}{
        \infer{ \Gamma }{ \ttype_1 }{ \Host }
        \qquad
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \check{ \Gamma,x:\ttype_1' }{ \ttype_2 }{ \Binary }
    }{
        \infer{ \Gamma }{ \Arrow{(x:\ttype_1)}{\ttype_2} }{ \Binary }
    }
    \\\\[2em]
    \rule{I-PI-HOST}{
        \infer{ \Gamma }{ \ttype_1 }{ \Host }
        \qquad
        \check{ \Gamma }{ \ttype_2 }{ \Host }
    }{
        \infer{ \Gamma }{ \Arrow{\ttype_1}{\ttype_2} }{ \Host }
    }
    \\\\[2em]
    \rule{I-SIGMA-BINARY}{
        \infer{ \Gamma }{ \ttype_1 }{ \Binary }
        \qquad
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \check{ \Gamma,x:\ttype_1' }{ \ttype_2 }{ \Binary }
    }{
        \infer{ \Gamma }{ \Pair{(x:\ttype_1)}{\ttype_2} }{ \Binary }
    }
    \\\\[2em]
    \rule{I-SIGMA-HOST}{
        \infer{ \Gamma }{ \ttype_1 }{ \Host }
        \qquad
        \check{ \Gamma }{ \ttype_2 }{ \Host }
    }{
        \infer{ \Gamma }{ \Pair{\ttype_1}{\ttype_2} }{ \Host }
    }
    \\\\[2em]
    \rule{I-INTRO-SIGMA}{
        \infer{ \Gamma }{ \texpr_1 }{ \ttype_1 }
        \qquad
        \check{ \Gamma }{ \ttype_1 }{ \Binary }
        \qquad
        \infer{ \Gamma,x:\ttype_1 }{ \texpr_2 }{ \ttype_2 }
    }{
        \infer{ \Gamma }{ \pair{x=\texpr_1}{\texpr_2} }{ \Pair{(x:\ttype_1)}{\ttype_2} }
    }
    \\\\[2em]
    \rule{I-INTRO-PAIR}{
        \infer{ \Gamma }{ \texpr_1 }{ \ttype_1 }
        \qquad
        \check{ \Gamma }{ \ttype_1 }{ \Host }
        \qquad
        \infer{ \Gamma }{ \texpr_2 }{ \ttype_2 }
    }{
        \infer{ \Gamma }{ \pair{x=\texpr_1}{\texpr_2} }{ \Pair{\ttype_1}{\ttype_2} }
    }
    \\\\[2em]
    \rule{I-PROJ}{
        \infer{ \Gamma }{ \texpr }{ \ttype_1 }
        \qquad
        \eval{ \ttype_1 }{ \ttype_1' }
        \qquad
        \field(\ttype_1',x) = \ttype_2
    }{
        \infer{ \Gamma }{ \texpr.x }{ \ttype_2 }
    }
    \\\\[2em]
    \rule{I-ARRAY}{}{
        \infer{ \Gamma }{ \Array }{ \Arrow{\Binary}{\Arrow{\mathsf{Nat}}{\Binary}} }
    }
    \\\\[2em]
    \rule{I-LIST}{}{
        \infer{ \Gamma }{ \List }{ \Arrow{\Host}{\Host} }
    }
    \\\\[2em]
    \rule{I-CONS}{
        \infer{ \Gamma }{ \texpr_1 }{ \ttype }
        \qquad
        \check{ \Gamma }{ \texpr_2 }{ \List ~ \ttype }
    }{
        \infer{ \Gamma }{ \texpr_1 :: \texpr_2 }{ \List ~ \ttype }
    }
    \\\\[2em]
    \rule{I-SUBSCRIPT}{
        \infer{ \Gamma }{ \texpr_1 }{ \List ~ \ttype }
        \qquad
        \infer{ \Gamma }{ \texpr_2 }{ \mathsf{Nat} }
    }{
        \infer{ \Gamma }{ \texpr_1[\texpr_2] }{ \ttype }
    }
    \\\\[2em]
\end{array}
\\]

### Parsing

TODO

## Compilation

TODO

To non-dependent backend languages

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
