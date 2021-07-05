# Minimal language experiment

## Surface language

_surface.term_ ::=\
&emsp;| _surface.let-term_\
&emsp;| _surface.let-term_ `:` _surface.term_

_surface.let-term_ ::=\
&emsp;| _surface.fun-term_\
&emsp;| `let` _name_ `:` _surface.fun-term_ `=` _surface.let-term_ `;` _surface.let-term_

_surface.fun-term_ ::=\
&emsp;| _surface.app-term_\
&emsp;| `fun` `(` _name_ `:` _surface.term_ `)` `->` _surface.let-term_\
&emsp;| `fun` _name_ `=>` _surface.let-term_

_surface.app-term_ ::=\
&emsp;| _surface.atomic-term_\
&emsp;| _surface.app-term_ _surface.atomic-term_

_surface.atomic-term_ ::=\
&emsp;| _name_\
&emsp;| `Type`\
&emsp;| `(` _surface.term_ `)`

## Core language

_core.index_ ::= `last` | `prev(`_core.index_`)`\
_core.level_ ::= `first` | `next(`_core.level_`)`

_core.term_ ::=\
&emsp;| `var(`_core.index_`)`\
&emsp;| `ann(`_core.term_`,` _core.term_`)`\
&emsp;| `let(`_name_`,` _core.term_`,` _core.term_`,` _core.term_`)`\
&emsp;| `universe`\
&emsp;| `fun/type(`_name_`,` _core.term_`,` _core.term_`)`\
&emsp;| `fun/intro(`_name_`,` _core.term_`)`\
&emsp;| `fun/elim(`_core.term_`,` _core.term_`)`

_core.value_ ::=\
&emsp;| TODO

_core.elim_ ::=\
&emsp;| TODO

_core.closure_ ::=\
&emsp;| TODO

_core.env_ ::=\
&emsp;| _core.env_`,` _core.value_\
&emsp;| `empty`

TODO: evaluation, readback, normalisation

## Elaboration

_elab.context_ ::=\
&emsp;| _elab.context_`,` `(`_name_ `:` _core.value_ `=` _core.value_`)`\
&emsp;| `empty`

TODO: lookup, context-to-env, checking and synthesis
