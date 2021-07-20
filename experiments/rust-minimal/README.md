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
&emsp;| `?` _name_?\
&emsp;| `Type`\
&emsp;| `(` _surface.term_ `)`

## Core language

_core.index_ ::= `last` | `prev(`_core.index_`)`\
_core.level_ ::= `first` | `next(`_core.level_`)`

_core.term_ ::=\
&emsp;| `bound-var(`_core.index_`)`\
&emsp;| `problem-var(`_core.level_`)`\
&emsp;| `ann(`_core.term_`,` _core.term_`)`\
&emsp;| `let(`_name_`,` _core.term_`,` _core.term_`,` _core.term_`)`\
&emsp;| `universe`\
&emsp;| `function/type(`_name_`,` _core.term_`,` _core.term_`)`\
&emsp;| `function/intro(`_name_`,` _core.term_`)`\
&emsp;| `function/elim(`_core.term_`,` _core.term_`)`

_core.value_ ::=\
&emsp;| `stuck(`_core.head_`,` _core.elim_*`)`\
&emsp;| `universe`\
&emsp;| `fun/type(`_core.value_`,` _core.closure_`)`\
&emsp;| `fun/intro(`_core.closure_`)`

_core.head_ ::=\
&emsp;| `bound-var(`_core.index_`)`\
&emsp;| `problem-var(`_core.level_`)`

_core.elim_ ::=\
&emsp;| `function(`_core.value_`)`

_core.closure_ ::=\
&emsp;| `(`_core.env_`,` _core.term_`)`

_core.env_ ::=\
&emsp;| _core.env_`,` _core.value_\
&emsp;| `empty`

_core.context_ ::=\
&emsp;| _core.env_`,` _core.value_ `:` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document evaluation, readback, checking and synthesis

## Elaboration

_elab.context_ ::=\
&emsp;| _elab.context_`,` _name_ `:` _core.value_ `=` _core.value_\
&emsp;| `empty`

> **TODO:**
>
> document lookup, checking and synthesis
