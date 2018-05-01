# Prototype implementation of a DDL

This is a prototype implementation of a data description language in Prolog.
It is not designed to be efficient, rather it allows us to explore the
flow of data during static checking, and to test the reading of real data.

## Setup

To run the code you will need to install [SWI-Prolog][swi-prolog].
This package can be found on many distributions; on Ubuntu it can be
installed with `sudo apt-get install swi-prolog`.

[swi-prolog]: http://www.swi-prolog.org/

## Running the examples

In the shell, change to the `src` directory then start the interpreter with:

```sh
cd src
swipl load.pl
```

The full suite of examples can be run with:

```prolog
?- run.
```

or individual examples can be run with:

```prolog
?- runtest(Name).           % e.g. runtest(sudoku).
```

Note that the full suite is run with a "failure-driven loop", which means
that "false" will be printed at the end. This does not indicate failure
of a test case, only that there are no more test cases.

## Tracing Prolog execution

Tracing can be switched on in SWI-Prolog by entering the following:

```prolog
?- trace.
```

This will execute the next goal in trace mode.

A more convenient way to start debugging is to set a spy point on
the predicate you are interested in. The goal:

```prolog
?- spy(repr/4).
```

sets a spy point on the `repr/4` predicate (that is, the predicate named
`repr` which has four arguments), and also puts Prolog into debug mode.
In subsequent goals, execution will stop when it hits a "port" of the
predicate. For example:

```prolog
?- spy(repr/4).
% Spy point on repr/4
true.

[debug]  ?- runtest(xyz).
Test: xyz
 * Call: (9) repr([], [], sigma(x, int(16, eint(-32768), eint(32767)), sigma(y, int(16, eint(-32768), eint(32767)), sigma(z, int(16, eint(-32768), eint(32767)), unit))), _G453) ?
```

The first port shown here is the `Call` port, which shows the instantiation
of `repr/4` at the time it is first called. The `_G453` argument represents
something that is not yet instantiated.

At the `?` prompt you can hit `c` (or space/return) to "creep" to the next
port, or hit `s` to skip to the end. Type `?` for a list of other commands.

Since Prolog execution can involve backtracking, debuggers use a "4-port"
model of tracing. Aside from the expected `Call` and `Exit` ports, there
are also `Redo` and `Fail` ports, which indicate that we are re-entering
a goal for more solutions, and that we have exhausted all solutions,
respectively. SWI-Prolog additionally has ports `Unify` and `Exception`;
see [the documentation][debug-docs] for more
information.

[debug-docs]: http://www.swi-prolog.org/pldoc/man?section=debugoverview
