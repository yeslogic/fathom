# Prototype implementation of a DDL

This is a prototype implementation of a data description language in Prolog.
It is not designed to be efficient, rather it allows us to explore the
flow of data during static checking, and to test the reading of real data.

## Setup

To run the code you will need to install [SWI-Prolog][swi_prolog].
This package can be found on many distributions; on Ubuntu it can be
installed with `sudo apt-get install swi-prolog`.

[swi_prolog]: http://www.swi-prolog.org/

## Running the examples

In the shell, change to the `src` directory then start the interpreter with:

```sh
$ chdir src
$ swipl load.pl
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

