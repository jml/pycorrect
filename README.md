# pycorrect

[![Build Status](https://travis-ci.org/jml/pycorrect.svg?branch=master)](https://travis-ci.org/jml/pycorrect)

Analyzes Python code for correctness.

## Status

Currently has some experimental code for figuring out where identifiers are
defined, and which identifiers are references to them.

There is a very simple executable that prints out the unresolved references,
e.g.

```
$ pycorrect foo.py
Unresolved references in foo.py:
* TypeError
* super
* __name__
```

As you can see, we are not currently very clever at analyzing the code.

## Layout

There is:

* a `Scope` module that defines a rudimentary model of scopes and
  environments that is meant to be language agnostic.
* a `Python` module that uses `Scope` to define Python variable binding &
  reference rules
* `Types` defines a single thing, and is a legacy of the parent codebase

The top-level `PyCorrect` module is a hold-over from the `tasty-travis` Stack
template, and really ought to be turned into something that re-exports the
"public" APIs.

## Future

This is currently unfunded and unmaintained. Were I (jml) to work on it, I
would start by getting a reliable report of undefined names.

No doubt there are many improvements that could be done to make this more
idiomatic Haskell. This is just something that I whipped together fairly
quickly, so please don't assume that I think anything in here is a good idea.

Questions highly welcome.

## Contributing

Build the project:

```
stack build
```

Run the binary:

```
stack exec pycorrect
```

Run the tests:

```
stack test
```

Run the benchmarks:

```
stack bench
```

Generate documentation:

```
stack haddock
```
