# impe

An imperative, interpreted, simple, extendible language.

**interpreting** consists of these steps:

1. **parsing**
2. **typing**
3. **executing**

## Grammar

<!-- TODO -->

## Tools

This project demonstrates the following tools:

- [polysemy](https://hackage.haskell.org/package/polysemy)
- [lens](https://hackage.haskell.org/package/lens)
- [mtl](https://hackage.haskell.org/package/mtl)
- [parsec](https://hackage.haskell.org/package/parsec)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

## Features

- mutable variables
- branching (`if ... then ... else ...`)
- looping (`while ... do ... `)
- functions
  - nested function definitions
  - nested scope capturing
- procedures (functions that don't return a value)

<!-- TODO -->

## Motivation

<!-- TODO -->

## Prelude

The following functions are pre-defined:

```
_~_  :: (bool) -> bool
_&&_ :: (bool, bool) -> bool
_||_ :: (bool, bool) -> bool
show_bool :: (bool) -> string

_+_  :: (int, int) -> int
_-_  :: (int, int) -> int
_*_  :: (int, int) -> int
_/_  :: (int, int) -> int
_^_  :: (int, int) -> int
_%_  :: (int, int) -> int
_=_  :: (int, int) -> bool
_<_  :: (int, int) -> bool
_<=_ :: (int, int) -> bool
_>_  :: (int, int) -> bool
_>=_ :: (int, int) -> bool
show_int :: (int) -> string

_<>_  :: (string, string) -> string
write :: (string) -> void
read  :: () -> string
```

## To Do

- [x] optimize execution by stopping execution of function once a return has
      been reached
- [x] parsing infixed operators
- [x] `Initialization` grammar construct
- [x] finish implementing writing to output file (all at once) - rather than
      handling logs by IO printing, instead tell writer at `startInterpret`'s
      level
- [x] finish implementing reading from input file
- [x] organize imports/exports names

- [x] better organization for executing and parsing primitive functions and
      variables
- [x] error(exception) and warning codes (as ADTs)
- [x] parse config of verbosity
- [x] customize verbosity of logs throughout program
- [x] organize effects by newtype-wrapping logs and errors
- [x] abstract away interpreting programs/instruction/expression somehow
- [x] Main.Config module for grammar/parsing of command-line options
- [x] redefine synonyms to use `Member <effect> r => Sem r <type>` rather than
      explicitly defining a list of effects
- [x] error on duplicate names in same scope (requires using nested scoping in
      typechecking, so probably would be a good idea to just make a general data
      structure to use for both).
- [x] finish making new modulization for all REPL stuff
  - grammar
  - parsing
  - typing/executing/interpreting
  - commands
  - interface

## Experimental

- pointers (perhaps define whole new language for this)
- better type and execution errors e.g. line numbers and better messages (redue
  the amount of abstraction for errors and stuff i.e. give message in place for
  each error)
