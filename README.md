# impe

An imperative, interpreted, simple, extendible language.

**interpreting** consists of these steps:

1. **lexing**
2. **parsing**
3. **typing**
4. **executing**

## Grammar

<!-- TODO -->

## Tools

This project demonstrates the following tools:

- [parsec](https://hackage.haskell.org/package/parsec)
- [mtl](https://hackage.haskell.org/package/mtl)
- [lens](https://hackage.haskell.org/package/lens)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

## Features

- branching (if-then-else)
- looping (while-do)
- functions
  - nested function definitions
  - nested scope capturing
- procedures (functions that don't return a value)
- variables are references

<!-- TODO -->

## Motivation

<!-- TODO -->

## To Do

- [ ] organize effects by newtype-wrapping logs and errors
- [ ] abstract away interpreting programs/instruction/expression somehow
- [ ] Main.Config module for grammar/parsing of command-line options
- [ ] redefine synonyms to use `Member <effect> r => Sem r <type>` rather than
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
