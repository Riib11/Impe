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
- [optparse-simple](https://hackage.haskell.org/package/optparse-simple)

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

- pointers (perhaps define whole new language for this)
- better type and execution errors e.g. line numbers and better messages (redue
  the amount of abstraction for errors and stuff i.e. give message in place for
  each error)
- rename "Typing" to "Typechecking
