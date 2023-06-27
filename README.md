# Scope Graph-Based Type Checker for a Scala Subset

This project is part of the [Research Project 2023](https://github.com/TU-Delft-CSE/Research-Project) of `TU Delft`.

Our project handles object binding with various imports of Scala, with the following features included:

- object declarations
- type annotated val declarations in objects
- type declarations in objects
- function defs in objects with multiple, n-ary parameter clauses
- lexical scoping
- wildcard imports (`import O.N._`)
- explicit name imports (`import A.B.x`)
- qualified value and type references

Our scoping rules follow the [Scala Specification.](https://www.scala-lang.org/files/archive/spec/2.13/02-identifiers-names-and-scopes.html)

## Installation

To run this research project, you will need `Haskell, Cabal, and GHC (Glasgow Haskell Compiler)` installed on your system. 
Follow the steps below to set up the project:

#### Project Setup:

Once you have Haskell, Cabal, and GHC installed, navigate to the project directory in your terminal and run the following command to set up the project dependencies:

```shell
$ cabal update && cabal build
```

#### Running the Project:

After successfully building the project, you can run it using the following command:



```shell
$ cabal run scala-test
```
This command will execute the tests and display the associated scope graphs.

## Syntax

The grammar used was inspired by the  [official Scala grammar.](https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html).
It includes essential language features related to definitions, objects and import mechanisms.

## Notes on Implementation

Scala paths are of the form: ``P*DEF*WI?(EI|VAL)``

Here:

- P    denotes a lexical "parent" edge induced by nesting block scope
- DEF  denotes a "definition" edge
- WI   denotes a "wildcard" import edge
- EI   denotes an explicitly imported value/type declaration
- VAL  denotes a value declaration
