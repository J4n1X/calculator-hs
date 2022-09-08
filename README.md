# Calculator in Haskell

This project aims to implement a parser and interpreter for Math expressions in Haskell.

It currently can:
  - Calculate simple arithmetics
  - Define and Reference Variables
  - Declare and Call Functions

It aims to also:
  - Support branching
  - Use static typing
  - Compile to native assembly or transpile to a language that does so

## Usage

Simply compile the project and run it using `stack run`. You will enter a REPL in which you can run code.

To get help, type `:help`

## Syntax

### Function Definition

Functions currently can only consist of a single expression, but support for full statement bodies will be added later.
```
foo(x, y) = x + y
```

### Variables

To define a variable, it's as simple as assigning a value to it.
```
x = 10
```

Referencing is just as easy. Simply enter it's identifier whenever you need it.
```
x = 10
x + x # Result: 20
```

### Comments 

Comments are simple as well, simply prepend your comment with a **#**.

### Exposed Functions

The following functions are exposed from Haskell to get access to more advanced calculations:
  - exp
  - sqrt
  - log
  - sin
  - tan
  - cos
  - asin
  - atan
  - acos
  - sinh
  - tanh
  - cosh
  - asinh
  - atanh
  - acosh
  - ** (power)
  - logBase

  They can be called freely:
  ```
  logBase(2, 4) # Result: 2
  ```

