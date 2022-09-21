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

Functions can either consist of a single expression:
```
foo(x: int, y: int): int = x + y
```

Or they can be built out multiple expressions seperated by semicolons, with the last expression being the returned result:
```
foo(x: int): int = {
  y: int = x * 10;
  y + x
}
```

### Variables

To define a variable, it's as simple as giving it a name, a type, then assigning a value to it.
```
x: int = 10
```

Referencing is just as easy. Simply enter it's identifier whenever you need it.
```
x: int = 10
x + x # Result: 20
```

### Comments 

Comments are simple as well, simply prepend your comment with a **#**.

### Exposed Functions

The following functions are exposed from Haskell to get access to more advanced calculations:
  - exp(float): float
  - sqrt(float): float
  - log(float): float
  - sin(float): float
  - tan(float): float
  - cos(float): float
  - asin(float): float
  - atan(float): float
  - acos(float): float
  - sinh(float): float
  - tanh(float): float
  - cosh(float): float
  - asinh(float): float
  - atanh(float): float
  - acosh(float): float
  - pow(float, float): float
  - logBase(float): float

  They can be called freely:
  ```
  logBase(2.0, 4.0) # Result: 2
  ```

