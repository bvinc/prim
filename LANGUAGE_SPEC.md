# Prim Language Specification

## Overview

Prim is a statically-typed programming language with a focus on simplicity and clarity. It compiles to native machine code via the Cranelift backend.

## Grammar

### Program Structure
```
program        → function*
function       → "fn" IDENTIFIER "(" parameters? ")" ( "->" type )? block
parameters     → parameter ( "," parameter )*
parameter      → IDENTIFIER ":" type
block          → "{" statement* "}"
```

### Statements
```
statement      → let_stmt | expr_stmt
let_stmt       → "let" IDENTIFIER ( ":" type )? "=" expression terminator
expr_stmt      → expression terminator
terminator     → ";" | NEWLINE | "}"
```

### Expressions
```
expression     → equality
equality       → addition ( "==" addition )*
addition       → multiplication ( ( "+" | "-" ) multiplication )*
multiplication → unary ( ( "*" | "/" ) unary )*
unary          → ( "-" ) unary | call
call           → primary ( "(" arguments? ")" )*
primary        → INT_LITERAL | FLOAT_LITERAL | IDENTIFIER | "(" expression ")"
arguments      → expression ( "," expression )*
```

### Types
```
type           → "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" 
               | "usize" | "isize" | "f32" | "f64"
```

## Lexical Rules

### Tokens
- **Keywords**: `fn`, `let`
- **Types**: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `usize`, `isize`, `f32`, `f64`
- **Operators**: `+`, `-`, `*`, `/`, `=`, `==`, `->`, `(`, `)`, `{`, `}`, `,`, `:`, `;`
- **Literals**: 
  - Integer: `42`, `0`, `123u32` 
  - Float: `3.14`, `2.0f32`
- **Identifiers**: `[a-zA-Z_][a-zA-Z0-9_]*`
- **Built-ins**: `println` (special function)

### Whitespace and Comments
- Whitespace (spaces, tabs) is ignored except for token separation
- Newlines are significant as statement terminators
- Comments are not yet supported

## Semantic Rules

### Variables
- Variables must be declared with `let` before use
- Variables are immutable after declaration
- Type annotations are optional; types are inferred when omitted
- Variable names must be unique within their scope

### Functions
- Every program must have a `main` function with signature `fn main()`
- Functions can have parameters with required type annotations
- Functions can have optional return types
- Function calls require parentheses even with no arguments
- `println` is a built-in function that accepts one argument

### Type System
- Integer types: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `usize`, `isize`
- Floating-point types: `f32`, `f64`  
- No implicit type conversions
- Type inference for `let` bindings without explicit types

### Operator Precedence (highest to lowest)
1. Function calls `()`
2. Unary minus `-`
3. Multiplication `*`, Division `/`
4. Addition `+`, Subtraction `-`
5. Equality `==`

### Statement Termination
Statements can be terminated by:
- Semicolon `;` (explicit termination)
- Newline (implicit termination)
- Closing brace `}` (end of block)

Examples:
```prim
fn main() {
    let x = 1;    // semicolon termination
    let y = 2     // newline termination (newline follows)
    println(x + y)  // brace termination
}
```

## Built-in Functions

### println
- Signature: `println(value)` where value is any printable type
- Prints the value followed by a newline to stdout
- Returns no value

## Error Handling

The compiler provides clear error messages for:
- Syntax errors with position information
- Type mismatches
- Undefined variables
- Missing main function
- Statements outside function scope

## Memory Model

- All variables are stack-allocated
- No heap allocation or garbage collection
- Function parameters are passed by value
- No pointers or references

## Examples

### Basic Program
```prim
fn main() {
    let x = 42
    println(x)
}
```

### Function with Parameters
```prim
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let result = add(5, 3)
    println(result)
}
```

### Complex Expression
```prim
fn main() {
    let x = 10
    let y = 5
    let result = (x + y) * 2 - x / y
    println(result)
}
```