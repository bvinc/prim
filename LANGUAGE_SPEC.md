# Prim Language Specification

## Overview

Prim is a statically-typed programming language with a focus on simplicity and clarity. It compiles to native machine code via the Cranelift backend.

## Grammar

### Program Structure
```
module_unit    → module_header? import* ( struct | function )*
module_header  → "mod" IDENTIFIER terminator
import         → "import" import_path ( "." IDENTIFIER | ".{" IDENTIFIER ( "," IDENTIFIER )* "}" )? terminator
import_path    → IDENTIFIER ( "." IDENTIFIER )*

program        → function*                    # single-file mode
function       → "fn" IDENTIFIER "(" parameters? ")" ( "->" type )? block
parameters     → parameter ( "," parameter )*
parameter      → IDENTIFIER ":" type
block          → "{" statement* "}"
```

Notes:
- In multi-file mode, a module is a directory of `.prim` files. Each file must begin with `mod <name>`.
- `import name` pulls in another module located as a sibling directory (see Modules section).
- The CLI accepts either a single file or a directory. When given a directory, files are merged after stripping the `mod` and `import` headers; `import`s are resolved first.

### Statements
```
statement      → let_stmt | loop_stmt | break_stmt | expr_stmt
let_stmt       → "let" IDENTIFIER ( ":" type )? "=" expression terminator
loop_stmt      → "loop" block
break_stmt     → "break" terminator
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
primary        → INT_LITERAL | FLOAT_LITERAL | STRING_LITERAL | CHAR_LITERAL | BOOL_LITERAL | IDENTIFIER | "(" expression ")"
arguments      → expression ( "," expression )*
```

### Types
```
type           → "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" 
               | "usize" | "isize" | "f32" | "f64" | "bool"
```

## Lexical Rules

### Tokens
- **Keywords**: `fn`, `let`, `loop`, `break`, `if`, `true`, `false`, `mod`, `import`
- **Types**: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `usize`, `isize`, `f32`, `f64`, `bool`
- **Operators**: `+`, `-`, `*`, `/`, `=`, `==`, `->`, `(`, `)`, `{`, `}`, `,`, `:`, `;`
- **Literals**: 
  - Integer: `42`, `0`, `123u32` 
  - Float: `3.14`, `2.0f32`
  - String: `"hello world"`, `"with\nescapes"`
  - Character: `'a'`, `'\n'`, `'\''`
  - Boolean: `true`, `false`
- **Identifiers**: `[a-zA-Z_][a-zA-Z0-9_]*`
- **Built-ins**: `println` (special function)

### Whitespace and Comments
- Whitespace (spaces, tabs, newlines) is ignored except for token separation
- Operator spacing:
  - Binary infix operators must have whitespace on both sides or on neither side.
  - Prefix unary operators (`+`, `-`, `*`) require whitespace on the left and no whitespace on the right.
  - Postfix unary operators require no whitespace on the left and whitespace on the right (none defined yet).
- **Line comments**: `// comment text` - from `//` to end of line
- Comments are completely ignored by the parser

### String and Character Literals
- **String literals**: Enclosed in double quotes `"text"`
  - Support escape sequences: `\n`, `\t`, `\r`, `\\`, `\"`
  - Can span multiple lines (if escaped properly)
  - Empty strings allowed: `""`
- **Character literals**: Enclosed in single quotes `'c'`
  - Exactly one character (or escape sequence)
  - Support escape sequences: `\n`, `\t`, `\r`, `\\`, `\'`
  - Examples: `'a'`, `'\n'`, `'\''`

## Semantic Rules

### Variables
- Variables must be declared with `let` before use
- Variables are immutable after declaration
- Type annotations are optional; types are inferred when omitted
- Variable names must be unique within their scope

### Functions
- Every binary must have a `main` function with signature `fn main()`
- Functions can have parameters with required type annotations
- Functions can have optional return types
- Function calls require parentheses even with no arguments
- `println` is a built-in function that accepts one argument

### Type System
- Integer types: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `usize`, `isize`
- Floating-point types: `f32`, `f64`
- Boolean type: `bool` (values: `true`, `false`)
- No implicit type conversions
- Type inference for `let` bindings without explicit types

### Control Flow
- `loop { ... }` executes its body repeatedly without an implicit exit condition.
- `break` terminates the innermost enclosing `loop` and resumes execution after that loop's block.
- `break` requires a statement terminator (newline, `;`, or `}`) and is rejected outside of loops.
- Code that appears after a `break` inside the same block is still parsed and type-checked even though it is unreachable at runtime.

### Operator Precedence (highest to lowest)
1. Function calls `()`
2. Unary minus `-`
3. Multiplication `*`, Division `/`
4. Addition `+`, Subtraction `-`
5. Equality `==`

### Statement Termination
Statements can be terminated by:
- Semicolon `;` (optional, explicit termination)
- Closing brace `}` (end of block)

When no semicolon is present, the parser treats the current statement as complete as soon as the next token cannot continue it, then starts a new statement.

Examples:
```prim
fn main() {
    let x = 1
    let y = 2
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

## Modules

- Definition: A module is a directory containing one or more `.prim` files. Each file starts with a header `mod <name>` declaring the module name used by that file.
- Entry modules: For binaries, the entry module must be named `main` and must define `fn main()`.
  - The CLI accepts a module directory or its `cmd/` subdirectory. When `cmd/` is used, imports are resolved relative to the parent directory.
- Imports: Top-of-file `import` statements declare dependencies on sibling modules located under the current module root.
  - `import foo.bar` loads module `foo/bar` when it exists; if that module is missing but `foo/bar` defines a top-level item named `bar`, the identifier import falls back to that symbol.
  - `import foo.bar.Baz` first searches for module `foo/bar/Baz`; when absent it selects symbol `Baz` from `foo.bar`.
  - `import foo.bar.{Baz, Quux}` loads only the listed definitions from `foo.bar` and leaves the rest of the module untouched.
  - When a module and a symbol share the same name, the module is preferred; use braces to import the symbol explicitly.
  - Imports must appear before any `struct` or `fn` definitions (after the `mod` header, if present).
  - Import cycles are not allowed; the compiler reports an error on cycles.
  - Resolution order: imported modules are compiled/merged before the current module.
- Visibility and namespacing (current behavior): Imported symbols become available unqualified (merged compilation unit). Names across the combined modules must be unique.
  - Future work may introduce explicit namespacing and `module::symbol` references.

Example selective imports:
```prim
import std.io.print_str          // single symbol, falls back to std.io when std.io.print_str module is absent
import util.math.{Vector2, dot}  // explicit list from util/math
// import util.math.Vector2      // loads util/math module if it exists; use braces to force the struct symbol
```

### Examples

Single-file (no module header required):
```prim
fn main() {
    println(1)
}
```

Multi-file module in a directory `app/`:
```prim
// app/main.prim
mod main
import util

fn main() {
    println(add2(5))
}
```

```prim
// app/util/lib.prim
mod util

fn add2(x: i64) -> i64 { x + 2 }
```

Nested with cmd/ entry:
```prim
// tool/cmd/main.prim
mod main
import core

fn main() { println(run()) }
```

```prim
// tool/core/lib.prim
mod core
fn run() -> i64 { 42 }
```

## Examples

### Basic Program
```prim
fn main() {
    let x = 42
    println(x)
}
```

### Boolean Usage
```prim
fn main() {
    let flag: bool = true
    let active = false
    println(flag)
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
