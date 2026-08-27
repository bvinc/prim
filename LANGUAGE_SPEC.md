# Prim Language Specification

## Overview

Prim is a statically-typed programming language with a focus on simplicity, safety, and concurrency. It compiles to WebAssembly (wasm32 + WASI), run via wasmtime. Prim uses ownership, move semantics, and **second-class references** for memory safety (no garbage collection) and provides green threads built on the WebAssembly stack-switching (typed-continuations) proposal, where the engine manages each task's stack.

The memory model — ownership, moves, `Drop`, and the `read`/`mut`/`own` binding
modes — is specified in [`MEMORY_MODEL.md`](MEMORY_MODEL.md). This document
describes the grammar and semantics of the core language.

## Grammar

### Program Structure
```
module_unit    → module_header? import* ( struct | function )*
module_header  → "mod" IDENTIFIER terminator
import         → "import" import_path ( "." IDENTIFIER | ".{" IDENTIFIER ( "," IDENTIFIER )* "}" )? terminator
import_path    → IDENTIFIER ( "." IDENTIFIER )*

program        → function*                    # single-file mode
function       → "unsafe"? "fn" IDENTIFIER "(" parameters? ")" ( "->" type )? block
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
primary        → INT_LITERAL | FLOAT_LITERAL | STRING_LITERAL | CHAR_LITERAL | BOOL_LITERAL | IDENTIFIER | "(" expression ")" | "unsafe" block
arguments      → expression ( "," expression )*
```

### Types
```
type           → "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" 
               | "usize" | "isize" | "f32" | "f64" | "bool"
               | "Array" "[" type "," const "]" | "Vec" "[" type "]"
               | "fn" "(" type ("," type)* ")" "->" type
```

## Lexical Rules

### Tokens
- **Keywords**: `fn`, `let`, `loop`, `break`, `if`, `true`, `false`, `mod`, `import`, `struct`, `enum`, `impl`, `trait`, `type`, `match`, `while`, `for`, `in`, `return`, `const`, `read`, `mut`, `own`
- **Types**: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `usize`, `isize`, `f32`, `f64`, `bool`
- **Operators**: `+`, `-`, `*`, `/`, `=`, `==`, `->`, `(`, `)`, `{`, `}`, `,`, `:`, `;`
- **Literals**: 
  - Integer: `42`, `0`, `123u32` (radix prefixes `0b`/`0o`/`0x` and `_` digit-group separators are supported: `0xFF_FFu16`, `1_000`)
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
- Variables are immutable after declaration unless declared `let mut`
- Type annotations are optional; types are inferred when omitted
- Variable names must be unique within their scope

### Functions
- Every binary must have a `main` function with signature `fn main()`
- Functions can have parameters with required type annotations
- Functions can have optional return types
- Function calls require parentheses even with no arguments
- `println` is a built-in function that accepts one argument

### Blocks (closures)
- A block is a non-capturing, first-class function value written `|a, b| { ... }`
  (or `|| { ... }` for no parameters), Smalltalk/Swift/Kotlin style.
- A block's value is its trailing expression — the last statement without a
  trailing semicolon — not `return` (blocks do not support `return`).
- Parameter types are optional: `|e: i32| { ... }` annotates one, `|e| { ... }`
  infers it from context.
- A **trailing block** after a call is passed as the final argument:
  `get(v, i) |e| { ... }` is `get(v, i, |e| { ... })`. (Only a single `|`
  is a block; `||` remains logical-or. Because `|` also means bitwise-or,
  parenthesize when mixing a trailing block with a tighter operator, e.g.
  `x + (get(v, i) |e| { e })`.)
- Blocks are typed `fn(T, U) -> R` (structurally equal when their parameter and
  return types match) and are carried as a 4-byte function-table index.
- A block body may name only its own parameters, its `let` bindings, module
  functions, and globals — referencing an enclosing local is a compile-time
  error ("blocks are non-capturing").

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
Statements are terminated by:
- A newline (the usual case — no semicolon needed)
- Semicolon `;` (explicit; only required to put two statements on one line)
- Closing brace `}` (end of block)

```prim
fn main() {
    let x = 1
    let y = 2
    println(x + y)
}
```

### Line Continuation
Prim follows Go's rule for deciding whether a newline ends a statement or
continues it onto the next line. The decision depends only on the **last token
of the line**, never on how the next line starts:

- A newline **ends** the statement when the last token can end one: an
  identifier, a literal, `return`/`break`, or a closing `)`, `]`, or `}`.
- A newline **continues** the statement when the last token cannot end one: a
  binary operator, `=`, `,`, `.`, or an open `(`, `[`, or `{`.

So a long expression must break **after** an operator, not before it:

```prim
let x = 1 +      // ends in `+` → continues
    2 +
    3            // ends in `3` → statement ends here

let y = 1
    + 2          // previous line ended in `1` → `let y = 1` is complete;
                 // `+ 2` is a separate statement
```

`return` ends a statement, so its value must begin on the same line; a newline
right after `return` is a bare return:

```prim
return value     // returns `value`
return           // bare return — the next line is a new statement
```

Method chains continue only when the line ends with the `.`, since a line that
ends in `)` is already a complete statement:

```prim
let a = x.
    foo().
    bar()        // continues: each line ends in `.`

let b = x
    .foo()       // does NOT continue: `x` ends the statement, `.foo()` is an error
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

Prim has no garbage collection (by design — this is a permanent decision).
Memory is managed through **ownership** and **compile-time move checking**:
each value has one owner; assignment and argument passing move (except
copyable scalar and pointer values, which copy); values are destroyed at
statically known points via `Drop`. The full model — including moves, copies,
destructors, representation, and panics — is in
[`MEMORY_MODEL.md`](MEMORY_MODEL.md). What follows is the ownership and
reference surface as it appears in the language.

Pointers `*const T` and `*mut T` are raw, unmanaged memory (the unsafe core's
escape hatch), with explicit dereference — they are not the safe reference
mechanism; `read`/`mut` are.

Raw-pointer powers — `*p` (load/store), integer↔pointer or byte-region↔typed-
pointer reinterpretation (`at`, `from_addr`), `drop_in_place`, and the
allocator — are available only inside an `unsafe` context: the body of an
`unsafe fn`, or an `unsafe { ... }` block. Calling an `unsafe fn` likewise
requires an `unsafe` context. Naming, storing, and passing pointers as opaque
values is always allowed, as is computing or inspecting an address (`null`,
`addr`, and the wrapping `add`/`sub`/`byte_*` arithmetic) — mirroring Rust,
where only memory access and pointer fabrication are `unsafe`. `unsafe` does
not disable the normal checks (move dataflow, `Drop`, bounds); it marks where
the author upholds the raw-memory invariants the compiler cannot prove.

### Ownership and Borrowing (second-class references)

Prim has ownership semantics with compile-time move checking (no garbage
collection; memory safety enforced entirely at compile time):

- Each value has a single owner. When the owner goes out of scope, the value is
  dropped.
- References are **second-class**: `read` / `mut` / `own` exist only as
  *parameter modes*, *match-arm bindings*, and *call-site argument marks* —
  never as type modifiers, `let` bindings, or stored values. There is no
  reference type, no `&T`/`&mut T`, no borrow expressions, and no returned
  references.
  - `fn len(read v: Vec[T])` — a read-only borrow, released on return.
  - `fn push(mut v: Vec[T], own x: T)` — `mut` is writable and released on
    return (the callee's writes are visible to the caller); `own` takes
    ownership (a move).
  - `match mut e { Some(mut v) => ... }` — arm bindings borrow like parameters;
    `mut` bindings write back into the scrutinee. Bare arm bindings are `read`.
    A `mut` arm binding requires the explicit `match mut` (enforced): an
    exclusive write-back through a bare/`read` match would mutate through a
    read-only access. Consumption is always inferred from the arms — `match own
    e` is documentation of the inferred consume (rejected if the arms don't
    move a payload), and a consuming match cannot borrow a payload out of the
    scrutinee.
  - Call sites mark the mode: `f(mut x)`, `f(own x)`, `f(read x)`.
- A `let` binding always *moves* a non-`Copy` RHS into the new binding:
  `let x = a` transfers ownership of `a` (a later use of `a` is a compile
  error); `let own x = a` is the explicit, redundant spelling. Copy values
  copy. `let read x` is a parse error (a local cannot hold a second-class
  borrow); `let mut x` is a mutable owned local.
- A borrow parameter has the plain type `T` inside the body: storing it,
  returning it, moving out of it, or boxing it into a trait object is rejected
  by the move checker. Modes are erased before code generation.
- Access is copies plus whole-structure methods: `Vec.get`/`Array.get` return
  a copy, enforced via a `T: Copy` bound on the method; reading a non-`Copy`
  element (which would alias the slot and double-free on drop) is a type error
  at the call site. `Vec.set`/`Vec.push`/`Vec.swap` mutate in place, and
  match arms read enum payloads via `read`/`mut` bindings.

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
import std.io.println            // single symbol, falls back to std.io when std.io.println module is absent
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
