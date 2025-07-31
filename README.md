Prim is a programming language that values simplicity, safety, and a useful and rich type system.  It draws inspiration from both rust and go.

Primitive integer types: u8, i8, u16, i16, u32, i32, u64, i64, usize, isize.
Primitive floating point types: f32, f64.

## Let expressions

```
let x: u32 = 0
let x: u32 = 0u32
let x = 0u32
```

## Structs

```
struct Point {
    x: f64,
    y: f64,
}
```

## Functions

```
fn double(x: u32) -> u32 {
    let prod = x*x
    prod // return can be omitted
}
```

## Methods

```
impl Point {
    fn x(&self) -> f64 {
        self.x
    }
}
```

## Control Flow

```
if x == 5 {
    println("It is 5")
}
```

