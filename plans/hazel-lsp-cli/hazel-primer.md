# Hazel Language Primer for AI Agents

This document provides a comprehensive guide to Hazel syntax for AI agents writing Hazel programs.

## Overview

Hazel is a gradually-typed functional programming language with:
- **Typed holes**: Incomplete programs are valid and can be executed
- **Live evaluation**: Programs run even with type errors
- **Structure editing**: The parser recovers from syntax errors automatically

## Basic Values and Types

### Literals

```hazel
# Integers (arbitrary precision by default)
42
-17
10000000000000000000000000

# Floats
3.14
0.0
-2.5

# Booleans
true
false

# Strings
"Hello, world!"
"Line 1\nLine 2"

# Unit (like void/None)
()
```

### Basic Types

```hazel
Int       # Arbitrary precision integers
SInt      # Fixed-width system integers
Nat       # Non-negative integers
Float     # Floating point numbers
Bool      # Booleans
String    # Strings
()        # Unit type
```

## Let Bindings

```hazel
# Basic let binding
let x = 5 in x + 1

# With type annotation
let x: Int = 5 in x + 1

# Multiple bindings (chained)
let x = 5 in
let y = 10 in
x + y

# Tuple destructuring
let (a, b) = (1, 2) in a + b

# Sequencing with semicolon
let x = 5 in
print("hello");
x + 1
```

## Operators

### Arithmetic (Integers)

```hazel
x + y      # Addition
x - y      # Subtraction
x * y      # Multiplication
x / y      # Division
x ** y     # Exponentiation
-x         # Negation
```

### Arithmetic (Floats)

Float operators have a dot suffix:

```hazel
x +. y     # Float addition
x -. y     # Float subtraction
x *. y     # Float multiplication
x /. y     # Float division
x **. y    # Float exponentiation
```

### Comparison

```hazel
# Integer comparison
x == y     # Equal
x != y     # Not equal (note: not <>)
x < y      # Less than
x > y      # Greater than
x <= y     # Less or equal
x >= y     # Greater or equal

# Float comparison (dot suffix)
x ==. y
x <. y
# etc.

# String comparison (dollar prefix)
s $== t    # String equality
```

### Boolean

```hazel
x && y     # And
x || y     # Or
!x         # Not
```

### String

```hazel
s ++ t     # Concatenation
```

## Functions

### Basic Functions

```hazel
# Anonymous function
fun x -> x + 1

# With parameter type annotation
fun x: Int -> x + 1

# Let-bound function
let f = fun x -> x + 1 in f(5)

# Multiple arguments (curried)
let f = fun x -> fun y -> x + y in f(2)(3)

# Multiple arguments (tuple parameter)
let f = fun (x, y) -> x + y in f(2, 3)
```

### Recursive Functions

**Important**: Hazel has NO `let rec` keyword. Recursion works automatically when a function has an arrow type annotation.

```hazel
# Recursive function - REQUIRES arrow type annotation
let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl => 1 + length(tl)
end
in length([1, 2, 3])

# Without type annotation, recursion won't work!
# WRONG: let length = fun xs -> ... length(tl) ...
# RIGHT: let length : [Int] -> Int = fun xs -> ...
```

### Mutual Recursion

Use tuple bindings for mutually recursive functions:

```hazel
let (even : Int -> Bool, odd : Int -> Bool) = (
  fun n -> if n == 0 then true else odd(n - 1),
  fun n -> if n == 0 then false else even(n - 1)
)
in even(4)
```

### Function Type Annotations

```hazel
# Single argument
Int -> Int

# Multiple arguments (curried)
Int -> Int -> Int

# Tuple argument
(Int, Int) -> Int

# With complex types
[Int] -> Option(Int)
```

## Conditionals

```hazel
if condition then expr1 else expr2

# Example
let abs = fun x: Int ->
  if x < 0 then -x else x
in abs(-5)
```

## Tuples

```hazel
# Creating tuples
(1, 2)
(1, true, "hello")
(1, (2, 3))

# Tuple types
(Int, Bool)
(Int, Int, String)

# Destructuring
let (a, b) = (1, 2) in a + b
let (x, (y, z)) = (1, (2, 3)) in x + y + z
```

## Lists

```hazel
# List literals
[]
[1, 2, 3]
["a", "b", "c"]

# Cons operator (prepend)
1::2::3::[]      # Same as [1, 2, 3]
hd::tl           # Prepend hd to list tl

# List append
xs @ ys

# List type
[Int]
[String]
[[Int]]          # List of lists
```

## Pattern Matching

```hazel
case expr
| pattern1 => result1
| pattern2 => result2
| _ => default
end
```

### Pattern Examples

```hazel
# Matching integers
case n
| 0 => "zero"
| 1 => "one"
| _ => "many"
end

# Matching lists
case xs
| [] => "empty"
| hd::[] => "one element"
| hd::tl => "multiple elements"
end

# Matching tuples
case pair
| (0, y) => y
| (x, 0) => x
| (x, y) => x + y
end

# Matching options
case opt
| None => 0
| Some(x) => x
end

# Matching custom types
case expr
| Var(name) => ...
| App(f, arg) => ...
end
```

## Algebraic Data Types (ADTs)

### Defining Types

```hazel
# Sum type (variants)
type MyBool = True + False in ...

# With data
type Option = None + Some(Int) in ...

# Recursive types
type List = Nil + Cons(Int, List) in ...

# Multiple fields
type Expr =
+ Var(String)
+ Lam(String, Expr)
+ App(Expr, Expr)
in ...
```

### Using ADTs

```hazel
type Result =
+ Ok(Int)
+ Error(String)
in

let safe_div = fun (x, y) ->
  if y == 0
  then Error("division by zero")
  else Ok(x / y)
in

case safe_div(10, 2)
| Ok(n) => n
| Error(msg) => 0
end
```

## Built-in Option Type

```hazel
# Option(T) is built-in
None              # No value
Some(x)           # Has value x

# Example
let find : (Int, [Int]) -> Option(Int) =
fun (target, xs) ->
case xs
| [] => None
| hd::tl =>
  if hd == target
  then Some(hd)
  else find(target, tl)
end
in find(3, [1, 2, 3, 4])
```

## Holes

Hazel allows incomplete programs with holes:

```hazel
# Explicit hole
let x = ? in x + 1

# Type hole (for "polymorphic" functions)
let id : ? -> ? = fun x -> x in id(5)

# Holes in types
let f : Int -> ? = fun x -> x + 1 in f(5)
```

**Note on "polymorphism"**: Hazel doesn't have implicit type variables. For functions that should work on multiple types, use `?` holes in type annotations. This doesn't enforce type constraints but shows intent:

```hazel
# "Polymorphic" identity - use ? for type parameters
let id : ? -> ? = fun x -> x in

# "Polymorphic" list length
let length : [?] -> Int =
fun xs ->
case xs
| [] => 0
| _::tl => 1 + length(tl)
end
in length([1, 2, 3])
```

## Tests

Test forms check boolean conditions during evaluation:

```hazel
# Basic test
test 2 + 2 == 4 end

# Multiple tests
test 1 + 1 == 2 end;
test 2 * 3 == 6 end;
test 10 / 2 == 5 end;

# Tests in programs
let factorial : Int -> Int =
fun n ->
  if n == 0 then 1
  else n * factorial(n - 1)
in

test factorial(0) == 1 end;
test factorial(5) == 120 end;

factorial(10)
```

## Probes (Debugging)

Probes let you see runtime values of expressions:

```hazel
# Wrap an expression to see its value
let x = 5 in ^^probe(x + 1)

# Inside functions - see value for each call
let f : Int -> Int = fun x -> ^^probe(x * 2) in
f(1); f(2); f(3)
```

**Note**: The `^^probe(...)` syntax is currently the only projector useful for text-based development.

## Number Formats with `use`

Change default number interpretation:

```hazel
# Use Nat (non-negative integers)
let result =
use Nat in
1 + 2 * 5
in result

# Available: Int, SInt, Nat, Float
```

## Comments

```hazel
# This is a comment
let x = 5 in  # Inline comment
x + 1
```

## Program Structure

A complete Hazel program is a single expression. Use `let ... in` to build up:

```hazel
# Define types
type Expr = Var(String) + App(Expr, Expr) in

# Define helper functions
let helper : Expr -> Int = fun e -> ... in

# Define main logic
let process : Expr -> Expr = fun e -> ... in

# Tests
test process(Var("x")) == Var("x") end;

# Final expression (program result)
process(App(Var("f"), Var("x")))
```

## Common Patterns

### List Processing

```hazel
# Map
let map : (Int -> Int) -> [Int] -> [Int] =
fun f -> fun xs ->
case xs
| [] => []
| hd::tl => f(hd)::map(f)(tl)
end
in

# Filter
let filter : (Int -> Bool) -> [Int] -> [Int] =
fun p -> fun xs ->
case xs
| [] => []
| hd::tl =>
  if p(hd)
  then hd::filter(p)(tl)
  else filter(p)(tl)
end
in

# Fold
let fold : (Int -> Int -> Int) -> Int -> [Int] -> Int =
fun f -> fun acc -> fun xs ->
case xs
| [] => acc
| hd::tl => fold(f)(f(acc, hd))(tl)
end
in

map(fun x -> x * 2)([1, 2, 3])
```

### Error Handling

```hazel
type Result =
+ Ok(Int)
+ Error(String)
in

let bind : Result -> (Int -> Result) -> Result =
fun r -> fun f ->
case r
| Error(e) => Error(e)
| Ok(x) => f(x)
end
in

let safe_div : (Int, Int) -> Result =
fun (x, y) ->
  if y == 0 then Error("div by zero") else Ok(x / y)
in

bind(safe_div(10, 2))(fun x -> Ok(x + 1))
```

## Built-in Functions

Hazel provides many built-in functions. See [hazel-builtins.md](./hazel-builtins.md) for the complete list.

### Key Differences from OCaml/ReasonML

**No module namespaces**: Hazel doesn't have a module system yet. Functions that would be `List.length` or `String.concat` in OCaml are just `length` or `string_concat` in Hazel.

```hazel
# OCaml: List.length xs
# Hazel:
length([1, 2, 3])  # => 3

# OCaml: List.map f xs
# Hazel: Note reversed argument order!
map([1, 2, 3], fun x -> x * 2)  # => [2, 4, 6]

# OCaml: String.length s
# Hazel: string_ prefix
string_length("hello")  # => 5
```

**Reversed `map` arguments**: Unlike OCaml's `List.map f xs`, Hazel's `map` takes the list first, then the function: `map(xs, f)`. This applies to similar functions like `filter`, `fold_left`, etc.

```hazel
# Hazel map: list first, function second
map([1, 2, 3], fun x -> x + 1)      # => [2, 3, 4]
filter([1, 2, 3, 4], fun x -> x > 2) # => [3, 4]
fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)  # => 6
```

**String function naming**: String functions use `string_` prefix instead of `String.`:
- `string_length` (not `String.length`)
- `string_concat` (not `String.concat` or `^`)
- `string_sub` (not `String.sub`)
- `string_split`, `string_join`, `string_trim`, etc.

### Commonly Used Builtins

```hazel
# List operations
length(xs)           # List length
hd(xs)               # First element (may fail)
tl(xs)               # Rest of list (may fail)
hd_opt(xs)           # First element as Option
nth(xs, i)           # Element at index i
rev(xs)              # Reverse list
append(xs, ys)       # Concatenate lists (also: xs @ ys)
concat(xss)          # Flatten list of lists
range(start, end)    # Generate [start..end-1]

# Higher-order list functions (note: list first, function second)
map(xs, f)           # Apply f to each element
filter(xs, p)        # Keep elements where p is true
fold_left(xs, f, init)  # Left fold
find(xs, p)          # First element matching p
exists(xs, p)        # Any element matches p
for_all(xs, p)       # All elements match p

# String operations
string_length(s)     # String length
string_concat(s1, s2) # Concatenate (also: s1 ++ s2)
string_sub(s, start, len)  # Substring
string_split(s, sep) # Split by separator
string_join(sep, strings)  # Join with separator

# Conversions
string_of_int(n)     # Int to String
int_of_string(s)     # String to Int
float_of_int(n)      # Int to Float

# Math
abs(n)               # Absolute value
sqrt(x)              # Square root (Float)
sin(x), cos(x), tan(x)  # Trigonometry (Float)
```

## Things to Avoid

1. **`let rec`**: Doesn't exist in Hazel. Use arrow type annotations for recursion.

2. **Implicit type variables**: Use `?` holes instead of type variables like `'a` or `T`.

3. **Explicit polymorphism**: Avoid `typfun`, `poly`, and `@<>` syntax - this feature is incomplete.

4. **Projectors other than probes**: Avoid `^^slider`, `^^checkbox`, etc. - these are GUI features not useful in text.

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Let binding | `let x = e in body` |
| Function | `fun x -> body` |
| Recursive fn | `let f : A -> B = fun x -> ... f(...) ... in` |
| Application | `f(x)` or `f(x, y)` |
| If-then-else | `if c then e1 else e2` |
| Case | `case e \| p1 => e1 \| p2 => e2 end` |
| List | `[1, 2, 3]` or `1::2::[]` |
| Tuple | `(a, b, c)` |
| Type def | `type T = A + B(Int) in` |
| Test | `test expr end` |
| Probe | `^^probe(expr)` |
| Hole | `?` |
| Comment | `# comment` |
