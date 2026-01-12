# Hazel CLI API for AI Agents

This document describes the Hazel CLI commands available for AI-assisted development.

## Prerequisites

The Hazel CLI is invoked via the `./hazel` script in the repository root. It requires:
- Node.js
- The project to be built (automatically triggered on first run)

## Commands

### `run` - Execute a Hazel Program

Evaluates a Hazel program and prints the resulting value.

```bash
./hazel run <file.hz>
./hazel run -              # Read from stdin
```

**Examples:**
```bash
echo 'let x = 5 in x + 3' | ./hazel run -
# Output: 8

echo 'let f = fun x -> x * 2 in f(21)' | ./hazel run -
# Output: 42

echo '(1, 2, 3)' | ./hazel run -
# Output: (1, 2, 3)
```

**Exit codes:**
- 0: Success
- Non-zero: Evaluation failed

---

### `format` - Normalize Hazel Code

Reconstructs code from its AST, making holes explicit and normalizing whitespace.

```bash
./hazel format <file.hz>
./hazel format -           # Read from stdin
```

**Examples:**
```bash
echo 'let  = 5 in  + 3' | ./hazel format -
# Output:
# let ? = 5 in
# ? + 3

echo 'let x=5 in x+3' | ./hazel format -
# Output:
# let x = 5 in x + 3
```

**Notes:**
- Holes become explicit `?` markers
- Whitespace is normalized
- Comments are not preserved

---

### `analyze` - Static Type Checking

Performs static analysis and reports type errors.

```bash
./hazel analyze <file.hz>
./hazel analyze -          # Read from stdin
```

**Examples:**
```bash
echo 'let x = 5 in x + 3' | ./hazel analyze -
# Output: No static errors found.

echo 'let x = true in x + 3' | ./hazel analyze -
# Output:
# Found 1 static error:
#
# error: Expecting type Int but got inconsistent type Bool
#   --> -:1:17
#   |
# 1 | let x = true in x + 3
#   |                 ^
```

**Exit codes:**
- 0: No static errors
- 124: Static errors found

**Error format:** Rust-style with source context, line numbers, and carets pointing to the error location.

---

### `probe` - Run with Inline Value Display

Runs a program and displays probed expression values inline with the code.

```bash
./hazel probe <file.hz>
./hazel probe --many <file.hz>  # Show multiple values per probe
./hazel probe -                 # Read from stdin
```

**Probe syntax:**
```hazel
# Wrap an expression with ^^probe(...) to see its value
let x = 5 in ^^probe(x + 1)
```

**Examples:**
```bash
# Simple probe
echo 'let x = 5 in ^^probe(x + 1)' | ./hazel probe -
# Output: let x = 5 in ^^probe(x + 1)     ≡ 6

# Multiple calls to a probed function (single mode - shows last value)
echo 'let f : Int -> Int = fun x -> ^^probe(x * 2) in f(1); f(2); f(3)' | ./hazel probe -
# Output: let f : Int -> Int = fun x -> ^^probe(x * 2) in f(1); f(2); f(3)     ≡ 6

# Multiple calls with --many flag (shows all values)
echo 'let f : Int -> Int = fun x -> ^^probe(x * 2) in f(1); f(2); f(3)' | ./hazel probe --many -
# Output: let f : Int -> Int = fun x -> ^^probe(x * 2) in f(1); f(2); f(3)     ≡ 2 ⫽ 4 ⫽ 6
```

**Output symbols:**
- `≡` - Value follows
- `∅` - No samples collected (probe not executed)
- `⫽` - Separator between multiple values (with `--many`)

---

### `test` - Run Tests and Report Results

Runs a Hazel program and reports the results of all `test ... end` forms.

```bash
./hazel test <file.hz>
./hazel test --verbose <file.hz>  # Show all tests, not just failures
./hazel test -                    # Read from stdin
```

**Test syntax:**
```hazel
# Basic test - expression must evaluate to a boolean
test 2 + 2 == 4 end

# Named test (hinted) - provides a description for test output
hint "addition works"
test 2 + 2 == 4 end
```

**Examples:**
```bash
# File with passing and failing tests
cat > demo.hz << 'EOF'
let _ = test true end in
let _ = test 1 + 1 == 2 end in
let _ = hint "addition works" test 2 + 2 == 4 end in
let _ = test 2 + 2 == 5 end in
let _ = hint "this will fail" test 10 < 5 end in
()
EOF

./hazel test demo.hz
# Output:
# Test Results: Out of 5 tests, 2 are failing
#
# FAIL [line 4]: test 2 + 2 == 5 end
# FAIL [line 5, "this will fail"]: hint "this will fail" test 10 < 5 end

./hazel test --verbose demo.hz
# Output:
# Test Results: Out of 5 tests, 2 are failing
#
# PASS [line 1]: test true end
# PASS [line 2]: test 1 + 1 == 2 end
# PASS [line 3, "addition works"]: hint "addition works" test 2 + 2 == 4 end
# FAIL [line 4]: test 2 + 2 == 5 end
# FAIL [line 5, "this will fail"]: hint "this will fail" test 10 < 5 end
```

**Exit codes:**
- 0: All tests pass (or no tests)
- 124: One or more tests fail
- Non-zero: Parse or evaluation error

**Output format:**
- `PASS` / `FAIL` / `INDET` - Test status
- `[line N]` - Source line number
- `[line N, "hint"]` - Line number and hint for named tests
- Source text of the test expression

**Notes:**
- Tests must be let-bound to ensure they're evaluated: `let _ = test ... end in ...`
- Use named tests (`hint "name" test ... end`) for better error messages
- When a test fails, use probes to debug the actual values

---

## Hazel Syntax Quick Reference

### Basic Expressions

```hazel
# Literals
42              # Int
3.14            # Float
true, false     # Bool
"hello"         # String
()              # Unit

# Arithmetic
x + y, x - y, x * y, x / y

# Comparison
x == y, x != y, x < y, x > y, x <= y, x >= y

# Boolean
x && y, x || y, !x
```

### Let Bindings

```hazel
let x = 5 in x + 1
let x: Int = 5 in x + 1    # With type annotation
```

### Functions

```hazel
# Anonymous function
fun x -> x + 1

# With type annotations
fun x: Int -> x + 1

# Let-bound function
let f = fun x -> x + 1 in f(5)

# Multiple arguments (curried)
let f = fun x -> fun y -> x + y in f(2)(3)

# Multiple arguments (tuple)
let f : (Int, Int) -> Int = fun (x, y) -> x + y in f(2, 3)

# Application
f(x)
```

### Recursive Functions

**Important**: Hazel does NOT have `let rec`. Recursion works automatically when a function has an arrow type annotation.

```hazel
# Recursive function requires arrow type annotation
let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl => 1 + length(tl)
end
in length([1, 2, 3])

# Mutual recursion uses tuple binding
let (even : Int -> Bool, odd : Int -> Bool) = (
  fun n -> if n == 0 then true else odd(n - 1),
  fun n -> if n == 0 then false else even(n - 1)
)
in even(4)
```

### Tuples and Lists

```hazel
# Tuples
(1, 2, 3)
let (a, b) = (1, 2) in a + b

# Lists
[1, 2, 3]
1::2::[]           # Cons syntax
xs @ ys            # Append
```

### Pattern Matching

```hazel
case x
| 0 => "zero"
| 1 => "one"
| _ => "many"
end

case opt
| None => 0
| Some(x) => x
end
```

### Types

```hazel
Int, Float, Bool, String, ()

# Option type
None: Option(Int)
Some(5): Option(Int)

# Function types
Int -> Int
(Int, String) -> Bool

# List type
[Int]

# Tuple type
(Int, Bool, String)
```

### Holes

Hazel allows incomplete programs with holes:

```hazel
let x = ? in x + 1     # Explicit hole
let = 5 in  + 1        # Implicit holes (shown in format output)
```

### Test Forms

```hazel
# Basic test - records pass/fail during evaluation
test 2 + 2 == 4 end

# Named test with hint - better error messages
hint "addition works"
test 2 + 2 == 4 end

# Tests must be let-bound to run during evaluation
let _ = test 2 + 2 == 4 end in
let _ = hint "subtraction" test 5 - 3 == 2 end in
result
```

### Probes (Debugging)

```hazel
# Wrap expression to see its runtime value
let x = 5 in ^^probe(x + 1)

# Inside functions (see value for each call)
let f = fun x -> ^^probe(x * 2) in
f(1); f(2); f(3)
```

## Workflow for AI Development

1. **Write program** to a `.hz` file
2. **Check for errors**: `./hazel analyze program.hz`
3. **Run program**: `./hazel run program.hz`
4. **Run tests**: `./hazel test program.hz` to check all test assertions
5. **Debug failures**: When a test fails, add `^^probe(...)` wrappers around expressions in or near the failing test, then run `./hazel probe program.hz` to see actual values

**Debugging workflow for failing tests:**
```hazel
# Original failing test
let _ = test my_function(5) == 10 end in ...

# Add probes to see what's happening
let _ = test ^^probe(my_function(5)) == 10 end in ...
```

Then run: `./hazel probe program.hz` to see the actual value of `my_function(5)`.

## Known Limitations

- No JSON output mode for programmatic consumption
- No incremental/watch mode
- No code completion or suggestions
- No auto-probing of failing test expressions (must manually add probes)

See [vision.md](./vision.md) for planned improvements.

## Common Pitfalls for AI Agents

1. **No `let rec` keyword**: Hazel doesn't have `let rec`. For recursion, just use `let` with an arrow type annotation.
   - Wrong: `let rec f = fun x -> f(x-1)`
   - Right: `let f : Int -> Int = fun x -> f(x-1)`

2. **Gradual typing**: Programs with type errors still run. The `analyze` command will report errors, but `run` will execute anyway.

3. **Holes are valid**: Empty expressions become holes. `let x = in x + 1` is valid Hazel (the empty spot becomes a hole).
