# Arcane Calculator

Expression parser and evaluator with operator precedence and associativity.

## Program Overview

**Lines:** ~270
**Tests:** 28
**Concepts:** Tokenization, recursive descent parsing, AST construction, operator precedence, left associativity, mutual recursion

This is the most complex of the study programs, implementing a mini-language interpreter.

### Pipeline

```
"2 + 3 * 4" → tokenize → [TNum(2), TPlus, TNum(3), TStar, TNum(4), TEnd]
                              ↓
                           parse
                              ↓
                    Add(Num(2), Mul(Num(3), Num(4)))
                              ↓
                          evaluate
                              ↓
                             14
```

### Data Model

```
Token = TNum(Int) | TPlus | TMinus | TStar | TSlash | TLParen | TRParen | TEnd

Expr = Num(Int)
     | Add(Expr, Expr)
     | Sub(Expr, Expr)
     | Mul(Expr, Expr)
     | Div(Expr, Expr)
     | Neg(Expr)
```

### Grammar (Precedence Encoded)

```
expr   = term (('+' | '-') term)*      # Lowest precedence
term   = factor (('*' | '/') factor)*  # Higher precedence
factor = '-' factor | atom             # Unary minus
atom   = number | '(' expr ')'         # Highest precedence
```

### Key Functions

- `tokenize` - Converts string to token list
- `parseExpr/parseTerm/parseFactor/parseAtom` - Mutually recursive parser
- `evaluate` - Recursively evaluates AST

### Operator Properties

| Operator | Precedence | Associativity |
|----------|------------|---------------|
| +, -     | Low        | Left          |
| *, /     | High       | Left          |
| - (unary)| Highest    | Right         |

---

## Bug Variants

### calculator-bug-precedence.hz

**Bug:** All operators handled at the same precedence level

**Difficulty:** Medium

**What's wrong:**
```hazel
# Buggy - parseExpr handles ALL operators:
fun tokens ->
  let (left, rest) = parseFactor(tokens) in
  let loop = fun (acc, toks) ->
    case toks
    | TPlus :: tail => ... loop(Add(acc, right), rest2)
    | TMinus :: tail => ... loop(Sub(acc, right), rest2)
    | TStar :: tail => ... loop(Mul(acc, right), rest2)  # Wrong!
    | TSlash :: tail => ... loop(Div(acc, right), rest2) # Wrong!
    | _ => (acc, toks)
    end
  in loop(left, rest)

# Correct - parseExpr only handles +/-, parseTerm handles */:
# parseExpr calls parseTerm, parseTerm calls parseFactor
```

**Failing test:** "multiplication before addition" - `2 + 3 * 4` returns 20 instead of 14

**What happens:**
- Buggy: `2 + 3 * 4` → `(2 + 3) * 4` → `20`
- Correct: `2 + 3 * 4` → `2 + (3 * 4)` → `14`

**Probe strategy:**
- Probe `parse("2 + 3 * 4")` to see the AST structure
- Buggy shows `Mul(Add(Num(2), Num(3)), Num(4))`
- Correct shows `Add(Num(2), Mul(Num(3), Num(4)))`

**Why this bug is realistic:**
- When first implementing a parser, it's tempting to handle all binary operators in one place
- Understanding why precedence requires separate grammar levels is non-obvious

---

### calculator-bug-associativity.hz

**Bug:** Right-associative instead of left-associative

**Difficulty:** Medium

**What's wrong:**
```hazel
# Buggy - recursive call to parseExpr (right-associative):
fun tokens ->
  let (left, rest) = parseTerm(tokens) in
  case rest
  | TPlus :: tail =>
      let (right, rest2) = parseExpr(tail) in  # parseExpr, not parseTerm!
      (Add(left, right), rest2)
  ...

# Correct - iterative loop (left-associative):
fun tokens ->
  let (left, rest) = parseTerm(tokens) in
  let loop = fun (acc, toks) ->
    case toks
    | TPlus :: tail =>
        let (right, rest2) = parseTerm(tail) in  # parseTerm
        loop(Add(acc, right), rest2)             # Then loop
    ...
  in loop(left, rest)
```

**Failing test:** "subtraction is left associative" - `10 - 5 - 2` returns 7 instead of 3

**What happens:**
- Buggy (right): `10 - 5 - 2` → `10 - (5 - 2)` → `10 - 3` → `7`
- Correct (left): `10 - 5 - 2` → `(10 - 5) - 2` → `5 - 2` → `3`

**Probe strategy:**
- Probe `parse("10 - 5 - 2")` to see the AST structure
- Buggy shows `Sub(Num(10), Sub(Num(5), Num(2)))`
- Correct shows `Sub(Sub(Num(10), Num(5)), Num(2))`

**Why this bug is realistic:**
- The recursive descent pattern naturally leads to right-associativity
- Getting left-associativity requires the iterative loop pattern
- Many tutorials don't explain this distinction clearly

---

## Development Notes

### Major Issues Encountered

**1. Comment syntax:** Used `# comment` instead of `# comment #`, causing parse failures

**2. Reserved name `eval`:** Named my evaluator function `eval` which silently conflicts with a builtin. Symptoms:
- Program returns `?`
- Error message: "Expected a constructor" (misleading!)
- Fix: Rename to `evaluate`

**3. "No tests available":** Parse errors from bad comments prevented test discovery. The error message didn't indicate parse failures.

### Debugging Approach

Heavy use of echo-pipe for incremental testing:
```bash
# Test tokenizer alone
echo '...[tokenizer]... tokenize("5+3")' | ./hazel run -

# Test tokenizer + parser
echo '...[both]... parse("5+3")' | ./hazel run -

# Isolate the problem
echo '...[minimal]... eval(Add(Num(5), Num(3)))' | ./hazel run -
# Returns ? - found the culprit!
```

### Test Coverage

The working version tests:
- Numbers (single digit, multi-digit, with spaces, zero)
- Basic operators (+, -, *, /)
- Precedence (* and / before + and -)
- Associativity (left-to-right for all binary operators)
- Parentheses (override precedence, nested, complex)
- Unary minus (standalone, in expression, with parens, double negative)
- Edge cases (division by zero returns 0)

### Mutual Recursion Pattern

The parser uses Hazel's tuple binding for mutual recursion:
```hazel
let (parseExpr : [Token] -> ParseResult,
     parseTerm : [Token] -> ParseResult,
     parseFactor : [Token] -> ParseResult,
     parseAtom : [Token] -> ParseResult) = (
  fun tokens -> ...,  # parseExpr body
  fun tokens -> ...,  # parseTerm body
  fun tokens -> ...,  # parseFactor body
  fun tokens -> ...   # parseAtom body
)
in
```

This allows all four functions to call each other.

### Complexity Assessment

This program is significantly more complex than tic-tac-toe or game of life:
- More abstract concepts (parsing, ASTs, precedence)
- Mutual recursion
- String manipulation
- Multiple interacting phases (tokenize → parse → evaluate)

The bugs require understanding the parser architecture, not just spotting a typo.
