# Professor Demo Examples

These examples are meant to show that Hazel is checking configurable math
profiles, not just running a broad simplifier. The strongest demos show one
visible move, background cleanup, and rejected over-simplification.

## 1. Distribution Without Evaluation

Mode: Arithmetic / One Step

Valid:

```text
2 * (1 + 2)
-> 2*1 + 2*2
```

Invalid:

```text
2 * (1 + 2)
-> 6
```

Point: distribution can count as one visible step without allowing the checker
to also fold the arithmetic.

## 2. Same Step, Different Cleanup Profile

Mode: Algebra / One Step

Valid by default:

```text
x * (1 + 2 + x)
-> 1*x + 2*x + x**2
```

Then open Profile and turn off `Comm *`.

Invalid with `Comm *` off:

```text
x * (1 + 2 + x)
-> 1*x + 2*x + x**2
```

Still valid with `Comm *` off:

```text
x * (1 + 2 + x)
-> x*1 + x*2 + x**2
```

Point: the visible move is still distribution, but multiplication reordering is
a separate cleanup capability.

## 3. Power Notation As Cleanup

Mode: Algebra / One Step

Valid when power notation cleanup is on:

```text
x * (1 + x)
-> x*1 + x**2
```

Then turn off `Power notation`.

Invalid with `Power notation` off:

```text
x * (1 + x)
-> x*1 + x**2
```

Still valid with `Power notation` off:

```text
x * (1 + x)
-> x*1 + x*x
```

Point: `x*x = x**2` is separate from distribution.

## 4. Too Much Algebra Rejection

Mode: Algebra / One Step

Invalid:

```text
x * (1 + x)
-> x + x**2
```

Point: this combines distribution, `x*1 -> x`, and power notation. Hazel should
reject it as too much for one visible step.

## 5. Reverse Direction: Factoring

Mode: Algebra / One Step

Valid:

```text
x*a + x*b
-> x*(a + b)
```

Then turn off `Factor a common term`.

Invalid with factoring off:

```text
x*a + x*b
-> x*(a + b)
```

Point: factoring is a separate visible operation, not just distribution run
backwards in an always-on simplifier.

## 6. Cancellation As Its Own Operation

Mode: Algebra / One Step

Valid:

```text
x + y - y
-> x
```

Then turn off `Cancel a common additive term`.

Invalid with cancellation off:

```text
x + y - y
-> x
```

Point: cancellation can be enabled or disabled independently from distribution
and factoring.

## 7. Trig Identity Without Whole-Expression Collapse

Mode: Trigonometry / One Step

Valid:

```text
sin(x)**2 + cos(x)**2
-> 1
```

Try a mixed expression:

```text
sin(x)**2 + cos(x)**2 + 3 + 4
```

Expected behavior:

```text
3 + 4
-> 7
```

should be valid when selecting just the arithmetic subterm.

But the whole-expression jump:

```text
sin(x)**2 + cos(x)**2 + 3 + 4
-> 8
```

should be rejected as a one-step move if it requires both a trig identity and
arithmetic folding.

Point: the checker can validate local subterm rewrites without allowing a broad
whole-expression simplification.

## 8. Course Policy Story

Mode: Algebra / One Step

Use the same source expression:

```text
x * (1 + 2 + x)
```

Strict policy:

```text
x * (1 + 2 + x)
-> x*1 + x*2 + x*x
```

Cleanup-friendly policy:

```text
x * (1 + 2 + x)
-> 1*x + 2*x + x**2
```

Too much for one step:

```text
x * (1 + 2 + x)
-> x*3 + x**2
```

Point: profiles should let an instructor decide whether association,
commutation, and notation cleanup are tolerated around the visible move, while
still rejecting additional simplification such as constant folding.
