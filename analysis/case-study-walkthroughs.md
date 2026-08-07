# Case-study walkthroughs

This document gives a concrete path through each of the eight initial Hazel
case studies. The paths are intentionally aligned with the locked math profile
for each exercise. They are not the only mathematically valid derivations, but
they show the expected amount of work.

## General stepper instructions

1. Select the **entire expression being rewritten**, not just a variable or an
   inner sum. Double-clicking the main operator of an expression is usually the
   easiest way to select that whole expression.
2. Confirm the expression under **From:** before typing a result. If **From:**
   is smaller than the expression described below, close the box and select
   again.
3. In **One Step** exercises, enter exactly one local rewrite. In **Check
   Result** exercises, a larger result may be accepted if the active profile
   can construct and certify a route to it.
4. Use Hazel operators, not textbook shorthand:

   - Integer power: `**`
   - Float power: `**.`
   - Float arithmetic: `+.`, `-.`, `*.`, and `/.`

5. The current red `x not found` decorations are a separate binding issue. They
   are intentionally deferred and are not part of these walkthroughs.
6. Each case study locks its math profile and automation stage across the
   Prelude, Lemmas/Scratch Space, and theorem steppers. The editable math-mode
   controls are therefore hidden; the locked mode listed below is already
   active.

## 1. Order of Operations

**Locked mode:** Arithmetic / One Step

**Goal:**

```text
3 + 4 * 2 ** 2 - 6 / 3 == 17
```

Evaluate one local arithmetic operation at a time:

1. Select `2 ** 2` and enter `4`.
2. Select `4 * 4` and enter `16`.
3. Select `6 / 3` and enter `2`.
4. Select `3 + 16` and enter `19`.
5. Select `19 - 2` and enter `17`.

The completed left side should now be exactly `17`. Do not select the complete
initial expression and jump directly to `17`; that skips the teaching goal of
the exercise.

## 2. FOIL, Written Out

**Locked mode:** Algebra / One Step

**Goal:**

```text
(x + 1) * (x + 1) == x * x + x * 1 + 1 * x + 1 * 1
```

Use the following path because it produces the four products in the same order
as the right side:

1. Select the entire `(x + 1) * (x + 1)` by selecting its central `*`.
   Confirm that **From:** contains the whole product, then enter:

   ```text
   x * (x + 1) + 1 * (x + 1)
   ```

2. Select the entire `x * (x + 1)` and enter:

   ```text
   x * x + x * 1
   ```

3. Select the entire `1 * (x + 1)` and enter:

   ```text
   1 * x + 1 * 1
   ```

If distribution leaves the four products nested differently, select the whole
sum and enter the same four products without the extra grouping. Hazel treats
all addition or multiplication reassociation as one cleanup step in this
profile; you do not need to remove one pair of parentheses at a time.

The left side is now the required four-product FOIL result. Do not collect the
middle products. This profile allows multiplication commutativity, so `1 * x`
and `x * 1` may be reconciled without combining the two middle products into
`2 * x`.

### If the other distribution direction was used

The also-valid first step

```text
(x + 1) * x + (x + 1) * 1
```

produces the middle terms in the opposite order. Finish both products, then
swap the adjacent middle terms if necessary. In particular:

- Select `(x + 1) * x`, not only `x + 1`, and enter
  `x * x + 1 * x`.
- Select `(x + 1) * 1`, not only `x + 1`, and enter
  `x * 1 + 1 * 1`.

If **From:** says only `x + 1`, the selection is too narrow. Rewriting that sum
as `1 * x + 1 * 1` would introduce multiplication identities and distribution
at once, so One Step correctly rejects it.

Check the order inside the second distributed product carefully:

```text
(x + 1) * 1 = x * 1 + 1 * 1
```

It is not `1 * x + 1 * 1`. That mistake duplicates `1 * x` and omits the
required `x * 1`, so the two sides are not equal even though they look very
similar.

## 3. FOIL with Algebraic Cleanup

**Locked mode:** Algebra / One Step

**Goal:**

```text
(2 * x - 3) * (x + 4) == 2 * x ** 2 + 5 * x - 12
```

This exercise allows more cleanup than verbose FOIL, while keeping each
distribution visible:

1. Select the entire product `(2 * x - 3) * (x + 4)`.
2. Open **One Step** and distribute one side of the product.
3. Continue distributing until all four products are visible.
4. Use the enabled algebra cleanup to combine the two linear terms and simplify
   the constants, reaching:

   ```text
   2 * x ** 2 + 5 * x - 12
   ```

Whole-product polynomial expansion is disabled here. The distinction from the
first FOIL exercise is that multiplication, reordering, and collection may be
absorbed as cleanup around each visible distribution step.

## 4. Completing the Square

**Locked mode:** Algebra / Check Result

**Goal:**

```text
x ** 2 + 6 * x + 5 == (x + 3) ** 2 - 4
```

The conceptual calculation is:

```text
x ** 2 + 6 * x + 5
= x ** 2 + 6 * x + 9 - 4
= (x + 3) ** 2 - 4
```

In the stepper:

1. Select `6 * x + 5` and enter:

   ```text
   6 * x + 9 - 4
   ```

   This explicitly adds and subtracts `3 ** 2`, where `3` is half of the
   linear coefficient `6`.

2. In the resulting expression, select the full trinomial
   `x ** 2 + 6 * x + 9` and enter:

   ```text
   (x + 3) ** 2
   ```

3. Leave the trailing `- 4` in place. The complete left side should now be
   `(x + 3) ** 2 - 4`.

The profile deliberately disables arbitrary polynomial factorization. For
example, `(x + 1) * (x + 5)` is mathematically equal to the original
quadratic, but it does not demonstrate completing the square and is not the
intended route.

## 5. Trigonometric Power Reduction

**Locked mode:** Trigonometry / Check Result

**Goal:**

```text
1. +. 2. *. sin(x) **. 4.
== 7. /. 4. -. cos(2. *. x) +. (1. /. 4.) *. cos(4. *. x)
```

The useful mathematical route is:

```text
sin(x)^4
= (sin(x)^2)^2
= ((1 - cos(2x)) / 2)^2
```

then expand the square and use

```text
cos(2x)^2 = (1 + cos(4x)) / 2.
```

Concrete Hazel steps:

1. Select `sin(x) **. 4.` and enter:

   ```text
   (sin(x) **. 2.) **. 2.
   ```

2. Select the inner `sin(x) **. 2.` and enter:

   ```text
   (1. -. cos(2. *. x)) /. 2.
   ```

3. Expand the resulting square, retaining `cos(2. *. x) **. 2.` as a visible
   term. The Check Result profile may perform the routine scalar cleanup.
4. Select `cos(2. *. x) **. 2.` and enter:

   ```text
   (1. +. cos(4. *. x)) /. 2.
   ```

5. Select the complete left side and enter the target:

   ```text
   7. /. 4. -. cos(2. *. x) +. (1. /. 4.) *. cos(4. *. x)
   ```

6. Run the profile/Rocq check and replace the expression after it reports
   **Valid**.

The Float dots are required. Writing `^`, `+`, `*`, or `/` here changes the
numeric family and can produce cascading static errors.

## 6. Polynomial Derivative, Check Each Result

**Locked mode:** Calculus / Check Result

**Goal:**

```text
deriv (x ** 3 + 2 * x) by x == 3 * x ** 2 + 2
```

Use **Search** on the selected derivative expression. Check Result may compose
the visible rules enabled by the profile even though automatic
`derivative.basics` cleanup is disabled.

The shortest intended path is:

1. Select the entire derivative and open **Search**. Under
   **Differentiation**, choose **linearity (sum rule)**:

   ```text
   (deriv (x ** 3) by x) + (deriv (2 * x) by x)
   ```

2. Select exactly `deriv (x ** 3) by x` and open **Search**. Under
   **Differentiation**, choose **power rule**. Hazel should produce:

   ```text
   3 * x ** 2 * (deriv x by x)
   ```

3. Select `deriv x by x` and choose **derivative of a variable**, producing
   `1`. Then apply **Remove multiplicative identity** to obtain:

   ```text
   3 * x ** 2
   ```

4. Select `deriv (2 * x) by x` and choose **product rule**. This keeps both
   elementary derivatives visible:

   ```text
   (deriv 2 by x) * x + 2 * (deriv x by x)
   ```

5. Apply the constant and variable derivative rules, then the visible zero and
   identity cleanup steps, to obtain `2`.

As a Check Result shortcut, selecting either complete derivative and choosing
**Simplify** may suggest its fully checked result (`3 * x ** 2` or `2`). That
shortcut is useful for verification, but the named-rule path above best matches
the exercise's teaching goal.

The completed expression is:

```text
3 * x ** 2 + 2
```

This exercise intentionally disables automatic basic-derivative cleanup so
the variable, constant, and identity rules remain visible in **One Step**
mode. That setting does not prohibit Check Result from certifying a sequence of
those same enabled rules. For example, changing `x ** 3` to `x ** 2` while it
is still underneath `deriv` is not a derivative rule and must remain invalid.

## 7. Taylor Approximation, First Steps

**Locked mode:** Calculus / Check Result

The read-only prelude defines the deliberately simple function:

```text
f(t : Int) = t ** 2 + 3 * t + 2
```

The theorem asks for its second-order Taylor polynomial about `0`:

```text
let f1 = D f in
let f2 = D f1 in
f(0) + f1(0) * x + (f2(0) / 2) * x ** 2 == x ** 2 + 3 * x + 2
```

Work through the two derivative checkpoints before simplifying the assembled
polynomial:

1. Select the complete `D f` expression by selecting its `D`, open
   **Search**, and enter:

   ```text
   fun t -> 2 * t + 3
   ```

   Choose **Run Rocq Search**, then **Replace** after Hazel reports **Valid**.

2. Select `D f1`, open **Search**, and enter:

   ```text
   fun t -> 2
   ```

   Again run the Rocq check and replace only after it reports **Valid**.

3. Select the assembled left side of the equality and use **Simplify** to
   check:

   ```text
   x ** 2 + 3 * x + 2
   ```

This introductory version exposes the Taylor pattern—evaluate `f`, `f1`, and
`f2` at the center and divide the quadratic coefficient by `2`—using only
integer polynomial syntax. It avoids Float operators, trigonometric chain
rules, mixed coefficients, and a third derivative.

## 8. Taylor Approximation from a Derivative Chain

**Locked mode:** Calculus / Check Result

The read-only prelude defines:

```text
a = 3. /. 10.
f = fun t -> 7. /. 4. -. cos(2. *. t) +. (1. /. 4.) *. cos(4. *. t)
```

The theorem then binds `f1 = D f`, `f2 = D f1`, and `f3 = D f2`. This keeps
the long trigonometric definition out of every derivative checkpoint.

For each checkpoint, select the complete derivative application (`D f`, then
`D f1`, then `D f2`) by selecting its `D`. Open **Search**, enter the cleaned
function below, choose **Run Rocq Search**, and choose **Replace** only after
Hazel reports **Valid**.

### First derivative (`f1`)

Replace the first derivative with:

```text
fun t -> 2. *. sin(2. *. t) -. sin(4. *. t)
```

### Second derivative (`f2`)

Replace the second derivative with:

```text
fun t -> 4. *. cos(2. *. t) -. 4. *. cos(4. *. t)
```

### Third derivative (`f3`)

Replace the third derivative with the cleaned coefficient form:

```text
fun t -> (0. -. 8.) *. sin(2. *. t) +. 16. *. sin(4. *. t)
```

Use `0. -. 8.` rather than an integer-style unary negative so the expression
stays in Hazel's Float arithmetic family.

After all three replacements, the named functions supply the coefficients in

```text
f(a)
+. f1(a) *. (x -. a)
+. (f2(a) /. 2.) *. (x -. a) **. 2.
+. (f3(a) /. 6.) *. (x -. a) **. 3.
```

The prelude supplies both `a` and `f`. The theorem already contains the Taylor
assembly and its expanded target, so replacing the three derivative
checkpoints makes the sides agree. Routine chain-rule, scalar, and affine
cleanup belongs to the locked Calculus Check Result profile; the learner is
responsible for identifying the three cleaned derivative functions.

## Quick mode summary

| Case study | Math level | Validation mode | Main teaching goal |
| --- | --- | --- | --- |
| Order of Operations | Arithmetic | One Step | Local evaluation order |
| FOIL, Written Out | Algebra | One Step | Every distribution is visible |
| FOIL with Cleanup | Algebra | One Step | Visible distribution plus collection cleanup |
| Completing the Square | Algebra | Check Result | Add/subtract a square and factor it |
| Trig Power Reduction | Trigonometry | Check Result | Choose the identities; automate cleanup |
| Polynomial Derivative | Calculus | Check Result | Certify results through visible derivative rules |
| Taylor Approximation, First Steps | Calculus | Check Result | Two simple derivatives and a second-order polynomial |
| Trig Taylor Approximation | Calculus | Check Result | Build and certify the longer derivative chain |
