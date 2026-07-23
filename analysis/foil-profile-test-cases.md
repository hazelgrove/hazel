# FOIL and Polynomial Expansion Profile Tests

Run each applicable case in both **One Step** and **Check Result**. After a
result is reported as **Valid**, click **Replace** and confirm that the selected
expression is actually replaced.

## Standard expansions

Keep the default Algebra profile enabled for these cases.

```text
(x+1)*(x+2)
-> x*x + 3*x + 2

(x+2)*(x+3)
-> x**2 + 5*x + 6

(2*x+3)*(x+4)
-> 2*x**2 + 11*x + 12

(x-2)*(x+5)
-> x**2 + 3*x - 10

(2*x-3)*(3*x+4)
-> 6*x**2 - x - 12

(a+b)*(c+d)
-> a*c + a*d + b*c + b*d
```

The power-form targets also exercise **Use power notation**. The symbolic
four-term target does not require **Collect like terms** because it does not
combine any like terms.

## Named identity

```text
(x+y)*(x-y)
-> x**2 - y**2
```

This can be authorized directly by **Use difference of squares identity**. It
is therefore not a clean negative test for disabling distribution. To require
this case to fail, disable both **Distribute multiplication over addition** and
**Use difference of squares identity**.

## Larger products

```text
(x+1)*(x**2+x+1)
-> x**3 + 2*x**2 + 2*x + 1

(x+1)*(x+2)*(x+3)
-> x**3 + 6*x**2 + 11*x + 6

3*(x+2)*(x-1)
-> 3*x**2 + 3*x - 6
```

These are useful checks that polynomial expansion is structural rather than a
special case for two binomials.

## Teaching-oriented distribution steps

Test the convenient complete expansion:

```text
(x+1)*(x+2)
-> x*x + x*2 + 1*x + 1*2
```

Also test the same work as individual visible distributions:

```text
(x+1)*(x+2)
-> x*(x+2) + 1*(x+2)

x*(x+2) + 1*(x+2)
-> x*x + x*2 + 1*(x+2)

x*x + x*2 + 1*(x+2)
-> x*x + x*2 + 1*x + 1*2
```

Subsequent cleanup of `1*x`, `1*2`, and collected coefficients should depend
on the corresponding active Profile capabilities.

## Profile boundaries

### Distribution disabled

Turn off **Distribute multiplication over addition**. Ordinary FOIL and
distribution results should not be **Valid**. Do not use the difference of
squares example for this check unless its named identity is also disabled.

### Collection disabled

Turn off **Collect like terms**.

- A collected target such as `2*x**2 + 11*x + 12` should not be **Valid**.
- The uncollected target `a*c + a*d + b*c + b*d` should remain **Valid** when
  distribution is enabled.
- In **Check Result**, a mathematically equivalent but profile-disallowed
  target may say **Equivalent, outside profile**, but it must not offer
  **Replace**.

### Power notation disabled

Turn off **Use power notation**. Targets that introduce `x**2` or `x**3` from
repeated multiplication should not silently bypass that Profile boundary.

## Mathematically invalid targets

These must remain **Invalid**, rather than merely outside the Profile:

```text
(x+1)*(x+2)
-> x**2 + 4*x + 2

(x-2)*(x+5)
-> x**2 + 4*x - 10
```

