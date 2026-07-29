# What Is in a Hazel Math Profile?

The Profile currently has three conceptually different kinds of controls.

## Current contents

| Profile section | What it controls | Example |
| --- | --- | --- |
| Allowed step operations | Conceptual rules a student may apply | Distribute, factor, product rule, sine-sum identity |
| Automatic simplification | Small automatic simplifications around an allowed operation | `x + 0 → x`, `x**1 → x`, collect like terms |
| Allowed multi-step methods | Larger normalization methods allowed when validating a proposed result | Expand a polynomial, verify a factorization, normalize affine arithmetic |

The Profile is therefore not merely a list of available buttons. It is the
policy defining which kinds of mathematical reasoning Hazel may accept.

## 1. Allowed step operations

These are the mathematical ideas students are expected to see and apply.

Examples include:

- Evaluate constants.
- Distribute multiplication.
- Factor a common term.
- Use the difference-of-squares identity.
- Apply trigonometric identities.
- Apply linearity of differentiation.
- Apply product, quotient, power, and chain rules.

These operations can appear in One Step suggestions and proof explanations.

A visible operation can permit particular cleanup around it. Polynomial
expansion, for example, may permit reassociation and constant folding as part
of the conceptual step.

Suggested badge:

```text
ONE STEP
```

## 2. Automatic simplification

These are small quality-of-life transformations that Hazel may apply around a
larger mathematical operation:

- Reassociate addition.
- Commute addition.
- Reassociate multiplication.
- Commute multiplication.
- Remove additive identity.
- Remove multiplicative identity.
- Fold constants.
- Collect like terms.
- Simplify identity powers.
- Use power notation.
- Simplify basic derivatives.

These are not necessarily the main mathematical idea of a step. For example:

```text
diff(x**2, x)
→ 2 * x**1 * diff(x,x)
→ 2 * x
```

The power rule is the visible operation. Removing `x**1`, simplifying
`diff(x,x)`, and removing multiplication by `1` are automatic
simplifications.

Cleanup toggles affect both:

- What automatic result Hazel produces.
- What proposed results Hazel is permitted to accept.

Suggested badge:

```text
AUTOMATIC
```

## 3. Allowed multi-step methods

The current name makes these entries sound like additional rewrite buttons.
They are actually permissions to recognize larger, multi-step
transformations.

Current examples include:

- Normalize affine arithmetic.
- Expand a polynomial product.
- Verify a factored polynomial.

For example:

```text
(2*x + 3)*(x + 4)
→ 2*x**2 + 11*x + 12
```

This contains several primitive transformations. Check Result can accept the
complete jump if polynomial expansion is enabled and all its prerequisites,
such as distribution and collecting like terms, are enabled.

Suggested explanatory text:

> Choose which larger transformations Check Result may accept. These methods
> do not appear as individual One Step operations and require their listed
> rules and simplifications.

Suggested badge:

```text
CHECK RESULT
```

Other possible section names include:

- Check Result shortcuts.
- Whole-result validation.
- Accepted multi-step transformations.
- Advanced result checking.

**Allowed multi-step methods** communicates both the scope and the policy role
without exposing implementation terminology such as "normalizer." Whether
this derived section should remain teacher-facing is a separate architecture
question; the terminology change does not alter its behavior.

## Recommended Profile organization

### Allowed step operations

> Mathematical rules that may be used as a visible student step.

Examples:

- Distribute multiplication.
- Factor a common term.
- Difference of squares.
- Product rule.
- Chain rule.

### Automatic simplification

> Small transformations Hazel may apply around an allowed step.

Examples:

- Fold constants.
- Remove identities.
- Collect like terms.
- Simplify identity powers.
- Simplify basic derivatives.

### Allowed multi-step methods

> Larger transformations that Check Result may validate when all required
> capabilities are enabled.

Examples:

- Normalize affine arithmetic.
- Expand polynomial products.
- Verify polynomial factorization.

This final section could live inside a collapsed **Advanced** section because
most teachers will probably start by configuring allowed steps and automatic
simplification.

## How the categories interact

Consider:

```text
(2*x + 3)*(x + 4)
→ 2*x**2 + 11*x + 12
```

The complete reasoning may require:

```text
Allowed step operation:
  Distribute multiplication

Automatic simplification:
  Reassociate addition
  Reassociate multiplication
  Fold constants
  Collect like terms
  Use power notation

Allowed multi-step method:
  Expand polynomial products
```

The multi-step method is available only if its prerequisites are enabled.

Turning off **Collect like terms** should therefore prevent Check Result from
marking the collected answer **Valid**, even if Rocq can prove that the
expressions are mathematically equal.

## What should not appear in the Profile

Raw Rocq tactics should not become teacher-facing Profile options:

```text
ring
nra
lra
rewrite
reflexivity
```

Those are implementation details used to certify a Profile-authorized
transformation.

A teacher should choose capabilities such as:

```text
Allow polynomial expansion
Allow collecting like terms
Allow commutativity
```

They should not need to choose:

```text
Allow nra
```

The rule catalog should map the teacher-facing capability to the appropriate
Hazel behavior and Rocq certificate.

## Meeting-ready explanation

> A Hazel Profile has three layers. First are allowed step operations—the
> mathematical ideas students can explicitly apply, such as distribution or
> the product rule. Second are automatic simplifications, such as removing
> zero, folding constants, or collecting like terms. Third are allowed
> multi-step methods, which let Check Result recognize larger transformations,
> such as expanding or factoring a polynomial, but only when all prerequisite
> operations and simplifications are enabled. Rocq tactics are not Profile
> capabilities; they only certify transformations that Hazel has already
> authorized.

The Profile terminology is:

```text
Allowed step operations
Automatic simplification
Allowed multi-step methods
```

This replaces the older terminology:

```text
Visible operations
Cleanup policies
Check Result operations
```
