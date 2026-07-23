# One-Step Distribution Demo Plan

## Goal

Add one small, teacher-facing Profile option that controls whether repeated
distribution may be accepted as a single written step.

With the option disabled:

```text
(x - 2) * (x + 5)
  -> (x - 2) * x + (x - 2) * 5
```

and

```text
(x - 2) * (x + 5)
  -> x * (x + 5) - 2 * (x + 5)
```

remain valid One Step results, but this does not:

```text
(x - 2) * (x + 5)
  -> x**2 - 2*x + 5*x - 10
```

Check Result remains a multi-step mode and may compose several enabled
distribution steps. The existing **Allowed multi-step methods** controls remain
Check Result controls only.

## Profile control

Add a small **One Step behavior** section between **Allowed step operations**
and **Automatic simplification**.

The Algebra option is:

> **Allow repeated distribution in one step**  
> Full polynomial expansion may count as one written step.

It defaults to enabled for Algebra and profiles that inherit Algebra, preserving
current behavior. It has no effect when **Distribute multiplication over
addition** is disabled.

This option must not reuse or modify the
`alg.expand_polynomial` checkbox under **Allowed multi-step methods**. The two
settings have different scopes:

- the new option controls One Step granularity;
- `alg.expand_polynomial` controls Check Result's composite expansion method.

## Minimal implementation

1. Add a persisted One Step option override to `ProfileBoard.Model`.
   Keep it separate from mathematical rule overrides so the UI does not pretend
   that step granularity is itself a rewrite rule.
2. In `ProfileBoard.apply_model_to_profile`, use the override to set
   `profile.one_step_policy.allow_polynomial_expansion`.
3. Render the checkbox only for profiles that inherit Algebra.
4. Leave the primitive distribution branches in
   `RewriteChecker.check_single_distribution_or_expansion` unchanged.
5. Leave the existing full-expansion branches guarded by
   `profile.one_step_policy.allow_polynomial_expansion`.
6. Decouple Check Result authorization from this One Step setting:
   `trace_rule_allowed_by_profile` should authorize `alg.expand_polynomial`
   using its Check Result selection and the enabled distribution rule, without
   consulting `one_step_policy.allow_polynomial_expansion`.

No new rewrite search, FOIL recognizer, rewrite-count type, or Rocq tactic is
needed.

## Required tests

### Profile plumbing

- Algebra defaults to repeated distribution enabled.
- Turning the new option off changes only
  `one_step_policy.allow_polynomial_expansion`.
- The Check Result `alg.expand_polynomial` selection is unchanged.
- Turning the option back on restores the current behavior.

### One Step positive cases with repeated distribution disabled

- `(x-2)*(x+5) -> (x-2)*x + (x-2)*5`
- `(x-2)*(x+5) -> x*(x+5) - 2*(x+5)`
- `(a+b)*(c+d) -> (a+b)*c + (a+b)*d`
- `(a+b)*(c+d) -> a*(c+d) + b*(c+d)`

Each trace should contain `alg.distribute_mul_add` and should not contain
`alg.expand_polynomial`.

### One Step negative cases with repeated distribution disabled

- `(x-2)*(x+5) -> x**2 - 2*x + 5*x - 10`
- `(a+b)*(c+d) -> a*c + a*d + b*c + b*d`
- an inequivalent target such as
  `(x-2)*(x+5) -> x**2 + 4*x - 10`

### Enabled behavior

With the option enabled, the two correct fully expanded examples above should
remain valid One Step results and record `alg.expand_polynomial`.

### Mode separation

With repeated One Step distribution disabled and the Check Result expansion
method enabled:

- One Step rejects a complete FOIL expansion;
- Check Result accepts the same correct expansion;
- Check Result still rejects an incorrect expansion.

With the Check Result expansion method disabled:

- the new One Step option remains independently configurable;
- Check Result cannot use the disabled composite method;
- One Step may still accept one primitive distribution.

## Browser demo checklist

1. Select Algebra and One Step.
2. Leave distribution enabled and disable repeated distribution.
3. Confirm both single-distribution orientations are Valid and replaceable.
4. Confirm complete FOIL is Invalid as One Step.
5. Switch to Check Result without changing the expression.
6. Confirm the correct complete expansion is still valid when its multi-step
   method is enabled.
7. Re-enable repeated distribution and confirm complete FOIL becomes Valid in
   One Step again.

## Deferred architecture

The later profile-proof-search refactor should replace this boolean with the
general rule-usage policy already under consideration: disabled, once, bounded,
or repeated. This demo option is intentionally a narrow adapter over the
existing `allow_polynomial_expansion` field.
