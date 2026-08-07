# Dissertation case-study exercise plan

## Scope decisions

- Turn each in-scope dissertation case study into a short sequence of
  exercises, rather than treating a whole case study as one large prompt.
- Exclude the inequality-bounding case study.
- Do not add a Taylor-series math mode or restore the deleted Taylor macro.
  Taylor approximation is a calculus-level case study that composes existing
  trigonometry, differentiation, arithmetic, `let`, and proof-export support.
- Give each exercise a fixed math policy: a base level, a profile snapshot,
  an automation stage, and controls that the learner may or may not change.
- Use one common exercise mechanism for all case studies. Do not add a new
  language-level `rewrite_level` constructor for each lesson.

## Canonical initial case studies

The first deliverable is eight exercises. The longer sequences below describe
possible internal milestones or later variants; they do not expand the initial
scope beyond these eight.

### 1. Order of operations

**Level:** Elementary arithmetic

**Example:** `3 + 4 * 2^2 - 6 / 3`

The learner evaluates powers, multiplication/division, and addition/subtraction
in order. The profile permits local constant evaluation but does not collapse
the whole expression automatically. A useful required path is:

```text
3 + 4 * 2^2 - 6 / 3
= 3 + 4 * 4 - 6 / 3
= 3 + 16 - 6 / 3
= 3 + 16 - 2
= 19 - 2
= 17
```

This establishes selection, one-step arithmetic, and the idea that the active
mode controls how much work Hazel performs.

### 2. Verbose FOIL

**Level:** Introductory algebra

**Example:** `(x + 1)^2`

First rewrite the square as `(x + 1)(x + 1)`. Then require one distribution at
a time and leave the two middle `x` terms separate until the distribution is
complete. Disable polynomial expansion and automatic collection. The important
activity is constructing all four products, not merely reaching the answer.

### 3. FOIL with automation

**Level:** Algebra

**Example:** `(2*x - 3)(x + 4)`

The learner chooses to expand the product, while the profile may perform larger
distribution steps, reorder terms, evaluate scalar products, and combine like
terms. The target is `2*x^2 + 5*x - 12`. This demonstrates that the same basic
mathematics can be supported with less mechanical work at a higher automation
setting.

### 4. Completing the square

**Level:** Algebra

**Example:** `x^2 + 6*x + 5 = (x + 3)^2 - 4`

The learner identifies half of the linear coefficient, adds and subtracts its
square, and factors the perfect-square trinomial. The exercise should expose
those conceptual steps while allowing routine arithmetic cleanup. This needs a
general completing-the-square operation or milestone design; it must not be an
example-specific recognizer.

### 5. Trigonometric identity

**Level:** Precalculus / college numerical methods

**Proposal example:**

```text
1 + 2*sin(x)^4
= 7/4 - cos(2*x) + (1/4)*cos(4*x)
```

The learner chooses the Pythagorean and double-angle/power-reduction identities.
The profile automates the algebraic and scalar cleanup that follows those
choices. Multiple profile-authorized identity paths should be accepted.

### 6. Verbose polynomial differentiation

**Level:** Introductory calculus

**Example:** `deriv (x^3 + 2*x) by x`

Require linearity first, then differentiate `x^3` and `2*x` in visible local
steps before cleaning up to `3*x^2 + 2`. This teaches the derivative rules on a
simple polynomial without trig or a product/chain-rule tree obscuring them.

### 7. Taylor approximation, first steps

**Level:** Introductory calculus

Use the integer polynomial `f(x) = x ** 2 + 3 * x + 2`. Find the first two
derivatives and assemble the second-order Taylor polynomial about `0`. This
keeps function application, derivative rules, and Taylor coefficients visible
without introducing trigonometric syntax or floating-point arithmetic.

### 8. Taylor approximation of the trigonometric example

**Level:** Calculus / numerical methods

**Proposal function:**

```text
f(x) = 7/4 - cos(2*x) + (1/4)*cos(4*x)
```

Use sequential `let` bindings to construct `f1 = D f`, `f2 = D f1`, and
optionally `f3 = D f2`; evaluate the coefficients about `x0 = 0.3`; then
assemble first-, second-, or third-order approximations. Hazel should automate
routine derivative and arithmetic cleanup while the learner controls the
derivative chain, approximation order, and polynomial assembly.

This is a calculus-profile exercise, not a Taylor math mode. Its final proof
export should produce reusable theorems for the stepped derivative bindings and
use them in the approximation result.

## Curriculum story

The exercises should make the amount of automation part of the pedagogical
argument:

- At the middle- and high-school level, expose the structure of the work.
  Require small distribution, factoring, and rewriting steps before enabling
  cleanup such as collecting like terms.
- At the college level, preserve the mathematically meaningful choices while
  automating routine algebra and arithmetic. A learner should choose a trig
  identity, derivative rule, or approximation order; the profile may clean up
  the resulting expression.
- Reuse the same problem at adjacent automation levels when that comparison is
  the point of the case study. The changed profile, not a changed answer,
  demonstrates the progression.

## Exercise sequences

### 1. Algebraic distribution

Case-study target: derive `(x + 1)^2 = x^2 + 2*x + 1`.

1. **Square to product.** Rewrite `(x + 1)^2` as
   `(x + 1) * (x + 1)`.
2. **FOIL, one distribution at a time.** Require the learner to distribute one
   factor across one sum at each step. Do not offer the polynomial-expansion
   macro or collect like terms automatically.
3. **FOIL with collection disabled.** Reach the fully distributed expression,
   including the two distinct middle terms, and stop before combining them.
4. **FOIL with collection enabled.** Repeat or continue the example with
   collecting like terms enabled so that the final cleanup becomes one step.
5. **Factoring in reverse.** Start from an expanded expression and use common
   factoring or the square-of-a-sum identity in the reverse direction.
6. **Completing the square.** Add a follow-on activity once the required
   rewrite support and goal checks are designed; do not disguise a general
   polynomial normalizer as a hand-worked completing-the-square step.

Profile snapshots:

- `foil-distribution-only`: Algebra parent; enable
  `alg.distribute_mul_add`; disable `alg.expand_polynomial`,
  `alg.collect_like_terms`, and broad affine/polynomial normalization.
- `foil-expanded-no-collection`: the same visible rules, with enough
  associativity/identity cleanup to select and replay a local distribution,
  but no `CollectLikeTerms` cleanup.
- `foil-with-collection`: inherit the previous profile and enable
  `CollectLikeTerms` for the final stage.
- `factoring-guided`: enable `alg.factor_common` and selected algebraic
  identities; keep polynomial factor normalization out of manual steps.

### 2. Trigonometric identities

Case-study target:
`1 + 2*sin(x)^4 = 7/4 - cos(2*x) + (1/4)*cos(4*x)`.

1. **Choose an identity.** Present the useful Pythagorean and double-angle
   rewrites as contextual alternatives without automatically choosing one.
2. **Power reduction, guided.** Require each trig-identity application and
   allow only modest local algebra cleanup.
3. **Power reduction, college level.** Keep the trig rewrite visible, then
   allow profile-directed algebra/scalar normalization to remove the tedium.
4. **Compare valid paths.** Let learners solve from a different first identity
   and compare the resulting derivations, rather than grading one exact path.

Profile snapshots:

- `trig-identities-guided`: Trigonometry parent; visible identity rules are
  enabled, but automatic algebraic normalization is restrained.
- `trig-identities-practitioner`: Trigonometry parent; keep identity selection
  explicit and enable profile-authorized algebra and scalar cleanup after it.

The existing `Ex_TrigPowerReduction` theorem is the natural seed, but its spec
must carry the exercise policy instead of inheriting the user's current
stepper settings.

### 3. Symbolic differentiation

Case-study target: differentiate `x^3 * sin(x)`.

1. **Rule identification.** Apply the product rule without simplifying either
   derivative.
2. **Derivative tree.** Step through power, trig, and chain rules one at a
   time, preserving the structure produced by each rule.
3. **Cleanup.** Enable scalar and identity cleanup only after the derivative
   structure has been constructed.
4. **Practitioner comparison.** Use the same target with profile-directed
   calculus enabled so a compound derivative may be checked or suggested in
   fewer steps.

Profile snapshots:

- `differentiation-rule-by-rule`: Calculus parent; derivative rules are
  visible in manual stepping, while broad automatic cleanup is disabled.
- `differentiation-with-cleanup`: inherit the rule-by-rule profile and enable
  derivative basics, scalar products, identities, and appropriate trig
  cleanup.
- `calculus-practitioner`: use the ordinary Calculus profile with the desired
  multi-step or automatic stage. This is a profile choice, not a separate
  derivative implementation.

### 4. Taylor approximation at the calculus level

Case-study function:
`f(x) = 7/4 - cos(2*x) + (1/4)*cos(4*x)`, expanded about `x0 = 0.3`.

This case study is a sequence of calculus exercises, not a Taylor math mode:

1. **Build the derivative chain.** Use sequential bindings such as
   `let f1 = D f`, `let f2 = D f1`, and `let f3 = D f2`. Step each bound
   expression and retain the relationship to the preceding binding.
2. **Evaluate coefficients.** Evaluate `f(x0)`, `f1(x0)`, `f2(x0)`, and
   optionally `f3(x0)`, allowing arithmetic automation at this level.
3. **Assemble approximations.** Construct first-, second-, and third-order
   Taylor polynomials from those named results. The learner chooses the order
   and assembly; the profile handles routine simplification.
4. **Export the theorem chain.** Export the stepped facts for `f1`, `f2`, and
   `f3`, then use them in the final evaluation/approximation theorem. This is
   the motivating acceptance test for multi-`let` Rocq export.
5. **Inspect propagation.** Optionally ask the learner to identify which
   coefficient changes when an earlier derivative is wrong.

Profile snapshots:

- Reuse `differentiation-with-cleanup` for the derivative-chain exercise.
- Reuse the ordinary Calculus profile at the multi-step/automatic stage for
  coefficient evaluation and polynomial assembly.
- If a locked variant is needed, name it for its instructional behavior (for
  example `calculus-approximation-practitioner`), not “Taylor mode.” It should
  be a stored Calculus-derived `CustomMathMode.definition`, not a new
  `rewrite_level`.

## Exercise math-policy design

The existing profile system already has the right reusable representation:
`CustomMathMode.definition` stores parents plus rule, cleanup, and automation
usage overrides, and resolves to an `Axioms.math_profile`. What is missing is
an exercise-owned policy and a way to lock it.

Add a serializable policy shared by theorem and stepper-style exercises:

```reason
type exercise_math_policy = {
  profile: CustomMathMode.definition,
  automation_stage: Axioms.automation_stage,
  lock_profile: bool,
  lock_automation_stage: bool,
  show_profile_summary: bool,
};
```

Implementation notes:

- Store a complete profile definition in the exercise, rather than a reference
  to the browser's custom-mode library. Exercises must behave consistently on
  another machine and after a teacher edits their personal library.
- Resolve and validate the profile using `CustomMathMode.resolve`; do not add a
  parallel whitelist or bypass the shared rule catalog.
- Initialize `StepperView.Model` from the exercise policy. Feed the resolved
  profile through the existing `active_profile` argument used by stepping,
  suggestions, checking, and Rocq export.
- When locked, hide or disable the Math Mode Builder, level selector, profile
  mutation controls, and automation-stage selector. Still show a read-only
  profile summary so the learner can see the mathematical capabilities in
  force.
- An unlocked instructor preview may clone the policy into the builder, but
  changes become part of the exercise only through an explicit “save exercise
  policy” action.
- Preserve the policy in exercise serialization, persistence, export, and
  grading. A restored attempt must not silently fall back to the default
  Calculus profile.
- Keep `write_out_steps` independent. It controls whether intermediate steps
  are required; the profile controls which transformations and cleanup are
  permitted.

## Progression and grading

Represent a case study as an ordered lesson containing exercise IDs. Each
exercise should declare:

- a starting expression or theorem;
- one or more acceptable goal expressions;
- its locked math policy;
- whether written steps are required;
- optional structural milestones, such as “a product-rule step occurred” or
  “reach fully distributed form before collecting terms”; and
- prerequisite exercise IDs.

Grade semantic equivalence and profile-authorized derivations, not one exact
string of steps. Structural milestones should refer to recorded rule IDs and
derivation states, not recognize a particular sample expression.

Suggested progression:

1. order of operations;
2. verbose FOIL;
3. FOIL with collection and greater automation;
4. completing the square;
5. the trigonometric identity;
6. verbose polynomial differentiation; and
7. the calculus-level Taylor derivative/evaluation/assembly chain.

## Implementation phases

### Phase 1: policy plumbing

1. Add `exercise_math_policy` and a safe default for existing exercises.
2. Add it to relevant exercise specs, serializers, model conversion, and
   persistence.
3. Initialize the stepper with the exercise's profile and automation stage.
4. Implement locked/read-only controls and a visible profile summary.
5. Test that a disabled rule is rejected in manual steps, checking,
   suggestions, automatic cleanup, and Rocq export.

### Phase 2: algebra and trig pilot

1. Encode the three FOIL profiles and two trig profiles as exercise-owned
   definitions.
2. Build the distribution progression and adapt `Ex_TrigPowerReduction`.
3. Add milestone grading for distribution-before-collection and explicit trig
   identity use.
4. Use these pilots to refine authoring UI before creating every exercise.

### Phase 3: calculus and Taylor sequence

1. Build the guided and practitioner differentiation exercises.
2. Build the Taylor derivative-chain, coefficient, and assembly exercises
   using only Calculus-derived profiles.
3. Exercise the recent multi-`let` export with one, two, and three derivatives
   that refer to prior bindings.
4. Verify exported Rocq for each intermediate theorem and the final assembled
   result.

### Phase 4: completing the square

1. Specify general completing-the-square operations and grading before adding
   the activity.
2. Implement the exercise without an example-specific recognizer or an
   unrestricted polynomial-normalization fallback.

## Acceptance tests

- A locked exercise opens with the same profile on a fresh browser and after a
  saved attempt is restored.
- Learners cannot enable a forbidden rule or cleanup through the builder,
  profile board, automation menu, suggestion flow, or proof search.
- Manual FOIL accepts one distribution at a time but rejects direct polynomial
  expansion and premature collection.
- The collection-enabled FOIL exercise accepts the same derivation plus the
  final collection step.
- Guided trig accepts multiple profile-authorized identity paths.
- Guided differentiation exposes product/chain steps; practitioner calculus
  can automate their routine descendants without bypassing its profile.
- Taylor exercises use a Calculus-derived profile, contain no Taylor macro or
  rewrite level, and export a proof chain through sequential derivative
  bindings.
- No inequality exercise, rule, or numerical error-bound requirement is added.
- Existing exercises without a policy retain their current behavior.
