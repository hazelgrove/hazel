# Generic Step Policy and Profile Board Plan

Last updated: July 7, 2026

This note plans the next step after the first strict distribution checker. The
goal is to replace distribution-specific one-step logic with a generic policy
system that can say:

- apply exactly one visible mathematical rule
- permit selected hidden cleanup around that visible rule
- reject hidden cleanup that the active profile does not allow
- eventually certify the accepted step in Rocq/JSCoq using only allowed facts

The same system should also support a proof-of-concept customization board for
math levels, so Arithmetic, Algebra, Trigonometry, and future levels can be
scripted from one front-end model.

## Target Behavior

For a profile such as "Algebra, manual distribution, AC cleanup allowed":

```text
x * (1 + 2 + x)
```

should accept any target produced by one distribution plus allowed association
and commutation:

```text
x*1 + x*2 + x*x
1*x + 2*x + x*x
x*x + 2*x + 1*x
```

but should reject targets that require additional cleanup:

```text
x*3 + x*x       // constant folding
x + x*2 + x*x   // multiplicative identity simplification
x + 2*x + x**2  // identity simplification plus power notation/collection
```

Another profile could enable those extra cleanup capabilities and accept a
larger visible step without changing the distribution implementation itself.

## Core Model

Add a small vocabulary for hidden cleanup capabilities:

```reason
type cleanup_capability =
  | AddAssoc
  | AddComm
  | MulAssoc
  | MulComm
  | AddIdentity
  | MulIdentity
  | ConstFold
  | PowerNotation
  | CollectLikeTerms;
```

Add a generic policy for visible rules:

```reason
type visible_step_mode =
  | VisibleOnce
  | VisibleRepeatFuel(int)
  | VisibleRepeatUntilStuck;

type visible_rule_policy = {
  rule_id: string,
  mode: visible_step_mode,
  allowed_cleanup: list(cleanup_capability),
};
```

Then replace the current narrow `one_step_policy` fields with a policy map:

```reason
type step_policy = {
  visible_rules: list(visible_rule_policy),
  default_cleanup: list(cleanup_capability),
};
```

`math_profile` should eventually own:

```reason
type math_profile = {
  ...
  step_policy,
  rocq_tactic_plans,
};
```

The existing Rocq tactic plans remain the certificate/checking side. The new
step policy is the Hazel-side pedagogical shape layer.

## Validation Pipeline

Manual one-step validation should become:

1. Read the active `math_profile.step_policy`.
2. Enumerate candidate applications of enabled visible rules.
3. Apply exactly the configured amount of visible work, such as one
   distribution or one trig identity.
4. Compare the user's target modulo only the rule's `allowed_cleanup`.
5. If the target matches, emit a structured proof trace:

```json
{
  "visibleRule": "alg.distribute_mul_add",
  "visibleMode": "VisibleOnce",
  "cleanup": ["AddAssoc", "AddComm", "MulAssoc", "MulComm"],
  "from": "x * (1 + 2 + x)",
  "to": "x*1 + x*2 + x*x"
}
```

6. Ask Rocq/JSCoq to certify the equality. Initially this can still use the
   existing profile tactic plan, but the long-term certificate should replay
   the visible rule and cleanup capabilities explicitly.

Check-result and auto-simplify modes can use broader policies:

- Check result: allow repeat/fuel and finish-only tactics.
- Auto simplify: allow repeat-safe cleanup and selected normalizers.
- Manual one step: never allow a broad finisher as the only evidence.

## Cleanup Matching Strategy

Do not treat cleanup as general algebraic equality. Each cleanup capability
should correspond to a bounded equivalence relation or normalizer.

Initial capabilities:

`AddAssoc`

Flatten addition trees. Preserve term order.

`AddComm`

Sort or multiset-compare flattened addition terms. Only active if `AddAssoc`
is also active or the selected local shape is already flat.

`MulAssoc`

Flatten multiplication trees. Preserve factor order.

`MulComm`

Sort or multiset-compare flattened multiplication factors. Only active if
`MulAssoc` is also active or the selected local shape is already flat.

`ConstFold`

Evaluate adjacent integer constants or all integer constants, depending on a
later parameter. Start with adjacent-only in one-step contexts.

`MulIdentity` and `AddIdentity`

Remove `* 1`, `1 *`, `+ 0`, and `0 +`. These should be separate because many
teaching modes want distribution without immediately hiding `x * 1`.

`PowerNotation`

Treat repeated multiplication as powers, such as `x*x = x**2`.

`CollectLikeTerms`

Combine monomial coefficients. This is a larger algebra cleanup and should be
off in strict one-step Algebra.

## Implementation Phases

### Phase 1: Data Model

- Add `cleanup_capability`, `visible_step_mode`, `visible_rule_policy`, and
  `step_policy` to `Axioms.re`.
- Keep the current `distribution_step_policy` temporarily as a compatibility
  shim.
- Define named policy presets:
  - `arithmetic_strict_one_step_policy`
  - `algebra_manual_distribution_ac_policy`
  - `algebra_permissive_simplifying_policy`
  - `trigonometry_selected_identities_policy`
- Add label helpers for UI and tests.

### Phase 2: Generic Cleanup Matcher

- Extract the current association-only distribution comparison into a generic
  cleanup matcher.
- Implement `expression_matches_under_cleanup(cleanup, expected, actual)`.
- Start with `AddAssoc`, `AddComm`, `MulAssoc`, and `MulComm`.
- Add tests that verify AC cleanup does not imply constant folding, identity
  removal, power notation, or collection.

### Phase 3: Generic Visible Rule Driver

- Represent a visible rule application as:

```reason
type visible_rule_candidate = {
  rule_id: string,
  from_local: exp,
  to_local: exp,
  from_whole: exp,
  to_whole: exp,
  occurrence: int,
};
```

- Refactor distribution to produce candidates instead of directly deciding
  validity.
- Add a generic validator:

```reason
validate_one_visible_rule_step(profile, from_exp, user_to_exp)
```

which enumerates candidates and tests the user's target under the candidate's
allowed cleanup.

### Phase 4: Rocq/JSCoq Certificate Alignment

- Map each visible rule and cleanup capability to a prover atom:
  - `alg.distribute_mul_add`
  - `arith.add_assoc`
  - `arith.add_comm`
  - `arith.mul_assoc`
  - `arith.mul_comm`
- For the first pass, continue using the existing purpose-specific tactic plan
  to check the final equality.
- Then add a stricter export path that emits a proof script from the recorded
  visible rule plus cleanup atoms.
- Keep `ring`, `lia`, `nia`, and `nra` as `FinishOnly` for check-result, not
  one-step justification.

### Phase 5: Extend Beyond Distribution

- Add factoring as the reverse visible rule.
- Add selected trig identities as visible rules with the same cleanup layer.
- Add polynomial expansion as either:
  - a repeat/fuel visible rule sequence, or
  - a macro rule with recorded primitive substeps.
- Add collection/cancellation as explicit visible rules, not accidental cleanup,
  unless a profile opts into permissive algebra simplification.

## Profile Board Product Path

Build the board as the first working customization surface, not as a throwaway
debug panel. The first version can be intentionally small, but it should use the
same profile data model that one-step validation and Rocq/JSCoq checking use.
That keeps experiments honest: if the board says a profile allows one
distribution plus AC cleanup, the checker should make the same decision.

### Product Shape

Start as an in-app "Profile Board" that can run beside the stepper:

- a compact panel near the existing math-level controls for local testing
- a larger board view once profile editing becomes a first-class workflow
- the same underlying model for built-in profiles, teacher-authored profiles,
  per-assignment overrides, and imported experiments

The board should not fork policy logic. It should edit a `step_policy` value,
run the normal checker, and display the normal checker result.

### Board Sections

`Level`

Choose Arithmetic, Algebra, Trigonometry, Functions/Lists, or Calculus.

`Visible Rules`

List rule toggles grouped by level:

- constant fold
- adjacent add swap
- reorder add terms
- reorder mul factors
- distribute multiplication over addition
- factor common term
- collect like terms
- trig identities, each separately toggleable

`Cleanup Allowed In One Step`

Checkboxes for cleanup capabilities:

- addition association
- addition commutation
- multiplication association
- multiplication commutation
- additive identity
- multiplicative identity
- constant folding
- power notation
- collect like terms

`Automation Modes`

Per mode, show the profile policy:

- One Step
- Check Result
- Auto Simplify

Each mode should expose the visible step mode:

- once
- repeat with fuel
- repeat until stuck
- finish only

`Examples`

Hard-code a small set of test examples at first:

```text
2 * (1 + 2) -> 2*1 + 2*2
2 * (1 + 2) -> 6
x * (1 + x) -> x*1 + x*x
x * (1 + x) -> x + x**2
x * (1 + 2 + x) -> 1*x + 2*x + x*x
sin(x)^2 + cos(x)^2 -> 1
sin(x+y) -> sin(x)*cos(y) + cos(x)*sin(y)
```

For each example, show:

- Accepted or rejected.
- Which visible rule matched.
- Which cleanup capabilities were used.
- Whether Rocq/JSCoq certified the equality.
- The generated proof plan or tactic summary.

### Serialization And Persistence

Make the board read and write a JSON-like profile object from the beginning:

```json
{
  "id": "algebra.manual_distribution_ac",
  "extends": "algebra",
  "oneStep": {
    "visibleRules": [
      {
        "ruleId": "alg.distribute_mul_add",
        "mode": "once",
        "allowedCleanup": [
          "add.assoc",
          "add.comm",
          "mul.assoc",
          "mul.comm"
        ]
      }
    ]
  }
}
```

This lets us later support:

- built-in profiles
- teacher-authored profiles
- per-assignment overrides
- import/export for experiments
- generated Rocq plan previews

Initial persistence can be local only:

- start from built-in profiles in `Axioms.re`
- edit a draft copy in UI state
- export/import the profile JSON manually
- optionally store the latest draft in browser local storage after the checker
  behavior is stable

Server-backed persistence can wait until the semantics are settled.

### Board Implementation Phases

`Board Phase 1: Pure Model Preview`

- Add pure functions that summarize a `step_policy`.
- Add pure functions that run the fixed example suite against a selected
  profile.
- Expose enough data for the UI to show accepted/rejected examples and matched
  rule IDs.

`Board Phase 2: In-App Read-Only Board`

- Render the active built-in profile.
- Render the example suite.
- Show rule toggles and cleanup checkboxes as disabled controls if write-back is
  not ready yet.
- Use this to verify the product shape without changing checker behavior from
  the UI.

`Board Phase 3: Editable Draft Board`

- Let toggles mutate an in-memory draft profile.
- Re-run the examples after every change.
- Keep draft profiles separate from built-in profiles.

`Board Phase 4: Import/Export`

- Export the draft profile as JSON.
- Import a JSON profile into the board.
- Validate unknown rule IDs and cleanup IDs before using them.

`Board Phase 5: Assignment/User Integration`

- Attach a profile ID or profile JSON to an assignment.
- Let the stepper receive an active profile value rather than only a built-in
  rewrite level.
- Keep Rocq/JSCoq tactic plan preview visible so users can see what the profile
  can formally certify.

## Testing Plan

Unit tests should lead the implementation.

Data model tests:

- each built-in profile exposes the expected cleanup capabilities
- labels serialize consistently
- profile lookup remains cumulative by math level

Cleanup matcher tests:

- `AddAssoc` accepts reassociation only
- `AddComm` accepts reordering only when enabled
- `MulComm` accepts factor reordering only when enabled
- AC cleanup rejects constant folding and identity cleanup
- enabling identity cleanup changes only identity examples

Visible rule tests:

- strict Algebra accepts `x*(1+x) -> x*1+x*x`
- strict Algebra accepts `x*(1+2+x) -> x*1+x*2+x*x`
- AC Algebra accepts reordered products and sums
- strict Algebra rejects `x*(1+2+x) -> x*3+x*x`
- strict Algebra rejects `x*(1+x) -> x+x**2`
- permissive Algebra accepts the same examples when corresponding cleanup is
  enabled

Rocq/JSCoq tests:

- accepted one-step examples still generate a valid check script
- rejected one-step examples are rejected before prover fallback
- check-result mode can still accept broader equivalence when its profile
  allows broad finishers

Board tests:

- toggling cleanup capabilities changes example acceptance immediately
- profile JSON round-trips without changing behavior
- examples report the matched visible rule and cleanup set
- read-only board results match direct checker unit tests
- imported profiles reject unknown visible rule IDs and unknown cleanup IDs

## Open Decisions

- Whether `AddComm` should imply `AddAssoc` for flattened UI expressions, or
  whether the profile must enable both explicitly.
- Whether constant folding in one-step contexts means adjacent constants only
  or all constants in a flattened sum/product.
- Whether broad AC reordering should be represented as a cleanup capability, a
  visible macro rule, or both depending on profile.
- How soon to require Rocq/JSCoq scripts to replay cleanup atoms explicitly
  instead of using the existing profile tactic plan as a certificate.
- Whether the profile board should ship only in development mode at first.
