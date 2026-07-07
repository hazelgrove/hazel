# Rocq Tactic Plan Architecture

Last updated: July 7, 2026

This note records the proof-of-concept results and implementation plan for
parameterizing Hazel math levels by ordered Rocq tactic plans. The goal is to
support pedagogical distinctions like "apply this rewrite once", "repeat this
safe simplification until stuck", and "use this broad solver only as a final
certificate", while leaving room for course and user customization later.

## Motivation

The current math-profile work parameterizes levels by rewrite groups and a
single Rocq tactic group name:

```reason
type math_profile = {
  level: rewrite_level,
  rank: int,
  label: string,
  detail: string,
  enabled: bool,
  groups: list(rewrite_group),
  rocq_macro_rule_id: string,
  rocq_tactic_group: string,
  rocq_domain_policy,
};
```

That is a useful first cut, but the Rocq side is still too coarse. A tactic
group such as `hazel_algebra` hides several policy decisions:

- Some tactics represent one visible mathematical step.
- Some tactics are safe to repeat until no progress is possible.
- Some tactics should be bounded by fuel.
- Some tactics are broad finishers and should not justify a lower-scope
  pedagogical step by themselves.

We want the active math profile to own an explicit plan for these tactics.

## Local Coqc Experiment

A throwaway Rocq file was compiled locally with Rocq 9.0.0:

```sh
coqc /private/tmp/hazel_tactic_plan_poc.v
```

The experiment confirmed the following.

1. A single rewrite tactic can validate one visible step:

```coq
Goal forall x y : Z, 2 * (x + y) = 2 * x + 2 * y.
Proof.
  intros x y.
  hazel_distribute_once.
  reflexivity.
Qed.
```

2. The same once tactic does not validate a multi-distribution expansion:

```coq
Goal forall a b c d : Z,
  (a + b) * (c + d) = (a * c + b * c) + (a * d + b * d).
Proof.
  intros a b c d.
  Fail (hazel_distribute_once; reflexivity).
Abort.
```

3. Repetition can validate a multi-step expansion:

```coq
Goal forall a b c d : Z,
  (a + b) * (c + d) = (a * c + b * c) + (a * d + b * d).
Proof.
  intros a b c d.
  hazel_distribute_repeat.
  reflexivity.
Qed.
```

4. Fuel gives a useful bound:

```coq
Goal forall a b c d : Z,
  (a + b) * (c + d) = (a * c + b * c) + (a * d + b * d).
Proof.
  intros a b c d.
  Fail (hazel_repeat_fuel 1%nat hazel_distribute_once; reflexivity).
Abort.

Goal forall a b c d : Z,
  (a + b) * (c + d) = (a * c + b * c) + (a * d + b * d).
Proof.
  intros a b c d.
  hazel_repeat_fuel 3%nat hazel_distribute_once.
  reflexivity.
Qed.
```

5. Safe oriented rewrites are good repeat candidates:

```coq
Goal forall x : Z, 0 + (x + 0) = x.
Proof.
  intro x.
  hazel_safe_arith_repeat.
  reflexivity.
Qed.
```

6. Symmetric rewrites such as commutativity should be policy-marked as once
   steps, not repeated blindly.

7. Broad solvers can exceed a pedagogical level. For example, `lia` proves
   some distributivity-by-constant goals, so `lia` should not be a raw
   Arithmetic one-step validator.

8. `ring` over `R` proves algebra through opaque trig calls:

```coq
Goal forall x : R, 2 * (cos x + 1) = 2 * cos x + 2.
Proof.
  intro x.
  ring.
Qed.
```

9. Plain `ring` does not prove trig identities:

```coq
Goal forall x : R, (sin x) ^ 2 + (cos x) ^ 2 = 1.
Proof.
  intro x.
  Fail ring.
Abort.
```

This supports the design where Algebra can reason over preserved trig
applications as opaque ring atoms, while trigonometric identities remain gated
behind the Trigonometry level.

## Proposed Data Model

Replace the single tactic-group string with a tactic plan. Keep the current
string temporarily for compatibility while we migrate call sites.

```reason
type tactic_mode =
  | Once
  | TryOnce
  | RepeatUntilStuck
  | RepeatFuel(int)
  | FinishOnly;

type rocq_tactic_step = {
  id: string,
  label: string,
  tactic: string,
  mode: tactic_mode,
  rule_ids: list(string),
};

type rocq_tactic_plan = {
  id: string,
  label: string,
  steps: list(rocq_tactic_step),
};
```

Then extend `math_profile`:

```reason
type math_profile = {
  level: rewrite_level,
  rank: int,
  label: string,
  detail: string,
  enabled: bool,
  groups: list(rewrite_group),
  rocq_macro_rule_id: string,
  rocq_tactic_group: string, /* compatibility */
  rocq_tactic_plan: rocq_tactic_plan,
  rocq_domain_policy,
};
```

## Tactic Modes

`Once`

Use for one visible step. If the step does not solve the local goal after one
application plus `reflexivity`, it should fail.

Examples:

- distribute once
- factor once
- commute selected addition once
- apply a selected trig identity once

`TryOnce`

Use for optional cleanup that should not make the plan fail. This is useful for
small normalizing steps after the main step.

`RepeatUntilStuck`

Use only for oriented simplifications that are known to make monotonic
progress. Every repeated tactic should be wrapped with `progress`.

Examples:

- remove additive identity
- remove multiplicative identity
- unfold a bounded simplifier with a clear decreasing measure

`RepeatFuel(n)`

Use for bounded expansion/search. This gives customization a safe knob without
allowing unbounded proof search.

Examples:

- bounded distributivity
- bounded rewrite search
- small AC normalization

`FinishOnly`

Use for broad solvers that can certify a result but should not define the
pedagogical step shape.

Examples:

- `ring`
- `lia`
- `nia`
- `nra`

These should be available in check-result or export modes, but not as the only
evidence that a one-step Arithmetic or Algebra rewrite is pedagogically valid.

## Rocq Compilation Strategy

The Hazel-side compiler should emit tactic scripts from tactic plans.

Example plan:

```reason
{
  id: "hazel_algebra_plan",
  label: "Algebra",
  steps: [
    {
      id: "distribute_once",
      label: "distribute once",
      tactic: "hazel_distribute_once",
      mode: Once,
      rule_ids: ["alg.distribute_mul_add"],
    },
    {
      id: "safe_arith_cleanup",
      label: "safe arithmetic cleanup",
      tactic: "hazel_safe_arith_step",
      mode: RepeatFuel(8),
      rule_ids: ["arith.add_zero", "arith.mul_zero", "arith.const_fold"],
    },
    {
      id: "ring_finish",
      label: "ring certificate",
      tactic: "ring",
      mode: FinishOnly,
      rule_ids: ["alg.expand_polynomial", "alg.collect_like_terms"],
    },
  ],
}
```

Generated Rocq shape:

```coq
hazel_distribute_once.
hazel_repeat_fuel 8%nat hazel_safe_arith_step.
try ring.
reflexivity.
```

Mode compilation:

```coq
(* Once *)
TACTIC.

(* TryOnce *)
try TACTIC.

(* RepeatUntilStuck *)
repeat progress TACTIC.

(* RepeatFuel(n) *)
hazel_repeat_fuel n%nat TACTIC.

(* FinishOnly *)
try TACTIC.
```

The shared prelude should include:

```coq
Ltac hazel_repeat_fuel n tac :=
  lazymatch n with
  | O => idtac
  | S ?n' =>
    first [
      progress tac; hazel_repeat_fuel n' tac
    | idtac
    ]
  end.
```

## Suggested Built-In Plans

Arithmetic one-step:

- once: selected constant fold
- once: selected `+ 0`, `0 +`, `* 1`, `1 *`
- once: selected add/mul commutativity
- finish only: a very constrained constant arithmetic tactic, not raw `lia`

Arithmetic repeat/simplify:

- repeat/fuel: safe identity removal
- repeat/fuel: adjacent constant folding
- no symbolic collection through trig/function terms

Algebra one-step:

- once: distribution
- once: factoring
- once: cancellation
- once: power split
- finish only: `ring` only when the local step is already classified as
  algebraic

Algebra check-result:

- repeat/fuel: bounded distributivity and factoring
- finish only: `ring` or `nia` as certificate after the profile permits the
  construct policy

Trigonometry one-step:

- once: selected trig identity
- try once: algebra cleanup after the trig identity, if the UI represents that
  as part of a macro step

Trigonometry check-result:

- once/repeat-fuel: allowed trig identities
- repeat/fuel: algebra cleanup
- finish only: real algebra tactics such as `ring`/`nra`

## Customization Model

Customization should start as profile overlays over known tactic atoms, not as
arbitrary Rocq script text.

```json
{
  "id": "algebra_i_distribution_unit",
  "label": "Algebra I: Distribution",
  "extends": "algebra",
  "enabledRuleIds": [
    "arith.const_fold",
    "arith.add_zero",
    "arith.mul_one",
    "alg.distribute_mul_add"
  ],
  "disabledRuleIds": [
    "alg.factor_common",
    "alg.power_add",
    "alg.power_mul"
  ],
  "tacticPlan": [
    {"tactic": "safe_arith_step", "mode": "repeat_fuel", "fuel": 8},
    {"tactic": "distribute_once", "mode": "once"}
  ]
}
```

Course and user profiles should be able to configure:

- enabled rule IDs
- disabled rule IDs
- tactic step order
- tactic mode
- tactic fuel
- allowed automation stages
- maximum local search depth
- construct policy, such as whether Algebra may treat preserved trig calls as
  opaque atoms
- domain policy, such as integers by default or reals by default

The first customization surface should only expose a curated tactic atom
registry:

```reason
type tactic_atom = {
  id: string,
  label: string,
  rocq_tactic: string,
  allowed_modes: list(tactic_mode),
  rule_ids: list(string),
};
```

Later, advanced users can add custom lemmas, but only after Rocq verifies them.
Verified custom lemmas can then become normal rewrite rules.

## Implementation Plan

Phase 1: Internal tactic-plan types

- Add `tactic_mode`, `rocq_tactic_step`, and `rocq_tactic_plan`.
- Add `rocq_tactic_plan` to `math_profile`.
- Keep `rocq_tactic_group` for compatibility during migration.
- Add unit tests for profile plan contents.

Phase 2: Rocq script generation

- Add a tactic-plan compiler in the proof export/search backend.
- Emit `hazel_repeat_fuel` in the Rocq prelude.
- Replace tactic-group emission in `ProofSearchBackend.rocq_search_program`
  with generated plan scripts.
- Add tests for generated Rocq snippets.

Phase 3: Split tactic atoms by pedagogical role

- Extract current monolithic `hazel_arithmetic`, `hazel_algebra`, and
  `hazel_trigonometry` Ltac definitions into smaller atoms.
- Mark each atom with its allowed modes.
- Move broad solvers such as `lia`, `nia`, `ring`, and `nra` to `FinishOnly`
  unless the profile explicitly permits them for check-result mode.

Phase 4: Connect plans to UI modes

- One Step should run only `Once` and tightly scoped `TryOnce` steps relevant
  to the selected rule.
- Check Result may use `RepeatFuel` and `FinishOnly` steps after construct
  policy passes.
- Auto simplify may use repeat/fuel plans, but should still emit trace
  summaries that explain which profile and tactic atoms were used.

Phase 5: Profile overlays

- Define a serializable `math_profile_config`.
- Resolve built-in profile plus course overlay plus user overlay.
- Validate overlays against the tactic atom registry.
- Reject unknown tactic atoms or illegal mode choices.

Phase 6: Verified custom lemmas

- Allow custom lemma statements and proofs in advanced profiles.
- Run Rocq verification before registering the lemma.
- Convert verified lemmas into normal `rewrite_rule` entries.
- Treat failed verification as a profile validation error.

## Open Questions

- Should `FinishOnly` be allowed in One Step when the local Hazel checker has
  already classified the step as a specific rule?
- How much cleanup can be bundled into a visible macro step before the UI
  should show multiple steps?
- Should repeated distributivity be available only in Check Result, or also as
  a named macro such as "expand polynomial"?
- What is the best serialized format for course profiles: JSON, Hazel module
  metadata, or a small DSL?
- Should user profiles be stored per scratchpad, per course, or globally?

## Recommendation

Implement tactic plans internally first, with no user-facing customization.
Use them to replace monolithic tactic groups and to make the one-step versus
repeat/check-result distinction explicit.

After that, expose customization as safe profile overlays over a curated tactic
atom registry. Do not expose arbitrary Rocq text until custom lemma
verification is implemented.
