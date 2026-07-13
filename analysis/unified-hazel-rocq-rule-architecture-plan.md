# Unified Hazel/Rocq Rule Architecture Plan

## Goal

Unify `One Step`, `Check Result`, and `Auto Eval` around one profile rule
system. Each math profile should declare:

- which rules exist
- which rules are enabled
- which rules are visible student steps
- which cleanup rules are automatic
- how often each rule may run
- how Hazel checks it
- how Rocq proves it

The target architecture is:

```text
profile
  -> stage plan
     -> Hazel checker/search
     -> Rocq tactic program
     -> UI controls
     -> export metadata
```

## Phase 1: Stabilize The Rule Catalog

Add a real rule catalog in `Axioms.re` or a new module such as
`MathRuleCatalog.re`.

Each rule should have:

```text
id
display name
short name
example
level
kind: visible | cleanup | normalization | tactic-only
directions: forward | backward | both
hazel backend id
rocq backend id
default stage policies
```

Example:

```text
alg.distribute_mul_add
  kind: visible
  one_step: once
  check_result: repeat/search
  auto_eval: disabled or deterministic
  hazel backend: distribution matcher
  rocq backend: hazel_distribute_mul_add
```

## Phase 2: Define Stage Plans

Create a shared stage-plan type:

```text
stage_plan:
  stage: OneStep | CheckResult | AutoEval
  phases:
    - pre_cleanup
    - visible_step
    - post_cleanup
    - finish
```

One Step plan:

```text
pre_cleanup: cleanup rules, normalize
visible_step: exactly one visible rule
post_cleanup: cleanup rules, normalize
finish: none
```

Check Result plan:

```text
search: visible + cleanup rules, bounded search
finish: allowed tactic finisher
```

Auto Eval plan:

```text
normalize: deterministic simplification rules, repeat until stuck
```

## Phase 3: Wrap Current Hazel Logic As Rule Backends

Do not rewrite everything immediately. First wrap existing behavior behind
catalog-facing backend ids.

Current mappings:

```text
alg.distribute_mul_add -> existing distribution checker
alg.factor_common -> existing factoring checker
alg.cancel_common_add -> existing cancellation checker
trig.* -> existing trig rewrite checker
power.notation -> existing x*x == x**2 cleanup
AC cleanup -> existing associative/commutative comparison
```

This gives the profile system a generic API while preserving current behavior.

## Phase 4: Route One Step Through Stage Plans

Refactor `RewriteChecker.re` so One Step no longer directly asks:

```text
is this distribution?
is this factoring?
is this trig?
```

Instead:

```text
resolve active profile
compile One Step stage plan
try exactly one visible rule backend
compare modulo allowed cleanup
return trace with rule ids
```

Target behavior:

```text
x * (1 + 2 + x) -> 1*x + 2*x + x**2
```

is valid when `Comm *` and `Power notation` are enabled, and invalid when those
cleanup capabilities are disabled.

## Phase 5: Make Rocq Tactic Plans Rule-Based

Add rule-level Rocq metadata:

```text
rule_id -> tactic atom / lemma / tactic group
application mode: once | try_once | repeat | bounded | finish
```

Then compile:

```text
profile + stage -> Rocq tactic script
```

For One Step:

```text
cleanup*
one visible rule
cleanup*
```

For Check Result:

```text
bounded search over allowed rules
optional finisher
```

For Auto Eval:

```text
repeat deterministic simplifier
```

## Phase 6: Align Export With Accepted Traces

Export should prefer the exact accepted Hazel trace:

```text
accepted Hazel rule ids
-> corresponding Rocq tactic atoms
-> generated proof
```

Fallback tactic groups can remain, but should be labeled as macro proof
fallback rather than precise replay.

Important invariant:

```text
If Hazel accepts a One Step move, Rocq export should know the same rule that
justifies it.
```

## Phase 7: Persist Profiles And Customization

Once rule and stage plans are stable, persist profiles as data:

```json
{
  "base": "Algebra",
  "visible_rules": {
    "alg.distribute_mul_add": true,
    "alg.factor_common": true
  },
  "cleanup_rules": {
    "add.comm": true,
    "mul.comm": false,
    "power.notation": true
  },
  "stage_overrides": {
    "OneStep": {
      "alg.distribute_mul_add": "once"
    }
  }
}
```

This enables custom course modes without changing code.

## Phase 8: Tests

Catalog tests:

```text
every enabled Hazel rule has Rocq metadata
every profile rule id resolves
unknown ids fail loudly
```

Behavior tests:

```text
One Step accepts exactly one visible operation plus cleanup
Check Result accepts multi-rule proofs
Auto Eval produces expected normalized result
disabled rules reject formerly valid steps
```

Export tests:

```text
accepted One Step trace exports to Rocq
Rocq proof uses matching rule ids
Hazel/Rocq disagreement is surfaced
```

## Suggested Implementation Order

1. Add rule catalog types and populate them from existing metadata.
2. Add stage-plan types and compile built-in profiles to plans.
3. Wrap current Hazel distribution, factoring, cancellation, and trig checks as
   rule backends.
4. Route One Step through the stage plan.
5. Route Rocq tactic generation through the same stage plan.
6. Add exact trace-to-export mapping.
7. Persist profile overrides.
8. Add custom identity authoring.

## Realistic Scope

MVP:

- Algebra and trigonometry visible rules
- global cleanup toggles
- One Step and Check Result stage plans
- Rocq export labels tied to rule ids

Full version:

- custom identities
- repeat semantics
- rule authoring UI
- exact Rocq replay for all cleanup
- Auto Eval fully profile-driven

The current branch has many of the data pieces: profiles, visible-rule metadata,
cleanup toggles, purpose-specific Rocq plans, and profile-aware one-step checks.
The largest remaining work is replacing hard-coded Hazel checker dispatch with
a shared stage-plan interpreter and connecting every catalog rule to a Rocq
backend.
