# Standardness completion (FR-std-4): contracts and limits

This is the candidate base for 3f597d47. It is not accepted main. Observations come from leased batch `b1` (std-c1) and are copied in `fleet/reports/reset-2026-10-28/Reset-standardness-completion/b1/alcotest-out`.

## Policy that settles these contracts

- **D65/D66/D67:** automatic discharge stays conservative.
  - Exact fact match, without instantiation.
  - Closed evaluation, with no fixpoints.
  - The structural `!!` routine.
- **ADDENDUM-5 (G1-prime):** a typed operation needs actual operand class evidence.
  - Unknown is never Int/Bool evidence.
  - No class is inferred for an application, `if` or `case` result.
- **D56:** a proof with pending conditions is **not proven**. It shows a quiet count of the pending conditions.
  - `ProvenModulo` remains only an internal carrier.

An unsupported proof therefore stays **Pending**. It is not Proven and it is not refused. The original expectations below are kept verbatim in the test sources, next to the new assertion.

## Compatibility-loss table

| Test | Original expectation | Observed now (b1) | Reason | Supported alternative / limit |
|---|---|---|---|---|
| Core 21 `structural standardness judgement` | `x + 1` with `x` marked standard but **untyped**: standard | not standard | Unknown is not Int evidence (ADDENDUM-5) | A standard `x : Int` makes `x + 1` standard (new typed control, passes) |
| Recursive `len_unannotated_list_arg` | witness `len(xs)`, `!!len(xs)`: Structural | Pending | `1 + len(t)` adds an application result, and there is no result-class inference | No automatic proof. A manual proof of the `!!` premise is needed (limit) |
| Recursive `rev via snoc` | every `!!` gate discharged | exactly one Pending gate, the inlined `rev(t) @ [x]`; all others Structural | `@` on an application result is not list evidence | limit, as above |
| Recursive `rebinding_inner_lambda` | Structural | Pending | `(fun (r : Int) -> r)(r(t))` ascribes an application result, which has no evident type (rec-3) | write the inner binder unannotated, or accept the limit |
| Equivalence 1 `lenfold` | no obligations, **Proven** | 1 Pending gate (inlined `len`), ProvenModulo, so shown as not proven | as `len_unannotated` | limit |
| Equivalence 2 `lenfoldf` | no obligations, **Proven** | 1 Pending gate (inlined `len`) | same | limit |
| Equivalence 7 `lenfa` | no obligations, **Proven** | 1 Pending gate (inlined `len`) | same | limit |
| Equivalence 10 `snoc` | **Proven** | 1 Pending gate (inlined `rev`) | `rev(t) @ [x]` | limit |
| Equivalence 11 `invol` | no obligations, **Proven** | 1 Pending gate (inlined `rev`) | same | limit |
| Equivalence 15 `round` (ADT `to_int`) | exactly one obligation (the positivity guard), ProvenModulo | one ordinary obligation (the guard) plus 2 Pending `to_int` gates and 1 Structural gate `!!m`; still ProvenModulo | ADT recursion is not certified, and the result is an application | limit |
| Equivalence 13 `pin: rewrite does not match through the environment` | (inherited) | **FAIL**, unchanged | fails on the 440 base as well; unrelated to standardness | not in scope |
| Operand-class `generalize_case_binder_shadow` (g1pC) | Pending | the g1pC fixture was **malformed**: static error "Variable end is not bound" | a stray third `end` | kept verbatim as a historical negative (static error). The corrected statics-clean pair: shadowing case binder gives **Pending**; non-shadowing binder gives **Structural** |
| Evaluator.Contradiction 0-4, 6 | "contradiction incurs nothing": no obligations at all | **b2: FAIL** (1–2 obligations each); after the std-c3 contract, see b3 | each fixture's `assume P =>` records its D47/D51 `!!P` gate, which is a premise of the assume and not something the contradiction incurs | count only ORDINARY obligations, and require the contradiction node itself to record none; gates are printed |
| Evaluator.ProofMap 28 `implication intro via assume` | untyped `forall x`: `!!(x == 2)` discharged, Proven | **b2: FAIL** (gate Pending) | untyped `x` is not Int evidence (G1-prime) | the untyped original is asserted as the Pending limit; the same proof over `forall (x : Int)` keeps the original contract |
| ObligationsPanel status chip | `proven* (N pending obligations)`, green-tinted `modulo` class | `not proven: N pending condition(s)`, not-proven `unknown` class | D56 | navigation is unchanged: the pending rows sort first |
| Theorem score (`Theorems.get_score`) | counted the legacy `true` mark, including ProvenModulo | counts only `Proven` | D56: a proof with gaps is not proven | — |

All other original assertions in these groups are unchanged and pass in b1.

## Closed-evaluation cost (std-c2)

Measured with test-local instrumentation (`test/Test_ResetClosedEvalCost.re`) in batch b2. It uses the real injected step function, and compares the old loop (a replica) with the production loop on the same goal:

| goal | old loop | production (std-c2) | verdict |
|---|---|---|---|
| `!!(1 / 0)` | 1000 steps, 31.1 s | 1 step, 0.004 s | not true (same) |
| `1 / 0 == 1` | 1000 steps, 37.4 s | 1 step | not true (same) |
| `(fun (x : Int) -> true)(1 / 0)` | 1000 steps, 42.6 s | 1 step | not true (same; the old loop never reached `true`) |
| `int_of_string("abc") == 1` | 1000 steps, 36.6 s | 1 step | not true (same) |
| `if false then 1 / 0 == 1 else true` | 2 steps | 2 steps | true (same) |
| `!!3`, `!!(1, [2])`, `2 * 3 == 6`, `6 / 2 == 3`, `2 * 3 == 7` | 2–6 steps | same | same |

**Mechanism.** The evaluator marks an error step final only within one run. `eval_via_step` re-enters it, so each call re-steps the error hole's payload into a freshly nested error hole. The term prints the same, but it is never structurally equal to the previous one, and nesting grows.

**The change.** `eval_via_step` stops at the first result containing a `DynamicErrorHole`. Both callers act only on a literal `true` (discharge) or `false` (contradiction), so this is fail-closed. The fuel bound (1000) is unchanged, and D66 (no fixpoints) is unchanged.

## Explicit limits (no automatic proof)

- Recursive results, whether unannotated (`len`, `rev`) or ADT (`to_int`).
- Annotated recursive definitions: the `Asc` wrapper can get stuck, so it is not erased.
- Ascribed rebinding of an application result.
- Unknown-domain or untyped arguments: `forall y` used at `f : Int -> Int`, or `fun (y : ?) -> y + 1`.
- Case binders: they carry no class evidence.
