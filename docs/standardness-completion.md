# Standardness completion (FR-std-4): contracts and limits

This is the candidate base for 3f597d47. It is not accepted main. Observations come from leased batch `b1` (std-c1) and are copied in `fleet/reports/reset-2026-10-28/Reset-standardness-completion/b1/alcotest-out`.

## Policy that settles these contracts

- **D65/D66/D67:** automatic discharge stays conservative.
  - Exact fact match, without instantiation.
  - Closed evaluation, with no fixpoints.
  - The structural `!!` routine.
- **ADDENDUM-5 (G1-prime):** a typed operation needs actual operand class evidence.
  - Unknown is never Int/Bool evidence.
  - No class is inferred for the result of a recursive function (inlined fixpoint), a lambda, a builtin, `if` or `case`.
  - The one supported application result: a STANDARD context-bound function variable with a precise declared type `dom -> cod` (no Unknown), applied to an argument evidently of `dom`, has class `cod`. So `h(0) + 1` with `h : Int -> Int` is supported (final closure fa1cbafb).
- **D56:** a proof with pending conditions is **not proven**. It shows a quiet count of the pending conditions.
  - `ProvenModulo` remains only an internal carrier.

An unsupported proof therefore stays **Pending**. It is not Proven and it is not refused. The original expectations below are kept verbatim in the test sources, next to the new assertion.

## Compatibility-loss table

| Test | Original expectation | Observed now (b1) | Reason | Supported alternative / limit |
|---|---|---|---|---|
| Core 21 `structural standardness judgement` | `x + 1` with `x` marked standard but **untyped**: standard | not standard | Unknown is not Int evidence (ADDENDUM-5) | A standard `x : Int` makes `x + 1` standard (new typed control, passes) |
| Recursive `len_unannotated_list_arg` | witness `len(xs)`, `!!len(xs)`: Structural | Pending | `1 + len(t)` adds the result of an inlined RECURSIVE function, which has no class evidence | No automatic proof. A manual proof of the `!!` premise is needed (limit) |
| Recursive `rev via snoc` | every `!!` gate discharged | exactly one Pending gate, the inlined `rev(t) @ [x]`; all others Structural | `@` on the result of an inlined recursive function is not list evidence | limit, as above |
| Recursive `rebinding_inner_lambda` | Structural | Pending | `(fun (r : Int) -> r)(r(t))` ascribes the result of a recursive call, which has no evident type (rec-3) | write the inner binder unannotated, or accept the limit |
| Equivalence 1 `lenfold` | no obligations, **Proven** | 1 Pending gate (inlined `len`), ProvenModulo, so shown as not proven | as `len_unannotated` | limit |
| Equivalence 2 `lenfoldf` | no obligations, **Proven** | 1 Pending gate (inlined `len`) | same | limit |
| Equivalence 7 `lenfa` | no obligations, **Proven** | 1 Pending gate (inlined `len`) | same | limit |
| Equivalence 10 `snoc` | **Proven** | 1 Pending gate (inlined `rev`) | `rev(t) @ [x]` | limit |
| Equivalence 11 `invol` | no obligations, **Proven** | 1 Pending gate (inlined `rev`) | same | limit |
| Equivalence 15 `round` (ADT `to_int`) | exactly one obligation (the positivity guard), ProvenModulo | one ordinary obligation (the guard) plus 2 Pending `to_int` gates and 1 Structural gate `!!m`; still ProvenModulo | ADT recursion is not certified, and its result has no class evidence | limit |
| Equivalence 13 `pin: rewrite does not match through the environment` | (inherited) | **FAIL**, unchanged | fails on the 440 base as well; unrelated to standardness | not in scope |
| Operand-class `generalize_case_binder_shadow` (g1pC) | Pending | the g1pC fixture was **malformed**: static error "Variable end is not bound" | a stray third `end` | kept verbatim as a historical negative (static error). The corrected statics-clean pair: shadowing case binder gives **Pending**; non-shadowing binder gives **Structural** |
| Evaluator.Contradiction 0-4, 6 | "contradiction incurs nothing": no obligations at all | **b2: 6 of 8 FAIL** (1–2 obligations each); **b3 (std-c3): 7/8**, with case 2 classified below | each fixture's `assume P =>` records its D47/D51 `!!P` gate, which is a premise of the assume and not something the contradiction incurs | count only ORDINARY obligations, and require the contradiction node itself to record none; gates are printed |
| Evaluator.ProofMap 28 `implication intro via assume` | untyped `forall x`: `!!(x == 2)` discharged, Proven | **b2: FAIL** (gate Pending); **b3: 44/44** | untyped `x` is not Int evidence (G1-prime) | the untyped original is asserted as the Pending limit; the same proof over `forall (x : Int)` keeps the original contract |
| ObligationsPanel status chip | `proven* (N pending obligations)`, green-tinted `modulo` class | `not proven: N pending condition(s)`, not-proven `unknown` class | D56 | navigation is unchanged: the pending rows sort first |
| Theorem score (`Theorems.get_score`) | counted the legacy `true` mark, including ProvenModulo | counts only `Proven` | D56: a proof with gaps is not proven | — |

**Groups observed passing in b1 (std-c1):**
- `^ResetClosedEvalCost$` 6/6, `^ResetOperandClass$` 53/53, `^ResetRecursiveStandardness$` 20/20;
- `^ResetStandardnessGates$` 9/9, `^ResetStandardnessCorrections$` 11/11;
- `^ObligationsPanel$` 17/17, `^ExistentialsAppContext$` 9/9, `^ResetExistentials$` 25/25;
- `^Equivalence inductions$` 15/16. The only FAIL is 13, an inherited pin: "rewrite does not match through the environment", which fails on the 440 base too.

The evaluator groups below were first run in b2/b3 and are classified separately. **No clean suite is claimed.**

## Evaluator groups run in b2/b3: bounded classification (raw failures kept)

These groups were not run at the base (3f597d47), and no matched-baseline run exists. So attributing each failure to a pre-existing base change is **INFERENCE** from the assertion text and the fixture. In every case the failing check is a count or status that includes `!!` gates. None involves the closed-evaluation early stop (std-c2): ProofMap 44/44 includes the Evaluated-discharge tests, and Have 8/8 passes. The fixtures and assertions are unchanged, and the failures are left red.

| Group, case (b3 at std-c3) | Failed expectation | Observed | Inferred cause |
|---|---|---|---|
| Contradiction 2 `case_eq substitution` | Proven | ProvenModulo: `!!(isz(e) == true)` Pending | G1-prime: `isz(e)` is an inlined recursive function result over an ADT |
| ClosureLibrary 0–5 (`nonzero_of_pos/neg`, `nonzero_mul`, `pos_mul`, `pos_add`, `pow_pos` proofs) | gate-aware count: "a standardness gate is Pending" must not hold | assume gates Pending | lemma statements bind `a1`, `b1` UNTYPED (`forall a1 -> …`); G1-prime: untyped is not Int evidence |
| ClosureLibrary 9 `unverified Algebrite rewrite still launders the lemma to Proven` | Proven | ProvenModulo | same untyped-binder gates (now Pending) |
| ClosureLibrary 10 | "the gates are discharged" | a gate Pending | same |
| ClosureLibrary 13 | "standardness gates: none pending" | 10 Pending | same |
| FunContracts 7, 8, 10 | "the two antecedent obligations" = 2 | 4 | D80 (corr-1): an axiom instantiation also records `!!` of each instantiation; the count includes them |
| FunContracts 9 | "one antecedent obligation" = 1 | 2 | same |
| Definedness 0 | "exactly the guard obligation" = 1 | 2 | same D80 instantiation gate (a count that includes gates) |
| Definedness 2 | "guard + scanned denominator" = 2 | 3 | same |
| HypothesisNaming 10 | "alias incurs no obligation of its own" = 0 | 1 | count includes a `!!` gate recorded under the alias (not separately analysed) |

**Supported alternatives:**
- write the closure-library statements with typed binders (`forall (a1 : Int)`); this is the same remedy the ProofMap 28 contract shows passing;
- make the counts gate-aware. That is not done here: these fixtures are kept as observed evidence.

**Newly lost Proven examples:** Contradiction 2 and ClosureLibrary 9 (above), in addition to the table further up. None is a Matt-used showcase, to my knowledge; flagged to the integrator for the liaison.

## Early-stop (std-c2) limit

The fail-closed property is read from the source: both callers act only on a literal `true` or `false`, and a term containing an error hole is neither. That no positive is lost is shown only for the paired exercised shapes in `Test_ResetClosedEvalCost` (10 goals). Beyond those shapes it is **inference**: the nesting mechanism means the old loop could not progress past an error hole in an evaluated position.

## D57

There is no existing ✓* stated-theorem marker on this lineage, so none is added and none is claimed as preserved visually.

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
- Ascribed rebinding of a recursive call's result.
- Unknown-domain or untyped arguments: `forall y` used at `f : Int -> Int`, or `fun (y : ?) -> y + 1`.
- Case binders: they carry no class evidence.

## Browser: early preview W1

- **Preview:** `fleet/previews/Std-completion-85d61ecc`, served at `http://127.0.0.1:8210`.
  - Production source is std-c2 (239f3903); std-c3 is test/docs only.
  - The served `hazel.js` sha256 is `9832987974c3…`; the report's `servedBundleSha256` matches it.
- **Capture:** `thr_gibrk3hhzm/qa-existentials/run-2026-10-28T14-04-12-833Z`.
  - Isolated headless profile, removed afterwards.
  - 12 scenarios, 38 screenshots, 0 console errors, 5/5 source expectations matched.
  - Screenshots were viewed for the rows below.

| Flow | Observed |
|---|---|
| Witness 3, then step `3 * 3 == 9`, then `9 == 9` | **proven true**; `!!3` discharged by evaluation |
| Witness 2 | ends `false`, **incomplete** (not refuted) |
| One Undo after commit | source back to `proof ?` |
| Divergent witness `loop(0)` | refused: "may not terminate", incomplete |
| `witness 1 / 0` | settles; `0 != 0` and `!!(1 / 0)` listed pending, incomplete |
| `forall y` (untyped) with `f : Int -> Int`, witness `f(y)` | **not proven: 1 pending condition**; `!! f(y)` pending |
| assume `loop(0) == 0` (curry) | **not proven: 1 pending condition**; `!!(loop(0) == 0)` pending |

**Limits:**
- At 640 and 800 px the open ExplainThis sidebar covers the right part of the proof panel, including the chip text at 640. This is the existing layout and was not changed here; no reference side-by-side was run.
- No visual PASS is claimed. Reload flashes and native tooltips were not established.
