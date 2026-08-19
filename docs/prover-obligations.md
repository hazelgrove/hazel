# Prover design: partiality, implication, and obligations

Status: design agreed 2026-08-18; implementation phased below.
Scope: the theorem/proof system embedded in the stepper (`src/language/proof/`,
`src/language/term/Proof.re`, `src/web/app/editors/stepper/`). The teaching
deriver (`src/language/derivation/`) is unaffected.

This document records the semantic and architectural decisions for extending
the prover with hypothetical reasoning (implication), domain restrictions, and
a tracked-obligation system. The motivating workflows are (a) applied
mathematics — use assumptions like `n != 0` freely mid-proof and decide later
whether to prove them, derive them, or add them to the statement — and (b)
eventual programming-language metatheory (e.g. type soundness of a lambda
calculus encoded as a Hazel ADT).

---

## 1. Semantics

### 1.1 The domain

Goal expressions denote values in the flat domain `{false, err, ⊥, true}`:

- `err` — **domain errors**: terminating computations that hit an off-domain
  primitive application (`1/0`, `head([])`). Detectable; their absence is
  expressible as boolean conditions (`n != 0`, `xs != []`).
- `⊥` — **genuine divergence**: non-termination from general recursion. Not
  detectable; not expressible in the object language.

These are kept strictly distinct. Domain errors get dischargeable boolean
obligations; divergence gets checker-side refusal. Collapsing them would force
the (impossible) expression of termination as a boolean, or the (unsound)
treatment of divergence as a value.

**Theoremhood = the goal denotes `true`.** Not "never evaluates to `false`"
(rejected: it makes `forall x -> x/x == 1` vacuously valid at 0), and not
Lean/Isabelle-style totalization or underspecification (rejected: we do not
want `n/0` to be a number, so `0 * (1/0) == 0` must be unprovable — and it is:
it denotes `err`).

### 1.2 The refinement invariant

> The proof semantics agrees with evaluation wherever evaluation renders a
> boolean verdict. Every proof rule is validated against the proof semantics
> alone.

Corollary requirement on the dynamics: **evaluation must never render a
boolean verdict off-domain.** `1/0` produces an error/indet result, and
`err == e` propagates `err` — it never returns `true` or `false`. (Phase 0
audits current behavior against this.)

### 1.3 One logic: Kleene-idealized evaluation

The proof semantics is evaluation with exactly one idealization: the boolean
connectives (`&&`, `||`, `==>`, `!`) are interpreted as symmetric **Kleene**
connectives rather than evaluation's left-to-right McCarthy short-circuit.
Kleene refines McCarthy (agrees wherever McCarthy terminates), so the
invariant holds, and the symmetric laws — commutativity, associativity,
De Morgan, `a ==> b == !a || b` — are genuine laws. Everything else
(arithmetic, user functions) is strict, as in evaluation.

Consequences:

- **Operational guarantee** (the product's headline): a proven theorem's every
  closed instance evaluates to `true` — terminates, no error, `true`. This is
  demonstrable by running instances in the stepper.
- Quantifiers (`forall`) range over **total values**. This was already
  implicitly committed: induction's exhaustiveness via `Coverage.check` never
  covers ⊥.
- A theorem can be **refuted** (goal rewrites to literal `false`), and
  refutation genuinely means false. Proof-⊥ (neither provable nor refutable)
  covers divergent goals and statements whose truth depends on off-domain
  values.
- Lineage: LPF / VDM / PVS ("TCC" style), not Lean/ACL2 totalization. The
  design differs from historical LPF tooling in that obligations are
  first-class editor objects (§3), which is what makes the obligation traffic
  livable.

### 1.4 The consistency fence

Divergence is never assigned a value. If `f(x) = f(x) + 1` were given an
"unknown value", one unfolding yields `0 == 1`. Domain errors are safe to
gate with boolean obligations precisely because no defining equation
constrains their off-domain behavior. This asymmetry is the entire
consistency argument; eval steps (definition unfolding) remain unconditionally
sound because divergence stays ⊥.

### 1.5 Floats are total (IEEE)

Float arithmetic is deliberately total in Hazel: `1.0 /. 0.0` is the *value*
`infinity`, `0.0 /. 0.0` is `nan` — first-class IEEE values with
`is_nan`/`is_finite` builtins. Nothing is off-domain, so the refinement
invariant is not implicated and no definedness gates fire on float
arithmetic. The costs land elsewhere and are accepted:

- Float theorems mean IEEE: `nan == nan` is `false`, associativity of `+.`
  fails, etc.
- The **Algebrite gate must refuse float-typed rewrites** (Phase 3): CAS
  field laws are false for IEEE floats independent of any partiality story.
- Conversions *out* of float (`int_of_float`) **are** off-domain on
  `nan`/`inf` and must error (Phase 0 fix; the current dynamics silently
  yields an Int).

---

## 2. Language additions

### 2.1 Material implication `==>`

- New boolean binary operator. Right-associative, precedence below `||`.
- Evaluation: McCarthy left-to-right (`false ==> _` short-circuits to `true`).
- Proof interpretation: Kleene. The "ND rules" ship as **built-in equational
  axioms** (decided in Phase 2), not new step forms — `impl_def :
  (a ==> b) == (!a || b)`, commutativity/associativity/units/De Morgan for
  the connectives, `impl_true`/`impl_false` — each sound under the Kleene
  reading of §1.3 and applied through the ordinary axiom-step machinery. No
  new checker logic; the rewriting architecture is the proof calculus.
- **Implication introduction unifies with `assume`**: when the goal is
  `A ==> B` and the proof says `assume A' => body` with `A'` alpha-equal to
  `A`, the checker strips the antecedent (body's incoming goal is `B`) and
  incurs **no obligation** — that is intro, sound unconditionally. Otherwise
  `assume` incurs its obligation as usual. One form, two readings; intro is
  the degenerate case of assume-then-bake.
- `ProofRule.exp_to_rule` peels `==>` after `forall`s into a resurrected
  `assumptions` field (the commented-out hook at `ProofRule.re:11` and
  `:28-35`), making conditional rewrite rules first-class.

### 2.2 Restricted binders (predicate-subtyping style)

- `forall n where n != 0 -> ...` — sugar for `forall n -> n != 0 ==> ...`,
  rendered on the binder (syntax decided 2026-08-18: `where` keyword, which
  avoids ambiguity with induction-case `|` separators). Restrictions are
  ordinary boolean expressions; there is **no separate restriction language**
  (restrictions are instantiated with arbitrary expressions, so a sublanguage
  cannot stay small; see §4.3 for how predictability is achieved instead).
- **Function contracts**: restrictions on `fun` binders,
  `fun x where x != 0 -> ... 1/x ...`. Two effects:
  - *Definition-time discharge*: defining `f` emits one obligation — the body
    is defined given the restriction — proven once.
  - *Caller-vocabulary obligations*: a use of `f(e)` emits the contract
    `e != 0`, never conditions about `f`'s internals. This keeps obligations
    at the caller's altitude and is the main ergonomic lever of the design.
- A theorem with a restricted binder *is* a conditional rule; its uses emit
  the restriction as an obligation through the ordinary mechanism.

---

## 3. Obligations

### 3.1 Representation

- An obligation is a **sequent**: (local context, boolean goal). The local
  context matters because obligations raised inside induction cases mention
  case-local binders.
- **Single source of truth is program text.** A locally-proven obligation is
  an inline subproof at the step; a floated restriction is binder syntax.
  The (!) indicator is *computed* by the checker, never stored.
- `ProofMap` step entries gain a provenance field: which in-scope fact (by
  binder/step id) discharged each incurred condition, or `Pending`.
- `status_of_proof` gains a third outcome, `ProvenModulo(obligations)`,
  distinct from proven / refuted / error.

### 3.2 The (!) states

| state | meaning | rendering |
|---|---|---|
| silent | discharged locally (inline proof) or trivially (lookup/eval) | nothing; receipt on hover |
| linked | discharged by a remote fact (binder restriction, `case_eq`, hypothesis) | (!) linking to the fact; fan-in count on the fact |
| pending | nothing in scope covers it | mark + menu of code-editing shortcuts |

Deleting a restriction flips its dependents to pending on recheck — the
reverse-dependency diff is free.

### 3.3 Float scoping

The float target is computed, not chosen: the innermost binder introducing
any free variable of the obligation (co-context machinery answers this).

| binder kind | prove here | float as restriction | split true/false |
|---|---|---|---|
| theorem `forall` | yes | yes (rewrites the public statement; triggers recheck) | yes |
| induction case | yes | **no — unsound** (a restricted case breaks exhaustiveness) | yes |

Obligations dedup at the float target. Accepting a restriction changes the
theorem's statement (loudly) and its IH on recheck; pass-through marks make
breakage visible rather than silent.

"Split" is bool case analysis via the existing induction-on-any-expression
mechanism; the branch's `case_eq` (`P == true`) then discharges the
obligation in that branch through ordinary lookup. Within a branch `case_eq`
is a genuine symmetric equation (the split gate has already required the
scrutinee defined and terminating).

---

## 4. Gates and discharge

### 4.1 The gate table

| step | checks | emits |
|---|---|---|
| axiom/lemma instantiation | divergence (structural) + domain scan of matched terms | domain obligations (`d != 0`, `xs != []`, contract instances) |
| conditional rule application | binder restriction | the restriction, instantiated by the match |
| Algebrite rewrite | verified in the checker, not only the UI + domain scan of both sides | domain obligations per denominator (and even roots, `log`, `0^0`, …) |
| bool split | divergence of scrutinee (refuse) + domain scan | domain obligations |
| eval step | nothing | nothing — always free |
| induction | nothing (scrutinee is a quantified total) | nothing |

Divergence checking is two-tier, checker-side (never a boolean obligation —
termination is not expressible in-language):

1. **Structural totality**: the expression's call graph avoids
   general-recursive functions. Silent; passes literals, constructors,
   arithmetic including division, and non-recursive functions. Covers nearly
   all applied-math instantiations.
2. **Structural-recursion detection**: recognizes functions that recurse on
   strict subterms (e.g. metatheory decision procedures `infer`, `step`).
   Required for the metatheory milestone, where such calls appear in every
   interesting instantiation. When neither tier applies, the gate refuses
   honestly ("cannot establish this terminates").

The domain scan is conservative in v1 (all denominators etc. in the matched
term, path-insensitive). Over-strong conditions are repaired manually by
splitting on the guard and eval-stepping the conditional away. A
weakest-precondition pass is a later, purely-ergonomic upgrade — conservative
is always sound.

Underdetermined instantiation (a conditional rule whose restriction mentions
metavariables not fixed by matching the conclusion) is detected via
`MatchExp`'s match context and **refused** in v1; explicit user
instantiation is a later feature.

### 4.2 Discharge channels (in order)

1. **Binder lookup** — dumb syntactic match (alpha-equivalence + the normal
   form of §4.3) against facts visible from the obligation site: binder
   restrictions, `case_eq`, hypotheses. Transparent: "why discharged?" =
   "it's written there".
2. **Closed evaluation** — ground obligations (`2 != 0`) just run. The only
   built-in decision procedure, and replayable in the stepper by
   construction.
3. **Proposed closure lemmas** — when lookup misses, the (!) menu suggests
   matching lemmas from a curated library (`a != 0, b != 0 |- a*b != 0`,
   sign facts, …). **Automation may propose; only visible steps dispose.**
   Library placement (decided): built-in OCaml-side axioms in Phase 3 (like
   `refl_eq`), migrated to a self-hosted proven Hazel prelude once Phase 4
   makes them provable — trusted base grows temporarily, then shrinks.

   *Self-hosting status (2026-08-19, `test/evaluator/Test_ClosureLibrary.re`)*:
   the migration is **blocked, and not by a Phase-4 gap** — 0 of 6 are
   provable in-language today. The proof calculus is rewriting (§2.1), so
   relating two predicates needs an equation mentioning both; no built-in
   equation mentions two different arithmetic comparisons (the Kleene set is
   purely boolean, `refl_eq` is reflexivity), and evaluation is stuck on a
   symbolic comparison. What is missing is **Phase 5**'s ordered-arithmetic
   polarity engine, whose monotonicity lemmas are exactly these facts. Per
   lemma, all six pinned as mark-free partial proofs (antecedents intro'd,
   bool split on the goal predicate, `true` branch closed by `refl_eq`,
   `false` branch open):
   - `nonzero_of_pos` — still trusted: needs `a > 0` ⟹ `a != 0`, i.e. an
     equation relating `>` to `!=`. None exists but the axiom itself.
   - `nonzero_of_neg` — still trusted: same wall at `<` vs `!=`.
   - `nonzero_mul` — still trusted: needs zero-divisor reasoning on a
     symbolic product; no equation relates `a != 0`/`b != 0` to `a*b != 0`.
   - `pos_mul` — still trusted: sign monotonicity of `*` (Phase 5's
     sign-condition matrix, open item 7).
   - `pos_add` — still trusted: monotonicity of `+` (Phase 5).
   - `pow_pos` — still trusted, doubly: monotonicity as above, **plus** an
     inductive exponent. `induction` on an Int or Nat binder is accepted but
     offers only a literal and a catch-all pattern — no successor, hence no
     `ih` at all, so Phase 4b's `generalize` has no induction principle to
     quantify. Switching the exponent to Nat does not help (Nat is an atom
     type here, and `**` parses to the Int operator class anyway, so a
     Nat-typed variant is a different lemma).
   Not purely negative: each lemma's `true` branch is genuinely closed
   in-language; `nonzero_of_pos`'s `a == 0` sub-case closes outright by the
   Phase-4c ex-falso idiom (splitting the symbolic `a` yields the bare
   equation `a == 0` as its `case_eq`, which rewrites `a` to `0` in the
   reverted antecedent so evaluation can falsify it) — the wall is
   specifically SYMBOLIC operands; and all 12 closed instances checked are
   fully proven through channel 2, spot-checking §1.3's operational
   guarantee. Two hazards pinned in the same file: an **Algebrite rewrite
   still launders any of these lemmas to `Proven`**, because the checker
   does not yet re-verify a step's equational content (§4.1's CAS TODO; the
   Float gate is the only content gate that fires, and a Bool-typed rewrite
   sails past it) — so Algebrite cannot self-host the library without
   growing the unchecked base; and a `Parens` node around a nested
   antecedent (`A ==> (B ==> C)`, the shape `Axioms.re`'s `==>>` reads as)
   defeats assume-intro, which matches `BinOp(Implies)` on the bare term, so
   the inner `assume` falls through to assume-then-bake and goes pending.
   Channel distribution on this workload (§4.3): 13 obligations, 1 channel 1
   (`pow_pos`'s `**` domain condition, discharged against the intro'd
   guard), 12 channel 2, 0 pending — 100%, which reads as a caution about
   the metric: it measures obligation ergonomics, not proof completeness,
   since the automation debt here surfaces as six open branches that incur
   no obligations at all.

4. **Manual** — split/eval repair, or a real subproof.

Binder fact-sets are extendable by **explicit one-time derivations**: a
binder carrying `n > 0` can also carry the derived fact `n != 0`, attached by
one visible closure-lemma application and rendered as a child of the
restriction. All subsequent discharge remains pure lookup.

**Receipts everywhere**: every silent discharge and every pending failure is
inspectable — "discharged: `n != 0` by restriction on `forall n`",
"pending: no matching fact; nearest miss `n > 0` on `forall n` — bridge with
`nonzero_of_pos`?".

### 4.3 Human-first boundary

The system must never grow opaque solver dispatch. The load-bearing wall is
the **discharge relation**: it stays a syntactic lookup. If matching ever
gains slack, it does so only via a small, *named, documented normal form*
(e.g. comparison symmetry + constant folding, nothing else) applied to binder
fact-sets — a graded guarantee over one proposition language, not an
admissibility wall between two. Entailment gaps are closed at the binder by
explicit derivation (above), never by smartening the matcher.

Ergonomics is empirical: measure, on real worked examples, the fraction of
obligations dying in channels 1+2 (target: ~90%), channel 3, and channel 4.
Channel-4 residue is the enumerable "automation debt" and directs closure-
library growth.

---

## 5. Interactions with existing machinery

- **Eval steps** are unconditionally sound (denotation-preserving) — the one
  free step, unchanged.
- **Induction** (`ProofCheck.check_induction`): restricted theorem statements
  must thread restrictions into generated IHs; a guarded IH is then a
  conditional rule whose use emits obligations through the ordinary path.
  (The separate, still-open question of IH *generalization* over unpeeled
  binders is Phase 4.)
- **Algebrite** moves from UI-trusted to checker-verified, and its rewrites
  emit domain obligations (§4.1). The CAS reasons in a field; the obligations
  are what make that sound here.
- **Directed stepping** (`==>`-stepping, chained inequalities with
  co/contravariance) is Phase 5: a polarity/variance engine over the rewrite
  path, builtin variance table first, monotonicity lemmas as conditional
  rules (whose side conditions are — obligations). Independent of the
  definedness machinery.

Deliberately deferred: `exists` (encode with `Option` + `is_some`), inductive
relation definitions with inversion (encode judgments as decision
procedures), any tactic language.

---

## 6. Phases

Each phase is a separate jj commit (or small stack); later phases depend only
on earlier ones.

- **Phase 0 — semantics groundwork.** This document. Audit the dynamics for
  the refinement invariant: off-domain primitive applications (int div/mod by
  zero, list/string partial ops, failed matches) must yield error/indet,
  never a boolean — including under `==`. Fix violations.

  *Audit results (2026-08-18)*: the int/Nat/SInt/list/match/hole/cast surface
  already satisfies the invariant — `1/0` becomes
  `DynamicErrorHole(_, DivideByZero)`, and both `DHExp.ty_comparable` and
  `DHExp.poly_equal` independently refuse boolean verdicts on error/indet
  operands (`1/0 == 1/0` steps to indet, not `true`). Float arithmetic is
  IEEE-total by design (§1.5), not a violation. Fixes required:
  1. `Atom.re` float→Int/SInt conversions unguarded on `nan`/`inf` (silently
     yields an Int) — add a `NonFiniteFloat`-style error.
  2. `BuiltinsUtil.re` drops all `Atom.convert` errors (`| R(_) => None`), so
     `int_of_string("abc")` is an anonymous stuck term instead of
     `DynamicErrorHole(_, InvalidOfString)` — thread the error through, so
     "off-domain" has one shape for the Phase 3 domain scan.
  3. `string_search` silently totalizes an out-of-range index to `-1` —
     should be `IndexOutOfBounds` (contrast `string_sub`, which is correct).
  4. `DHExp.poly_equal` on `DrvQuote` with `~skip_hole=false` renders
     `Some(false)` for hole-containing derivations — the one indet-ish
     payload that yields a verdict; return `None` when either side has holes.
  5. Hygiene: `is_value: true` on `dynamic_error_hole` branches in
     `Transition.re`; uncaught JS exception from malformed regexps in
     `StringUtil.re`; (`StepperBase.status_of_entry` missing the
     `proof_is_clean` conjunct — deferred, that file is under active
     parallel work.)
- **Phase 1 — obligation substrate.** Sequent obligations; `ProofMap`
  provenance; (!) three states + receipts in the stepper UI;
  `ProvenModulo` status; inline prove-here proof syntax. No new logic yet —
  independently useful with a bare `assume`.
- **Phase 2 — implication and restrictions.** `==>` operator (forms, statics,
  dynamics, ND rule set); `ProofRule.assumptions` resurrected → conditional
  rules; apply-with-obligation elimination; restricted `forall` sugar; float
  UX (restrict/prove/split menu, scoping rules of §3.3); discharge channels
  1–2. *Applied-math workflow substantially served.*
- **Phase 3 — definedness gates.** Structural-totality check; conservative
  domain scan at the gates of §4.1; function contracts with definition-time
  discharge; Algebrite checker-verification + domain obligations; closure-
  lemma library + (!) suggestions. *Measure the channel-distribution metric
  here.*

  *3a landed (2026-08-18)*: `Totality.re` (structural totality, tier 1 —
  any reachable `FixF` refuses; tier-2 structural-recursion detection stays
  Phase 4) and `DomainConditions.re` (path-insensitive domain scan), wired
  into `ProofCheck`: instantiation gate on axiom steps
  (`PossiblyDivergentInstantiation` + scan obligations; quantified-variable
  / literal / total-op instantiations emit nothing), split gate on computed
  induction scrutinees (`PossiblyDivergentScrutinee` + scan; bare-variable
  induction emits nothing), Algebrite domain obligations on both sides plus
  refusal of Float-typed rewrites (`FloatAlgebrite`, §1.5); eval steps stay
  gate-free. v1 scan coverage: Int/SInt/Nat `/` → `b != 0`
  (Float `/.` emits nothing, §1.5); `*_mod` builtins incl. float →
  `b != 0`; Int/SInt `**` → `e >= 0`; `{int,sint,nat}_of_float` →
  `is_finite(arg)`. Skipped in v1: `int_of_string`-family (no boolean
  parseability predicate), `string_sub`/`string_search` index bounds,
  `nat_of_float` negativity, `Int→SInt/Nat` conversion errors. Still TODO:
  checker-side CAS re-verification of Algebrite rewrites (the CAS lives in
  the browser as `window.Algebrite`; the equational content remains
  UI-trusted), function contracts, closure-lemma library.

  *3b landed (2026-08-18)*: function contracts + closure-lemma library.
  `fun p where g -> e` (`FunWhere`, mirroring the ForallWhere plumbing
  end-to-end: forms/parse/print round-trip, statics — `g` analyzes Bool
  with `p` in scope, otherwise types like `Fun` — substitution, alpha-
  equality, matching). The guard has **no dynamic effect** in v1: a
  FunWhere evaluates exactly like Fun (runtime contract enforcement would
  change program behavior; separate future feature). The two §2.2 effects:
  (a) *caller vocabulary* — `DomainConditions.scan` no longer descends
  into `Fun`/`FunWhere` bodies; an application of a FunWhere emits the
  instantiated contract `g[p := arg]` instead; (b) *definition-time
  discharge* — at each theorem, `ProofCheck.definition_obligations` walks
  the env's non-builtin, non-recursive function definitions once, scans
  each BODY (parameter in scope), discharges conditions that `fast_equal`-
  match the guard or one of its `&&`-conjuncts, and records leftovers as
  obligations keyed by the *function's own id* in the ProofMap (minimal
  entry: incoming/outgoing None; origin = the function id; idempotent
  across theorems). So `fun x where x != 0 -> 1/x` leaves zero residue and
  an unguarded `fun x -> 1/x` yields `x != 0` once, at the definition —
  never per call. Closure lemmas (§4.2 channel 3, built-in OCaml-side in
  `Axioms.re` as decided): `nonzero_mul`, `nonzero_of_pos`,
  `nonzero_of_neg`, `pos_mul`, `pos_add`, `pow_pos` — guarded
  equality-form (`… ==> (P == true)`) conditional rewrite rules over Int;
  applying one incurs its antecedents through the ordinary Phase-2
  machinery (obligations about obligations, same channels). The planned
  `nonneg_pow` was rejected as invalid for negative bases ((-2)**3 = -8);
  `pow_pos` (positive base, `**` errors only on negative exponents) ships
  instead. v1 limitations: contract instantiation handles simple variable
  parameters only (destructuring patterns skip at call sites, still
  discharged at the definition); applied *inline* unguarded lambdas emit
  nothing; recursive (FixF) definition bodies are not scanned (tier-2
  totality is Phase 4); definition obligations don't yet affect
  Proven/ProvenModulo status (they live outside proof subtrees) and the
  (!)-menu proposal UI for channel 3 is still to come; closure lemmas
  cover the Int operator class only (SInt/Nat variants are mechanical
  later additions).
- **Phase 4 — induction power.** Bool splits as first-class UX; IH
  generalization via an **explicit `generalize` step** (decided 2026-08-18: a
  proof form that re-quantifies an already-peeled variable before induction,
  rather than implicit unpeeled-prefix IHs — more syntax, but recoverable
  mid-proof and visible in the proof text); structural-recursion detection.
  *Unlocks the lambda-calculus soundness milestone.*

  *4a landed (2026-08-18)*: structural-recursion detection (tier 2 of
  §4.1) — `StructuralRecursion.re`, wired into `Totality.re` where tier 1
  used to refuse every reachable `FixF`. The detector accepts a fix when
  every recursive call passes, in ONE fixed argument position (the same
  across all call sites — per-call valid-position sets are intersected), a
  variable that is a strict subterm of that position's parameter. Strict
  subterms arise syntactically: case/destructuring-let patterns on a
  parameter (or on a variable already strict in it — tracking is
  transitive, and nested patterns like `Ap(Lam(b), a)` count both layers),
  cons/list patterns, constructor patterns directly in a `fun` binder;
  tuple parameters are tracked per component, so `f((b, n))` passes when
  the decreasing component is strict (components of a non-strict tuple
  alias never become strict — reconstruction can't sneak through).
  Accepted fixes are then walked by Totality with the self-name assumed
  total (sound by well-founded induction on the subterm order), so the
  rest of the body is still tier-1-checked; refusals now say "recursive
  and not visibly structural (…)" to distinguish tier-2 from tier-1
  failures. Conservative rejections: self-name escaping non-call position,
  re-constructed or arithmetic arguments (`f(n - 1)` still refuses),
  inconsistent decreasing positions, shadowed subterm variables, any
  unanalyzed form. Limitations: **mutual recursion** (tuple-bound fix or
  two fixes calling each other) is refused outright; no
  lexicographic/multi-position measures; nested fixes must each pass the
  check independently. Unlocks Term-ADT decision procedures (`size`,
  `infer`-shaped) and the `snoc`/`rev` list shapes end-to-end through the
  instantiation gate.

  *4b landed (2026-08-19)*: the `generalize` proof step.
  `generalize x => <proof>` is a prefix proof form (mirroring `assume`'s
  wiring, with an `[Exp]` child) whose argument must be a bare in-scope
  variable — a previously peeled binder. Semantics: with incoming goal
  `G`, the body's incoming goal is `forall x -> G`; the node's outgoing
  is `true` ONLY when the body proves the re-quantified goal to literal
  `true` (sound: `forall x -> G` denoting true entails `G` at the
  ambient `x`). Anything else marks (`MalformedGeneralize` for a
  non-variable/out-of-scope argument) and passes the goal through.
  **Restriction travel**: a `where`-restricted binder re-quantifies as
  `forall x where g -> G` — the restriction is recovered as the
  hypotheses installed under the "where" base name whose fact mentions
  `x` free (conjoined with `&&` if several), so after re-peeling, the
  guard is a hypothesis again and discharges through channel 1.
  Re-attaching any ambient hypothesis as a guard is sound regardless of
  attribution (it weakens the generalized statement; its ambient
  instance is discharged by that same hypothesis), so approximate
  recovery cannot compromise soundness. **Capture**: inside the body,
  every fact whose statement mentions `x` free (assume-hypotheses,
  `case_eq`, IHs, the where guard itself) is REMOVED from the body's
  semantic ctx — both the `ProofOf` ctx entries behind channel-1
  `lookup_fact` and the `ProofObject` env entries behind rule lookup —
  rather than relying on the env-shadowing `is_captured` machinery
  (which covers only rule lookup, and only after the body re-peels).
  The mention test is free occurrence via `ProofRule.get_coctx`, so
  global lemmas binding the same name (`forall x -> ...`) stay
  available. **The payoff**: `generalize t => induction e` runs
  IH generation on the quantified goal, so IHs come out
  forall-quantified and are citable at other instantiations through the
  ordinary axiom-step machinery (`exp_to_rule` peels the `forall`);
  `check_induction` needed no changes for forall goals — the goal is
  substituted wholesale. One adjacent fix: generated IHs are now
  env-substituted before installation (like `assume`/`case_eq`
  hypotheses already were) — rule exps are matched against
  env-substituted targets, so a raw `Var f` fact could never match its
  inlined value; the quantified-IH path is the first to exercise IH
  citation end-to-end. Known limitations: statics checks the generalize
  body in the unchanged ctx (`x` re-binds at the same name/type, and
  captured hypotheses remain statically resolvable — the checker
  refuses their use dynamically); multiple recovered restrictions
  conjoin into one `&&` guard, which channel-1 lookup matches only
  whole; `AlgebriteStep` still locates its target with the
  env-unaware `nth_exp`, so written rewrite patterns fail to match goal
  regions where axiom splices inlined definition closures (pre-existing,
  observed while testing — the axiom-step path is env-aware and
  unaffected).

  *4c landed (2026-08-19)*: the fixes the Phase-4 milestone
  (`test/evaluator/Test_Milestone_STLC.re`, an STLC progress attempt)
  identified as blockers.

  1. **IH generation for recursive ADTs.**
     `ProofHacks.get_inductive_hypotheses` kept a sub-pattern only when
     its statics type `Typ.fast_equal`ed the scrutinee's type — but a
     recursive ADT's constructor payloads carry the unrolled `Rec` form
     while the scrutinee carries the alias, so the comparison ALWAYS
     failed and ADT inductions generated **zero** inductive hypotheses
     (list inductions, whose element types compare nominally, were
     unaffected — which is why Phase 4b's tests passed). Both sides are
     now `Typ.normalize`d against the induction's type context — the same
     normalization `check_induction` already hands `Coverage.check` — so
     the comparison is modulo alias unrolling.
     `get_inductive_hypotheses` takes a `~tyctx` argument for this.
  2. **The `revert` proof step**, the symmetric partner of assume-intro:
     `revert <fact> => <proof>`. With incoming goal `G` and an in-scope
     fact `F` whose statement `Exp.fast_equal`s the env-substituted
     argument, the body's incoming goal is `F ==> G`; no matching fact
     marks `UnknownFactReverted` and passes the goal through. **Soundness
     and completeness in one line**: `F` holds in this scope, so under
     the Kleene reading of §1.3 `(F ==> G)` denotes exactly what `G`
     denotes — hence *no obligation*. The step only MOVES a fact from the
     context into the goal, which is where all the eval/rewrite machinery
     lives. The fact is NOT removed from scope (reverting is not
     consuming), and the node's outgoing is `true` only when the body
     proves `F ==> G` to literal `true` (a partial outgoing must not
     leak: `F ==> G` is not a sub-expression of `G` — cf.
     `forall`/`generalize`).
  3. **Bare-boolean facts as rewrite rules.** A cited fact holds, i.e.
     its conclusion denotes `true`, so a conclusion `ProofRule.classify`
     calls `Other` (a disjunction, a decision-procedure application, a
     `where` guard) additionally admits the reading `F == true`
     (`ProofRule.with_bool_fact_reading`, applied at the citation site in
     `ProofCheck`'s axiom step — deliberately NOT in
     `classify`/`exp_to_rule`, so goal classification and the co-context
     machinery keep seeing the proposition as written). Occurrences of
     `F` in the goal now rewrite to `true`.

  Together these unlock the **ex-falso idiom** with no absurdity rule:
  `revert` the contradictory fact, rewrite it in the goal with the other
  facts in scope (typically a `case_eq`) until the antecedent evaluates
  to `false`, and McCarthy `==>` returns `true`. The milestone's
  canonical-forms theorem in `where`-restricted phrasing is now fully
  proven this way, and two of `progress`'s four open leaves close.

  *Still open after 4c* (documented in place in the milestone file):
  - **Quantified-IH instantiation.** The other two `progress` leaves are
    vacuous via `IH(x0)` at a type produced by a split. The IH now
    exists and has a rewrite reading, but it is `forall t0 -> A(t0) ==>
    D` with `t0` absent from `D`, so matching the conclusion leaves the
    binder unresolved while the antecedent mentions it —
    `UnderdeterminedInstantiation`, refused in v1 (§4.1). `revert`
    cannot substitute for it: the leaf goal is `false`, so `IH ==> false`
    is genuinely false, and cashing the contradiction would mean
    refuting a `forall` inside the goal by exhibiting a witness. The
    missing feature is **explicit user instantiation** of a quantified
    fact (§4.1's deferred item), not another reading.
  - The `FixUnwrap` self-unrolled matching asymmetry (an occurrence of
    the recursing function created by the hidden self-substitution is
    not addressable by a written `at_exp`, while env-inlined occurrences
    of OTHER definitions are). This blocks the three step-unfolding
    lemma proofs; the fix is an equality/substitution change, not
    trivially safe. Pinned by `test_pin_self_unrolled_unaddressable`.
  - Annotated-`let` matching; prime-counted hypothesis names as a UX
    problem (citing `case_eq''''''` by counting primes); obligation
    display inlining.
  - Assume-intro's antecedent test re-substitutes an already-substituted
    goal antecedent, so introducing a *reverted* antecedent by `assume`
    fails to `fast_equal`. Worked around in the milestone by reverting in
    the leaf that needs it rather than above the split.
- **Phase 5 — directed stepping.** Polarity/variance engine covering
  **boolean polarity and ordered arithmetic in the same cut** (decided
  2026-08-18): `!`/`&&`/`||`/`==>` polarity plus `<=`/`<` chains with
  monotonicity of `+`, `-`, and sign-conditional `*` — whose sign side
  conditions are ordinary obligations. *Applied-math workflow completed.*

### Open items (tracked, non-blocking for Phases 0–2)

1. Enumerate the concrete ND/Kleene rule set for `==>` and connectives
   (Phase 2; soundness criterion fixed by §1.3).
2. Define the matching normal form (§4.3) when/if slack is first needed.
3. Underdetermined-instantiation UX beyond refusal.
4. WP pass for path-sensitive definedness.
5. Surface syntax for inline obligation proofs and the apply step (binder
   syntax decided: `where`).
6. Exact `generalize` step semantics vs. the `case_eq`/IH machinery
   (Phase 4 detail, direction decided).
7. Sign-condition matrix for `*` monotonicity (Phase 5 detail, scope
   decided).
