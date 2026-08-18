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

---

## 2. Language additions

### 2.1 Material implication `==>`

- New boolean binary operator. Right-associative, precedence below `||`.
- Evaluation: McCarthy left-to-right (`false ==> _` short-circuits to `true`).
- Proof interpretation: Kleene. Natural-deduction rules (enumerated in
  Phase 2) let goals be proven without evaluating either side.
- `ProofRule.exp_to_rule` peels `==>` after `forall`s into a resurrected
  `assumptions` field (the commented-out hook at `ProofRule.re:11` and
  `:28-35`), making conditional rewrite rules first-class.

### 2.2 Restricted binders (predicate-subtyping style)

- `forall n | n != 0 -> ...` — sugar for `forall n -> n != 0 ==> ...`,
  rendered on the binder. Restrictions are ordinary boolean expressions;
  there is **no separate restriction language** (restrictions are
  instantiated with arbitrary expressions, so a sublanguage cannot stay
  small; see §4.3 for how predictability is achieved instead).
- **Function contracts**: restrictions on `fun` binders,
  `fun x | x != 0 -> ... 1/x ...`. Two effects:
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
- **Phase 4 — induction power.** Bool splits as first-class UX; IH
  generalization (design round needed: keep unpeeled `forall` prefix vs.
  explicit `generalize` step); structural-recursion detection. *Unlocks the
  lambda-calculus soundness milestone.*
- **Phase 5 — directed stepping.** Polarity/variance engine; `==>`-directed
  rewriting; inequality chains with monotonicity obligations. *Applied-math
  workflow completed.*

### Open items (tracked, non-blocking for Phases 0–2)

1. Enumerate the concrete ND/Kleene rule set for `==>` and connectives
   (Phase 2; soundness criterion fixed by §1.3).
2. Define the matching normal form (§4.3) when/if slack is first needed.
3. Underdetermined-instantiation UX beyond refusal.
4. WP pass for path-sensitive definedness.
5. Surface syntax bikeshed: inline obligation proofs, apply step,
   restricted-binder concrete syntax.
6. IH generalization mechanics (Phase 4 design round).
7. Variance engine shape (Phase 5 design round).
