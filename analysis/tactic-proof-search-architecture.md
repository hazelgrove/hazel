# Tactic Specification, Proof Export, and Rewrite Search

Last updated: June 24, 2026

This note summarizes the architecture we are building for Hazel math steps:
rewrite rules are specified as small, named operations; proof search composes
those operations into candidate steps; proof export replays the recorded
operations in a theorem prover.

## Goal

Hazel should not accept a math step because a black-box CAS simplified both
sides. It should accept a step because Hazel can explain the step as a bounded
sequence of named mathematical operations, and each operation should have a
corresponding theorem-prover tactic or lemma.

The target shape is:

1. A user writes a step, for example `sin(x+y)` to
   `sin(x)*cos(y) + sin(y)*cos(x)`.
2. Hazel finds a small proof trace, for example `trig.sin_sum`, plus any
   needed arithmetic reordering.
3. Hazel stores the trace as structured proof breadcrumbs.
4. Coq/Rocq or Lean export replays those breadcrumbs as cuts and rewrites.

## Tactic Specification

The specification lives in `src/language/proof/Axioms.re`.

The important records are:

```reason
type rewrite_level =
  | Arithmetic
  | Algebra
  | Trigonometry
  | FunctionsAndLists
  | Calculus;

type rewrite_rule = {
  id: string,
  label: string,
  prover_hints: list(prover_hint),
};

type rewrite_group = {
  name: string,
  label: string,
  level: rewrite_level,
  rank: int,
  rules: list(rewrite_rule),
};
```

A rewrite group is the unit of mathematical capability. Arithmetic includes
constant folding, additive/multiplicative commutativity, associativity, and
coarse term reordering. Algebra adds distribution, factoring, polynomial
expansion, collection, and cancellation. Trigonometry adds identities such as
Pythagorean, sum/difference, double-angle, half-angle, and reflection rules.

Levels are cumulative. If the active level is Trigonometry, search may use
Arithmetic, Algebra, and Trigonometry rules. This is how a trig identity can be
followed by arithmetic reordering without manually switching modes.

`prover_hints` are currently mostly Lean-shaped hints. Coq/Rocq export has its
own mapping in `CoqProofExport.re`. Long term, the rule record should become
the central source for both Lean and Coq/Rocq proof mappings.

## Primitive And Macro Rules

Not every Hazel step should pretend to be one primitive theorem-prover rewrite.
The rule catalog should distinguish small primitive rewrites from larger macro
steps that have a planned lowering.

Proposed shape:

```reason
type proof_kind =
  | Primitive({
      prover_hints: list(prover_hint),
    })
  | Macro({
      basis_rules: list(string),
      prover_hints: list(prover_hint),
      expansion_status: expansion_status,
    })
  | NonExportable(string);

type expansion_status =
  | FullyExpanded
  | CoarseTrustedTactic
  | PlannedOnly;
```

Primitive rules map directly to one prover rewrite or tactic. For example,
`trig.pythagorean_sin_cos` should carry both the Coq mapping
`rewrite sin2_cos2` and the Lean mapping `rw [Real.sin_sq_add_cos_sq]`.

Macro rules cover larger ergonomic steps such as AC reordering, FOIL,
polynomial collection, and auto-simplification. A macro rule should name its
basis rules, record whether it is currently expanded or coarse, and include
enough per-step trace data to replay or later expand it:

- macro rule ID
- source and target expressions
- occurrence or redex path
- basis rule IDs
- primitive substeps when available
- backend tactic only when the macro is explicitly marked as coarse

This lets the UI offer larger mathematical steps without losing the theorem
prover contract: every accepted step is either primitive, a macro with an
explicit lowering plan, or visibly non-exportable.

## Proof Search

The bounded search lives in `src/web/app/editors/stepper/AxiomSearch.re`.

Search inputs:

- source expression
- target expression
- active rewrite level
- optional allowed rule IDs
- maximum depth
- maximum number of states

Search refuses to run when:

- either side has a hole
- the active math level does not support a construct in the source or target
- the search exceeds its bounded depth or state budget

The search has two paths:

1. Targeted AC-style reordering for small sums/products.
2. Bounded BFS over allowed rewrite rules.

The targeted path exists because raw associativity/commutativity search is the
wrong shape. For examples like `1+2+3+4 -> 4+3+2+1` or
`2*sin(x)*cos(x) -> 2*cos(x)*sin(x)`, we should treat reordering as a
controlled normalization primitive, not as a deep sequence of arbitrary
commute/associate moves.

Current targeted rules:

- `arith.reorder_add_terms`
- `arith.reorder_mul_factors`

These are intentionally coarse for now. The exported proof currently replays
them with a small prover tactic. The long-term version should expand each
coarse reorder into local assoc/comm breadcrumbs when needed.

The BFS path applies every allowed rule at every expression occurrence. Each
application records:

- the rule
- the local before/after expression
- the whole-expression before/after expression
- the occurrence number

That application becomes a `RewriteChecker.prover_step`.

## Proof Breadcrumbs

The central proof breadcrumb is `RewriteChecker.prover_step`.

Conceptually it records:

- origin: manual rewrite, normalization, or auto evaluation
- rule ID
- local before/after expression
- full before/after expression
- occurrence number
- optional detail text

This is the key architectural point: search result acceptance and proof export
use the same trace. The UI can show a broad step, but the stored trace should
remain small enough for theorem-prover replay.

The current system already records prover steps for bounded axiom search and
some trig/manual rewrite paths. The remaining work is to make normalization and
auto evaluation emit similarly fine-grained steps instead of only a final
normal form.

## Proof Export

The main export code is split across:

- `src/web/app/editors/stepper/CoqExport.re`
- `src/web/app/editors/stepper/CoqProofExport.re`

`CoqExport.re` handles expression printing and domain choice. It currently
supports integer arithmetic and real-valued trig export. It also treats `pi`
as a mathematical constant in the real domain.

`CoqProofExport.re` handles proof script generation. The current Coq/Rocq
export strategy is:

1. Pick a domain:
   - integer domain for arithmetic/algebra without trig
   - real domain for trig
2. Emit a small prelude of rewrite tactics.
3. For each recorded Hazel prover step, generate an assertion/cut:
   `H_hazel_step_n : before = after`.
4. Prove that local assertion with the tactic mapped from the rule ID.
5. Rewrite the main goal using those assertions.
6. Finish with `reflexivity` when possible.

For example, a trig sum followed by multiplication reordering should export as
two cuts:

1. `sin(x+y) = sin x * cos y + cos x * sin y`
2. `sin x * cos y + cos x * sin y =
   sin x * cos y + sin y * cos x`

The first cut uses a trig identity such as `sin_plus`. The second uses a small
multiplication-reordering tactic. This is better than sending a large
unexplained simplification to the prover.

## Current Workarounds

Some export paths are still pragmatic rather than final:

- Symbolic affine arithmetic can fall back to `lia` while we finish emitting
  finer normalization breadcrumbs.
- Coarse reordering rules such as `arith.reorder_mul_factors` are replayed by
  a small prover tactic rather than by an expanded local assoc/comm trace.
- Coq/Rocq export has explicit rule-to-tactic mappings separate from
  `Axioms.re`.
- JSCoq is integrated as a browser-side checker, but local `coqc` is still the
  more reliable validation path for generated files.

These are acceptable interim steps only if we keep the exported comments and
recorded Hazel rule IDs honest about what happened.

## Desired Architecture

The mature architecture should make math automation modular:

```text
math capability =
  supported forms
  + rewrite rules
  + normalization primitives
  + theorem-prover mappings
  + UI labels/filtering
```

Examples:

- Arithmetic: integers, constants, addition, multiplication, identities,
  associativity, commutativity.
- Algebra: variables, monomials, polynomial addition/multiplication,
  distribution, factoring, collection, cancellation.
- Trigonometry: sin/cos/tan, pi, angle expressions, trig identities.
- Future programming-language levels: AST-sensitive rewrites where
  associativity/commutativity may deliberately be disabled.

This matters because the same user-facing UI should eventually let users choose
which structures and rewrites are active. A user might enable trig identities
but disable broad AC normalization, or enable arithmetic constants but not
variables.

## Preliminary Generalization Plan

The current `rewrite_level` enum is useful as a UI and rollout scaffold, but it
should not remain the deep source of proof-search behavior. The next
architecture pass should split "what math exists" from "what automation is
allowed" and make proof search consume that combined specification directly.

Proposed shape:

```text
math profile =
  structures
  + operation capabilities
  + rewrite groups
  + normalization primitives
  + search policy
  + prover export metadata
```

Structures describe the expressions and semantic domains in scope. Examples:

- integers and integer constants
- floating-point literals or approximated numeric evaluation
- rational/real variables
- additive and multiplicative expressions
- powers and polynomial terms
- trigonometric functions and constants such as `pi`
- differentiable functions and derivative forms
- programming-language AST nodes where algebraic rewrites may be invalid

Operation capabilities describe which transformations are available for those
structures. These should be independent toggles, not hard-coded consequences of
a level:

- associativity for addition or multiplication
- commutativity for addition or multiplication
- additive/multiplicative identity elimination
- constant folding
- distribution and factoring
- polynomial expansion, collection, and cancellation
- trig identities, grouped by family such as Pythagorean, sum/difference,
  double-angle, reflection, and reciprocal identities
- differentiation rules, such as sum/product/chain rules
- numeric approximation, with explicit precision and export limitations

Rewrite groups can remain the user-facing packaging. Arithmetic, Algebra,
Trigonometry, and Calculus are good presets, but a preset should lower to a
resolved `math profile` before search starts. For example, an Algebra preset
may enable integer constants, variables, addition, multiplication,
associativity, commutativity, distribution, and polynomial collection. A
programming-language preset may enable AST rewrites and beta-reduction while
leaving commutativity disabled.

Proof search should receive the resolved profile, not only a level. That means
search asks the profile questions such as:

- Is this construct supported?
- Which primitive rewrites are allowed at this occurrence?
- Which normalization primitives are allowed for this operator/domain?
- Which macro steps may be proposed visibly?
- Which prover breadcrumbs are required for export?
- Which transformations are deliberately disabled even if Hazel knows about
  them globally?

The practical migration path is to keep `rewrite_level` as a preset selector
while introducing an internal capability spec that is derived from the selected
level. Existing callers can continue passing levels, but `AxiomSearch`,
normalizers, `RewriteChecker`, and export code should move toward consuming the
resolved spec. Once that seam exists, the UI can expose finer toggles without
rewriting the search engine.

Concrete item 4 slice:

- add a small profile resolver near `Axioms.re` that turns the selected level
  plus optional UI toggles into supported structures, enabled rewrite-family
  IDs, normalization primitives, and prover-export mappings
- make `AxiomSearch` ask that resolved profile for applicable rules instead of
  filtering only by `rewrite_level`; the same lookup should drive dropdown
  rule availability
- have normalizers return planned steps tagged with profile ID, rewrite-family
  ID, primitive or macro status, occurrence, and local before/after expressions
- require `CoqProofExport.re` to resolve every recorded rule or macro through
  the same profile metadata; missing mappings should make the step
  non-exportable rather than falling back to an unnamed simplification

## Configurable AC Normalization

Associativity/commutativity normalization should be a first-class configurable
primitive, not a special-case fast path outside the architecture.

The current targeted reordering path is the right performance instinct: raw BFS
over associativity and commutativity explodes quickly. The architectural issue
is that `arith.reorder_add_terms` and `arith.reorder_mul_factors` are currently
treated as special search shortcuts. Long term they should become profile-owned
normalization primitives with explicit input, output, and proof obligations.

An AC primitive should declare:

- operator family: addition, multiplication, or another AC operator
- domain restrictions: integers, reals, symbolic terms, trig products, etc.
- enabled laws: associativity, commutativity, identities, coefficient folding
- stable term ordering policy
- whether constants or coefficients may be grouped
- visible macro rule ID and label
- lowering strategy into primitive prover breadcrumbs

When proof search considers a source/target pair, it can ask the active profile
whether AC normalization is available for the relevant operator. If enabled,
the primitive may use flattened/sorted internal forms to find a path, but the
accepted result must still record breadcrumbs. At minimum the trace should
record the macro ID, operator, source order, target order, occurrence, and
normal form. For export, that macro should either lower to local
assoc/comm/identity rewrites or be marked as a macro with a known prover tactic
and a tracked obligation to expand it later.

This keeps the important performance property without making AC reordering an
unexplained escape hatch. It also lets different profiles make different
choices: algebra can enable AC normalization broadly, trig can enable
multiplication reordering for factors around trig identities, and AST-sensitive
programming-language profiles can leave AC normalization off.

## Normalization Strategy

Normalization should guide search, not replace proof.

For arithmetic/algebra:

- flatten additions and multiplications when the active structure permits it
- sort terms/factors by a stable order
- group constants
- group like monomials
- keep trig terms farther to the right in mixed expressions

For proof export:

- the normalizer may use coarse internal forms to decide equality
- every accepted normalization step must eventually produce replayable
  breadcrumbs
- coarse rules are allowed temporarily, but they should be labeled as coarse
  and later expanded into local rewrites

This avoids proof-search blowup while preserving the path to theorem-prover
trust.

## UI Implications

The stepper UI should expose the same architecture:

- mode controls choose the active math level and automation stage
- unsupported constructs show compact warnings such as `Needs Trigonometry`
- dropdowns must operate on the effective selected subexpression, including
  reparenthesized selections
- proof search can show valid/invalid only after checking the current source
  and target
- console logging should remain useful for debugging search traces

The key invariant is that the UI selection, checker source expression, search
trace, and exported proof target all refer to the same selected subterm.

## Near-Term Implementation Priorities

1. Make every accepted proof-search result export as a checked Coq/Rocq proof.
2. Move Coq/Rocq tactic mappings closer to the `rewrite_rule` specification.
3. Expand arithmetic normalization breadcrumbs, especially add/mul reordering.
4. Extend algebra proof breadcrumbs for distribution, factoring, FOIL, and
   collection.
5. Keep trig identity steps small and exportable before adding larger trig
   simplification search.
6. Keep validating both local `coqc` and browser JSCoq after export changes.

## Design Rule

No automation step should be accepted only because Hazel can compute that two
expressions are equal. It should be accepted because Hazel can name the
operations that connect them, store those operations, and either replay them in
a theorem prover now or clearly mark the missing proof export work.

## Proposed JSCoq/Rocq Tactic-Search Backend

This is a larger alternative backend, not a replacement for the current local
search on day one.

The idea is to let JSCoq/Rocq participate in each written-step check, instead
of using JSCoq only after Hazel has already generated a full export file. For a
selected source expression and target expression, Hazel would generate a small
Rocq goal and ask a level-specific tactic family to prove it.

The target user-facing shape is:

```text
By high-school trig, show source = target.
```

Internally that means:

1. Hazel chooses the active math profile: arithmetic, algebra, trigonometry,
   etc.
2. Hazel generates a Rocq goal for the selected subexpression.
3. Hazel loads only the tactic family allowed by that profile.
4. JSCoq tries the bounded tactic.
5. If the tactic succeeds, Hazel stores a certificate describing what was used.
6. Export replays either the same tactic certificate or a lowered trace of
   primitive Hazel proof steps.

### Backend Seam

Add a `ProofSearchBackend` layer beside `AxiomSearch.re`.

Inputs should match the current bounded search:

- source expression
- target expression
- active rewrite level/profile
- allowed rule IDs or allowed tactic groups
- maximum depth/state/time budget
- current statics context and environment

Outputs should lower to the existing `RewriteChecker.trace_summary`. A backend
must not return only `proved: true`; the UI and export path need at least:

- rule or tactic family ID
- source and target expressions
- selected occurrence/path
- local before/after terms when known
- proof export policy

The existing `AxiomSearch.search` should become the first implementation:

```text
ProofSearchBackend.LocalAxiomSearch -> AxiomSearch.search
ProofSearchBackend.JSCoqTacticSearch -> JSCoq/Rocq tactic backend
```

This keeps offline/local behavior working and gives us a regression oracle
while the prover-backed backend matures.

### What JSCoq May Do

The first JSCoq backend should be conservative:

- It may prove a single selected step using a named tactic family.
- It may use a bounded sequence of rewrite lemmas registered for the active
  math profile.
- It may use a macro tactic such as `hazel_high_school_trig` only if the
  resulting Hazel trace marks that as a collapsed macro.

It should not silently call broad tactics such as `ring`, `field`, `lra`, or
unbounded `auto` and pretend the result was a primitive Hazel rewrite.

Good first tactic families:

```coq
Ltac hazel_arith :=
  first [
    hazel_arith_normalize
  | hazel_ac_reorder
  | hazel_bounded_rewrite_search
  ].

Ltac hazel_trig :=
  first [
    hazel_trig_identity
  | hazel_ac_reorder
  | hazel_bounded_rewrite_search
  ].
```

The useful experiment is to learn what Rocq can discharge from a small curated
rewrite database, not to maximize proving power immediately.

### Current Branch Status: Rocq-Backed Search

As of June 28, the Search pane has an experimental JSCoq/Rocq-backed path for
all enabled math levels: Arithmetic, Algebra, and Trigonometry.

The active flow is:

1. Hazel computes the selected source expression and the typed target
   expression from the Search pane.
2. `ProofSearchBackend.re` chooses a domain:
   - `Z` for integer/arithmetic/algebra expressions
   - `R` for trig, floats, `pi`, or real builtins such as `sin` and `cos`
3. Hazel generates a small Rocq theorem:
   ```coq
   Theorem hazel_rocq_search : forall ..., source = target.
   Proof. intros. hazel_<level>. Qed.
   ```
4. Browser JSCoq checks that theorem.
5. If JSCoq succeeds, Hazel stores a `CollapsedMacro` trace summary with a
   level-specific rule ID:
   - `rocq.arithmetic_tactic_search`
   - `rocq.algebra_tactic_search`
   - `rocq.trigonometry_tactic_search`
6. Full export replays that same macro tactic in `CoqProofExport.re`, and local
   `coqc` is still the stronger validation path.

The current named tactics are deliberately coarse:

```coq
Ltac hazel_arithmetic :=
  first [lia | hazel_power_normalize | hazel_rewrite_search 8%nat
        | hazel_mul_reorder | reflexivity].

Ltac hazel_algebra :=
  first [nia | lia | hazel_power_normalize | hazel_rewrite_search 10%nat
        | hazel_mul_reorder | reflexivity].

Ltac hazel_trigonometry :=
  first [hazel_rewrite_search 12%nat | hazel_power_normalize
        | hazel_mul_reorder | hazel_algebra | reflexivity].
```

These are not yet primitive proof traces. They are accepted only as labeled
macro rules, which is important: the UI should say that Rocq proved the step
with an arithmetic/algebra/trig tactic family, not that Hazel found every
individual rewrite. This gives us sound checking now while preserving the
future requirement to lower macros into smaller theorem-prover breadcrumbs.

Current verified capabilities:

- Arithmetic over integers: constant folding, reordering, and linear affine
  equalities such as `x + 3 = 3 + x`.
- Algebra over integers: distribution/factoring-style polynomial equalities
  such as `x * (y + z) = x*y + x*z`, plus small literal powers such as
  `x^4 = x^1 * x^3`.
- Trigonometry over reals: Pythagorean identity, sum/difference and
  double-angle rewrites already present in the rewrite family, plus simple
  power normalization such as `sin(x)^4 = sin(x)^2 * sin(x)^2`.

Current limitations:

- Macro tactics can be too coarse for thesis-level explanation. A successful
  `hazel_algebra` proof currently records one macro step, not every
  distribution, associativity, or commutativity rewrite.
- `lia`, `nia`, and `lra` are broad theorem-prover tactics. They are sound, but
  they are not acceptable as final primitive explanations. They should either
  remain explicitly labeled as collapsed macros or be replaced/lowered into
  smaller traces.
- Browser JSCoq startup is still expensive because it loads Rocq packages in
  the browser. The Search pane now warms JSCoq earlier, but each actual check
  uses a fresh hidden session to avoid stale-success bugs.
- JSCoq success tells us the equality is provable, but not which subterm or
  lemma sequence was used. Hazel still needs its own occurrence/path metadata
  for good UI explanations.
- The Reals profile is fragile across JSCoq/local `coqc` versions because of
  import-prefix differences (`Coq` vs `Stdlib`) and package availability.

### Trace And Export Policy

There are three acceptable result classes:

1. `PrimitiveTrace`: JSCoq found or checked a sequence that Hazel can represent
   as existing `RewriteChecker.prover_step`s.
2. `CollapsedMacro`: JSCoq proved the step with a named tactic family, but
   Hazel cannot yet expand every primitive substep. The UI/export must label it
   as a macro such as `trig.high_school_identity`.
3. `Rejected`: JSCoq proved something using tactics outside the selected
   profile, timed out, or could not produce an acceptable certificate.

This lets us use JSCoq as a real proof backend while preserving the thesis
constraint: Hazel steps remain explainable and exportable, not black-box CAS
simplifications.

### Risks

- LTac can prove goals with lemmas outside Hazel's selected math level unless
  the tactic environment is carefully restricted.
- JSCoq success does not automatically tell Hazel which subterm changed.
- Browser JSCoq and local `coqc` may disagree because of library versions,
  import prefixes, or package availability.
- Tactic search can become slow or nondeterministic unless every backend call
  has a strict timeout and depth budget.
- If the first version stores only "Coq proved it," we lose the proof-step
  breadcrumbs needed for UI explanation and future Lean/Rocq exports.

### Recommended First Branch

Build this on a separate branch from the current local-search implementation.
The first milestone should be a parallel experimental backend:

1. Add the `ProofSearchBackend` interface.
2. Route current search through `LocalAxiomSearch` without behavior changes.
3. Add a JSCoq backend that checks one small class of goals, such as trig
   identity steps over reals.
4. Log the generated goal, tactic family, timeout, and result in the console.
5. Store successful results as `CollapsedMacro` unless they can already lower
   to primitive `prover_step`s.
6. Keep local `coqc` export and browser JSCoq checks as separate validation
   paths.

Open design questions:

- Should JSCoq be authoritative for acceptance, or should it initially be an
  advisory checker behind Hazel's local result?
- Should Hazel enumerate candidate tactics and ask JSCoq to check each, or
  should Rocq own the bounded search inside one tactic?
- How much trace detail is required before a JSCoq-found step may appear as
  `Valid` in the normal stepper flow?
