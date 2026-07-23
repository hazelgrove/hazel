# Hazel Tactic Search: Simple, Medium, and Detailed Explanations

This document explains Hazel's current tactic-search architecture at three
levels of detail. The short version is intended as a meeting-ready summary;
the longer sections describe the current implementation and its remaining
architectural debt.

## Simple explanation: 30-second version

Hazel treats a math Profile as a list of permitted moves.

When a student proposes a result:

1. Hazel looks for a sequence of enabled rules and cleanup operations that
   transforms the original expression into the proposed result.
2. If Hazel finds that sequence, it records the exact steps used.
3. Hazel sends that exact proof trace to Rocq for independent verification.
4. Only a result with both an enabled Profile trace and a successful Rocq proof
   is marked **Valid** and can be inserted with **Replace**.
5. If Rocq proves the equality but Hazel cannot derive it using the active
   Profile, Hazel labels it **Equivalent, outside profile** and does not allow
   replacement.

The key idea is:

> Hazel decides whether the method is allowed; Rocq verifies that the allowed
> method is mathematically sound.

## Medium explanation: architectural overview

### 1. Profiles define available mathematical capabilities

A Profile contains:

- Visible operations, such as distribution, factoring, trigonometric
  identities, and differentiation rules.
- Cleanup operations, such as constant folding, associativity, commutativity,
  identity removal, and collecting like terms.
- Check Result normalizers.
- Rocq tactics associated with the enabled rules.
- Policies controlling larger One Step operations, such as complete polynomial
  expansion.

Math levels form an inheritance graph rather than merely a linear list:

```text
Arithmetic
    │
  Algebra
   ├─────────────┐
   │             │
Trigonometry   Functions/Lists
   │
Calculus
```

Consequently:

- Algebra inherits Arithmetic.
- Trigonometry inherits Algebra.
- Calculus inherits Trigonometry, Algebra, and Arithmetic.
- A future linear-algebra-like branch could inherit Algebra without inheriting
  Trigonometry.

The Profile UI can enable or disable individual inherited capabilities.

### 2. One Step validates a bounded mathematical operation

One Step asks:

> Can the selected source become this target through one permitted conceptual
> operation and its permitted cleanup?

For example:

```text
(x + 1) * (x + 2)
→ x*x + 3*x + 2
```

Hazel may record:

```text
expand polynomial
distribute multiplication
collect like terms
fold constants
```

This is displayed as one conceptual FOIL operation, but its internal proof
trace retains the smaller semantic operations. If **Collect like terms** is
disabled, Hazel cannot silently use it to accept a collected result.

### 3. Check Result searches for a Profile-valid trace

Check Result is broader than One Step. It tries, in order:

1. Specialized calculus checking when derivatives are involved.
2. Direct cleanup or recognized single-operation traces.
3. Hazel normalizers, such as polynomial or affine normalization.
4. Bounded local rule search using only rule IDs enabled by the active Profile.

The resulting trace contains:

- The rule IDs used.
- The before and after expressions.
- Local subexpressions affected by each rule.
- Whole-expression states.
- Whether each operation was a visible rewrite or cleanup.
- Exportability information.

### 4. Rocq certifies the trace

Once Hazel finds a Profile-valid trace, it generates a compact Rocq theorem over
the real numbers. A polynomial transition may produce a theorem conceptually
like this:

```coq
Theorem hazel_rocq_search :
  forall x : R,
    (x + 1) * (x + 2) = x*x + 3*x + 2.
Proof.
  intros.
  nra.
Qed.
```

Using `nra` here does not mean that `nra` decided which operations were
permitted. Hazel has already authorized the exact transition through the
Profile. Rocq is certifying that the Profile-approved transition is a true
equality.

### 5. Outside-profile equivalence is kept separate

If Hazel cannot find an enabled trace, it can optionally ask Rocq whether the
expressions are mathematically equivalent.

If Rocq succeeds, Hazel displays:

```text
Equivalent, outside profile
```

It does not display **Valid**, and **Replace** is unavailable. This gives useful
mathematical feedback without allowing powerful Rocq automation to bypass a
teacher's Profile.

## Complex explanation: implementation details

### Source map: where to see the architecture in Hazel

These are the most useful entry points when reading the implementation:

- [`Axioms.re`](../src/language/proof/Axioms.re) is the shared policy catalog.
  It defines math levels, cleanup capabilities, visible-rule policies,
  Hazel/Rocq backends, Profile stage plans, and concrete rule IDs. For example,
  the algebra catalog contains `alg.distribute_mul_add`, `alg.factor_common`,
  and `alg.expand_polynomial`; the calculus group contains `calc.diff_sum`,
  `calc.diff_product`, and `calc.diff_power`.
- [`AxiomSearch.re`](../src/web/app/editors/stepper/AxiomSearch.re#L676) is the
  generic local graph search. Its `search` function constructs a bounded BFS
  frontier, applies every allowed rule at every applicable occurrence, dedupes
  expression states, and records the chosen applications.
- [`RewriteChecker.re`](../src/web/app/editors/stepper/RewriteChecker.re) holds
  the structural One Step and Check Result checkers. It contains affine and
  polynomial representations, normalization traces, FOIL/distribution checks,
  calculus trace construction, and the final Profile rule-ID guard.
- [`ProofSearchBackend.re`](../src/web/app/editors/stepper/ProofSearchBackend.re)
  orchestrates Check Result. `local_profile_trace` orders the Hazel-side
  checkers, while `profile_search_definitions` generates the bounded Rocq Ltac
  search from the active Profile.
- [`AlgebraIdentityRewrite.re`](../src/web/app/editors/stepper/AlgebraIdentityRewrite.re),
  [`TrigRewrite.re`](../src/web/app/editors/stepper/TrigRewrite.re), and
  [`DifferentiationRewrite.re`](../src/web/app/editors/stepper/DifferentiationRewrite.re)
  are concrete examples of domain-specific rewrite implementations used by
  catalog backends.
- [`AxiomsBox.re`](../src/web/app/editors/stepper/AxiomsBox.re) connects this
  machinery to the One Step/Check Result UI and the asynchronous Rocq verdict.

The important boundary is that `Axioms.re` says which capabilities are
available, while the checker/search modules implement how those capabilities
produce and certify a trace.

### What search algorithm does Hazel use?

There is no single global algorithm, and Hazel currently does **not** use A*.
The path depends on which layer can handle the goal:

1. **Structural checkers and normalizers are goal-directed, not graph search.**
   One Step and the first Check Result passes parse the source and target and
   try to construct a certificate directly. Polynomial checking, for example,
   canonicalizes both sides and derives the required semantic rule IDs. It does
   not enumerate arbitrary intermediate expressions.
2. **The generic local Hazel search is bounded BFS.**
   [`AxiomSearch.search`](../src/web/app/editors/stepper/AxiomSearch.re#L676)
   maintains a frontier for one rewrite depth at a time. It checks the entire
   current frontier for the target before generating the next frontier, so the
   first ordinary result has the fewest graph edges under that enumeration.
   By default it searches to depth 4 with at most 250 retained states per
   depth. It also caps generated successors, deduplicates states by expression
   key, and excludes broad reorder rules from ordinary expansion.
3. **Exact reordering has a targeted shortcut.** Before and during BFS,
   addition/multiplication reorder rules can compare the current expression to
   the requested target and construct the needed reorder trace directly. This
   avoids exploding the frontier with every associative/commutative
   permutation. It is goal-directed, but it is not an A* heuristic.
4. **Generated Rocq search is bounded depth-first/backtracking.**
   [`profile_search_definitions`](../src/web/app/editors/stepper/ProofSearchBackend.re#L901)
   emits `hazel_profile_search_exact n`. Rocq's `first [...]` tries enabled
   tactic branches in their generated order; each successful `progress` step
   recurses with smaller fuel. Operationally this is depth-limited DFS with
   backtracking, preceded by guarded direct finishers and finish-only
   normalizers.

So a concise answer for a meeting is:

> Hazel first uses structural, goal-directed certificate builders; if those do
> not apply, its local rewrite graph search is bounded BFS. Its generated Rocq
> fallback is bounded depth-first/backtracking over Profile-enabled tactics.
> It does not currently use A* or a learned cost heuristic.

This hybrid is intentional. Canonicalization avoids search for domains where a
direct certificate is available, BFS gives understandable short local rewrite
paths, and the Rocq layer independently checks or searches only the tactic
branches compiled from the active Profile.

### A. The rule catalog is the policy source

Rules are represented in a shared catalog. Each rule can specify:

- A stable rule ID.
- Display metadata.
- The math level where it is introduced.
- Whether it is visible, normalization-only, or guarded normalization.
- Direction: forward, reverse, or both.
- A Hazel implementation backend.
- A Rocq implementation backend.
- Allowed cleanup operations.
- Required cleanup operations.
- Required supporting rules.
- A repetition mode or fuel limit.

An entry is conceptually similar to:

```text
Rule: Distribute multiplication over addition
ID: alg.distribute_mul_add
Introduced at: Algebra
Direction: both
Hazel backend: algebra distribution
Rocq backend: distributivity lemmas
Allowed cleanup: selected associativity/commutativity operations
```

For the real definitions, compare the lightweight rewrite groups around
[`algebra_rewrite_group`](../src/language/proof/Axioms.re#L643) with the richer
`math_rule` type near the top of the same file. The group supplies user-facing
rule IDs and labels; the richer catalog attaches direction, prerequisites, and
Hazel/Rocq backends. Actual expression rewrites are dispatched by
[`apply_rule_at_root`](../src/web/app/editors/stepper/AxiomSearch.re#L204) and
then lifted to every subexpression by
[`apply_rule_everywhere`](../src/web/app/editors/stepper/AxiomSearch.re#L370).

A Profile is constructed from the catalog capabilities inherited through the
math-level graph and then modified by the user's Profile toggles.

### B. Profiles compile into stage-specific plans

The same Profile is compiled differently for three automation stages:

- `Manual`: One Step or primitive validation.
- `MultiStepCheck`: Check Result.
- `AutoEval`: automatic simplification.

A compiled stage plan contains:

- Cleanup atoms.
- Visible-rule atoms.
- Check-only normalizer atoms.
- Pre- and post-cleanup capabilities.
- Rocq backends for active normalizers.
- An active Rocq tactic plan.

A Rocq tactic-plan step is retained only if every rule ID on which it depends
is enabled by the Profile. This prevents a tactic group from remaining active
after one of its semantic dependencies has been disabled.

### C. Hazel searches for semantic traces first

The main Check Result path calls `local_profile_trace`. It tries increasingly
general Hazel-side mechanisms:

```text
calculus trace
      ↓
direct cleanup or recognized written step
      ↓
normalization trace
      ↓
bounded axiom search restricted to allowed rule IDs
```

Any trace returned by bounded search is checked again to ensure that every
recorded rule is permitted by the active Profile.

The orchestration is visible in
[`local_profile_trace`](../src/web/app/editors/stepper/ProofSearchBackend.re#L33):
calculus and written-step checkers run first, and only then does it invoke
`AxiomSearch.search` with the union of enabled visible and normalization rule
IDs. The post-search check uses
`trace_rule_allowed_by_profile` in
[`RewriteChecker.re`](../src/web/app/editors/stepper/RewriteChecker.re#L3891).

The result is a `trace_summary` containing semantic provenance rather than only
the fact that a Rocq tactic succeeded.

### D. Polynomial and FOIL recognition is structural

FOIL is not implemented as a recognizer for a particular expression such as
`(x+1)*(x+2)`. Hazel parses expressions into a polynomial representation and
asks questions such as:

- Does this factor normalize to multiple polynomial terms?
- Are the source and target polynomially equivalent?
- How many multi-term factors are being multiplied?
- Does the target require collecting terms?
- Is complete polynomial expansion permitted at this math level?

The same machinery therefore handles:

```text
(x-2)*(x+5)
(a+b)*(c+d)
(x+1)*(x**2+x+1)
(x+1)*(x+2)*(x+3)
3*(x+2)*(x-1)
```

Incorrect coefficients are rejected because polynomial equivalence fails.

### E. The proof trace records transitions, not just tactic names

Each prover step includes:

```text
origin
rule ID
local before expression
local after expression
whole before expression
whole after expression
occurrence number
human-readable detail
```

Several internal rule IDs can describe the same whole transition. A single
FOIL operation may record both:

```text
alg.expand_polynomial
alg.distribute_mul_add
```

If they describe the same before-and-after whole expression, the exporter
groups them into one recorded transition. This avoids duplicate Rocq
assertions.

### F. Exact replay is kept compact

For a single whole-expression transition, Hazel replays the certificate
directly. It does not generate an intermediate assertion and immediately apply
it when it can simply run the certificate tactic.

For multi-transition traces, intermediate assertions remain useful because
they connect successive recorded states.

The exporter also chooses a small import prelude unless a trace explicitly
requires Hazel's larger custom tactic library.

### G. Different transitions get different certificates

The exporter does not indiscriminately call one universal tactic:

- Direct distribution uses explicit distributivity rewrites.
- Identity cleanup uses exact identity lemmas.
- Calculus uses derivative lemmas and differentiability evidence.
- Complete real-polynomial transitions use `nra`.
- Named polynomial identities use a deterministic polynomial certificate.

`nra` is used only after Hazel has authorized the transition. It is a
certificate backend, not an unrestricted Profile-search fallback.

The verdict boundary is:

```text
Hazel trace exists + Rocq succeeds
    → Valid

No Hazel trace + broad Rocq equivalence succeeds
    → Equivalent, outside profile

Hazel trace exists + Rocq fails
    → Certificate/export failure

No Hazel trace + Rocq fails
    → Invalid
```

### H. Calculus has a specialized path

Differentiation requires additional semantics because `diff` is a Hazel
mathematical construct rather than ordinary Rocq arithmetic syntax.

The calculus checker records rules such as:

- Linearity.
- Product rule.
- Quotient rule.
- Power rule.
- Chain rule.
- Derivative of a constant.
- Derivative of the active variable.

Enabled cleanup can be incorporated after an appropriate derivative operation,
while disabled cleanup remains visible in the result. Rocq export translates
those semantic steps into real-analysis derivative statements and supporting
proofs.

### I. The equivalence fallback is deliberately non-authoritative

The broader equivalence program may use a tactic powerful enough to establish
equality. Its success is not treated as evidence that the Profile permits the
transformation.

The UI assigns the verdict based on whether an enabled local trace existed:

```text
Rocq success + local trace     → ProfileValid
Rocq success + no local trace  → EquivalentOutsideProfile
```

Only `ProfileValid` enables **Replace**.

## Meeting example

Suppose the Profile enables distribution, constant folding, and collecting
like terms. The student proposes:

```text
(2*x + 3) * (x + 4)
→ 2*x**2 + 11*x + 12
```

Hazel:

1. Recognizes a product of multi-term polynomials.
2. Verifies polynomial equivalence.
3. Builds a trace using expansion, distribution, constant arithmetic, and
   collection.
4. Confirms that every capability is enabled.
5. Sends the exact transition to Rocq.
6. Rocq verifies the equality.
7. The UI reports **Valid** and enables **Replace**.

If **Collect like terms** is disabled:

1. Hazel cannot construct the required enabled trace.
2. Rocq may still prove the equality.
3. The UI reports **Equivalent, outside profile**.
4. **Replace** remains unavailable.

## Current maturity

The architecture is substantially Profile-driven, but it remains transitional:

- The shared catalog increasingly defines both Hazel and Rocq behavior.
- Some specialized normalizers and calculus certificates still have custom
  implementations.
- Some powerful Rocq tactics remain as certificate backends.
- The UI currently orchestrates asynchronous JSCoq checking; the generic
  `JSCoqTacticSearch` backend entry point is not itself the entire
  implementation.
- Future work should decompose every tactic group completely into catalogued
  primitive proof atoms.

A concise characterization is:

> We have separated pedagogical authorization from mathematical certification.
> Hazel constructs a Profile-valid semantic proof trace, and Rocq checks that
> trace. Some certificate generation is still specialized, but powerful
> automation cannot convert an outside-Profile transformation into a valid
> student step.
