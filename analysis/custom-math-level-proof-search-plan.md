# Custom Math Levels and Profile-Safe Proof Search

## Status

Implementation-ready architecture plan, first audited on 2026-07-22 and
refreshed against the current source on 2026-07-28. The refresh accounts for
Auto Simplify, function-valued differentiation, scoped
`taylor_derivatives(...) in ...` lowering, and written-step persistence. Those
features add migration constraints; they do not change the plan's central
architecture or count as completion of the phases below. This document defines
migration order and validation gates; it does not itself authorize
implementation as part of unrelated audit-fix work.

## Goal

Allow an instructor to define a math level by composing existing mathematical
capabilities instead of selecting a hard-coded Hazel mode. The resulting level
must behave consistently in One Step, Check Result, automatic cleanup, proof
traces, and Rocq validation.

The migration should also retire policy fields, compatibility constructors,
labels, UI rows, and adapters that are proven redundant once all consumers use
the shared compiled proof plan. This cleanup is intentionally deferred to the
refactor: remove only data with no remaining semantic choice or production
caller, rather than mixing opportunistic deletions into behavior fixes.

The motivating proof of concept is a noncommutative algebra level suitable for
matrix- or linear-algebra-shaped expressions:

- inherit ordinary additive and multiplicative syntax;
- permit associativity;
- optionally permit additive commutativity;
- do **not** permit multiplicative commutativity;
- permit only explicitly selected identity, distribution, and normalization
  capabilities;
- never regain a disabled law through `ring`, `lra`, or a broad tactic group.

This is a capability-composition problem, not a request to add special-case
matrix recognizers.

NOTE NISHANT: To be clear: I do not want to add this as a math level BUT I do want to make sure we can support this as a custom profile. The goal is to make sure that we can support this as a custom profile without having to add a new math level, and instead modify axioms/rewrite rules to support linear algebra type contructs and rules.

## Architectural invariant

No Rocq tactic may make a result Profile-valid unless Hazel first produces an
explicit proof plan composed entirely of capabilities enabled by the active
profile.

Broad tactics may be used only:

1. as deterministic certificates for a transition already authorized by a
   capability with a precise contract; or
2. by the separate mathematical-equivalence fallback, whose strongest verdict
   is `Equivalent, outside profile`.

## Model

### Math-level graph

Math levels form a DAG. A level declares zero or more parents and inherits their
catalog capabilities. Display order is separate from inheritance.

Example:

```text
Arithmetic
└── Algebra
    ├── Trigonometry
    │   └── Calculus
    └── Noncommutative Algebra
```

`Noncommutative Algebra` may inherit syntax and selected additive behavior from
Algebra while overriding the effective capability set to exclude
multiplicative commutativity. An inherited default is not irrevocable: custom
profiles need explicit enable/disable overrides after ancestor composition.

### Capability catalog

Every capability should declare:

- stable ID and user-facing metadata;
- the level/category where it is introduced;
- supported automation stages;
- a precise semantic contract;
- prerequisite capabilities;
- conflicts or laws that make it inapplicable;
- Hazel transition generator/checker;
- certificate strategy;
- cost/search metadata;
- optional Profile category path.

The catalog is the shared source for UI controls, Hazel search, proof traces,
Rocq certificate generation, and tests. Per-level tactic bundles must not be a
second source of mathematical authority.

### Stage-specific rewrite usage

Capability availability and application cardinality are separate policy
questions. A custom Profile must be able to compile each primitive capability
to a stage-specific usage allowance:

```text
RewriteUsage =
  | Disabled
  | AtMostOne
  | BoundedClosure { max_uses, max_states, cost }
```

`Disabled` is the zero-use case. `AtMostOne` permits one primitive occurrence
in the authorized plan. `BoundedClosure` permits a goal-directed sequence of
that primitive rule, subject to explicit search budgets. Built-in presets may
derive reasonable budgets from expression size, but every search must retain a
hard state/step ceiling.

This is not a return to the removed `visible_step_mode` metadata. That type had
one constant value, was mirrored across layers, and did not control all
consumers. The replacement belongs in the compiled `(profile, stage)` plan,
must have real execution semantics, and must be consumed uniformly by
Suggestions, One Step, Check Result, Replace, and export.

The same capability can have different allowances by stage. For example:

| Capability                   | One Step                              | Check Result                        | Reason                                                                                                     |
| ---------------------------- | ------------------------------------- | ----------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| Additive commutativity       | bounded closure                       | bounded closure                     | Reordering commonly needs several swaps.                                                                   |
| Multiplicative commutativity | Profile-dependent closure or disabled | same                                | A noncommutative level must compile this to disabled everywhere.                                           |
| Distribution                 | at most one                           | bounded closure                     | Introductory work exposes one distributive-law application, while result checking may find a sequence.     |
| Expand polynomial product    | disabled or one macro operation       | bounded/derived                     | Standard Algebra may treat full expansion as one conceptual shortcut; an introductory Profile disables it. |
| Collect like terms           | disabled or one cleanup phase         | bounded deterministic normalization | Availability remains independent of distribution.                                                          |

Reversible rules such as commutativity must never use naïve
"repeat until stuck": they do not have a natural stuck state and can loop.
Their closure should use a target-directed bounded search or a deterministic
canonicalization algorithm that emits the corresponding primitive swap trace.
Likewise, repeated distribution must record every occurrence rather than using
an opaque polynomial-equivalence success as authorization.

Primitive and macro operations remain distinct. `alg.distribute_mul_add`
means one use of the distributive law at one occurrence. A separate
`alg.expand_polynomial` capability may authorize a complete expansion as one
teacher-enabled conceptual operation, but its evidence must contain or justify
a plan composed from repeated distribution plus only the enabled cleanup
capabilities. This is polynomial-wide behavior, not a FOIL recognizer.

#### Temporary repeated-distribution bridge

The current implementation includes a deliberately small bridge in
[`ProfileBoard.re`](../src/web/app/editors/stepper/ProfileBoard.re): the
distribution operation has an **Allow repeated distribution in one step**
control under **Allowed step operations → Algebra**. It updates
`one_step_policy.allow_polynomial_expansion` through the temporary
`one-step.repeat-distribution` option. One Step uses that switch to distinguish
one primitive distribution from a complete expansion, while Check Result keeps
its independently authorized expansion route.

This is general polynomial behavior rather than an expression-specific FOIL
recognizer, but its Profile rendering and policy field are intentionally
rule-specific. During this refactor:

- replace the hard-coded `alg.distribute_mul_add` UI placement with
  catalog-driven stage-usage metadata;
- compile controls such as `Disabled`, `AtMostOne`, and `BoundedClosure` for
  every eligible primitive rule;
- preserve primitive factor order so distribution does not implicitly require
  multiplication commutativity;
- migrate the existing checkbox state to the compiled distribution allowance;
- remove `one-step.repeat-distribution` and
  `allow_polynomial_expansion` after every consumer uses the shared proof-plan
  policy.

The intended result should look much like the current UI—a repeated-use
refinement located beside distribution—but it must be generated from catalog
metadata and reusable for commutation, reassociation, or other rules rather
than inserted by a distribution-specific branch.

We should still be able to support something like a full FOIL in algebra one-step mode with our rules: but if it requires us allowing, say, multiple distribute operations in a "single step" that is fine. Our UX for math levels and our search procedure do need to permit such a capacity.

### Proof-plan intermediate representation

Hazel should produce a typed proof plan before asking Rocq to validate:

```text
ProofStep {
  capability_id,
  source,
  target,
  occurrence,
  application_index,
  evidence,
  certificate_strategy
}
```

An `AuthorizedPlan` should also expose per-capability use counts so the plan
validator can check them against the compiled stage usage policy before Rocq is
invoked.

Primitive rules can use rewrite lemmas. Macro capabilities can use typed
strategies such as affine, polynomial, derivative, or trigonometric
certificates, but only after their Hazel-side contracts authorize the exact
transition.

## Current implementation map

The refactor should start from the following concrete boundaries rather than
introducing another parallel proof abstraction.

| Area                                      | Current source                                                                                                                                                                                                                    | Current responsibility and migration concern                                                                                                                                                                                                                                                                                                                                                                                                     |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Catalog and Profile compilation           | [`Axioms.re`](../src/language/proof/Axioms.re)                                                                                                                                                        | Defines rules, cleanup, normalizers, tactic backends, `math_profile`, `check_result_rule_ids`, one-step policy, and stage plans. It is the intended policy source, but normalizer enablement and the temporary repeated-distribution choice are still separately stored Profile state. It also catalogs calculus and Taylor operations; catalog presence must not be confused with syntax lowering or proof authorization. |
| Semantic recognition and traces           | [`RewriteChecker.re`](../src/web/app/editors/stepper/RewriteChecker.re)                                                                                                                               | Defines affine/polynomial representations, structural checks, calculus completion, Profile authorization checks, `prover_step`, and `trace_summary`. At more than 4,000 lines it is both a semantic library and an orchestrator. Recent generalized scalar, rational, and calculus paths make the need to split contracts from orchestration more urgent, not less.                                                          |
| Function-valued differentiation           | [`DifferentiationRewrite.re`](../src/web/app/editors/stepper/DifferentiationRewrite.re)                                                                                                               | Owns function-valued `diff`, explicit-variable lowering, capture-safe derivative-chain names, and standalone Taylor teaching expansion. It can emit useful calculus transitions, but those transitions still need conversion into the same catalog-derived plan as every other consumer. It must not become a second calculus policy engine.                                                                              |
| Taylor surface syntax and lowering         | [`MakeTerm.re`](../src/haz3lcore/lang/MakeTerm.re), [`Parser.mly`](../src/menhirParser/Parser.mly), and [`ExpToSegment.re`](../src/haz3lcore/pretty/ExpToSegment.re)                                    | `taylor_derivatives(f, n) in body` now lowers directly to genuine nested `let` bindings whose continuation is `body`; it is no longer represented as a disguised `Seq`. Both the structure editor and text parser must produce the same scope. The current eager lowering can make the surface `in` lose token identity and lets ordinary evaluation substitute generated bindings immediately, so visible staged declarations remain a separate UI/evaluator problem. The sugar grants no mathematical capability and must not bypass the Profile authorizer. |
| Scalar/sign normalization                 | [`TrigRewrite.re`](../src/web/app/editors/stepper/TrigRewrite.re)                                                                                                                                     | Historically owns `simplify_scalar_products`, even though integer-coefficient reduction, rational scalar cancellation, and double-negative normalization are general arithmetic operations. Move this behavior to a focused arithmetic-normalization provider; `TrigRewrite` should retain only genuinely trigonometric identities and transformations.                                                          |
| Generic local graph search                | [`AxiomSearch.re`](../src/web/app/editors/stepper/AxiomSearch.re)                                                                                                                                     | Runs bounded BFS, but imports expression helpers and trace types from `RewriteChecker`, which will complicate a clean split unless those shared types/helpers move first.                                                                                                                                                                                                                                                                        |
| Suggestions                               | [`AxiomsBox.re`](../src/web/app/editors/stepper/AxiomsBox.re)                                                                                                                                         | Generates trig, calculus, cleanup, and normalization suggestions and manually constructs several `trace_summary` values. Suggestions therefore do not all pass through the same authorizer as typed One Step results.                                                                                                                                                                                                                            |
| Check Result, Auto Simplify, and Replace   | [`MissingStep.re`](../src/web/app/editors/stepper/MissingStep.re)                                                                                                                                     | Builds the active Profile, debounces selection-driven Algebrite candidate generation for Auto Simplify, asks for a local trace, chooses Profile replay versus equivalence fallback, assigns `ProfileValid`, caches the trace, and enables Replace. Candidate generation is separate from authorization in intent, but the boundary is not yet a dedicated API. A collapsed fallback summary must never authorize Replace.       |
| Rocq search and replay                    | [`ProofSearchBackend.re`](../src/web/app/editors/stepper/ProofSearchBackend.re)                                                                                                                       | Repeats route ordering and Profile gating, generates bounded Ltac search, chooses guarded finishers, contains specialized calculus export, and creates collapsed macro summaries.                                                                                                                                                                                                                                                                |
| Expression-to-Rocq translation            | [`CoqExport.re`](../src/language/proof/CoqExport.re)                                                                                                                                                   | Translates supported Hazel mathematical expressions to Rocq over reals, including derivative encodings. This is distinct from proving a whole Hazel program containing `let`, function, sequence, or Taylor sugar; unsupported program structure must remain explicit rather than being silently flattened.                                                                                                                                    |
| Derivation export                         | [`CoqProofExport.re`](../src/web/app/editors/stepper/CoqProofExport.re)                                                                                                                               | Replays `RewriteChecker.prover_step` values, but still uses group names, justification strings, tactic groups, and broad fallbacks in some paths. Certificate selection is not yet entirely typed.                                                                                                                                                                                                                                               |
| Stored and rebased steps                  | [`WrittenStep.re`](../src/web/app/editors/stepper/WrittenStep.re), [`EvaluatorStep.re`](../src/web/app/editors/stepper/EvaluatorStep.re), and [`StepperBase.re`](../src/web/app/editors/stepper/StepperBase.re)                            | Persist traces and attempt to retain compatible evaluator/written steps as the program changes. Ordinary `let` continuation structure now supports more function/Taylor workflows, but a retained UI step is not automatically a still-valid proof plan: source, target, selection, Profile fingerprint, and program alignment must all remain compatible.                                                            |
| Profile UI state                          | [`ProfileBoard.re`](../src/web/app/editors/stepper/ProfileBoard.re)                                                                                                                                   | Uses the same rule-override mechanism for visible rules and `check_result_rule_ids`, then filters both fields independently. It is currently a dense expert/debug view, not a teacher-oriented mode editor. Keep it available while introducing a clearer builder, and remove its duplicated multi-step state only after normalizers are derived.                                                                                                   |
| Teacher mode and rewrite authoring        | Not yet implemented                                                                                                                                                                                   | Needs a separate teacher-facing builder over serializable level definitions and the shared rule catalog. It must configure the same compiled Profile consumed by proof planning; it must not create UI-only permissions or accept arbitrary tactic text as a shortcut around catalog validation.                                                                                                                                                  |

### Current Profile-valid path

The Check Result UI currently performs this sequence:

```text
MissingStep
  -> ProofSearchBackend.local_profile_trace
       -> calculus trace
       -> RewriteChecker written-step/normalizer trace
       -> bounded AxiomSearch trace
  -> if local trace exists: generate exact/profile Rocq replay
  -> otherwise: generate broad equivalence-only Rocq program
  -> Rocq success + local trace: ProfileValid
  -> Rocq success + no local trace: EquivalentOutsideProfile
  -> ProfileValid trace is cached and passed to Replace
  -> WrittenStep stores the trace for later Rocq export
```

This verdict boundary is correct and must be retained. The refactor should
replace the parallel trace constructors underneath it, not weaken the
`ProfileValid`/`EquivalentOutsideProfile` distinction.

Auto Simplify adds a candidate-producing layer in front of that same path:

```text
stable selection
  -> debounced Algebrite candidate
  -> the same ProfileProofPlan authorization used by Check Result
  -> the same Rocq certificate boundary
  -> Replace only when an AuthorizedPlan exists
```

The debounce and Algebrite call are usability mechanisms, not authorization.
For identical source, target, Profile, and stage policy, Auto Simplify and an
explicitly typed Check Result target must receive the same verdict and plan.

Function and Taylor work adds another boundary. Syntax/evaluator rewrites may
expose a mathematical subexpression, such as a `diff` body, inside ordinary
`let` structure. The mathematical transition should be authorized and exported
at that expression boundary. Whole-program `let`, function application,
sequence, and structural Taylor desugaring remain evaluator structure unless a
separate program-level Rocq evidence design is explicitly adopted.

### Concrete duplication and fragility to remove

- `RewriteChecker.trace_rule_allowed_by_profile` special-cases polynomial
  expansion and separately recognizes normalization-rule kinds.
- `RewriteChecker.check_result_backend_allowed` authorizes behavior by fragile
  justification strings such as `"arithmetic"` and `"algebra"`.
- `ProofSearchBackend.local_profile_trace` repeats route ordering already
  partially encoded by `RewriteChecker` and the catalog stage plan.
- `AxiomsBox` manually builds trace records for trig, calculus cleanup, and
  argument normalization suggestions.
- `AxiomsBox` can generate a general scalar/sign target through
  `TrigRewrite.simplify_scalar_products` and then manufacture a legacy trace
  when the shared Profile checker cannot authorize that exact transition.
  This can leave an attractive suggestion visible even though Rocq validation
  fails. Suggestion eligibility must instead require an `AuthorizedPlan`.
- Auto Simplify and explicit Check Result share most of their intended
  acceptance semantics but still enter through UI/candidate-generation paths
  in `MissingStep`; no Algebrite result should gain a separate authorization
  shortcut.
- `DifferentiationRewrite` can produce function-valued and Taylor-derived
  transitions while `AxiomsBox`, `RewriteChecker`, and
  `ProofSearchBackend` still assemble calculus evidence through other paths.
  Capture-safe lowering should remain reusable semantics, while authorization
  converges on one plan builder.
- Step persistence can retain structurally aligned UI steps after edits, while
  cached Check Result evidence has additional Profile and endpoint validity
  requirements. Conflating those two notions risks retaining a stale Replace
  plan for a newly edited program.
- Scoped Taylor syntax now lowers to ordinary nested `Let` forms, not `Seq`.
  Eager lowering currently loses the surface `in` token identity and ordinary
  evaluation may substitute the generated bindings before a learner steps
  them. Preserve lexical scope first; solve visible staging at the evaluator/UI
  boundary rather than restoring a fake sequence. Treating the surface builtin
  as its own proof authority would duplicate evaluator semantics and calculus
  policy.
- Expression export and whole-program export are separate concerns. Current
  real-term translation can certify selected mathematical expressions, but a
  monolithic theorem over arbitrary Hazel programming constructs is not yet
  specified.
- `ProofSearchBackend` separately computes affine direction requirements for
  distribution, factoring, and constant reordering.
- `CoqProofExport` sometimes selects tactics from group names or broad fallback
  tactic groups instead of a typed certificate strategy attached to the plan.
- `check_result_rule_ids` acts as independent UI state even when a normalizer
  is intended to be only an implementation of enabled primitive operations.
- `AxiomSearch` depends on many utility functions and trace types owned by
  `RewriteChecker`; splitting orchestration without extracting those first
  would create a dependency cycle or another miscellaneous utility module.

## Proposed module and API boundary

Names are provisional, but the dependency direction should be preserved.

### 1. `ProofTrace.re`: data only

Move `prover_step_origin`, `prover_step`, and the durable trace/plan record out
of `RewriteChecker` into a small module with no dependency on search or Rocq
generation. Preserve the current JSON representation initially so stored
written steps do not change in the type-extraction commit.

The eventual plan should distinguish authorization from certification:

```text
AuthorizedPlan {
  stage,
  source,
  target,
  steps,
  capability_ids,
  certificate_strategy,
  exportability,
  profile_fingerprint
}
```

`certificate_strategy` should be typed—for example lemma replay, affine,
polynomial, rational-polynomial, trigonometric, or derivative certificate—and
must not itself grant authorization. The exact constructors should be chosen
only after the normalizer-contract audit.

### 2. `MathRewriteUtil.re`: shared expression helpers

Move only genuinely generic expression operations currently imported by
`AxiomSearch` from `RewriteChecker`: wrapper stripping, operator recognition,
expression constructors, factor extraction, equality helpers, and deduplication
where appropriate. Affine and polynomial semantics should not be placed in
this utility module.

### 3. Focused semantic providers

Keep or extract pure semantic modules that answer questions without consulting
Profile UI state:

- primitive arithmetic/algebra transitions;
- affine recognition and equivalence evidence;
- polynomial recognition, expansion, and factorization evidence;
- trig identity transitions;
- derivative transitions and cleanup evidence.

During migration, `RewriteChecker` may remain a compatibility facade over
these providers. It should stop constructing system-wide policy plans.

### 4. `ProfileProofPlan.re`: the sole authorizer

The central API should accept an already resolved Profile and a stage.
Candidate origin is useful metadata but is not a source of permissions:

```text
authorize({profile, stage, candidate_origin, settings, env, source, target})
  -> Authorized(AuthorizedPlan)
   | Rejected(ProfileRejection)

suggest({profile, stage = Manual, settings, env, source})
  -> list(AuthorizedPlan)
```

For example, `AutomaticSimplify` and `UserEntered` candidates should both use
the `MultiStepCheck` stage policy. Recording their origins may improve UI
diagnostics, but must not change available rules, budgets, route priority, or
certificate strategy.

`ProfileRejection` should distinguish at least unsupported syntax, no semantic
route, disabled required capabilities, exhausted bounded search, and
non-exportable evaluation. This is useful for diagnostics but must not turn
Rocq equivalence into authorization.

The authorizer owns deterministic route priority. The initial priority should
preserve current behavior while making stage differences explicit:

```text
Manual / Suggestions:
  direct cleanup
  -> one visible primitive operation (including applicable trig/calculus rules)
  -> supported evaluator step

MultiStepCheck:
  full calculus completion
  -> direct cleanup
  -> one visible primitive operation
  -> derived normalizer route
  -> bounded catalog-rule BFS
```

The current implementation reaches calculus through more than one nested path;
the facade should preserve the user-visible priority while removing that
duplicate invocation.

Every returned plan must pass one final invariant check: endpoints match,
steps are contiguous, all authorizing capability IDs are enabled for the
stage, prerequisites and side conditions hold, and the chosen certificate
strategy is permitted for that evidence.

### 5. Certificate consumers

`ProofSearchBackend` and `CoqProofExport` should consume an `AuthorizedPlan`.
They may select or generate the attached certificate, but must not reconstruct
Profile policy. Broad equivalence checking remains a separate function that
accepts only source and target and can return only `EquivalentOutsideProfile`.

Suggestions, One Step, Check Result, Replace, stored Written Steps, and export
should pass the same plan value forward. Replace should not infer a new route
from the target. If the Profile changes while a candidate is open, the cached
plan must be invalidated or reauthorized before Replace is enabled.

## Normalizer-contract audit

Removing the multi-step section requires auditing every normalizer, not only
the three controls currently visible there.

| Normalizer                                       | Current state                                                                                                                                                                    | Required decision before derivation                                                                                                                                                                                |
| ------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `arith.affine_normalize`                         | Guarded catalog rule with no Hazel backend; `RewriteChecker` builds affine evidence while `ProofSearchBackend` separately checks direction and constant-reordering capabilities. | Define the supported affine grammar and direction-sensitive primitive requirements. Do not require distribution for a pure reorder, or permit factoring merely because `lra` succeeds.                             |
| `alg.expand_polynomial`                          | Catalogued as a normalizer but also treated specially by `one_step_policy.allow_polynomial_expansion` and `trace_rule_allowed_by_profile`.                                       | Decide whether full expansion is a named manual macro, a Check Result derived route, or both with different stage contracts. Express its primitive distribution/cleanup requirements once.                         |
| `alg.factor_polynomial_normalize`                | Finish-only `ring` certificate, no Hazel backend, and a static prerequisite list containing distribution, factoring, and cancellation.                                           | Produce structural polynomial evidence and direction-sensitive requirements. Factoring should require factoring capability; expansion should require distribution, rather than always requiring every listed rule. |
| `alg.power_add` / `alg.power_mul`                | Marked as normalization rules with replay-only Hazel backends and prerequisites that currently include distribution.                                                             | Audit whether the dependency is mathematically real or an artifact of tactic packaging; define their stage and direction explicitly.                                                                               |
| Trig-argument and calculus cleanup normalization | Some suggestion traces are assembled manually in `AxiomsBox`; calculus completion also invokes affine finishing.                                                                 | Route their evidence through the same authorizer and record the precise cleanup/argument-algebra capabilities used.                                                                                                |

For each row, the contract test matrix must include structurally different
positive cases, wrong-result negatives, each individually disabled primitive
prerequisite, direction-sensitive cases, unsupported syntax, and a generated
Rocq certificate check.

## Noncommutative proof of concept

Create a test-only profile assembled from catalog capabilities and write an example for me to try this out. Do not add expression-specific logic.

Expected accepted examples when multiplication associativity and distribution
are enabled but multiplication commutativity is disabled:

```text
(a*b)*c  -> a*(b*c)
a*(b+c)  -> a*b+a*c
(a+b)*c  -> a*c+b*c
a*1      -> a
```

Expected rejected examples:

```text
a*b      -> b*a
a*b*c    -> c*b*a
a*(b+c)  -> b*a+c*a
```

The last example is deliberately rejected: it requires commuting `a` past
`b` and `c`, not merely distributing.

Check Result must obey the same boundary. It must not use ordinary commutative
`ring` as a certificate for noncommutative goals. Suitable certificates should
be lemma replay or a future noncommutative normalization procedure with a
corresponding Rocq proof.

The idea is that these can behave like matrices and vectors with ease.

## Work phases

This is a significant tactic- and proof-search-architecture migration, not a
Profile-label cleanup. Use small commits and do not combine type extraction,
behavior changes, UI removal, and tactic deletion in one patch.

### Phase 0: characterization baseline and checkpoint

- Commit or otherwise checkpoint the current behavior before the refactor.
- Add a table-driven test helper that runs the same source/target/Profile pair
  through suggestions, manual authorization, Check Result authorization, Auto
  Simplify authorization, Rocq replay generation, and disabled-capability
  variants.
- For a fixed Auto Simplify candidate, assert exact verdict and plan parity
  with entering the same target manually in Check Result. Do not test
  Algebrite's choice of target as if it were proof authorization.
- Record current route priority for representative arithmetic, FOIL,
  factorization, power, trig, calculus, function-valued differentiation, and
  scoped Taylor examples.
- Classify current transitions as mathematical/exportable or
  evaluator-structural/non-exportable. In particular, record where `let`,
  substitution, function application, sequence, and eager Taylor-to-`let`
  lowering currently appear in stored derivations. Characterize whether the
  generated derivative bindings remain visible long enough for teaching.
- Add explicit tests that Rocq equivalence without an enabled Hazel trace can
  produce only `EquivalentOutsideProfile` and never a replaceable plan.
- Characterize plan invalidation when the selection, source, target, Profile,
  math level, or surrounding program changes, separately from ordinary
  written-step persistence.
- Record generated-program byte counts and browser JSCoq timings for a small
  stable fixture set so compact-certificate performance does not regress.

**Checkpoint:** tests describe existing behavior without changing production
authorization.

### Phase 1: extract shared trace types and expression utilities

- Add `ProofTrace.re` and move the existing trace data types and constructors
  without changing their fields or JSON representation.
- Update `WrittenStep`, `StepperBase`, `AxiomsBox`, `MissingStep`,
  `AxiomSearch`, `ProofSearchBackend`, and `CoqProofExport` to use the extracted
  type.
- Add `MathRewriteUtil.re` and move only the generic helpers required by both
  `AxiomSearch` and semantic checkers.
- Keep program-alignment/persistence metadata separate from mathematical proof
  evidence. A persistent UI step may refer to an `AuthorizedPlan`, but it must
  not change the plan's endpoint or Profile invariants.
- Keep compatibility aliases in `RewriteChecker` for one phase if that makes
  the review mechanical.

**Checkpoint:** no verdict, trace, generated Rocq program, or persisted written
step changes. `RewriteChecker` becomes smaller and `AxiomSearch` no longer
depends on it merely for trace types or syntax helpers.

### Phase 2: formalize catalog contracts and effective stage capabilities

- Add an explicit stage-support field or equivalent contract to catalog rules.
- Add the typed zero/one/bounded-closure usage policy to the compiled
  `(profile, stage)` capability view. Do not store a display-only mode on every
  catalog rule.
- Define inheritance/override rules for stage usage so a child Profile can
  tighten an inherited closure to one use or disabled, but cannot silently
  widen it through a tactic backend.
- Define safe default bounds and costs. Reversible rules require goal-directed
  search or trace-producing canonicalization, never repeat-until-stuck.
- Separate `required_rule_ids` from direction-sensitive route requirements;
  static prerequisites alone are insufficient for expansion versus factoring.
- Define a single catalog function that compiles the effective enabled
  capabilities for `(profile, stage)`.
- Define one predicate that validates whether a proposed sequence of
  capability IDs is authorized by that compiled set.
- Keep candidate origin separate from authorization stage. Suggested values
  include `UserEntered`, `AutomaticSimplify`, and `DisplayedSuggestion`; Auto
  Simplify should compile to the same multi-step authorization policy as Check
  Result rather than gaining a more powerful stage.
- Replace string-based backend authorization with typed rule/capability checks.
- Characterize unknown IDs, dependency failures, and disabled prerequisites as
  structured errors rather than silently filtering them.

**Checkpoint:** existing Profile state, including `check_result_rule_ids`, is
still honored, but all consumers can query one compiled capability view.

### Phase 3: introduce `ProfileProofPlan.authorize` as a compatibility facade

- Implement the proposed request/result API and the final plan-invariant
  validator.
- Allow the request to carry candidate-origin metadata for diagnostics and
  telemetry, while forbidding that metadata from widening the compiled
  capability set.
- Initially delegate to existing semantic checkers and `AxiomSearch` in the
  current priority order; do not rewrite every normalizer in this phase.
- Convert existing `trace_summary` results into `AuthorizedPlan` at this one
  boundary.
- Return structured rejection when no local Profile route exists.
- Add parity tests comparing the old entry points with the new facade across
  positive, negative, structurally different, and disabled cases.

**Checkpoint:** the new authorizer is available and behavior-compatible, but
old UI callers have not yet been deleted.

### Phase 4: migrate suggestions and One Step

- Make `AxiomsBox` request authorized suggestions instead of manually building
  trig, calculus-cleanup, and argument-normalization traces.
- Move `simplify_scalar_products` out of `TrigRewrite` into the focused
  arithmetic-normalization provider. Register its precise scalar/sign grammar,
  required Profile capabilities, and certificate strategy in the catalog; do
  not replace it with expression-specific double-negative or trigonometric
  recognizers.
- Make One Step validation call `ProfileProofPlan.authorize` with `Manual`.
- Preserve the distinction between a single visible conceptual operation and
  its allowed cleanup.
- Enforce the compiled One Step usage allowance against the primitive steps in
  the returned plan. A single-distribution Profile must reject a plan with two
  distribution occurrences even when the endpoints are polynomial-equivalent.
- Treat complete polynomial expansion as its own optional conceptual
  capability rather than inferring permission from the Algebra level or a
  hard-coded `allow_polynomial_expansion` boolean.
- Ensure every displayed suggestion already carries the exact plan that will
  be inserted; clicking its arrow must not reconstruct a different trace.
- Remove the migrated manual `trace_summary` constructors from `AxiomsBox`.
- Route function-valued derivative suggestions through the same calculus
  provider and authorizer as explicit `diff(body, variable)` expressions.
- Keep `taylor_derivatives(f, n) in body` as capture-safe syntax lowering to
  ordinary nested `let`s whose final continuation is exactly `body`. Both
  parser paths must agree, and the surface form must never lower through
  `Seq`. Authorize each mathematical derivative transition inside that result;
  do not introduce a Taylor-only proof tactic or a monolithic proof step for
  the whole generated program.
- Decide how to expose those nested derivative bindings as visible teaching
  steps without changing their lexical scope. Prefer a general evaluator pause
  or staged-desugaring mechanism over Taylor-specific sequencing. Preserve the
  source token IDs if the surface sugar remains visible after lowering, so a
  valid `in` is not rendered as an orphan/error token.

**Checkpoint:** Suggestions and One Step share the authorizer and still produce
the same displayed targets, replacement behavior, and exported steps.

### Phase 5: migrate Check Result, verdicts, and Replace

- Replace `ProofSearchBackend.local_profile_trace` orchestration with
  `ProfileProofPlan.authorize(... MultiStepCheck ...)`.
- Store `AuthorizedPlan` only for `ProfileValid`; model the equivalence fallback
  result separately and stop creating a collapsed macro trace when no local
  route exists.
- Make Replace accept an `AuthorizedPlan`, not an arbitrary target plus
  optional trace.
- Invalidate or reauthorize a cached plan if Profile controls, source, target,
  or selection identity change before Replace is clicked.
- Preserve the current rule: Rocq success plus no authorized plan is
  `EquivalentOutsideProfile`, never `ProfileValid`.
- Compile Check Result closure budgets from the active Profile. Search may use
  several commutations or distributions only when the corresponding usage
  allowance permits them, and the resulting plan must enumerate those uses.
- Migrate Auto Simplify together with Check Result. Its debounced Algebrite
  result is only a target proposal; pass that target to the same authorizer,
  cache the same `AuthorizedPlan`, and enforce the same Replace rules.
- Include selection identity, source, target, effective Profile fingerprint,
  and relevant program alignment in cache validation. Debounce timing must not
  affect authorization or permit reuse of a plan for a different term.

**Checkpoint:** Suggestions, One Step, Check Result, and Replace pass the same
typed plan through the UI, and Auto Simplify is merely another candidate
origin. The old local-trace orchestrator is unused.

### Phase 6: make Rocq certification and derivation export plan-driven

- Attach typed certificate evidence/strategy to authorized plans.
- Generate Check Result replay from the plan rather than recompiling a
  subject-wide tactic policy in `ProofSearchBackend`.
- Make `CoqProofExport` dispatch on typed certificate strategy and recorded
  steps rather than `justification`, `group_name`, or broad default tactic
  groups.
- Keep broad mathematical equivalence generation in a separate API and type.
- Migrate specialized calculus export without allowing derivative tactics to
  expand the authorizing capability set.
- Make the first program-structure boundary explicit: certify each selected
  mathematical expression transition with its own plan/theorem, while keeping
  evaluator-only `let`, substitution, application, sequence, and Taylor
  lowering steps non-exportable (or separately evidenced) until program-level
  Rocq semantics are designed.
- Do not invent a whole-program theorem by erasing binders or lowering away
  scopes. Any future program-level export must preserve binding, capture
  avoidance, evaluation order, and the relationship between structural and
  mathematical steps.
- Preserve minimal imports and measure generated-program size after each
  certificate migration.

**Checkpoint:** all Profile-valid Rocq checks and exported written steps consume
the same plan; certification code cannot create authorization.

### Phase 7: derive normalizers and remove separate multi-step Profile state

- Complete the normalizer-contract audit above one normalizer at a time.
- For each proven-conservative normalizer, derive availability from the
  compiled primitive capability set and semantic side conditions.
- Make the normalizer emit the same plan shape and authorizing primitive IDs as
  ordinary search; its compact tactic is only the certificate strategy.
- Replace the current level-derived `allow_polynomial_expansion` switch with
  catalog/Profile policy: primitive distribution has its own usage allowance,
  while complete polynomial expansion is a separate optional macro capability.
- Provide data-only presets such as introductory expansion (single
  distribution, no expansion macro) and standard Algebra (expansion macro
  enabled), without expression-specific FOIL modes.
- If a normalizer cannot be conservatively derived, expose it as a genuine
  precisely defined teacher-facing capability or leave it unavailable.
- Remove `check_result_rule_ids`, `check_result_rule_enabled`, and the related
  ProfileBoard rule overrides only after every current caller has migrated.
- Remove **Allowed multi-step methods** from the Profile UI in the same final
  state-removal commit, not before.

**Checkpoint:** the Profile contains allowed step operations and automatic
simplification; Check Result methods are derived, not independently enabled.

### Phase 8: custom profile definitions and noncommutative proof of concept

- Introduce a serializable level definition containing parents, capability
  overrides, cleanup defaults, and presentation metadata.
- Validate unknown capability IDs, dependency failures, conflicts, and cycles.
- Resolve multiple-parent inheritance deterministically.
- Show inherited, locally enabled, and locally disabled states distinctly.
- Assemble the noncommutative proof-of-concept level only through catalog
  configuration.
- Run positive, negative, and disabled tests in One Step and Check Result and
  inspect every certificate for accidental commutative tactics.

**Checkpoint:** a branching custom level demonstrates that the shared plan is
actually capability-driven rather than hard-coded to the current subject line.

### Phase 9: teacher-facing Math Mode Builder and rewrite authoring

Build a separate **Math Mode Builder** for teachers rather than making the
existing dense Profile pane carry both authoring and debugging responsibilities.
The builder should edit the serializable level definition introduced in Phase
8 and compile through the same catalog/Profile path as built-in levels.

The initial mode-building workflow should let a teacher:

- name and describe a mode and choose one or more parent modes;
- browse inherited operations grouped by mathematical topic rather than by
  implementation module;
- enable, disable, or locally override operations and cleanup defaults;
- choose stage usage such as unavailable, once, or bounded/repeated where the
  operation contract supports it;
- preview the effective inherited rules, dependencies, conflicts, and Rocq
  support before saving;
- try representative source/target expressions in a small validation
  playground; and
- save, duplicate, import, or export a reusable mode definition without
  changing the built-in profiles.

Keep the current Profile pane as an expert/debug inspector during this phase.
It may show stable rule IDs, compiled inheritance, and certificate details,
while the teacher builder uses mathematical names and concise explanations.
Both views must edit or inspect the same underlying definition; there must not
be a second UI-only profile model.

Add a constrained **Add rewrite** workflow backed by the shared rule catalog.
A teacher-authored rewrite definition should contain at least:

- a stable ID, display name, mathematical category, and description;
- source and target patterns with explicit metavariables;
- allowed direction (forward, backward, or both);
- any type restrictions or semantic side conditions;
- stage availability and default usage policy; and
- an approved Rocq lemma/certificate reference that proves the same schema.

Before installation, validate pattern well-formedness, metavariable
consistency, types/sorts, side conditions, dependencies, and certificate
compatibility. Warn about bidirectional cycles and normalization rules that may
not terminate. Arbitrary teacher-supplied Ltac or tactic source is outside the
first proof of concept: the trust boundary should initially be a registry of
approved Rocq lemmas/certificate adapters. General lemma import can be designed
later as a separately reviewed capability.

Use the four trigonometric sum/difference identities as the first end-to-end
proof of concept:

```text
sin(a + b) <-> sin(a) * cos(b) + cos(a) * sin(b)
sin(a - b) <-> sin(a) * cos(b) - cos(a) * sin(b)
cos(a + b) <-> cos(a) * cos(b) - sin(a) * sin(b)
cos(a - b) <-> cos(a) * cos(b) + sin(a) * sin(b)
```

These must be ordinary data/catalog entries, not four new expression-specific
branches in `TrigRewrite` or `RewriteChecker`. Demonstrate that a teacher can
enable only a subset, choose its allowed direction/usage, and obtain identical
authorization behavior from suggestions, One Step, Check Result, Replace, and
Rocq export. Regression coverage must include structurally different positive
instances, incorrect targets, and disabled/subset profiles.

**Checkpoint:** a teacher can create a readable custom mode and add a
Rocq-certified trig sum/difference rewrite without changing Hazel source code;
all proof consumers respect that exact configuration.

### Phase 10: remove compatibility paths and broad legacy tactic groups

- Delete `RewriteChecker` compatibility aliases and orchestration functions
  only after `rg` shows no production callers.
- Audit adjacent catalog, Profile, stage-plan, and UI data for fields that have
  become constant, duplicated, or unused after migration. Remove their types,
  constructors, serialization entries, display labels, tests, and adapter
  plumbing together; do not leave deprecated metadata mirrored across layers.
- Keep fields that still encode a real teacher-facing choice, search bound,
  semantic prerequisite, or certificate behavior even if their current
  built-in profiles happen to share a value.
- Delete a broad tactic group only after all of its callers use typed plan
  certificates.
- Keep import/export compatibility adapters if saved written steps depend on
  the old JSON shape; version the persisted representation if an adapter is not
  possible.
- Compare unit results, generated programs, browser verdicts, Replace behavior,
  program size, and validation latency with the Phase 0 baseline.

**Checkpoint:** one catalog compiler, one authorizer, one durable plan type,
typed certificate consumers, and no independent multi-step permission state.

## Required regression strategy

### Plan invariants

Add reusable assertions for every `AuthorizedPlan`:

- plan source and target equal the request endpoints;
- each step's target equals the following step's source;
- every step records a stable catalog capability ID;
- every authorizing capability is enabled in the compiled Profile stage;
- each capability's recorded use count is within its compiled zero/one/closure
  allowance for that stage;
- all prerequisites and semantic side conditions are present as evidence;
- the plan is nonempty unless reflexivity is represented explicitly;
- certificate strategy does not introduce additional authorizing IDs;
- replay/export consumes the plan endpoints without silently normalizing them
  to a different theorem;
- candidate origin does not alter enabled capabilities, search bounds, or the
  final verdict; and
- a rebased/persisted step cannot retain its plan after any endpoint, selection,
  or Profile-fingerprint mismatch.

### Consumer-parity matrix

For representative goals, assert the expected relationship across consumers:

| Case                                                | Suggestion                                                | One Step                                                                | Check Result / same Auto Simplify target                 | Replace/export                                                        |
| --------------------------------------------------- | --------------------------------------------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------- | --------------------------------------------------------------------- |
| Primitive distribution                              | offered when enabled                                      | valid with one recorded occurrence                                      | valid with identical verdict and plan                   | same recorded distribution plan                                       |
| Complete expansion with distribution limited to one | offer only the next primitive distribution                | invalid unless the expansion macro is enabled                           | valid only if Check Result permits distribution closure | no collapsed or invented macro trace                                  |
| Polynomial expansion macro                          | offered only when enabled                                 | valid as one conceptual operation with derived evidence                 | valid with identical verdict and plan                   | evidence records repeated distribution and exact cleanup requirements |
| FOIL with uncollected target                        | offered/valid according to the expansion policy           | valid only with the macro or sufficient One Step distribution allowance | valid only with sufficient closure allowance            | no implicit collect-like-terms capability                             |
| FOIL with collected target                          | only when its conceptual-step contract permits collection | valid only with required cleanup                                        | valid only with required cleanup and usage allowances   | recorded plan names collection                                        |
| Factoring                                           | offered when factoring is enabled                         | valid                                                                   | valid with identical verdict and plan                   | direction requires factoring, not distribution alone                  |
| Affine reorder/collection                           | cleanup-dependent                                         | stage policy decides                                                    | derived only from enabled AC/collection capabilities    | compact certificate replays authorized plan                           |
| Trig identity plus argument algebra                 | visible identity route                                    | valid at inherited level                                                | valid with exact supporting algebra                     | plan records both capabilities                                        |
| Derivative plus cleanup                             | visible derivative route                                  | valid with enabled cleanup                                              | valid with same route                                   | calculus certificate uses recorded steps                              |
| Function-valued derivative                          | same route as explicit-variable derivative                | valid according to the same calculus/cleanup capabilities               | identical after capture-safe lowering                   | export the mathematical body step, not an invented whole-program proof |

Every row needs at least two structurally different positive examples, an
inequivalent target, and one case for each disabled prerequisite. The reported
bug expression must be accompanied by other shapes so tests do not encode a
single example recognizer.

### Equivalence-boundary tests

- A broad Rocq tactic may prove an equality when no plan exists, but the result
  is `EquivalentOutsideProfile` and has no Replace plan.
- Disabling commutativity, distribution, factoring, collection, or calculus
  cleanup independently removes every plan that requires that capability.
- A zero-use allowance rejects every plan containing the capability; an
  at-most-one allowance rejects two applications; and bounded closure rejects
  a trace that exceeds either its use or state budget.
- Multiple commutations can authorize a reorder only when commutativity closure
  is enabled; the same target is outside Profile when commutativity is disabled
  or limited below the required trace length.
- No `ring`, `lra`, `nra`, subject-wide tactic group, or certificate constructor
  can change a rejection into `ProfileValid`.
- Stale plans are rejected after Profile or candidate edits.

### Persistence and browser checks

- Round-trip existing Written Step trace JSON before changing its durable
  schema; add a versioned fixture if the schema must change.
- Verify the Profile UI no longer shows multi-step controls only after Phase 7.
- Run calibrated browser smoke tests for Algebra, Trigonometry, and Calculus in
  both One Step and Check Result, including Replace and the stored derivation
  justification.
- Repeat selected Check Result cases through Auto Simplify and assert identical
  Profile verdicts, plan routes, Replace availability, and disabled-rule
  behavior. Browser timing only verifies the debounce/UI plumbing.
- Exercise function-valued `diff` and scoped
  `taylor_derivatives(f, n) in body` with a continuation that calls several
  generated derivatives, binder-name collisions, edits before and after the
  selected subexpression, and Profile changes. Compatible ordinary steps may
  persist; stale proof plans must not. A collision policy must keep generated
  names both capture-safe and predictably referenceable from the continuation.
- Verify both parser paths lower scoped Taylor syntax to the same ordinary
  nested `let` structure, with the user continuation inside the innermost
  binding and no surrounding `Seq`, synthetic parentheses, or semicolon.
  Verify each derivative body remains independently step-able. Do not require
  a whole-program Rocq export until that evidence model is specified.
- Capture the exact JSCoq program for at least one primitive replay, one derived
  normalizer, one calculus plan, and one outside-Profile equivalence result.

### Verification commands

At each implementation phase:

```text
./run_tests test 'RewriteChecker' -q
make
```

Run the focused test subset during iteration and the full `make` required by
this repository after every code or documentation edit. Browser checks are
final evidence for UI plumbing, not a substitute for the invariant and
contract tests.

## Implementation guardrails

- Do not remove `check_result_rule_ids` or hide its UI before the derived
  normalizer contracts and replacement authorization path exist.
- Do not move all of `RewriteChecker` into a differently named large module.
  Extract data, utilities, semantic providers, orchestration, and certification
  along actual dependency boundaries.
- Do not add expression-specific recognizers for reported examples.
- Do not let a certificate tactic serve as the test that a transition is
  allowed.
- Preserve route priority until a separately reviewed pedagogical policy
  intentionally changes it.
- Do not require the generic BFS to reproduce a normalizer's potentially large
  primitive path at runtime; a compact semantic witness is acceptable if its
  contract proves that such a path uses only enabled capabilities.
- Keep outside-Profile equivalence useful but mechanically unable to return an
  `AuthorizedPlan`.

## Acceptance criteria

- `RewriteChecker` no longer owns durable proof-plan types or system-wide
  Profile orchestration.
- `AxiomSearch` no longer imports `RewriteChecker` solely for shared syntax
  helpers or trace types.
- One compiled `(profile, stage)` capability set is the source of authorization
  for every consumer.
- The compiled stage policy expresses zero, one, or bounded multiple uses of a
  primitive capability, and every authorized plan is checked against it.
- A custom level can branch from Algebra without inheriting Trigonometry.
- A custom profile can disable multiplication commutativity while retaining
  multiplication associativity.
- A teacher-facing builder can create and preview a custom mode without
  exposing the current expert-oriented Profile layout or creating a second
  authorization model.
- A structured teacher-authored rewrite becomes one validated catalog entry
  consumed consistently by suggestions, One Step, Check Result, Replace, and
  Rocq export.
- The trig sum/difference proof of concept supports independently enabling the
  four identities and rejects disabled identities, malformed patterns,
  incorrect targets, and mismatched certificate schemas.
- Teacher rewrite authoring cannot inject arbitrary Rocq tactic text in the
  initial trusted design.
- One Step, Check Result, cleanup, and Rocq export agree on the effective
  capability set.
- One Step can expose a single distribution while Check Result uses a recorded
  sequence of distributions; complete polynomial expansion is enabled as a
  separate catalog operation rather than a hard-coded FOIL or level mode.
- Suggestions, One Step, Check Result, Replace, and Rocq export consume the
  same catalog-derived proof plan rather than constructing parallel traces.
- Auto Simplify remains a debounced target producer and receives the same
  verdict and `AuthorizedPlan` as Check Result for the same source, target, and
  Profile.
- Check Result normalizers are derived accelerators for enabled primitive
  capabilities, not independent or permanently enabled Profile permissions.
- The Profile UI does not expose a separate multi-step-method section once all
  current methods are safely derived from the shared plan.
- No disabled rule is recovered by a broader tactic or certificate.
- Each `Valid` verdict exposes an enabled authorizing route.
- `EquivalentOutsideProfile` carries no replaceable plan.
- Replace inserts the exact previously authorized plan and invalidates stale
  candidates.
- Rocq search/replay and full derivation export consume typed certificate
  strategies rather than authorization strings or subject-wide fallbacks.
- Function-valued `diff` lowers to the same derivative semantics and Profile
  capabilities as explicit-variable differentiation.
- Scoped Taylor teaching syntax lowers to genuine nested `let` scope (never a
  disguised sequence); its individual mathematical derivative steps use the
  shared authorizer rather than a Taylor-specific proof shortcut.
- Persistent UI steps and cached proof plans have separate validity checks, so
  program edits can preserve compatible work without retaining stale
  authorization.
- Until program-level Rocq semantics are designed, selected mathematical
  expression steps may export independently while evaluator-only program
  structure remains explicitly non-exportable.
- Existing Written Steps remain readable or have an explicit versioned
  migration.
- Structurally different positive, negative, and disabled cases cover every
  search change.
- Existing Algebra, Trigonometry, and Calculus behavior and performance do not
  regress.

## Decisions to revisit before implementation

- Whether a custom level may override inherited defaults directly or must
  inherit from a smaller capability base than the current Algebra level.
- Whether user-authored levels are stored per exercise, per instructor, or as
  reusable exported profile definitions.
- Whether teacher-authored rewrites are embedded in a mode definition or kept
  in a separately versioned rewrite library referenced by multiple modes.
- How approved Rocq lemma/certificate references are registered, versioned,
  and migrated when Hazel or Rocq libraries change.
- What authoring affordance best represents typed metavariables and side
  conditions without making the teacher pane look like an internal AST editor.
- Whether a later trusted workflow may import a newly proved Rocq lemma, and
  how that proof is checked before its rewrite becomes selectable.
- How dependency conflicts are presented when a macro capability subsumes a
  disabled primitive capability.
- Whether full polynomial expansion is a named Manual macro, a derived
  MultiStepCheck route, or both with distinct contracts.
- Whether non-exportable evaluator steps belong in `AuthorizedPlan` or remain a
  separate written-step result type.
- Whether candidate origin belongs in durable plan metadata or only in the
  authorization request and UI diagnostics.
- What exact structural identity is sufficient to rebase an authorized plan
  after a surrounding program edit, as distinct from rebasing an ordinary UI
  step.
- Whether program-level Rocq export should compose expression-level theorems or
  introduce semantics for Hazel `let`, functions, sequence, and generated
  Taylor bindings. The initial implementation should not invent a monolithic
  theorem by erasing this structure.
- Whether scoped Taylor sugar should remain eagerly lowered or use a general
  staged-desugaring/evaluator-pause facility so learners can see and step each
  generated binding before ordinary substitution removes it.
- How generated derivative names are exported to the continuation when a name
  such as `f_deriv_1` already occurs in the differentiated closure or outer
  scope. Freshening alone is hygienic but not usable unless the continuation
  can predict or bind the fresh names; consider explicit pattern/basename
  syntax rather than hidden fallback names.
- Whether the persisted trace schema can remain byte-for-byte compatible when
  types move into `ProofTrace.re`.
- Whether any current multi-step normalizer represents a genuinely irreducible
  teacher-facing capability rather than a derived search optimization.
- What performance threshold should fail CI for generated-program size or
  JSCoq validation latency, given browser timing variability.
- Which noncommutative certificate mechanism is sufficient for the first proof
  of concept.
