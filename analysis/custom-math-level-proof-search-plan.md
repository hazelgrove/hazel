# Custom Math Levels and Profile-Safe Proof Search

## Status

Deferred architecture work. This document records the intended direction; it
does not authorize implementation as part of the current audit-fix work.

## Goal

Allow an instructor to define a math level by composing existing mathematical
capabilities instead of selecting a hard-coded Hazel mode. The resulting level
must behave consistently in One Step, Check Result, automatic cleanup, proof
traces, and Rocq validation.

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
    ├── Functions/Lists
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

### Proof-plan intermediate representation

Hazel should produce a typed proof plan before asking Rocq to validate:

```text
ProofStep {
  capability_id,
  source,
  target,
  occurrence,
  evidence,
  certificate_strategy
}
```

Primitive rules can use rewrite lemmas. Macro capabilities can use typed
strategies such as affine, polynomial, derivative, or trigonometric
certificates, but only after their Hazel-side contracts authorize the exact
transition.

## Noncommutative proof of concept

Create a test-only or disabled experimental profile assembled from catalog
capabilities. Do not add expression-specific logic.

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

## Work phases

### 1. Audit Profile-valid entry points

- Enumerate every path capable of returning `Valid`.
- Record whether it starts from an enabled Hazel trace or from Rocq tactic
  success.
- Add an invariant test that every Profile-valid result names its authorizing
  catalog capabilities.
- Keep equivalence fallback mechanically incapable of returning `Valid`.

### 2. Formalize capability contracts

- Define the exact affine-expression grammar and normalization relation.
- Separate authorization metadata from tactic/certificate implementation.
- Replace remaining tactic-string authority with typed certificate strategies.
- Preserve existing compact certificates and performance.

### 3. Compile all stages from the catalog

- Derive One Step candidates from enabled visible capabilities.
- Derive Check Result search transitions from enabled capabilities.
- Derive cleanup passes and prerequisites from the same profile.
- Derive Rocq dependencies from the selected proof plan rather than loading a
  fixed subject-wide tactic group.

### 4. Custom profile definitions

- Introduce a serializable level definition containing parents, capability
  overrides, cleanup defaults, and presentation metadata.
- Validate unknown capability IDs, dependency failures, conflicts, and cycles.
- Resolve multiple-parent inheritance deterministically.
- Show inherited, locally enabled, and locally disabled states distinctly.

### 5. Noncommutative experiment

- Assemble the proof-of-concept level only through catalog configuration.
- Run positive, negative, and profile-disabled tests in One Step and Check
  Result.
- Inspect every generated Rocq certificate for accidental commutative tactics.
- Use the results to identify catalog assumptions that are still hard-coded to
  ordinary commutative algebra.

### 6. Remove redundant legacy tactic groups

- Delete a broad group only after all of its callers use typed proof plans.
- Keep compatibility wrappers temporarily if exports or saved settings depend
  on stable IDs.
- Compare generated-program size and validation latency before and after each
  removal.

## Acceptance criteria

- A custom level can branch from Algebra without inheriting Trigonometry.
- A custom profile can disable multiplication commutativity while retaining
  multiplication associativity.
- One Step, Check Result, cleanup, and Rocq export agree on the effective
  capability set.
- No disabled rule is recovered by a broader tactic or certificate.
- Each `Valid` verdict exposes an enabled authorizing route.
- Structurally different positive, negative, and disabled cases cover every
  search change.
- Existing Algebra, Trigonometry, and Calculus behavior and performance do not
  regress.

## Decisions to revisit before implementation

- Whether a custom level may override inherited defaults directly or must
  inherit from a smaller capability base than the current Algebra level.
- Whether user-authored levels are stored per exercise, per instructor, or as
  reusable exported profile definitions.
- How dependency conflicts are presented when a macro capability subsumes a
  disabled primitive capability.
- Which noncommutative certificate mechanism is sufficient for the first proof
  of concept.
