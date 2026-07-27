# Step Persistence When the Program Changes

## Goal

When the source program changes, preserve as much of the existing derivation as
is still meaningful. Hazel should replay the existing steps against the edited
program, retain the longest unambiguously valid prefix, and stop before it would
silently apply a step to the wrong expression.

This is different from serialization persistence. Hazel already saves and
loads step chains. This plan concerns **edit reconciliation**: keeping a useful
step chain while its input program is changing.

## Scope decision

Implement this by extending the machinery that arithmetic steps already use.
Do not introduce a new general anchoring framework, reconciliation subsystem,
or parallel representation of step history unless concrete tests demonstrate
that the existing mechanism cannot support a required case.

The initial goal is not to preserve every possible Hazel step. It is to make
the newer step types that naturally fit arithmetic's replay model participate
in that same model:

- persist the operation information already recorded by the step;
- relocate its target with the existing expression-plus-occurrence machinery;
- recompute or revalidate the operation against the edited expression;
- recursively retain subsequent steps through `StepperBase`; and
- fall back to `MissingStep` at the first step that cannot be replayed.

Ambiguous or structurally difficult edits may conservatively truncate the
chain. Supporting fewer cases safely is preferable to adding speculative
plumbing or silently moving a step to the wrong expression.

## Checkpoint

The work immediately preceding this plan was committed and pushed as:

- `b875b48eb5` — `Refresh open math checks when settings change`

## Implemented initial slice

The first implementation deliberately reuses the existing arithmetic replay
contract rather than adding a new correspondence system:

- arithmetic `SingleStep` behavior is covered directly by edit-recalculation
  tests;
- recursive recalculation preserves the longest replayable prefix and replaces
  the first incompatible step with `MissingStep`;
- `AxiomStep`, `WrittenStep`, and `AlgebriteStep` are covered as compatible
  users of the existing expression-plus-occurrence lookup;
- a shared `ProofHacks.replace_nth_exp` helper removes the duplicated local
  lookup-and-replacement code;
- local reparenthesization and Auto Simplify results can survive unrelated
  edits around their original target; and
- reparenthesization that immediately evaluates a selected child remains
  conservative, because its stored child ID cannot safely be relocated after
  the surrounding source changes.

This does **not** infer that an edited target is a new version of the old target.
For example, changing `1 + 2` to `1 + 4` invalidates a stored constant-addition
step rather than guessing a replacement operation. Likewise, edits that shift
indistinguishable duplicate occurrences remain outside this initial slice.

Persisted targets now prefer their stable expression ID and use structural
fallback only when exactly one matching candidate exists. This prevents a
newly inserted identical expression from stealing an old step. Full Profile
reauthorization of historical Written, Algebrite, and Auto Simplify results is
still deferred to the shared profile-proof-plan work.

## What arithmetic steps do today

The existing arithmetic path already provides the beginning of the desired
architecture.

1. [`StepperView.re`](../src/web/view/StepperView.re) recalculates the same
   persisted root step against the newly elaborated source expression.
2. [`StepperBase.re`](../src/web/app/editors/stepper/StepperBase.re) compares the
   current expression with the expression previously saved at each step.
3. Each existing step kind is asked to recalculate itself against that current
   expression.
4. If it succeeds, `StepperBase` recursively recalculates the stored next step
   against the newly produced expression. This naturally preserves a replayable
   prefix of a multi-step derivation.
5. If a step cannot be recalculated, `StepperBase` replaces that point with a
   fresh `MissingStep`; the old suffix is no longer attached.

For ordinary arithmetic/evaluation steps, [`SingleStep.re`](../src/web/app/editors/stepper/SingleStep.re)
stores an [`EvaluatorStep`](../src/language/dynamics/stepper/EvaluatorStep.re)
recipe rather than only its old output. The persistent recipe contains:

- the expression that was stepped; and
- its occurrence index in the full expression.

`EvaluatorStep.refresh_step` finds the corresponding expression in the edited
input, checks that the same evaluator operation is still available there, and
then recomputes the result. Thus a step such as constant arithmetic can survive
an upstream edit when its target can still be located and the same operation is
still applicable.

This behavior is valuable and should remain the basis of the implementation.
The project does not need a second, parallel persistence engine.

## Current limitations

### 1. Location is mostly expression plus occurrence index

`EvaluatorStep`, `AxiomStep`, `AlgebriteStep`, and `WrittenStep` all depend in
some form on `ProofHacks.nth_exp(old_expression, occurrence, current_expression)`.
This works for simple edits but can retarget incorrectly or fail when an edit:

- inserts or removes an earlier matching expression;
- creates two structurally identical candidates;
- changes surrounding associativity or parentheses; or
- changes a selected subtree while leaving a similar subtree elsewhere.

Incorrectly preserving a step is worse than conservatively dropping it.

### 2. Step kinds preserve different amounts of intent

- `SingleStep` stores a semantic evaluator-step recipe and recomputes it.
- `AxiomStep` stores a rule name, direction, and target location, then reapplies
  the rule. This is fairly replayable once the target is found.
- `AlgebriteStep` and `WrittenStep` primarily store the old replacement
  expression. They can relocate the source target, but they do not generally
  recompute the target from a shared proof plan.
- `ReparenthesizeStep` and `AutoSimplifyStep` currently require the entire input
  expression to equal the original expression, so even an unrelated upstream
  edit invalidates them.

### 3. Failure is only represented as fallback

The recursive calculation distinguishes success from failure internally, but
does not say why reconciliation failed. It cannot distinguish:

- target no longer exists;
- target is ambiguous;
- rule is no longer applicable;
- active Profile no longer authorizes the step; or
- a stored certificate no longer validates.

That makes both debugging and a future user-facing “steps preserved” message
difficult.

### 4. There are few direct edit-reconciliation tests

Current tests cover step execution and serialization metadata, but they do not
systematically calculate a chain, change its source expression, recalculate the
same chain, and assert exactly which prefix survives.

## Proposed contract

On every program edit, Hazel should:

1. Reconcile steps from the root in order.
2. Locate each step's intended target conservatively in the current expression.
3. Replay the stored operation, not blindly reuse the old output.
4. Revalidate the operation under the current context, settings, math level,
   automation stage, and Profile.
5. Preserve the step and continue only when the target and replay are
   unambiguous.
6. At the first failure, retain the valid prefix and replace the remaining
   derivation with a `MissingStep`.
7. Never silently move a step to one of several equally plausible targets.

For an initial implementation, discarding the invalid suffix from the active
chain is acceptable because normal Hazel undo still provides recovery. A later
version could retain it as a detached suffix for richer UI recovery.

## Design

### A. Use the recursive `calculate_with_level` mechanism unchanged

The recursion in `StepperBase.Stepper.calculate_with_level` already expresses
the correct high-level algorithm. Keep its existing `option`-based contract:
successful calculation preserves the step and recursively calculates its
stored successor; failure replaces that point with `MissingStep`.

Do not add a second reconciliation result type in the initial implementation.
Failure diagnostics can be added later if experience shows they are needed.

### B. Reuse arithmetic's existing target relocation

Use `ProofHacks.nth_exp(at_exp, at_idx, current_exp)` in the same manner as
`EvaluatorStep.refresh_step`, `AxiomStep`, `AlgebriteStep`, and `WrittenStep`
already do. Preserve the existing serialized representation where possible.

Tests must cover shifted and duplicate occurrences. If the existing lookup is
unsafe for a concrete case, fix that helper conservatively or decline to retain
that case. Do not preemptively add a new `StepAnchor` abstraction.

### C. Replay existing intent per compatible step kind

Each persistent step should contain the smallest recipe needed to reproduce the
operation:

| Step kind | Replay intent |
| --- | --- |
| Arithmetic `SingleStep` | No architectural change; use it as the reference implementation. |
| `AxiomStep` | Reuse its stored rule ID, direction, `at_exp`, and `at_idx`; reapply the rule to the relocated current target. |
| `WrittenStep` | When a recorded Profile trace exists, replay or revalidate that trace at its existing relocated target. Otherwise keep the current conservative stored-replacement behavior. |
| `AlgebriteStep` | Retain it only when the existing target relocates and the replacement can be regenerated or revalidated without bypassing the Profile. |
| Reparenthesization | Support only edits for which the existing original/reparenthesized operation can be safely relocated with a small local extension. Otherwise truncate. |
| Auto Simplify | Reuse the recorded source/target or Profile trace where it can be recalculated locally; otherwise truncate. |

This should align with the planned shared profile-proof-plan architecture rather
than inventing another expression-specific proof system.

### D. Preserve Profile correctness

Reconciliation is a fresh authorization check, not a historical exemption.
Math steps must be replayed using only the rules and cleanup capabilities enabled
by the current Profile. If a program or Profile edit disables a required rule,
the derivation stops immediately before that step.

No `ring`, `lra`, Algebrite result, or other broad normalizer may preserve a step
unless its allowed operation plan can be reconstructed from the active catalog
and Profile.

### E. Keep the current UI behavior

The valid prefix remains and a normal missing-step editor appears at the end.
No new status, recovery UI, or detached-suffix model is required for this
feature.

## Implementation phases

### Phase 0: Characterize existing arithmetic behavior

Before changing production behavior, add tests that recalculate the same
arithmetic chain after editing its input:

- an unrelated outer edit preserves a step;
- changing the target so the evaluator operation disappears truncates there;
- a two- or three-step chain preserves its longest replayable prefix; and
- document duplicate matching subexpressions as unsupported until the stored
  persistence recipe can distinguish them without new anchoring machinery.

These tests document the behavior that already works and expose the occurrence-
index boundary.

### Phase 1: Lock down and extend the existing replay path

- Add direct edit/recalculation tests for `EvaluatorStep`/`SingleStep`.
- Keep their persistent format and refresh implementation as the reference.
- Add only small safety improvements to the existing occurrence lookup when a
  failing regression test requires them.

Arithmetic should require little or no production change in this phase.

### Phase 2: Migrate rule-based math steps

- Verify and test the existing `AxiomStep` replay behavior across source edits.
- Extend `WrittenStep` to replay its existing trace instead of only reusing its
  stored replacement when a trace exists.
- Extend `AlgebriteStep` only where regeneration or Profile-proof revalidation
  is already available.
- Add enabled/disabled Profile cases for every migrated class.

### Phase 3: Handle structural and automatic steps

- Attempt a small local replay extension for reparenthesization.
- Reconcile Auto Simplify only through proof information it already records.
- Cover changes in associativity, explicit parentheses, and identical sibling
  expressions.
- Leave cases unsupported when they require a new general correspondence
  system.

### Phase 4: Nested programming constructs and diagnostics

- Test steps beneath `let`, function bodies, applications, and relevant
  induction/forall steppers.
- Preserve binder safety and avoid capture during target relocation.
- Defer user-facing diagnostics and detached-suffix restoration.

## Regression matrix

Each migrated step class needs structurally different positive, negative, and
ambiguous cases.

### Arithmetic

- `1 + 2 + x -> 3 + x`; edit `x` to `y`: preserve.
- `1 + 2 + x -> 3 + x`; edit `2` to `4`: replay as `5 + x`.
- Replace the stepped addition with a variable: truncate at that step.
- Add a second `1 + 2` before the original: do not silently select by shifted
  occurrence alone.

### Multi-step prefix

- Edit only data consumed by step three: preserve steps one and two.
- Edit the input to step one incompatibly: preserve no derived steps.
- Edit unrelated surrounding code: preserve the whole chain.

### Algebra and calculus

- Distribution/FOIL remains replayable after an unrelated edit.
- Disabling distribution or repeated distribution truncates the relevant step.
- A derivative step remains valid after renaming an unrelated free variable.
- A binder rename is either reconciled capture-avoidantly or rejected.

### Structural ambiguity

- Identical siblings, reordered sums, and inserted parentheses.
- A moved subtree with unique ancestor context can be retained.
- Multiple equally plausible targets force truncation.

### Persistence lifecycle

- Save/load a chain, then edit the source and reconcile it.
- Undo and redo an edit around a reconciled chain.
- Change math level or Profile while the chain is open and revalidate the same
  step recipes.

## Likely files

- [`src/web/view/StepperView.re`](../src/web/view/StepperView.re)
- [`src/web/app/editors/stepper/StepperBase.re`](../src/web/app/editors/stepper/StepperBase.re)
- [`src/web/app/editors/stepper/StepInterface.re`](../src/web/app/editors/stepper/StepInterface.re)
- [`src/language/dynamics/stepper/EvaluatorStep.re`](../src/language/dynamics/stepper/EvaluatorStep.re)
- [`src/web/app/editors/stepper/SingleStep.re`](../src/web/app/editors/stepper/SingleStep.re)
- [`src/web/app/editors/stepper/AxiomStep.re`](../src/web/app/editors/stepper/AxiomStep.re)
- [`src/web/app/editors/stepper/AlgebriteStep.re`](../src/web/app/editors/stepper/AlgebriteStep.re)
- [`src/web/app/editors/stepper/WrittenStep.re`](../src/web/app/editors/stepper/WrittenStep.re)
- [`test/evaluator/Test_StepperBase.re`](../test/evaluator/Test_StepperBase.re)
- profile search tests in [`test/Test_RewriteChecker.re`](../test/Test_RewriteChecker.re)

## Recommended first implementation slice

Keep the first PR narrow and use only existing plumbing:

1. Add edit-reconciliation tests for existing arithmetic steps.
2. Verify that `AxiomStep` already retains and recomputes a valid rule step.
3. Extend `WrittenStep` with a recorded trace and the simplest compatible
   `AlgebriteStep` case to follow the same refresh pattern.
4. Confirm that the existing recursion preserves the longest valid prefix and
   truncates at the first incompatible step.
5. Add Profile-disabled and duplicate-expression negative cases.

That gives a demonstrable improvement without a new persistence architecture or
coupling the work to the larger proof-search refactor. Reparenthesization and
Auto Simplify can follow only if they fit the same pattern cleanly.
