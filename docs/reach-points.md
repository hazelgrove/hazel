# Reach points (reachability & dead-code analysis)

A **reach point** is a marker you attach to any expression in a Hazel program.
It asks the SMT solver:

> For what inputs does evaluation reach this node — or is it **unreachable**
> (dead code)?

It is the reachability half of the "test input generation" feature. Its sibling,
the **TestGen** refractor, answers a different question ("what inputs make *this
boolean expression* true?"). Both share the same SMT machinery; this document is
about Reach.

> New to the area? Read this top-to-bottom once. The two non-obvious ideas are
> (1) the analysis runs in `mk_info`, which is the only place a projector has
> the *whole-program* statics map, and (2) "unreachable" is sound but
> "reachable" is a per-function witness, not a whole-program proof.

## What the user sees

- Add it via the context menu **"Find reaching inputs"** on any expression, or
  by typing the `^^reach(...)` trigger. It's a *refractor* (like Probe and
  Statics), so the underlying syntax stays visible and editable.
- An offside decoration shows the **symbolic path condition** and a 🎯 button.
- Clicking 🎯 solves and shows a result pill:
  - **reached when x = 3, y = -1** — a concrete input that reaches the node.
  - **unreachable — dead code** — no input reaches it.
  - **unknown** — the solver couldn't decide, or the path used a construct we
    can't fully model (see *Soundness* below).
- A small **group chip** (• = solo) merges reach points: put several in the same
  group to ask for one input that reaches *all* of them (or "incompatible").

## Semantics

The reach condition of a node is the conjunction of everything that must hold to
get there, collected on the path from the program root down to the node:

- **`if` guards** — `+cond` in the then-branch, `¬cond` in the else-branch.
- **`match`/`case` arm conditions** — to reach arm *i*, the scrutinee must not
  match arms `0..i-1` and must match arm *i*. An earlier wildcard makes a later
  arm dead.
- **`let` bindings in scope** — declared and constrained (`v == def`).

**Inputs** are the variables in scope at the node: function **parameters** and
program-level **free variables**, treated as free SMT variables. This is a
**per-function / intraprocedural** analysis — call sites are *ignored* (a `fun`
on the path just contributes its parameters as inputs).

### What the verdicts mean (and what's sound)

- **unsat ⇒ unreachable (dead code).** Sound, and robust to call sites: if no
  assignment of the in-scope variables satisfies the path condition, then no
  call — whatever arguments it passes — can ever reach the node.
- **sat ⇒ "reached when …".** A true statement *about this code over its
  inputs* — i.e. a test input for that branch. It is **not** a claim that the
  running program reaches it (the actual call might never supply those args).
- **Omitted-guard rule.** When a guard uses something we can't translate, we
  drop it. Dropping only *weakens* the constraint, so:
  - finding **unsat** is still sound (the real, more-constrained set is also
    empty) → we still report dead code;
  - finding **sat after dropping anything** is reported as **unknown**, never a
    confirmed witness.

So the verdict that carries a guarantee is **unreachable**; treat "reached" as a
witness/test input.

### Merging (groups)

Each reach point carries a `group: int` (0 = solo). Points with the same group
`N ≥ 1` are **conjoined**: solving asks for one input reaching all of them in a
single execution, or reports **incompatible** (e.g. two points in mutually
exclusive branches). Variables are unified by **binder identity** (`origins`),
not by name: points that see the same binder share its variable, while
same-named variables from different scopes are renamed apart (`x!1`, `x!2`, …
— `!` cannot appear in a Hazel identifier, so the fresh names never collide
with source names).

## Architecture & data flow

The hard part is that a projector's `view`/`update` only sees its **own node**'s
`info`, but reachability needs the **whole program**. The whole-program statics
map *is* available in one place — `ProjectorInfo.mk_info` — so the analysis runs
there and its result is stashed in `info`.

```
ProjectorInfo.mk_info (has the whole-program Statics.Map.t)
  └─ for a Reach refractor: info.reach = resolve_reach lookup, else Reach.analyze(id, map)
        Reach.analyze(target_id, map):
          walk the node's ANCESTOR chain (from the statics map's user_terms),
          collecting signed if-guards + match-arm conditions + let-scope,
          inlining in-scope let-bound function calls, resolving input sorts
        → Reach.t { guards; lets; var_sorts; inputs; origins; complete }

ReachProjView (web) on 🎯 click:
  Reach.smtlib2(info.reach)  → (SMT-LIB2 string, complete)   [web-free]
  Z3Wasm.solve(script, k)    → raw solver text               [z3-solver WASM]
  TestGen.parse_model(text)  → outcome                       [web-free, shared]
  Reach.interpret(~complete, ~inputs, outcome) → reach verdict
```

Key points:

- **`Reach.analyze` walks ancestors, not a fresh tree pass.** `Info.exp.ancestors`
  gives the path root→node; for each ancestor we look up its `user_term` and add
  the guard/binding for whichever child the path goes through (using
  `Exp.find_by_id` to decide which branch/arm contains the target).
- **Groups are resolved in the `mk_info` caller**, which has the whole refractor
  set. `ProjectorInfo.resolve_reach` reads each Reach refractor's group from its
  serialized model and, per group, computes the merged (conjoined) condition via
  `Reach.merge`; `mk_info` takes a `~reach_map` so each point gets its own (solo)
  or its group's (merged) condition. The web `RefractorView` builds the map; other
  `mk_info` callers pass an empty one.
- **Solving is async and on-demand** (the WASM solver returns a Promise), so the
  projector `update` only stores the result; the view kicks off the solve and
  dispatches `SetResult` when it resolves.
- **Backends are shared with TestGen and swappable behind SMT-LIB2 text:** the
  `z3-solver` WASM package in the browser/node (`src/web/.../Z3Wasm.re`), and the
  system `z3` binary natively (`src/testgenZ3/Z3Native.re`, used by tests/CLI).
  Both parse output through the shared `TestGen.parse_model`.

## Supported fragment

The translator (`ConstraintGen`) and the path walk (`Reach`) handle:

- literals (`Int`/`SInt`/`Nat`→`Int`, `Float`→`Real`, `Bool`, `String`),
  variables, the numeric/bool/string operators, `if`→`ite`;
- `let` (including tuple-pattern bindings of tuple values);
- `match`/`case` with **literal and wildcard** patterns (including tuple
  patterns), desugared to nested `ite` / arm conditions;
- application of **in-scope let-bound functions**, inlined by beta-reduction
  (`Reach.inline_aps` + `subst`); recursive/unknown calls are left in place →
  incomplete;
- the integer **modulo** builtins (`int_mod`/`sint_mod`/`nat_mod`) → SMT `(mod …)`;
- **tuples** component-wise (equality `(a,b)==(c,d)` → `(and (= a c) (= b d))`,
  tuple lets/params/match patterns) — for tuples of base-typed components.

Anything else (constructor/ADT patterns, lists, higher-order/recursive
functions, `float_mod`, …) is **unsupported** → marks the result incomplete.
Per the omitted-guard rule that means "unknown" for sat, while "unreachable"
stays sound.

### Variable sort resolution (a subtlety worth knowing)

To declare an input in SMT we need its base sort. Because inlining isn't
reflected in the statics map (and free vars can be `Unknown`), `Reach` resolves a
variable's sort in this order: **synthesized type → expected type (`ana`) →
infer from operator usage** in the (inlined) guards (`a * 0` ⇒ Int, `b && c` ⇒
Bool, `n == 0` ⇒ the literal's sort). If you add an operator/builtin, make sure
`infer_sort` (and `ConstraintGen`) know about it, or operand sorts may fail to
resolve and the guard will be dropped (→ unknown).

## Key files

| File | Role |
|---|---|
| `src/haz3lcore/testgen/Reach.re` | The analysis: `analyze`, `merge`, `smtlib2`, `interpret`, the ancestor `step`, pattern/inlining helpers |
| `src/haz3lcore/testgen/ConstraintGen.re` | `Exp.t` → SMT-LIB2 expression (operators, `ite`, match, tuple equality, builtins). Web-free, no Z3 dep |
| `src/haz3lcore/testgen/TestGen.re` | Shared SMT assembly, `outcome` type, `parse_model`; the TestGen (predicate) sibling |
| `src/haz3lcore/projectors/ProjectorInfo.re` | `mk_info` populates `info.reach`; `resolve_reach`/`reach_group_count` handle groups |
| `src/haz3lcore/projectors/ProjectorBase.re` | `info.reach`, `info.reach_group_count` fields |
| `src/haz3lcore/projectors/implementations/ReachProj.re` | Refractor logic (model `{group, result}`, actions) |
| `src/web/projectors/ReachProjView.re` | Offside view: group chip, 🎯 button, path condition, result pill |
| `src/web/projectors/Z3Wasm.re` | Browser/node solver backend (z3-solver WASM FFI) |
| `src/testgenZ3/Z3Native.re` | Native solver backend (system `z3` binary) — tests/CLI |
| `src/language/ProjectorKind.re` | `Reach` kind + `refractors` list |
| `test/Test_Reach.re` | Path-condition + end-to-end solve tests (native-only; z3-guarded) |

## How to extend

- **New operator / builtin:** add it to `ConstraintGen.smt_of_exp` (and, if it
  determines operand sorts, to `Reach.infer_sort`). See `int_mod` for a builtin
  example.
- **New control-flow construct on the path** (something that guards
  reachability): add a case to `Reach.step` that contributes the right guard(s)
  for the child the path goes through. See the `If` and `Match` cases.
- **New pattern form:** extend `Reach.bind_pattern` (binding positions: `let`,
  params, inlining) and/or `Reach.pat_matches` (match conditions). Keep
  unsupported forms returning `None`/`PUnknown` so they degrade to incomplete.
- **New solver backend:** consume the SMT-LIB2 from `Reach.smtlib2` and feed the
  raw output to `TestGen.parse_model`.

When you bound coverage (drop a guard, cap something), make sure it flows into
the `complete` flag so a sat result is honestly reported as "unknown".

## Limitations / future work

- Constructor/ADT patterns and lists would need SMT datatypes (not done).
- Recursive and higher-order function calls aren't inlined → incomplete.
- Shadowed variables surface with synthetic `x!k` names in solutions; a real
  UI design (e.g. hover/link to the binder) is a future follow-up.
- "Incompatible" groups don't yet pinpoint *which* points conflict (no unsat
  core).
- `float_mod` and many other builtins are unmapped.
