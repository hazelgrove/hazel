# Hazel Deriver System

## Overview

The deriver lets students construct natural-deduction-style derivations — typing, evaluation, and logical entailment proofs — inside Hazel. A derivation exercise is rendered as a tree of editors (one editor per deduction node); each node carries a **judgement** (its conclusion) and a **rule** choice. The system verifies each node against the selected rule's spec and reports per-node status (Correct / PartialCorrect / Incorrect / Pending) in real time.

The deriver ships with several built-in **rule sets** the student can target:

- **Propositional Logic** — first-order propositional logic with the usual introduction/elimination rules for `∧`, `∨`, `⇒`, `¬`, `⊤`, `⊥`.
- **AL, ALB, ALF, ALFp, ALFA, RecursiveALFA, GradualALFA** — a family of small toy languages used in EECS 490 (*Programming Languages*) at the University of Michigan. Each adds features to its predecessor (booleans, functions, pairs/sums, fixpoints, recursive types, gradual typing) and each is specified by typing, synthesis/analysis, value, evaluation, and type-validity judgements. An exercise pins one rule set; `RuleImage.to_rule` handles any rule-set-specific remappings (e.g. `T_LetAnn` acquires an extra type-validity premise in `RecursiveALFA`).

Derivations coexist with normal Hazel code in the same project: they live inside a dedicated scratchpad "kind" (`Drv(...)` in `ScratchMode`) and inside exercise modules (`DerivationExercise`). Under the hood a derivation is just a Hazel expression with a `DrvQuote(...)` wrapper that embeds derivation-sort syntax into the enclosing `Exp`-sort context.

## Syntax

### Judgement forms inside a derivation cell

The conclusion of a deduction is written in ALFA-flavored concrete syntax:

```
e \=/ v                       -- evaluation:    e evaluates to v
val e end                     -- value:         e is a value
gamma |- e : tau              -- typing:        e has type tau in gamma
gamma |- e => tau             -- synthesis:     e synthesizes tau
gamma |- e <= tau             -- analysis:      e analyzes against tau
delta |- valid tau end        -- type validity
gamma |- p                    -- propositional: gamma entails p
```

Contexts can use the usual list manipulators:

```
[]              -- empty context
[p1, p2, ...]   -- context literal
gamma_1 @ gamma_2
p :: gamma
```

And propositional connectives:

```
A /\ B            A \/ B            A ==> B           !A           Truth           Falsity
```

See `src/language/derivation/DrvGrammar.re` for the canonical grammar; the full ALFA type system and evaluation semantics are documented in the ALFA reference table used by the associated course materials (external to this repo).

### Embedding derivations into regular Hazel code (quotations)

Derivation-sort syntax is only valid inside a `Drv(_)` editor context. To reference a derivation subterm from regular Hazel code you wrap it in a **quotation** form:

```
of_jdmt       gamma |- e : tau      end        -- quote a judgement
of_ctx        [x : Num, y : Bool]   end        -- quote a context
of_prop       A /\ B                 end        -- quote a proposition
of_alfa_exp   let x = 1 in x + 2     end        -- quote an ALFA expression
of_alfa_typ   Num -> Bool            end        -- quote an ALFA type
of_alfa_pat   (x, y)                 end        -- quote an ALFA pattern
of_alfa_tpat  t                      end        -- quote an ALFA type pattern
```

Each `of_*` form has type `DrvQuoteTy(sort)` in the Hazel type system. Quotations are used by the deriver UI (the verifier needs to parse the derivation node's content back to a `Drv.Exp.t`) and also let exercises construct reusable derivation fragments by binding them with `let`:

```
let ctx_a = of_ctx (a : Num)::[] end in
let tb    = of_alfa_typ (Num -> Num) -> Num end in
/* derivation proving  {ctx_a, b : tb} |- b : tb  via T-Var */
```

## What Works

| Feature                                         | Status |
| ----------------------------------------------- | ------ |
| Tree-of-editors UI with nested deductions       | Works  |
| Rule selection per-node via command palette     | Works  |
| Live verification: Correct / Partial / Incorrect/ Pending | Works |
| Abbreviation nodes (reuse a subtree by index)   | Works  |
| Multiple rule sets (`RuleImage.rule_set`)       | Works  |
| Quotation forms (`of_jdmt`, `of_ctx`, …)        | Works  |
| Explain-this pane: rule spec, premises, tests   | Works  |
| Scratchpad kind: derivation slides             | Works  |
| Exercise kind: graded derivation exercises      | Works  |
| CLI grading report for derivation exercises     | Works  |

## Known Limitations

### `Drv(Jdmt)`, `Drv(Ctx)`, `Drv(Prop)` collapse to `Drv(Exp)` at the mold level

Because tile molding can't yet disambiguate the three "meta" derivation sorts, all three are stored as `Drv(Exp)` in the grammar. Statics reconstructs the true sub-sort on a per-node basis (via `DrvInfo.sort_of`) so the cursor inspector, tooltip, and CSS coloring can still distinguish them. See `src/language/derivation/DrvSort.re` for the note and `src/language/statics/Info.re :refine_sort_from_mold` for the lookup helper used by `Code.re` and `Arms.re`.

### Rules are enumerated, not extensible in-editor

`Rule.t` and `RuleImage.t` are fixed OCaml variants. Adding a new rule requires touching `Rule.re`, `RuleImage.re` (to add it to the appropriate rule sets), and `RuleSpec.re` (to define its premises/conclusion/tests). There is no user-facing rule editor.

### Menhir parser does not support `DrvQuote`

`src/menhirParser/Conversion.re` raises `Failure("DrvQuote not supported")` when asked to convert core `DrvQuote(_)` values back to the menhir AST, because the menhir grammar has no syntax for derivation terms. Exercises therefore cannot be round-tripped through the menhir parser; they are persisted via the regular zipper-serialization path (see `src/web/derivation/examples/*.ml`).

## Architecture

### Sorts

`Sort.Drv(DrvSort.t)` is a single outer sort that covers seven sub-sorts:

| Sub-sort | What it represents                              |
| -------- | ----------------------------------------------- |
| `Jdmt`   | Judgement — the conclusion of a deduction      |
| `Ctx`    | Context — the `Γ` on the left of `⊢`            |
| `Prop`   | Proposition — the `φ` on the right of `⊢`       |
| `Exp`    | ALFA expression — the `e` inside a judgement    |
| `Pat`    | ALFA pattern                                     |
| `Typ`    | ALFA type                                        |
| `TPat`   | ALFA type pattern                                |

`DrvSort.to_string` has two "prefix styles" that should be kept in sync with the naming conventions in the research literature: `Drv*` for meta-level sorts (`DrvJdmt`, `DrvCtx`, `DrvProp`, `DrvPat`, `DrvTPat`) and `ALFA*` for object-language sorts (`ALFAExp`, `ALFATyp`). The cursor inspector uses `DrvSort.to_string_short`, which drops the `Drv` prefix but keeps `ALFA` for readability.

### Grammar layer (`src/language/derivation/`)

The grammar is defined once and then instantiated at two abstraction levels via the functor `DrvGrammar.M`:

- **`Annotated` instance** (`DrvTermBase.re`, via `M_Annotated`): terms tagged with `IdTagged.IdTag.t`, used by the editor, cursor inspector, evaluator, and most of the pipeline.
- **`Id` instance** (`RuleSpec.M_Id`): terms without ids, used inside rule specs so spec-side variables (`e`, `gamma`, `t`) can be compared symbolically.

Key types:

- `DrvGrammar.exp_term` — the full union of judgement / context / prop / ALFA-exp constructors (`Val`, `Eval`, `Entail`, …, `NumLit`, `Neg`, `BinOp`, `If`, `Let`, `Fun`, `Ap`, …).
- `DrvGrammar.pat_term`, `typ_term`, `tpat_term` — ALFA pattern/type/type-pattern vocabulary.
- `DrvGrammar.any_t` — the Σ-type `Exp | Pat | Typ | TPat`, used whenever we need to refer to "any derivation subterm regardless of sort" (e.g. the `term` field of `DrvInfo.t`, the `specced` pair in `RuleVerify`, abbreviation hole contents).

`DrvTerm.{Exp,Pat,Typ,TPat,Any}` provide per-sort utilities (`fresh`, `term_of`, `rep_id`, `cls_of_term`, `map_term`, `eq`, `contains_hole`, `subst`, `glb`, context helpers, …). The `Drv` module is a thin re-export alias.

### Form layer

Derivation concrete-syntax forms live inside `Form.drv_compound_form` (in `src/haz3lcore/lang/Form.re`). They fall into three groups:

1. **Quotation interface forms** (`OfJdmt`, `OfCtx`, `OfProp`, `OfAlfaExp`, `OfAlfaTyp`, `OfAlfaPat`, `OfAlfaTPat`). Each produces out-sort `Exp` with a single body child at the appropriate `Drv(_)` sort. These are what bridge between regular Hazel and the deriver.

2. **Judgement/context/prop/formula forms** (`Val`, `Eval`, `Entail`, `UnaryEntail`, `Consistent`, `MatchedArrow`, `MatchedProd`, `MatchedSum`, `Ctx`, `Cons`, `Concat`, `Valid`, `HasType`, `Syn`, `Ana`, `And`, `Or`, `Impl`, `Not`, plus pretty-print-only `Subst`/`SubstTy`/`Glb`). All have out-sort `Drv(Exp)` because of the remolding-issue sort collapse.

3. **ALFA expression / type / pattern forms** (`Neg`, `Plus`, `Minus`, `Times`, `Lt`, `Gt`, `Eq`, `If`, `Let`, `Fix`, `Fun`, `Case`, `Rule`, `Cast`, `Arrow`, `Prod`, `Sum`, `Rec`, `ApExp`, `ApPat`, `CommaExp`, `CommaPat`, `ParenExp`/`ParenPat`/`ParenTyp`/`ParenProp`). Standard ALFA syntax, restricted versions of their Hazel counterparts.

TyDi autocompletion supports `Drv(_)` sorts natively (see `TyDiForms.Delims.leading`/`infix`/`const_mono`): when the cursor is inside a `Drv(Exp)` context, typing a prefix offers all the derivation-form keywords. Earlier code had `*Fake` form twins that existed only because TyDi's delim helpers didn't understand Drv sorts; those are gone.

### MakeTerm (concrete → abstract)

`src/haz3lcore/lang/MakeTerm.re` handles the parse-ish step that turns a zipper segment into a `Drv.*.t`. The main entry points are `drv_exp`, `drv_pat`, `drv_typ`, and the shared `any` dispatcher. Each case matches on label/mold/children and constructs the corresponding constructor (e.g. `(["consistent", "~"], [l, r])` → `Consistent(l, r)`).

`IdTagged` ids are preserved across MakeTerm so that cursor inspector / statics / verifier all see the same ids as the editor UI.

The top-level wrappers `DrvQuote(drv, sort)` and the type `DrvQuoteTy(sort)` are built by the `of_jdmt … end` etc. match arms: seeing `(["of_jdmt", "end"], [Drv(Exp(j))])` produces `DrvQuote(Exp(j), Jdmt)`.

### Statics (`drv_to_info_map` in `Statics.re`)

When `uexp_to_info_map` encounters `DrvQuote(term, sort)` in the enclosing Exp context, it:

1. Adds an `InfoExp` for the quotation with `elab_syn_ty = DrvQuoteTy(sort)`.
2. Delegates to `drv_to_info_map(term, ~sort)`, which recursively walks every `Drv.Any.t` inside and emits an `InfoDrv(DrvInfo.t)` entry per id.

`DrvInfo.derived` computes each node's expected sorts (`sorts_of_exp`, `sorts_of_ctx`, …) and compares them to the enclosing sort to produce a `status`: `NotInHole`, `BadToken`, `MultiHole`, `FreeVar`, `VarNoJoin(expect, actual)`, or `NoJoin(expect, actuals)`. The `VarNoJoin`/`FreeVar` cases come from `add_quote` handling `Quote(x)` nodes (abbreviation references): they look `x` up in the enclosing Hazel `Ctx` and check that its annotated type is a `DrvQuoteTy(sort)` matching the expected Drv sort.

This is why the cursor inspector can show things like "Expected a variable of sort `Prop`, got `Int`" when an abbreviation is used in the wrong sort.

### Rules and rule sets

- `Rule.t` (in `Rule.re`) enumerates **every rule across every supported rule set**, with names following a `sort_judgement_form` convention (e.g. `T_Plus`, `S_Ap`, `A_Case`, `E_If_T`, `V_Num`, `TV_Arrow`, `C_UnkL`, `Truth_I`, `Assumption`, `Implies_I`, …).
- `RuleImage.t` (in `RuleImage.re`) is the **display-level** image of a rule used in the UI (the rule's "icon" as it appears in command-palette listings and node labels). `RuleImage.to_rule(rule_set, image)` resolves an image to a concrete `Rule.t` *given a specific rule set*, handling rule-set-specific remappings (e.g. `T_LetAnn` is remapped to `T_LetAnn_TV` in `RecursiveALFA` where it needs an extra type-validity premise).
- `RuleImage.rule_set` enumerates the supported rule sets: `PropositionalLogic`, `AL`, `ALB`, `ALF`, `ALFp`, `ALFA`, `RecursiveALFA`, `GradualALFA`. Each exercise pins one rule set (`DerivationExercise.spec.rule_set`), and scratch slides default to `PropositionalLogic` (changeable from the Rule Set cell).
- `RuleImage.all_rules_of_rule_set(rule_set)` filters the universe of rules to those available in a given rule set — used to populate the command-palette list.

### Rule specifications (`RuleSpec.re`)

Each `Rule.t` is mapped to a `Spec.t(exp, formula)`:

```
{
  prems:  list(exp);        -- premise shapes with spec-variables (e.g. `gamma |- e : t`)
  concl:  exp;              -- conclusion shape
  tests:  list(formula);    -- side conditions (e.g. `n1 + n2 = n3`, `x ∈ gamma`)
}
```

Spec terms use `DrvGrammar.M_Id` — the grammar instantiated without ids — so `Var("e")`, `Var("gamma")`, etc. are **spec-side variables**, not Hazel free variables. `SymbolMap.M` provides the canonical spellings (`e, e', e1, e_def, v, t, t_in, t_out, n, x, y, gamma, delta, a, b, c, tpat`).

Side conditions use `RuleFormula.M_Annotated`, a small GADT DSL with `LookUp*` leaves (resolve a spec-variable name at verify time) and constructors like `Plus`, `Times`, `Gt`, `EqExp`, `EqCtx`, `EqTyp`, `Mem` (membership), `Subset`, `UnboxNumLit`, `UnboxCtx`, `Subst`, `Glb`, `Rec`, etc.

Example shape (from `RuleSpec.of_spec`):

```reason
T_Plus => {
  prems:  [has(gamma, e1, Num), has(gamma, e2, Num)],
  concl:   has(gamma, Plus(e1, e2), Num),
  tests:  [],
}

E_Plus => {
  prems:  [eval(e1, n1), eval(e2, n2)],
  concl:   eval(Plus(e1, e2), n3),
  tests:  [Eq(Plus(UnboxNumLit(n1), UnboxNumLit(n2)), UnboxNumLit(n3))],
}
```

### Verification (`RuleVerify.re`)

Verification walks a spec term and the user's concrete derivation term **in lockstep**, binding each spec-side `Var(s)` to whatever subterm it matched against, then runs the side-condition tests against that binding:

1. `go_spec` recurses structurally; constructors must match, and `Var(s)` calls `register(s)`.
2. `register(s)` binds `s` to the matched `specced = (spec_term, user_term)` on first sight, and on subsequent sights requires the `user_term` to be `Drv.Any.eq` to the previously-bound term (producing `NotEqual(...)` otherwise).
3. Structural mismatches produce `FailMatch(specced)` (via `failunbox`). Arity mismatches in premise counts produce `Mismatch(expected, actual)`.
4. Once `go_spec` is done, `go_test` evaluates each `RuleFormula.t(bool)` against the binding map and produces `FailTest(map, test)` on failure.

The final `res = list(failure)` is then classified:

- `[]` → `Correct`
- All failures are "hole-in-the-right-place" → `PartialCorrect(specced)` (the user's tree is consistent with the rule modulo one or more `?` holes)
- Otherwise → `Incorrect(first_failure)`

"Partial correct" detection lives in `RuleVerify.partial_correct_specced`; it essentially asks "is every failure just a `FailMatch`/`FailUnbox`/`NotEqual` where the user-side term is a hole?" — if so, filling the hole could plausibly finish the proof.

### Grading (`DrvGrading.re`)

`DrvGrading` composes the pieces into a per-node status tree:

1. `ProofTree.mk(eds, ~stitched_results)` — takes the raw editor trees plus the evaluator's results (for each node, the stitched program is evaluated so that its concluding `DrvQuote(Exp(_), _)` can be extracted as the *parsed* conclusion). Produces a tree whose leaves are `result(Drv.Exp.t, ExternalError.t)` — the conclusion, or an error like `NotAJudgment` / `NoResult`.
2. `VerifiedTree.verify(rule_set, proof_tree)` — folds over the tree, calling `verify_single` at each node: resolve `Abbr(_)` references, look up the selected rule in the rule set, build a spec via `RuleSpec.of_spec`, and call `RuleVerify.verify` against the premises (which are the sub-trees' conclusions).
3. Each node's result is wrapped in `VerifiedTree.info = { rule, res }` where `res : Correct | PartialCorrect(specced) | Incorrect(failure) | Pending(ExternalError.t)`.

`VerifiedTree.strip_abbr` inlines abbreviation references, used when rendering the whole fully-resolved tree for the Explain-This pane.

`GradeExercise.score_of_verified_tree(spec, verified)` turns this into a point total for the top-bar score indicator (all-or-nothing by default).

### UI (`DerivationExerciseMode.re`, `DrvCursorInspector.re`, `DrvExplainThis.re`, `HoverRuleSpec.re`)

The derivation editor lays out four cells in scratch/doc mode, plus extra chrome for exercise mode:

- **Rule Set** — cell with a styled `<select>` driven by `RuleImage.all_of_rule_set`.
- **Prelude** (exercise mode only; in student view read-only).
- **Setup** — free-form Hazel expressions (used to bind abbreviations with `let ... = of_jdmt ... end in ...`).
- **Derivation** — the actual proof tree of `deduction` nodes rendered via nested `deduction_view` calls.

Each deduction node is a three-row layout:

```
  ┌──────── premises (child deductions) ────────┐
  │                                             │
  ├─── horizontal bar + rule label (click to open ninja-keys-rules palette)
  │
  └──────── conclusion (a single CellEditor) ───┘
```

`NinjaKeys.open_command_palette(~rule_set, ~pos, ~inject)` pops a ninja-keys palette of available rules (filtered by `all_rules_of_rule_set`) when the user clicks the rule label.

`DrvCursorInspector` shows, for a node under the cursor, the chosen rule's spec via `HoverRuleSpec.view` — rendering the spec's premises and conclusion through `DrvExplainThis.exp_show` and side-conditions through `DrvExplainThis.test_show` (both of which build a read-only `Editor.Model.mk(..., ~root=Drv(Jdmt))` and reuse `CodeWithStatics.View.view` for syntax highlighting).

Highlight colors between the spec view and the user's derivation come from `ColorSteps` applied to `RuleVerify.specced` ids, so hovering a spec-side `e` lights up the user's matched subterm and vice versa.

### The `DerivationExercise.pos` type

A `pos` picks out one cell inside a derivation slide:

```reason
type pos =
  | Prelude
  | Setup
  | Trees(int, Tree.pos);  /* which tree (i), and which node inside it */
```

This is used both as:

- An argument in `Update.Editor(pos, action)` for dispatching editor actions to the right cell.
- The pos component of `DerivationExerciseMode.Selection.InCell(pos, _)` for tracking which cell the user's focus is in (so that `get_cursor_info`, `get_derivation_info`, cell-selection highlighting, and command-palette targeting all agree on "the active cell" — see `docs/*` and the git history for a fix addressing a stale-`model.pos` bug this discipline avoids).

### Abbreviations

Inside a derivation tree you can mark a subtree as an **abbreviation** target (`Abbr.Abbr(Some(i))`): it renders as a labeled "•" leaf and re-uses the `i`-th abbreviation tree's verified conclusion. Abbreviations must be:

- Acyclic (abbreviation `i` can only reference earlier abbreviations).
- Leaf-only (they never have their own children — children would be silently dropped).
- Resolvable (no dangling `Abbr(None)` references in strip; those are rendered as `Pending(NoAbbr)`).

`VerifiedTree.strip_abbr` inlines these when constructing a fully-expanded tree (used by `Explain-This` and grading).

### Dynamics

The evaluator barely touches `DrvQuote` — `Transition.drv_transition` does two things only:

1. Resolves `Quote(x)` (abbreviation references inside a quotation) to the already-evaluated `DrvQuote(...)` bound to `x` in the environment.
2. Simplifies `Cons(p, ctx)` / `Concat(e1, e2)` when their context arguments reduce to `Ctx(es)` literals.

Everything else inside a `DrvQuote` is carried through evaluation as-is. The result of evaluating `of_jdmt e end` is `DrvQuote(Exp(e'), Jdmt)` where `e'` has had any abbreviations resolved; `DrvGrading.ProofTree.conclusion_of_result` extracts `e'` back out for verification.

### Persistence

A derivation exercise's persistent form (`DerivationExercise.persistent_state = p(PersistentZipper.t)`) stores:

- Title, module name, prompt, max points, rule-set selection.
- Prelude and setup zippers.
- Trees: nested `abbr_trees(deduction(PersistentZipper.t))`, i.e. per-node judgement zippers plus the rule choice.

The `.ml` example files in `src/web/derivation/examples/` and `src/web/exercises/examples/` are the serialized form (one giant OCaml-literal record). They are loaded on app start by `Init.re`.

## Extending the deriver

The amount of work to extend the deriver scales with how invasive the change is. The four common cases below are ordered from least to most disruptive; each later case subsumes the earlier ones.

### Case 1 — Add a rule to an existing rule set

Use this when the rule uses existing term syntax and an existing judgement (e.g. "add a `T_Div` rule to `ALFA`").

1. **`Rule.re`** — add a new constructor to `Rule.t`, e.g. `| T_Div`. Follow the `sort_judgement_form` naming convention. `[@deriving enumerate]` will auto-add it to `Rule.all`.
2. **`RuleImage.re`** — add a matching constructor to `RuleImage.t` if the display form is new (the image is what the UI shows in the rule label and the ninja-keys-rules palette). For each rule set that should accept this rule, extend the `to_rule` arm to dispatch the new image to the new rule. *Don't rely on the fall-through `_ => None`*: the warning comment at the top of `to_rule` explicitly calls out that new rules will silently be unavailable unless you list them explicitly.
3. **`RuleSpec.re`** — add the new rule's `Spec.t` in `of_spec`: list its premise shapes (using `SymbolMap` canonical variables like `e1`, `e2`, `gamma`, `t`), its conclusion, and any side-condition `RuleFormula.t(bool)` tests. For inspiration, look at the existing typing rules (`T_*`), synthesis/analysis rules (`S_*`/`A_*`), value rules (`V_*`), and evaluation rules (`E_*`) in the same file.
4. **`Test_Derivation.re`** — add a small positive test (a concrete derivation the rule verifies against) and, ideally, a negative test (a deliberate mismatch that should fail).

No UI changes are required: the ninja-keys-rules palette is populated by `RuleImage.all_rules_of_rule_set`, which reads back from the `to_rule` dispatch automatically.

### Case 2 — Add a new rule set for an existing language

Use this when you want to expose a new curated subset of existing rules (e.g. a "teaching" rule set that hides analysis-mode rules, or a variant of ALFA with an added/removed feature).

1. **`RuleImage.re`** — add a new constructor to `rule_set` (the `[@deriving enumerate]` auto-populates the Rule Set dropdown). Extend `rule_set_of_string` and the error message in the fall-through. Extend `to_rule` with a new arm listing exactly which `RuleImage.t` images this rule set accepts and how each maps to a `Rule.t`. Rule sets are conventionally organized cumulatively — start from a sibling arm and diff.
2. **Tests** — add a rule-set-specific test in `test/Test_Derivation.re` that exercises a rule available in this set and one unavailable rule (verifying the `Pending(NotAvailable)` status).

No grammar, statics, or UI changes: the new rule set is immediately selectable via the Rule Set cell's dropdown.

### Case 3 — Add new syntactic forms for an existing language

Use this when the new language has new surface syntax — a new term constructor, a new judgement, a new connective. For concreteness imagine adding references: `ref e` / `!e` / `e := e'` with a `store` judgement form.

1. **Grammar** (`DrvGrammar.re`) — add new constructors to the relevant union in the `M` functor: `exp_term` for new expression/judgement forms, `typ_term` for new types, `pat_term`/`tpat_term` for new patterns. Because `DrvGrammar.M` is a functor, the same constructors become available to both the `Annotated` instance (editor terms) and the `Id` instance (rule specs).
2. **Term utilities** (`DrvTerm.re`) — extend `map_term`, `eq`, `subst`, `contains_hole`, `is_hole`, and anything else that matches on the new constructor. Miss one and OCaml's exhaustiveness check will flag it.
3. **Statics** (`drv_to_info_map` in `Statics.re`) — add a branch for the new constructor that recurses on its subterms and calls `add`. Also update `DrvInfo.sorts_of_exp` (or the appropriate `sorts_of_*`) so cursor-inspector sort reporting knows in which sorts the new form is admissible.
4. **Dynamics** (`drv_transition` in `Transition.re`) — add a reduction case if the form has any meta-level behavior (most ALFA forms don't; they're data for the verifier). For pure-data forms just propagate the recursion. The rule-level semantics (how the form interacts with the `E_*`/`T_*` rules) lives in the specs, not here.
5. **Pretty-printing** (`ExpToSegment.re`, in `drv_exp_to_pretty` / `drv_typ_to_pretty`) — emit the surface syntax so the form round-trips.
6. **Concrete syntax / forms** (`Form.re`) — add constructors to `drv_compound_form` and corresponding cases to `drv_get` with labels, precedences, and child sorts. Choose tokens carefully — they go straight into TyDi's completion database (`TyDiForms.Delims.leading_drv_exp` etc. regenerate automatically from `Form.delims` on build).
7. **Parsing** (`MakeTerm.re`, in `drv_exp` / `drv_typ` / `drv_pat`) — add label-driven match arms that turn `([token_label...], [kid_terms...])` into the new constructors. IDs propagate automatically via `IdTagged`.
8. **Rules and specs** (`Rule.re` + `RuleImage.re` + `RuleSpec.re`) — introduce the rules that govern the new form (as in Case 1). If the rule needs a new side-condition primitive (e.g. `FreshLoc(store, l)`), extend the `RuleFormula.M` GADT and its verifier in `go_test` (`RuleVerify.re`).
9. **Examples** — add a `.ml` derivation example to `src/web/derivation/examples/` demonstrating the new syntax; register it in `src/web/init/Init.re` so it appears in documentation mode.

### Case 4 — Add a new derivation sub-sort

Use this when the new feature needs a judgement category that genuinely can't share a sort with any existing one. For example, if you wanted a dedicated `Effect` sort separate from `Jdmt`.

1. **`DrvSort.re`** — add the new constructor to `DrvSort.t`. Extend `class_of` (which CSS class it maps to), `to_string`, `to_string_short`, `to_string_verbose`, and `consistent`.
2. **`DrvInfo.re`** — include the new sort in `sorts_of_exp`/etc. as appropriate. Make sure `status` and `error` still cover the new sort.
3. **Statics / remolding** — decide whether the new sub-sort will have its own mold (like `Drv(Typ)` does) or piggyback on `Drv(Exp)`'s mold via statics-time refinement (like `Jdmt`/`Ctx`/`Prop` do — see the note in `DrvSort.re` about the "remolding issue"). The piggyback path is usually cheaper; it's what the existing sub-sorts use. If you go the refinement route, make sure `Info.refine_sort_from_mold` returns the right sort for the new constructors.
4. **CSS** — if the new sort should have its own color, add `--token-<sort>`, `--shard-<sort>` variables in `variables.css`, and `.token.<Class>`, `.child-line.<Class>`, `svg.shard.indicated.<Class>` rules in `editor.css`. The `.<Class>` string comes from `DrvSort.class_of`.
5. Then proceed as in Case 3 to add the actual forms that live in the new sort.

### General checklist

Regardless of which case you're in:

- **Do not rely on `_` fall-throughs** in `RuleImage.to_rule`, `DrvInfo.sorts_of_exp`, or `DrvSort.class_of`/`to_string`/`consistent`. The existing code uses explicit cases deliberately so that adding a new variant fails to compile (or behaves predictably) until every site is updated.
- **Keep `Annotated` and `Id` grammar instantiations in sync.** Both live in `DrvGrammar.re` via the functor; anything you add to the functor body is picked up by both. Anything you add outside (e.g. a helper in `DrvTermBase.re` or `RuleSpec.M_Id`) has to be replicated in both places.
- **Run `./run_tests 'Derivation*'`** after making changes. The rule-verification and grading pipelines are independent enough that a regression in one rule rarely breaks unrelated tests, so errors that do show up are usually informative.
- **Prefer adding to `Test_Derivation.re` before changing code**: it uses `Web.DerivationExercise` at the same abstraction level as the UI, and writing a failing test first often clarifies which layer a bug is in.

---

## Key Files

### Core term layer

| File                                         | Purpose                                                                   |
| -------------------------------------------- | ------------------------------------------------------------------------- |
| `src/language/derivation/DrvSort.re`         | `DrvSort.t` enum + `class_of`, `to_string`, `to_string_short`, `consistent` |
| `src/language/derivation/DrvGrammar.re`      | Functor `M` producing `exp_term`/`pat_term`/`typ_term`/`tpat_term`/`any_t` |
| `src/language/derivation/DrvTermBase.re`     | `Annotated` instantiation of `DrvGrammar` + per-sort term utilities       |
| `src/language/derivation/DrvTerm.re`         | Higher-level term helpers (`fresh`, `subst`, `glb`, ctx ops, `eq`, …)     |
| `src/language/derivation/Drv.re`             | Thin alias: `module Exp = DrvTerm.Exp`, etc.                              |
| `src/language/derivation/DrvInfo.re`         | `DrvInfo.t`, status computation, `sorts_of_exp`                           |
| `src/language/derivation/SymbolMap.re`       | Canonical spec-variable names (`e`, `gamma`, `t`, …)                      |

### Rules and verification

| File                                         | Purpose                                                                   |
| -------------------------------------------- | ------------------------------------------------------------------------- |
| `src/language/derivation/Rule.re`            | Universe of rules (`Rule.t` enum, ~200 rules)                             |
| `src/language/derivation/RuleImage.re`       | `RuleImage.t` + `rule_set` + `to_rule` + `all_rules_of_rule_set`          |
| `src/language/derivation/RuleSpec.re`        | `of_spec` — builds a `Spec.t` from a `Rule.t`                             |
| `src/language/derivation/RuleFormula.re`     | Side-condition DSL GADT + `get_symbols`                                   |
| `src/language/derivation/RuleVerify.re`      | `verify`, `go_spec`, `go_test`, `partial_correct_specced`                 |

### Hazel language integration

| File                                         | Purpose                                                                   |
| -------------------------------------------- | ------------------------------------------------------------------------- |
| `src/haz3lcore/lang/Form.re`                 | `drv_compound_form` + `drv_get` (forms and molds)                         |
| `src/haz3lcore/lang/MakeTerm.re`             | `drv_exp`/`drv_pat`/`drv_typ` tiles → `Drv.*.t`; quotation wrapping       |
| `src/language/term/TermBase.re`              | `DrvQuote(drv, sort)` / `DrvQuoteTy(sort)` constructors in Exp/Typ        |
| `src/language/statics/Statics.re`            | `drv_to_info_map` — statics for every `Drv.Any.t` node                    |
| `src/language/statics/Info.re`               | `InfoDrv`, `refine_sort_from_mold` helper                                 |
| `src/language/term/Typ.re`                   | `DrvQuoteTy` type, `desugar_sig`, `DrvSort.to_string` in pretty-printing  |
| `src/language/dynamics/transition/Transition.re` | `drv_transition`, `DrvQuote` traversal during evaluation             |
| `src/haz3lcore/TyDi/TyDiForms.re`            | Drv-sort autocompletion (`leading_drv_exp`, `infix_drv_exp`, …)           |
| `src/haz3lcore/pretty/ExpToSegment.re`       | `drv_exp_to_pretty`, `drv_typ_to_pretty`, `drv_formula_to_pretty`         |
| `src/menhirParser/Conversion.re`             | One-way conversion; errors on `DrvQuote`/`DrvQuoteTy` (see limitations)   |

### UI / editor

| File                                         | Purpose                                                                   |
| -------------------------------------------- | ------------------------------------------------------------------------- |
| `src/web/derivation/DerivationExercise.re`   | Exercise spec record (`p('code)`), `pos`, stitching, persistence          |
| `src/web/derivation/DrvGrading.re`           | `ProofTree` + `VerifiedTree` — turns editor trees into graded `info` trees |
| `src/web/derivation/DrvCursorInspector.re`   | Cursor inspector panel content for a derivation node                      |
| `src/web/derivation/DrvExplainThis.re`       | Spec/formula rendering used in the Explain-This pane                      |
| `src/web/derivation/HoverRuleSpec.re`        | Hover preview of a rule's spec                                            |
| `src/web/view/DerivationExerciseMode.re`     | The main derivation-mode view (cell layout, tree rendering, ninja keys)   |
| `src/web/app/common/Icons.re`                | `entail` icon used in the scratchpad toolbar                              |

### Exercise examples

| File                                                                | What                                          |
| ------------------------------------------------------------------- | --------------------------------------------- |
| `src/web/exercises/examples/BlankDerivationExercise.ml`             | Empty spec — starting point for new exercises |
| `src/web/exercises/examples/Ex_EvaluationDerivation.ml`             | Evaluation-derivation exercise                |
| `src/web/derivation/examples/Ex_Conjunction_Commutativity.ml`       | Propositional logic exercise (doc mode)       |
| `src/web/derivation/examples/Ex_Curried_Function_Derivation.ml`     | Typing derivation (doc mode)                  |
| `src/web/derivation/examples/Ex_PairMap_Derivation.ml`              | Typing derivation (doc mode)                  |
| `src/web/derivation/examples/Ex_Shadowing_And_Closures.ml`          | Evaluation derivation (doc mode)              |
| `src/web/derivation/examples/Ex_Type_Validation_Derivation.ml`     | Type-validity derivation (doc mode)           |

### CSS

| File                                         | What                                                                       |
| -------------------------------------------- | -------------------------------------------------------------------------- |
| `src/web/www/style/editor.css`               | `.token.Drv`, `.child-line.Drv`, `svg.shard.indicated.Drv` (drop-shadow)   |
| `src/web/www/style/exercise-mode.css`        | `.cell-derivation`, `.deduction-*`, `.rule-set-select`, `.drv-explainthis` |
| `src/web/www/style/variables.css`            | `--token-drv`, `--shard-drv`, `--shard-caret-drv`, …                       |

### Tests

| File                                   | What                                                               |
| -------------------------------------- | ------------------------------------------------------------------ |
| `test/Test_Derivation.re`              | Rule verification / rule-set dispatch / grading unit tests         |
| `test/Test_DerivationCase.re`          | Derivation-mode case-expression edge cases (ID collision, etc.)    |
