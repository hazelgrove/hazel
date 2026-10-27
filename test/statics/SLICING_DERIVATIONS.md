# Slicing test justifications

Every expectation in `Test_Statics_Slicing_*.re` that was changed rather than
fixed in the engine is justified here, one entry per test name. An entry gives
the judgement instance, the rule chain, the resulting slice, and the evidence it
rests on. No expectation may be changed to match engine output without an entry.

## Two principles the analysis entries rest on

**P1 (the focus is the hole).** An analysis slice is a slice of a one-hole
*context* `C` (Definition 7.1): `(κ, γ)` with `κ ∈ ⌊C⌋`. The focus is the hole,
so no part of the focused subterm can appear in `κ`. The hand-written cases
`ana-app-arg` (`f(?)`), `ana-ascription-inner` (`(? : Int)`) and
`ana-tuple-element` (`((?, ?) : (Int, ?))`) already encode this.

**P2 (γ scopes over the context, not the hole).** γ assigns types to the free
variables of `κ`. A variable occurring only at the focus is not free in `κ` and
gets no entry; the type demanded of the focus is the output witness φ, not a γ
entry. `ana-app-arg`'s γ = `f : Int -> ⬜` is the paper's own example
(`analysis.tex`, §7.1): there `f` is in the context, and the focus is the hole.

**Askability.** The query must lie in `⌊τ⌋` for the type `τ` recorded at the
focus. Several entries below changed a query that was taken from the *parent*
node's type and is therefore not a query about the focus at all.

## How the evidence was obtained

`Info.exp.ana` is Hazel's own record of "parental type expectations" at a node,
so it says which construct demands what, with no appeal to a reading of the
rules. The probe used for the entries below printed, per program and focus,
`ana`/`ty`/`elab_syn_ty` plus the engine's slice over a ladder of queries from
the recorded `ana` down to `⬜`; monotonicity across that ladder is itself a
check. That probe lives in `Test_Statics_Slicing_Probe.re`: it asserts nothing, it
prints, so add a case there whenever a new expectation needs evidence.

## Analysis

### ana-app-function

`(f(1) : Bool)` under `f : Int -> Bool`, focus `f`.

Hazel's `Ap` rule puts the function in an arrow-shaped analysis position:
`ana(f) = ⬜ -> ⬜`, *not* `⬜ -> Bool` (probe evidence). The ascription
constrains the application's result, which the rule does not propagate back into
the function's position, so the context enforces nothing about `f`'s codomain.
`ty(f) = Int -> Bool`.

- Old query `Bool`: not in `⌊Int -> Bool⌋`, so it is not a query about this focus
  at all. Rejected by the askability check.
- New query `⬜ -> Bool` ∈ `⌊Int -> Bool⌋`. Since the hole's position is
  informationless (`⬜ -> ⬜`), the analysis slice is degenerate: nothing in the
  context explains the codomain. `κ = (✓(?) : Bool)` with the ascription kept as
  the ancestor spine (the engine's documented focus policy keeps strict ancestor
  nodes; a stricter minimality would hole the annotation too), and γ = ∅ by P2.
- The old γ `f : ⬜ -> Bool` named the focus itself, which P2 forbids.

### regression-ana-projection-base-query

`x.A` under `x : (A=())`, focus `x`.

`ana(x) = ⬜`: Hazel's `Dot` synthesises its receiver. `ty(x) = (A=())`, so the
query `(A=⬜)` is askable. Projection is outside the formalism, so this is
real-logic: the field label is what makes a product carrying label `A` wanted
here, and the label is retained; the receiver is the hole and the rest is
omitted. `κ = ✓.A`, γ = ∅ by P2 (`x` is the hole, so the old γ entry
`x : (A=⬜)` cannot arise).

### regression-ana-sum-list-cons-query

`Z :: Z`, focus the head `Z`.

`ty(head) = + Z + ⬜` and `ana(head) = ⬜` (Hazel's `Cons` synthesises the head
and analyses the tail at `[ty_head]`). The old query `[+ Z + ⬜]` is the *cons
node's* type, not the head's, so it fails askability. At the askable query
`+ Z + ⬜` the head sits in a synthesis position, so by §7's remark (a well-typed
context does not influence why a term synthesises a type) the slice is
degenerate: `κ = ✓ :: ?`, γ = ∅.

### regression-ana-let-list-pattern-query

`let [] = a in (())` @ `[⬜]`, focus the definition `a`.

`ana(a) = [⬜]`, demanded by the pattern `[]` (probe evidence). So the pattern is
the explanation and the body is irrelevant: `κ = let [] = ✓ in ?`, γ = ∅. The
old expectation retained the focus `a`, which P1 forbids.

### regression-ana-fix-sum-body-query

`fix Z -> Z` @ `+ Z + ⬜`, focus the body.

`ana(body) = + Z + ⬜`, demanded by the fix pattern (`fix p -> e` analyses `e`
against `ty(p)`; probe evidence). The constructor pattern `Z` is what supplies
the sum, so `κ = fix Z -> ✓`, γ = ∅. The old expectation retained the focus.

### regression-ana-fix-atom-body-query

`fix 0 -> 0` @ `Int`, focus the body.

Same rule: `ana(body) = Int`, demanded by the atom pattern `0`, so
`κ = fix 0 -> ✓`, γ = ∅. The old expectation `fix ? -> 0` was exactly inverted:
it dropped the only term that explains the demand and kept the hole. Read
together with `regression-ana-fix-sum-body-query` the two old expectations
contradicted each other under one rule, which is what first marked them as
engine echoes rather than derivations.

Control (new): `fix (x : Int) -> x` @ `Int` focus the body gives
`fix (? : Int) -> ✓` — with an annotated binder the annotation carries the
demand and the bound name carries nothing, matching the atom case where the
pattern itself is the carrier.

### regression-ana-cons-tail-head-query

`0 :: [[]]` @ `Int`, focus the inner `[]`.

`ana([]) = Int`: the head `0` synthesises `Int`, the tail is analysed at
`[Int]`, hence its element at `Int` (probe evidence; `elab_syn_ty` of the focus
is `[⬜]`, which is why Hazel's fixed `ty` is the demanded `Int`). The head is
therefore the explanation: `κ = 0 :: [✓]`, γ = ∅. The old expectation
`?:: [[]]` both omitted the head and retained the focus.

### regression-ana-fix-annotated-hole-query

`fix(Z:(⬜ -> ⬜)) -> ?` @ `⬜ -> ⬜`, focus the body hole. **Expectation kept.**

`ana(body) = ⬜ -> ⬜`, demanded by the annotation on the fix pattern, so
`κ = fix(? : (⬜ -> ⬜)) -> ✓`: the annotation is retained (a shaped query still
demands its shape, even though `⬜ -> ⬜` is empty in the lattice), and the
constructor pattern `Z` is omitted because a constructor supplies a sum shape,
which has no overlap with an arrow demand. This entry records an engine fix, not
a test change: `omit_typ` now holes a type node only when the query is a gap
(not merely empty), and `slice_ctor_pat` omits a constructor pattern whose own
type does not overlap the query.

## Extended examples

### The alias binder's parameter list: `type Option(?) = ?`

Affects `ex2-seq-error-ana`, `ex2-seq-error-ana-refined`, `demo-focus-f-use`,
`demo-focus-errfn-ana-full`, `-fold-digit`, `-fold-string`, `-fold-pin`.

These expected `type Option = ?` where the engine produces `type Option(?) = ?`.
Reconstruction is by **id omission**: a slice is the source term with the
omitted ids holed. Holing the parameter of the alias binder `Option(A)` yields
`Option(?)`; deleting the parameter list is not an omission of any id, it is a
rewrite of the binder, so `type Option = ?` is not in the image of
reconstruction at all. The two carry the same information (the alias name is
retained, nothing about its parameters is), and `demo-focus-seq-use` already
expected the `Option(?)` form for the same alias, so the expectations were
inconsistent with each other. Changed to the holed-parameter form.

### demo-focus-errfn-ana-gap

Query `⬜`, focus the `d4` function. A gap query demands nothing, so by the Gap
rule every subderivation is omitted and the minimal analysis slice retains only
the ancestor spine of the focus: `let ? = ? in … ?(fun ? -> …)(?)`. The old
expectation additionally retained the whole `seq` binding as a shell
(`let seq = abs ? -> abs ? -> fun ? -> fun (? : ?) -> ?`), which is the old
engine's keep-head heuristic: those nodes explain nothing about `⬜` and their
retention is not minimal. Monotonicity is the check here: the slice at `⬜` must
be below the slice at every more precise query, and the `⬜` slice is now the
least of the family (`-fold-pin`, `-fold-digit`, `-fold-string`, `-ana-full`).

### demo-focus-seq-use

Focus the use of `seq`; the query is the type the inspector shows for that
focus. Two changes.

**The query is written out explicitly** as
`poly A -> (poly B -> … -> String -> Option((String, B)))` rather than taken
from `Info.exp.elab_syn_ty`. That field is *normalised*
(`… -> + None + Some((String, B))`), so asking it makes the query demand the
`None` tag, and the slice then has to retain `None` and the `Option` definition
body to explain it. The figure asks the question the UI asks, which is the
written, alias-form type, so the test now states it. (When the unnormalised
inspector type lands, `~query=None` will again agree with this spelling.)

**The second case branch `f(?)` is retained** where the old expectation had
`case ? | ? => ? | ? => ? end`. The λ rule gives `ψ = φ_p funarr ψ_body`, so the
codomain of the sliced type comes from the body's slice and from nowhere else.
The query demands `… -> String -> Option((String, B))`, so some retained term
must supply `Option((String, B))`; with both branches omitted the case supplies
`⬜` and `ψ ⊒ υ` fails, i.e. the old expectation is not a slice at all. The
`f(a)` branch supplies it through `f`'s annotation, so it (and the binder `f` it
mentions) is retained, and the `None` branch is omitted because `None` supplies
only `Option(⬜)`, whose overlap with the demand is `⬜`. The old expectation
came from an engine whose `ψ` was read off the type checker rather than built
from the body slice, which is the design this branch deliberately drops.

## Binding and modules

### pat-ctor-shadow-ann

`type T1 = +A(Int) in type T2 = +A(String) in let A(x) : T1 = A(1) in 0`,
focus the pattern variable `x`, query `Int`. The old expectation retained
`type T2 = + A(String)`; the engine omits it.

The constructor `A` is declared by both aliases, so something has to select
`T1`: the pattern's annotation does, and it is retained for exactly that reason
(this is the real-logic rule for constructor patterns — the annotation is kept
when, and only when, the constructor is ambiguous in the context, which is what
distinguishes this case from `pat-ctor-component`, where the annotation is
dropped). Once `T1` is named, `T2` plays no part: it neither supplies `x`'s type
nor affects how `A` resolves. Checked by type-checking the slice:
`type T1 = +A(Int) in type ? = ? in let A(?) : T1 = ? in ?` elaborates with **no
marks at all**, so the omission costs nothing. `pat-ctor-shadow-unann` (no
annotation) is the complementary case and keeps `T2`, because there scope
resolution is what selects the constructor.

### signature-type-member

`let m : { type T = Int; let x : T } = { type T = Int; let x = (1 : T) } in m.x`
at query `T`. The old expectation retained the module literal's own
`type T = Int`; the engine holes it, keeping the signature's copy.

Modules are outside the formalism, so this is real-logic, checked by
synthesising the type of each candidate slice:

| program | synthesises | marks |
| --- | --- | --- |
| the full program | `T` | free-tyvar `T`, 2 × ExpectationMismatch |
| slice with `type T = ?` | `T` | free-tyvar `T` |
| slice with `type T = Int` | `T` | free-tyvar `T`, 3 × ExpectationMismatch |

Both candidates synthesise the queried `T`, so `ψ ⊒ υ` holds either way and
minimality decides: the module's own definition explains nothing about `T`,
which is enforced by the signature's `let x : T`. The retained copy is in fact
worse than neutral — it reinstates the signature mismatch that the original
program carries, while the holed body is gradually consistent with it. (The
free-tyvar mark is in the test program itself and appears in all three.)

### Alias demands for a join of constructors

`if-join-either-full` and `if-join-either-left`. These expected the γ entry for
`Either` to be the definition with its payloads intact
(`typfun A, B -> Left(A) + Right(B)`); the engine yields
`typfun A, B -> ? + Left(?) + Right(?)` and, for the one-branch case,
`typfun A, ? -> Left(?) + ?`.

γ is the *minimal assumption* about `Either`, not a copy of its definition. The
query is in alias form (`Either(Int, Bool)`), so each constructor justifies its
result positionally — `Left@<Int, ?>` has result `Either(Int, ⬜)` from its own
schema, which is itself a γ entry — and the definition's payload positions are
never consulted. What is needed is the arity and the variant tags: hence holed
payloads, and binders kept exactly where the query constrains them (`A, B` when
both branches contribute, `A, ?` when only `Left` does; the holed binder still
witnesses the arity). The same file's single-constructor cases
(`param-option-explicit`, `param-either-explicit`) already expected this holed
form, so the join cases were the inconsistent ones. The contrast is
`param-result-sum-query`, where the query is the *expanded* sum
(`Error(?) + Ok(Bool)`): justifying an expanded type does require unfolding the
definition, so there the payload position `Ok(A)` and its binder are retained —
the engine now distinguishes the two by whether the constructor was explicitly
instantiated.

This entry also records an engine fix: joining two alias demands must merge
their binder masks positionally (keep a parameter named if either side names
it), not take one side's binder, or a join of `typfun A, ? -> …` and
`typfun ?, B -> …` loses a parameter.

## Property-harness corrections

The three QCheck properties (`Test_Statics_Slicing_Properties.re`) are now run
rather than skipped, and their counterexamples drove both engine fixes and two
corrections to the harness itself. The harness changes are recorded here
because they are test changes, not engine changes.

### Precision comparison must ignore `Parens`

`precision_geq` asked `Typ.meet(ctx, ty, query) = ty` under `Typ.fast_equal`.
`Typ.meet` strips `Parens`/`Projector` wrappers on both sides (its first two
arms), so a sliced type that legitimately retains the user's parentheses, e.g.
`[(rec y -> …)]`, can never be syntactically equal to its own meet. Parens
carry no precision: `⌊(τ)⌋ = ⌊τ⌋`, and every lattice operation in `Typ`
descends through them. The comparison therefore also accepts equality after a
deep `Parens` strip. This is a fix to the harness's encoding of `ψ ⊒ υ`, not a
weakening of the property.

### Coverage must enumerate variant tag ids

`omitted_cover` subtracts the ids still present in the reconstruction from the
ids of the whole term, so `all_term_ids` has to enumerate every id the engine
can omit. A sum's variant *tags* live in `ConstructorMap.Variant(_, ann, _)`
rather than in a `Typ.t` node, so `Typ.map_term` never visits them, while
`SliceQuery.omit_typ` does omit them (via `Typ.children`'s synthetic
`Var(name)` nodes). A tag id was therefore counted at a precise query, where it
is omitted directly, and uncounted at a less precise one, where it vanishes
inside a wholly holed annotation — a spurious monotonicity violation.
`all_term_ids` now notes each variant's `ann.ids`.

## Engine fixes found by the properties

Recorded here so the counterexamples are not re-derived later.

- **`fix p -> e` synthesises the *pattern's* type.** `Statics.re`'s `FixF` rule
  sets `elab_syn_ty = p'.elab_syn_ty` and analyses the body against it, so the
  body contributes nothing to the synthesised type. `syn_fix` previously
  returned the body's slice (ψ = ψ_body, pattern sliced at ⬜), which holed the
  pattern and so lost the type: `fix [] -> …` at `[⬜]` sliced to `fix ? -> …`,
  whose type is ⬜. The rule now slices the pattern at the query and omits the
  body, giving `fix [] -> ?`.
- **The unit type is not empty.** `Typ.is_empty` folded `Prod([])` to `true`
  (`List.for_all` over no components), so `()` was treated as carrying no
  information: `if false then (()) else ?` at `()` omitted the branch that
  supplies the whole type. Unit is a maximally precise type with no holes to
  omit, so `is_empty(())` is now `false`.
- **"No demand" is not "no information".** `pat_keep` used `is_empty(φ)` to
  decide whether a binder's *bindings* are demanded, where `φ` is built from
  the children's demands; with unit no longer empty, a `(())` pattern claimed a
  demand and kept its annotation at a gap query, breaking monotonicity. The
  demand test is now a separate `is_demandless`, which folds products of
  demandless components (the empty product included) to "nothing demanded".
- **`meet` keeps an equal-headed type application nominal.** Meeting
  `Option(⬜)` with `Option((String, B))` fell through to the
  `TypParamAp(_), _` arms, which weak-head-normalise both sides, so the meet of
  two alias applications came back expanded — the last normalisation site
  leaking `+ None + Some(String, B)` into a `let`-bound function's type after
  M0.5. Equal heads now meet argument-wise and keep the head.
