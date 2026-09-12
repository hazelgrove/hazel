# PLDI 2021 livelits vs. this branch: implementation status

> Analysis of `integration/modules-livelits` at `de629edccc` (2026-09-11),
> which is `origin/dev` at `fa6cd4eb09` plus the merges listed below. Every
> code claim cites a file and line range in that tree; line numbers will
> drift, the identifiers will not. Claims marked **verified** were checked by
> running the branch's own CLI or test runner, with the command given in
> "How each claim was checked"; everything else is read from the source and
> says so.
>
> This re-runs, against the branch, the comparison that
> `docs/research-implementation-status.md` ran against `dev` at `0943461829`.
> That file is not in this tree: it lives on `docs/research-status-report`
> (PR #2491), and its PLDI 2021 row reads *Partial (about one third). Three
> builtin livelits over the projector substrate. No definitions, parameters,
> or splices; expansion type is asserted, not checked.*

## Purpose

The paper is Omar, Moon, Blinn, Voysey, Collins and Chugh, *Filling Typed
Holes with Live GUIs*, PLDI 2021. Its central claim is that a client can fill
a typed hole by manipulating a GUI that a *library author* defined in the
language itself, and can reason about the result from the livelit's declared
interface alone — without reading the expansion or the implementation.

Realizing that on `dev` was blocked on two capabilities that have nothing to
do with livelits as such:

1. **Actual modules.** A livelit definition is a named bundle of types and
   values — `Model` and `Action` in the paper's body, the expansion type in
   its declaration, plus four functions. Until a module could *declare types
   in its signature*, there was nowhere in the language to put them. `dev` has
   namespace sugar whose signatures drop every `type` entry, so the previous
   report's verdict on type members was that they are "a `let`-site trick that
   cannot be declared or constrained in a signature".
2. **Views authored in Hazel.** The paper's `view` returns `Html(Action)`, a
   value in the language. On `dev` a livelit's view is an OCaml function
   returning a `Virtual_dom` node, so only the Hazel implementors could write
   one.

This branch merges both, plus the livelit stack, and then asks what is left:

| Merged | PR | What it supplies |
| --- | --- | --- |
| Modules II, parts 1 and 2 | #2493, #2494 | `Sig` types with manifest type members, module values, width subtyping |
| HTML substrate | #2115 | `Html.T` and friends as values in the language |
| User-defined livelits | #2411 | `let ^name = {...}` definitions |
| Expansion typing | #2488 | the use-site expansion check |
| Error dedup | #2489 | one mark per faulty use |

## Status vocabulary

Carried over from `docs/research-implementation-status.md`: Implemented,
Partial, Diverged, Missing.

## Executive verdict

**This branch realizes the paper's *functional* livelits. Its *macro* livelits
are still ahead.**

The two terms are Cyrus's, and `docs/livelits.md` now defines them. A
**functional livelit** has `expand : Model -> Expansion` and produces a value.
A **macro livelit** has `expand : Model -> (Exp, List(SpliceRef))` and produces
a program fragment with holes for the client's own expressions. Macro livelits
subsume functional ones: empty splice list, closed `Exp`. The paper describes
macro livelits throughout; what is implemented here is the functional fragment
of them, complete and checked.

Put in terms of the paper's two halves: one is *livelits as typed graphical
macros* — a provider defines a livelit in the language, a client fills a hole
with it, and the client reasons from the declared expansion type. That half is
implemented on this branch and checked. The other is *compositionality* —
parameters, abbreviations, and splices, with the closure collection that makes
a splice's value visible inside the GUI. None of that is here, and it is
exactly what separates functional from macro.

| Paper's halves | On `dev` | On this branch |
| --- | --- | --- |
| Provider defines a livelit in the language | Missing (OCaml only) | Implemented |
| Client reasons from the declared expansion type | Diverged (asserted) | Implemented (checked per use) |
| Compositionality (parameters, splices) | Missing | Missing |
| Live feedback from the client's run-time context | Missing | Diverged (probes, not closure collection) |

Counting the paper's mechanisms the way the previous report did, `dev` was at
about one third; this branch is at about two thirds, with the remaining third
concentrated in splices and everything that hangs off them.

## Feature comparison

| Paper's mechanism | Status here | Evidence |
| --- | --- | --- |
| Livelit definition written in the language (§3, Fig. 3) | **Implemented.** `let ^name = { type Model; type Action; type Expansion; let init; update; view; expand } in ...`. Four shipped examples define one each. | `src/language/statics/UserLivelit.re:1-45,111-158`; `hazel-programs/docs/livelits/*.hz` |
| Declaration separate from implementation (§3.1: `livelit $color at Color {...}`) | **Diverged.** There is no `livelit ... at ...` declaration form. The interface is three *type members* of the definition module, read back off the signature the module synthesizes. The information is the same; the syntax is a module, not a declaration. | `UserLivelit.re:127-142` (reads `Sig.TypeManifest` members), `src/language/term/Sig.re:57,107` |
| Definition-site context clause (§3.2: `context { }`) | **Diverged.** The module's own members are the definition-site context; helpers are ordinary extra members. Nothing declares or enforces what the definition may use from its enclosing scope. | `UserLivelit.re:33-36`; `hazel-programs/docs/livelits/color-picker.hz` (`css`, `pick`, `dot_x`) |
| `Model`, `Action`, `init`, `update`, `view`, `expand` (§3.2.1-3.2.5) | **Implemented**, with the paper's monads absent: `update : (Model, Action) -> Model` and `view : Model -> Html.T` are plain functions, because `UpdateCmd`/`ViewCmd` exist only to mediate splices. | `UserLivelit.re:247-258`; `hazel-programs/docs/livelits/README.md` |
| `view` returns `Html(Action)` (§3.2.3) | **Implemented.** `view : Model -> Html.T`, handlers emit Actions, same substrate the MVU apps use. | `hazel-programs/docs/livelits/defined-slider.hz:27-42`; `src/language/builtins/BuiltinsADT.re:498,621-637` |
| Expansion type is what the client sees (§2.3) | **Implemented, verified.** A use synthesizes the *declared* `Expansion` whatever `expand` computes. | `Statics.re:1784-1800`; verified below |
| Expansion validated against the declaration (§3.2.5) | **Implemented, verified.** Statics types the expansion of each use and marks `BadLivelitExpansion` when it is inconsistent with the declaration. Per-use validation is the paper's own strategy, not an approximation of it. | `Statics.re:1799-1828`, `UserLivelit.re:371-382`; verified below |
| Livelit context Φ (§4.2.1) | **Implemented.** A `LivelitEntry` in the ordinary typing context, added at the `let` that binds `^name`. | `Statics.re:2317-2336`, `src/language/statics/LivelitCtx.re:18-34` |
| Well-formedness of the other three members (Fig. 3 signatures) | **Missing, verified.** Nothing checks `init` against `Model`, `update` against `(Model, Action) -> Model`, or `view` against `Model -> Html.T`. The shipped examples ascribe their members, so the ascriptions do the work, not the system. Only the expansion carries a checked obligation. Note the calculus's Def. 4.3 asks only for `⊢ d_expand : τ_model → Exp`; this gap is against §3.2, not against the calculus. | verified below |
| Capture avoidance (§2.4.3) | **Implemented, verified.** An expansion that names a definition-site binding keeps it even when the client shadows that name. | verified below |
| Context independence (§2.4.3) | **Partial.** Statics permits an expansion to use enclosing bindings, and the main evaluation honours them; but the projector's event-time path evaluates `update`/`view` in the builtin environment, so a definition that captures outer bindings will disagree with the authoritative render. The paper enforces this with the explicit `context` clause; here it is an unenforced convention. | `src/haz3lcore/projectors/implementations/LivelitProj.re:236-258`, `654`; verified below |
| Model stored in the syntax tree (§3.2.1) | **Implemented, differently.** The model is the use's own argument, `^name(model)`, so state survives in the program text and is editable by unprojecting. The paper hides the model and uses the application slot for *parameters* instead. | `LivelitProj.re:42-50`; `Statics.re:1782-1792` |
| Model type must be serializable (§3.2.1) | **Diverged.** No serializability check; instead the projector decides per update whether the new model is checkpointable, and keeps it out of the program text if not. | `LivelitProj.re:14-25` |
| Parameters (§2.4.1) | **Missing.** The one application slot holds the model. A use is exactly `Ap(LivelitName, model)`. | `Statics.re:1782-1786` |
| Abbreviations, `let $uslider = $slider 0` (§2.4.1) | **Missing.** Nothing partially applies a livelit. | — |
| Splices (§2.4.2), `SpliceRef`, `new_splice`/`set_splice`/`eval_splice` | **Missing**, and this is the load-bearing gap: the `UpdateCmd`/`ViewCmd` monads, the splice typing discipline, splice editors inside the GUI, and Theorem 4.4's parameterized expansion all hang off it. The intended shape is written down. | `UserLivelit.re:27-32`; `docs/livelits.md` ("Functional livelits and macro livelits") |
| Closure collection for live feedback (§4.3, Defs 4.5-4.8, Thm 4.9) | **Diverged.** No cc-expansion, no proto-environments, no resumption. A projected use instead folds `view(model)` into the main evaluation, sampled at the projector's id by the probe machinery, so the view renders against the program's real run. That covers the model; it does not cover the paper's motivating case, a splice's value shown inside the GUI, because there are no splices. Selecting among several closures for one livelit inside a twice-applied function was not exercised. | `Statics.re:704-730`, `UserLivelit.re:299-330`, `LivelitProj.re:159` |
| Theorems 4.1-4.9 and the Agda mechanization | **Missing here**, as on `dev`. The mechanization lives in `hazelgrove/hazelnut-livelits-agda`; nothing in this repository corresponds to it. | — |
| Livelits for other sorts (types, patterns, modules — §6 future work) | **Missing.** Expressions only. | — |

## What the branch newly supplies

### 1. The definition is a module, and its types are its interface

`UserLivelit.detect` takes the bound module, asks `ModuleHelpers.module_sig_type`
for the signature Modules II synthesizes, and reads `Model`, `Action` and
`Expansion` out of it as `Sig.TypeManifest` members
(`UserLivelit.re:127-142`). Everything the paper puts in a declaration is
therefore ordinary module machinery: members may appear in any order, a member
type may be stated in terms of an earlier one, shadowing follows module rules,
and helpers are just more members. At run time the same holds: the evaluated
definition is a `Module` whose items are `ModVal(x, v)`, and the projector
reads members by name (`LivelitProj.re:188-215`).

This is what "the missing feature was actual modules" amounts to concretely.
Without signature type members there is no place to write `Expansion`, and
without `Expansion` there is nothing for a client to reason against — which is
the paper's whole point.

### 2. Providers now write views in Hazel

`dev` carries three livelits implemented in OCaml (`Slider`, `Emotion`, `Js`).
On this branch only `Js` remains in OCaml
(`src/language/Livelit.re:205`, against `origin/dev:src/language/Livelit.re:452`);
the slider and the emotion face were rewritten as Hazel definitions and ship as
documentation slides, joined by a colour picker and a graph editor
(`hazel-programs/docs/livelits/`, 307 lines for four livelits). The paper's
provider role moved out of the compiler and into the language — which is the
second missing feature, and the reason a livelit's `view` can now be read,
edited and type-checked like the rest of the program.

### 3. The reasoning principle is checked, not asserted

The previous report's sharpest finding about `dev` was that the declared
expansion type was *asserted*: statics emitted the expansion with
`elab_syn_ty = expansion_t` and no marks, so a livelit whose `expand` produced
the wrong type was silently trusted. On this branch a use still synthesizes the
declared type — clients reason against the interface — but statics separately
types the expansion and marks the use when the two are inconsistent
(`Statics.re:1799-1828`). Consistency rather than equality is the test, so an
expansion that synthesizes `Unknown` stays gradual.

## What is still missing, in priority order

1. **Splices — that is, macro livelits.** Everything the paper calls
   *compositional* and most of what it calls *live* is downstream of them, as
   are the `UpdateCmd`/`ViewCmd` monads, a reflected `Exp` type, and
   Theorem 4.4's parameterized expansion. `docs/livelits.md` records the
   intended encoding: `expand : Model -> (Exp, List(SpliceRef))`, with today's
   `expand : Model -> Expansion` the empty-splice-list case.
2. **Parameters and abbreviations.** Cheaper than splices and independently
   useful, but they collide with the current use of the application slot for
   the model; the syntax question has to be settled first.
3. **Definition-site well-formedness.** Three of four members are unchecked
   (verified below). A definition whose `update` returns the wrong type is
   accepted and misbehaves only in the GUI. This is a small, self-contained
   piece of work: analyze each member against the type its declaration implies.
4. **Livelits as module members.** The paper says definitions are "scoped and
   packaged like any other definition". Scoping holds here — `^name` obeys
   ordinary lexical scope, and a `^name` bound inside a module works within
   that module — but packaging does not: the name does not export, and
   `M.^name` is rejected as an invalid label (both verified below), so a
   livelit cannot be shipped in a library module and used from outside it.
   Given that a definition *is* a module now, this is the most incongruous
   gap, and probably the cheapest of these to close.
5. **Closure collection.** The probe-based view sampling covers the common
   case and is better integrated with the editor than the paper's machinery
   would be; the gap only becomes visible once splices exist.

## How each claim was checked

Built from the branch with `./hazel` (which builds `src/CLI/cli.bc.js` and runs
it); test groups run with `bash test/run_node.sh test <group>`.

| Claim | Command | Result |
| --- | --- | --- |
| The four shipped examples type-check and evaluate through their expansions | `./hazel run hazel-programs/docs/livelits/*.hz` | `100`; `("hwb(160 25% 10%)", "hwb(280 5% 40%)")`; `[(3, 2), (0, 0)]`; `"What a day!"` |
| A use synthesizes the *declared* expansion type | client writes `let client : String = ^pct(25)` where `Expansion = Int` and `expand` returns `m + 1` | `error: Expecting type String but got inconsistent type Int` |
| A wrong expansion is marked at the use | `expand` returns a `String` where `Expansion = Int` | `error: Livelit expands to type String, but declares Expansion = Int` |
| A wrong model is marked at the use | `^pct("twenty five")` where `Model = Int` | `error: Expecting type Int but got inconsistent type String` |
| `init`, `update`, `view` are **not** checked | `let init = "not an int"`; `let update = fun (m, a) -> "wrong"`; `let view = fun m : Model -> 17`, each with the rest well-formed, both bare and wrapped in `^^livelit(...)` | `No static errors found.` in every case |
| Capture avoidance | definition's `expand` calls a definition-site `bump`; client rebinds `bump` to `fun x -> x + 1000` before the use | `26`, not `1025` |
| Lexical scoping | `^pct` defined inside a function, used outside it | `error: Variable pct is not bound` |
| A livelit works inside its module | `module Widgets = { let ^pct = {...}; let inside = ^pct(25) } in Widgets.inside` | `No static errors found.`; evaluates to `25` |
| but is not reachable through it | same module, used as `Widgets.^pct(25)` | `error: Invalid label` and `error: Variable pct is not bound` |
| The model slot accepts client expressions | `let n = 7 in ^pct(n)` | type-checks; evaluates to `8` |
| Test coverage | `bash test/run_node.sh test UserLivelits` | `Test Successful in 4.311s. 34 tests run.` |

Not verified here: anything requiring the browser (the widgets rendering and
responding to interaction, the optimistic render path, multi-closure
selection). The PR description reports the slider, the MVU counter and the
Modules slide checked by hand in the browser.

## Key files

`docs/livelits.md`; `docs/modules.md`;
`src/language/statics/UserLivelit.re` (whole file);
`src/language/statics/LivelitCtx.re`;
`src/language/statics/Statics.re:498-505` (bare name), `:704-730` (view
instrumentation), `:1282-1311` (member access), `:1782-1865` (use site),
`:2317-2336` (definition binding);
`src/language/statics/ModuleHelpers.re`; `src/language/term/Sig.re:57,107,307`;
`src/language/term/Grammar.re:141` (`ModVal`);
`src/language/builtins/BuiltinsADT.re:498-637` (the HTML modules);
`src/language/Livelit.re` (the one remaining OCaml livelit);
`src/haz3lcore/projectors/implementations/LivelitProj.re`;
`hazel-programs/docs/livelits/`; `test/Test_UserLivelits.re`.

Paper: <https://hazel.org/papers/livelits-pldi2021.pdf>. Mechanization:
<https://github.com/hazelgrove/hazelnut-livelits-agda>.
