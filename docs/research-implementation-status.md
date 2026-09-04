# Hazel research papers vs. the `dev` codebase: implementation status

> Analysis of `dev` at `0943461829` (2026-09-02). Every code claim cites a
> file and line range in that tree; every historical claim cites a commit,
> PR, branch, or tag. Line numbers will drift; the cited identifiers will
> not. Dates attached to a bare commit hash are author dates and may precede
> the commit's arrival on `dev` by weeks or months; dates attached to a PR
> number are merge dates, which is when the change landed. Where something
> was inferred rather than verified, the text says so.

## Purpose

The Hazel project has produced a sequence of major-venue papers (POPL, ICFP,
PLDI, OOPSLA) plus workshop papers, each describing a calculus or mechanism
that Hazel is meant to embody. This document answers two questions:

1. For each major conference paper, how far along is `dev` in realizing it?
   Which parts are implemented, partial, missing, superseded by a different
   design, or live only on an unmerged branch or in a separate repository?
2. How do modules and type members work in the repo, today and historically?

Status vocabulary:

| Status | Meaning |
| --- | --- |
| Implemented | On `dev`, tested, matches the paper's design in substance |
| Partial | On `dev` with named gaps relative to the paper |
| Diverged | The idea is on `dev` but the mechanism differs materially from the paper |
| Superseded | Was on `dev` (or its predecessor) and was replaced by a later design |
| Branch only | Exists only on an unmerged branch of `hazelgrove/hazel` |
| Separate repo | Implemented outside `hazelgrove/hazel` |
| Missing | Not implemented anywhere we could find |

## Executive summary

| Year | Venue | Paper | Status on `dev` | One line |
| --- | --- | --- | --- | --- |
| 2017 | POPL | Hazelnut: A Bidirectionally Typed Structure Editor Calculus | Diverged | Every edit state is typed, but through tylr tiles, greedy completion, and marks, not the Hazelnut action calculus. Non-empty holes became marks. |
| 2019 | POPL | Live Functional Programming with Typed Holes | Diverged | Indeterminate evaluation survives. Casts everywhere, non-empty holes, Delta, hole closures, instance numbering, and fill-and-resume are gone; probes and id-keyed incremental evaluation took their place. |
| 2020 | ICFP | Program Sketching with Live Bidirectional Evaluation (Smyth) | Missing; separate system | Six integration branches, 2020–2022, all abandoned. |
| 2021 | PLDI | Filling Typed Holes with Live GUIs (livelits) | Partial (about one third) | Three builtin livelits over the projector substrate. No definitions, parameters, or splices; expansion type is asserted, not checked. User-defined livelits and expansion typing are on an open PR chain ending in #2488. |
| 2023 | OOPSLA | Live Pattern Matching with Typed Holes | Implemented in behaviour, Diverged in formulation | Errors only when necessarily inexhaustive or redundant, tested against the paper's figures; but via a Maranget matrix, not the paper's constraints, and hole patterns now match at run time. |
| 2024 | POPL | Total Type Error Localization and Recovery with Holes | Part 1 Implemented (about 90 percent); Part 2 Missing | Marking is the architecture of `Statics.re`, fused with elaboration. Type hole inference has never been on `dev` (branch only, five closed or open PRs). |
| 2024 | OOPSLA | Statically Contextualizing LLMs with Typed Holes (ChatLSP) | Superseded | On `dev` from 2025-07-09 to 2026-03-19; replaced by a tool-using structure-editor agent with type-checker-gated edits. |
| 2025 | POPL | Grove: Collaborative Structure Editor Calculus | Separate repo | `hazelgrove/GRV`; nothing in this repository, no collaboration on `dev`. |
| 2025 | OOPSLA | Incremental Bidirectional Typing via Order Maintenance | Separate repo | Whole-program statics on every edit; incremental evaluation was built instead. |
| 2025 | OOPSLA | Syntactic Completions with Material Obligations | Separate repo; Hazel-native successor Branch only | tylr's data model is the editor's backbone; the completion algorithm is in `hazelgrove/tylr`, with a Hazel-native engine on three open PRs. |
| 2026 | OOPSLA | Interactive Data Analysis with Lively Typed Tables | About two thirds Implemented | Labeled tuples, table primitives, CSV loader, table projector, rich probes on `dev`; live typing exists only as draft PR #1988. |

Workshop and other papers with subsystems on `dev`: tylr (TyDe 2022,
Implemented), ExplainThis (Onward! 2022, Implemented), explicit polymorphism
(TFP 2024, Implemented; implicit instantiation Missing), Hazel Deriver
(VL/HCC 2025, Implemented), the stepper with scoped filters (arXiv 2605.31517,
2026, Implemented), Hazel Tutor (HATRA 2020, Diverged), decomposable type
highlighting (HATRA 2025, Branch only), Grove and the PROPL vision papers
(collaboration Missing).

A pattern recurs across the calculus papers. The behaviour the paper promises
is on `dev`; the mechanism the paper defines is not, having been replaced by
an engineering design that is easier to keep total and fast in a tile editor.
The marked lambda calculus is the exception: it is the architecture. The
other recurring pattern is that paper artifacts live on branches that never
merged because the statics core was rewritten under them three times
(Self/Mode in 2023, `ana` in 2025, Elastatics in 2026).

## Repository eras

| Era | Dates | Defining commits | Core directories | Terms and editor | Evaluator |
| --- | --- | --- | --- | --- | --- |
| HZ prototype | 2016-07 to 2017-04 | separate repo `hazelgrove/HZ` | | Hazelnut Z-expressions, OCaml | statics only |
| Hazel 1 | 2017-01-26 to 2020-01-27 | `132093953e` initial commit; Coq core `53d26233c0` (2017-04-15); Reason port PR #18 (2017-06-24); Coq removed `fd70aa34e3` (2019-02-07); incr_dom PR #81 (2019-09-04) | flat `src/*.re`, later `src/hazelcore`, `src/hazelweb` | semantics defined in Coq `Semantics.v` and extracted, then Reason `UHExp`/`ZExp`; Hazelnut actions | big-step Hazelnut Live dynamics with hole instances (POPL 2019 artifact tags, 2018-10/11) |
| Hazel 2 | 2020-01-27 to 2022-09-02 | PR #89 "new year, new codebase" (David Moon) | `src/hazelcore`, `src/hazelweb`, `src/pretty` | `UHExp`/`ZExp`, `Action_Exp`, `CursorMap`; `docs/overview.md` describes this era | elaborator plus evaluator, hole closures; environment model attempted 2022 |
| Hazel 3 (haz3l) | 2022-07-25 to present | `f1e319f3dd` "haz3l is go" (Andrew Blinn); on `dev` from `dd1b7ddbf2` (2022-08-02); Hazel 2 core deleted `9d5e432a3f` (2022-09-02); `dev` re-pointed to the `haz3l-tests` line between 2022-11 and 2023-01 | `src/haz3lcore`, `src/haz3lweb`; `src/language` split out via PR #1669 (2025-06-13), `haz3lweb` renamed `web`; `src/pretty` deleted 2026-08-11 | tylr tiles and zipper; `DHExp` and `UExp` merged (PR #1197, 2024-07); projectors (PR #1218, 2024-08); probes (PR #1420, 2025-03); modules (PR #2123, 2026-02) | environment-based `Transition` shared with a substitution-mode stepper (2023-12); Elastatics fused statics and elaboration (PR #2213, 2026-04); incremental evaluation (PR #2222, 2026-05); streaming (PR #2339, 2026-07) |

Two refinements to the usual three-era story. The Hazel 1 to Hazel 2 boundary
is a refactor of the same tree, not a new layout. The Hazel 2 to Hazel 3
boundary is a history rewrite: Hazel 2 PRs merged to `dev` in August 2022
(for example #656) are not ancestors of today's `dev`.

Tags: `popl19-artifact` and `popl19aec` mark `1443dde4d5` (2018-10-19), the
POPL 2019 artifact; `popl19-artifact-final` (2018-11-15) is off `dev`;
`school-done-milestone` marks the `haz3lschool` separation (2022-10);
`haz3l-july-6-2025` marks `bfae450d96`, a snapshot just before the July 2025
landing of the LLM assistant and the editing rework.

Volume and authorship. Commits per year: 2020 (1,667) was a student cohort
push on Hazel 2; 2025 (2,963) is dominated by Alexander Bandukwala, Andrew
Blinn, Matt Keenan and Russell Rozenbaum with essentially no AI trailers; in
2026 (1,720 to September 2) 68 percent of non-merge commits carry a
`Co-Authored-By: Claude` trailer, first appearing 2026-01-07 in Probes II.
Mechanizations were never in this repository except the 2017–2019 Coq core;
Agda developments live in about fifteen separate `hazelgrove` repositories
(`agda-popl17`, `hazelnut-dynamics-agda`, `hazelnut-livelits-agda`,
`patterns-agda`, `error-localization-agda`, `hazelnut-polymorphism-agda`,
`incremental-statics-agda`, `tylr-agda`, and others).

## Part I. Editor calculi

### POPL 2017 — Hazelnut: A Bidirectionally Typed Structure Editor Calculus

**What the paper defines.** Edit states are Z-expressions (a term with a
cursor). The action language is tree-structured: `move`, `construct`, `del`,
`finish`. Every edit state is well typed because ill-typed subterms are wrapped
in non-empty holes and missing subterms are empty holes. The metatheory
(sensibility, reachability, constructability) guarantees "no meaningless edit
states". SNAPL 2017 is the accompanying vision statement.

**Overall status: Diverged.** The spirit survives (every reachable edit state
gets a type and error information, and the editor never presents a state the
statics cannot process), but almost none of the mechanism does. `dev` is a
tylr-style tile editor, not a Hazelnut-style tree editor.

| Hazelnut concept | Status on `dev` | Evidence |
| --- | --- | --- |
| Edit state = term + cursor (Z-expression) | Diverged. The zipper is token-level over a flat segment of tiles, with caret `Outer \| Inner(int)` and arbitrary range selection. The "indicated" term is derived from the caret's neighbours by a heuristic. | `src/haz3lcore/zipper/ZipperBase.re:12-19`, `CaretBase.re:4-6`, `zipper/action/Indicated.re:37-177` |
| Empty holes | Implemented. Convex grout is the material hole; `MakeTerm` turns it into `EmptyHole`, typed `Unknown(Internal)`. | `tiles/Grout.re:4-12`, `lang/MakeTerm.re:298-302,724`, `src/language/statics/Statics.re:433-440` |
| Non-empty holes wrapping ill-typed terms | Superseded. There is no `NonEmptyHole` constructor. Type errors are marks (`Mark.t`) attached by statics; syntactic junk is `Invalid`, unparsable runs are `MultiHole`. | `src/language/statics/Mark.re:49-104`, `src/language/term/Grammar.re:28-32` |
| Every edit state is typed | Implemented, by completion heuristics rather than by construction. Before statics, `Dump` greedily puts down missing delimiters; `MakeTerm` absorbs anything unparsable into holes. The editing test harness asserts term construction is total after every action across 652 golden cases. | `zipper/Dump.re:12-61`, `MakeTerm.re:1633-1637`, `test/Test_Editing.re:57-71` |
| `construct`, `del`, `finish`, `move child/parent` | Superseded by text-like `Insert(string)` with keyword expansion and backpack put-down, `Destruct(dir)`, and caret movement (`Goal(Hole)` jumps to the next grout). `finish` has no analogue because marks are recomputed. | `zipper/action/Action.re:138-157`, `Insert.re:82-106`, `Move.re:251-275` |
| Type-directed construction | Partial, new. `Introduce` fills an empty hole whose expected type is known with the canonical form (arrow to `fun`, product to tuple of holes, etc.). | `zipper/action/Introduce.re:23-41,100-195`; PR #1416, merged 2025-04-08 |
| Sensibility theorem | Not proved. The enforced invariant is syntactic: after every edit, remold/regrout guarantee adjacent nibs fit, and `Skel.mk` raises `Nonconvex_segment` otherwise. Tile invariants are documented in comments, not checked at runtime. | `tiles/Skel.re:18-19,190`, `zipper/Relatives.re:153-210`, `tiles/Base.re:11-15` |
| Hazelnut-style action tests | Example-based only: `Test_Editing.re` (652 cases in 30 groups), `Test_Indication.re` (164), `Test_Reassociate.re` (29), `Test_Introduce.re` (17), `Test_Undo.re` (6). No property-based tests of the action semantics on `dev`; PR #2374 (open) adds a QCheck action fuzzer. | `test/Test_Editing.re:5348-5380` |

**History.** Hazel 1 (2017–2019) and Hazel 2 (2020–2022) implemented Hazelnut
directly: `src/hazelcore/Action.re`, `Action_Exp.re`, `ZExp`, `UHExp` were all
present at `e8df2ecb0b^1`, and `docs/overview.md` still documents that
architecture. The tylr-derived core entered `dev` on 2022-08-02
(`dd1b7ddbf2`, "rebase haz3l on tests-1", Andrew Blinn) and Hazel 2's core
was gone from `dev` by 2022-09-06 (`e8df2ecb0b`). The POPL 2019 artifact
tags (`popl19-artifact`, 2018-10-19) preserve the Hazel 1 implementation.

**Verdict.** Conceptually realized, formally diverged: spirit fully,
mechanism perhaps one fifth. The one recent step back toward Hazelnut's
type-directed construction is `Introduce` (2025). The SNAPL 2017 vision items
are mostly present as engineering (statics and dynamics of holes, an action
layer, type-directed suggestions, live probes, livelits); the missing item is
a formal calculus with metatheory that the editor is derived from.

### TyDe 2022 tylr, VL/HCC 2023 obligations, OOPSLA 2025 Syntactic Completions with Material Obligations

**What the papers define.** tylr: programs are flat segments of tiles; a tile
is a form's delimiters (a label, possibly multi-token) with children between
them, carrying a mold (sort plus two nibs with shape and precedence).
Multi-delimiter tiles may be present partially as shards, carried in a
backpack. Where adjacent nibs do not fit, grout is inserted; remolding and
regrouting after every edit keep the segment parseable by operator-precedence
parsing. VL/HCC 2023 frames missing shards as obligations the editor tracks.
OOPSLA 2025 generalizes obligations to missing operands, operators,
delimiters and sort transitions, with a molder and a melder (an
error-handling operator-precedence parser), the meldr calculus, and
completion ranking that minimizes obligations.

**Overall status: tylr Implemented (about 80 percent, with deliberate
divergences); OOPSLA 2025 algorithm Separate repo, in development on
unmerged Hazel branches.**

| Concept | Status on `dev` | Evidence |
| --- | --- | --- |
| Tiles, labels, shards, molds, nibs, sorts | Implemented. The whole language surface is the `compound_form`/`atomic_form` tables in `Form.re`. | `tiles/Base.re:3-23`, `Tile.re`, `Mold.re`, `Nib.re`, `lang/Form.re:38-44,318-539` |
| Grout as material holes | Implemented (convex and concave). | `tiles/Grout.re`; `Zipper.re:5-22` seeds one |
| Remolding and regrouting | Implemented, as hand-written per-sort walkers rather than grammar-derived ones. | `tiles/Segment.re:99-727` (remold), `:741-857` (regrout), `Relatives.re:94-210` |
| Operator-precedence parse of a segment | Implemented (shunting-yard over nib shapes and precedences). | `tiles/Skel.re:66-191` |
| Persistent backpack | Superseded. Since PR #1805 (2025-07-24) the backpack is derived from incomplete tiles near the caret; only the decoration persists. | `Zipper.re:854-874`, `Siblings.re:57-62`, `src/web/app/editors/decoration/Backpack.re` |
| Put-down and delimiter matching | Implemented: caret-local put-down, label-based `rescan`, and cross-generation `Reassociate` (PR #2154, 2026-04). | `Zipper.re:929-947`, `Segment.re:938-1029`, `zipper/Reassociate.re` |
| Delayed keyword expansion | Removed in PR #1865 (2025-08-21); expansion and matching are instant. | `Insert.re:82-106` |
| Obligations shown to the user | Partial. Missing shards appear in the backpack decoration; incomplete tiles and concave grout are listed in the Problems sidebar. Nothing is placed in situ as a completion. | `src/haz3lcore/ProblemCollection.re:163-203` |
| Molder, melder, meldr calculus, obligation-minimizing ranking | Missing on `dev`. No `meld` or `molder` identifiers in `src`. Semantics use `Dump` (greedy put-down); the only ranking-like logic is `Reassociate.should_accept_repair`. Reference implementation: `hazelgrove/tylr` (branches `oopsla25`, `tall`, `melds`; `src/core/parser/{Molder,Melder}.re`). | `Dump.re:12-61`, `Reassociate.re:5-11,107-118,629-646` |
| Hazel-native completion engine | Branch only. `CanonicalCompletion.re` (2621 lines) and `CompletionQuery.re` on `origin/completion-provenance` (PR #2374, open, 245 commits ahead); derived grout (`GroutPlace.re`) and the "quiver" UI on `origin/artifact-grout` (PR #2406) and `origin/quiver-assist` (PR #2398). | `git show origin/completion-provenance:src/haz3lcore/derived/CanonicalCompletion.re` |

**History.** The earliest tylr experiment inside this repository is
`3a553b89da` (2021-09-29, David Moon, `src/hzcore/`, branch
`origin/bottom-up-tiles`). `f1e319f3dd` (2022-07-25, "haz3l is go") imported a
tylr snapshot as `src/tylr/` on a side branch; that commit is not an ancestor
of `dev`. haz3l landed on `dev` via `dd1b7ddbf2` (2022-08-02) and
`282d1c8e49` (2022-08-28, "rename Core3 to Haz3lcore"). The tag
`haz3l-july-6-2025` marks `bfae450d96`, the state just before the 2025
editing rework (Backpack II #1805, polyflex #1865). Abandoned or closed:
"Structural Insertion and Delete" (PR #1804, closed 2026-01-29 as no longer
adaptable after instant expansion), "Virtual Grout" (PR #2165, closed
2026-07-31, superseded by #2406). Recent merged work in `dev`: `Triggers.re`
(#1865), `Refractors.re` (#1879, 2026-01), `ProblemCollection.re` (#2176,
2026-04), `Reassociate.re` (#2154, 2026-04), `FastParse.re`/`MarkerParse.re`
(#2426, 2026-08-14).

**Verdict.** The tylr data model and mechanisms have been the editor's
backbone since September 2022. Obligations exist as data (missing shards,
grout) and are displayed, but the OOPSLA 2025 completion algorithm is not on
`dev`; it lives in the separate `tylr` repository, and a Hazel-native
successor is in active development on three open PRs.

**Key files.** `src/haz3lcore/tiles/Base.re`, `lang/Form.re`,
`zipper/Zipper.re`, `tiles/Segment.re`, `zipper/action/Insert.re`,
`zipper/Dump.re`, `lang/MakeTerm.re`, `test/Test_Editing.re`.

## Part II. Dynamics

### POPL 2019 — Live Functional Programming with Typed Holes (Hazelnut Live)

**What the paper defines.** External expressions elaborate to internal
expressions, inserting casts at every consistent-but-unequal boundary
(gradual cast calculus) and wrapping ill-typed subterms in non-empty holes,
with a metavariable context Delta. At runtime, holes carry environments (hole
closures) so evaluation proceeds around them and produces indeterminate
results; hole instances are numbered and inspected in a context inspector.
The headline metatheorem is commutativity of hole filling with evaluation
(fill-and-resume). The artifact (tag `popl19-artifact`, 2018-10-19) is Coq
with extraction and implements exactly this.

**Overall status: Diverged.** Indeterminate evaluation and
"elaborate then run" survive; the specific mechanisms (casts everywhere,
non-empty holes, Delta, hole closures, instance numbering, fill-and-resume)
are gone and their use cases have been re-derived from different primitives.

| Paper mechanism | Status on `dev` | Evidence |
| --- | --- | --- |
| Separate elaborator, external vs. internal language | Superseded. `Elaborator.re` was deleted (`cc620bbbdc`, 2026-04-03, PR #2213 "Elastatics"); `Statics.mk` returns the info map and the elaborated term together, and each `Info.exp` carries `user_term` and `elab_term`. `DHExp` is `include Exp` since PR #1197 (2024-07-31). | `src/language/statics/Statics.re:204-268,4313-4354`, `Info.re:36-41`, `src/language/dynamics/DHExp.re:1-11` |
| Cast insertion at every boundary | Diverged. `fresh_ascription` inserts an `Asc` only for list elements, cons, and `if` branches; `Fun` stores its parameter type; `Ap` and free variables get no wrapper. | `statics/StaticsBase.re:373-385`; call sites `Statics.re:524,578-589,2315-2318` |
| `FailedCast` | Missing. An inconsistent ascription is stuck as an indeterminate `Asc`; ill-typed primitive operations become indeterminate through unboxing failure. | `dynamics/transition/Transition.re:1177-1207`, `Unboxing.re:136-140,185-254` |
| Cast semantics (push through values, `ITApCast`) | Implemented under the name "ascriptions" (rename 2025-05-27, Alexander Bandukwala). | `transition/Ascriptions.re:20-300`, `Transition.re:670-683` |
| Non-empty holes | Missing. Grammar has `Invalid`, `EmptyHole`, `MultiHole`, `DynamicErrorHole`, `Deferral`, `Undefined`; errors are marks in the info map. | `src/language/term/Grammar.re:27-33` |
| Metavariable context Delta | Missing. `Delta.re` deleted `dd4f3b2b8d` (2025-03-26). Holes are identified by syntax id only. | grep `Delta` in `src/language` is empty |
| Hole closures in results | Missing, deliberately. `EmptyHole` is `Indet` with the closure line commented out ("uncomment for hole closures"); `Evaluator.finish` substitutes all closures away. `MultiHole` and `DynamicErrorHole` do retain an environment internally. | `Transition.re:1162-1176`, `Evaluator.re:366-367`, `Substitution.re:16-17` |
| Hole instance numbering, context inspector | Missing. Postprocessing removed `bdf0310d41` (2024-02-08, PR #1197); the Hazel 2 inspector with "closure above observed at" and instance navigation was deleted 2022-09-02 (`9d5e432a3f`). Today's inspector shows only the static context. Stale docstring remains in `ProgramResult.re:5-6`. | `src/web/view/ContextInspector.re:71-104` |
| Final forms: values vs. indeterminate | Implemented as rule classes `Step \| Constructor \| Indet \| Value`; tests over holes report `Indet`. | `Transition.re:135-144`, `Evaluator.re:45-56`, `state/TestStatus.re:112-116` |
| Substitution-based semantics | Superseded by an environment-based big-step evaluator sharing one `Transition` functor with a substitution-mode stepper. | `Transition.re:157-181,396-431`, `Evaluator.re:59,124-140`, `stepper/EvaluatorStep.re:333-385` |
| Fill-and-resume | Missing. Three dead branches: `origin/fill-and-resume-backend` (Jonathan Lam, 2022-03), `origin/fill-and-resume` (Yuchen Jiang, 2022-03), `origin/haz3l-fill-and-resume` (Yanjun Chen, PR #903, closed 2024-02-29). Their preconditions (metavariables, hole environments) were removed by PR #1197. | `git show origin/haz3l-fill-and-resume:src/haz3lcore/dynamics/FillResume.re` |
| Practical successor: incremental evaluation and streaming | Implemented. `IncrEval` memoizes top-level bindings by syntax id with dependency tracking (PR #2222, merged 2026-05-19); the evaluator runs on a yielding trampoline and streams results (PR #2339, 2026-07-30). Checked by a QCheck property that incremental agrees with fresh evaluation after an edit. | `dynamics/IncrEval.re:3-6,313-348`, `evaluation/Trampoline.re`, `test/evaluator/Test_Evaluator_Properties.re:308-309` |
| Small-step stepper | Implemented, plus a scoped filter language (`hide`, `eval`, `pause`, `debug`). Published as "Practical Algebraic Stepping with Scoped Filters" (Fei, Keenan, Omar, arXiv 2605.31517, May 2026). | `stepper/EvaluatorStep.re`, `EvalCtx.re`, `FilterMatcher.re`, `lang/Form.re:401-404`, `docs/stepper-and-filter.md` |
| Probes | Implemented (not in the paper). Any subexpression can be probed; each sample records value, filtered environment, and call stack. This is the de facto replacement for hole closures and the context inspector. | `dynamics/Sample.re:98-118`, `Evaluator.re:199-255`, `projectors/implementations/ProbeProj.re` |

**History.** Hazel 2 moved to an environment model in 2022 (PR #586, closed;
copied into haz3l by PRs #756 and #761, 2022-09). Hole-instance numbering and
postprocessing were removed 2024-02 and the DHExp/UExp merge landed as PR
#1197 (2024-07-31, Matt Keenan). Generalized closures with hole closures
commented out: PR #1453 (2025-02). Cast to ascription rename: 2025-05/06.
Shared `Environment.t` with structure sharing: PR #1985 (2025-10-22).
Elaborator folded into statics: PR #2213 (2026-04-22). Incremental
evaluation: PR #2222 (2026-05). Streaming: PR #2339 (2026-07).

**Verdict.** The liveness payoff of Hazelnut Live is present, but derived
from different primitives: id-keyed incremental evaluation instead of
fill-and-resume, probes instead of hole closures and instance inspection,
unboxing failure instead of non-empty holes and failed casts. Type
preservation for stepping is known not to hold and the corresponding property
test is disabled (`Test_Evaluator_Properties.re:675-689`). Roughly: the
indeterminate-evaluation half is implemented, the cast-and-closure half is
superseded, and the fill-and-resume theorem has no counterpart.

**Test coverage.** `test/Test_Elaboration.re` (59 cases), `test/evaluator/`
(28 files, about 12k lines, including QCheck properties for evaluator and
stepper consistency), `Test_Evaluator_Incremental.re` (38 cases).

### ICFP 2020 — Program Sketching with Live Bidirectional Evaluation (Smyth)

**Status: Missing on `dev`; separate system; six abandoned integration
branches.** No Smyth code has ever been on `dev`. Hazel 2 attempts vendored
the Smyth implementation and wrote term converters: PR #410 "Synthesis
drafting" (2020-07, closed 2021-01), branches `shmyth`, `shmyth-unplugged`
(2020-10/11, "hazel to smyth conversion feature-complete"), PR #455
`shmyth-interactive` (closed 2022-08), PR #472 `shmyth-feedback` (closed
2022-02), PR #473 `smyth-synthesis-contexts` (closed 2022-02-21). The last
trace is `origin/yash/smyth` (`d8859be832`, 2022-10-19), a Reason port dropped
into the haz3l tree with no integration code.

**Key files (dynamics).** `src/language/dynamics/transition/Transition.re`
(header 1-97, hole rules 1162-1176, ascription rule 1177-1207),
`Ascriptions.re`, `Unboxing.re`, `Evaluator.re`, `IncrEval.re`,
`stepper/EvaluatorStep.re`, `Sample.re`; for history,
`git show popl19-artifact:src/semantics/Semantics.v` (lines 971-1450).

## Part III. Static semantics

### POPL 2024 — Total Type Error Localization and Recovery with Holes (marked lambda calculus), with TFP 2024 polymorphism

**What the paper defines.** Part 1: a total marking procedure that decorates
every term with marks (free variable, inconsistent types, inconsistent
ascription, lambda against non-arrow, application of non-function,
inconsistent branches, pair against non-product, projection of non-product),
bidirectional with matched arrow/product, the switch type `?⇒` for unannotated
let-bound variables, and System F marks (free type variable, non-forall).
Metatheory: totality, well-formedness, unicity, mechanized in Agda. Part 2:
type hole inference with provenance-carrying unknown types, constraint
generation, union-find over PotentialTypeSets, and a suggestion UI. TFP 2024
adds explicit polymorphism (forall types, type functions, type application,
matched forall, casts on forall) and proposes editor-inserted implicit type
application.

**Overall status: Part 1 Implemented (about 90 percent, and it is the
architecture of `Statics.re`, not a feature); Part 2 Missing on `dev`
(branch only); TFP 2024 explicit System F Implemented, implicit
instantiation Missing.**

Architecture on `dev`. `Statics.mk` returns the info map and the elaborated
term together (PR #2213 "Elastatics", merged 2026-04-22). Synthesis is
encoded as analysis against `Unknown(SynSwitch)`, literally the paper's
switch type (`StaticsBase.re:247`). The old `Self`/`Mode`/`status` design
became `elab_syn_ty`, `ana`, and `marks: list(Mark.t)`; `Info.is_error` is
"marks is non-empty". Every constructor of every sort has a case ending in
`add`, so every well-formed term gets an info entry, and `CachedStatics`
returns the elaborated term regardless of errors, so errors never block
evaluation (`CachedStatics.re:104-114`). Marks live in a side table keyed by
id; the program text and the elaborated term are unchanged (no marked-term
sort, no casts).

| Paper mark or rule | `dev` realization | Status |
| --- | --- | --- |
| Free variable, synthesizes `?` | `Mark.Free(name)`, `Unknown(Internal)` (`Statics.re:667-681`) | Implemented |
| Inconsistent types | `ExpectationMismatch{ana, syn}` added generically in `add` when `Typ.meet` fails (`Statics.re:224-229`, `StaticsBase.re:319-337`) | Implemented |
| Inconsistent ascription | `ExpectationMismatch` on the annotated pattern (`Statics.re:3618-3633`) | Diverged (localized to the pattern) |
| Lambda against non-arrow, application of non-function, pair against non-product, non-forall marks | `MatchedTyp` tolerant matching yields `?` components, then a generic `ExpectationMismatch` against `? -> ?` (or `? x ?`); the inspector says "inconsistent with expected type" rather than "expected a function" | Diverged (five marks collapse into one) |
| Inconsistent branches | `NoMeet(Id, sources)` for `if` and `case`; also `NoMeet(List, ..)` and `NoMeet(PolyEq, ..)` (`Statics.re:2297-2352`) | Implemented (computed even in analytic mode, unlike the paper's rule) |
| Projection of non-product | Labeled-tuple projection marks `DotOperatorRequiresTuple`, `LabelNotFound`, `BadLabel` (`Statics.re:1254-1469`) | Superseded (no positional projections) |
| Switch type `?⇒` | `Unknown(SynSwitch)` provenance (`Grammar.re:161-164`) | Implemented |
| Free type variable | `TypFreeTypeVariable` (`Statics.re:3780-3790`) | Implemented |
| Exhaustiveness and redundancy (deferred by the paper) | `InexhaustiveMatch`, `Redundant` (see OOPSLA 2023 below) | Implemented, beyond the paper |
| Type hole inference: provenance with hole ids, constraints, PotentialTypeSets, suggestions | Missing. Provenance on `dev` is only `SynSwitch \| Hole \| Internal`; no constraint type, no solver, no suggestion UI | Branch only |

`Mark.t` has 40 constructors (`Mark.re:49-104`), of which four are never
constructed (`IsLivelitName`, `TypDuplicateLabels`, `TypWantTypeFoundAp`,
`TypWantConstructorFoundAp`). There is one warning kind, `UnusedVar`, from
co-contexts (`Warning.re`, `CoCtx.re`). Tests: `test/statics/*` compare an
annotated expected tree of marks against the info map (Sums 49 cases, Tuples
46, Modules 74, Types 26, and others).

Polymorphism (TFP 2024) on `dev`: `Poly(tpat, typ)`, `TypFun`, `TypAp`
(surface `poly X -> T`, `typfun X -> e`, `e@<T>`), `TVarEntry{kind:
Abstract}` for bound type variables, matched forall (`MatchedTyp.re:25-35`),
alpha-aware `meet` (`Typ.re:822-837`), capture-avoiding substitution
(`Typ.re:313-369`), type application by substitution (`Transition.re:632-653`).
Casts on forall are realized as ascription propagation (`Ascriptions.re:112-128`).
Missing: implicit type application (PR #2376, open, atop the unmerged
parameterized-types PR #2254), type-argument inference, let-generalization.
Naming trap: the expression keyword `forall` on `dev` is a Bool-valued
proposition quantifier for the theorem-proving features, not System F; the
type former was renamed `Forall` to `Poly` on 2025-07-22.

**History.** `SynSwitch` predates the paper's write-up: it appears
2022-08-07 (`f540008f5b`) and "actually triggers switch to syn" on 2022-08-19.
`Info.re` was split from statics 2023-02-01 (`6f6e49a679`, Andrew Blinn) with
the `Self`/`Mode`/`status` design; ADTs landed as PR #990 (2023-08-06);
`Mode.re` was deleted 2025-02-26 (`d22a778d64`, Matt Keenan); `Mark.re`
appeared 2026-04-09 and Elastatics merged 2026-04-22. Explicit polymorphism:
PR #1092 (Crazycolorz5, merged 2024-04-18), after Kevin Li's PR #958 and the
Hazel 2 PR #641 (Yuchen Jiang). Type hole inference: nine branches and five
PRs, zero merges. The POPL artifact engine (PotentialTypeSet, provenance with
`exp(u)`, matched-arrow provenances) is on `origin/haz3l-thi-artifact` and
`origin/thi-old-inference-engine` (Raef Maroof, Anand Dukkipati, 2022–2024;
PR #1155 closed 2026-04-21); a rewrite by Alexander Smart is PR #1872 (open
since 2025-08) with integration PRs #2290 and #2322 by Cyrus Omar (May 2026).
Each generation was written against a statics core that was rewritten under
it.

**Verdict.** Part 1 is the most faithfully realized paper in the repository:
a total per-id marking fused with elaboration, with the paper's eight core
marks plus polymorphism marks all present, though five shape-mismatch marks
are not distinguished from plain inconsistency and marks are a side table
rather than a marked-term sort. Part 2 has never been on `dev`. Explicit
polymorphism is complete; the paper's implicit-instantiation proposal is not.

**Key files.** `src/language/statics/Statics.re` (`add` at 204-269, `mk` at
4313), `StaticsBase.re` (`syn` 247, `fixed_typ` 252, `expectation_mismatch_mark`
319), `Mark.re`, `Info.re`, `MatchedTyp.re`, `src/language/term/Typ.re`
(`meet` 773, `subst` 321), `Grammar.re:27-164`; off `dev`,
`git show origin/thi-old-inference-engine:src/haz3lcore/inference/PotentialTypeSet.re`.

### OOPSLA 2023 — Live Pattern Matching with Typed Holes (Peanut)

**What the paper defines.** Pattern holes; a ternary classification of
matches (necessarily exhaustive, indeterminately exhaustive, necessarily
inexhaustive; likewise for redundancy) with errors shown only for the
"necessarily" cases; a constraint language with three-valued satisfaction,
truify/falsify/dual, and the `incon` judgment for deciding validity; and a
dynamics in which a match against an indeterminate scrutinee or hole pattern
stops at the first indeterminate rule. Mechanized in `hazelgrove/patterns-agda`.

**Overall status: Implemented in behaviour (about 80 percent), Diverged in
formulation (about 30 percent).**

| Paper feature | Status on `dev` | Evidence |
| --- | --- | --- |
| Pattern holes type-check | Implemented. `EmptyHole`, `MultiHole`, `Invalid` patterns get constraint `Hole(None)`. No non-empty pattern hole form; marks and `Hole(Some _)` stand in, and that payload is never read. | `Statics.re:2821-2825,2928-2950`, `Coverage.re:781,909` |
| Constraint language with truify/falsify/dual and `incon` | Superseded. `Coverage.re` is a Maranget pattern-matrix checker whose constraints are pattern shapes (`Truth \| Hole \| literals \| Ap \| Tuple`); no negation, conjunction, disjunction, or entailment. Holes are wildcards for exhaustiveness and bottom for redundancy. | `Coverage.re:1-5,11-36,781-790,909-972` |
| Errors only when necessarily inexhaustive or redundant | Implemented. `Mark.InexhaustiveMatch` (with a synthesized missing-pattern witness) and `Mark.Redundant`; checks also run on `let` and `fun` patterns. Peanut Figures 1–3 are literal regression tests. Output is binary, so the "indeterminately" classes are not exposed. | `Statics.re:2392-2438,1881-1896,2120-2133`, `Mark.re:54,100`, `test/Test_Coverage.re:137-262` |
| Three-valued run-time matching | Implemented: `DoesNotMatch \| IndetMatch \| Matches`; unboxing failures on non-values and stuck ascriptions yield `IndetMatch`. | `transition/Unboxing.re:53-57,71-255`, `PatternMatch.re:5-16` |
| Match stops at first indeterminate rule | Implemented. | `Transition.re:1102-1138`, `test/evaluator/Test_Evaluator_Match.re:150-172` |
| Hole patterns indeterminately match | Diverged. Since `d87a27aa9d` (2024-04-03, PR #1197) hole patterns are grouped with `Wild` and match everything; Hazel 2 returned `IndetMatch`. | `PatternMatch.re:27-30` |
| Pair pattern matches non-pair indeterminate scrutinee via projections | Missing; pursued in open PRs #1910 and #2247. | `Unboxing.re:158-159` |
| Casts in matching | Superseded by ascriptions; a stuck ascription reaching a literal pattern is `IndetMatch` (issue #1640 debated `DoesNotMatch`). | `PatternMatch.re:113`, `Ascriptions.re:22-200` |

**History.** The paper's constraint machinery was written for Hazel 2 by
Yuning30 (2020-10) and Eric Griffis (`origin/match`, PR #585, closed
2022-09-30, never merged). It was ported to Hazel 3 by DavidFangWJ (`Incon.re` authored
2023-07-09 as `0beba60e47`, PR #1094 into a feature branch) and Jiezhong Yang
(PRs #1124, #1144, and #1114). It reached `dev` with PR #1114 on 2024-05-19
(`0beba60e47` is not an ancestor of `dev` before that merge), so the paper's
algorithm was on `dev` from May 2024 to February 2025. Cyrus Omar
replaced it with the Maranget formulation in PR #1491 (merged 2025-02-24,
motivated by exponential blowup, issue #1473), creating `Coverage.re`. Missing-
pattern examples came in PR #1667 (2025-09-22). Yongwei Yuan, the paper's first
author, has no pattern-matching commits in this repository.

**Verdict.** The user-visible promises hold and are tested against the
paper's own figures; the paper's formulation was replaced by a different
algorithm justified by the paper, and the dynamics drifted in one place (hole
patterns now match).

**Key files.** `src/language/statics/Coverage.re`, `Statics.re:2392-2438`,
`src/language/dynamics/transition/PatternMatch.re`, `Unboxing.re`,
`test/Test_Coverage.re`; for the paper's algorithm,
`git show origin/match:src/hazelcore/Incon.re`.

### OOPSLA 2025 — Incremental Bidirectional Typing via Order Maintenance

**What the paper defines.** Authors: Thomas Porter, Marisa Kirisame, Ivan
Wei, Pavel Panchekha, Cyrus Omar. A marked and annotated lambda calculus in
which every expression carries its analyzed and synthesized types; a
small-step update-propagation dynamics over dirtied annotations; and Malcom,
an OCaml implementation using order-maintenance timestamps, binder pointers,
and a priority queue of dirty locations, evaluated at about 276x speedup over
from-scratch marking. Mechanized in Agda. The paper says Hazel integration is
future work.

**Overall status: Missing on `dev`; Separate repo.** Nothing of the algorithm
(order maintenance, binder and occurrence pointers, dirty priority queue, the
annotation discipline) exists on `dev` or on any of the 686 remote branches.
The workbench and proofs are `hazelgrove/incremental-hazelnut` and
`hazelgrove/incremental-statics-agda`, both quiet since spring 2025.

What `dev` does instead:

| Mechanism | Status | Evidence |
| --- | --- | --- |
| Re-typing after an edit | Whole-program. `Statics.mk` is one top-down pass wrapped in `Core.Memo.general`, memoizing identical inputs rather than updating; `CodeWithStatics` rebuilds on every edit. | `Statics.re:4313-4355`, `CachedStatics.re:139-155`, `src/web/app/editors/code/CodeWithStatics.re:150-167` |
| Downstream change detection | Physical-equality comparison of info map, dynamics map, and elaborated term in `CachedSyntax.calculate`. | `CachedSyntax.re:119-147` |
| Constant-factor work | No deep type normalization in statics (PR #2404, 2026-08-12: keystroke stall from about 2 s to about 100 ms); linear-time `FastParse` for text inserts (PR #2426). | `FastParse.re:1-26` |
| Incremental evaluation (a different problem, one the paper lists as open) | Implemented: `IncrEval` reuses a sub-expression when the call stack is empty, the elaboration is deep-equal, no co-context variable is dirty, binding ids are unchanged, and probe targets are unchanged; function bodies are a deferred boundary. Streaming via a yielding trampoline. | `IncrEval.re:3-6,313-348`, `Evaluator.re:257-331`, `Trampoline.re:47-115`; PRs #2222 (2026-05-19), #2339 (2026-07-30) |
| Benchmark CI | Partly stranded: `perf.yml` runs `bench/build-and-run.sh`, which exists only on `origin/perf-bench`. | `.github/workflows/perf.yml:122,134`, `bench/` |

**Verdict.** Not started in this repository. A parallel audit found the
in-flight answer to statics latency is compositional rather than
order-maintained: draft PR #2469 "Modular editors" (Andrew Blinn, 2026-08-27) re-analyzes
only changed top-level definitions. We did not independently verify that
PR's measurements.

## Part IV. Editor services and data

### PLDI 2021 — Filling Typed Holes with Live GUIs (livelits)

**What the paper defines.** A livelit is a GUI literal that fills a typed hole
and expands to code of a declared type. Definitions (`livelit $name at Typ
{...}`) provide `Model`, `Action`, `init`, `update`, `view`, `expand`, and a
capture context. Livelits take parameters (with abbreviations for partial
application) and contain splices: typed holes inside the GUI whose values are
live-evaluated in the surrounding context and passed to the expansion as
function arguments (hygiene). The expansion is validated at each invocation
site against the declared type. The calculus is mechanized in
`hazelgrove/hazelnut-livelits-agda`.

**Overall status: Partial (about one third).** `dev` has the shell:
builtins only, model stored in program text, expansion performed by statics.
Everything distinctive about the paper is absent from `dev` and lives either
on the archived Hazel 2 `origin/livelits` branch or on the open 2026 PR chain.

| Paper feature | Status on `dev` | Evidence |
| --- | --- | --- |
| Definition form, user-defined livelits | Missing. The only `LivelitEntry` construction site is `Builtins.re:31`, from three OCaml modules `Slider`, `Emotion`, `Js`. Branch only: `origin/user-livelits` (PR #2411) adds `let ^name = { type Model ...; let expand ... }`. | `src/language/builtins/Builtins.re:31`, `src/language/Livelit.re:452-454`, `statics/LivelitCtx.re:17-55` |
| Model / action / view / expand architecture | Implemented for builtins. No `UpdateCmd`/`ViewCmd` monads (they only mediate splices, which are absent). An interaction is a syntax edit to the model argument via `SetSyntax`. | `LivelitCtx.re:17-29`, `projectors/implementations/LivelitProj.re:52-64,98-114` |
| Parameters and abbreviations | Missing. `raw_livelit` has no parameter field; statics handles exactly `Ap(LivelitName(s), arg)`; a bare name synthesizes `expansion_t`, not a function type. PR #996 (2023) closed unmerged. | `Statics.re:497-509,1591-1598`, `term/Grammar.re:49` |
| Splices and splice environment | Missing. No `SpliceRef`, `new_splice`, `set_splice`, or `eval_splice`; the projector API has no nested editors. PR #1660 "Splices" (projector rewrite) closed unmerged 2026-03-31. | grep `splice` in `src` finds only unrelated uses |
| Model storage | Implemented, differently. The model is the literal argument in program text (`^slider(50)`); the projector's own model is `unit`. Typing `^name` then space inserts `model_default` and auto-projects. | `LivelitProj.re:8`, `zipper/action/Triggers.re:177-201`, `test/Test_MakeTerm.re:265-268` |
| Expansion typing (Fig. 5 premise 5) | Diverged. Statics analyzes the argument against `model_t`, runs `expand` on the surface term, and emits the expansion with `elab_syn_ty = expansion_t` and no marks. The declared type is asserted, never checked; `Mark.IsLivelitName` is dead code. PR #2488 (open, Matthew Hammer) adds the invocation-site check `BadLivelitExpansion`. | `Statics.re:1591-1655`, `Mark.re:64-69` |
| Live evaluation into the GUI | Missing for livelits: `LivelitProj.dynamics = false`. Probes provide per-closure values generally, but are not wired into livelit views. | `LivelitProj.re:80,84-89` |
| Invocation syntax | Diverged: `^name`, not `$name`. | `lang/Token.re:240-243` |
| Livelit context | Partial. Livelits live in the ordinary typing context as `LivelitEntry`, populated only by builtins; shown in the context inspector and offered by TyDi. | `statics/Ctx.re:37-41,102-108`, `TyDi/TyDiCtx.re:41-44` |
| Builtin view on `dev` | Probably broken (static evidence). Since PR #2078 (2026-02-04) statics stores a `Projector(...)` wrapper in `user_term`, and `LivelitProj.get_model` does not unwrap it, so builtin livelits likely render "No livelit found". The fix (`strip_wrappers`) exists only on `user-livelits` (`a39caeb898`). Not runtime-verified. | `LivelitProj.re:12-23,125-127`, `MakeTerm.re:1428-1458` |

**Projectors, the substrate.** Projectors (PR #1218 "Leaf Projectors",
Andrew Blinn, merged 2024-08-07) are the general mechanism that replaces a
piece of syntax with a GUI: `module type Projector` with `model`, `action`,
`init`, `view`, `update`, `dynamics`, `elaborate_syntax`. Kinds on `dev`:
`Fold | Probe | Statics | Checkbox | Slider | SliderF | Card | Livelit |
TextArea | Table | Csv`. Probes and Statics are "refractors" (additive
decorations, not syntax-replacing). A livelit is the one kind whose meaning is
not its syntax: statics replaces `^name(model)` by `expand(model)`. Rich
probes (PR #1998, 2026-05-26) are the paper's `result_view` idea detached from
livelits. See `src/haz3lcore/projectors/ProjectorBase.re:181-246`,
`src/language/ProjectorKind.re:8-36`.

**History.**
- Hazel 2 artifact: `origin/livelits` (3701 commits, 2019-08 to 2024-07)
  contains the full paper machinery (`LivelitDefinition.re`, `SpliceGenCmd`,
  `LivelitAbbrev`, and `Statics_Exp.syn_ApLivelit`, which analyzes the
  expansion against `Arrow(captures_ty, expansion_ty)`). PR #96 (opened
  2020-02-01) was closed unmerged on 2022-02-21 when `dev` moved to Hazel 3.
- First Hazel 3 attempt: `origin/haz3l-livelits` (Alexander Bandukwala, 2023;
  PRs #955 and #996) closed 2024-07-25, two weeks before projectors merged.
- What landed: PR #1465 "Livelits" (Gregory Croisdale, merged 2025-05-08),
  listing user-defined livelits, splices, parameters, and pattern livelits as
  future work.
- Open lines: `exolivelits` (PR #1911, iframe-hosted external livelits with a
  JSON protocol, a different strategy from the paper); the chain `hazel-html`
  (PR #2115) to `user-livelits` (PR #2411) to `livelit-expansion-typing`
  (PR #2488) to `livelit-error-dedup` (PR #2489), which realizes definitions,
  invocation-site expansion typing, and view sampling, still without splices
  or parameters. Three PRs must land before user-defined livelits reach `dev`.

**Verdict.** The projector substrate the paper needed is mature and merged.
The livelit layer on top is about one third of the paper on `dev`, about two
thirds on the open PR chain, and the splice third has no merged foundation.
`docs/livelits.md` is accurate about the three stated gaps but silent about
unchecked expansion typing, the absence of live evaluation, and the likely
view regression; its `BuiltinLivelit` listing has also drifted from
`LivelitCtx.re`.

**Key files.** `docs/livelits.md`, `src/language/statics/LivelitCtx.re`,
`src/language/Livelit.re`, `Statics.re:497-509,1591-1655`,
`zipper/action/Triggers.re:177-201`,
`projectors/implementations/LivelitProj.re`, `projectors/ProjectorBase.re`;
off `dev`: `git show origin/livelits:src/hazelcore/Statics_Exp.re` (about
line 763) and `git show origin/livelit-expansion-typing:src/language/statics/UserLivelit.re`.

### OOPSLA 2024 — Statically Contextualizing Large Language Models with Typed Holes (ChatLSP), with VL/HCC 2022 and Onward! 2022

**What the paper defines.** Hole filling by an LLM whose prompt is
contextualized by the language server: the expected type at the hole, the
transitively reachable type definitions, and value headers retrieved by
type-directed relevance, followed by up to two error-correction rounds driven
by static errors in the completion. The protocol is "ChatLSP", five LSP-style
methods. VL/HCC 2022 describes an assistant architecture (analyzers,
synthesizers, scorers, ranker, presenter) prototyped as the Hazel Assistant.
Onward! 2022 describes ExplainThis, contextualized documentation.

**Overall status: ChatLSP Superseded (on `dev` 2025-07-09 to 2026-03-19);
VL/HCC architecture Branch only, with TyDi as a narrow descendant;
ExplainThis Implemented.**

| Mechanism | Status on `dev` | Evidence |
| --- | --- | --- |
| Expected type at the hole in the prompt | Missing. The agent receives a whole-program snapshot with folded bindings, static errors and test results every turn; no inferred types are printed into it. | `src/web/view/agentCore/Message.re:284-343`, `CompositionCore/CompositionView.re:150-184` |
| Type-directed retrieval of definitions and headers | Missing on `dev`. `RelevantTypes.re` and `RelevantValues.re` were merged in PR #1575 (2025-07-09) and removed from `dev` by PR #2131 (2026-03-19; the deleting commit `00d7656fcb` was authored 2025-12-01 on the agent branch). The substitute is agent-driven `expand`/`collapse` over a binding-path tree. | `git show 2b4a189132:src/web/app/helpful-assistant/ChatLSP.re`; `ToolJsonDefinitions/ViewTools.re`, `HighLevelNodeMap.re:885-921` |
| Error-correction rounds (max 2) | Diverged. Instead of repairing a candidate completion, every structural edit tool re-runs statics and refuses edits that raise the local error count; diagnostics are re-sent each turn; the agentic loop is open-ended. | `CompositionGo.re:125-190`, `test/Test_AgentTools.re` (244 cases) |
| ChatLSP protocol / language server | Missing. `src/CLI` (2026-01) is a batch tool for external agents (`analyze`, `format`, `probe`, `grade-*`), not a server. The `??` LLM-hole token survives as a dead form. | `src/CLI/Cli.re:510-634`, `lang/Token.re:341`, `Form.re:664` |
| What is there instead: "Filbert", a tool-using structure-editor agent | Implemented (about 12k lines): structural edit tools over binding paths, probe/statics/projector overlay tools, workbench tasks, session modes, compaction, streaming, floor-only prompt caching. Sole provider: OpenRouter. | `CompositionCore/prompt_factory/CompositionPrompt.re:6,383-396`, `src/util/OpenRouter.re:463`, `agent-docs/prompt-caching-findings.md` |
| TyDi type-directed completion | Implemented, active, tested (45 cases). One ghost suggestion, lexicographic ranking, insertion only: type-consistent variables, constructors, applications, module fields, forms. | `src/haz3lcore/TyDi/TyDi.re`, `TyDiCtx.re:27-190`, `Editor.re:195-210`, `test/Test_TyDi.re` |
| VL/HCC 2022 synthesizers, scorers, ranker | Branch only: `origin/assistant-1` (341 commits, PR #543 closed 2022-09-30). The paper never uses the name "TyDi". | `git show origin/assistant-1:src/hazelcore/assistant/Suggestion.re` |
| ExplainThis | Implemented and faithful: specificity selection (a menu, not a slider), hover colour highlighting, live-evaluated locked example cells, thumbs feedback (stored locally), 57 data modules covering 154 forms, property-based and golden tests. | `src/web/app/explainthis/ExplainThis.re:10-110,232-400`, `test/Test_ExplainThis.re` |

**History.** Hazel 2 assistant (2020–22, `origin/assistant-base`,
`origin/assistant-1`; Hannah Potter, Andrew Blinn). Hazel 3: TyDi (PR #1024,
merged 2023-12-05). The OOPSLA 2024 artifact is `origin/llama-lsp-lookahead`
(Blinn, 2023-08 to 2024-07, 121 commits; `LS/*.re` completion server,
`ChatLSP.re`, `Filler.re`, MVUBench, OpenAI and Azure GPT-4 calls), never
merged. A single-file port (`ChatLSP.re`, PR #1492) reached `dev` via PR #1575
"LLM Hole fillings and Assistant Sidebar" (Russell Rozenbaum, 2025-07-09), and
was removed from `dev` by PR #2131 "Coding Agent" (2026-03-19; deletion
authored 2025-12-01 as `00d7656fcb`) as the team pivoted to the coding agent
(PR #2210 followed on 2026-07-29). The ChatLSP shell and the agent work
therefore overlapped for about three months rather than handing off.
ExplainThis: Hannah Potter, 2022-08 onward ("LangDoc"), refactor PR #993
(2024-01), simplification PR #2424 (2026-08-17).

**Verdict.** The paper's thesis ("AIs need IDEs") is kept, but the interface
changed from retrieval into a single prompt to agent-pulled context plus
type-checker-gated tools. None of the four defining ChatLSP mechanisms is on
`dev` as described. ExplainThis is essentially complete.

**Key files.** `CompositionCore/prompt_factory/CompositionPrompt.re`,
`agentCore/Message.re:284-343`, `CompositionGo.re:125-190`,
`agentCore/AgentSend.re`, `src/util/OpenRouter.re`; `TyDi/TyDi.re`;
`explainthis/ExplainThis.re`.

### OOPSLA 2026 — Interactive Data Analysis with Lively Typed Tables (Hazel Lab)

**What the paper defines.** Authors: Alexander Bandukwala and Cyrus Omar.
Tables as lists of labeled tuples with five primitives (extension and four
contractions), `to_lvs`/`from_lvs`, broadcast projection `table.label`; live
typing (synthesize a dynamic type for every statically unknown expression
from observed inhabitants, re-run statics, show secondary marks); rich probes
whose table view rewrites the underlying syntax; a CSV loader; evaluation on
B2T2 (49 signatures; 38 ensures enforced statically, 96 with live typing) and
a seven-participant study.

**Overall status: about two thirds Implemented; live typing is the missing
third (Branch only).**

| Paper feature | Status on `dev` | Evidence |
| --- | --- | --- |
| Labeled tuples in expressions, patterns, types; `t.l` | Implemented (PR #1235, Anthony Li, merged 2025-02-11) | `src/language/term/LabeledTuple.re`, `TermBase.re:211-229`, `Statics.re:906-1217` |
| Extension `...` | Implemented | `lang/Form.re:469,472`, `Statics.re:835-848` |
| Contractions, `to_lvs`, `from_lvs`, `group_by_label` with custom statics | Implemented with the paper's names (PR #1555, 2025-08-20) | `builtins/BuiltinsTupleOperations.re:7-258`, `statics/CustomStatics.re:89-121`; 60 statics and 18 evaluator tests |
| Broadcast projection over a list of tuples | Implemented | `Statics.re:1254-1280` |
| CSV loader as a projector | Implemented (PR #1860, 2025-11-25): all fields strings, header toggle, no schema induction | `projectors/implementations/CSVProjector.re:25-91,130-161` |
| Table projector for results | Implemented; auto-projection gated by `project_tables` | `TableProj.re:20-28`, `CoreSettings.re:19,35`, `ExpToSegment.re:1210-1218` |
| Rich probes with a table renderer whose actions rewrite syntax | Implemented (PR #1998, 2026-05-26). Registry holds exactly one renderer, "table". | `RichProbe.re:19-52`, `RichProbeRegistry.re:13-15`, `TableRenderer.re:151-451`, `TableTransforms.re:138-447` |
| Live typing | Missing on `dev`. Draft PR #1988 (`origin/dynamic_statics`, 323 commits ahead, `LiveTyping.re`, `DynamicTypInfer.re`), rebased 2026-09-03, deliberately excluded from PR #1998. Rich-probe menus fall back to a type synthesized from the sample value. | `TableTransforms.re:58-80`; `git show origin/dynamic_statics:src/language/statics/LiveTyping.re` |
| B2T2 benchmark and datasheet | Implemented as documentation slides: 63 `.hz` files covering all 49 signatures, all 14 error scenarios, 7 of 8 programs (`sampleRows` omitted as impure), datasheet dated 2025-11-05. Slides carry `enforced=("static"\|"live"\|"no")` tags; the "live" tags describe behaviour a `dev` build cannot show. | `src/b2t2/{Slides.re,Datasheet.md}`, `hazel-programs/docs/b2t2/*.hz` |

**History.** Labeled tuples: abandoned 2021 attempt (`origin/labeled-tuples`),
restarted by Anthony Li 2023–2024, merged 2025-02-11. Tables PR #1555 (2025-03
to 2025-08). CSV, B2T2, `pivot_table` and `unique` (PR #2005), table
projector and rich probes (2026-03 to 2026-05). Study builds live on
`origin/tables-study` and related branches (300 to 500 commits ahead).

**Verdict.** Contribution 1 and the CSV loader are fully on `dev`;
contribution 3 is on `dev` with one renderer; contribution 2 is not, so the
paper's headline live-typing numbers are not reproducible from `dev`.

**Key files.** `LabeledTuple.re`, `BuiltinsTupleOperations.re`,
`CustomStatics.re`, `CSVProjector.re`, `TableRenderer.re`,
`TableTransforms.re`, `RichProbe.re`, `src/b2t2/`, `docs/rich-probes.md`.

## Part V. Collaboration, teaching, and vision

### POPL 2025 — Grove: A Bidirectionally Typed Collaborative Structure Editor Calculus

**What the paper defines.** Authors: Adams, Griffis, Porter, Satish, Zhao,
Omar. Edits as commuting graph patches (a CmRDT over a labeled multigraph),
decomposition of graphs into groves (trees plus explicitly represented
conflicts), and a total bidirectional marking type system over groves.
Mechanized in Agda; implemented as the OCaml "Grove Workbench" with a
js_of_ocaml collaborative editor. The paper says Hazel integration was blocked
by editor churn.

**Status: Separate repo; nothing in this repository.** The implementation is
`hazelgrove/GRV` (created 2020-04-14, last push 2024-09-20; `Grove.mli`
declares the paper's decomposition into multiparented, deleted, reachable,
and wreath components). `git log --all -i --grep grove` in the hazel repo
returns only `hazelgrove` URLs. `dev` has no multi-user, network-sync, or
conflict-representation code; persistence is a single IndexedDB database
(`src/web/HazelDB.re`, PR #2186), JSON export/import (`src/web/Export.re`),
and URL-encoded scratchpad text (`ScratchMode.re:500-533`, PR #1441).
Collaboration work exists only on unmerged branches and is architecturally
unrelated to Grove: Automerge/Patchwork whole-piece sync (`origin/patchwork`,
Andrew Blinn, 2026-01/02) and Automerge data projectors (`origin/automerge-proj`,
David Moon, 2026-02 to 2026-06).

### VL/HCC 2025 — Hazel Deriver

**Status: Implemented and merged** (PR #1302 "Derivation trees", Zhiyao Zhong,
opened 2024-05-04, merged 2026-04-27, about 41.6k lines added). Derivation
sorts inside Hazel (`Sort.Drv`, seven sub-sorts), eight rule sets
(`RuleImage.re:185-193`: propositional logic, AL, ALB, ALF, ALFp, ALFA,
RecursiveALFA, GradualALFA), 147 rule constructors with a spec DSL, live
per-node verification (`Correct | PartialCorrect | Incorrect | Pending`,
`DrvGrading.re:81-96`), abbreviations, quotation forms, scratchpad and
exercise kinds, and CLI grading. All three limitations in `docs/deriver.md`
were verified in code (sub-sort collapse to `Exp` at the mold level, closed
rule variants, no Menhir support). Gaps: unit tests cover only propositional
logic (`test/Test_Derivation.re`, 42 cases); no shipped exercise for AL, ALB,
ALFA, or GradualALFA; derivation grading is all-or-nothing
(`GradeExercise.re:41-50`); `docs/deriver.md` mentions a `strip_abbr` function
that does not exist. Note that `src/language/proof/*` is not part of the
Deriver; it supports Matt Keenan's stepper-based Hazel Prover (theorem forms,
proof steps; PRs #1533, #1835), which is the target of HATRA 2024's design
criteria.

### HATRA 2020 — Hazel Tutor, and the teaching infrastructure

**Status: Diverged.** The paper's Cursor Inspector survives (`CursorInspector.re:142-154`,
plus the inline `Statics` refractor). Its Strategy Guide (type-driven,
pedagogically grouped hole-filling suggestions that the student must type)
was never built; TyDi and the coding agent are the nearest analogues and are
productivity tools. What `dev` has instead is mature assessment
infrastructure: exercise specs with prompts, hidden tests, mutation testing,
syntax predicates and point distributions (`src/web/exercises/CodeExercise.re:40-53`,
`CodeGrading.re`, `SyntaxTest.re`), instructor and student builds, tutorial
lessons (PR #1760, 2025-08), three exercise kinds (code, derivation, theorem),
and an OCaml CLI grader (`src/CLI/Grade.re`, replacing a Python grader
2026-04-25). Exercises date from PR #682 "Haz3l School Mode" (2022-08-06) and
Cyrus Omar's September 2022 series; the tag `school-done-milestone` is
`9fc6a502f4` (2022-10-06).

### HATRA 2025 — Decomposable Type Highlighting

**Status: Branch only.** Max Carroll's `origin/type-slicing-v2` (274 commits
to 2026-07-30) adds `src/language/statics/Slice.re`, a cursor-inspector slice
UI, and eight test files; nothing is on `dev`. `dev` has variable-binding
highlighting (`VarHighlight.re`, PR #2187, 2026-03) and ExplainThis colouring
instead.

### Vision papers (SNAPL 2017, PROPL 2024, PROPL 2025)

Live evaluation, holes, projectors and livelits, editor services,
documentation slides and local persistence are present. Collaboration as a
shared medium, Fairground, persistent identifiers, and any server-side or
distributed execution are absent from `dev`.

## Part VI. Modules and type members

### Summary

Hazel today has namespace sugar, not a module system. A module is parsed into
its own sort but is lowered by statics to nested `let`/`type` bindings ending
in a labeled tuple, and its type is that labeled product. Signatures are
desugared to labeled products, dropping every `type` entry. "Type members"
exist only as `type T = ...` aliases in module bodies, surfaced as `M.T`
through a type-alias injection at `let` sites. There are no abstract types,
no sealing, no width subtyping, no type members in signatures, no exported
constructors, and no module-specific dynamics. Three designs have existed;
the merged one (2026) is the simplest and best integrated with the editor,
and is behind the unmerged 2023–24 design on module types.

### Today on `dev`

**Sorts and terms.** `Sort.t` gains `Mod | Sig | MPat` (`src/language/term/Sort.re:10-12`).
Expressions gain `Module(list(mod_t))` and `ModuleExp(mpat, exp, exp)`;
types gain `Sig(list(sig_t))` (`Grammar.re:77-78,118`). Module items are
`ModLet(pat, exp) | ModType(tpat, typ) | ModExp(exp) | ModuleMod(mpat, exp)`
plus holes; signature items are `SigLet(pat) | SigType(tpat, typ)` plus holes;
module patterns are `Var | Asc(mpat, typ)` plus holes (`Grammar.re:131-153`).
There is no abstract-type item: `SigType` requires `=` (`lang/Form.re:539`),
so `type T;` cannot be written.

**Forms and parsing.** Module and signature forms are "heterogeneous prefix"
forms whose body sort differs from their out sort (`Form.re:525-539`, helper
`mk_pre_c'` at 69-78, `Mold.mk_pre'` at `tiles/Mold.re:40-55`). `;` inside
`{}` is `ModSeq`/`SigSeq` at precedence `mod_seq = 47`, looser than `let`
(`lang/Precedence.re:109-112`); the remolders return early on `;` under a
Mod/Sig parent so the parent can claim it (`tiles/Segment.re:261-275,445-458`),
and `Skel.mk` takes a `~sort` so grout between items gets the same precedence
(`tiles/Skel.re:96-108`). `MakeTerm` flattens the `MultiHole` chain produced
by `;` into a list, wrapping stray expressions as `ModExp` (`MakeTerm.re:69-105`),
and absorbs the brace and semicolon ids onto the `Module` node (`:243,647-660`).

**Statics.** `Statics.re:2656-2687` lowers a module with
`ModuleHelpers.lower` (`ModuleHelpers.re:308-326`): each `ModLet` becomes a
`Let` carrying the item's id, `ModType` a `TyAlias`, `ModExp` a `let _ =`,
and the body ends in `(x=x, y=y, ...)` over the names not re-bound later
(shadowing: last binding wins, `:111-151`). If the module is analyzed against
a labeled product, the expected field types are ascribed onto the `let`
patterns so errors land on definitions rather than on the synthetic tuple
(`:170-181,265-273`). The module's own type is built from the export
patterns' types (`module_actual_type`, `:402-428`), and item infos are
re-classified with `Cls.Mod` so the cursor inspector shows "Let declaration"
(`:378-399`). `module M = def in body` is a `Let` in disguise
(`Statics.re:2688-2736`). `Typ.desugar_sig` (`Typ.re:615-656`) converts
`SigLet(x : T)` to `TupLabel(x, T)` and returns `None` for `SigType`,
holes, and non-variable patterns (issue #2242). Width subtyping is absent
because `Typ.meet` on products requires equal arity (`Typ.re:858-879`);
three "limitation" tests assert this (`test/statics/Test_Statics_Modules.re:376-394`).

**Qualified type access `M.T`.** In the `Let` case, if the pattern binds
exactly one variable and the definition is a literal module,
`ModuleHelpers.collect_type_exports` gathers the body's type aliases into a
labeled product `(T=Int, ...)` and injects it as a type alias named `M`
(`Statics.re:2087-2116`, `Ctx.extend_alias`). If the definition is a variable
bound to a module, its exports are copied under the new name. `M.T` parses
as `ProdProjection(Var M, Label T)` (type-level `.` came in PR #1946,
2025-10-01, four months before modules) and resolves through
`weak_head_normalize`, `lookup_alias`, `project_type` (`Typ.re:407-411,473-524`).
Consequences: the injection fires only at `let`/`module` sites with a literal
or variable right-hand side, so module-typed parameters and computed modules
lose their type exports; and the bare module name becomes a valid type
denoting its export product (a review item Cyrus Omar raised on PR #2123
that was deferred).

**Dynamics.** None specific to modules. Elaboration is the checked expansion
with signature-derived ascriptions stripped (`ModuleHelpers.re:330-375`);
`Module`/`ModuleExp` reach the evaluator only as pass-throughs and an
`Indet` arm with the comment "Modules should be expanded before reaching
dynamics" (`Transition.re:1253-1260`). Probes inside module bodies work
because items are real `let`s with preserved ids.

**Editor integration.** TyDi completes `m.` in expression position from
`dot_labels` and `M.` in type position from `LabelProjectionExpected`
(`TyDi/TyDi.re:46-67`); qualified value completion is one level deep
(`TyDiCtx.re:112-114`). Jump-to-definition resolves capitalized module names
(`Info.re:364-372`) but not through `M.x` (issue #2252). The Menhir parser
supports all module forms (`Parser.mly:285,472-518`); two round-trip tests
are skipped because Menhir yields `Constructor("M")` where the editor yields
`Var("M")`. `module` became a reserved keyword in PR #2270 (2026-05-12).

**Behaviour verified by running the CLI.** Using the built `hazel analyze`
from a tree one commit off `dev` (the only difference in `Statics.re` is a
memoization module name):

| Program | Result |
| --- | --- |
| `module M : { type T = Int; let x : T } = { let x = 1 } in M.x` | error "Type variable T is not bound": a signature's own `type` entry does not bind |
| `module M : { type T = Int; let x : T } = { type T = Int; let x = 1 } in M.x` | no error: the signature's `T` is captured by the module body's `T` |
| `let f = fun (m : { let x : T }) -> m.x + 1 in f({ type T = Bool; let x = 1 })` | error "Expecting type T but got inconsistent type Int": `T` resolved to the argument's `Bool` |
| `let m : { let x : Int } = { let x = 1; let y = 2 } in m.x` | error: no width subtyping |
| `module M = { type T = Int; let x : T = 1 } in let y : M.T = 2 in y` | no error: qualified type access works |
| `let g = fun _ -> { type T = Int; let x = 1 } in let m = g(()) in let z : m.T = 3 in z` | error "Expected a tuple type, found type m": computed modules lose type exports |
| `module M = { type S = Circle(Int) + Square(Int) } in M.Circle(1)` | error "Label not found": constructors are not exported (issue #2243) |

The first three rows show that dropping `SigType` is not merely a missing
feature: a free type name in a signature is captured by whatever the checked
value happens to declare. `docs/modules.md` describes this as the entry being
"dropped", which understates it.

**Tests.** `test/statics/Test_Statics_Modules.re` (74 entries: well-typed,
signature annotation, type errors, limitations, keyword, qualified types,
aliasing), `test/evaluator/Test_Evaluator_Modules.re` (17), plus module cases
in elaboration, MakeTerm, Menhir, editing, pretty-printing, equality,
abbreviation, indentation, TyDi, unused-warning, and probe suites. Not
covered: any signature `type` semantics, abstract types, module-typed
parameters or returns, constructors through `M.`, error-location attribution,
functors, `include`, `open`.

**Discrepancies between `docs/modules.md` and the code.** The referenced
`plans/module-future-work.md` exists on no branch (`plans/` is gitignored and
was removed from the tree on 2026-02-04); the same dead link is in
`Test_Statics_Modules.re:343`. The document still describes a second
expansion in `Elaborator.re`, deleted 2026-04-03. The doc slide is now
`hazel-programs/docs/reference/modules.hz`, not `src/web/init/docs/Modules.ml`.
Stated test counts (81 statics) do not match the 74 registered.

### History

**Hazel 1 and 2 (2017–2022): no modules.** The only relative is labeled
tuples, requested in issue #13 (2017) and attempted by Erin Deutschman on
`origin/labeled-tuples` (PR #315, 2020–2021, closed 2022). Two closed Hazel 2 PRs by
`hejohns`, #493 "modules" and #494 "basic-modules" (both opened 2021-03-30),
are the earliest module attempts on GitHub; we did not inspect their
content. Context foundations both later designs reuse:
Kevin Li's split of context entries into `VarEntry` and `TVarEntry`
(`4df97e84e3`, 2022-10-26), the `Singleton | Abstract` kinds (2022-11 to
2023-02), and ADTs (PR #990, 2023-08).

**Design 1 (2023-05 to 2024-05): first-class modules with real type members.**
Author `gensofubi` (Tao Zhu, University of Michigan), 79 commits on
`origin/haz3l-module`, PR #1020 "module system". Syntax: `module M = let x =
1 in type T = ... in ?` with a trailing hole, and `{ x : T; type T = Int }`
module types in type position. A module type was literally a context,
`Typ.Module({inner_ctx: Ctx.t, incomplete: bool})`, with `VarEntry`s for
values, `TVarEntry`s for type members and `ConstructorEntry`s for
constructors (`origin/haz3l-module:src/haz3lcore/statics/TypBase.re:22-41`).
`M.T` was a type-level `Dot` resolved through nested contexts to
`Member("M.T", ty)`, a name-carrying wrapper transparent to `join`; binding a
module renamed member types via `Ctx.modulize` so locally aliased members
stayed nameable outside (commit `0a11c6f33f`). Analyzing a body against a
module type that declares `T` checked the alias against the declaration and
marked inconsistency (`45b8c80c9a`, 2023-06-20). Signatures could declare
manifest type members only; no abstract types. `incomplete` module types
(created by dot access on an unknown) admitted extra members in joins, but
complete module types had no width subtyping. Dynamics: `ModuleVal(env,
names)` closures, `Dot` stepping into the closure, and member-wise casts with
the empty incomplete module as ground type (`87320cae10`, 2024-04-19).
Functors fell out as ordinary functions over modules. Why it never merged:
review latency and repeated conflicts (2023-06 to 2025-03), the tylr and
statics rewrites underneath it (a 2024-10 merge attempt "not compiling"),
and syntax disagreements. Closed 2026-04-21 as "added via an alternative
design, inspired directly by this PR". PR #1241 (`module_implicts`, Zhen Xu,
2024-03) planned modular implicits on top of it; the branch's own commits are
parameterized-types work and merge fixes, and it adds no implicit-related code
beyond its base (the same single `src` file mentions "implicit" on both
branches).

**Design 2 (2025-05 to 2025-08): gc-modules.** Discussion #1558 (Gregory
Croisdale, 2025-03-10) reset the effort, motivated by a standard library,
user-defined livelits, and constructor namespaces; Alexander Bandukwala noted
that "current designs don't have opaque types". PR #1788 (Croisdale,
Bandukwala, Keenan) introduced `ModuleEntry`/`ModuleSignatureEntry` sorts
with `val`/`typedef` items and `;;` separators, `Typ.ModuleSignature(entries)`,
and stepwise evaluation of `Module({final, todo, env})`. `M.T` by dot lookup
was intended but its test is skipped. Unfinished; closed 2026-04-21.

**Design 3 (2026-01 to 2026-02): the merged one.** Andrew Blinn's PR #2075
(sort-dependent expansion, closed), PR #2104 "Baby Modules" (closed; its
`plans/modules-phase-1.md` states the premise "modules as a syntactic gloss
over labeled tuples"), and PR #2123 "Modules I" (merged 2026-02-27, about
9.4k lines, self-described as "transitional progress toward a full modules
system including proper statics and dynamics"). Review addressed auto-probing,
unused-variable counting through types, and nested alias shadowing; the
bare-module-name-as-type item was deferred. Post-merge: Elastatics (PR #2213)
removed the separate elaborator; Matt Keenan consolidated `ModuleHelpers.re`
(2026-04); the `~expand` normalization filter used by `module_actual_type`
arrived 2026-08-03. Open issues filed by Cyrus Omar on 2026-05-02 name the
missing pieces directly: #2257 type members in signatures, #2258 abstract
types, #2259 private types, #2260 `include`, #2261 `open`; plus #2243
constructors not exported, #2295 and #2467 function-let members, #2466, #2252.

### Comparison of the three designs

| Aspect | Design 1 (Zhu, 2023–24, PR #1020) | Design 2 (Croisdale et al., 2025, PR #1788) | Design 3 (Blinn, 2026, PR #2123, on `dev`) |
| --- | --- | --- | --- |
| Module syntax | `module M = let x = 1 in type T = .. in ?` | `{ val x = 1 ;; typedef T = Int }` | `{ let x = 1; type T = Int }`, `module M = .. in` |
| Module type | `Typ.Module({inner_ctx, incomplete})`, a context | `Typ.ModuleSignature(entries)` | none; labeled product |
| Type members in bodies | `TVarEntry` in inner context; `M.T` as type-level `Dot` to `Member` | `TypeDef` entry; dot lookup (unfinished) | `TyAlias`; `M.T` via injected alias plus `ProdProjection` |
| Type members in signatures | yes, manifest only | designed | parsed, then dropped |
| Abstract types, sealing | no | no | no |
| Width subtyping | none for complete types; `incomplete` admits extras | not implemented | none |
| Constructors exported | yes | via context of signature | no |
| Dynamics | `ModuleVal` closures, member-wise casts | stepwise entry evaluation | none; nested `let` and tuple |
| Functors | yes, as functions over modules | intended | values pass, type exports do not |
| Editor integration, tests | weak | partial | strong: sorts, remolding, TyDi, Menhir, 74 statics tests |
| Status | closed 2026-04-21 | closed 2026-04-21 | merged 2026-02-27 |

### Verdict and what "type members" would need

Value members, nesting, shadowing, sequential scoping, dot access, signature
syntax, and editor integration are solid and well tested. Type members are a
`let`-site trick that cannot be declared or constrained in a signature, does
not flow through functions, exports no constructors, and turns the module
name into a type alias. A parallel audit's property-test run reported that
the only counterexamples to "every elaborated-term id is a user-term id" were
module expressions (synthetic tuple and `let _ =` wrappers get fresh ids),
which the test's own docstring says makes such subtrees un-incrementalizable;
we did not re-run that suite. Making type members real would need a
representation of "the type exports of the value bound to `x`" that travels
with `x`'s type (a `Sig` type that survives `desugar_sig`, or a field kind on
products), an `Abstract` kind for `type T;` in signatures (the kind already
exists in `Ctx.re:13-15`), and a consistency rule for signatures that is not
product equality. Nothing on any branch has implemented abstract or
existential type members, sealing, `include`, or `open`.

**Key files.** `docs/modules.md`; `src/language/term/Grammar.re:77-78,118,131-153`;
`src/language/statics/ModuleHelpers.re` (whole file); `Statics.re:2003-2116`
(let and injection), `:2656-2736` (module and module-exp); `Typ.re:407-411,473-524,615-656,858-879`;
`lang/Form.re:525-539`; `test/statics/Test_Statics_Modules.re`. Historical:
`git show origin/haz3l-module:src/haz3lcore/statics/TypBase.re` and
`.../Module.re`; `git show origin/gc-modules:src/language/term/Grammar.re`;
PRs #1020, #1788, #2123; discussion #1558.

## Cross-cutting observations

**Merged, then removed.** Several paper mechanisms were on `dev` for a
while: the Peanut constraint checker (2024-05 to 2025-02), hole-instance
numbering and postprocessing (until 2024-02), the ChatLSP port (2025-07 to
2026-03), a separate elaborator (until 2026-04). Readers of the papers
should not expect to find these by name.

**Stale documentation and dead code worth fixing.**
- `docs/overview.md` describes Hazel 2 and is marked WIP.
- `src/language/dynamics/ProgramResult.re:5-6` still mentions `HoleInstanceInfo`.
- `docs/modules.md` links a nonexistent `plans/module-future-work.md`, describes a deleted `Elaborator.re`, and has off test counts; `Test_Statics_Modules.re:343` carries the same dead link.
- `docs/livelits.md` lists `expansion_f`/`expansion_to_hazel` where the code has `expand`/`expand_to_hazel`, and is silent about unchecked expansion typing.
- `docs/deriver.md` mentions a `strip_abbr` function that does not exist.
- `Message.re:3-5` refers to a removed `Info.derived_*`; `Typ.meet_type_provenance` (`Typ.re:212-228`) has a comment that disagrees with its code on `SynSwitch` vs `Internal`.
- Four `Mark.t` variants are never constructed (`IsLivelitName`, `TypDuplicateLabels`, `TypWantTypeFoundAp`, `TypWantConstructorFoundAp`); the `??` LLM-hole token and `LLMHole` form are dead.
- `.github/workflows/perf.yml` runs `bench/build-and-run.sh`, which exists only on `origin/perf-bench`.

**Probable regression.** The builtin livelit view likely renders "No livelit
found" since PR #2078 (2026-02), because `LivelitProj.get_model` does not
unwrap the `Projector` wrapper statics now stores; the fix exists only on the
`user-livelits` branch. Not runtime-verified.

**Signature type capture.** A free type name in a module signature is bound
by whatever the checked value declares (verified with the CLI, Part VI).

## Method

The analysis ran against a read-only worktree of `origin/dev` at
`0943461829`. Ten parallel deep dives, one per paper cluster plus one on
modules and one on repository eras, read the source, ran `git log`, `git
show` and pickaxe searches over all 686 remote branches and tags, and queried
GitHub PR and issue metadata with `gh`. Paper texts were taken from the PDFs
on hazel.org or arXiv where they parsed. Behavioural claims about modules
were checked by running programs through the built `hazel analyze` CLI. The
findings were cross-checked against an independent audit of the same
repository performed the same day; where that audit reported measurements we
did not reproduce (full test-suite counts, property-test counterexamples), the
text attributes them. Not done: running the web UI, running the full test
suite, or reading the separate `hazelgrove` repositories beyond their
listings and READMEs.
