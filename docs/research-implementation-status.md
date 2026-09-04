# Hazel research papers vs. the `dev` codebase: implementation status

> **Status: DRAFT / PREVIEW.** This document is being assembled from a
> codebase-wide analysis of `dev` (as of 0943461829, 2026-09-02). Sections
> marked *TODO* are still being analyzed and will be filled in. Claims that
> are already verified cite files and commits; unverified statements are
> labeled as such.

## Purpose

The Hazel project has produced a sequence of major-venue papers (POPL, ICFP,
PLDI, OOPSLA) plus many workshop papers. Each describes a calculus or a
mechanism that Hazel is meant to embody. This document answers two questions:

1. For each major conference paper, how far along is `dev` in realizing it?
   Which parts are implemented, partial, missing, superseded by a different
   design, or live only on an unmerged branch or in a separate repository?
2. How do **modules** and **type members** work in the repo, both today and
   historically?

Status vocabulary used below:

| Status | Meaning |
| --- | --- |
| Implemented | Present on `dev`, tested, matches the paper's design in substance |
| Partial | Present on `dev` but with named gaps relative to the paper |
| Diverged | The idea is present but the design differs materially from the paper |
| Superseded | Replaced on `dev` by a later design |
| Branch only | Exists only on an unmerged branch |
| Separate repo | Implemented outside `hazelgrove/hazel` |
| Missing | Not implemented anywhere we could find |

## Paper inventory

Major-venue papers (from [hazel.org](https://hazel.org/)):

| Year | Venue | Paper | Overall status on `dev` |
| --- | --- | --- | --- |
| 2017 | POPL | Hazelnut: A Bidirectionally Typed Structure Editor Calculus | *TODO* (Hazel 3 replaced the Hazelnut action model with tylr-style tiles; holes and typed edit states survive) |
| 2019 | POPL | Live Functional Programming with Typed Holes | *TODO* (elaboration, casts, hole closures, stepper present; fill-and-resume to be checked) |
| 2020 | ICFP | Program Sketching with Live Bidirectional Evaluation (Smyth) | *TODO* |
| 2021 | PLDI | Filling Typed Holes with Live GUIs (livelits) | Partial per `docs/livelits.md`: builtins only, no parameters, no splices, no user-defined livelits; projectors generalize the mechanism |
| 2023 | OOPSLA | Live Pattern Matching with Typed Holes | *TODO* (`Coverage.re`, `PatternMatch.re` to be compared against the paper) |
| 2024 | POPL | Total Type Error Localization and Recovery with Holes | *TODO* (marking is the core of `Statics.re`/`Info.re`; type hole inference to be checked) |
| 2024 | OOPSLA | Statically Contextualizing LLMs with Typed Holes (ChatLSP) | *TODO* (`CompositionCore`, `TyDi`, `src/CLI`, agent workbench) |
| 2025 | POPL | Grove: A Bidirectionally Typed Collaborative Structure Editor Calculus | *TODO* (likely separate repo) |
| 2025 | OOPSLA | Incremental Bidirectional Typing via Order Maintenance | *TODO* (dev has incremental evaluation and cached statics; order-maintenance typing to be checked) |
| 2025 | OOPSLA | Syntactic Completions with Material Obligations (tylr) | *TODO* |
| 2026 | OOPSLA | Interactive Data Analysis with Lively Typed Tables | *TODO* (labeled tuples, CSV/table projectors, rich probes, B2T2) |

Workshop and other papers touched on where relevant: SNAPL 2017 (vision),
HATRA 2020 (Hazel Tutor), TyDe 2022 (tylr), VL/HCC 2022 (assistant
architecture), Onward! 2022 (ExplainThis), VL/HCC 2023 (obligations), TFP
2024 (polymorphism), HATRA 2024, PROPL 2024/2025, VL/HCC 2025 (Hazel
Deriver), HATRA 2025 (type highlighting).

## Repository eras

*TODO: verify boundaries.* Working hypothesis:

| Era | Years | Core | Notes |
| --- | --- | --- | --- |
| Hazel 1 | 2017–2019 | OCaml derived from the POPL17 `HZ` prototype | POPL19 artifact tags `popl19-artifact*` |
| Hazel 2 | 2020–2022 | `src/hazelcore` (`UHExp`/`ZExp`), `src/hazelweb`, `pretty` | `docs/overview.md` describes this era |
| Hazel 3 (haz3l) | 2022– | `src/haz3lcore` (tylr tiles/zipper), `src/language`, `src/web` | current `dev` |

## Per-paper analysis

### POPL 2017 — Hazelnut

*TODO.*

### POPL 2019 — Hazelnut Live

*TODO.*

### ICFP 2020 — Smyth

*TODO.*

### PLDI 2021 — Livelits

*TODO: full analysis.* Preliminary, from `docs/livelits.md`: built-in livelits
are implemented as OCaml modules satisfying `BuiltinLivelit`, invoked with a
`^name` prefix, and expand to an expression of a declared type. Parameters,
splices, and user-defined livelits are not implemented on `dev`. In-progress
branches include `livelit-expansion-typing` (PR #2488).

### OOPSLA 2023 — Live Pattern Matching

*TODO.*

### POPL 2024 — Marked Lambda Calculus (and TFP 2024 polymorphism)

*TODO.*

### OOPSLA 2024 — ChatLSP

*TODO.*

### POPL 2025 — Grove

*TODO.*

### OOPSLA 2025 — Incremental Bidirectional Typing

*TODO.*

### OOPSLA 2025 — Syntactic Completions with Material Obligations

*TODO.*

### OOPSLA 2026 — Lively Typed Tables

*TODO.*

## Modules and type members

### Today (`dev`)

Per `docs/modules.md` (to be verified against code): modules are a syntactic
gloss over labeled tuples. `{ let x = 1; let y = true }` expands to nested
`let`s ending in the labeled tuple `(x=x, y=y)`; module types are labeled
tuple `Prod` types. Signatures `{ let x : Int; ... }` are desugared to labeled
tuples before checking. Type aliases inside modules are exposed for `M.T`
access by injecting a `TVarEntry` whose body is the module's type exports,
resolved through `ProdProjection`. Known limitations documented there: no
width subtyping, `type` entries in signatures are dropped, and modules infer
`Prod` rather than `Sig` types.

*TODO: verify each claim, enumerate tests, and characterize what "type
member" means in this design (only concrete aliases; no abstract types or
sealing).*

### History

Two distinct module designs exist in the repository's history:

1. **2023–2024: a first-class module design with real type members**
   (author `gensofubi`, on branches including `origin/modules`). Commits
   include `1135c5cbe2` (2023-05-17, "copy module from let"), `98453161a4`
   ("concrete module typing ... and dot access operator"), `362077114d`
   (2023-06-14, "basic type members"), `45b8c80c9a` (2023-06-20, warnings
   for undefined type members and type-member inconsistency), `d2034bac04`
   ("change dot type to be a Typ postfix"), `11e3b27940` (2024-01-07,
   "basic module alias"), `87320cae10` (2024-04-19, "Cast for modules with
   Unknown type"), `5e567bf1e5` (2024-05-20). Never merged to `dev`.
   *TODO: describe the design (module types, type members, casts) and why it
   was not merged.*
2. **2026: the labeled-tuple encoding now on `dev`** (documented in
   `docs/modules.md`, updated in `cc3dba542f`, 2026-02-14). *TODO: PRs,
   authors, rationale.*

*TODO: comparison table of the designs.*

## Method

The analysis was performed against a read-only worktree of `origin/dev` at
0943461829. Evidence is drawn from reading the source, `git log`/`git show`
over all 686 remote branches and tags, and GitHub PR metadata via `gh`.
