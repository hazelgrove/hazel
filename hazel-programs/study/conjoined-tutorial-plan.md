# Conjoined Tutorial Plan (Hazel intro → probes)

Goal: one Tutorial-mode sequence that takes an FP-familiar-but-not-Hazel study
participant from "what is a hole" through enough Hazel to read/write the task
programs, then through **all** probe features the study needs (per
`probes-user-study/README.md`: "teach all features without prescribing usage").

Two source sequences now both live in `probes-study` and both render today:

- **Onboarding lessons** — 22 hand-written `Tu_*.ml`, gated Tutorial mode.
  The Hazel-programming spine.
- **Probe-study slides** — 16 `.hz` files (was Documentation-only). As of this
  branch they're *also* generated into Tutorial mode (`TuGen_*`, appended after
  the 22) via `./hazel gen-tutorial`. The probe-feature half.

## A. Onboarding lessons (the Hazel spine), in order

| # | Lesson | Keep? |
|---|--------|-------|
| 1 | Holes | ✅ |
| 2 | The Tylr Parser and Backpack | ✅ (editor mechanics — essential for non-Hazel users) |
| 3 | Integer Arithmetic | ✅ |
| 4 | Floating Point Arithmetic | ✅ |
| 5 | Let Bindings | ✅ |
| 6 | **Probes** | ✅ — already spliced here; see merge note |
| 7 | Type Annotations | ✅ |
| 8 | Functions | ✅ |
| 9 | Multi-Argument Functions | ✅ |
| 10 | Partial Application | ➖ optional (depends on task needs) |
| 11 | Reverse Function Application (Pipelining) | ✅ (mentions task uses pipelines) |
| 12 | If Expressions | ✅ |
| 13 | Case Expressions | ✅ |
| 14 | List Literals | ✅ |
| 15 | Mapping Over Lists | ✅ |
| 16 | Folding Lists | ✅ |
| 17 | Mean of String Integers | ➖ optional (a worked example/task) |
| 18 | Labeled Tuples | ❌ **table-specific** |
| 19 | Labeled Tuple Projection | ❌ **table-specific** |
| 20 | Labeled Tuple Extension | ❌ **table-specific** |
| 21 | Labeled Tuple Omission | ❌ **table-specific** |
| 22 | Labeled Tuple List Conversions | ❌ **table-specific** |

## B. Probe-study slides (the probe half), in order

| File | Teaches | Paper dimension |
|------|---------|-----------------|
| 01-fundamentals | probe basics + Hazel let/sum-type/case | spatial (manual) |
| 02-functions-and-cursor | sample-per-call, navigating samples | spatial + cursor |
| 03-auto-probe | autoprobe mode | spatial (auto) |
| 04-pin / 04-pinning / 04-variant-* | call pinning | **temporal** |
| 05-step-into / 05-variant-* | step into nested calls | relational |
| 06-print | print-statement debugging | study baseline (probes-vs-print) |
| 07-writing | composing a program with probes | (writing-task warmup) |
| 08-clamp | write a clamp fn w/ probes | (writing task) |
| 09-running-sum | accumulation/fold w/ probes | (writing task) |
| 10-colors-and-alignment / 10-sample-colors | sample coloring by call-stack relation | **dynamic cursor** |

## Merge opportunities (dedupe Hazel content)

The probe-study slides were written standalone, so several re-teach Hazel that
the onboarding spine already covers. When promoting them, strip the redundant
Hazel and lean on the earlier lessons:

- **01-fundamentals** re-teaches `let`, sum types, and `case`. Onboarding
  already has Let Bindings (5), Case Expressions (13), and a Probes lesson (6).
  → Fold 01's *probe* content into/after lesson 6; drop its Hazel re-teaching.
- **02-functions-and-cursor** overlaps Functions (8) and the existing Tu_Probes
  ("a sample per call"). → Merge the cursor/navigation bits into the probes
  thread; don't re-introduce functions.
- **08-clamp / 09-running-sum** are write-a-function tasks that overlap Fold
  (16) / Mean-of-String-Integers (17). → Keep as *probe-driven writing*
  practice at the end, or merge into the study's writing-task set.
- **Variant slides** are alternate takes, not a sequence — consolidate to one
  each: `04-*` (pin) → keep one; `05-*` (step-into) → keep one;
  `10-*` (colors) → keep one. (They were all brought over so you can pick.)

## Table content to remove (for the probe study)

- **Tutorial:** onboarding lessons **18–22** (the five "Labeled Tuple …"
  lessons) — they exist to teach the spreadsheet/table-projector feature.
  Remove from `TutorialSettings_base.re`.
- **Slides/data:** `src/b2t2/` (B2T2 spreadsheet-benchmark slides; only 1 slide
  remains on this branch) and its `Slides.re` registry entry, if you want them
  out of the Documentation tab.
- **Keep (do not delete):** the table *projector* implementation
  (`TableCore`/`TableProj`/`TableRenderer`/`SliderProj`/`CSVProjector`) and the
  labeled-tuple *language* support — these are core/shared and may be exercised
  by probe sample rendering. Removing them is a code change, not a tutorial
  edit; only do it if you specifically want to slim the build.

## Proposed conjoined order (target)

**Phase 1 — Hazel basics** (onboarding 1–9, 11–16; drop 18–22; 10/17 optional):
Holes → Parser/Backpack → Int → Float → Let → Type Annotations → Functions →
Multi-arg → Pipelining → If → Case → Lists → Map → Fold.

**Phase 2 — Probes** (probe-study, consolidated):
Probe basics → sample-per-call/navigation → Autoprobe → Pin (temporal) →
Step-into → Sample colors / dynamic cursor → Print (baseline) →
probe-driven writing practice (clamp, running-sum).

Where exactly probes "starts" is a knob: the current build keeps a short Probes
intro early (lesson 6, so participants can use probes *while* learning Hazel)
and puts the deeper probe features in Phase 2. That matches the study's "live
values during authoring" angle.

## Status in this branch (v1)

- All 16 probe-study slides are generated into Tutorial mode and appended after
  the 22 onboarding lessons (so the 5 table lessons + the variant slides are
  still present — prune per above).
- Every generated slide currently has a placeholder passing test (`test true
  end`) so it shows ✔; add `@test` sections to gate.
- Authoring loop + format: see `hazel-programs/tutorial/README.md`.
