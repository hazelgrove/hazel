# Task Feature Audit (what the study tasks actually use)

Near-verbatim keep of `study/task-feature-audit.md`.

Method: read every debugging working program (9 families) and every writing
solution (10 tasks; 11 counting log-cleaner v1+v2), via two opus subagents, plus
grep verification. Goal: determine what the pre-study tutorial *must* teach,
what can be cut or made implicit, and where tasks overlap.

**Inventory.** Debugging (9): companion-plotter, crop-plotter, growth-plotter,
harvest-streak, lunar-growth, nutrient-rotation, soil-plotter, tamagotchi,
tictactoe. Writing (10): clamp, garden-path, crop-tally, night-bloom,
garden-survey, mentions, log-cleaner (v1+v2), running-sum, crop-plotter-extend,
harvest-streak-extend. (`writing-tasks-plan.md` lists more ideas not present.)

## Feature usage tiers

### Tier 1 — Universal (teach for sure)
- `let`; **type annotations** (pervasive in debugging; given in most writing)
- Int arithmetic; comparisons & booleans (`==`,`<`,`>`,`&&`,`||`,`!`)
- `if`; **`case` / pattern matching** (constructor + nullary + wildcard; tuple
  patterns in lunar-growth/tictactoe/running-sum; Option patterns in
  tictactoe/nutrient-rotation)
- `fun` literals; **multi-arg via tuple params**; function application
- **higher-order functions** (lambdas / named fns passed to map/fold/filter)
- list literals; **list builtins** — `fold_left` (THE workhorse: drives every
  debugging task's update loop; also crop-tally, running-sum), `map`/`mapi`
  (grids + string tasks), `filter` (string tasks, lunar), `nth`, `length`
- **records / labeled tuples** (`label = value` + `.field` projection) — most
  debugging tasks model state this way (Cell, Model, Stats, Soil, Ledger…)
- **ADTs** (`type T = + A + B(payload)`) — ALL debugging tasks model the domain
  with sum types (Action, Crop, Stage, MoonPhase, Quality, GameStatus…)
- positional tuples (fold accumulators; some Model construction; tictactoe
  3-tuples)
- `test … end`

### Tier 2 — Subset, but intrinsic to the (kept) tasks that use them
- **String functions** — Central to the 6 writing string-tasks: `string_split`
  (garden-path, crop-tally, night-bloom, garden-survey, mentions), `string_match`
  (night-bloom, garden-survey), `string_sub`+`string_length` (mentions),
  `string_trim`+`string_replace` (log-cleaner). (`string_concat` listed but
  unused.) These six **are** the auto-probe-while-writing showcases.
- `cons` (`::`) — harvest-streak, lunar-growth, harvest-streak-extend (prepend).
- `Option` — tictactoe (built-in), nutrient-rotation (inline `+None +Some`).

### Tier 3 — Rare / Absent (cut candidates) — verified by grep
**Used by ZERO tasks:**
- **Floating point** — 0 task `.hz` files (the only `*.`/`float_of_int` hits are
  in README prose). Only the probe tutorial slide `probes/02` uses a float
  multiplier, and that's eliminable.
- **Partial application** — 0. (`fold_left(actions, update, init)` passes a named
  function as a *value* = first-class function, not partial application.)
- **Recursion** — 0. `fold_left` covers all iteration; even running-sum is a
  fold, not `let rec`.
- **Pipelining `|>`** — 0. String pipelines are written as nested calls /
  sequenced `let`, not `|>`.
- **Advanced labeled-tuple ops** (extension, omission, list-conversions) — 0.
  Tasks use record literals + `.field` only.
- `int_of_string` / `string_of_int` — 0.

## Answers to the two open questions
- **Floating point: skip it.** No task uses it. → Drop the Floating-Point basics
  lesson; optionally de-float `probes/02`.
- **Strings: mostly implicit.** String *literals* are opaque identity keys
  (`==`) — nothing to teach. String *functions* are central only to the 6 writing
  tasks, and those tasks are precisely about **discovering function behavior via
  live probe values** (split arg-order, match false-positives, sub off-by-one).
  Let probes teach them in situ; at most a one-line "string functions exist —
  use probes to learn them." No dedicated string lesson.

## Cut / consolidation opportunities
- **Lessons with zero task support** → cut candidates: Floating Point, Partial
  Application, the advanced labeled-tuple lessons (Projection-as-feature,
  Extension, Omission, List-Conversions). Recursion needs no dedicated lesson.
  - *Pipelining caveat:* tasks don't use `|>`, but it's the natural, probe-legible
    way to write the writing-task pipelines (one sample per stage). Optional —
    teach as a nicety or skip.
- **Task redundancy:** the grid "plotter" debugging tasks (companion, crop,
  growth, soil; lunar/nutrient nearby) are **feature-near-identical** (ADT
  Action + record Model + `fold_left(update, actions)` + grid `map`/`mapi` +
  `case` + index `if`); companion/crop/soil even share the *same* wrong-index
  (`i` vs `j`) bug. 2–3 cover the feature space. But their **bugs** differ
  meaningfully, so keep variety for fault type (per "adapt over cut").

## Gaps (tasks need it; basics may not teach it)
- **ADTs / sum types — the biggest gap.** Central to *every* debugging task, but
  no dedicated basics lesson for `type T = + A + B(payload)` — only "Type
  Annotations" (#07), which is annotations, not constructor definitions. Probe
  slides introduce sum types only incidentally. → **Add an explicit ADT lesson.**
- **records + `.field`** central → keep a basic records/projection lesson
  (refines the earlier "cut all labeled-tuple lessons": keep the basics, cut the
  advanced ops).
- **The MVU skeleton** (`Action` ADT + `Model` record + `fold_left(update,
  actions)`) is the shared shape of all 9 debugging tasks. → consider a capstone
  slide that reads/builds a tiny MVU program.

## Probe-multiplicity alignment (confirms the spine)
- **Many samples**: every debugging task calls `update` per action via
  `fold_left`; grids call per-cell via `map`/`mapi`; writing tasks per-element via
  `map`/`filter`. → "0/1/many → navigate / pin / step-into" is motivated
  everywhere.
- **`∅` (untaken branches)**: every task has `case`/`if`.
- **No recursion anywhere** → step-into matters for "which *iteration* / *cell*",
  NOT recursion depth. The recursive-fib step-into demo is tutorial-only; for
  tasks, prioritize step-into-into-a-fold-iteration.
- **Best auto-probe writing showcases**: log-cleaner (shadowed-`let` pipeline +
  `string_replace` arg-order), mentions / garden-path (`string_split` arg-order +
  `nth` off-by-one), garden-survey (`string_match` "yes" matches "yesterday"),
  running-sum / crop-tally (fold-accumulator evolution).
- **Best debugging showcases**: harvest-streak (streak across harvests),
  tamagotchi (stat decay across ticks), grid tasks (which cell).

## Bug-variety inventory (reason to keep several similar tasks)
wrong-neighbor (companion); wrong index `i`/`j` (crop, soil — duplicate);
stuck-stage `case` arm (growth); wrong field restored (nutrient); not-its-own-
opposite (lunar); stat/threshold/sign (tamagotchi ×3: decay, priority, bonus);
diagonal-indices / turn-not-switching (tictactoe ×2).
