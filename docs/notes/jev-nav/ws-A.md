# Workstream A — Jev transport + selection core

2026-09-22. Files: `src/util/OpenRouter.re` (`SystemOne` only),
`src/haz3lcore/CompositionCore/JevNav.re`, `test/Test_JevNav.re`, `test/haz3ltest.re`.
All Stage 0 public names/types kept; only helpers added.

## Built

- **`SystemOne`**: pure `json_of_request(~model_id, ~state, questions)` and
  `reply_of_json(~ids, option(json))`; `decide` wires them to `API.request` (POST
  `/api/v1/systemone`, Bearer). Usage: `input_tokens` (null → 0), `cost` via
  `Utils.num_field` (int/float/string tolerant).
- **`JevNav.Nodes`** (node building): program-order walk of `HighLevelNodeMap`;
  `code` = `CompositionGo.Local.segment_of_term` (binding without body) after
  `CompositionView.Local.ViewUtils.collapse_terms` on the children's defs, comments
  stripped with `PrettySegment.is_comment`, printed by `CompositionView.Public.print_segment`;
  `typ` = pretty-printed pattern type (Let only, else `""`); refs/used_by from
  `GeneralTreeUtils.get_refs_to`, each use attributed to the innermost binding whose
  definition (not body) contains it.
- **Pure core**: `question_of`, `state_of` (+ `json_of_node`), `tokens_of` (chars/4),
  `batches`, `classify`, `close_ancestors`, `selection_of_replies` (pure fold of all
  batch replies → selection + metrics).
- **`select`**: all batches issued at once, replies stored by batch index, finishes on
  the last one (works sync and async); latency via `JsUtil.timestamp`; `Log.record` then
  `on_done`. Any failed batch → `failed: true`, `open_paths: []`.

## Deviations / decisions

- `code` includes the binding header (`let f(y) = … in`), not just the rhs: keeps
  fn-sugar parameters visible and costs a few tokens.
- `reply_of_json` takes `~ids`; a missing answer for any asked id is `Failed` (a
  silent "no" could hide a relevant binding).
- `API.request` exposes no HTTP status, so `Failed` codes come from the
  `{"error":{"code","message"}}` body; no response → `Failed(0, …)`.
- `Log` moved above `select` (it records there); unchanged API.

## Tests

`test/Test_JevNav.re`, 16 cases in `JevNav.SystemOne`, `JevNav.nodes`, `JevNav.pure`,
`JevNav.select`: all pass. Full suite (`dune build @runtest --profile dev`): 4015 tests, all green.

## Open issues

- Shadowed bindings share a path (no `#k` suffix yet); their answers collide by id.
- Helper `typ` for unannotated params is `? -> Int` (statics has no inferred param type).
- Stripped comments can leave double spaces in `code`.
- Running `dune build @src/fmt @test/fmt --auto-promote` once reformatted other
  agents' in-progress files (`AgentSend.re`, `Test_AgentTools.re`), formatting only.
  After that I promoted only my own files.

## Round 2: JevEdit and Choice (2026-09-23)

Files: `SystemOne` Choice section, `src/haz3lcore/CompositionCore/JevEdit.re`,
`test/Test_JevEdit.re`, `test/haz3ltest.re`. All Stage 0 public names/types kept.

### Built

- **`SystemOne`, DRY refactor**: Noul and Choice now share `json_of_questions` (envelope),
  `error_of_json`, `usage_of_json`, `answers_of_json` (every asked id must be answered)
  and `post` (HTTP). Round-1 public names unchanged.
- **Choice wire**: options go out as positional keys `c0..cN` in `criteria` (option
  text is code), and `choice_reply_of_json(~questions, json)` maps them back. An
  unknown key, a missing answer or an error body gives `ChoiceFailed`.
- **Candidates** (`JevEdit.Candidates`): `TyDi.suggest` runs on the hole's Info with
  its ctx cut to the program's own bindings (`Ctx.added_bindings` minus the builtin
  ctx), because hundreds of builtins type-check at most holes. Kept: complete
  references (`FromCtx`), applications as `f(?)` (`FromCtxAp`; the argument becomes
  a hole in the next round), and `true`/`false`. Dropped: keyword forms, operators,
  lookahead forms (`x::`, `x, `) and the backpack, which are partial syntax the
  sketch is responsible for. Then ∪ `names` ∪ `literals`, deduped, capped at 254
  (+ `escalate` = 255).
- **Holes** (`JevEdit.Holes`): the EmptyHole infos in the target's definition (inside
  the binding and not under its body; the same rule as JevNav's `body_id_of`), in
  program order via `Segment.ids`. Ids are positional (`hole_1`, …) and stable for
  one round. The expected type comes from `ErrorPrint.Print.typ(ana)`.
- **State**: intent, view, target, `current_definition` (the target rendered with
  each hole written as its id, so Jev sees where `hole_k` sits after round 1),
  new_names, literals, holes. `state_of` = `state_with_current` with the sketch as
  the current definition.
- **`edit`**: the sketch goes through `CompositionGo.Public.go(Update(Definition, …))`,
  the agent's own path with the static-error veto. Then rounds of: holes → one
  Choice request → `apply_answers` (pure) fills each hole via
  `PerformUtils.overwrite_term`. `normalize_top_level` runs at the end.
  `Log.record`, then `on_done`.

### Decisions

- **Failed Choice → the program from before the sketch.** A transport failure
  makes the whole edit do nothing, so the planner can retry the same call.
  A rejected sketch also fails, without calling Jev.
- **Escalated holes** (the escape option, confidence < `min_confidence`=0.5, or a
  fill the editor rejects) stay as `?` for the planner and are not re-asked; they
  are tracked by term id.
- Added the optional `~min_confidence` to `edit`, which is compatible with existing
  calls. Added the helpers `state_with_current`, `apply_answers`, `max_candidates`.
- Planner `names`/`literals` are offered at every hole without a type check, so
  only TyDi candidates are well-typed by construction.

### Tests

`Test_JevEdit.re`, 13 cases:
- Choice request/reply, key mapping, failures, the 255 cap
- a typed hole offers `volume(?)` and no builtins/keywords; holes outside the target are ignored
- the fix-middle edit: two rounds fix `billed`, and the program evaluates 12 → 36
- the state shows hole positions per round
- escalate (not re-asked), low confidence, the `max_rounds` cap, a failed Choice, a rejected sketch

Full suite: 4037 tests, green.

### Open issues

- Without builtins in the ctx, builtin constructors (`Some`, `None`) and functions
  are offered only if the planner lists them in `names`.
- A hole whose type is unknown (e.g. an unannotated parameter `r`) accepts nearly
  every program binding, so the candidate order is TyDi's, not ranked.
- Hole ids are positional, so an escalated `hole_1` and a later new `hole_1`
  share a label across rounds (not within one).

## Round 3: Jev builds structure (2026-09-24)

Files: `JevEdit.re`, `test/Test_JevEdit.re`. Public names/types unchanged; no new
record fields. `edit` gained the optional `~max_rounds_build=10` and `~max_holes=40`.

### Built

- **Forms catalogue** (`Candidates.forms(~names)`): `if ? then ? else ?`, `(?, ?)`,
  `[?]`, `[]`, and per planner name `fun <n> -> ?` and `let <n> = ? in ?`. The
  binary operators come from `TyDiForms.Typ.of_infix_delim` (reused, not
  re-listed) as `? op ?`, except `,` `;` `|>` `!` `\/`. A test checks that every
  form parses and prints back to itself.
- **Order and filter**: program references (TyDi), then planner names and
  literals, then forms. Capped at 254. The only type filter is that a form's
  result type (TyDiForms' own table) must be consistent with the hole's expected
  type. Anything subtler is left to the integrator's fill guard (static-error
  count must not rise).
- **Pattern holes**: `Holes.located` now also finds pattern EmptyHoles; their
  candidates are planner names only. The question wording is now
  "What should replace `hole_k`", which fits expressions and patterns alike.
- **Build mode**: an empty sketch becomes `?` as the definition of an existing
  path, or `let <path> = ? in` for new code, and the round cap becomes
  `max_rounds_build`.
- **Hole budget**: if the holes already asked plus the open holes would pass
  `max_holes`, the edit stops, and the open holes are reported as escalated
  (not failed).

### Tests (9 new, 39 Jev tests total)

- all forms round-trip
- a Bool hole offers `? == ?`, not `? + ?`
- `double` is built from an empty sketch in an empty program:
  `fun x -> ?`, then `? + ?`, then `x`/`x` (3 rounds, 4 fills)
- runaway `(?, ?)` stops at 31 asked with 32 escalated
- a pattern hole in `fun ? -> 1` is filled from names
- an off-menu ill-typed answer `? + ?` at a Bool hole escalates through the guard

Full suite: 4050 tests, green.

### Open issues

- Binder forms exist only for planner names, so Jev cannot introduce a name the
  planner did not list (by design, option A).
- For unknown-typed holes, every form fits, so options there are long (about 30
  forms plus references). Watch Jev accuracy on these.
- `match` and module forms are not in the catalogue yet (their case/arm syntax
  needs a multi-hole template; add when a task needs it).

### Round 3 add-on: typed signature (2026-09-24)

- `JevEdit.request` gained `signature: string` (`[@yojson.default ""]`, sexp default ""). This is the
  one field added. B3's `CompositionUtils` record literal had to add it too (flagged in advance).
- Build mode (empty sketch) with a signature creates `let <name> : <sig> = ? in`. For an existing
  path it rewrites the clause (`Update(BindingClause)`), which sets the annotation and resets the
  definition to one hole. With a sketch, the signature is ignored, because the sketch fixes its
  own shape.
- Validation runs before any Jev call and fails with a clear error in two cases. (1) The static
  error count rises (e.g. `Nonsense -> Int`). (2) `complete_type`: the text, parsed as a type and
  printed with holes shown, contains `?`. The parser is error-tolerant, so `Int ->` otherwise
  passes with a hole.
- Jev's state now carries `"signature"`.
- Tests (+3, 42 Jev tests): with `Int -> Int` the root hole has fewer options than without, and
  `? ++ ?` is gone, and `double : Int -> Int = fun x -> x + x` still builds. `Nonsense -> Int` and
  `Int ->` fail with 0 requests. A signature on an existing path rebuilds it. Full suite: 4054, green.

## Round 4: refuse zero-hole sketches (2026-09-24)

- Russ's rule: Jev never re-types what the planner wrote. The check runs after the sketch is
  placed and before any Jev call: in `edit`'s `start`, a non-empty sketch that owns no holes
  fails with `JevEdit.no_holes_error` (new public string, the exact message requested). As
  with every failure, the outcome is the original program, so nothing is applied. Build mode
  (empty sketch) is unaffected.
- Test: zero-hole sketches on both the update path and the create path fail with that error,
  make 0 requests, and leave the program text unchanged. 43 Jev tests; full suite 4055, green.

## Round 5: tighter nav question, keyed state, exact mentions (2026-09-24)

After Eval 002 (precision 6%). Files: `JevNav.re`, `test/Test_JevNav.re`. Metrics record shape unchanged.

- **Wording**: `question_of` asks for strict need, not relevance. Instructions: "Must the agent
  read or change binding `<path>` to carry out the intent?" True side: "The intent cannot be
  carried out without reading or changing `<path>`." False side: "`<path>` can stay folded: the
  intent does not require its code."
- **Keyed state**: `bindings` is an object keyed by path (`json_of_binding`, with no path field
  inside). `json_of_node` was removed; nothing else used it, and `tokens_of` now counts
  path + binding.
- **Exact mentions**: new `mentioned_paths(~intent, nodes)`. The intent is split into word runs
  (letters, digits, `_ ' . /`), with trailing `.`/`/` trimmed. A binding counts as mentioned if a
  word equals its full path, its dotted form, or its last segment when that segment is unique.
  `select` opens mentioned paths without asking Jev and asks only about the rest; with nothing
  left, it makes no request. Mentioned paths go first in `metrics.yes`; the count =
  `yes` minus Jev's asked yes. `selection_of_replies` gained the optional `~mentioned=[]`.
- **Threshold**: `default_thresholds.yes_at` 0.7 → 0.8; the band is unchanged.
- **Tests** (+3; 46 Jev tests):
  - the exact wording
  - keyed state JSON
  - `mentioned_paths`: dotted, slash, unique suffix; `cost` ≠ `fuel_cost`; a shared `total`
    needs the path
  - `select` asks only the unmentioned binding, and makes 0 requests when all are mentioned
  - 0.79 falls in the band, 0.8 is yes

  Full suite 4066, green.
- Open: a failed batch still clears the whole view, mentioned paths included (fail-safe kept).
  One-word binding names that are also common English words (`total`, `value`) will match
  the intent's prose.
