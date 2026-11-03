# Findings: what exists today vs. what the harness eval needs

Audit date: 2026-09-14, against `origin/dev` at `79f394c816`.
All paths are repo-relative.

## 1. The system prompt

Assembled once per app session by `CompositionPrompt.self`
(`src/haz3lcore/CompositionCore/prompt_factory/CompositionPrompt.re:384-396`),
concatenated with `\n` in `AgentUtils.init` (`src/web/view/agentCore/AgentUtils.re:6`).
Roughly 40 KB of text in 12 sections. Nothing live is injected into it; the
editor state rides in a separate trailing `<context>` system message rebuilt
every turn (`Message.re:305-364`).

| # | Section | Source | Class |
|---|---|---|---|
| 1 | identity | `CompositionPrompt.re:3-19` | persona / editor |
| 2 | guidelines | `:21-51` | persona + tool names |
| 3 | message_channels | `:53-62` | editor protocol |
| 4 | session_modes | `:369-382` | editor |
| 5 | partnering_and_user_intent | `:64-120` | persona |
| 6 | **hazel_language_guide** = `HazelSyntaxNotes.self` | `HazelSyntaxNotes.re:1-248` | **language** |
| 7 | program_model | `:122-180` | language (`:125-145`) then editor (paths, folds) |
| 8 | ProjectorCatalog blurb | `ProjectorCatalog.re:63-100` | editor |
| 9 | toolkit | `:182-290` | editor |
| 10 | task_planning | `:292-323` | editor |
| 11 | formatting_rules | `:325-357` | editor (`:328-346`) + language (`:348-356`) |
| 12 | few_shot_examples = `Eg_RecFib.self` | `Eg_RecFib.re` | editor (tool-call transcripts) |

**Language-only content** that an external harness can reuse:

- `HazelSyntaxNotes.re` (~8 KB). Only three editor sentences: lines 4-5
  ("structure-aware editing", "every edit state maintains a valid AST") and
  line 208 ("use holes as scaffolding").
- `CompositionPrompt.re:125-145` (program shape: `let/type ... in` chain,
  binding anatomy). Line 146 is the seam where tool prose begins.
- `CompositionPrompt.re:348-356` (comment syntax; duplicates syntax-notes rule 10).
- `HazelDocumentation.re` (~11 KB: polymorphism, recursive types, two full
  MVU sample programs). **Dead code**: bound nowhere in the shipped prompt,
  referenced only by `test/Test_PromptFactory.re:272-277`. Five editor
  sentences at lines 4, 6, 79, 84-86.

Everything else is editor-, tool-, or persona-specific and does not transfer.

`test/Test_PromptFactory.re` parses, round-trips, and statics-checks the
documented snippets and probes every reserved word, so any extracted
reference is already machine-validated.

**Authorship** (git blame): russell-rozenbaum wrote 1,530 of 1,586 prompt
lines (96%). disconcision did a language-accuracy pass on 2026-07-20/21
(reserved words, `let f(x,y)` sugar, float ops, module syntax, recursive
types). David Moon's 2026-03 syntax-notes contribution has been fully
overwritten. Cyrus Omar has one surviving line (`HazelDocumentation.re:43`).

## 2. Tools

37 tools are declared (`src/haz3lcore/CompositionCore/ToolJsonDefinitions/`),
listed in `CompositionUtils.Local.tools` (`CompositionUtils.re:14-51`), and
dispatched by name in `CompositionUtils.action_of` (`:108-273`). Coverage is
enforced by `test/Test_AgentTools.re:3262-3289`. No phantom names in the
prompt; no unhandled names.

Which tools need the live editor:

| Group | Tools | Live zipper? | File-harness equivalent |
|---|---|---|---|
| View | `expand`, `collapse` | no (agent-context state only) | none needed; whole file is visible |
| Edit | `update_definition/body/pattern/binding_clause`, `delete_*`, `insert_before/after` | yes | text edit + `hazel analyze` |
| Probe | `place/remove/toggle_probe` | yes | `^^probe(e)` in source + `hazel probe` |
| Statics overlay | `place/remove/toggle_statics` | yes | `hazel analyze` |
| Syntax projectors | `place/remove/toggle_syntax_projector` | yes | `^^kind(e)` in source |
| Workbench | 17 task-board tools | no | scratch `TODO.md` or nothing |

**There is no explicit typecheck, test-run, or evaluate tool.** All three are
implicit:

- Typecheck is a veto: every edit runs statics before/after and refuses if
  the error count grows (`CompositionGo.re:150-188`, `:767-810`, `:706-716`;
  `AgentToolCallHandler.re:200-210`).
- Test results and probe values reach the model only through the next
  `<context>` snapshot (`AgentUtils.re:61-78`, `Message.re:325-343`).
- A successful tool result echoes the model's own arguments back with no
  program text (`Message.re:262-269`).

This is the main structural asymmetry against a CLI harness, where
`analyze` / `test` / `probe` return output directly to the caller. Decide
explicitly whether that is part of what the experiment measures.

## 3. The CLI

Binary: `./hazel <cmd>`, a wrapper that runs `dune build` on every call and
then `node -r src/CLI/polyfill.js _build/default/src/CLI/cli.bc.js`.
Commands defined in `src/CLI/Cli.re:666-683`.

| Cmd | Purpose | Exit codes |
|---|---|---|
| `analyze [-W] FILE` | typecheck only | 0 clean, 124 static errors, 125 missing file |
| `test [-v] FILE` | run in-file `test ... end` blocks | 0 no failures, 124 failures |
| `run FILE` | evaluate | 0 always (even ill-typed) |
| `probe [-a] [-m] FILE` | inline `expr ≡ value` | 0 always |
| `format`, `bench-eval`, `grade-json`, `grade-report` | not relevant here | |

`FILE` may be `-` for stdin. Single file only; the language has no import
mechanism. Hidden tests from a separate file work by concatenation:

    cat submission.hz hidden_tests.hz | ./hazel test -

`;` is `Seq`, so this matches the in-app AST-level stitching
(`src/web/exercises/CodeExercise.re:678-770`).

`src/CLI/README.md` documents only `run`/`format`/`analyze`, omits `test`,
and still advertises removed `slide-*` subcommands.

## 4. Exercises, scorer, dashboard

- **Exercise data model already matches the plan.** `CodeExercise.spec`
  (`src/web/exercises/CodeExercise.re:51-56`) has `prompt`, `prelude`,
  `your_impl` (starter), `correct_impl`, `hidden_bugs`, `hidden_tests`. But
  each exercise is one compiled `.ml` file (`src/web/exercises/examples/Ex_*.ml`)
  linked into the app. Answer-hiding is the compile-time instructor/student
  switch, not a filesystem boundary. No extractor emits `.hz` files.
- **No scorer.** `grade-json` / `grade-report` consume a browser-exported
  sexp blob keyed to compiled spec UUIDs (`src/CLI/Grade.re:55-58`, `:158-159`).
  They cannot grade a loose `.hz`.
- **`hazel test` alone is unsound as a scorer.** See `bugs.md` B1-B3.
- **No structured output.** No `--json` on any subcommand.
  `TestResults.t` already derives yojson (`src/language/dynamics/state/TestResults.re:5-13`).
- **No dashboard.** Zero hits for "dashboard". In-repo precedents for a
  results format: `agent-docs/caching-eval/outputs/summary/*.jsonl` and
  `bench/compare.js`'s `[{name, ...}]`.
- **No external-harness invocation.** The only LLM drivers are
  `src/CLI/AgentEval.re` (headless Filbert; on `cost-display-refinements`)
  and `agent-docs/caching-eval/harness/run_eval.mjs` (direct OpenRouter).
