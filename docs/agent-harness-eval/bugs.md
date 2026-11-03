# Bugs and defects

Grouped by whether they must be fixed before the comparison is meaningful.
Locations are against `origin/dev` at `79f394c816`.

## Blocking the eval

| ID | Where | Defect | Fix |
|---|---|---|---|
| B1 | `src/CLI/Cli.re:331` | `hazel test` exits 0 when the program has zero tests. An agent that deletes the tests scores "pass". | Exit non-zero when `total == 0` (or let the scorer require `total > 0`). |
| B2 | `src/CLI/Cli.re:331` | `hazel test` exits 0 when every test is indeterminate, which is what an ill-typed program produces. `unfinished` is tracked in `TestResults.t` but never consulted. | Exit non-zero when `unfinished > 0`; scorer must also gate on `analyze`. |
| B3 | `src/CLI/Cli.re:17-18` | Unparseable input under `test` falls back to the recovering parser, prints `SLOW PARSE (...)` to **stdout**, and exits 0. Same input under `analyze` correctly exits 124. | Route the notice to stderr; fail `test` when the fast parse fails or when `analyze` would. |
| B4 | `src/web/view/agentCore/AgentToolUtils.re:107-118` | Prompt (`CompositionPrompt.re:378`) says all workbench tools are disabled in converse mode, but `update_active_task`, `update_active_subtask`, `delete_task`, `delete_subtask` are registered `Ungated` and are sent and executable. | Gate them as `WorkbenchGated`, or fix the prose. |
| B5 | `CompositionPrompt.re:9` | `"f.body"` is given as an example structural path. No dot form exists; `HighLevelNodeMap` splits on `/` only (`HighLevelNodeMap.re:602`). | Replace with a valid example such as `"M/inner"` or `"f#2"`. |

## Not blocking

| ID | Where | Defect | Fix |
|---|---|---|---|
| N1 | `prompt_factory/HazelDocumentation.re` | Dead: not in the shipped prompt; `self(~summarized)` ignores its argument. | Fold into the shared language ref, or delete. |
| N2 | `ToolJsonDefinitions/ReadTools.re` | `view_entire_definition`, `view_context`, `show_references` are defined but never exposed or dispatched. Vestigial handling in `ToolCallSummary.re:172-176` and `test/Test_AgentUX.re:1144-1156`. | Delete, or wire up. |
| N3 | `CompositionActions.re:6-9` | `language_server` (`ShowUseSites`/`ShowReferences`) is never constructed; handler returns "not implemented yet". | Delete. |
| N4 | `CompositionUtils.re:196-207` vs `:240-241` | `create_new_task` accepts a flat object when `task` is missing; `add_new_subtask_to_active_task` has no such fallback. | Make symmetric; document or remove the leniency. |
| N5 | `WorkbenchTools.re:722`, `:781` | `update_active_task` / `update_active_subtask` have `required: []`; a no-arg call is a silent no-op. | Require at least one field, or return an error. |
| N6 | `CompositionPrompt.re:168-177`, `:203` | `place_syntax_projector(kind="fold")` mutates the program; `collapse` is view-only. Prompt never distinguishes them. | One sentence in the folds section. |
| N7 | `AgentToolExec.re:18-21` | Probe/statics/projector/workbench/boundary-insert results are dropped from the workbench view (still reach the LLM). | Include them, or document that it is intentional. |
| N8 | `src/CLI/Cli.re:26-31` | `hazel run` exits 0 on ill-typed programs and prints the stuck term. | Exit non-zero, or document. |
| N9 | `src/CLI/Cli.re:340-458` | `hazel probe` always exits 0, even on "Failed to parse program". | Exit non-zero on parse failure. |
| N10 | `src/CLI/README.md` | Advertises removed `slide-*` subcommands; omits `test`, `probe`, `bench-eval`, `agent-eval`, `grade-*`. | Rewrite. |
| N11 | `Message.re:213` | Comment says "the ~20k system prompt"; it is ~40 KB of text. | Update. |
| N12 | Eight workbench tools | `mark_active_task_incomplete`, `mark_active_subtask_incomplete`, `mark_active_subtask_failed`, `reorder_subtasks_in_active_task`, `update_active_task`, `update_active_subtask`, `delete_task`, `delete_subtask` never appear in prompt prose. | Mention, or accept schema-only discovery. |
