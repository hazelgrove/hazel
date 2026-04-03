# Branch: `coding-agent-projector-tools-extension`

This document summarizes work on the Hazel **coding agent** (Filbert): **syntax projector tools**, **path / node-map behavior**, **tool result UX (diff + honest success)**, **prompt and message-channel policy**, and related **tests** and **UI**. Use it to onboard a future agent or reviewer without re-deriving context from chat history.

---

## 1. Executive summary

| Area | What changed |
|------|----------------|
| **Agent tools** | New OpenAPI tools: `place_syntax_projector`, `remove_syntax_projector`, `toggle_syntax_projector` (kinds: fold, slider, sliderf, check, text, card, csv, livelit — not probe/statics). |
| **Execution** | `ToolCallHandler` in `Agent.re` resolves paths via `HighLevelNodeMap`, calls `ProjectorPerform` helpers, expands agent context paths on success. **Fails** if zero paths successfully apply (no more silent no-op “success”). |
| **Diffs** | `mk_diff` now supports `SyntaxProjectorAction`, `ProbeAction`, and `StaticsAction` using full-program segment print comparison — chat tool cards can show before/after for these tools. |
| **Paths** | `closest_valid_path_to_ill_path` scoring favors the **last path segment** and **deeper** bindings so `double/t` beats `double` when the user typed `t`. `path_to_id` error text mentions `outer/inner` paths. |
| **Prompts** | New **Message channels**, **Partnering / user intent** (avoid “no errors so I won’t edit”), **CONTEXT UPDATE echo ban**, mandatory retry handling; compaction prompt lists new sections; `EditTools` clarifies path vs body-chain nesting; `SyntaxProjectorTools` clarifies valid **binding paths**. |
| **Retries** | Empty-response retries and workbench nudges can be sent to the API as **synthetic `user`** messages (`mk_retry_note_message` + `deliver_as_user_on_api`) for better model compliance. |
| **Policy comment** | Top of `Agent.re`: canonical description of UI vs API roles, context blocks, tool channel, synthetic user. |
| **Tests** | `Test_AgentTools.re`: typed `let t : Int`, body-chain vs def-nested paths, `closest_valid_path`, `update_binding_clause` cases, existing projector parse tests. |
| **UI** | `AgentMessageMarkdown.re` + `ChatMessagesView` + `agent-chat-messages.css` for safer/richer assistant markdown rendering. |

---

## 2. Syntax projector agent tools (end-to-end)

### 2.1 Tool definitions (JSON / OpenAPI)

- **`src/haz3lcore/CompositionCore/ToolJsonDefinitions/SyntaxProjectorTools.re`**  
  - Descriptions for the three tools; **paths** must be **HighLevelNodeMap binding paths** (`map`, `filter`, `outer/inner`), **not** type-applied pretty-print (`@<Int>`) or statics overlay copy-paste.
  - Cross-ref to projector catalog for per-kind expectations.

### 2.2 Parsing and stringification

- **`src/haz3lcore/CompositionCore/CompositionUtils.re`**  
  - `action_of`: branches for `place_syntax_projector`, `remove_syntax_projector`, `toggle_syntax_projector` → `SyntaxProjectorAction(...)`.  
  - `string_of_action`: reverse for logging.  
  - Registers tools in the public tool list (with other composition tools).

### 2.3 Action ADT

- **`src/haz3lcore/CompositionCore/CompositionActions.re`**  
  - `syntax_projector_action` type and `SyntaxProjectorAction(...)` wrapper in the main `action` variant.

### 2.4 Core projector operations (reusable from agent)

- **`src/haz3lcore/projectors/ProjectorPerform.re`**  
  - `with_selection_after_term` — `Select.term` then run a function on the selection.  
  - **`try_place_syntax_projector`**, **`try_toggle_syntax_projector`**, **`try_remove_syntax_projector`** — mirror editor menu behavior; idempotent place when kind already matches.  
  - These are the **single source of truth** for “what happens when we place a syntax projector at term id”.

### 2.5 Web agent handler

- **`src/web/view/AgentCore/Agent.re`** — module `ToolCallHandler` (search `SyntaxProjectorAction`):  
  - Builds `node_map` from zipper + statics; **`path_to_id_opt`** per path.  
  - Folds over paths; counts **`n_placed`** when `try_*` returns `Some`.  
  - If `paths` non-empty and **`n_placed == 0`** → **`Error(Failure.Info(...))`** with actionable text (wrong path shape vs unsupported term).  
  - On success, may **`AgentContext.Update.Expand(paths_to_expand)`** (same pattern as probes/statics).  
  - **`mk_diff`** (same file, `handle_tool_call` helper): for `SyntaxProjectorAction` / `ProbeAction` / `StaticsAction`, compares `CompositionView.Public.print_segment` of full `Select.all` before vs after to populate `AgentToolResult.diff`.

### 2.6 Prompt documentation for projectors

- **`src/haz3lcore/CompositionCore/prompt_factory/ProjectorCatalog.re`** — blurb for composition + compaction about syntax projector **agent** tools vs probe/statics.  
- **`src/haz3lcore/CompositionCore/prompt_factory/CompositionPrompt.re`** — toolkit bullets for the three tools (aligned with `EditTools` / catalog).  
- **`src/haz3lcore/CompositionCore/prompt_factory/CompactionPrompt.re`** — tool vocabulary list includes syntax projector tools; compaction excerpt list mentions partnering / message channels.

---

## 3. HighLevelNodeMap (paths and “Perhaps you meant …”)

- **`src/haz3lcore/CompositionCore/HighLevelNodeMap.re`**  
  - **`closest_valid_path_to_ill_path`**: scoring order — (1) Levenshtein on **last** segment, (2) list distance, (3) full-string distance, (4) on ties prefer **deeper** path (`neg_depth`). Avoids suggesting top-level `double` when the real binding is **`double/t`**.  
  - **`path_to_id`** failure message: append note that **nested bindings use paths like `outer/inner`**.

### Mental model for tests / agent mistakes

- **`let a = 1 in let b = 2 in body`**: `a` and `b` are **siblings** in the map; paths are **`a`**, **`b`** — not `a/b` (body-chain).  
- **`let a = let b = 1 in b in a`**: **`b`** is inside **`a`**’s definition → path **`a/b`**.  
  See **`test/Test_AgentTools.re`** — search `partnering`, `HighLevelNodeMap`, `body-chain`, `def-nested`, `closest path`.

---

## 4. Edit tools and `initialize` / paths

- **`src/haz3lcore/CompositionCore/ToolJsonDefinitions/EditTools.re`** — `update_binding_clause` description:  
  - Path resolved on **program before** edit; `code` does not affect path lookup.  
  - **`outer/inner`** for definition-nested lets vs **separate top-level names** for `let … in let …` body chains.

### Initialize / parsing gotcha (for docs and support)

- **`Parser.to_segment`** simulates typing; **literal `\` + `n`** in tool strings can merge badly with `?` (e.g. spurious `n?` token). JSON should use **real newlines** in `code` strings.  
- **`Agent.re`** `Initialize`: full-program static check after `introduce`; failures surface as tool errors.

---

## 5. Agent message channels, retries, and policy

### 5.1 File header (implementer source of truth)

- **`src/web/view/AgentCore/Agent.re`** (top comment block):  
  - System vs context vs tool vs **synthetic user**; **`mk_developer_msg` → `system` on wire** (`OpenRouter.re`); context = structured snapshot + `<staticErrorsInfo>`; UI-only messages with `api_message: None`.

### 5.2 Retry / nudge messages

- **`Message.Utils.mk_retry_note_message`**: `~deliver_as_user_on_api: bool` — when `true`, API payload is **`mk_user_msg`** prefixed with **`[Required follow-up — injected by Hazel, not the human user]`**; UI role stays **`System(RetryNote)`**.  
- **`RetryEmptyResponse`** and **active-subtask nudge** use strong **MANDATORY** copy and `deliver_as_user_on_api=true`.  
- **`handle_tool_call`**: generic success string; **`mk_diff`** behavior per action type (§2.5).

### 5.3 System prompt content

- **`src/haz3lcore/CompositionCore/prompt_factory/CompositionPrompt.re`**  
  - **`message_channels`**: what each channel means; do not echo context banners.  
  - **`partnering_and_user_intent`**: user leads; no “error-free” refusal loops; anchor on latest user message; statics vs intent; scope discipline; narration when blocked.  
  - Guidelines: **never paste `[CONTEXT UPDATE…]`** into user-visible replies.  
  - MANDATORY acknowledgment for Retry / Workbench / Required follow-up lines.

- **`src/haz3lcore/CompositionCore/prompt_factory/CompactionPrompt.re`**  
  - Excerpt list includes message channels + partnering; preserve “user wanted edits despite clean statics” in summaries.

---

## 6. Tests

- **`test/Test_AgentTools.re`**  
  - Search: **`HighLevelNodeMap`**, **`UpdateBindingClause`**, **`path_to_id`**, **`closest_valid_path`**, **`syntax_projector`**, **`place_syntax_projector`**.  
  - Covers typed patterns, body-chain vs def-nested paths, `update_binding_clause` + path rules, projector tool **parsing** / registration (counts may be asserted — adjust if tools list grows).

Run agent-heavy tests:

```bash
dune build test/haz3ltest.bc.js
node --stack-size=8192 --require ./test/idb_stub.js _build/default/test/haz3ltest.bc.js test "AgentTools"
```

---

## 7. UI: assistant markdown

- **`src/web/view/AgentView/AgentMessageMarkdown.re`** — Omd-based rendering with **allowlisted** link/image URLs (`http`, `https`, `mailto`, `#`); code blocks and inline code styled for agent bubbles.  
- **`src/web/view/AgentView/ChatMessagesView.re`** — agent chunk messages use **`AgentMessageMarkdown.view`** instead of plain `text` where applicable.  
- **`src/web/www/style/agent/agent-chat-messages.css`** — styles for `.agent-md-*` and related layout.

Ensure new `.re` files are picked up by the web `dune` rules (this repo often uses `(include_subdirs …)` / glob — verify `dune build src/web/` after adding modules).

---

## 8. Related files quick index

| Topic | Primary files |
|--------|----------------|
| Tool JSON | `SyntaxProjectorTools.re`, `CompositionUtils.re` |
| Actions | `CompositionActions.re` |
| Projector logic | `ProjectorPerform.re` |
| Agent dispatch + diff + retries | `Agent.re` (`ToolCallHandler`, `handle_tool_call`, `mk_diff`, `mk_retry_note_message`, `RetryEmptyResponse`, file header) |
| Node paths | `HighLevelNodeMap.re` |
| Edit tool docs | `EditTools.re` |
| Prompts | `CompositionPrompt.re`, `CompactionPrompt.re`, `ProjectorCatalog.re` |
| OpenRouter roles | `src/util/OpenRouter.re` (`Developer` → `"system"`) |
| Context string | `Agent.re` `mk_context_message`, `CompositionView.re` `zipper_for_agent_context` / `print` |
| Tests | `test/Test_AgentTools.re` |

---

## 9. Suggested follow-ups (optional)

- Extend **no-op detection** for probe/statics tools the same way if they can silently skip paths.  
- **Post-filter** assistant text to strip `[CONTEXT UPDATE` if models still leak it despite prompts.  
- **Compaction** summaries: ensure partnering rules survive aggressive truncation.  
- **Language server** `LanguageServerAction` stub in `Agent.re` — wire when ready; keep diagnostics in context blocks per policy comment.

---

## 10. Branch / git

Branch name observed in development: **`coding-agent-projector-tools-extension`**. After pulling, run:

```bash
dune build
```

and the targeted test command in §6.

---

*Last updated: generated as part of branch documentation commit; amend this file when you add agent tools or change `ToolCallHandler` / prompt contracts.*
