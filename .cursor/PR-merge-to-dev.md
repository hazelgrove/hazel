# PR: `coding-agent-ui-updates` → `dev`

Branch commits (newest first): compaction/OpenRouter polish → `.gitignore` for `.cursor/` → slash `/compact` → read-only fold `⋱` fix → Program View crash fix → auto-compaction on context limit → context meter UI → chunk UI + context meter foundation.

Use this list as the full merge description; it is meant to match **everything** we intend to land from this branch.

---

## Agent chat UI & context budget

- **Chunked UI chat:** Messages are grouped into user chunks and agent response chunks (`ChunkedUIChat`) for rendering in `ChatMessagesView` / `ChatView` / `ChatBottomBar`.
- **Context meter:** Bottom bar shows **last response `prompt_tokens` / `total_tokens`** vs the active model’s **`context_length`** (from OpenRouter’s model list), with a **themed progress bar** and **hover tooltip** (usage fraction to six decimals where applicable).
- **`AgentGlobals`:** Parses optional **`context_length`** per model; **`effective_context_meter_limit`** / **`context_meter_limit_for_active`** cap the budget used for the meter and for **auto-compaction** (below).
- **Styling:** Related chat/agent UI tweaks in `agent-chat-messages.css` (and connected views).

## Agent Context / “Program View”

- **`<agentEditorView>` in chat:** The agent context area shows the program using the **same read-only Hazel segment pipeline** as tool-result diffs (live editor semantics + collapse rules where applicable), with surrounding context text still below.
- **Section title:** **“Program View”** (not a generic “Program” label).
- **Crash fix:** Incomplete/bad syntax no longer crashes the Program View path (`bc527b680d`).

## Read-only code & folds (`Code.re`)

- **`CodeViewable.view_segment`** uses an **empty `shape_map`**, so the generic projector path used to render **nothing** for folds.
- **Fix:** When `pr.kind == Fold` and `shape_map` is empty, render the canonical **⋱** glyph with **`fold-projector`** styling (aligned with `FoldProj`). The **main editor** still uses a non-empty `shape_map`; its fold layout is unchanged.

## Compaction (auto + manual) & slash commands

- **Auto-compaction:** After **`HandleLLMResponse`**, if **`prompt_tokens` ≥ context meter limit** (and no compaction already running), start the same compaction flow as manual (shared `maybe_start_compaction`).
- **Slash commands:** Typing **`/`** opens a **palette** (filter, alphabetical commands, **ArrowUp/Down**, **Enter** to run, **Escape** to dismiss). **`/compact`** forces compaction (same path as auto).
- **`ChatSystem`:** `slash_menu` state; **`RequestForcedCompaction`**; optional **`compaction_method_override`** (e.g. “Slash command (/compact)”) for the compaction summary line.
- **`CompactionPrompt.re`:** Long summarizer system prompt lives in **`prompt_factory/`** next to `CompositionPrompt` (not inlined in `Agent.re`). Payload includes a **truncated live agent system prompt** + **developer notes**.
- **Agent view in compaction API:** A **final `<context>` message** is appended (same shape as production: **`<agentEditorView>`**, static errors, test results, workbench) built from the **current** editor + **`agent_view`** — **read-only**; does not change UI folds/state.
- **UI:** **“Conversation compacted”** notice (method + summary body); **“Compacting conversation…”** banner at the **bottom** of the scrollable log (above the composer).

## OpenRouter response parsing

- Assistant **`content`** may be a **JSON array** of parts (e.g. `{"type":"text","text":"…"}`); **`first_message_content`** now decodes string / null / array and optionally falls back to a top-level **`reasoning`** string when content is empty — avoids **empty compaction/chat text** when providers return non-string `content`.

## Repo / tooling

- **`.gitignore`:** Ignore **`.cursor/`** for local IDE noise. **`PR-merge-to-dev.md`** is intentionally tracked (e.g. `git add -f`) so this checklist lives in the repo for reviewers.

---

## Files / areas touched (high level)

`Agent.re`, `AgentGlobals.re`, `OpenRouter.re`, `CompactionPrompt.re`, `CompositionPrompt.re` (removal of inline compaction blurb), `ChatMessagesView.re`, `ChatBottomBar.re`, `ChatView.re`, `Code.re`, `CodeViewable.re` / tool-result views, `agent-chat-messages.css`, `.gitignore`, this file.
