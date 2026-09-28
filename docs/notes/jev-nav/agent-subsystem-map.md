# Agent subsystem map (dev @ 761b865b)

Read-through notes for the Hazel coding agent ("Filbert"), written to ground the
Jev navigation experiment. Authorship: mostly russell-rozenbaum (63 commits) and
disconcision (53), since 2025-12-01. `agent-docs/` holds the team's own research
notes; this file is the map I needed and could not find there.

## Where things live

| Concern | Path |
|---|---|
| Agentic loop, HTTP, retries, stop | `src/web/view/agentCore/AgentSend.re` |
| Reply → tool exec → follow-up turn | `agentCore/AgentResponse.re` |
| One tool call → editor action | `agentCore/AgentToolExec.re`, `AgentToolCallHandler.re` |
| Context snapshot refresh | `agentCore/AgentUtils.re` (`update_context`) |
| Wire message shaping + cache anchor | `agentCore/Chat.re` (`messages_for_openrouter`) |
| Snapshot format | `agentCore/Message.re` (`context_snapshot_body_for_llm`) |
| Settings: key, model, mode | `agentCore/AgentGlobals.re` |
| Tool JSON schemas (36 tools) | `src/haz3lcore/CompositionCore/ToolJsonDefinitions/` |
| Tool name → action variant | `CompositionCore/CompositionUtils.re` (`action_of`) |
| Action ADT | `CompositionCore/CompositionActions.re` |
| Binding index (the "structure") | `CompositionCore/HighLevelNodeMap.re` |
| Expand/collapse state | `CompositionCore/AgentContextCore/AgentContext.re` |
| Collapsed-view renderer (`⋱`) | `CompositionCore/CompositionView.re` |
| System prompt | `CompositionCore/prompt_factory/CompositionPrompt.re` |
| HTTP (XHR + SSE) | `src/util/API.re`, `src/util/OpenRouter.re` |
| Tests | `test/Test_Agent{Tools,UX,MultiTool,ControlFlow}.re` |

## The loop

1. `SendMessage` appends the user message (paints this frame).
2. `DispatchSend` (0 ms macrotask later) → `update_context` rebuilds the snapshot →
   `send_llm_request` streams SSE from `POST openrouter.ai/api/v1/chat/completions`.
3. `HandleLLMResponse`: no tool calls → idle (maybe compaction). Tool calls →
   executed **sequentially, stop on first failure**, remaining calls marked skipped;
   each produces a tool-result message; then `dispatch_follow_up_llm` fires the next
   turn immediately. Repeat until a reply has no tool calls.
4. Empty replies retry ≤2 with a nudge; 429/5xx retry ≤3 with backoff.

Wire order every turn: `[system prompt, dev notes, …history after last compaction,
context snapshot]`. The snapshot is **one system message refreshed in place** and is
always last. OpenRouter→Anthropic honors `cache_control` only on system messages
(see `agent-docs/prompt-caching-findings.md`), so the prompt prefix stays cached
while the snapshot churns. Any new navigation step must keep that ordering.

## Navigation as it exists today

- **Index.** `HighLevelNodeMap.build(zipper, info_map)` walks Let / TyAlias /
  ModuleExp bindings into `Id.Map(node)`; `node = {info, path, children, siblings,
  sibling_idx, name}`. Addressed by slash paths (`"a"`, `"M/inner"`), `#k` for
  shadowed names. Rebuilt from scratch on every tool call (O(bindings), fine).
- **State.** `AgentContext.Model.expanded_paths : list(string)` per chat. Paths,
  not ids, so edits do not invalidate them; `freshen_paths` drops dead ones.
- **Render.** `CompositionView.print` folds every top-level binding *not* in
  `expanded_paths` via the `Fold` projector and prints it as `⋱`. That string is
  the `<agentEditorView>` block of the snapshot.
- **Tools.** `expand(paths)` / `collapse(paths)` are pure list mutations followed by
  a re-render. They are the only tools left in `converse` mode. Probe / statics /
  projector tools auto-expand what they touch.

**The cost:** each `expand` or `collapse` is a full main-model turn carrying the
~400-line system prompt, the history, and the snapshot. Navigation is the cheapest
decision in the system and pays the most expensive price. That is the gap Jev fills.

## Transport facts for a Jev call

- `API.request(~method=POST, ~url, ~headers, ~body, handler)` is a plain XHR wrapper;
  `OpenRouter.Utils.chat` hardcodes the chat URL and adds `Authorization: Bearer key`.
- The key is `settings.agent_globals.api_key`: entered in the UI, persisted with
  settings, **never in the repo**. A Jev call through OpenRouter reuses it as-is.
- OpenRouter serves Jev at a separate endpoint from chat completions
  (see `plan.md`); the model slug is `typesafe/jev-latest`.

## Testing conventions

- Alcotest; `make test` (dune, auto-promote fmt). `make test-quick` for the fast alias.
- `Test_AgentControlFlow` drives `Agent.Update.update` with a `scheduled` ref and
  drains it — no HTTP. `AgentSend.defer_dispatch_send` is a ref tests override to run
  synchronously. Same seam pattern is the right way to inject a fake Jev client.
- `Test_AgentTools.re:1386+` builds node maps from source strings; reuse
  `build_node_map` for navigation tests.
- 343 agent test cases today across the four files.

## Gotchas

- `docs/overview.md` is stale (pre-`haz3lcore` names). Ignore it.
- `.gitignore` ignores `plans/`, so `docs/plans/` is silently untracked. Use `docs/notes/`.
- Toolchain: global opam switch `5.2.0`, dune 3.19.1. `make deps` needs `OPAMYES=1` and
  brew `gmp zlib` when run non-interactively. Full `make test` ≈ 12 min / 3982 tests.
