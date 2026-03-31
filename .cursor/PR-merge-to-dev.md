# PR: `coding-agent-ui-updates` → `dev`

Branch summary (for the GitHub merge description):

- **Chunked chat UI** — User/agent message chunks in chat views; bottom bar **context meter** (tokens vs model `context_length`, bar + tooltip).
- **AgentGlobals** — Model `context_length`, **`effective_context_meter_limit`** for meter + **auto-compaction** when `prompt_tokens` crosses the cap.
- **Program View** — Read-only `<agentEditorView>` in agent context (same pipeline as tool diffs); crash fix for bad syntax; section titled **“Program View”**.
- **Folds in read-only code** — Empty `shape_map` fold path renders **⋱** (`fold-projector`); main editor unchanged.
- **Compaction** — Manual **`/compact`** slash palette + shared auto path; **`CompactionPrompt`** in `prompt_factory/`; compaction API appends final **`<context>`** (read-only); compacted notice + bottom **“Compacting…”** banner.
- **OpenRouter** — Decode assistant `content` as string, array of text parts, or empty + **`reasoning`** fallback.
- **Tests** — **`test/Test_AgentUX.re`** (“Agent UX” in `haz3ltest`): slash menu, dialogue slice, compaction prompt, `handle_chat`, context meter (tool tests stay in **`Test_AgentTools`**).
- **Repo** — **`.gitignore`** `.cursor/`; this file tracked with **`git add -f`** for reviewers.
