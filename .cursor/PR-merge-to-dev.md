# PR: merge into `dev`

- Agent Context panel renders the program under `<agentEditorView>` with the same read-only Hazel segment view used for tool-result diffs in chat (live editor + collapse rules), with the rest of the context message still shown as text below.
- Chat bottom bar shows last-response `prompt_tokens` vs OpenRouter `context_length` with a themed progress bar and hover tooltip (fraction to six decimals); model list parsing stores optional `context_length` on each LLM.
