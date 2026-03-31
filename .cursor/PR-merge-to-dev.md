# PR: merge into `dev`

- Agent Context panel renders the program under `<agentEditorView>` with the same read-only Hazel segment view used for tool-result diffs in chat (live editor + collapse rules), with the rest of the context message still shown as text below.
- Chat bottom bar shows last-response `prompt_tokens` vs OpenRouter `context_length` with a themed progress bar and hover tooltip (fraction to six decimals); model list parsing stores optional `context_length` on each LLM.
- **Read-only folds:** `CodeViewable.view_segment` passes an empty `shape_map`, so fold projectors used to render as blank. When `kind == Fold` and `shape_map` is empty, we render the same **⋱** glyph and fold styling as `FoldProj`; the full interactive fold projector UI is not used in read-only unless we later wire projector shape/measurement there.
