# Agent harness eval: structural edits vs. text edits

Working folder for the experiment Andrew proposed: run coding tasks through
**Hazel's in-editor agent (Filbert, structural path-addressed tools)** and
through **third-party file-based harnesses (opencode / pi / codex, plain text
edits + the Hazel CLI)**, score both with the same hidden tests, and compare.

The variable under test is the **action language**. Everything else (language
reference, exercises, scorer) must be shared between the two arms.

| File | What it is |
|---|---|
| [`findings.md`](./findings.md) | Audit of the current prompt, tools, and CLI against the plan (2026-09-14). |
| [`bugs.md`](./bugs.md) | Every defect found, with location and whether it blocks the eval. |
| [`plan.md`](./plan.md) | Ordered work items to get the experiment running. |

Related: `agent-docs/` holds the prompt-caching research; the headless driver
for the in-editor arm (`src/CLI/AgentEval.re`) currently lives on the
`cost-display-refinements` branch.
