# caching-eval

Lightweight, headless eval suite that validates whether OpenRouter prompt caching actually
works, per model, for every model family the app sends `cache_control` to.

- **[pre-implementation-report.md](pre-implementation-report.md)** — question, method, model
  matrix, externally-validated facts, expected cost. Read this first.
- **[findings.md](findings.md)** — post-run conclusions (which models cache effectively).
- **[harness/](harness/)** — `run_eval.mjs` (Node ≥18, zero deps) + `models.json` (matrix/protocol).
- **[outputs/](outputs/)** — append-only data: `raw/` request+response JSONL per run,
  `summary/` structured results, verdicts, credits ledger, CSV.

Two drivers (see the report for the full method):

```sh
# Driver A (primary): the REAL agent loop, headless — simulates a user session
# on the site's coding agent (real prompts, tools, cache anchors, /cost math).
OPENROUTER_API_KEY=... ./hazel agent-eval                       # full matrix, ~$0.30 expected
OPENROUTER_API_KEY=... ./hazel agent-eval --models anthropic/claude-haiku-4.5 --turns 2

# Driver B (control): provider-isolated JS harness, no app code.
cd agent-docs/caching-eval/harness
OPENROUTER_API_KEY=... node run_eval.mjs --dry-run   # free — validates models, exact cost estimate
OPENROUTER_API_KEY=... node run_eval.mjs             # paid — ~12 LLM calls, ~$0.05 expected
```
