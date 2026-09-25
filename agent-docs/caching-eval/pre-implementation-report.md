# Prompt-Caching Eval — Pre-Implementation Report

_Status: harness implemented, **not yet run** (awaiting go-ahead; the run spends real credits)._
_Companion to [`../prompt-caching-findings.md`](../prompt-caching-findings.md). Written 2026-08-04._

## Question

For every model family the app allowlists for explicit prompt caching
(`OpenRouter.Payload.Utils.cache_control_provider_prefixes` = `anthropic/`, `google/`, `qwen/`):
**does caching actually work over the wire, measurably, per model** — cache writes on turn 1,
cache reads on later turns, and a billed cost that reflects the discount?

This turns the earlier findings doc's Claude-centric evidence into a repeatable, per-model,
data-backed check, and directly validates the arithmetic the `/cost` display
(PR [#2423](https://github.com/hazelgrove/hazel/pull/2423)) builds on.

## Method — two drivers

### Driver A (primary): `hazel agent-eval` — the real agent, headless

`./hazel agent-eval` (new CLI subcommand, `src/CLI/AgentEval.re`) simulates a user session on
the website's coding agent by driving the **production `Agent.Update` state machine** — the same
harness pattern the agent tests use, but with a live API key:

- real system prompt + dev notes (carrying the production `cache_anchor` floor breakpoint),
  real tool definitions, real payload serialization (`json_of_payload`), real streaming/response
  parsing, real tool execution if the model calls tools
- scripted user inputs (a small coding ask, a follow-up, a terse control turn), 3 turns/model
- network via a synchronous curl-backed `XMLHttpRequest` polyfill (`src/CLI/polyfill.js`), so
  every handler completes before the CLI exits
- per-turn raw usage rows, the real `AgentSlashFormat.cost_payload` output (the exact numbers
  `/cost` shows a user), per-model verdicts, and a `/credits` before/after ledger — all JSONL

This answers the real question: **is caching working for our users, through our code, end to end.**
Side effect of authenticity: each new chat also fires the app's chat-naming call
(`google/gemini-3.1-flash-lite`, ~tiny), and agentic tool rounds add LLM calls beyond the 3
scripted turns.

### Driver B (control): `harness/run_eval.mjs` — provider-isolated JS harness

Headless Node script, no app code. It replicates the app's
production wire shape exactly (from `src/util/OpenRouter.re`):

- endpoint `POST https://openrouter.ai/api/v1/chat/completions` (OpenAI-compat, non-streaming)
- one `cache_control: {type: "ephemeral"}` breakpoint on a multipart content block on the
  **system** message — the app's static "floor" anchor
- top-level `session_id` per model run — sticky provider routing, same as production
- `tools` present (two inert function defs) so the payload shape matches the agent's

**Protocol per model** — 3 turns, one session, ~6,000-token deterministic system prefix
(well above every provider's min-cacheable threshold), `max_tokens: 16`, `temperature: 0`:

| turn | messages | expectation if caching works |
|---|---|---|
| 1 | system(anchored) + user | `cache_write_tokens` ≈ prefix, `cached_tokens` ≈ 0 |
| 2 | + assistant + user | `cached_tokens` ≥ 50% of the turn-1 write; cost < turn 1 |
| 3 | + assistant + user | `cached_tokens` still ≈ floor (history is NOT cached on OpenRouter — known limitation) |

**Verdict rule**: `WORKING` if some later turn's `cached_tokens` ≥ 0.5 × turn-1 write (falling
back to the prefix estimate for implicit-cachers that report no write) *and* billed cost does not
contradict it; `NOT_WORKING` otherwise; `INCONCLUSIVE` when usage fields are missing.

**Spend cross-check**: `/credits` is snapshotted before and after the run; the delta must
reconcile with the sum of per-request `usage.cost` — the same invariant the `/cost` UI relies on.

## Model matrix

Cheapest viable member of each allowlisted family + the production default. Extensible via
`harness/models.json`; ids are validated against the live `/models` endpoint before any spend.

| model | list price in/out per M | why |
|---|---|---|
| `anthropic/claude-haiku-4.5` | $1 / $5 | cheapest Claude with caching; strictest min-prefix threshold |
| `anthropic/claude-sonnet-4.6` | $3 / $15 | the app's default model — the result that matters most |
| `google/gemini-3-flash-preview` | $0.50 / $3 | cheap; the model where `prompt_tokens` over-reporting was measured |
| `qwen/qwen3-coder` | $0.22 / $1.80 | cheap representative of the third family |

## External validation (web, checked 2026-08-04)

Verified against OpenRouter's docs before implementation:

- **Explicit `cache_control` support**: Anthropic, Google Gemini, Alibaba Qwen — exactly the
  app's allowlist. OpenAI/DeepSeek/Groq/etc. cache implicitly and ignore the field.
- **Usage fields**: cache metrics arrive in `prompt_tokens_details` — `cached_tokens` (reads)
  and `cache_write_tokens` (writes, explicit-caching models only). `usage.cost` is the amount
  actually charged (credits) and is now **always included** — the old `usage: {include: true}`
  flag is deprecated. `cache_discount` states the per-response saving directly.
- **Pricing multipliers**: Anthropic & Qwen — writes 1.25×, reads 0.1×. Gemini — reads 0.25×
  (+ storage on writes). Groq reads 0.5×, DeepSeek 0.1×.
- **Min cacheable prefix**: 1,024–4,096 tokens depending on model → the 6k prefix clears all.
- **Gemini caveat**: only the final breakpoint applies; first-system-message content is treated
  as immutable — fine here (our prefix is byte-stable).

Sources: [openrouter.ai/docs/features/prompt-caching](https://openrouter.ai/docs/features/prompt-caching),
[openrouter.ai/docs/use-cases/usage-accounting](https://openrouter.ai/docs/use-cases/usage-accounting),
per-model pricing pages.

## Expected cost & call count

**Driver B (control harness)**: 12 LLM calls (4 models × 3 turns), ~6k-token synthetic prefix,
output ≤ 48 tokens. Expected **~$0.05**, worst case ~$0.09. `--dry-run` recomputes from live
`/models` pricing for free.

**Driver A (real agent)**: the production payload is ~24k tokens/turn (~22.3k stable prefix,
per the findings doc), and agentic tool rounds make call count variable: 3 scripted turns can
become 3–10 agent calls per model, +1 chat-naming call. Estimated per model (caching working /
broken):

| model | expected | worst case (no caching, max tool rounds) |
|---|---|---|
| claude-haiku-4.5 | ~$0.06 | ~$0.25 |
| claude-sonnet-4.6 | ~$0.17 | ~$0.75 |
| gemini-3-flash-preview | ~$0.03 | ~$0.12 |
| qwen3-coder | ~$0.02 | ~$0.06 |
| **total** | **~$0.30** | **~$1.20** |

Combined A+B: **~$0.35 expected**, hard ceiling ~$1.30; roughly **16–44 LLM calls** for A plus
12 for B. The `/credits` ledger records actual spend either way.

## Data accumulators

Everything append-only under [`outputs/`](outputs/) — raw first, summaries derived:

| file | contents |
|---|---|
| `raw/<runid>.requests.jsonl` | every request body exactly as sent |
| `raw/<runid>.responses.jsonl` | every raw response JSON exactly as received (+ latency, errors) |
| `summary/results.jsonl` | one structured row per (run, model, turn): tokens, cache read/write, cost, discount |
| `summary/verdicts.jsonl` | one verdict row per (run, model) with the reason |
| `summary/ledger.jsonl` | `/credits` before/after each run — independent spend cross-check |
| `summary/results.csv` | flat CSV of all result rows (regenerated each run) |
| `summary/estimate.json` | dry-run cost estimate from live pricing |

Post-run conclusions go to [`findings.md`](findings.md).

## Known limitations

- One run is a point sample; caches are shared infrastructure, so a cold-cache turn 1 is not
  guaranteed (a prior identical prefix could pre-warm it). The deterministic prefix embeds no
  run id precisely so reruns are comparable; use `--turns`/reruns for repeated measures.
- The 3-turn protocol tests the **floor anchor** only. `--anchor-history` adds the app's
  advancing anchor to probe the known-dead phase-2 path if we ever want fresh evidence.
- Costs are OpenRouter credits (≈ USD); BYOK upstream costs are out of scope.
