# Prompt-Caching Eval — Findings

_Run 2026-08-04. Both drivers, full 4-model matrix. Raw data in [`outputs/`](outputs/);
method in the [pre-implementation report](pre-implementation-report.md)._

## TL;DR

- **Prompt caching works end-to-end through our production code** for Claude Haiku 4.5,
  Claude Sonnet 4.6, and Gemini 3 Flash — both in the isolated control harness and in real
  headless agent sessions. Claude turn-2 cost drops ~87% vs turn 1 in the control harness.
- **Qwen3-coder does not cache at all** (no cache writes, no reads, full price every turn),
  despite OpenRouter docs listing Qwen as `cache_control`-capable and our payload carrying the
  marker. Real-session consequence: an 11-call Qwen session billed **$0.23** while the
  equivalent 10-call Haiku session billed **$0.06** — the "cheap" model was ~4× dearer.
- **The new `/cost` accumulator (PR #2423) ran live in every real-agent session** and produced
  coherent billed/cached/saved numbers (e.g. Sonnet: billed $0.285, 471k cached tokens,
  $1.16 saved vs list price).

## Verdicts

| model | control harness (wire-only) | real agent session | notes |
|---|---|---|---|
| anthropic/claude-haiku-4.5 | **WORKING** — read 5,669 = 100% of write; cost $0.0074 → $0.0009 | **WORKING** — t1 write 23,944; later reads 25,193 | |
| anthropic/claude-sonnet-4.6 | **WORKING** — read 5,670 = 100% of write; cost $0.0223 → $0.0028 | **WORKING** — t1 write 23,602; later reads 26,478 | |
| google/gemini-3-flash-preview | **WORKING** — read 5,090; cost $0.0007 → $0.0003 | **WORKING** — t1 write 20,818; later reads 20,832 | turn-1 reports read=write (see anomalies) |
| qwen/qwen3-coder | **NOT_WORKING** — zero cache fields, flat full price | **NOT_WORKING** — zero across 11 calls | provider ignores our (correct) `cache_control` |

## Real agent sessions (3 scripted user turns each)

| model | LLM calls | billed | Σ cache_read | Σ cache_write | output tokens |
|---|---|---|---|---|---|
| claude-haiku-4.5 | 10 | $0.061 | 218,878 | 27,809 | 918 |
| claude-sonnet-4.6 | 20 | $0.285 | 471,236 | 29,824 | 2,134 |
| gemini-3-flash-preview | 11 | $0.022 | 229,040 | 41,650 | 1,011 |
| qwen3-coder | 11 | $0.233 | 0 | 0 | 932 |

Call counts above 3 are agentic tool rounds (the models genuinely used the workbench tools;
Sonnet was the most tool-happy at 20 calls). Reads accumulate per call, which is why session
totals dwarf the ~24k per-request floor.

## `/cost` accumulator validation (PR #2423 code, run live)

`AgentSlashFormat.cost_payload` executed on each real session's chat:

| model | billed | cached tokens | saved vs list |
|---|---|---|---|
| claude-haiku-4.5 | $0.0613 | 218,878 | $0.162 |
| claude-sonnet-4.6 | $0.2853 | 471,236 | $1.160 |
| gemini-3-flash-preview | $0.0216 | 229,040 | $0.079 |
| qwen3-coder | $0.2327 | 0 | — (correctly absent) |

The `saved` gate behaved as designed: present only where caching produced a real discount,
absent for Qwen where list price ≈ billed.

## Cross-checks

- [x] Σ per-request `usage.cost` ≈ `/credits` ledger delta — approximately: deltas lag summed
      costs by ~5–20% immediately after a run (e.g. Sonnet run: ledger $0.255 vs summed $0.285),
      and the control-harness delta read $0.00 seconds after finishing. **OpenRouter's
      `/credits` usage counter updates with a delay**; per-request `usage.cost` is the reliable
      real-time signal (good news for the `/cost` display, which uses exactly that).
- [x] `cached_tokens` ≥ 50% of turn-1 write on every WORKING model (in fact ~100%).
- [x] Billed cost consistent with published multipliers — Claude turn-2 ≈ 0.1× input rate on the
      cached portion; turn-1 carries the 1.25× write premium.
- [x] `prompt_tokens` over-reporting on cache-write turns: **confirmed on Gemini** (turn 1
      reports `cached_tokens` = `cache_write_tokens` = 5,090 on a cold cache — the read is
      folded into the reported count). Not observed on Anthropic (turn-1 read = 0). This is the
      provider quirk the PR #2423 write-subtraction fix corrects.

## Anomalies & follow-ups

1. **Qwen3-coder**: our payload carries a syntactically correct ephemeral `cache_control`
   (verified in `outputs/raw/*.requests.jsonl`), yet the serving provider reports no cache
   activity and bills full price. Follow-up: try a Qwen model hosted by Alibaba directly, or
   drop `qwen/` from the allowlist's *expectations* (the marker is harmless but the savings
   assumption is false). Until then, Qwen models are a poor default for agent sessions —
   4× Haiku's cost in this eval despite a lower list price.
2. **Gemini turn-1 read=write**: cosmetic reporting quirk, already compensated for in the
   `/cost` math; no action needed.
3. **`/credits` lag**: don't use the credits endpoint for real-time spend verification;
   per-request `usage.cost` is authoritative.

## Total eval spend

~$0.65 (control harness ~$0.05 + real-agent sessions ~$0.60). Above the ~$0.35 midpoint
estimate — Sonnet's 20-call tool enthusiasm and Qwen's uncached full-price calls — but under
the ~$1.30 worst-case ceiling.
