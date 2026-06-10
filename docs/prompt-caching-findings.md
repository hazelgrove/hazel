# Prompt Caching — Fix Report

_2026-06-09 · Phase 1 shipped & verified live: ~95% cache reads on `anthropic/claude-sonnet-4.6`._

## The bug
Our one cache breakpoint sat on the **context snapshot** — appended last, regenerated every
turn. Caching is a prefix match, so a breakpoint on volatile last content ≈ zero hits: we paid
the 1.25× write premium every turn and rarely read it back.

## The fix
Moved the breakpoint **off the snapshot** onto the stable **dev-notes** message, so the prefix
`tools + system prompt` caches across turns. The snapshot stays last, uncached.
(`OpenRouter.re` `cache_anchor` flag + `Message.re`.)

```mermaid
flowchart TB
    subgraph now["BEFORE ❌"]
        a1["system prompt ~20k"]:::s --> a2["dev notes"]:::s --> a3["history"]:::s --> a4["snapshot 🔄"]:::v
        a4 --- abp(["🔖 breakpoint"]):::bp
    end
    subgraph fix["AFTER ✅"]
        b1["system prompt ~20k"]:::s --> b2["dev notes"]:::s
        b2 --- bbp(["🔖 breakpoint"]):::bp
        b2 --> b3["history"]:::s --> b4["snapshot 🔄"]:::v
    end
    classDef s fill:#d6f5d6,stroke:#2e7d32,color:#000;
    classDef v fill:#ffd6d6,stroke:#c62828,color:#000;
    classDef bp fill:#fff3c4,stroke:#f9a825,color:#000;
```

## Result (verified live)
`cache_read 22,293 / prompt 23,436` → **~95% of every request served at 0.1× (~10× cheaper
input per turn).**

## How billing works (the one thing to know)
Each turn re-sends the whole prefix, billed: unchanged prefix **0.1×** (read), new delta
**1.25×** (write; **2×** for 1-hr TTL), tail after the breakpoint **1×**. You never pay full
price on what repeats; cache refreshes free on each hit (5-min sliding TTL). [1]

## Meeting questions, settled
- **"Prompt" = system prompt, or the whole thing?** The whole input (`tools → system → all
  messages`). Caching reuses *any prefix* of it — "cache the system prompt" and "cache the
  history" are the same feature at different breakpoint positions. [1][2]
- **"20k cached, next turn 21k — pay all 21k?"** No. The 20k bills at 0.1× (read), only the
  ~1k delta is written. You're never charged full price on the repeat. [1]
- **"So we pay the 25% write premium on the whole history every turn?"** No — only the *new
  delta* is written each turn; everything before it is read at 0.1×. Premium is paid once per
  chunk, recouped immediately on the next read. [1]
- **"Does it cache *any* prefix? Does context editing break it?"** Not arbitrary — it matches
  the longest cached prefix and only looks back **~20 blocks** per breakpoint. It *is* a strict
  prefix match, so editing mid-history invalidates everything below the edit. [1][3]
- **"Why not a breakpoint on every message?"** Unnecessary and impossible — a breakpoint caches
  *everything before it* (cumulative), and the system auto-checks prior block boundaries within
  the 20-block window. Hard cap is **4** breakpoints; one near the end usually suffices. [1]
- **"Is this all providers?"** No. Anthropic = explicit `cache_control` you place (what we use);
  OpenAI/Gemini/DeepSeek/Grok = automatic. Read ~0.1× (Anthropic/DeepSeek) to ~0.25–0.5× (OpenAI/
  Gemini). [1][4][5]

## Constraints
- **Strict prefix match** — editing mid-history (compaction, clipping stale views) invalidates
  everything below the edit. Keep history append-only; batch any edits. [1][3]
- **Anthropic-only** mechanism (explicit `cache_control`); other providers auto-cache. Stay on
  Anthropic models for cost control. [1][5]

## Left to do (optional)
- **Phase 2** — cache accruing history (2nd breakpoint before the snapshot; needs
  `cache_control` on user/tool messages → wants a live smoke-test). Marginal: our history is light.
- 1-hr TTL for >5-min think-gaps. Confirm OpenRouter's read rate on a real invoice.

## Sources
[1] [Anthropic — Prompt Caching](https://platform.claude.com/docs/en/build-with-claude/prompt-caching) ·
[2] [Claude Code](https://code.claude.com/docs/en/prompt-caching) ·
[3] [arXiv — "Don't Break the Cache"](https://arxiv.org/abs/2601.06007) ·
[4] [OpenRouter](https://openrouter.ai/docs/guides/best-practices/prompt-caching) ·
[5] [PromptHub provider comparison](https://www.prompthub.us/blog/prompt-caching-with-openai-anthropic-and-google-models)
