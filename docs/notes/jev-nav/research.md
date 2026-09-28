# Research notes — Jev, OpenRouter, prior art

Everything read for this design, with what each source changed. Dated 2026-09-22.

## Jev / TypeSafe (official)

- **What it is** — https://docs.typesafe.ai/introduction/coding-agents ·
  https://docs.typesafe.ai/concepts/system-one. System One model: `state` + typed
  questions → typed answers with probabilities and calibrated confidence. Never text.
  Not a drop-in for the LLM behind a coding agent; meant to be *called by code* the
  agent writes or runs.
- **API** — https://docs.typesafe.ai/api.md. `POST https://api.typesafe.ai/v1/systemone`,
  Bearer key, `model: "jev-latest"`, `state` (string | object | array), `questions`
  map. Answer types: Noul (p ∈ [0,1]), Choice (≤255 options, per-option probs +
  confidence), Score (2–10 rubric levels). Errors 401/422/429/529; back off.
- **State** — https://docs.typesafe.ai/concepts/state.md. Prefer an object with
  descriptive keys; group what a decision compares; all questions see the same state.
  No documented size limit.
- **Batching** — https://docs.typesafe.ai/cookbooks/parallel_questions.md. One state,
  N questions; "the document dominates every request"; 12.2× cheaper than N requests.
- **Semantic find** — https://docs.typesafe.ai/cookbooks/semantic_find.md. 218 tagged
  lines (`L052| …`) in one request; thresholds FOUND ≥ 0.7, ABSENT ≤ 0.35, between =
  partial. → our per-binding thresholds and the "band = escape hatch" rule.
- **Hierarchical classification** — https://docs.typesafe.ai/cookbooks/hierarchical_classification.md.
  Traverse a tree with one Choice per node's children; beam K=3 by geometric-mean
  edge probability beat greedy 4/4 vs 2/4; siblings in parallel. → frontier descent.
  (We use Noul per child, not Choice, because several children may open.)
- **Function calling** — https://docs.typesafe.ai/cookbooks/function_calling.md. One
  `Choice("__tool__")` + per-argument questions in one request; **sets → one yes/no
  per candidate**; confidence = least-certain judgement. → Noul-per-binding; V3 shape.
- **Skill suggestion** — https://docs.typesafe.ai/cookbooks/skill_suggestion.md.
  Choice over 182 skills + gating Nouls; two-stage; 2.3× fewer wrong loads vs Haiku
  alone (488 requests). → V2 route gate, later.
- **Cascade** — https://docs.typesafe.ai/cookbooks/sde_cascade.md. Cheap rung → Jev
  verifier → escalate on any P(wrong) > 0.7. → pattern for "escalate to the main model".
- **Confidence routing** — https://docs.typesafe.ai/patterns/confidence-routing.md.
  0.6 floor for low-stakes reversible actions; 0.85+ for risky; ambiguous band → confirm.
  → `expand` is reversible: 0.6–0.7 tuning range; edits never Jev.
- **Jaggedness (weaknesses)** — https://docs.typesafe.ai/model-jaggedness/jev-1.13.md.
  Literal reading; not a calculator; dates as text; **indirection / multi-hop**;
  **large irrelevant state**; adversarial state; contradictory instructions; `P(noul)`
  vs `1−P(not noul)` not comparable; no generation. Mitigations: name relevant state
  directly, filter context, align wording, keep arithmetic in code. → question wording
  rules; no "sufficient?" question; strip comments; Noul-only for navigation.
- **Agent skill** — https://docs.typesafe.ai/agent-skill.md (Claude Code skill for
  generating integrations; optional).
- **Docs index** — https://docs.typesafe.ai/llms.txt (111 pages).

## OpenRouter route

- https://openrouter.ai/docs/guides/community/typesafe-sdk — endpoint
  `https://openrouter.ai/api/v1/systemone`, slugs `jev-1.13` / `jev-latest`
  (auto-prefixed `typesafe/`), OpenRouter key as Bearer; SDK `base_url="https://openrouter.ai/api"`.
- https://openrouter.ai/provider/typesafe — `typesafe/jev-1.13`, $0.042/M in, $0 out,
  32K context.
- Verified 2026-09-22: `POST /api/v1/systemone` and `POST /api/alpha/decisions` both
  return 401 without a key (exist). Not listed in `/api/v1/models` (444 models, 0 hits)
  because it is not a chat model. One aggregator site names `tokenra.io/v1/decisions`
  — wrong, ignore.

## Independent evaluations (via https://github.com/AbdelStark/awesome-typesafe-jev)

- `jev-calibration-audit` — https://github.com/jujumilk3/jev-calibration-audit. Jev
  chose "unknown" for 95% of ambiguous items when offered; **0% accuracy** when that
  option was removed. → never remove the escape hatch; the 0.35–0.7 band is it.
- `jev-orderby-bench` — https://github.com/yodablocks/jev-orderby-bench. Passed topic
  membership; failed 4/6 relevance-ranking conditions with tied probabilities. →
  threshold, never rank.
- `jev-certify` — https://github.com/nikkoxgonzales/jev-certify. 84.75% auto-routing
  on CLINC150 with 2.25% loss; the bound broke when out-of-scope prevalence shifted.
  → thresholds are distribution-dependent; re-tune on our tasks.
- `Janus` — https://github.com/FirasSX914/Janus. Jev+LLM ensemble sometimes matched
  Jev alone at 47% higher cost. → do not add a second model to the navigation path.
- JevBench https://github.com/fstandhartinger/jevbench · Jevals https://jevals.com/ —
  composite accuracy/calibration/speed/cost; Jev vs six LLMs on PubMedQA, Banking77,
  HelpSteer2.

## Coding-tool integrations that already use Jev per code chunk

- `jgrep` — https://github.com/kyu1204/jgrep. Semantic grep: `state.chunks[]` + one
  Noul per chunk, **16 chunks per request** (`--batch`), ~1.8 s for a TypeScript `src/`,
  cache keyed `(model, question, chunk)`. → batch cap and the per-item-in-one-state shape.
- `Distill` — https://github.com/samuelfaj/distill. Coding agent routing utility tasks
  and context retention via Jev.
- `jev-use` — https://github.com/shitianfang/jev-use. Claude Code plugin with batched
  `jev_judge` and PreToolUse gating.
- `Switchboard`, `Canny`, `jev-belay`, `fx` — model-tier choice, rule checks, "done"
  verification, permission review. Evidence that "Jev gates, LLM acts" is a working
  pattern in agent harnesses.

## Prior art on context curation for coding agents

- **ContextCurator** — https://arxiv.org/abs/2604.11462 "Escaping the Context
  Bottleneck: Active Context Curation for LLM Agents via RL". Small policy curates
  working memory for a frozen big model; prunes noise, keeps "reasoning anchors".
  Same two-model split as ours; theirs is learned, ours is Jev + a walker.
- **Forge / Intent.Lisp** — https://arxiv.org/abs/2604.13108 "Formal Architecture
  Descriptors as Navigation Primitives for AI Coding Agents". Structural context cut
  navigation steps **33–44%** on 24 localization tasks; 100% vs 80% localization
  accuracy; 52% less behavioral variance across 7,012 Claude Code sessions. →
  headline metric = navigation steps.
- **CodeCompass** — https://arxiv.org/pdf/2602.20048 (navigation paradox in agentic
  code intelligence). **Active Context Compression** — https://arxiv.org/html/2601.07190v1
  ("sawtooth" context: grow while exploring, collapse when consolidating). **Context
  Pruning via Multi-Rubric Latent Reasoning** — https://arxiv.org/pdf/2605.15315.
  **Context Compaction Theory** — https://arxiv.org/html/2608.01326v1. Background;
  not yet read in depth.
- Industry pattern (Towards Data Science, "context compiler"): compress inactive
  files to signatures at 40% capacity; recently touched files stay full; auto-restore
  on access. → Hazel's `⋱` + `head` summaries are the same idea at binding granularity.

## Hazel-internal references

- `agent-docs/prompt-caching-findings.md` — OpenRouter→Anthropic honors
  `cache_control` only on system messages; snapshot must stay last. Constrains where
  Jev output may be injected (only into the snapshot).
- `docs/agent-harness-eval/{findings,bugs,plan}.md` (PR #2571) — tool inventory,
  scorer gaps, B1–B5.
- `origin/incr-eval-agent-bench` — `hazel agent` headless runner and five tasks.
