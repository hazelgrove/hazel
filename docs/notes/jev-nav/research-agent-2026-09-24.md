# Research sweep — Jev-style code agents (2026-09-24)

Independent web sweep. Complements [`research.md`](./research.md) (not repeated here).
Every link below was opened on 2026-09-24 unless marked **[unverified]**. Numbers are the
sources' own claims; none were reproduced.

## 1. Small/fast models selecting context for a big coding agent

- **SWE-grep / Fast Context (Cognition, 2025-10-16)** — https://cognition.com/blog/swe-grep.
  RL-trained retrieval subagent: ≤8 parallel tool calls × ≤4 turns, returns **files + line
  ranges** (not summaries). Scored by **F-β with β = 0.5 (precision-weighted)** because
  "irrelevant context pollutes" more than missing context hurts. Agents spent >60% of the
  first turn retrieving; ~10× faster at frontier-level retrieval.
  → *For us:* our metric for Jev nav should weight precision over recall (F0.5 over opened
  bindings vs gold). Validates "return pointers, let the planner read".
- **FastContext (Zhang et al., arXiv 2606.14066, June 2026)** — https://arxiv.org/abs/2606.14066.
  4B–30B explorer subagents returning paths + line ranges to Mini-SWE-Agent: up to **−60%
  main-agent tokens, +5.5% resolve**. **Withdrawn 2026-06-30 (IP)** — cite with caution.
  → Same shape as our pre-pick; gives a target effect size for a planner-token reduction.
- **SWE-Pruner (Wang et al., arXiv 2601.16746, Jan–May 2026)** — https://arxiv.org/abs/2601.16746.
  0.6B line-level "skimmer" guided by an **agent-written goal hint** ("focus on error
  handling"): **−23–54% tokens on SWE-Bench Verified while improving success**; 14.8×
  compression on LongCodeQA.
  → Closest analogue to `modify_view(intent)`. Evidence that a *planner-authored* intent
  string is the right conditioning signal; keep intent mandatory and short.
- **AgentDiet (arXiv 2509.23586, rev. Mar 2026)** — https://arxiv.org/abs/2509.23586.
  Removing useless/redundant/expired trajectory content: **−40–60% input tokens, −21–36%
  cost, same effectiveness**.
  → Collapse (not just expand) is where savings live; a Jev "still needed?" pass over
  already-open bindings is a cheap next experiment.
- **Minification (arXiv 2606.01326, May 2026)** — https://arxiv.org/abs/2606.01326.
  Lexical minification of code in context: **−42% input tokens but −12 pp resolve rate**.
  → Risk: lossy compression of *what the planner reads* hurts. Our fold-to-signature is
  lossless-on-demand (planner can re-open); keep it that way, don't strip names/types.
- **Cursor semantic search (Cursor blog; numbers via search snippets, page not opened [unverified])** — https://cursor.com/blog/semsearch.
  Semantic search +12.5% avg QA accuracy (6.5–23.5% by model); online A/B: **+0.3% code
  retention, +2.6% on repos ≥1,000 files**.
  → Retrieval gains are small on small repos. Our Hazel tasks are tiny; expect nav wins to
  show up as **tokens/turns**, not success — design the eval around that.
- **AI21 "explore cheap, patch frontier" (blog, 2026-07-15)** —
  https://www.ai21.com/blog/better-and-cheaper-together-open-models-explore-frontier-models-patch/.
  MiniMax-M3 explores → GPT-5.2 extracts context → Opus 4.8/Fable 5 patches: **80.8% SWE-Bench
  Pro at $5.99/task vs $18.28 solo Opus**; frontier = 25% of spend. Vendor blog, not peer-reviewed.
  → Strongest recent evidence that the cheap-context/expensive-edit split holds at scale.
- **Jev community tools for context (GitHub, Sept 2026)** — `fast-jev-compaction`
  https://github.com/tamaratran/fast-jev-compaction (two Nouls per tool call: keep call? keep
  result verbatim?; threshold 0.5), `Oko` https://github.com/bartlomein/oko (BM25 shortlist →
  Jev Noul rerank, MCP) **[listed in awesome-list, repo not opened]**. **No measured results
  published** by either.
  → Nobody has published Jev-for-code-context numbers yet; our eval would be first.

## 2. Typed / structured action spaces and type-constrained generation

- **Mündler et al., "Type-Constrained Code Generation with Language Models" (PLDI 2025)** —
  https://arxiv.org/abs/2504.09246 · DOI https://dl.acm.org/doi/10.1145/3729274 · code
  https://github.com/eth-sri/type-constrained-code-generation. Prefix automata + **search over
  inhabitable types**; >½ fewer compile errors, better functional correctness (HumanEval/MBPP,
  TypeScript). *Citation in `theory.md` is now verified.*
  → **Direct fix for eval-001 finding 1**: our menu builder needs their "reachable types"
  idea — offer `f(?, …)` for every in-scope `f` whose *result* type can reach the hole type
  (including via one more call), not only exact-type names.
- **TyFlow (Huang et al., arXiv 2510.10216, rev. Feb 2026)** — https://arxiv.org/abs/2510.10216.
  Generation as **synthesis decisions isomorphic to typing-derivation steps** instead of
  tokens; eliminates type errors and improves correctness.
  → Theoretical backing for "Jev picks typing rules at holes". Their decision vocabulary
  (rule + premise order) is a template for our candidate forms.
- **CodeStruct (Kim et al., ACL 2026, arXiv 2604.05407)** — https://arxiv.org/abs/2604.05407.
  `readCode`/`editCode` over named AST entities: **+1.2–5.0% Pass@1, −12–38% tokens** on
  SWE-Bench Verified across 6 LLMs; **GPT-5-nano +20.8%, empty-patch failures 46.6% → 7.2%**.
  → Structured actions help *weak* actors most — encouraging for a System-1 actor. Also a
  baseline to cite: they structure the *LLM's* actions; we delegate them to a classifier.
- **Projectional Decoding (Chen et al., FSE 2026 IVR, arXiv 2605.30054)** —
  https://arxiv.org/abs/2605.30054. Keeps a partial graph model alongside text during decoding
  for incremental semantic validation. No Hazel/holes mention; 5-page vision paper.
  → Related-work only; confirms "projectional" framing is live at FSE.
- **"Can LLMs Perform Synthesis?" (Egolf et al., arXiv 2603.20264, Mar 2026)** —
  https://arxiv.org/abs/2603.20264. Symbolic synthesizers solve more than Qwen-32B and match or
  beat GPT-5; Qwen + verifier loop until pass helps.
  → Supports search + verifier over one-shot picks. Also a warning: a pure enumerator may
  beat Jev on small typed holes — **add an enumerator-only (no Jev) baseline** to the build eval.
- Hazel side: nothing new since ChatLSP (OOPSLA 2024). Hazel's 2026 OOPSLA paper is on typed
  tables (via search snippet, **[unverified]**). No other structure-editor + small-policy work found.

## 3. Planner–executor splits

- **PEAR (arXiv 2510.07505, v3 Jan 2026)** — https://arxiv.org/abs/2510.07505.
  **A weak planner hurts clean performance more than a weak executor**; executor memory
  doesn't matter, planner memory does; attacks on the planner are most effective.
  → Supports Claude-plans/Jev-executes. Corollary: put eval effort into spec quality
  (spec-compression curve in `theory.md`), not a bigger executor.
- **"When does restricting a coding agent to execute_code help?" (arXiv 2607.10569, Jul 2026)** —
  https://arxiv.org/abs/2607.10569. Restricted tool surface was cheaper-or-equal in 3/4
  regime×agent cells, no success difference; SWE-bench × Claude was +14% cost (n.s.). "Cheapest
  tool surface is jointly determined by task regime and agent design"; **use cache-adjusted cost**.
  → Our Navigator mode (expand/collapse hidden) is a tool-surface restriction: expect
  planner-dependent results (Luna vs Claude) and report cache-adjusted cost, not raw tokens.
- Morph "planner + executor pairs" (https://www.morphllm.com/multi-agent-model-routing,
  "median 4× lower execution-side cost") — **[unverified: HTTP 429]**; treat as marketing.

## 4. TypeSafe / Jev updates and failure modes

- **Models page** — https://docs.typesafe.ai/models.md. Only `jev-1.13.0` (`jev-latest` =
  `jev-preview`). **64K tokens/request; 250K tok/s; 1,200 req/min**; English best; no fine-tuning.
  llms.txt (https://docs.typesafe.ai/llms.txt) lists no new primitives beyond Choice/Score/Noul.
- **Positional-index pitfall (pedramamini gist, Sept 2026)** —
  https://gist.github.com/pedramamini/014676fa8684d91bf7000f4623701ada. 320 items, one Noul
  each: indexing `items[i]` → **27% wrong at 150/request, 9% at 25/request; keyed object
  (`candidates.k137`) or item embedded in question → 0/320**. Limit: **state + longest
  question ≤ 32K tokens** (HTTP 400 at ~107.5K chars).
  → **Risk in our code:** `JevNav.state_of` sends `bindings` as a JSON *array*
  (`src/haz3lcore/CompositionCore/JevNav.re:302`). Questions name the path, which partly
  mitigates it, but switch to an object keyed by path. Also check `JevEdit` candidate lists.
  Our 8K batch cap (`AgentGlobals.re:83`) is safely under 32K.
- **Self-consistency cookbook** — https://docs.typesafe.ai/cookbooks/consistency_noul_cookbook.md.
  15 repeats: mean per-question SD 0.0102; **repeat sampling unnecessary**; recommends a
  **0.30–0.70 "uncertain" band**. ~111 ms, $0.000043/call.
  → Don't spend budget on repeats; the band we already use matches vendor advice.
- **Re-ranking cookbook** — https://docs.typesafe.ai/cookbooks/rerank_typesafe.md. BM25 → Noul
  per (query, passage), sort by P: top-1 5→18%, top-10 38→62% on CLERC; 1,200 calls $0.065.
  → Vendor itself sorts by Noul P, contradicting `jev-orderby-bench`. For beam/top-k in the
  build search, rank only among *already type-valid* candidates and treat ties as ties.
- **Speculative fan-out** — https://docs.typesafe.ai/patterns/fan-out.md. Ask speculative
  questions in one call; more questions ≈ no extra latency.
  → For build search: ask about every hole of every top-k candidate's child holes in one
  request (one round of lookahead for free).
- **Choice guidance** — https://docs.typesafe.ai/primitives/choice.md. ≤255 options; add
  `none of the above`; distinguish near-identical options in descriptions.
  → Candidate menus need a `none / escalate` option and descriptions that disambiguate
  `x` vs `f(x)` style near-duplicates.
- **Action-gate study (2026-09-19)** — https://github.com/ghubnab99/jev-enterprise-decision-fabric.
  111 cases: Jev 90.1% vs Opus 5 91.9% per-case; 6.5× faster, ~323× cheaper; **largest error
  source was the probability→action mapping (policy), not the model**; several outcomes within
  ±0.01 of thresholds.
  → Log raw P per decision and score threshold policy separately from Jev accuracy.
- **jevcal** — https://github.com/abhixhek/jevcal. Fits thresholds on a split; "under ~100
  labeled rows per question, expect it not to hold".
  → Our thresholds can't be honestly tuned on n=1 evals; need ≥100 labeled binding decisions.
- **Bicameral** — https://github.com/AbdelStark/bicameral. Pi harness: Jev gates tool calls,
  checks "honest finish" (weakened tests/stubs), **stuck detector**. No metrics.
  → A Jev stuck-detector would have caught eval-001's 14× identical retries.
- JS SDK changelog — https://docs.typesafe.ai/sdk/javascript/changelog.md: v0.6.0 (2026-09-15)
  breaking change, `Score.criteria` is now an ordered list. Irrelevant unless we use Score.

## 5. Evaluation methodology for navigation

- **ContextBench (arXiv 2602.05892, Feb 2026)** — https://arxiv.org/abs/2602.05892 ·
  https://github.com/EuniAI/ContextBench. 1,136 tasks, gold contexts; **context recall/
  precision/F1 at file, block, line level**, efficiency, and **explored-vs-utilized gap**.
  Sonnet 4.5: file R 0.72 / P 0.665; line R 0.374 / P 0.344. LLMs favor recall over
  precision; utilization drops >40% for some; complex scaffolds ≈ simple baseline.
  → Adopt their three metrics at *binding* granularity: opened∩gold (recall), gold∩opened/
  opened (precision), and **utilized** = opened bindings the planner later read/edited.
- **MULocBench (arXiv 2509.25242)** — https://arxiv.org/abs/2509.25242. Acc@1/@5 + P/R/F1 at
  file/class/function. → Acc@k is a fit for ranking-style nav if we ever rank.
- **Forge** (in `research.md`) used navigation steps; SWE-grep uses F0.5; the execute_code
  ablation uses cache-adjusted cost. Combine: **steps, F0.5, utilization, cache-adjusted $**.
- **Harness-1 (arXiv 2606.02373, Jun 2026)** — https://arxiv.org/abs/2606.02373. Moving
  bookkeeping (candidate pools, verification records) out of the policy into the harness:
  20B search agent 0.730 curated recall, +11.4 pp over next open subagent.
  → Supports Hazel-as-state-holder; the policy (Jev) should only see the decision, never history.

## Ranked top-5 actions

1. **Fix editor menus with inhabitation search** (Mündler PLDI'25, TyFlow): offer every in-scope
   function whose result type reaches the hole type as `f(?, …)`, plus `none/escalate`. This is
   the eval-001 blocker; re-run fix-middle after.
2. **Key Jev state by path, not array index** (`JevNav.state_of`, and `JevEdit` candidates) —
   27% error measured on positional arrays. Cheap, pure-function change + unit test.
3. **Nav metrics per ContextBench/SWE-grep**: log gold bindings per task; report recall,
   precision, **F0.5**, utilization, turns, and cache-adjusted cost; stop judging nav by goal-met
   (small repos → gains show in tokens/turns, per Cursor).
4. **Add baselines to the build eval**: enumerator-only (no Jev) and planner-writes-code, plus
   greedy vs beam with fan-out lookahead. Without the enumerator baseline, reviewers will ask
   whether Jev adds anything over type-directed enumeration.
5. **Collapse pass + stuck detector**: one Jev Noul per open binding ("still needed for intent?")
   each turn (AgentDiet/fast-jev-compaction), and a Noul over recent tool calls for "repeating
   without progress" (Bicameral) to replace the hand-coded "2 identical escalations" nudge.

## Dead ends

- No published Jev-for-code-context or Jev-for-synthesis numbers exist (awesome-lists checked:
  https://github.com/AbdelStark/awesome-typesafe-jev). Community tools report no metrics.
- No new Jev model/primitives since 1.13; no documented question-count limit (only the 32K state rule).
- No 2025–26 structure-editor + small-policy paper found; closest are CodeStruct and TyFlow.
- Morph planner/executor benchmark page (429, unverified); PEAR is a security benchmark, only
  its clean-performance finding is relevant.
- FastContext numbers are from a withdrawn paper.
