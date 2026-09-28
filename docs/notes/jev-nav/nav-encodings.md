# Navigation encodings — how Jev should select the view

Decision record for discussion #11. Question: given a binding tree, how do we ask
Jev which bindings should be open for an intent? Three encodings, the evidence, the
choice, and what the evidence changed. 2026-09-22.

## The frame

A **view** is a set of open bindings closed under ancestors: `foo/bar/baz` can only
be read if `foo` and `foo/bar` are open. Such sets are closed under **union**, so any
number of parallel selections merge without conflict. Siblings of an open binding
render folded (`⋱`: name visible, code hidden) — already `CompositionView`'s behavior.
Navigation = **selection over a finite structural set, merged by union**. That is the
"action-selector calculus" framing: a small algebra over structure, like Filbert's
edit tools.

Running example:

```
foo/bar/baz   foo/bar/dag   foo/bar/hug   goo/lan/mop   goo/noo
```

## The three encodings

### E1 — Frontier walk (top-down, level by level)
Noul per node at the current level; descend only into what opened; repeat.
`foo?, goo?` → `foo/bar?` → `baz?, dag?, hug?`. Rounds = depth reached.

### E2 — Per-arm depth
One question per root→leaf arm: "how deep along `foo/bar/baz` should the view go?"
(Choice or Score over `none | foo | foo/bar | foo/bar/baz`). One round. Arms sharing a
prefix each re-judge it.

### E3 — One-shot selection + ancestor closure (flat)
One Noul per node, **all nodes in one request** (split at ~32, sent in parallel), each
node shown with its full path. Open = yes-set ∪ ancestors(yes-set). One round.

## Evidence

| Source | Finding | Bears on |
|---|---|---|
| RAPTOR, ICLR 2024 (Sarthi et al.) — https://arxiv.org/abs/2401.18059 | Over a tree of nodes, **"collapsed tree"** retrieval (flatten all levels, score every node) was "notably superior" to **tree traversal** (top-k per layer, descend). Stated reason: questions need information at mixed granularities; layer-constrained search can't pick them together. | E3 over E1 |
| Flat vs hierarchical classification (Babbar et al., NeurIPS 2013) — https://proceedings.neurips.cc/paper_files/paper/2013/file/cbb6a3b884f4f88b3a8e3d44c636cbd8-Paper.pdf; HierFlat — https://link.springer.com/article/10.1007/s41060-017-0070-1 | Top-down decisions suffer **error propagation**: a wrong "no" high up cannot be corrected below. Neither dominates universally; best is **hybrid**: keep hierarchy only where it helps, **flatten** where it does not (large fan-out, sparse data favor hierarchy). | E3 for small trees; E1 only as a scale fallback |
| Agentless (Xia et al., FSE 2025) — https://arxiv.org/abs/2407.01489 | Hierarchical file → class/function → line localization works and is cheap ($0.34/issue, 32% on SWE-bench Lite at the time). | E1 is viable at repo scale, where a flat pass is too big |
| GraphLocator (arXiv 2512.22469) — https://arxiv.org/html/2512.22469v1 | Fixed hierarchical traversal "typically yield[s] low recall" on **symptom-to-cause** and **one-to-many** issues; independent entity judgment "prioritizes superficial relevance instead of underlying causality." Graph-guided causal search: +16.4% recall (cause ≥2 hops from symptom), +19.2% recall (one-to-many). | Weakness of **both** E1 and E3: pointwise judgments miss dependencies |
| Pointwise vs listwise reranking — https://zeroentropy.dev/articles/should-you-use-llms-for-reranking-a-deep-dive-into-pointwise-listwise-and-cross-encoders/ ; RankSteer https://arxiv.org/html/2602.03422 | Pointwise (judge each item alone) is parallel and scales; its known flaw for **LLMs** is **calibration** — scores not on a fixed, comparable scale. Listwise is more consistent but only viable for small lists. | E3 is pointwise; Jev is *built* to return calibrated Noul probabilities, which removes pointwise's main flaw |
| TypeSafe `semantic_find` / `jgrep` / `parallel_questions` (see `research.md`) | Jev's intended use: many items in one state, one Noul each, thresholded. 12.2× cheaper batched. | E3 matches Jev's strong zone exactly |
| TypeSafe `hierarchical_classification` | Beam top-down descent with Choice per node, for **single-label** leaf classification. | E1's best form — but our task is a **multi-label subset**, not one leaf |
| Jev jaggedness page + `jev-orderby-bench` | Ordinal/ranking judgments and multi-hop indirection are weak; tied probabilities break ranking. | Against E2 (depth is ordinal; per-arm re-judging of shared prefixes) |

## Decision

**E3: one-shot selection + ancestor closure**, with two amendments the evidence forced.

Why E3:
1. **Error propagation (Babbar, HierFlat):** E1 can't recover from a wrong "no" on
   `foo` — `baz` is never asked. E3 asks `foo/bar/baz` directly; a relevant leaf pulls
   its ancestors open even if `foo` alone looked irrelevant.
2. **Mixed granularity (RAPTOR):** an intent may need a whole top-level binding *and*
   one nested member elsewhere. Flat scoring picks both in one pass; collapsed-tree
   beat traversal for this reason.
3. **Jev's strong zone:** pointwise, batched, calibrated Noul — exactly
   `semantic_find`/`jgrep`. And pointwise's classic LLM flaw (calibration) is the thing
   Jev is designed to fix.
4. **One round trip, no loop, nothing to undo.** Latency = one Jev call regardless of depth.
5. **E2 rejected:** "how deep" is ordinal (ranking weakness); shared prefixes are
   judged once per arm and can disagree; no benefit over E3's closure.

### Amendment 1 — dependency context (from GraphLocator)
Pointwise judgment misses causality: `baz` may matter only because `hug` (relevant)
calls it. Fix without asking Jev to reason multi-hop: put the **structure in the
state**. Each node carries `refs` (bindings it uses) and `used_by`, computed
deterministically from statics (`GeneralTreeUtils.get_refs_to` already exists — reuse).
Then optionally a deterministic **dependency closure**: show direct dependencies of
selected bindings **folded** (names visible) so the main model can open them. Jev
stays single-hop; code does the hop.

### Amendment 2 — scale fallback (from Babbar/Agentless)
Hazel programs in scope are tens of bindings, so flat fits in 1–3 batched requests. If
a program exceeds a cap (e.g. 256 nodes), fall back to **E1 at the top level only**
(prune top-level bindings, then E3 inside the survivors). Hybrid, as the flat-vs-
hierarchical literature recommends. Not built until a task needs it.

## What the agent sees

Ancestor closure is required: a folded parent hides an open child. Selecting
`foo/bar/baz` opens `foo`, `foo/bar`; their other nested bindings stay `⋱`. This is the
current renderer's behavior for an ancestor-closed set — no new code. Spine-only
rendering (ancestor headers only) deferred; see discussion #13.

## What each node looks like in the state

**Jev sees real code, not the agent's folded view.** Each node carries its **own full
definition**, with only its *nested* bindings folded (`⋱` + their names). So every
line of the program appears exactly once across the nodes — no duplication — and Jev
judges from logic, not just names. (Russ, #12.)

```json
{"path": "foo/bar", "name": "bar", "type": "Int -> Int",
 "code": "fun n ->\n  let baz = ⋱ in\n  let dag = ⋱ in\n  baz(n) + dag(n)",
 "refs": ["goo/noo"], "used_by": ["goo/lan/mop"]}
```

- **Rendering:** reuse `CompositionView`'s fold printer on the node's definition with its
  child binding ids folded. No new printer.
- **Cost:** total state ≈ program size once. A 15-binding program ≈ 1–2k tokens ≈
  $0.0001 per selection. Comments stripped (adversarial-state rule).
- **Batching:** a parameter. Default: all nodes in one request when under ~8k tokens,
  else split and run in parallel. Cost is the same either way (nodes are disjoint);
  accuracy vs batch size is unknown — swept in the eval {1, 4, 16, all}. See #14.
- **Limit:** 32K context per request; a single huge binding is truncated to its head.

Question per node: *"Is binding `foo/bar` relevant to the intent?"* with positive
criteria on both sides. Thresholds 0.7 / 0.35 as in `plan.md` §3.

## Risks and how the eval checks them

- **Load-bearing names.** Reduced now that Jev sees full code, not just names. Keep the
  cheap check: one task duplicated with obfuscated names; compare selection recall.
- **Over-selection via closure.** A deep relevant leaf opens its whole ancestor
  chain; ancestors' other members stay folded, so cost is bounded. Log opened-count.
- **Irrelevant-state distraction** (jaggedness page): state = all nodes. Mitigate by
  one-line heads, comments stripped; alarm on >2k input tokens per request.
- **Pointwise misses one-to-many edits.** Amendment 1; measure with a task whose fix
  spans a caller and a callee in different subtrees.

## Metrics specific to this choice

`selection_recall` / `selection_precision` against each task's `nav_targets`;
`closure_added` (ancestors opened by closure only); `dep_added` (folded deps shown);
Jev requests (expected 1–3) and input tokens.

## Status

Recommended; replaces the frontier walk (`plan.md` §4) once Russ accepts. E1 kept as
an eval arm if we want to test the literature's claim on our own tasks.
