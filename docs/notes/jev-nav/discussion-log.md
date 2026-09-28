# Discussion log — Jev as a navigation subagent

Chronological record of the brainstorm between Russ and Claude, 2026-09-22. Each entry:
the question raised, the options on the table, the pushback, the decision. Newer
entries at the bottom. Decisions are collected first so the log can be skimmed.

## Decisions so far

1. Jev is the **ranking function**; a deterministic walker over `HighLevelNodeMap` is
   the subagent. Jev never edits, never picks tools (for now), never decides "done".
2. One pure function `curate_view(intent) → open_paths` with **two callers**: the tool
   `modify_view(intent)` and an automatic **pre-pass** on each new user message.
3. Action language = **one Noul per binding**: "should `<path>` be open?" Not Choice,
   not expand/collapse as separate questions. **Threshold** (0.7 / 0.35), never rank.
4. Iteration = **monotone frontier walk**: ask top-level, descend only into what opened,
   never re-ask, never ask to close. Ends when the frontier is empty (depth cap 3).
5. **Reset on new user message**, reset-with-knowledge on `modify_view` (Jev sees the
   current `open` set). `expand`/`collapse` stay as the escape hatch.
6. Everything flag-gated, default off ⇒ byte-identical to `dev`. 2×2 arms.
7. Metrics: Jev requests / questions / tokens / cost / latency per run; main-model
   navigation turns and cost; `goal_met`. Baseline recorded **before** Jev code lands.
8. Eval harness: Matt Keenan's `hazel agent` runner + five tasks, extracted file-by-file
   (no merge commit); port Russ's usage/cost JSONL from `AgentEval.re` into it.

---

## #1 — What is Jev, and can it be "the subagent"?

**Raised:** Russ: integrate Jev as a navigation subagent instead of letting the main
coding agent handle tool calls; it should expand/collapse structure fast and cheaply.

**Finding:** Jev (TypeSafe AI, public 2026-09-15) is a System One model: no text, no
code, no tool calls; `state` + typed questions → Choice / Score / Noul with
probabilities. ~100 ms, $0.042/M input, output free. Served on OpenRouter as
`typesafe/jev-latest`, so the key already in `AgentGlobals` works.

**Pushback (Claude):** Jev cannot *be* a subagent — it cannot walk a tree or stop.
The subagent is our walker; Jev is its ranking function.

**Decision:** accepted (D1).

## #2 — Three shapes, and the closed-set unlock

**Raised:** Russ: "we could have it score tools/nav actions or even just give each as a
binary yes or no whether it should take that route."

**Options:** V1 pre-pass (Noul per binding → `expanded_paths`); V2 route gate (Nouls
"needs nav? needs edit? converse only?"); V3 nav executor (Choice over the 18 tools
whose arguments are all closed sets — paths from `HighLevelNodeMap`, projector kinds,
task titles — so Jev can emit *complete* tool calls; code-taking tools excluded).

**Pushback:** Jev never decides when to stop; stakes rule (0.6 floor reversible,
0.85+ risky, edits never); state size is the cost; Jev may be worse than the model at
typed relevance — only a benchmark settles it.

**Decision:** V1 first, V3 later. (Superseded in #5 by the two-caller design, which
contains V1.)

## #3 — Manual, parallel batch, iterate until done?

**Raised:** Russ: give a common manual, then in a parallel batch ask per binding
expand/collapse, then iterate until Jev says nothing more to do?

**Answer:** (1) The "manual" is the question `instructions` + `criteria`; Jev has no
system prompt; keep it a paragraph — it is resent and billed every call. (2) Yes, one
request with N Nouls, not N requests. (3) **No** to "until Jev says done": it has no
done concept and a global "is the view sufficient?" is its weak zone. Let structure end
the loop: ask about children of what just opened; stop when nothing new has children.

**Decision:** D4.

## #4 — Why is "is the view sufficient?" weak?

**Raised:** Russ: "really? hmph… interesting, i wonder why."

**Answer:** "Is *this* binding relevant?" is a local comparison — classification. "Is
the *whole* view enough?" reasons about what is *absent* and what the task will need
next — global, counterfactual, multi-hop. System One models pattern-match; no reasoning
chain. Caveat stated: inferred from the design at the time. Later confirmed by
TypeSafe's jaggedness page (indirection, large irrelevant state) — see `research.md`.

## #5 — Jev as a tool the main agent can call

**Raised:** Russ: main agent gets a tool `modify view(intent: string)`; parallel Jev
requests decide expand/collapse to focus the view for that intent. Also floated a
planning agent → builder agents; withdrawn as too complex.

**Answer:** Yes, and better than the pre-pass alone because it fixes the division of
labor: the model judges *what it needs*, Jev does local relevance. Key move: it is
**the same function** as the pre-pass, two callers → 2×2 experiment for free. Details
fixed: Jev input `{intent, bindings[], currently_open[]}`, output replaces the set;
tool result tiny; one batched request; tool description demands concrete intents;
keep `expand`/`collapse`.

**Decision:** D2, D5.

## #6 — What does Jev see? Noul or Choice? How to recurse?

**Raised:** Russ: what does Jev see, prompt or not, tool calls for Jev, number of
parallel requests as a metric, cost tracking, is the action language something Jev is
*good* at, Noul per possible action?, and how to iterate when opening `a/b/c` reveals
`a/b/c/d` and `a/b/c/e`.

**Answer:** No prompt; JSON state + one Noul per binding keyed by path. No tool calls
for Jev. Noul not Choice because it is a set decision (TypeSafe's own rule: sets → one
yes/no per candidate); one question per *binding* ("open?"), not per action, to avoid
contradictory-instruction failures. Recursion = frontier BFS, monotone. Request count
= depth reached × ⌈N/32⌉, typically 1–2; logged. Good-at check: local, one-item,
literal, named-state relevance is their strong zone (semantic_find, jgrep); avoid
counting, comparing, negation, global judgments.

**Decision:** D3, D4, D7. Written as `plan.md` §2–§6.

## #7 — No undo inside a walk; hand-off granularity

**Raised:** Russ: Jev should not be able to go backwards — if it opens `foo`
revealing `bar`, `baz`, ask about those but never whether to close `foo`. Might be
tough for the nav logic? Perhaps surface back to the main agent for another nav
request. How granular or how high-level should nav requests be?

**Answer:** That is exactly the monotone walk, and it is the *easy* case (terminates
by construction; the version that can go back is the one that oscillates). Collapse
happens only at intent boundaries (new user message, new `modify_view`), where Jev is
shown the current `open` set. Three granularities: path (`expand`, today), intent
(`modify_view`), task (pre-pass). Walker runs to exhaustion, returns one tool result;
"undo" = a new intent from the model that can judge sufficiency. Nav requests should
be high-level; path-level `expand` after a `modify_view` is logged as a signal.

**Decision:** `plan.md` §10.

## #8 — Status check: no code yet

**Raised:** Russ: OpenRouter key to come later; not needed yet. **No go** on code —
still brainstorming and planning.

**State:** harness extracted and verified offline (`hazel agent --stub`); docs folder
in place; nothing committed. Key is needed only to record the control baseline.

**Decision:** planning continues. Code starts only on an explicit go.

## #9 — Eval suite fit; parallel workstreams

**Raised:** Russ: do we have the right eval suite? Then: split the plan across parallel
agents (evals, code/logic, Jev-facing prompts/tools); clean, modular, OOP + DRY +
functional; ponytail.

**Answer:** Runner right (Matt's `hazel agent` = real Filbert loop); tasks wrong
(10–15 bindings, built for incremental-eval, navigation barely matters); metrics
missing (nav tool calls dropped from traces, no cost JSONL, no arm flags).
Workstreams: Stage 0 contracts → A (Jev subagent), B (integration incl. prompts/tools),
C (evals, can start now). Pushback: separate prompts agent would collide with B on the
same files; "OOP" in ReasonML = modules + `.rei` signatures, not classes.

**Decision:** `workstreams.md`.

## #10 — Should the view reset at all?

**Raised:** Russ: "only the big model decides the view is wrong… a new user message
resets the view" — this gets yucky. Should we reset?

**Problem named:** a reset closes things the model deliberately opened, so the view
shifts under it and it re-expands (wasted turns, and a confound in the metrics).

**Proposal (Claude): ownership, not reset.** Split `expanded_paths` into two sets:
- `pinned` — opened by the model (`expand`). Only the model closes them (`collapse`).
- `suggested` — opened by Jev (pre-pass / `modify_view`). Replaced wholesale on each
  new intent; never touches `pinned`.
View = `pinned ∪ suggested`. Rule: *whoever opened it owns closing it.* No reset of
the model's choices ever; Jev's own suggestions refresh freely, which is what makes
the pre-pass on every user message safe. Cost: `AgentContext.Model` gains one list;
render unions them. Monotone walk unchanged.

**Status:** proposed, not decided. Supersedes D5 if accepted.

## #11 — Navigation as selection, not expand/collapse

**Raised:** Russ: instead of expand/collapse steps, make navigation a *selection*
problem. Example paths `foo/bar/baz`, `foo/bar/dag`, `foo/bar/hug`, `goo/lan/mop`,
`goo/noo`. Per arm (root→leaf), let Jev pick how deep to show. Or ask per path "is
this relevant at all?" Concerns: two arms both pick `bar` — what then? Does including
`baz` expand its siblings? Names become load-bearing. Feels like an action-selector
calculus.

**Formalization (Claude):** a *view* is a set of open bindings **closed under
ancestors** (you cannot see `baz`'s code unless `foo` and `bar` are open). Such sets
are closed under union, so parallel selections merge by union with no conflict:
two arms choosing `bar` → `bar` is open once. Siblings: opening `baz` opens its
ancestors; `bar`'s body then shows `dag`, `hug` **folded** (`⋱`, names visible, code
hidden) — that is already how `CompositionView` renders. Siblings are visible, not
expanded.

**Three encodings compared:**

| | Question | Rounds | Risk |
|---|---|---|---|
| Frontier walk (plan §4) | Noul per node, level by level | depth (1–3) | extra round trips |
| Per-arm depth | Choice/Score per leaf-arm over its prefixes | 1 | shared prefixes judged repeatedly, maybe inconsistently; ordinal ranking is a known Jev weak spot |
| **One-shot selection** | Noul per node, **all nodes at once**, each with its full path + head | **1** | more questions per call; still ≤32/req, batched in parallel |

**Recommendation:** one-shot selection + **ancestor closure**. Every binding asked once
("is `foo/bar/baz` relevant to the intent?"), then open = yes-set ∪ all ancestors of
the yes-set. One round trip, no iteration, no oscillation, no per-arm inconsistency.
Supersedes the frontier walk if accepted. Keeps the "no undo" property trivially:
there is no sequence to undo.

**Load-bearing names:** yes — Jev judges from `path`, `name`, `type`, `head`. Poorly
named bindings will under-select. Mitigate with `head`; measure by adding one
badly-named task variant to the eval set (same program, names obfuscated).

**Framing:** navigation becomes selection over a finite structural set with a join
(union) — fits Filbert's "small calculus over structure" identity.

**Status:** proposed. Researched in `nav-encodings.md` → E3 recommended, with dependency context and a scale fallback.

## #12 — Show Jev the real code

**Raised:** Russ: in the one-shot case, why only name + first line? Show each node's
full internal logic, child definitions collapsed — but not the agent's folded view.

**Answer:** Agreed. Each node = its own full definition, nested bindings folded. Every
line appears once across nodes, so total state ≈ program size (≈1–2k tokens, ~$0.0001).
Batch ~16 nodes/request (jgrep does full code chunks this way); smaller per-request
state also limits Jev's documented distraction by irrelevant state. Names matter less;
keep the obfuscated-names task as a cheap check.

**Decision:** `nav-encodings.md` node state updated.

## #13 — What the agent sees when a deep node is selected

**Raised:** Russ: level 1 selected, level 2 folded, but a level-3 node selected — what
does the view look like? Implicitly include levels 1 and 2 with nested defs folded?

**Answer:** Yes — that is the ancestor closure, and it is *required*, not optional:
today `CompositionView.zipper_for_agent_context` folds every binding not in
`expanded_paths`, so a folded parent hides an open child entirely. Selecting
`foo/bar/baz` therefore opens `foo` and `foo/bar`; every *other* binding inside them
stays `⋱`:

```
let foo =
  let bar =
    let baz = <full code of baz> in
    let dag = ⋱ in
    let hug = ⋱ in
    <bar's own body>
  in
  let other = ⋱ in
  <foo's own body>
in
let goo = ⋱ in
…
```

Options considered:
- **A — full ancestors (chosen):** ancestors show their own code; unselected nested
  bindings folded. Zero new rendering code — it is exactly what the current renderer
  does for an ancestor-closed `expanded_paths`.
- **B — spine only:** ancestors reduced to their binding headers, their own body
  logic elided. Saves tokens when ancestors are large, but Hazel's fold projector folds
  whole definitions; eliding part of a body needs a new render mode.

**Decision:** A. Revisit B only if logs show ancestor bodies dominate snapshot tokens
(metric: `closure_added` tokens).

## #14 — Why batch 16? Why not everything in one request?

**Raised:** Russ: why only 16 nodes per request — wouldn't all nodes in one request be
fastest?

**Answer:** 16 is jgrep's default, not a finding. The real trade-offs:
- **Cost: identical either way.** TypeSafe's 12.2× batching win is for a *shared*
  document asked many questions. Our nodes are *disjoint* code: one request with all N
  nodes and k requests with N/k nodes each send the same total tokens.
- **Speed: roughly equal.** Split requests run in parallel, so wall-clock ≈ the slowest
  one; a single request carries more tokens. Which is faster is an empirical question.
- **Accuracy: unknown direction.** Bigger state = more of Jev's documented
  "irrelevant state" distraction; but also more cross-node context (seeing the caller
  while judging the callee). No published accuracy-vs-batch-size data (jgrep reports none).
- **Hard limits:** 32K context per request; rate limits (429) if we fan out too wide.

**Decision:** batch size is a **parameter**, default = all nodes in one request if it
fits under ~8k tokens (simplest, one call), split otherwise. Sweep {1, 4, 16, all} in
the eval — cheap, and a publishable result either way.

## #15 — The agent is blind to expand/collapse

**Raised:** Russ: "jev and expand? i thought the idea was to get rid of expand… the
whole idea is that the agent is blind to expand and collapse. it simply requests a
program view and we give."

**Decision:** in the `modify_view` arm, `expand`/`collapse` are removed from the tool
list and refused if called anyway (the 40KB prompt still mentions them; it is the
cached prefix, so it is not edited per arm). `modify_view` is the only way to change
the view. Supersedes D5's "escape hatch". The pre-pass-only arm keeps `expand`/
`collapse`, since it has no other way to navigate. Tool result is now `open: a, b`.
Measured cost of blindness: count `modify_view` retries and refused `expand` calls.

## #16 — Jev as the implementor; LLM only plans

**Raised:** Russ: the orchestrator is purely a planner; the only code implementor is
Jev, choosing atomic structure-editor actions (navigate, create definitions, fill
bodies). "This is where the gold is." Names pitfall: A planner vocab, B namer tool,
C deterministic labels?

**Answer:** Researched in `v3-jev-implementor.md`. TypeSafe explicitly says chaining
choices to generate text "will not work well and will be very slow" — *unless* the
answer space is bounded. Hazel's typed holes bound it: at each hole the type system
lists the well-typed fillers, so writing becomes a Choice per hole (type-directed
synthesis with Jev as scorer). Risks: compounding error (~13% at 95% × 40 choices),
semantics need planner reasoning, savings are turns/latency not output tokens. Names:
**A** (planner vocabulary, also for literals). Staging: V3a selection edits (operator
swaps, argument choice) → V3b hole filling → V3c whole definitions.

**Status:** proposed; after the navigation study.

## #17 — 40 calls = 10 s? And "typed by construction"

**Raised:** Russ: would it take 40 calls (~10 s at 0.25 s each)? Strong LLM stays the
brain; Jev is the edit tool; restricting edits to typed actions enforces type
constraints in generated code.

**Answer:** No — rounds scale with tree depth, not nodes: batch same-depth holes in one
request; candidates are whole well-typed subexpressions, not tokens; the planner
sketches the shape and Jev fills holes. ~2–4 rounds ≈ 0.5–1 s for a small function.
Agreed on the division of labour. Pushback: typed-by-construction guarantees
well-typed, parse-valid edits — not correct ones (`volume + cost` type-checks and is
the bug). Tests/probes remain the correctness check.

**Status:** folded into `v3-jev-implementor.md`.

## #18 — Select first, then edit on the curated view

**Raised:** Russ: keep the parallel Jev requests — every path with nested levels
folded, each asked "is this necessary for the edit?"; then give Jev the finalized view
and let it perform the edit actions.

**Answer:** Yes — phase 1 is the built `select_view`, reused as-is. Additions: ask two
Nouls per node (context-needed vs edit-target) instead of one mixed "relevant";
phase 2 uses the curated view as one shared state with one Choice per hole per round
(the case batching makes 12× cheaper), and the smaller view limits distraction. Planner
still owns *what*; Jev owns *where* and *which well-typed piece*.

**Status:** folded into `v3-jev-implementor.md` §Proposed shape.

## #19 — Jev builds the shape, not just the leaves

**Raised:** Russ: why does Claude write a sketch with holes? The aim was Modes 3/5 — Claude
plans, Jev implements. Can't Jev make structure-editor actions to create code shapes? "That is
what holes are for."

**Answer:** Yes. Filling a hole with a structural form (`if ? then ? else ?`, operators,
`fun x -> ?`, `let … in`) *is* a structure-editor action, and its new holes are chosen next
round. The round-2 engine filtered forms out and relied on a planner sketch (#17) to keep
rounds low — a weaker version of the idea. Trade-off: ~5–8 rounds for fib vs 1–2, and Jev
must also choose the algorithm's shape (its documented weak zone) — measure, don't assume.

**Decision:** Round 3 — forms as candidates, pattern holes filled from `names`, sketch-free
"Jev builds" switch (`jev_edit_builds`, `/jev-builds`; jev_edit loses its `sketch` param),
higher round cap + total-holes cap, eval arm `edit+build`. Sketch mode stays as a comparison arm.

## #20 — Only System-1 decisions go to Jev; the spec makes them System 1

**Raised:** Russ: today's sketch mode is silly (Claude could type what Jev fills). Jev is
System 1 and poor at System 2, so maybe Jev-builds is unnecessary. Use Jev only where it
is truly good; the spec / implementation plan is what makes decisions System 1.

**Answer (Claude):** Agreed; this is the design principle. Claude does the System-2 work
(what to change and why) and writes a spec so precise that every remaining decision is a
local, obvious pick. Jev makes only those picks. Consequences:
- Sketch mode: no value as a goal (Jev re-types what Claude knew). Baseline only.
- Jev-builds a whole function: needs Jev to choose algorithm shape = System 2. Drop as
  a goal.
- **Where System 1 + speed pays off: fan-out.** One spec, many sites, each a local obvious
  decision, all in parallel: navigation relevance (Mode 3); applying one change across N
  call sites (add an argument, rename, propagate a signature change, repeat the same
  fix pattern, update tests). Claude would spend N tool calls / turns; Jev does N
  picks in one parallel round.
- Also System 1: "does this edit match the spec?" verification (TypeSafe cascade pattern).

**Status:** proposed pivot for the edit work: from "Jev writes a function" to "Jev applies a
spec across many sites". Round 3 (build mode) decision pending with Russ.

## #21 — The theory: sound-by-construction code with a System-1 policy

**Raised:** Russ: remove free-form text from code generation → guaranteed well-typed,
syntactically correct code. Could Jev, placed in a structure editor with a cursor and a
finite command set, build from mini specs ("nth fibonacci, tail recursive")? Is that System 1?

**Answer:** Sound-by-construction is real but established (Hazelnut, type-constrained
decoding); the novelty is a cheap calibrated System-1 classifier as the policy. Single picks
given a precise plan are System 1; choosing the plan (accumulators) needs lookahead =
System 2. Proposed novel combination: Jev as policy + search (beam/backtrack) + tests as
verifier ≈ System 2, affordable because Jev is cheap (AlphaGo pattern). Holes-in-parallel is
the batched cursor for construction; a cursor is still needed for transforming existing
code. Written up in `theory.md`.

## #22 — Planner is the System-2 step; plain-English search explanation

**Raised:** Russ: the novel part *is* giving a System-1 coding transformer a structure-editor
language inside a structure editor. The planner should be the System-2 pre-implementation step
(e.g. deciding "two accumulators"). Asked for #4 (System 1 + search) in plain English.

**Answer:** Agreed; recorded as the core architecture (planner = System 2 up front, Jev = System
1 implementor, Hazel = environment). Search explained as instinct + trying moves ahead (AlphaGo);
spec handles big decisions, search handles leftover small ambiguity; better spec → less search.

**Status:** `theory.md` updated (§ core architecture, § plain-English version).

## #23 — Live run: nsum replaced by itself; no re-typing; headless eval

**Observed (UI, /jev-mode sketch):** Claude's third `jev_edit` targeted the existing `nsum` to
"add tests"; on an existing path jev_edit replaces the definition, so nsum's RHS became a whole
`let nsum … in tests; nsum(10)` block. Valid syntax, wrong program (outer nsum always 55). Causes:
the edit arm had no way to add tests; the description didn't say "replaces" loudly; sketches
likely had zero holes (Jev re-typing Claude).

**Russ:** never let Jev re-type what Claude knew. Build a headless integration eval with GPT Luna
+ Jev. Key must never be published.

**Decision (round 4):** zero-hole sketches refused (A4); `/jev-mode` = spec-only build mode; new
`add_tests` tool (tests are spec, so the planner writes them); jev_edit descriptions say existing
path = replace RHS only (B4). `bench/jev-eval.sh` on `openai/gpt-6-luna` with spend caps, loud
failures, and key hygiene (C4). Key stored at `~/.config/hazel-jev/openrouter.env` (outside the
Ford-synced OneDrive repo, chmod 600); `.env` is not gitignored here, so no key file in the repo.

## #24 — Why Mode 3 was slower on the big program; refine and retry

**Raised:** Russ: why was the helper slower on the big program? Refine the prompts and retry.

**Answer:** Jev's own time was ~6 s total; the slowdown was more planner rounds (40 vs 34, each
modify_view replaced the view so the planner re-asked) and slower rounds (13 s vs 10 s, more code on
screen re-read every round). Also the task named the targets, so control never had to search.

**Decision (round 5):** strict Jev question ("must read or change"), threshold 0.8, keyed-by-path
state, mentioned paths opened without Jev (A5); modify_view additive by default + "ask for
everything at once" description (B5); symptom-style task variants that never name targets (C5b).
Then rerun the same 3 runs (~$0.07).

## Open questions

- Threshold tuning: 0.7/0.35 from `semantic_find` are a starting point; sweep on the
  five tasks once the baseline exists.
- Should the pre-pass run on *every* user message, or only the first in a chat?
  Current answer: every one (reset on new user message), measure.
- V3 (Jev picks and executes closed-set tool calls): after the 2×2 results.
- Feedback-loop asymmetry from `docs/agent-harness-eval/plan.md` §14 is shared by all
  arms, so it does not confound this study.

## #25 — No arbitrary cutoffs; one change at a time (2026-09-24)
- Russ: the 0.8 "yes" cutoff was never validated, so it's arbitrary. Take what Jev picks; if it over-selects, tighten the question.
- Done: removed `thresholds`/band from JevNav. `jev_says_yes` = Jev's own answer (p_yes > 0.5). Suite green (4066).
- Lesson: Eval 003 bundled 4 changes (question, cutoff, keyed state, mentioned paths), so no single one can be credited or blamed. From now on: one change per eval, each with a stated hypothesis.
- Cheaper method: score Jev's view picks directly against the known targets (no planner, ~$0.001 per question set) before paying for full agent runs.
- Same smell elsewhere: JevEdit `min_confidence=0.5` and the round/hole caps are also unvalidated knobs.
