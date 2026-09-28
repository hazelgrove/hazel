# V3 — Jev as the implementor, LLM as planner only

Status: **implemented** (rounds 2–3, 2026-09-23/24): sketch mode, create mode, Jev-builds mode
(typed forms, pattern holes, round/holes caps), fill type-guard. Framing refined in #20–#22 and
`theory.md`: sketch mode is a baseline; value is in fan-out edits and spec + search.

## The idea (Russ)

The orchestrating LLM only plans. Jev does all edits, by choosing among atomic
structure-editor actions: navigate, create definitions, fill bodies. Claimed win:
extremely fast, atomic edits. Known pitfall: free-form names.

## The headline constraint — and why Hazel may be the exception

TypeSafe, on generation (https://docs.typesafe.ai/model-jaggedness/jev-1.13.md):
> "jev-1.13 is not trained to generate text. While you can force it to by chaining
> choices, this will not work well and will be very slow." … "when the answer space is
> bounded, turn extraction into a Choice over the options."

So "Jev writes code token by token" is the documented anti-pattern. **But a typed
structure editor turns writing into bounded selection.** At a typed hole, Hazel knows
the expected type and the typing context (Blinn et al., OOPSLA 2024,
https://hazel.org/papers/chatlsp-oopsla2024.pdf — total error correction means a
meaningful sketch always exists). The well-typed fillers for a hole are a finite set:
in-scope variables of the right type, constructors, operators, functions whose result
type fits, a new hole. That is exactly TypeSafe's recommended shape — a Choice over
bounded options — and the classic setting of type-directed synthesis (Osera &
Zdancewic, PLDI 2015, https://dl.acm.org/doi/10.1145/2813885.2738007), where types
prune the search and a scorer picks. Recent work does the same with an LLM as the
scorer (LLM-guided enumerative synthesis, e.g. HySynth; survey
https://www.emergentmind.com/topics/program-synthesis). **Jev as that scorer is the
novel part.** Hazel is plausibly the one environment where this works.

## Where it breaks

1. **Compounding error.** A 5-line function ≈ 30–60 AST decisions. At 95% per
   choice, 40 choices ≈ 13% whole-function success. Mitigation: verify with Hazel's
   live eval/tests after each definition; escalate low-confidence holes (TypeSafe
   `sde_cascade`: escalate when any P(wrong) is high).
2. **Semantics need reasoning.** "Which of these 12 Int-typed expressions goes
   here?" is local selection (good). "What algorithm solves this?" is System Two
   (bad, per the jaggedness page). The planner must supply the semantics — risk: a
   plan precise enough for Jev is nearly the code itself.
3. **Where the savings actually are.** The main model's cost is dominated by
   *input* context per turn, not code output tokens. Jev writing code saves latency
   and big-model turns, not much writing cost. The win must be measured in turns and
   wall time, not "output tokens saved".
4. **Sequential depth.** Nested holes depend on their parents; ~100 ms × tree depth
   per definition (independent sibling holes can go in parallel).
5. **Choice, not Noul.** Picking one filler from N is a Choice (≤255 options), with an
   explicit `escalate` option — removing the abstain option drove Jev to 0% in
   jev-calibration-audit.

## Call count and latency (#17)

Naive: one call per AST node, sequential → 40 × 0.25 s = 10 s. Not the design.

1. **Batch by depth.** Holes at the same depth don't depend on each other → one request
   with one Choice per hole. Sequential rounds = tree **depth** (~4–8), not node count.
2. **Bigger candidates.** The engine enumerates well-typed expressions up to a small
   depth (`volume(r) * cost(r)`, `map(rows, ?)`) instead of single tokens, pruned by
   type and capped at 255. One Choice picks a whole subexpression.
3. **Planner sketches the shape.** The planner (the brain) writes a sketch with typed
   holes; Jev fills only the holes. Sketch granularity is the dial between "planner
   writes almost everything" and "Jev assembles a lot".
4. **Most edits are one swap.** Operator/argument/callee changes = 1 round.

Estimate for a 5-line function: 2–4 rounds × ~0.25 s ≈ **0.5–1 s**, ~5–10 Choices →
at 95% each ≈ 60–77% first-try, before test-driven retries. Measure, don't trust.

## What "typed by construction" buys — and doesn't

Every edit fills a typed hole with a candidate the type checker produced, so edits are
**syntactically valid and well-typed by construction** — no parse errors, no type errors
to retry. It does **not** make code correct or bug-free: a well-typed `volume(r) +
cost(r)` is exactly the `fix-middle` bug. Correctness still comes from the planner's
intent + tests/probes.

## Names (and literals): options

| | How | Verdict |
|---|---|---|
| **A. Planner vocabulary** | Plan declares new names (and literals). Jev picks from in-scope ∪ vocab. | **Recommended.** Names are load-bearing for every later read (planner and Jev, #11). The planner knows intent; naming is cheap text. |
| B. Namer tool | Jev selects "name this"; another model generates the name | Just generation via a second model; adds a hop for little gain. |
| C. Deterministic labels (`x1`, `f2`) | Engine mints names | Unreadable; degrades Jev's and the planner's later relevance judgments. Maybe as fallback, renamed by the planner at the end. |

Literals (numbers, strings) are the same problem as names → same answer: planner
vocabulary.

## Proposed shape — two Jev phases (#18)

```
Planner (LLM)  → Plan: [{intent, target paths?, new_names, literals, sketch?}]
Phase 1  SELECT (built: JevNav.select, one parallel round)
         per binding, own code + nested folded:
           Noul "needed as context for this edit?"
           Noul "is this a place the edit happens?"      ← second question, same batch
         → curated view  +  edit targets
Phase 2  EDIT (V3)
         state = curated view (one shared document)
         engine lists holes in the targets + well-typed candidates
         one request per depth round: one Choice per hole (+ "escalate")
         apply → tests/probes → pass | escalate to planner
```

- **Phase 1 is exactly what's built** (`select_view`); V3 reuses it unchanged except for
  the optional second Noul.
- **Two questions, not one.** "Relevant" mixes *context* (read it) with *target* (change
  it). Separate Nouls per node keep each judgment single (jaggedness: don't hide two
  judgments in one question); both ride in the same request.
- **Phase 2's state is a shared document** — the curated view — asked many hole
  questions. That is the case TypeSafe's batching cookbook measured at 12.2× cheaper, so
  phase 2 should be one request per round, not one per hole.
- **The view also shrinks phase 2's state**, which limits Jev's documented distraction
  by irrelevant input — phase 1 directly improves phase 2 accuracy.
- Latency: phase 1 ≈ 1 round, phase 2 ≈ 2–4 rounds → ~1–1.5 s end to end.
- The planner still decides *what* the edit is; Jev decides *where* (phase 1) and *which
  well-typed piece* (phase 2).

## Staging (smallest first)

- **V3a — selection edits:** swap an operator, choose an argument, pick which
  function to call, delete/reorder bindings. Many bug fixes are one type-correct swap
  (the `fix-middle` task's bug is `+` → `*`). No free-form text at all.
- **V3b — hole filling:** fill small expressions from candidates + vocab.
- **V3c — whole definitions:** only if V3b's per-definition success rate holds up.

Prerequisite: the navigation study (V1/V2), which builds the Jev plumbing and the
metrics V3 needs.

## Evidence still needed

- Per-hole Choice accuracy on real Hazel holes (build an offline set from the eval
  tasks' fixes: sketch + hole + correct filler).
- Candidate-set sizes per hole in practice (must stay ≤255).
- Planner plan length vs code length — if plans ≈ code, the architecture buys little.

## Sources

TypeSafe jaggedness (generation) · function_calling (closed-set args) · sde_cascade ·
Blinn et al. OOPSLA 2024 (typed holes + LLMs, Hazel) · Osera & Zdancewic PLDI 2015
(type-and-example-directed synthesis) · Plan-and-Act (planner/executor split)
https://arxiv.org/html/2503.09572v2 · planner-executor survey
https://www.emergentmind.com/topics/planner-executor-agentic-framework
