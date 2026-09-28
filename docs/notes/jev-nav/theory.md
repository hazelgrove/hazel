# Theory — typed structure editing with a System-1 policy

Working hypothesis for the lab, from discussion #21 (2026-09-24). Not yet tested.

## Claim

Code generation need not be free-form text. In a typed structure editor (Hazel, whose
foundation Hazelnut [Omar et al., POPL 2017] is a calculus of a cursor + finite action set
over holes), every reachable state is syntactically valid and well-typed. If a model only
*selects* actions, its output is sound by construction. Correctness beyond typing still
needs a spec and tests.

## The core architecture (Russ, #22)

The novelty is the pairing itself: a System-1 coding transformer (Jev) given a structure-editor
action language, acting inside a structure editor. Roles:

- **Planner (Claude) = System 2, before any code exists.** Decides the approach and the
  structure — e.g. "tail recursion with two accumulators, `go(n, a, b)`" — plus the names and
  literals. This is the pre-implementation plan / spec.
- **Jev = System 1, the implementor.** Makes only local picks from finite, typed menus.
- **Hazel = the environment.** Lists the legal actions and guarantees every state is valid.

## What is and isn't new

- **Not new:** sound-by-construction editing (Hazelnut); type-constrained LLM decoding
  (e.g. Mündler et al., PLDI 2025); LLM-guided enumerative synthesis (HySynth and similar);
  typed holes as LLM context (Blinn et al., OOPSLA 2024).
- **New:** the action-selecting policy is a cheap, calibrated System-1 classifier (Jev),
  not a generative LLM — and its cheapness makes *search* affordable.

## System 1 vs System 2, precisely

- A single pick given a precise local plan ("base case returns `a`") is System 1.
- Choosing the plan ("tail recursion needs two accumulators `go(n, a, b)`") needs
  lookahead — System 2. A System-1 policy can make a locally plausible pick that
  dead-ends later.
- **Spec compression trade-off:** the more the spec reduces every pick to System 1, the
  closer the spec is to the code. Measurable curve: spec length / code length vs Jev
  accuracy.

## The novel combination: System 1 + search ≈ System 2

AlphaGo pattern applied to programs:

| Role | Here |
|---|---|
| Policy (intuition) | Jev's probability over typed actions at each hole |
| Environment | Hazel's typed structure editor (states always valid) |
| Search | beam over top-k picks per hole, backtrack on dead ends |
| Reward / verifier | statics pass, tests pass, probes show expected values |

Economics: Jev is ~100× cheaper per decision than an LLM turn, so hundreds of policy calls
cost about one Claude turn. Search is affordable only because the policy is cheap.

## Plain-English version of "System 1 + search ≈ System 2"

- Jev is like a chess player who **only plays by instinct**: it looks at the board and instantly
  picks the move that *looks* best. Fast, but it never thinks ahead.
- AlphaGo worked the same way: an instinct model proposed a few good-looking moves, then the
  system **tried each a few moves ahead** and kept what worked. Instinct + trying things out
  played like deep thinking.
- **For code:** at each hole Jev gives its top 3 guesses instead of one. We try them in Hazel;
  Hazel says instantly whether it type-checks, and the tests say whether it is right. A dead
  end gets undone and the next guess is tried.
- **Why only Jev makes this affordable:** trying things out means hundreds of guesses. With
  Claude that is hundreds of expensive turns; with Jev it costs about one Claude turn.

**How the planner and search fit together:** the spec settles the *big* decisions up front
(approach, structure, names); search is the safety net for the *small* ambiguity left over
("is it `b` or `a + b` here?"). Better spec → less search needed — a trade-off the eval can
measure directly.

## Cursor vs holes

A single cursor is sequential (~20–30 actions ≈ 5–8 s for tail-recursive fib at 0.25 s).
Filling all open holes per round is the batched equivalent for construction (what
`JevEdit` does). A cursor is still needed for transforming existing code (wrap, unwrap,
move); navigation (`select_view`) can place it.

## Research program (most → least established)

1. **Navigation** (Mode 3) — System 1, measurable now.
2. **Fan-out edits from a spec** — one spec, many sites, each a local pick; parallel.
3. **Build from spec + search** — the novel claim. Experiments: spec-compression curve;
   greedy vs beam; tests as verifier; cost/latency vs Claude writing the code.

## Sources

Hazelnut (POPL 2017) https://hazel.org/papers/hazelnut-popl17.pdf · Blinn et al. OOPSLA 2024
https://hazel.org/papers/chatlsp-oopsla2024.pdf · Osera & Zdancewic PLDI 2015
https://dl.acm.org/doi/10.1145/2813885.2738007 · TypeSafe jaggedness (System 1 limits)
https://docs.typesafe.ai/model-jaggedness/jev-1.13.md · Mündler et al., type-constrained code
generation (PLDI 2025) — verify citation before publishing.
