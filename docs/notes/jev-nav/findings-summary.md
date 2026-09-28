# Jev study: findings so far (2026-09-28)

Scope: 3 evals, 8 runs, ~$0.20 total. Agent LLM = openai/gpt-6-luna; Jev = typesafe/jev-1.13.
Full data: [evals/](evals/) · Lab notebook https://claude.ai/artifact/HNRy6AxLRLXj7X9X3uFN1t

**Caveat up front:** almost every setup/test pair has been run once, and the evals changed several
things at a time. Treat everything below as early signal, not results.

## What we tried

| Setup | Who navigates | Who writes code | Runs |
|---|---|---|---|
| Agent navigates + edits (original) | agent | agent | 3 |
| Jev navigates, agent edits | Jev (agent asks in words) | agent | 4 |
| Jev navigates + builds, agent plans | Jev | Jev, from agent's spec | 1 |
| Jev picks first view / Agent sketches, Jev fills / Agent specs, Jev builds | – | – | 0 |

## Findings

1. **No Jev setup has beaten the original agent yet.** On the small program, Jev-navigates matched
   it (both correct, ~same cost). On the big fleet program it was correct but **~40–50% more expensive
   and ~20–55% slower**.
2. **Jev over-opens code.** Of what Jev opened, only **~5–6% was needed** (it did find 100% of the needed
   code). A bigger view makes every agent turn more expensive, which is where the extra cost comes from.
   Jev's own fee was small (~20% of the run).
3. **Jev building code failed** (1 run). The choices Hazel offered it didn't include the needed shapes
   (e.g. `map(?, ?)`), and half-built code was left in the program.
4. **The agent never says DONE.** Every run used all its turns, even after fixing everything. This
   inflates cost for all setups, including the original.
5. **Our grader is too strict.** One Jev run fixed all 3 bugs but was scored wrong because the agent
   rewrote the program's final line.
6. **Our process was flawed:** changes bundled per eval, unvalidated knobs (a 0.8 cutoff, now removed),
   and uneven run counts. See discussion-log #25.

## Conclusions we can draw

- Jev as a **per-definition yes/no navigator** is not paying off: its answers are cheap, but its
  over-selection makes the expensive model slower and costlier.
- The **navigation idea isn't disproven**. What's failing is the question format (one yes/no per
  definition, full code in, no budget). We haven't tested alternatives.
- The **"Jev writes all the code" idea is blocked by our engine**, not by Jev: it never got the right
  options to choose from.

## Where it's trending

- **Likely a poor fit:** Jev as a general navigator for small programs. The agent navigates fine alone,
  and there is little to save.
- **Plausible fit, untested:**
  - picking a *small, ranked* set of definitions in large programs (a "top-k" pick, not yes/no on each);
  - repetitive, local edits across many sites (the same fix applied many times), where a fast chooser
    over typed options is the natural shape;
  - filling holes when the typed option list is small and complete.
- **Open question:** whether the time and cost Jev saves can ever beat the extra agent turns it causes.
  If not, Jev's value is in *correctness by construction* (only type-correct choices), not in speed.

## Next (nothing runs without Russ's go-ahead; each costs < 5¢)

1. Jev-only navigation test: score its picks against known needed code, no agent (~$0.001 per test).
   Compare question wordings one at a time.
2. Harness fixes that help every setup: stop when the output is correct; grade each fix, not the final line.
3. Only then: a planned grid (every setup × every test × 3 runs), costed up front.
