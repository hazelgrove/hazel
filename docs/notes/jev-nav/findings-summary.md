# Jev study: findings so far (updated 2026-09-28, after Eval 004)

Scope: 4 evals, 37 runs, ~$0.97 total (Eval 004 = 29-run grid, see [evals/eval-004.md](evals/eval-004.md)). Agent LLM = openai/gpt-6-luna; Jev = typesafe/jev-1.13.
Full data: [evals/](evals/) · Lab notebook https://claude.ai/artifact/HNRy6AxLRLXj7X9X3uFN1t

**Caveat up front:** almost every setup/test pair has been run once, and the evals changed several
things at a time. Treat everything below as early signal, not results.

## Latest: Sept 28 grid (Eval 004, 29 runs, 2–3 per box)

| Test | Original | Jev navigates | Jev builds code | Full Jev |
|---|---|---|---|---|
| Small billing program | 2/3 · $0.0127 | 2/2 · 0% | 0/2 | 0/2 |
| Fleet, bugs named | 3/3 · $0.0285 | 3/3 · +2% | **2/2 · −22%** | 2/2 · +46% |
| Fleet, symptoms only | 3/3 · $0.0280 | 3/3 · +32% | 0/2 | 0/2 |

- **Jev navigates:** always correct (8 of 8). Same cost on 2 of 3 tests, +32% when only symptoms
  are given. Slower on every test (11–37%).
- **Jev builds code:** worked when the fix was spelled out: 2 of 2, 22% cheaper, and one run needed
  only 15 turns. It failed on symptoms-only (the agent had to work out the fix) and on the small
  program (it needs `map(?, ?)`, which Hazel doesn't offer).
- **Full Jev:** the most expensive setup and no more accurate.
- The "Jev picks first view" and "Jev fills blanks" setups were dropped. A fixed starting view and
  the agent writing blanks it could fill itself both add little.

## What we tried

| Setup | Who navigates | Who writes code | Runs |
|---|---|---|---|
| Agent navigates + edits (original) | agent | agent | 3 |
| Jev navigates, agent edits | Jev (agent asks in words) | agent | 4 |
| Jev navigates + builds, agent plans | Jev | Jev, from agent's spec | 1 |
| Jev picks first view / Agent sketches, Jev fills / Agent specs, Jev builds | – | – | 0 |

## Findings

> **Correction (2026-09-28):** per-turn data overturned two earlier claims. Jev did *not* make the
> agent's screen bigger, and the "~5% needed" figure was misleading. Details in 1–2 below.

1. **Mixed result, too few runs to call.** Small program: Jev navigating was slightly cheaper (−4%) and
   faster (−22%) than the original. Big fleet program: correct, but +41–60% cost and +20–81% time.
   Where the big-program gap comes from (symptoms test, +$0.0107):
   - **Jev's fee: 64%.** Every view request resends the whole program (~11k tokens × 15 requests).
   - **The agent writing more: 36%.** It produced 39% more output tokens (23.3k vs 16.8k).
   - **Not screen size.** Input per turn was the same (23.0k vs 23.1k tokens; 90% cached either way).
   - **Time:** Jev took about 6 s per run (~0.4 s per request). The slowdown came from the agent's own
     turns (13.2 s vs 10.9 s each).
2. **Jev's picks are better than "5%" suggested.** First requests were 18–50% on target (3/10, 3/6,
   3/17, 2/5) and never missed a bug location. The 5% figure pooled every request in a run against
   only the 3 bug locations, but later requests legitimately ask about other code. A fair score needs
   a per-request "what was needed", which we don't record yet.
3. **Jev building code failed** (1 run). The choices Hazel offered it didn't include the needed shapes
   (e.g. `map(?, ?)`), and half-built code was left in the program.
4. **The agent never says DONE.** Every run used all its turns, even after fixing everything. This
   inflates cost for all setups, including the original.
5. **Our grader is too strict.** One Jev run fixed all 3 bugs but was scored wrong because the agent
   rewrote the program's final line.
6. **Our process was flawed:** changes bundled per eval, unvalidated knobs (a 0.8 cutoff, now removed),
   and uneven run counts. See discussion-log #25.

## Conclusions we can draw

- Jev as a navigator is **fast and roughly on target, but its fee plus the agent's extra writing
  outweigh any saving** on big programs. The agent navigates fine by itself.
- The **biggest lever is cheaper Jev requests**: send one-line summaries instead of full code, or
  only what changed. That targets 64% of the gap.
- **Cost comparisons are mostly measuring the turn limit**, because every run hit it. Fix "stop when
  done" before drawing firm cost conclusions.
- **"Jev writes the code" is blocked by our engine, not by Jev**: it never got the right options.

## Where it's trending

- **Promising:** Jev building small, clearly specified fixes (2/2, −22%). Needs more runs, and
  `map(?, ?)`-style options to go beyond simple edits.
- **Not paying off yet:** Jev navigating when the bug is vague (+32%).
- **Worth testing (< 5¢ each):** cheaper Jev requests; stop-at-done then re-compare; top-k picks;
  filling holes with few options; the same fix in many places.
- **Open:** Jev's value may be type-correct-by-construction code rather than speed or cost.

Visual board: https://claude.ai/artifact/Thzcf8nyh8RzEhgYioSnEK

## Next (nothing runs without Russ's go-ahead; each costs < 5¢)

1. Jev-only navigation test: score its picks against known needed code, no agent (~$0.001 per test).
   Compare question wordings one at a time.
2. Harness fixes that help every setup: stop when the output is correct; grade each fix, not the final line.
3. Only then: a planned grid (every setup × every test × 3 runs), costed up front.
