# Eval 004: Sept 28 grid (29 runs)

Date 2026-09-28. Agent openai/gpt-6-luna; Jev typesafe/jev-1.13. Logged spend: $0.77.
Results: bench/results/jev-eval-20260928-* (gitignored); compact data: [eval-004-data.json](eval-004-data.json).
Board: https://claude.ai/artifact/Thzcf8nyh8RzEhgYioSnEK (also [../board.html](../board.html)).

Changes since Eval 3:
- Jev navigates uses Jev's own yes/no (P > 0.5); the 0.8 cutoff is removed.
- The "Jev picks first view" and "Jev fills blanks" setups are dropped. Their 9 started runs were
  stopped early and logged nothing.

| Test | Original | Jev navigates | Jev builds code | Full Jev |
|---|---|---|---|---|
| Small billing program (2 bugs) | 2/3 · $0.0127 · 132 s | 2/2 · $0.0127 (0%) · 147 s | 0/2 · $0.0129 | 0/2 · $0.0128 |
| Fleet, bugs named (61 defs) | 3/3 · $0.0285 · 374 s | 3/3 · $0.0290 (+2%) · 444 s | **2/2 · $0.0221 (−22%)** · 440 s | 2/2 · $0.0416 (+46%) |
| Fleet, symptoms only | 3/3 · $0.0280 · 403 s | 3/3 · $0.0371 (+32%) · 554 s | 0/2 · $0.0390 | 0/2 · $0.0408 |

Cells show: runs correct / runs, average cost (change vs the original), average time.

## Findings
- **Jev navigates:** correct in 8 of 8 runs. It costs the same as the original on 2 of 3 tests and 32%
  more on the symptoms-only test. It is 11–37% slower. Across whole runs, only ~5% of what Jev opens
  is a bug location (same pooled measure as before).
- **Jev builds code:** it succeeded when the fix was spelled out. On the fleet program with bugs
  named it was correct 2 of 2 times and 22% cheaper. The best run took 15 turns and 3 edits, with
  all 17 blanks filled by Jev. It failed on the other two tests (0 of 4):
  - Symptoms only: the agent had to work out the fix itself (~34 edits per run, ~23 blanks handed back).
  - Small program: the fix needs a call such as `map(?, ?)`, which Hazel doesn't offer yet.
- **Full Jev:** worse than Jev building code alone (+46% on the named-bug test, 0 of 4 elsewhere).
- **Turn limit:** almost every run still hits it. Stop-at-done remains the fairness fix for cost comparisons.
- **Lost run:** one small-program Jev-navigates run died at start (no output), so that cell has 2 runs.
