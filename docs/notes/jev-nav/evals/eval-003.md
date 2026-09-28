# Eval 003 — refined Jev navigator (Mode 3), big fleet program

Date 2026-09-24 · Round 5 code (strict question, yes ≥ 0.8, keyed state, additive modify_view) · planner openai/gpt-6-luna · Jev typesafe/jev-1.13
Spend: $0.100 (key-usage delta 842.6278 → 842.7278). Results: bench/results/jev-eval-20260924-140218, -141240 (backup in scratch eval3-backup/).
Report: Jev Lab Notebook https://claude.ai/artifact/HNRy6AxLRLXj7X9X3uFN1t

| Test | Setup | Goal | Turns | Total $ | Jev $ | Wall s | View calls | Jev precision |
|---|---|---|---|---|---|---|---|---|
| fleet, bugs named (nav-fleet) | Jev view | fail* | 40 | 0.0371 | 0.0066 | 618 | 14 | 5.3% |
| fleet, symptoms only (nav-fleet-sym) | control | met | 40 | 0.0261 | 0 | 438 | 19 expand | - |
| fleet, symptoms only (nav-fleet-sym) | Jev view | met | 40 | 0.0368 | 0.0069 | 526 | 15 | 5.7% |

\* All three bugs fixed correctly; planner replaced the program's final expression with its own checks, so the exact-value goal failed.

## Findings
- Grader too strict: grade per-fix tests, and tell the planner to keep the final line.
- Round-5 tweaks did not fix over-selection (precision still ~5%). Next: one-line headers + rank/pick-k instead of per-binding yes/no.
- Symptom task: Jev view = +41% cost, +20% wall vs control; Jev = 19% of its run cost. No win yet.
- No run said DONE; all hit 40 turns. Stop-on-goal still pending.
