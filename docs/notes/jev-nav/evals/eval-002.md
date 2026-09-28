# Eval 002 — Mode 3 (Jev navigates, Luna edits), 2026-09-24

Report: https://claude.ai/artifact/CWujakFy9fVnaHMSHnF2Bs (private; copy in `eval-002-report.html`).
Data with transcripts (key-free): `eval-002-data.json`. Spend $0.0701 (3 runs).

| Task | Setup | Goal | Turns | Total | Wall |
|---|---|---|---|---|---|
| fix-middle | Mode 3 | met | 24 (cap) | $0.0131 | 159 s |
| nav-fleet | Control | met | 34 | $0.0232 | 342 s |
| nav-fleet | Mode 3 | met | 40 (cap) | $0.0343 (+48%) | 528 s (+55%) |

**Why Mode 3 lost on nav-fleet**
1. Jev over-selects: intents named 2–3 bindings, Jev opened 6–53 of 61 (precision 6%). "Relevant"
   is too loose. Try "must be read or changed", higher threshold, exact-path mentions opened
   deterministically.
2. View churn: each modify_view replaces the view; planner used it like expand → 18 calls, +6 turns.
   Try additive modify_view by default.
3. Eval design: prompts name the target bindings, so control navigates in 3 expands. Rewrite nav
   tasks as symptoms (symptom-to-cause).
4. Jev cost 24% of the run: full code of 61 bindings per call (~11k tokens). Send heads for far
   bindings.

**Spend so far:** $0.095 over 5 runs (~$0.019/run; nav-fleet ~$0.029/run).
