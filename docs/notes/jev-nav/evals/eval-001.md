# Eval 001 — control vs full Jev on fix-middle (2026-09-24)

Report: https://claude.ai/artifact/MxFieYu6Xmq7VPo7Sgzdia (private; copy in `eval-001-report.html`).
Data (recovered, key-free): `eval-001-data.json`.

**Setup:** planner `openai/gpt-6-luna`, Jev `typesafe/jev-1.13`, task `fix-middle`, n = 1 per arm,
arms `control` and `jev` (prepass + view + edit + builds). Real spend $0.0245 (key delta).

| | Control | Full Jev |
|---|---|---|
| Goal (30700) | met | not met — `billed = fun rows -> ?` left open |
| Total cost | $0.0136 | $0.0125 (Jev $0.0019) |
| Wall | 205 s | 134 s |
| Turns | 24 (cap; never said DONE) | 24 (cap) |
| Navigation | 5 expand | 10 modify_view; first selection opened exactly the 2 targets |

**Findings → fixes**
1. Built-ins (`map`) offered only as bare names, never as typed calls `map(?, ?)` → Jev
   correctly escalated 14×. Fix: offer every in-scope function whose result type fits, as a call.
2. Partial builds left `?` in the program. Fix: jev_edit all-or-nothing (restore on open holes).
3. Binding forms offered for existing names (`fun volume -> ?`). Fix: only fresh planner names.
4. Neither arm said DONE; Jev arm retried the same failing spec 14×. Fix: stop on goal met;
   nudge planner after 2 identical escalations.

**Cost tracking:** ~$0.013 per run; projected 12-run eval ≈ $0.16; one Jev request ≈ $0.00002.

**Incident:** bench/results was wiped by a concurrent STUB test; data recovered from captured
output. Rule added: never delete results dirs; tests use temp dirs.
