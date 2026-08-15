# Probes User Study — Design

Distilled from the study planning doc (`probes-user-study/README.md`).

## Context

From the thesis proposal: a design evaluation leveraging Information Foraging
Theory and a small-N user study (N = 9–15) tracking performance and qualitative
impressions during debugging and authoring tasks, with emphasis on think-aloud
and post-task interviews.

Two task categories:
1. **Debugging** (probes vs print statements) — finding/fixing bugs in existing programs
2. **Program writing** (auto-probe mode) — writing new code with live feedback

Constraints: an artificial language environment (Hazel); participants vaguely
familiar with FP but not Hazel; editor quirks; short tasks to avoid frustration;
foreign-codebase debugging (not participants' own code).

## The core tension

Probes were built as a tool for one's *own* codebases, but the study requires
participants to debug *foreign* code. The question becomes: what aspects of
probes help with the particular challenges of understanding and debugging
unfamiliar code?

Probes plausibly help with: orientation (what values flow through unfamiliar
functions), hypothesis validation ("does this variable contain what I think?"),
correlation across execution points (dynamic focus), and hot-loop navigation.
They help less with: mechanical step-back-through-calls (go-to-definition does
this too), understanding logic (still requires reading), and knowing where to
look in the first place.

## Research questions

**Indirection & navigation**
- RQ1a: Does inline value display reduce the cognitive cost of correlating
  runtime behavior with source code, compared to print statements?
- RQ1b: How do programmers navigate multiple samples from the same function?
  Do dynamic focus and sample alignment help maintain context?
- RQ1c: For loops/recursion with many iterations, how do probes compare to
  interleaved console output for tracking specific iterations?

**Fault localization**
- RQ2a: Does seeing intermediate values narrow bug locations faster?
- RQ2b: When values transform through multiple functions (ADTs changing shape),
  do probes help follow the transformation?
- RQ2c: How is the "trace back" workflow (step-into, pin) used vs forward
  exploration?

**Program writing**
- RQ3a: Does auto-probe catch errors earlier during writing (before the
  implementation is "complete")?
- RQ3b: How do live intermediate values affect API exploration / understanding
  unfamiliar functions?
- RQ3c: Do programmers guide implementation differently than write-then-test?

**Information foraging**
- RQ4a: How do probes affect information "scent" during debugging?
- RQ4b: What cue types do probes provide (clear / fuzzy / elusive)?
- RQ4c: Do probes reduce useless foraging (dead-end paths)?

Most tractable given constraints: RQ1c (hot-loop navigation — clear contrast
with interleaved print output, observable behavior), RQ2b (ADT transformations —
a specific probe advantage), RQ3a (measurable error-detection timing), RQ3b
(observable via think-aloud).

## The print-statement comparison

The comparison is probes vs *print statements*, not probes vs nothing.

Where probes should win: hot loops (print output interleaves; samples are
per-expression), multiple samples from the same function (aligned vs mixed),
no syntax modification, automatic capture (auto-probe requires no upfront
decision about what to print).

Where print may be equivalent or better: simple one-value/one-location cases,
temporal ordering (print shows actual execution order), familiarity.

Task-design implication: tasks should sit in the probe-advantage zone — hot
loops, repeated calls, values transforming through ADTs.

Key feature distinction: step-into from a sample sets the dynamic focus to
*that call*; jump-to-definition doesn't. Tasks should require knowing *which
specific call* had the wrong value.

### Probe benefits inventory (for task design)

| Benefit | Print equivalent? | Task should exercise? |
|---------|-------------------|----------------------|
| Inline values | Sort of (console) | Baseline |
| Multiple samples organized (not interleaved) | No | Yes — hot loops |
| Dynamic-focus alignment | No | Yes — correlate across probes |
| Step-into preserves call context | No | Yes — which call went wrong |
| Environment on hover | No | Yes |
| Pin to filter | No | Maybe |

## Study design

Debugging: within-subjects, counterbalanced. Two debugging tasks of comparable
difficulty; each participant does one with probes, one with print statements;
order and task-condition assignment counterbalanced across participants.

Time budget (~75 min): intro/consent 5, tutorial 20–25, task 1 15 (soft),
survey 3, task 2 15, survey 3, final survey + debrief 10.

Hints: work freely 0–10 min; at 10 min offer "the bug is in [function]"; at
15 min end task and reveal the answer (educational, not punitive).

Instruction level (middle ground): the tutorial teaches all features; task
instructions are minimal ("this program has a failing test — find and fix the
bug"); no prescribed method. Ecological validity while ensuring participants
know the features exist.

## Measures

Quantitative: time to fix; completion (yes/no/with-hint); number of
probes/prints added (from logs).

From transcripts (LLM-assisted coding): VALUE_CHECK (looking at a value),
CORRELATION (connecting values across locations), HYPOTHESIS, CONFUSION,
FEATURE_USE.

IFT-inspired metrics: navigation efficiency (steps to find bug), backtracking
frequency, cue recognition time.

Post-task survey (Likert 1–7):
1. I could easily see what values expressions were taking.
2. I could connect runtime values to the relevant code.
3. When an expression had multiple values, I could tell which were related.
4. The tool helped me find the bug.

Open-ended: what helped most; what was confusing.
Final survey: preference between conditions; which made tracking values through
calls easier; would you use this in your own work.

## Related reading

- How Programmers Debug, Revisited: An IFT Perspective (IEEE)
- Projection Boxes (Lerner, CHI 2020)
- An IFT Perspective on Tools for Debugging (ACM)
- LIVE workshop series
