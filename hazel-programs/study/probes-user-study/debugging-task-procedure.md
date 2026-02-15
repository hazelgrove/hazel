# Debugging Task Procedure: Milestone Scaffolding System

This document defines the structured procedure for debugging tasks in the probes user study. The goals are:

1. **Set participants up for success** by guiding their initial orientation
2. **Enable assessment of partial success** through observable milestones
3. **Time-box effectively** by knowing when someone is stuck vs. making progress
4. **Balance structure with ecological validity** - scaffold the process enough to be assessable without dictating a single rigid path

---

## Design Principles

**Why scaffolding?** In a completely freeform debugging session, it's hard to distinguish "productively exploring" from "lost." With milestones, we can:
- Offer gentle nudges without spoiling the task
- Compare participants' progress at consistent checkpoints
- Assess partial credit (e.g., "found the suspicious expression but didn't fix it")
- Make better time-boxing decisions (let someone keep going if they just hit M3, cut it if they've been stuck at M1 for 8 minutes)

**Why not too much scaffolding?** We want to observe natural debugging strategies. Over-specifying the process would:
- Mask individual differences in approach
- Make every participant look the same (good for completion rates, bad for qualitative data)
- Reduce ecological validity

---

## Task Preparation (for all MVU-style debugging programs)

### Standardize type names

All MVU debugging programs should use consistent naming:
- **`Model`** - the core state type (rename from domain-specific names like `Ledger`, `Board`, etc.)
- **`Action`** - the action/message type (rename if needed)

Rationale: Reduces incidental complexity. Participants learn one pattern in the tutorial and recognize it across tasks. The types can still have domain-specific fields and constructors.

### Standardize comment structure

Each program should have:
1. A **header comment** (2-3 lines) explaining the program's concept
2. **Brief comments on Model fields** explaining what each field tracks
3. **Brief comments on Action constructors** explaining what each action does
4. **Section markers** for orientation: `# ===== DATA TYPES =====`, `# ===== CORE LOGIC =====`, `# ===== TESTS =====`
5. No comments inside function bodies (these are for the participant to read and understand)

### Standardize test presentation

Failing tests should use `hint` strings that describe the **expected behavior**, not the bug. Tests should be ordered so that:
- Passing tests come first (basic functionality)
- Failing tests appear in a natural cluster (the broken feature area)
- The first failing test is the simplest case of the broken behavior

---

## The Milestone Sequence

### M0: Orientation (~2 min)

**Participant instruction:**
> "Start by reading the comment at the top of the program to understand what it does. Then look at the `Model` and `Action` types to understand the program's state and the actions that can be taken. Feel free to glance at any type aliases referenced in Model and Action. You don't need to read the whole program yet — just get a feeling for what it's about."

**What we're looking for:**
- Does the participant read the header and types, or jump straight to tests/code?
- How long do they spend here?
- Do they verbalize understanding? ("OK so this is tracking harvests with some kind of streak...")

**Milestone achieved when:** Participant indicates they have a general sense of what the program does (either verbally or by moving on to the next step).

**Researcher note:** If participant skips this entirely and jumps to code, note it but don't intervene. Some participants prefer bottom-up orientation and that's valid data.

---

### M1: Identify the Failure (~1-2 min)

**Participant instruction:**
> "Scroll down to the tests section. Find the first test with a red X — that's a failing test. Read the test to understand what behavior it's checking."

**What we're looking for:**
- Can they locate the failing test?
- Do they read and understand what the test expects?
- Do they verbalize the discrepancy? ("It expected streakBonus to be 0 but something else is happening")

**Milestone achieved when:** Participant can articulate what the failing test expects and that the program isn't producing it.

---

### M2: Probe the Relevant Call (~2-4 min)

**Participant instruction:**
> "Now try to figure out what's going wrong. The test calls some function(s) — try adding a probe to the most relevant function call to see what values are flowing through. You can probe a function application to inspect its inputs and output."

**What we're looking for:**
- Where do they place their first probe? (On the `update`/`run` call in the test? On a subexpression? Somewhere in the function body?)
- Do they use the probe effectively to see the mismatch between expected and actual?
- How quickly do they identify which function is behaving incorrectly?

**Milestone achieved when:** Participant has a probe showing output from the relevant function and can see that something in its behavior doesn't match expectations.

**Researcher note:** "Relevant function call" is deliberately vague. In harvest-streak, the ideal target is `processHarvest` (or the `update` call that dispatches to it). But probing `run` or the test's final assertion are also reasonable starting points. We want to see their strategy, not mandate one.

---

### M3: Trace into the Bug Site (~3-6 min)

**Participant instruction:**
> "Now that you can see something is wrong, try to trace it back to where the problem originates. You can use Step Into on a probe sample to jump inside a function call, or you can navigate to the function definition and add probes there. The idea is to follow the chain of function calls and values until you find where the computation first goes wrong."

**What we're looking for:**
- Do they use Step Into (preserving call context) or Jump to Definition (losing context)?
- How do they navigate between samples when a function is called multiple times?
- Do they use the closure cursor bar to correlate probes across different points?
- How many "hops" does it take them to reach the buggy expression?

**Milestone achieved when:** Participant is looking at the function body (or expression) where the bug actually lives, with probe(s) showing the problematic values.

**What "bug site" means:** This is the expression or small region where the incorrect computation happens. It might be a comparison, an assignment, an argument — the place where if you look at the values flowing in and out, you can see something is wrong. In harvest-streak, this is the `continues` binding where `h.quality == newLast` is always true.

**Key distinction:** Reaching the bug site != understanding the bug. A participant might arrive at `continues` and see it's always `true` without yet understanding *why* (that `newLast` is the wrong thing to compare against). That understanding is M4.

---

### M4: Identify the Incorrect Expression (~2-5 min)

**Participant instruction (if needed):**
> "You've found where the computation goes wrong. Now try to figure out *why* it's wrong. Look at the values the probe is showing you — is something always a value it shouldn't be? Is a comparison using the wrong variable? The bug might be right here, or it might be that one of the values being used here got its wrong value from somewhere upstream."

**What we're looking for:**
- Can they articulate what's specifically wrong? ("This comparison is always true" / "This variable has the wrong value")
- Do they identify the root cause vs. a symptom?
  - **Root cause:** "It's comparing h.quality with newLast, but newLast is just h.quality, so it's comparing with itself"
  - **Symptom:** "continues is always true when it shouldn't be"
- If the bug is upstream (a wrong value being passed in), do they trace further back?

**Milestone achieved when:** Participant can point to a specific expression and say something is wrong with it, even if they haven't formulated the fix yet.

**Note on "upstream" bugs:** Sometimes the expression a participant arrives at isn't literally where the code needs to change — the value it's using got its wrong value from a different definition. In harvest-streak, `continues` *is* where the fix goes (use `ledger.lastQuality` instead of `newLast`). But in other tasks, M4 might involve one more hop. We track whether participants recognize this distinction.

---

### M5: Fix the Bug (open-ended)

**Participant instruction:** None needed — participants naturally attempt a fix once they understand the problem.

**What we're looking for:**
- Do they propose the correct fix?
- Do they make the edit and re-run tests?
- If their first fix is wrong, how do they iterate?
- Total time from M4 to successful fix

**Milestone achieved when:** All previously-failing tests now pass.

**Note:** Some participants will reach M4 (identify the wrong expression) but struggle to formulate the correct fix. This is still valuable partial success. The milestones help us distinguish:
- Didn't find the area (stuck at M1-M2)
- Found the area but not the expression (stuck at M3)
- Found the expression but not the fix (stuck at M4)
- Found and fixed it (M5)

---

## Timing and Intervention

| Time | Status | Action |
|------|--------|--------|
| 0:00-2:00 | M0-M1 expected | Let participant orient freely |
| 2:00-6:00 | M2-M3 expected | Observe probe strategy |
| 6:00-10:00 | M3-M4 expected | Note if stuck; no intervention yet |
| 10:00 | If < M3 | **Nudge:** "The bug is in the [function name] function" |
| 10:00 | If at M3+ | Let them continue |
| 12:00 | If < M4 | **Hint:** "Look at what values [key expression] is comparing" |
| 15:00 | End | Record final milestone reached; reveal answer for debrief |

**Intervention philosophy:** The nudges are designed to help a stuck participant skip to M3 (knowing which function) without spoiling M4-M5 (understanding what's wrong and fixing it). The 10-minute hint is equivalent to the existing plan but now contextualized — we're not just giving a hint, we're moving them past a specific milestone they're stuck at.

**Time-boxing insight:** The milestones make it clearer when to let someone keep going vs. when to intervene. A participant who just reached M3 at minute 9 is making progress — let them run. A participant who's been poking around at M1 since minute 3 is unlikely to find it without help.

---

## What Participants See vs. What We Track

### Participant-facing instructions

Participants receive a brief instruction card (physical or on-screen) before each task:

> **Debugging Task**
>
> This program has some failing tests. Your job is to find and fix the bug.
>
> **Suggested approach:**
> 1. Read the comment at the top, then look at the `Model` and `Action` types to understand the program
> 2. Scroll to the tests and find the first failing one (marked with a red X)
> 3. Use probes to investigate what's going wrong
> 4. Trace the problem back to its source and fix it
>
> Take your time and think out loud as you work.

This is deliberately lighter than the full milestone description. We don't tell them about M3 vs. M4, step-into vs. jump-to-def, etc. The scaffolding guides their first steps (orient, find failing test, start probing) without dictating strategy for the harder parts.

### Researcher tracking sheet

For each participant and task, the researcher records:

| Milestone | Time reached | Notes |
|-----------|-------------|-------|
| M0: Orientation | mm:ss | What did they read? Verbalized understanding? |
| M1: Identify failure | mm:ss | Which test? Articulated expectation? |
| M2: Probe relevant call | mm:ss | Where did they probe? What did they see? |
| M3: Reach bug site | mm:ss | How did they get there? (step-into / jump-to-def / manual scroll) |
| M4: Identify wrong expr | mm:ss | Root cause or symptom? Articulated clearly? |
| M5: Fix | mm:ss | Correct on first try? Iterations? |
| Hint given? | mm:ss | Which hint? Did it help? |

Plus qualitative notes on:
- Probe placement strategy (targeted vs. exploratory)
- Use of specific features (closure cursor, step-into, pin, environment hover)
- Confusion points and recovery
- Verbal expressions of insight or frustration

---

## Adapting for Non-MVU Tasks

The milestone sequence assumes an MVU structure but can be adapted:

- **M0 (Orientation):** For non-MVU programs, direct participants to the main data types and key function signatures instead of Model/Action specifically.
- **M1 (Identify failure):** Unchanged — all tasks have failing tests.
- **M2-M4 (Probe, trace, identify):** The general pattern of "probe a call, follow it inward, find the wrong expression" applies to any function-call-based program. The specifics of what "inward" means vary by task structure.

---

## Implications for Program Design

This milestone system suggests several changes to the debugging programs:

### 1. Rename types consistently
All programs should use `Model` and `Action` (or document clearly if they don't and why).

### 2. Ensure the "trace inward" path exists
The bug should be reachable by a sequence of: probe a call in the test -> step into -> see something wrong -> possibly step into again. If the bug requires more than 2-3 such hops, it may be too hard for the time limit. If it requires zero hops (bug is visible directly from the test), it's too easy and doesn't exercise probes.

### 3. Make the first failing test simple
The first failing test should exercise the bug with minimal setup so that the "wrong value" is easy to spot once probed. More complex failing tests can exist further down, but the entry point should be clean.

### 4. Comments should explain data, not logic
Comments on types and field meanings help M0 orientation. Comments inside function bodies would reduce the need to actually debug. The scaffold versions (with detailed comments) may be too much for the study — consider using the minimal-comment versions with only type-level documentation.

### 5. Consider adding a "guide test" comment
Above the first failing test, a brief comment like:
```
# Streak tests - these check that quality changes reset the bonus #
```
This helps participants at M1 understand what behavior area is broken without spoiling the bug location.

---

## Open Questions

- **How much detail in participant instructions?** The current version is light (4 bullet points). Should we add "try using Step Into" as a suggestion, or leave it for them to discover/remember from the tutorial?

- **Should the programs have in-code milestone markers?** E.g., comments like `# TIP: Start here #` near the tests. This trades ecological validity for smoother participant experience.

- **Do we track milestones in real-time or code post-hoc from recordings?** Real-time tracking is more accurate but requires researcher judgment in the moment. Post-hoc coding from screen recordings is more reliable but slower.

- **Should the hint at 10 minutes be consistent across conditions?** In the print-statement condition, the hint "the bug is in processHarvest" has the same information content. But the available tools for acting on that hint differ (probes vs. adding print statements). Is that a confound or the point?

- **M2 granularity:** Should we distinguish between "probed something" and "probed something *useful*"? A participant who probes `harvestValue` is probing but not in a productive direction. Currently M2 requires the probe to be on the "relevant" call, but relevance is a judgment call.
