# Probes User Study Planning

This document captures planning, research questions, and design considerations for the Live Probes user study.

## Context

From the thesis proposal, the evaluation plan is:
> "We plan a design evaluation leveraging Information Foraging Theory and a user study tracking performance and qualitative impressions during debugging and authoring tasks... a small-N study (N = 9–15 total participants) with an emphasis on think-aloud and post-task interviews."

**Two task categories:**
1. **Debugging** (probes vs print statements) - finding/fixing bugs in existing programs
2. **Program Writing** (auto-probe mode) - writing new code with live feedback

**Constraints we're working with:**
- Artificial language environment (Hazel)
- Participants vaguely familiar with FP but not Hazel specifically
- Hazel editor has bugs/slowness, syntax model quirks
- Need to keep tasks short to avoid frustrating participants
- Foreign codebase debugging (not participants' own code)

---

## The Core Tension

You articulated something important: you built probes as a tool *you* would want to use, for *your own* codebases. But the study requires participants to debug *foreign* code they've never seen before.

This is actually a rich area to explore. The question becomes: **what aspects of probes help with the particular challenges of understanding and debugging unfamiliar code?**

### What probes might help with in foreign code:
1. **Orientation** - Understanding what values flow through unfamiliar functions
2. **Hypothesis validation** - Quickly checking "does this variable contain what I think?"
3. **Correlation** - Connecting multiple execution points (closure cursor)
4. **Hot loop navigation** - When there are many samples, keeping them organized

### What probes might NOT help with as much:
1. The mechanical "step back through function calls" process (go-to-definition does this too)
2. Understanding the *logic* of code (still need to read it)
3. Knowing *where* to look in the first place

---

## Research Questions (Expansive)

Let's start broad and then narrow:

### RQ Category 1: Indirection and Navigation

**RQ1a**: Does inline value display reduce the cognitive cost of correlating runtime behavior with source code, compared to print statements?

**RQ1b**: How do programmers navigate between multiple samples from the same function? Do the closure cursor and sample alignment help them maintain context?

**RQ1c**: When debugging loops/recursion with many iterations, how do probes compare to interleaved console output for tracking specific iterations?

### RQ Category 2: Fault Localization

**RQ2a**: Does seeing intermediate values help programmers narrow down the location of bugs faster?

**RQ2b**: When values transform through multiple functions (ADTs changing shape), do probes help programmers follow the transformation?

**RQ2c**: How do programmers use the "trace back" workflow (step-into, pin) vs. forward exploration?

### RQ Category 3: Program Writing

**RQ3a**: Does auto-probe mode help programmers catch errors earlier during the writing process (before "completing" their implementation)?

**RQ3b**: How do live intermediate values affect the programmer's API exploration / understanding unfamiliar functions?

**RQ3c**: Do programmers use probes to guide their implementation differently than they would with write-then-test?

### RQ Category 4: Information Foraging

**RQ4a**: How do probes affect the "scent" of information during debugging? Do they provide clearer cues for where to look next?

**RQ4b**: What types of cues do probes provide (clear, fuzzy, elusive per [IFT research](https://www.sciencedirect.com/science/article/abs/pii/S1045926X18302003))?

**RQ4c**: Do probes reduce "useless foraging" (following dead-end paths)?

---

## Narrowing: What's Actually Testable?

Given constraints (small N, limited time, Hazel quirks), which questions can we realistically get data on?

### Most Tractable for Debugging:

**RQ1c (hot loop navigation)** seems promising because:
- Clear contrast with print statements (interleaved output vs. organized samples)
- Can design tasks that specifically exercise this
- Observable behavior (how participants navigate samples)

**RQ2b (value transformations through ADTs)** also seems valuable:
- Can design tasks where values change type/shape through the call chain
- Probes show the transformation; print statements require manual inspection
- This is a specific probe advantage, not just "values inline"

### Most Tractable for Program Writing:

**RQ3a (catching errors earlier)** is measurable:
- Can compare error introduction time to error detection time
- But: requires careful task design and instrumentation

**RQ3b (API exploration)** is observable:
- Think-aloud can reveal how participants use values to understand functions
- But: qualitative, hard to quantify

---

## The Print Statement Comparison

The comparison isn't just "probes vs. nothing" — it's "probes vs. print statements" which many programmers already use effectively.

### Where Probes Should Win:
1. **Hot loops** - print output interleaves; probe samples are per-expression
2. **Multiple samples from same function** - probes show them aligned; prints mix together
3. **No syntax modification** - probes don't require `print x; expr` pattern
4. **Automatic capture** - auto-probe doesn't require deciding what to print upfront

### Where Print Statements Might Be Equivalent or Better:
1. **Simple cases** - one value, one location
2. **Temporal ordering** - print output shows actual execution order
3. **Familiarity** - everyone knows how to use print

### Task Design Implication:
Tasks should be in the **probe advantage zone** — hot loops, multiple calls to same function, values that transform through ADTs.

---

## Debugging Task Analysis

### Current Tasks

| Task | Hot Loop? | ADT Transforms? | Mechanical Traceback? |
|------|-----------|-----------------|----------------------|
| Tamagotchi (decay bug) | No | Partial (Stats record) | Yes |
| Tamagotchi (bonus bug) | No | Partial | Yes |
| Emojipaint (setCell bug) | Yes (nested map) | No (just indices) | Yes |
| Game of Life | Yes (neighbor counting) | No | Yes, but performance issues |

### Analysis

**Tamagotchi** - The bug is `s.happiness < 30` instead of `s.hunger < 30`. This is:
- Findable by mechanical traceback (step into decayStats, see wrong field)
- Probes show the Stats record, but you still need to notice the wrong field
- Not particularly hot-loop-y

**Emojipaint** - The bug (j == col should be j == col) is inside nested mapi. This is:
- Hot loop: nested iteration over grid
- But the bug is subtle (using wrong variable `i` instead of `j`)
- Probes could help by showing incorrect cell updates

### Suggested Improvements

1. **Make values transform** - Have the bug manifest as a *transformed* value being wrong, not just a wrong field access. This makes "following the value back" more valuable.

2. **Add a "hot update" scenario** - A task where the update function is called many times in a fold, and you need to find which specific call went wrong. This is where print statements get messy.

3. **Reduce mechanical traceback** - If the bug is always "just step back 3 function calls and look at line 42", probes don't add much. Make the bug location less predictable.

---

## Program Writing Task Analysis

### The Challenge

You said: "I find it pretty compelling personally... getting live values as I'm writing programs. But what do we do with that — how do we make a research question that we can get answers to?"

### Possible Angles

**1. Error Prevention**
- Hypothesis: Auto-probe helps catch errors *before* the implementation is complete
- Measure: Time from introducing an error to noticing it
- Challenge: Need to instrument for this, hard to control

**2. API Discovery**
- Hypothesis: Live values help understand unfamiliar functions
- Measure: Time to correctly use a function with ambiguous argument order
- Example: `string_split(sep, str)` vs `string_split(str, sep)` — probes reveal which is which

**3. Incremental Confidence**
- Hypothesis: Programmers feel more confident about partial implementations with auto-probe
- Measure: Qualitative (think-aloud, interviews)
- More exploratory than confirmatory

### The "Mentions Extractor" Insight

The mentions extractor task from study-write is actually good because:
- Uses `string_split` which has confusable argument order
- Pipeline of transformations (split → filter → map)
- Each step can be checked incrementally
- Probes show intermediate lists, not just final result

---

## Information Foraging Theory Lens

From [IFT debugging research](https://dl.acm.org/doi/10.1145/2430545.2430551):
- Programmers follow "scent" more than they form explicit hypotheses
- Navigation is often scent-following rather than hypothesis-driven
- Different cue types: clear (obviously relevant), fuzzy (maybe relevant), elusive (hard to find)

### Probes Through IFT Lens

**Probes as scent enhancement:**
- Values are immediate cues about relevance ("this is 0 when it should be positive")
- Closure cursor provides "same-execution" scent across probes
- Auto-probe provides broad initial scent without manual instrumentation

**Potential IFT-based metrics:**
- Navigation efficiency (steps to find bug)
- Backtracking frequency (following dead ends)
- Cue recognition time (how quickly participants react to useful information)

---

## Study Design Options

### Option A: Focused Debugging Study (Most Practical)

**Design:** Between-subjects, probes vs. print statements, 2-3 debugging tasks

**Tasks:** Designed for hot loops and ADT transformations

**Measures:**
- Time to fix bug
- Navigation patterns (think-aloud, screen recording)
- Qualitative impressions (post-task interview)

**RQs addressed:** RQ1c, RQ2b, RQ4 (IFT aspects)

### Option B: Focused Writing Study (Also Practical)

**Design:** Within-subjects, auto-probe on/off, 3-4 small writing tasks

**Tasks:** API exploration, pipeline building

**Measures:**
- Time to correct implementation
- Error introduction/detection timing (if instrumentable)
- Qualitative impressions

**RQs addressed:** RQ3a, RQ3b

### Option C: Combined Study (More Ambitious)

**Design:** Mixed, debugging block + writing block

**Risk:** Too long, participant fatigue, too many variables

### Recommendation

**Start with Option A (debugging)** because:
- Cleaner comparison (probes vs. print)
- More established methodology (IFT precedent)
- Easier to design tasks that highlight probe advantages

**Then consider Option B separately** or as a smaller component.

---

---

## Concrete Study Design

### Debugging: Within-Subjects, Counterbalanced

- 2 debugging tasks (Task A, Task B) of comparable difficulty
- Each participant does one with probes, one with print statements
- Counterbalance: order and task-condition assignment

| Participant | First | Task | Second | Task |
|-------------|-------|------|--------|------|
| P1 | Probes | A | Print | B |
| P2 | Probes | B | Print | A |
| P3 | Print | A | Probes | B |
| P4 | Print | B | Probes | A |

### Time Budget

| Phase | Duration |
|-------|----------|
| Intro + consent | 5 min |
| Tutorial | 20-25 min |
| Debugging Task 1 | 15 min (soft limit) |
| Post-task survey | 3 min |
| Debugging Task 2 | 15 min |
| Post-task survey | 3 min |
| Final survey + debrief | 10 min |
| **Total** | ~75 min |

### Time Limits and Hints

| Time | What happens |
|------|--------------|
| 0-10 min | Work freely |
| 10 min | Offer hint: "The bug is in [function]" |
| 10-15 min | Continue with hint |
| 15 min | End task; reveal answer if not found |

Revealing the answer keeps it educational, not punitive.

---

## Instruction Level

**Approach: Middle ground**

- Tutorial teaches all features (step-into, closure cursor, pin, etc.)
- Task instructions are minimal: "This program has a failing test. Find and fix the bug."
- Don't prescribe method (read first vs. jump in, use step-into vs. manual probes)
- Observe what they actually use

This gives ecological validity while ensuring participants *know* the features exist.

---

## Probe Benefits Inventory (for task design)

| Benefit | Print equivalent? | Task should exercise? |
|---------|-------------------|----------------------|
| Inline values | Sort of (but console) | Baseline |
| Multiple samples organized (not interleaved) | No | **Yes** - hot loops |
| Closure cursor alignment | No | **Yes** - multiple probes, correlate |
| Step-into preserves call context | No (jump-to-def doesn't) | **Yes** - which call went wrong |
| Environment on hover | No | Yes |
| Pin to filter | No | Maybe |

**Key insight:** Step-into from a sample sets the dynamic cursor to that call. Jump-to-definition doesn't. Tasks should require knowing *which specific call* had the wrong value.

---

## Measures

### Quantitative
- Time to fix bug
- Completion (yes/no/with-hint)
- Number of probes/prints added (from logs)

### From Transcripts (LLM-assisted)
- VALUE_CHECK: Looking at a value
- CORRELATION: Connecting values across locations
- HYPOTHESIS: Forming theory about bug
- CONFUSION: Expressing uncertainty
- FEATURE_USE: Using specific tool feature

### Surveys

**Post-task (Likert 1-7):**
1. I could easily see what values expressions were taking.
2. I could connect runtime values to the relevant code.
3. When an expression had multiple values, I could tell which were related.
4. The tool helped me find the bug.

**Open-ended:**
- What helped most?
- What was confusing?

**Final (comparing conditions):**
- Which did you prefer?
- Which made it easier to track values through function calls?
- Would you use this in your own work?

---

## Next Steps

1. **Design new debugging task** - Exercise hot loops, step-into context, alignment
2. **Pilot with lab members** - Target: next week
3. **Calibrate difficulty** - Aim for ~50-70% completion with print in time limit
4. **Finalize tutorial** - Make sure it teaches all features without prescribing usage
5. **Set up logging** - What can we capture automatically?
6. **IRB** - Status?

---

## Open Questions

- [ ] How much Hazel syntax training needed?
- [ ] Provide cheat sheet?
- [ ] Handle editor bugs during study?
- [ ] Screen + audio recording setup?
- [ ] Writing tasks: include or defer?

---

## Related Reading

- [How Programmers Debug, Revisited: An IFT Perspective](https://ieeexplore.ieee.org/document/5674060/)
- [Projection Boxes (Lerner, CHI 2020)](https://cseweb.ucsd.edu/~lerner/papers/projection-boxes-chi2020.pdf)
- [IFT Perspective on Tools for Debugging](https://dl.acm.org/doi/10.1145/2430545.2430551)
- [LIVE Workshop](https://liveprog.org/live-2024)
