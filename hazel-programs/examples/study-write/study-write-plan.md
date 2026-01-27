# Study-Write: Program Writing Tasks for Probes User Study

This document describes the design of program-writing study tasks for evaluating auto-probes in Hazel. These tasks complement the debugging tasks in `../study/` and form the "program writing" portion of the user study described in the thesis proposal.

## Overview

### Research Context

From the thesis proposal (`/Users/andrewblinn/Dropbox/projects/thesisproposal/Main.tex`, lines 269-277):

> For our user study, the goal is to explore whether inline probes reduce indirection and cognitive load relative to print statements, and assess how auto-probing affects errors during program writing.

The study uses a **between-subjects design**:
- **Baseline**: Hazel with simple print statement support
- **Treatment**: Hazel with probes (specifically auto-probing)

### Primary Research Questions

1. **Error Prevention**: Do auto-probes help users catch mistakes earlier (as they type) vs. discovering them later through test failures?
2. **API Discovery**: Do live intermediate values help users understand unfamiliar function behavior (especially when type signatures are ambiguous)?
3. **Cognitive Load**: Does inline value display reduce the indirection cost of switching between code and output?

### Secondary/Exploratory Questions

- Does seeing live values change how users plan before coding (more trial-and-error vs. think-first)?
- Is there an information overload effect with too many visible values?
- Does live feedback help with program comprehension (understanding existing code)?

## Key References

| Document | Location | Relevant Content |
|----------|----------|------------------|
| Thesis Proposal | `../../../thesisproposal/Main.tex` | Dynamic contextualization, IFT framework, study design (lines 220-277) |
| Probes Guide | `../../docs/probes-guide.md` | Probe syntax, modes, navigation |
| CLI API | `../../plans/hazel-lsp-cli/cli-api.md` | Probe CLI output format (`≡`, `⫽` symbols) |
| Hazel Primer | `../../plans/hazel-lsp-cli/hazel-primer.md` | Hazel syntax reference |
| Stdlib Reference | `../../plans/hazel-lsp-cli/hazel-builtins.md` | Available functions (note: reversed param order from OCaml) |
| Debugging Tasks | `../study/` | Existing debugging study programs (emojipaint, calculator, tictactoe, tamagotchi) |

## Task Categories

### Category 1: Tiny Tasks (1-3 lines to write)

**Goal**: Isolate specific probe benefits in minimal contexts.

These are the smallest meaningful tasks where probes provide value. The user writes a single function body (1-3 lines) given tests. The key is that probes reveal something non-obvious during writing.

**Characteristics**:
- Single function to implement
- 2-4 tests provided
- Completes in ~2-5 minutes
- Targets ONE specific error pattern or discovery scenario

**Example**: `base_route` (in `./basepoint/`)
- User implements: `let base_route = fun path -> ...`
- Tests reveal expected behavior
- Probe insight: `string_split` parameter order is ambiguous from types alone

### Category 2: Small Tasks (5-10 lines to write)

**Goal**: Evaluate probes across slightly more complex logic with multiple intermediate values.

These involve writing a function with internal bindings, possibly with conditionals or list operations. Multiple lines get probed, showing how values flow through the computation.

**Characteristics**:
- Function with 2-4 internal `let` bindings or expressions
- May involve iteration via `map`/`filter`/`fold` or recursion
- 4-6 tests provided
- Completes in ~5-10 minutes
- Multiple points where probes provide feedback

### Category 3: Modification Tasks (1-5 lines to change)

**Goal**: Evaluate probes for understanding existing code and making targeted changes.

User receives a working small-to-medium program and must add/modify specific functionality. Auto-probes help them understand the existing code and verify their changes.

**Subtypes**:
- **Add Definition**: Add a helper function and use it somewhere
- **Extend Logic**: Add a case to a pattern match or condition
- **Fix + Enhance**: Correct a bug AND add a feature (combines debugging + writing)

**Characteristics**:
- Provided program: 20-50 lines (small) or 50-100 lines (medium)
- Change scope: 1-5 lines added/modified
- Auto-probe on the function being modified
- Tests for new behavior provided

## Error Patterns to Target

Each study task should target one or more of these patterns, where probes help users notice/avoid the error:

### 1. Parameter Order Ambiguity
**Pattern**: Function takes multiple parameters of same/similar types; order isn't obvious from types.
**Example**: `string_split(sep, str)` vs `string_split(str, sep)` - both String arguments.
**Probe benefit**: User sees actual split result immediately, can correct order without consulting docs.
**Hazel functions with this issue**: `string_split`, `string_sub`, `nth`, `fold_left`

### 2. Off-by-One Errors
**Pattern**: Index arithmetic where boundaries are easy to get wrong.
**Examples**:
- `nth(list, 0)` vs `nth(list, 1)` for "first element"
- `string_sub(s, 0, n)` vs `string_sub(s, 0, n-1)`
**Probe benefit**: See actual extracted element/substring immediately.

### 3. Fold Accumulator/Initial Value Errors
**Pattern**: Wrong initial value or accumulator update logic in fold.
**Examples**:
- `fold_left(list, fun (acc, x) -> acc + x, 1)` should start at 0
- `fold_left(list, fun (acc, x) -> acc + 1, 0)` miscounts (ignores x)
**Probe benefit**: See accumulator evolve step-by-step across iterations.

### 4. Condition Boundary Errors
**Pattern**: Off-by-one in comparison operators.
**Examples**: `x < 5` vs `x <= 5`, `length(xs) == 0` vs `length(xs) < 1`
**Probe benefit**: See which branch taken for boundary cases in tests.

### 5. List Construction Errors
**Pattern**: Wrong cons direction, append order, or forgetting to reverse.
**Examples**:
- Building list with `x::acc` in fold gives reversed result
- `append(xs, ys)` vs `append(ys, xs)`
**Probe benefit**: See intermediate list state as it's built.

### 6. Pattern Match Coverage
**Pattern**: Missing or wrong case in pattern match.
**Examples**:
- Forgetting empty list case `[]`
- Wrong destructuring: `(a, b, c)` vs `(a, (b, c))`
**Probe benefit**: See which pattern matched, values bound to each variable.

### 7. Type Coercion/Conversion Oversights
**Pattern**: Forgetting to convert between related types.
**Examples**:
- Using int where string expected (or vice versa)
- Forgetting `int_of_string` or `string_of_int`
**Probe benefit**: See actual type of intermediate value.

### 8. Scope/Shadowing Confusion
**Pattern**: Variable name reused, shadowing intended binding.
**Examples**:
- Inner `let x = ...` shadows outer `x`
- Lambda parameter shadows outer binding
**Probe benefit**: Environment display shows actual bindings in scope.

## Scaffolding Decisions

For each task, decide what to provide:

### Type Signatures
- **Provide**: When function signature is part of the "spec" and not the puzzle
- **Omit**: When figuring out the right type is part of the task

### Standard Library Functions
- **Full list**: Provide ~6-12 relevant functions (not just the needed ones)
- **Names only**: Let probes + autocomplete help discover usage
- **Names + signatures**: More guidance, especially for unfamiliar types
- **Names + examples**: Maximum guidance

Recommendation: Vary this across tasks to explore what level of scaffolding works best.

### Tests
- **Always provided** for study-write tasks (vs. study-debug where tests reveal the bug)
- Tests should be thorough enough to catch common errors
- Include edge cases (empty list, boundary values)

## File Organization

Each task lives in its own subdirectory:

```
study-write/
├── study-write-plan.md          # This document
├── basepoint/
│   ├── basepoint-sketch.hz      # What user receives (instructions + tests)
│   ├── basepoint-solution.hz    # Reference solution(s)
│   └── basepoint-storyboard.md  # Step-by-step writing with probe output
├── [task-name]/
│   ├── [task]-sketch.hz
│   ├── [task]-solution.hz
│   └── [task]-storyboard.md
└── ...
```

## Storyboard Format

Each task should have a storyboard showing what probe output looks like at each step of writing. This helps us:
1. Verify auto-probe behaves as expected
2. Identify the key "aha" moments where probes provide insight
3. Anticipate common mistakes and how probes reveal them

### Storyboard Template

```markdown
# [Task Name] Storyboard

## Setup
- Tests provided: [list]
- Functions provided: [list]
- Initial sketch: [code]

## Writing Steps

### Step 1: [Description]

**User writes:**
```hazel
let f = fun x ->
  let y = some_fn(x) in
  ?
```

**Probe output (auto-probe on `f`):**
```
let y = some_fn(x)     ≡ [value1] ⫽ [value2] ⫽ [value3]
```

**Insight**: [What the user learns from this output]

### Step 2: [Description]
...

## Common Mistake Paths

### Mistake A: [Description]
**User writes:**
[incorrect code]

**Probe output:**
[what they see - should reveal the mistake]

**How probe helps:**
[explanation of how the output indicates the error]
```

## Task Ideas

### Tiny Tasks

| Name | Description | Error Pattern | Key Function(s) |
|------|-------------|---------------|-----------------|
| base_route | Extract base path segment from URL | Parameter order | `string_split`, `nth` |
| word_count | Count words in string | Parameter order, off-by-one | `string_split`, `length` |
| initials | Get first letter of each word | List construction | `string_split`, `map`, `string_sub` |
| clamp | Constrain number to range | Condition boundary | comparisons, `if` |
| safe_head | Get first element or default | Pattern match | `case`, list patterns |
| sum_positive | Sum only positive numbers | Fold accumulator | `filter` or `fold_left` |
| repeat_string | Repeat string n times | Fold accumulator | `fold_left`, `string_concat` |
| last_element | Get last element of list | Fold logic | `fold_left` |

### Small Tasks

| Name | Description | Error Patterns | Notes |
|------|-------------|----------------|-------|
| csv_parse | Parse simple CSV row | Parameter order, list construction | String split + map |
| running_sum | Compute running totals | Fold accumulator, list construction | `[1,2,3] -> [1,3,6]` |
| validate_password | Check password rules | Condition logic, string operations | Multiple conditions |
| group_by_sign | Partition numbers into pos/neg/zero | Pattern match, tuple construction | Three-way split |
| find_max_index | Find index of maximum element | Fold with tuple accumulator | Track both value and index |
| interleave | Interleave two lists | List construction, pattern match | `[1,2], [a,b] -> [1,a,2,b]` |

### Modification Tasks

| Name | Base Program | Modification | Notes |
|------|--------------|--------------|-------|
| emojipaint-fill | emojipaint.hz | Add "fill all" action | Add case to pattern match |
| calculator-mod | calculator.hz | Add modulo operator | Extend tokenizer + evaluator |
| counter-undo | Simple counter MVU | Add undo functionality | Add history to model |

## Participant Considerations

From thesis proposal: Participants will be screened for functional programming familiarity but won't know Hazel-specific syntax. A separate tutorial will introduce:
- Basic Hazel syntax
- Probe usage (manual and auto-probe)
- Test-driven workflow

**Implications for task design**:
- Avoid exotic Hazel features (stick to `let`, `fun`, `if`, `case`, basic types)
- Provide syntax hints in task instructions when needed
- Keep programs short enough to read quickly
- Use meaningful variable/function names

## Research on FP Beginner Mistakes

Academic research on beginner mistakes in functional programming (see [Understanding beginners' mistakes with Haskell](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/understanding-beginners-mistakes-with-haskell/244DB6807F3BD77E14CE7D627514D6D3)) identifies common issues:

- **Type errors from wrong function application**: Using wrong library function, applying to wrong argument types
- **Parentheses confusion**: Missing or extra parens changing parse
- **Syntax unfamiliarity**: The "weird" syntax of FP languages is a major hurdle
- **Immutability adjustment**: Thinking in terms of mutation rather than transformation

For Hazel specifically:
- No `let rec` (recursion via type annotation) is unusual
- Comment syntax `# ... #` is non-standard
- Reversed argument order vs OCaml (`map(list, fn)` not `map fn list`)

## Potential Downsides to Monitor

When writing storyboards, note if any of these might arise:

1. **Information overload**: Too many values visible at once
2. **Reduced planning**: Users may rely on trial-and-error instead of thinking first
3. **Distraction**: Values updating constantly while typing
4. **False confidence**: Seeing "correct" values for test cases doesn't guarantee correct logic

## Next Steps

1. Flesh out `basepoint/` with proper sketch, solution, and storyboard files
2. Implement 2-3 more tiny tasks with full storyboards
3. Implement 1-2 small tasks
4. Implement 1 modification task
5. Validate probe output matches storyboard expectations using CLI: `./hazel probe --many file.hz`
6. Iterate on scaffolding levels based on how tasks "feel"

## CLI Commands for Development

```bash
# Run program
./hazel run program.hz

# Check types
./hazel analyze program.hz

# Run tests
./hazel test program.hz

# See probe output (single sample per probe)
./hazel probe program.hz

# See probe output (multiple samples)
./hazel probe --many program.hz

# Read from stdin for quick experiments
echo 'let x = 5 in ^^probe(x + 1)' | ./hazel probe -
```

Probe output format:
- `≡` separates expression from value
- `⫽` separates multiple samples (with `--many`)
- `∅` means probe never executed

### Future: CLI Auto-Probe Mode (not yet implemented)

A `--auto` flag is planned that will auto-probe all expressions without needing `^^probe()` wrappers. See `cli-autoprobe-proposal.md` for details. Once implemented:

```bash
./hazel probe --auto program.hz        # Auto-probe everything
./hazel probe --auto --many program.hz # With multiple samples
```

## Line Breaks and Auto-Probe Placement

**Important for agents/developers writing study programs:**

Auto-probe uses a "one probe per line" heuristic - it probes the **terminal expression** on each line. This means **where you insert line breaks determines what intermediate values you see**.

### Test Formatting for Maximum Feedback

The `test expr end` form returns **unit**, not the boolean result. To see both the actual computed value AND the test result, format tests with strategic line breaks:

**Minimal feedback** (one line):
```hazel
test clamp(5, 0, 10) == 5 end
```
Only shows the test form itself (returns `()`).

**Maximum feedback** (line breaks expose values):
```hazel
test clamp(5, 0, 10)
  == 5
end
```

This exposes THREE values:
1. Line 1: `clamp(5, 0, 10)` → actual result (e.g., `5` or wrong value)
2. Line 2: `== 5` → comparison result (`true`/`false`)
3. Line 3: test is recorded

**Recommended test format for study programs:**
```hazel
test
  function_call(args)
  == expected_value
end
```

This format:
- Shows what the function actually returned
- Shows whether it matched the expected value
- Works well with auto-probe for iterative development

### General Principle for Incremental Writing

When writing programs incrementally with auto-probe:
- Each line shows one probe value
- Put expressions you want to inspect at the end of their own lines
- Use let bindings on separate lines to see intermediate values:

```hazel
let base_route = fun path ->
  let parts = string_split("/", path) in    # ← probed: shows split result
  nth(parts, 1)                              # ← probed: shows final result
in
```

vs. cramming onto one line:
```hazel
let base_route = fun path -> nth(string_split("/", path), 1) in
```
(Only one probe for the whole body)
