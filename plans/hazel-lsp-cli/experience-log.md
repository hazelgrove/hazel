# AI Model Experience Log

This file is for AI models (Claude, etc.) to document their experiences using Hazel's CLI tooling. This helps identify pain points, missing features, and opportunities for improvement.

## How to Use This Log

When working with Hazel code, add an entry if you encounter:
- Confusing error messages
- Missing CLI features that would help
- Syntax that's unclear or error-prone
- Successful patterns worth documenting
- Suggestions for tooling improvements

## Log Format

```markdown
### YYYY-MM-DD: Brief title

**Context:** What were you trying to do?
**Experience:** What happened?
**Suggestion:** (Optional) How could this be improved?
```

---

## Entries

### 2025-01-11: Initial CLI tooling assessment

**Context:** Evaluating existing CLI commands for AI-assisted Hazel development.

**Experience:**
- `./hazel run -` works well for evaluating programs
- `./hazel format -` correctly shows where holes are inserted
- `./hazel analyze -` shows Rust-style errors with source context and carets
- `./hazel probe -` correctly shows probe values with `--many` flag for multiple samples

**Suggestion:** Future improvements could include:
- JSON output mode for programmatic consumption
- Combined analysis command (static + dynamic + tests in one call)
- Test results reporting

### 2025-01-11: Recursion syntax learning

**Context:** Writing recursive functions in Hazel.

**Experience:** Initially tried using `let rec` which doesn't exist in Hazel. The correct pattern is to use an arrow type annotation:
```hazel
# WRONG - Hazel has no let rec
let rec length = fun xs -> ...

# CORRECT - use arrow type annotation
let length : [Int] -> Int = fun xs -> ...
```

**Suggestion:** Error messages could potentially detect when a function calls itself without a type annotation and suggest adding one.

### 2025-01-11: Probe syntax

**Context:** Adding probes to inspect intermediate values.

**Experience:** The `^^probe(expr)` syntax works in text files. When running with `./hazel probe -`:
- Single value: Shows `≡ value` inline
- Multiple values (with `--many`): Shows `≡ v1 ⫽ v2 ⫽ v3`
- Empty: Shows `∅` (usually means probe_map wasn't built correctly)

**Suggestion:** ASCII-only mode would be helpful for environments that don't render Unicode well.

### 2025-01-12: Writing a 500-line study program (Tamagotchi)

**Context:** Creating a Tamagotchi-style virtual pet program (~500 lines) with comprehensive tests for the probe debugging user study.

**Experience:**

*Development approach:* Wrote the entire program in one pass after reading requirements and examples. This was overconfident - incremental development would have caught issues earlier.

*Type checking:* `./hazel analyze` passed immediately. No static errors because the program was structurally correct.

*Test failures:* `./hazel test` revealed 2 of 26 tests failing:
1. "age increases with time ticks" - expected age=5, got different
2. "well-cared-for toaster becomes golden" - got RustyToaster instead

*Debugging approach:* Used echo-pipe snippets to `./hazel run -` for targeted debugging:
```bash
echo 'range(0, 5)' | ./hazel run -
# Output: [0, 1, 2, 3, 4, 5]  -- 6 elements, not 5!
```

*Key discovery:* Hazel's `range(0, n)` is **inclusive on both ends**, returning n+1 elements. I assumed Python/OCaml semantics (exclusive end). This is a significant gotcha.

*Second bug:* The "golden toaster" test failed because I front-loaded care actions then let time pass. During 25 time ticks, stats decayed causing 22 neglect events - a death spiral. Fixed by interleaving care with time.

*What I didn't use:*
- Probes (`^^probe(...)`) - ironic for probe debugging research!
- `./hazel probe` command
- Incremental development

**Suggestion:**
1. Document `range` inclusivity prominently in hazel-primer.md (it's surprising)
2. A `./hazel repl` or watch mode would help iterative development
3. Consider adding `range_exclusive(start, end)` or clarifying the semantics

### 2026-01-12: Reserved name collision with `eval`

**Context:** Writing an expression evaluator for the calculator study program.

**Experience:** Named my evaluator function `eval` which silently conflicted with a builtin or reserved name. The program returned `?` instead of a value, with cryptic "Expected a constructor" errors pointing at call sites. Renaming to `evaluate` fixed the issue immediately.

**Suggestion:**
1. Better error when shadowing builtins - e.g., "warning: `eval` shadows builtin function"
2. Document reserved/builtin names prominently
3. The error message "Expected a constructor" was misleading - the actual issue was name shadowing

### 2026-01-12: Comment syntax gotcha - must close with `#`

**Context:** Adding explanatory comments to calculator code.

**Experience:** Used `# This is a comment` (single `#`) which caused parse failures. Hazel comments require `# ... #` syntax and cannot span multiple lines. Each line needs its own `# comment #`.

Correct:
```hazel
# This is a comment #
let x = 5 in x  # inline comment #
```

Wrong:
```hazel
# This will break
let x = 5 in x

# Multi-line
   comment #
```

**Suggestion:** Updated hazel-primer.md with this information. Error message could be clearer about unclosed comments.

