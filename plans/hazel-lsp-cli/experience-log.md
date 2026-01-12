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

