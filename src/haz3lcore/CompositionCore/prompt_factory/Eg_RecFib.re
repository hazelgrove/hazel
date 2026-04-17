let self = {|
The following demonstrates the ideal workflow pattern for implementing a user request.
Study this example carefully — it shows HOW to think, plan, and act.

---

### Example: User asks "Implement a fibonacci function and test it"

**Starting program state:**
```
?
```

**Step 1 — Plan.** (Illustrates the workbench on a **multi-step feature**; skip this entirely for small/trivial edits.) Create a task with meaningful milestones (not micro-steps).

Tool call: `create_new_task`
```json
{
  "task": {
    "title": "Implement fibonacci with tests",
    "description": "Implement a recursive fibonacci function and add test cases to verify correctness.",
    "subtasks": [
      {
        "title": "Implement fibonacci function",
        "description": "Define a recursive fib function: fib(0)=0, fib(1)=1, fib(n)=fib(n-1)+fib(n-2)."
      },
      {
        "title": "Add test cases",
        "description": "Add tests for base cases and a few recursive cases."
      }
    ]
  }
}
```

**Step 2 — Implement.** The program is empty (`?`), so use `insert_before` (or `insert_after`) with **no** `path` to seed the first binding. In real tasks, prefer a **minimal** first insert and then `insert_*` / `update_*` for the rest; this walkthrough uses one larger insert only to keep the transcript short.

Tool call: `insert_before`
```json
{
  "code": "let fib : Int -> Int = fun n ->\n  if n <= 0\n    then 0\n    else if n == 1\n      then 1\n      else fib(n - 1) + fib(n - 2)\nin"
}
```

Agent sees the updated program (via context):
```
let fib : Int -> Int = fun n ->
  if n <= 0
    then 0
    else if n == 1
      then 1
      else fib(n - 1) + fib(n - 2)
in
?
```
No static errors. Mark first subtask complete:
Tool call: `mark_active_subtask_complete`
```json
{ "summary": "Implemented recursive fibonacci: fib(0)=0, fib(1)=1, fib(n)=fib(n-1)+fib(n-2)." }
```

**Step 3 — Add tests.** Update the body of `fib` (the `?` after `in`) with test expressions.

Tool call: `update_body`
```json
{
  "path": "fib",
  "code": "test fib(0) == 0 end;\ntest fib(1) == 1 end;\ntest fib(5) == 5 end;\ntest fib(10) == 55 end;\nfib(10)"
}
```

Agent sees:
```
let fib : Int -> Int = fun n ->
  if n <= 0
    then 0
    else if n == 1
      then 1
      else fib(n - 1) + fib(n - 2)
in
test fib(0) == 0 end;
test fib(1) == 1 end;
test fib(5) == 5 end;
test fib(10) == 55 end;
fib(10)
```
No static errors. All tests pass. Mark subtask and task complete:
Tool call: `mark_active_subtask_complete`
```json
{ "summary": "Added 4 test cases covering base cases (0, 1) and recursive cases (5, 10)." }
```
Tool call: `mark_active_task_complete`
```json
{ "summary": "Implemented recursive fibonacci function with type annotation and 4 passing test cases." }
```

**Final message to user:** "The `fib` function is implemented and all 4 test cases pass. It handles base cases (0 and 1) and recursively computes fib(n) = fib(n-1) + fib(n-2) for larger values."

---

### Example: User asks "Add a helper function that computes fib for a list of numbers"

**Starting program state:**
```
let fib : Int -> Int =
  ⋱
in
test fib(0) == 0 end;
test fib(1) == 1 end;
test fib(5) == 5 end;
test fib(10) == 55 end;
fib(10)
```

Note: `fib` is collapsed (⋱). The agent needs to understand what `fib` does but doesn't need to see its implementation — the tests provide enough context.

**Step 1 — Plan.**

Tool call: `create_new_task`
```json
{
  "task": {
    "title": "Add map_fib helper",
    "description": "Add a function that maps fib over a list of integers, plus test cases.",
    "subtasks": [
      {
        "title": "Implement map_fib",
        "description": "Insert a map_fib function after fib that applies fib to each element of an Int list."
      },
      {
        "title": "Update program output with tests",
        "description": "Replace the current program body with tests for map_fib and a demo call."
      }
    ]
  }
}
```

**Step 2 — Insert the new function.** Use `insert_after` to add a binding after `fib`.

Tool call: `insert_after`
```json
{
  "path": "fib",
  "code": "let map_fib : [Int] -> [Int] = fun ns ->\n  case ns\n  | [] => []\n  | hd :: tl => fib(hd) :: map_fib(tl)\n  end\nin"
}
```

**Step 3 — Update the body** to include tests for the new function.

Tool call: `update_body`
```json
{
  "path": "map_fib",
  "code": "test map_fib([]) == [] end;\ntest map_fib([0, 1, 5]) == [0, 1, 5] end;\nmap_fib([1, 2, 3, 4, 5])"
}
```

No static errors.

**Step 4 — Verify with probes.** Use probes to confirm the output is correct.

Tool call: `place_probe`
```json
{ "paths": ["map_fib"] }
```

Agent sees (with probe results):
```
let fib : Int -> Int =
  ⋱
in
let map_fib : [Int] -> [Int] = fun ns ->
  case ns
  | [] => []                                   ≡ []
  | hd :: tl => fib(hd) :: map_fib(tl)        ≡ [1, 1, 2, 3, 5]
  end
in
test map_fib([]) == [] end;
test map_fib([0, 1, 5]) == [0, 1, 5] end;
map_fib([1, 2, 3, 4, 5])                      ≡ [1, 1, 2, 3, 5]
```

The `≡` values confirm `map_fib` works correctly. Clean up the probe and mark complete:
Tool call: `remove_probe` → `mark_active_subtask_complete` → `mark_active_task_complete`

**Final message to user:** "Added `map_fib` which maps the fibonacci function over a list. Verified with probes and 2 test cases."

---

### Key Patterns to Notice

1. **Plan when it helps**: For big, multi-turn work, use coarse workbench milestones; for quick edits, skip tasks and edit directly.
2. **Use the right tool**: on an empty scratchpad, use `insert_after`/`insert_before` with **no** `path` to seed the first binding (keep it small when possible); path-based `insert_after`/`insert_before` for adding bindings, `update_definition`/`update_body` for modifying existing code — prefer **many small** edits over one huge paste.
3. **Manage context**: Collapsed definitions (⋱) don't need to be expanded if tests or context already explain what they do. Expand only what you need.
4. **Verify with probes**: After implementing, use `place_probe` to observe runtime values and confirm correctness. Clean up with `remove_probe` when done.
5. **Check static errors**: After each edit, check the context for type errors. Fix them before moving on.
6. **Communicate clearly**: Tell the user what was done and why, without referencing tool internals.
|};
