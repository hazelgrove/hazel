/* The few-shot example, as data: its replies' tool calls and the views
   between them print here, and Test_AgentReply replays the replies in
   order, checking each view as shown */

open ToolExamples;

type step = {
  /* what the step does, before its calls */
  says: string,
  /* one reply's calls */
  calls: list(call),
  /* the agent's view after the reply, and the entries it has open */
  sees: option((string, list(string))),
};

type example = {
  asks: string,
  program: string,
  /* the agent's view of it, when shown in place of its text */
  start: option(string),
  steps: list(step),
  /* the final message to the user */
  final: string,
};

let task = (title, description, subtasks) => {
  tool: "create_new_task",
  args: [
    (
      "task",
      `Assoc([
        ("title", `String(title)),
        ("description", `String(description)),
        (
          "subtasks",
          `List(
            List.map(
              ((t, d)) =>
                `Assoc([
                  ("title", `String(t)),
                  ("description", `String(d)),
                ]),
              subtasks,
            ),
          ),
        ),
      ]),
    ),
  ],
};

let fib = {|let fib(n: Int): Int =
  if n <= 0
    then 0
    else if n == 1
      then 1
      else fib(n - 1) + fib(n - 2)|};

let fib_tests = {|hint "base cases" test
  fib(0) == 0 && fib(1) == 1
end;
hint "recursive cases" test
  fib(5) == 5 && fib(10) == 55
end|};

let map_fib = {|let map_fib(ns: [Int]): [Int] =
  case ns
  | [] => []
  | hd :: tl => fib(hd) :: map_fib(tl)
  end|};

let fib_example = {
  asks: "Implement a fibonacci function and test it",
  program: "?",
  start: None,
  steps: [
    {
      says: "**Step 1 — Plan.** (The workbench suits a multi-step feature; skip it for small edits.) Coarse milestones, not micro-steps:",
      calls: [
        task(
          "Implement fibonacci with tests",
          "A recursive fibonacci function with tests.",
          [
            (
              "Implement fibonacci function",
              "fib(0)=0, fib(1)=1, fib(n)=fib(n-1)+fib(n-2).",
            ),
            ("Add test cases", "Base cases and a few recursive ones."),
          ],
        ),
      ],
      sees: None,
    },
    {
      says: "**Step 2 — Implement.** The program is empty, so the function goes first in the top level:",
      calls: [
        call("insert", [("first_in", "/"), ("code", fib)]),
        call(
          "mark_active_subtask_complete",
          [("summary", "Implemented recursive fib.")],
        ),
      ],
      sees: None,
    },
    {
      says: "**Step 3 — Test it** at the end, above the result, and make its value the result:",
      calls: [
        call("insert", [("last_in", "/"), ("code", fib_tests)]),
        call("replace", [("target", "=>"), ("code", "fib(10)")]),
        call(
          "mark_active_subtask_complete",
          [("summary", "2 tests: base cases, and fib(5), fib(10).")],
        ),
        call(
          "mark_active_task_complete",
          [("summary", "fib, with 2 passing tests.")],
        ),
      ],
      /* the next example starts from its view */
      sees: None,
    },
  ],
  final: "`fib` is implemented and its 2 tests pass: the base cases 0 and 1, and fib(5), fib(10).",
};

let map_fib_example = {
  asks: "Add a helper that computes fib for a list of numbers",
  program: fib ++ "\nin\n\n" ++ fib_tests ++ ";\n\nfib(10)",
  start:
    Some(
      "#› program map · no errors · 2 of 2 tests pass #\n"
      ++ CompositionView.legend
      ++ {|
let fib(n: Int): Int = ⋯ in  #› 7 uses #

hint "base cases" test ⋯ end;  #› ✓ #
hint "recursive cases" test ⋯ end;  #› ✓ #

fib(10)  #› ⇒ 55 #|},
    ),
  steps: [
    {
      says: "`fib` is not open: its signature and tests say enough. **Add the helper** after `fib`, its tests with the others at the end, and make it the result. One reply:",
      calls: [
        call("insert", [("after", "fib"), ("code", map_fib)]),
        call(
          "insert",
          [
            ("last_in", "/"),
            (
              "code",
              "hint \"empty list\" test map_fib([]) == [] end;\nhint \"three values\" test\n  map_fib([0, 1, 5]) == [0, 1, 5]\nend",
            ),
          ],
        ),
        call(
          "replace",
          [("target", "=>"), ("code", "map_fib([1, 2, 3, 4, 5])")],
        ),
      ],
      sees:
        Some((
          "#› program map · no errors · 4 of 4 tests pass #\n"
          ++ CompositionView.legend
          ++ {|
let fib(n: Int): Int = ⋯ in  #› 7 uses #

let map_fib(ns: [Int]): [Int] = ⋯ in  #› 4 uses #

hint "base cases" test ⋯ end;  #› ✓ #
hint "recursive cases" test ⋯ end;  #› ✓ #
hint "empty list" test map_fib([]) == [] end;  #› ✓ #
hint "three values" test ⋯ end;  #› ✓ #

map_fib([1, 2, 3, 4, 5])  #› ⇒ [1, 1, 2, 3, 5] #|},
          [],
        )),
    },
  ],
  final: "Added `map_fib`, which maps fib over a list; its 2 tests pass.",
};

let examples = [fib_example, map_fib_example];

let example_text = (e: example): string =>
  "### Example: User asks \""
  ++ e.asks
  ++ "\"\n\n**Starting program:**\n```\n"
  ++ Option.value(e.start, ~default=e.program)
  ++ "\n```\n\n"
  ++ String.concat(
       "\n\n",
       List.map(
         s =>
           s.says
           ++ "\n\n"
           ++ String.concat("\n", List.map(call_text, s.calls))
           ++ (
             switch (s.sees) {
             | Some((view, _)) =>
               "\n\nThe agent then sees:\n```\n" ++ view ++ "\n```"
             | None => ""
             }
           ),
         e.steps,
       ),
     )
  ++ "\n\n**Final message to user:** \""
  ++ e.final
  ++ "\"";

let self =
  String.concat("\n\n---\n\n", List.map(example_text, examples))
  ++ {|

---

### Key patterns

1. **Plan when it helps**: coarse workbench milestones for multi-turn work; none for quick edits.
2. **Edit the smallest part**: replace `f:definition`, or `f/=>` past f's local definitions, rather than the whole entry.
3. **One reply, one change**: calls that belong together go in one reply; they apply together or not at all.
4. **Open only what you need**: signatures, tests and the map's annotations often say enough.
5. **Rename and move with their tools**: `rename` updates every use; `move` keeps code, probes and identity. Don't delete and re-insert.
6. **Check**: new behavior gets tests; read the errors and test results after each reply; probe to debug.
7. **Communicate**: tell the user what was done, without naming tools.|};
