open Util;

/* A benchmark task is a deterministic, scripted sequence of agent tool calls
   that can be executed without an LLM. Running one produces a normal chat
   (agent messages + tool results with before/after segments), so the replay
   step-through UI can navigate backward/forward through the steps with the
   Hazel editor state reset at each step. */

[@deriving (show({with_path: false}), sexp, yojson)]
type step = {
  narration: string, // shown as the agent message accompanying the tool call
  tool_name: string,
  args: API.Json.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  title: string,
  description: string, // shown as the seeding user message
  steps: list(step),
};

let str_arg = (key: string, value: string): (string, API.Json.t) => (
  key,
  `String(value),
);

let selector_step =
    (~narration: string, ~tool_name: string, ~selector: string, ~code=?, ())
    : step => {
  let args =
    switch (code) {
    | Some(code) =>
      `Assoc([str_arg("selector", selector), str_arg("code", code)])
    | None => `Assoc([str_arg("selector", selector)])
    };
  {
    narration,
    tool_name,
    args,
  };
};

/* Benchmark exercising selector-based edits:
   selector_update, overwrite (for insert-before/after), and selector_delete.
   Every selector below has a unique match, so cursor-relative match
   resolution is deterministic regardless of caret position. */
let selector_demo: t = {
  title: "Benchmark: Selector Tools",
  description: "Benchmark task (scripted, no LLM): build a small program using only selector-based edit tools. Each step below is a deterministic tool call. Use the replay controls to step backward and forward through the edit history; the editor is reset to the recorded program state at each step.",
  steps: [
    {
      narration: "Step 1/7: Initialize the program with a let binding and a body.",
      tool_name: "initialize",
      args: `Assoc([str_arg("code", "let x = 42 in x + 1")]),
    },
    selector_step(
      ~narration=
        "Step 2/7: Insert a helper binding `double` before `x` (overwrite).",
      ~tool_name="overwrite",
      ~selector="% let x",
      ~code="let double = fun n -> n * 2 in $",
      (),
    ),
    selector_step(
      ~narration=
        "Step 3/7: Replace the definition of `x` with a call to `double` (selector_update).",
      ~tool_name="selector_update",
      ~selector="let x = %",
      ~code="double(21)",
      (),
    ),
    selector_step(
      ~narration="Step 4/7: Insert a binding `y` after `x` (overwrite).",
      ~tool_name="overwrite",
      ~selector="let x = _ in %",
      ~code="let y = double(x) in $",
      (),
    ),
    selector_step(
      ~narration="Step 5/7: Update the definition of `y` (selector_update).",
      ~tool_name="selector_update",
      ~selector="let y = %",
      ~code="double(x) + 1",
      (),
    ),
    selector_step(
      ~narration=
        "Step 6/7: Delete the definition of `double`, leaving a hole (selector_delete).",
      ~tool_name="selector_delete",
      ~selector="let double = %",
      (),
    ),
    selector_step(
      ~narration=
        "Step 7/7: Re-fill `double` with a new implementation (selector_update).",
      ~tool_name="selector_update",
      ~selector="let double = %",
      ~code="fun n -> n + n",
      (),
    ),
  ],
};

/* Pair each scripted step with a synthetic OpenRouter tool call record so it
   flows through the same tool-result pipeline as real agent tool calls. */
let tool_calls_of =
    (benchmark: t): list((step, OpenRouter.Reply.Model.tool_call)) =>
  List.mapi(
    (i, step: step) => {
      let tool_call: OpenRouter.Reply.Model.tool_call = {
        id: "benchmark-" ++ string_of_int(i + 1),
        name: step.tool_name,
        args: step.args,
      };
      (step, tool_call);
    },
    benchmark.steps,
  );
