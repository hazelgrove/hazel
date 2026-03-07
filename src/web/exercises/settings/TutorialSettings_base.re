let filename = "haz3l-demo";
let log_key = filename;

let live_typing_prefix = "**Before you begin**, turn on live typing using the settings menu at the top left.\n\n";

let rich_probes_prefix = "**Tip:** Feel free to use rich probes to help you solve this task.\n\n";

let strip_live_typing_prefix = (prompt: string): string =>
  if (String.starts_with(prompt, ~prefix=live_typing_prefix)) {
    String.sub(
      prompt,
      String.length(live_typing_prefix),
      String.length(prompt) - String.length(live_typing_prefix),
    );
  } else {
    prompt;
  };

let add_live_typing_prefix = (prompt: string): string =>
  if (String.starts_with(prompt, ~prefix=live_typing_prefix)) {
    prompt;
  } else {
    live_typing_prefix ++ prompt;
  };

let add_rich_probes_prefix = (prompt: string): string =>
  if (String.starts_with(prompt, ~prefix=rich_probes_prefix)) {
    prompt;
  } else {
    rich_probes_prefix ++ prompt;
  };

/* Treatment group selection */
type treatment =
  | A
  | B;

let get_treatment = (): treatment =>
  switch (
    Util.JsUtil.QueryParams.get_param("treatment")
    |> Option.map(String.uppercase_ascii)
  ) {
  | Some("B") => B
  | _ => A
  };

/* Shared preamble (items before the swapped section) */
let preamble: list(Tutorial.spec) = [
  Tu_Holes.exercise,
  Tu_TylrParser.exercise,
  Tu_Arithmetic.int_exercise,
  Tu_FloatingPointArithmetic.exercise,
  Tu_LetBindings.exercise,
  Tu_Probes.exercise,
  Tu_TypeAnnotations.exercise,
  Tu_Functions.exercise,
  Tu_FunctionMultiArg.exercise,
  Tu_Partial.exercise,
  Tu_Pipelines.exercise,
  Tu_IfExpressions.exercise,
  Tu_CaseExpressions.exercise,
  Tu_Lists.exercise,
  Tu_ListMap.exercise,
  Tu_ListFold.exercise,
  Ta_StringMean.exercise,
  Tu_LabeledTuples.exercise,
  Tu_Projection.exercise,
  Tu_TupleExtension.exercise,
  Tu_LabelOmission.exercise,
  Tu_TupleList.exercise,
  Tu_TableProjector.exercise,
  Tu_TableProjection.exercise,
  Ta_GradebookMean.exercise,
];

/* Select task pairs based on treatment */
let (rich_probes_pre, rich_probes_post) =
  switch (get_treatment()) {
  | A => (Ta_GradebookOverallGrade.exercise, Ta_TidyTerm.exercise)
  | B => (Ta_TidyTerm.exercise, Ta_GradebookOverallGrade.exercise)
  };

let (bug_id_pre, bug_id_post) =
  switch (get_treatment()) {
  | A => (Ta_BugIdentification1.exercise, Ta_BugIdentification2.exercise)
  | B => (Ta_BugIdentification2.exercise, Ta_BugIdentification1.exercise)
  };

/* Apply positional transformations and assemble */
let lessons: list(Tutorial.spec) =
  preamble
  @ [
    rich_probes_pre
    |> Tutorial.with_title("Task 3: " ++ rich_probes_pre.title)
    |> Tutorial.with_rich_probes(Some(false)),
    Tu_RichProbes.exercise,
    rich_probes_post
    |> Tutorial.with_title("Task 4: " ++ rich_probes_post.title)
    |> Tutorial.with_rich_probes(Some(true))
    |> Tutorial.with_prompt(add_rich_probes_prefix(rich_probes_post.prompt)),
    bug_id_pre
    |> Tutorial.with_title("Task 5: " ++ bug_id_pre.title ++ " 1")
    |> Tutorial.with_prompt(strip_live_typing_prefix(bug_id_pre.prompt)),
    Tu_LiveTyping.exercise,
    bug_id_post
    |> Tutorial.with_title("Task 6: " ++ bug_id_post.title ++ " 2")
    |> Tutorial.with_prompt(add_live_typing_prefix(bug_id_post.prompt)),
  ];
