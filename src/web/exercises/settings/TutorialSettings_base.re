let filename = "haz3l-demo";
let log_key = filename;

let live_typing_prefix = "**Before you begin**, turn on live typing using the settings menu at the top left.\n\n";

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

/* Group A: current order */
let tail_a: list(Tutorial.spec) = [
  Ta_GradebookOverallGrade.exercise,
  Tu_RichProbes.exercise,
  Ta_TidyTerm.exercise,
  Ta_BugIdentification1.exercise,
  Tu_LiveTyping.exercise,
  Ta_BugIdentification2.exercise,
];

/* Group B: swapped pairs with modified titles, settings, and prompts */
let tail_b: list(Tutorial.spec) = [
  Ta_TidyTerm.exercise
  |> Tutorial.with_title("Task 3: Gradebook Tidy Term")
  |> Tutorial.with_rich_probes(Some(false)),
  Tu_RichProbes.exercise,
  Ta_GradebookOverallGrade.exercise
  |> Tutorial.with_title("Task 4: Gradebook Overall Grade")
  |> Tutorial.with_rich_probes(Some(true)),
  Ta_BugIdentification2.exercise
  |> Tutorial.with_title("Task 5: Bug Identification 1")
  |> Tutorial.with_prompt(
       strip_live_typing_prefix(Ta_BugIdentification2.exercise.prompt),
     ),
  Tu_LiveTyping.exercise,
  Ta_BugIdentification1.exercise
  |> Tutorial.with_title("Task 6: Bug Identification 2")
  |> Tutorial.with_prompt(
       add_live_typing_prefix(Ta_BugIdentification1.exercise.prompt),
     ),
];

let lessons: list(Tutorial.spec) =
  switch (get_treatment()) {
  | A => preamble @ tail_a
  | B => preamble @ tail_b
  };
