open Sexplib.Std;
open Haz3lcore;

/* TODO(andrew): document

     To start a run:
     1. reset model to init
     (assume for now list stdlib not required; otherwise will need to populate more slides)
     2. go to sketch slide (can be first slide for now)
     3. select all and delete


     start test:
     -1: add a new entry to test_reports
     0. go to sketch slide (can be first slide for now)
     1. select all and delete
     2. paste in sketch (or insert segment as zipper manually to save time)
     3. move caret to immediately after first ??
     4. schedule_action(PerformAction(Select(Term(Current))));
         schedule_action(Assistant(Prompt(Filler)));
     NOTE: will want to parameterize Filler action
     5. after 5, other actions will get scheduled; we
     need the last to trigger End test

     end test:
     1. Accept Completion
     2. collate errors
     3. add errors to test_reports

   */

[@deriving (show({with_path: false}), yojson, sexp)]
type reports('report) = VarMap.t_('report);

[@deriving (show({with_path: false}), sexp, yojson)]
type test_results = list(TestStatus.t);

[@deriving (show({with_path: false}), yojson, sexp)]
type updater =
  | Init(FillerOptions.t)
  | AddRoundOne(Settings.t, Ctx.t, Mode.t, OpenAI.reply)
  | AddRoundTwo(Settings.t, Ctx.t, Mode.t, OpenAI.reply)
  | Complete(option(test_results));

[@deriving (show({with_path: false}), sexp, yojson)]
type action('report) =
  | StartRun(unit)
  | StartTest(unit)
  | UpdateResult(string, updater)
  | LogTest(unit)
  | EndTest(unit);

//TODO(andrew): document
[@deriving (show({with_path: false}), yojson, sexp)]
type t('action, 'report) = {
  current_script: option(string),
  to_run: list((string, (FillerOptions.t, list('action)))),
  reports: reports('report),
};

let init: t('action, 'report) = {
  current_script: None,
  to_run: [],
  reports: VarMap.empty,
};

[@deriving (show({with_path: false}), yojson, sexp)]
type llm_report = {
  options: option(FillerOptions.t),
  time_start: option(float),
  time_end: option(float),
  first_round: option(Filler.round_report),
  second_round: option(Filler.round_report),
  tests: option(test_results),
};

[@deriving (show({with_path: false}), yojson, sexp)]
type llm_reports = VarMap.t_(llm_report);

[@deriving (show({with_path: false}), yojson, sexp)]
type final_report = {
  options: FillerOptions.t,
  time_elapsed: float,
  rounds: list(Filler.round_report),
  tests: test_results,
  num_rounds: int,
  parse_error: bool,
  num_static_errors: int,
  num_tests_passing: int,
  num_tests_failing: int,
  num_tests_indet: int,
  total_prompt_tokens: int,
};

[@deriving (show({with_path: false}), yojson, sexp)]
type final_status =
  | Ok(final_report)
  | Error(string);

[@deriving (show({with_path: false}), yojson, sexp)]
type final_statuses = VarMap.t_(final_status);

[@deriving (show({with_path: false}), yojson, sexp)]
let blank_llm_report = {
  options: None,
  time_start: None,
  time_end: None,
  first_round: None,
  second_round: None,
  tests: None,
};

let init_llm_report = (options, _) => {
  options: Some(options),
  time_start: Some(Sys.time()),
  time_end: None,
  first_round: None,
  second_round: None,
  tests: None,
};

let add_first_round_results = (first_round, report: llm_report) => {
  ...report,
  first_round: Some(first_round),
};

let add_second_round_results = (second_round, report: llm_report) => {
  ...report,
  second_round: Some(second_round),
};

let complete_llm_reports = (tests, report: llm_report) => {
  ...report,
  tests,
  time_end: Some(Sys.time()),
};

let final_report =
    (
      {time_start, time_end, first_round, second_round, tests, options, _}: llm_report,
    )
    : final_status => {
  switch (time_start, time_end, tests, options) {
  | (Some(time_start), Some(time_end), Some(tests), Some(options)) =>
    //NOTE: head is last round
    let rounds =
      switch (first_round, second_round) {
      | (None, _) => []
      | (Some(first), None) => [first]
      | (Some(first), Some(second)) => [second, first]
      };
    let total_prompt_tokens =
      switch (first_round, second_round) {
      | (None, _) => 0
      | (Some(first), None) => first.reply.usage.prompt_tokens
      | (Some(first), Some(second)) =>
        first.reply.usage.prompt_tokens + second.reply.usage.prompt_tokens
      };
    let num_rounds = List.length(rounds);
    switch (rounds) {
    | [] => Error("Incomplete report: No rounds recorded")
    | [final_round, ..._] =>
      let (parse_error, num_static_errors) =
        switch (final_round.error_report) {
        | NoErrors => (false, 0)
        | ParseError(_) => (true, 0)
        | StaticErrors(errors) => (false, errors |> List.length)
        };
      Ok({
        options,
        rounds,
        parse_error,
        total_prompt_tokens,
        num_rounds,
        num_static_errors,
        num_tests_passing:
          tests |> List.filter((==)(TestStatus.Pass)) |> List.length,
        num_tests_failing:
          tests |> List.filter((==)(TestStatus.Fail)) |> List.length,
        num_tests_indet:
          tests |> List.filter((==)(TestStatus.Indet)) |> List.length,
        time_elapsed: time_end -. time_start,
        tests,
      });
    };
  | _ => Error("Incomplete report")
  };
};
