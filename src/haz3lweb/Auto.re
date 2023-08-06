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
type action('report) =
  | StartRun(unit)
  | StartTest(unit)
  | UpdateResult(string, 'report => 'report)
  | LogTest(unit)
  | EndTest(unit);

//TODO(andrew): document
[@deriving (show({with_path: false}), yojson, sexp)]
type t('action, 'report) = {
  current_script: option(string),
  to_run: list((string, list('action))),
  reports: reports('report),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type test_results = list((KeywordID.t, TestStatus.t));

let init: t('action, 'report) = {
  current_script: None,
  to_run: [],
  reports: VarMap.empty,
};

[@deriving (show({with_path: false}), yojson, sexp)]
type llm_report = {
  time_start: option(float),
  time_end: option(float),
  completed_sketch: option(string),
  static_errors: option(list((Id.t, Info.error))),
  syntax_errors: option(list(string)),
  tests: option(test_results),
};

[@deriving (show({with_path: false}), yojson, sexp)]
type llm_reports = VarMap.t_(llm_report);

[@deriving (show({with_path: false}), yojson, sexp)]
type final_report = {
  time_elapsed: float,
  completed_sketch: string,
  num_syntax_errors: int,
  num_static_errors: int,
  num_tests_passing: int,
  static_errors: list((Id.t, Info.error)),
  syntax_errors: list(string),
  tests: test_results,
};

[@deriving (show({with_path: false}), yojson, sexp)]
type final_status =
  | Ok(final_report)
  | Error(string);

[@deriving (show({with_path: false}), yojson, sexp)]
type final_statuses = VarMap.t_(final_status);

[@deriving (show({with_path: false}), yojson, sexp)]
let blank_llm_report = {
  time_start: None,
  time_end: None,
  completed_sketch: None,
  static_errors: None,
  syntax_errors: None,
  tests: None,
};

let init_llm_report = _r => {
  time_start: Some(Sys.time()),
  time_end: None,
  completed_sketch: None,
  static_errors: None,
  syntax_errors: None,
  tests: None,
};

let complete_llm_reports =
    (
      tests,
      syntax_errors,
      static_errors,
      completed_sketch,
      report: llm_report,
    ) => {
  ...report,
  tests,
  syntax_errors: Some(syntax_errors),
  static_errors: Some(static_errors),
  completed_sketch: Some(completed_sketch),
  time_end: Some(Sys.time()),
};

let final_report =
    (
      {
        time_start,
        time_end,
        completed_sketch,
        static_errors,
        syntax_errors,
        tests,
      }: llm_report,
    )
    : final_status => {
  switch (
    time_start,
    time_end,
    completed_sketch,
    static_errors,
    syntax_errors,
    tests,
  ) {
  | (
      Some(time_start),
      Some(time_end),
      Some(completed_sketch),
      Some(static_errors),
      Some(syntax_errors),
      Some(tests),
    ) =>
    Ok({
      num_syntax_errors: syntax_errors |> List.length,
      num_static_errors: static_errors |> List.length,
      num_tests_passing:
        tests
        |> List.filter(((_, status)) => status == TestStatus.Pass)
        |> List.length,
      time_elapsed: time_end -. time_start,
      completed_sketch,
      static_errors,
      syntax_errors,
      tests,
    })
  | _ => Error("Incomplete report")
  };
};
