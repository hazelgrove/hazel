open Haz3lcore;

open Sexplib.Std;

/* Live evaluation of the editor contents. Interactively, evaluation
   runs in a forked worker process (EvalWorker) so the UI never blocks;
   the step limit bounds how long a doomed program burns background CPU
   before reporting. Replay/tests evaluate synchronously. */

[@deriving (show({with_path: false}), sexp)]
type t =
  | Pending
  | EvalOk(string)
  | EvalErr(string)
  | TimedOut;

let step_limit = 100_000_000;

/* Result-printing recipe duplicated from src/CLI/Print.re (CLI modules
   belong to another executable so they can't be linked from here).
   TODO(tui): extract into a shared haz3lcore helper. */
let exp_to_segment_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  project_tables: false,
};

let print = (exp: Language.Exp.t): string =>
  Printer.of_segment(
    ~holes="?",
    ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, exp),
  );

let no_tests: Language.TestResults.t = Language.TestResults.mk_results([]);

/* Evaluate the program, collecting probe samples for the
   statics-computed targets and `test ... end` results. Returns the
   displayable result, the sample map (Dynamics.Map.t is Sample.Map.t),
   and the test results. */
let run =
    (statics: CachedStatics.t)
    : (t, Language.Dynamics.Map.t, Language.TestResults.t) =>
  switch (
    Language.Evaluator.evaluate_and_limit(
      ~step_limit,
      ~targets=statics.targets,
      ~env=Language.Builtins.env_init,
      statics.elaborated,
    )
  ) {
  | exception (Language.EvaluatorError.Exception(reason)) => (
      EvalErr(Language.EvaluatorError.show(reason)),
      Language.Dynamics.Map.empty,
      no_tests,
    )
  | exception exn => (
      EvalErr(Printexc.to_string(exn)),
      Language.Dynamics.Map.empty,
      no_tests,
    )
  | StepLimitExceeded => (TimedOut, Language.Dynamics.Map.empty, no_tests)
  | Completed((result, state)) =>
    let dynamics =
      Language.Dynamics.Map.mk(Language.EvaluatorState.get_probes(state));
    let tests =
      switch (
        Language.TestResults.mk_results(
          Language.EvaluatorState.get_tests(state),
        )
      ) {
      | tests => tests
      | exception _ => no_tests
      };
    switch (print(result)) {
    | exception exn => (
        EvalErr("print failed: " ++ Printexc.to_string(exn)),
        dynamics,
        tests,
      )
    | text => (EvalOk(text), dynamics, tests)
    };
  };

/* Render the result pane: a dim separator line then up to [height - 1]
   lines of result text. */
let rows = (~width: int, ~height: int, result: t): list(Frame.row) =>
  if (height <= 0) {
    [];
  } else {
    let sep_text = " result ";
    let dashes = n => List.init(max(n, 0), _ => "─") |> String.concat("");
    let sep = [
      (Theme.pane_title, dashes(2) ++ sep_text),
      (Theme.pane_title, dashes(width - 2 - String.length(sep_text))),
    ];
    let (style, text) =
      switch (result) {
      | Pending => (Theme.pane_title, "...")
      | EvalOk(text) => (Theme.result_ok, text)
      | EvalErr(text) => (Theme.result_err, text)
      | TimedOut => (Theme.result_err, "<step limit exceeded>")
      };
    let lines =
      String.split_on_char('\n', text)
      |> Util.ListUtil.take(height - 1)
      |> List.map(line => [(style, line)]);
    [sep, ...lines];
  };

/* How many frame rows the pane wants for this result (max 5) */
let wanted_height = (result: t): int =>
  switch (result) {
  | Pending
  | TimedOut => 2
  | EvalOk(text)
  | EvalErr(text) =>
    min(5, 1 + List.length(String.split_on_char('\n', text)))
  };
