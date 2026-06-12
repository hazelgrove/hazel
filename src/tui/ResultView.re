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
  /* A result that parses as a table: plain text (shown when the pane is
     too short for a grid), headers, and pre-formatted cell text,
     rendered as a box-drawing grid by [rows] */
  | EvalTable(string, list(string), list(list(string)))
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

/* A result that is itself a table displays as a box-drawing grid
   (the TUI counterpart of the web's projected result tables) */
let table_data =
    (exp: Language.Exp.t): option((list(string), list(list(string)))) =>
  switch (TableProj.table_of(Exp(Language.DHExp.strip_ascriptions(exp)))) {
  | Some((headers, data)) =>
    Some((headers, List.map(List.map(TermProjector.table_cell), data)))
  | None => None
  | exception _ => None
  };

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
    /* residual Projector nodes (e.g. in unevaluated closure bodies)
       would print as their ^^table(...)-style triggers */
    let result = Language.Exp.strip_projectors(result);
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
    | text =>
      let view =
        switch (table_data(result)) {
        | Some((headers, cells)) => EvalTable(text, headers, cells)
        | None => EvalOk(text)
        };
      (view, dynamics, tests);
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
    let text_lines = (style, text) =>
      String.split_on_char('\n', text) |> List.map(line => [(style, line)]);
    let lines =
      switch (result) {
      | Pending => text_lines(Theme.pane_title, "...")
      | EvalOk(text) => text_lines(Theme.result_ok, text)
      | EvalErr(text) => text_lines(Theme.result_err, text)
      | TimedOut => text_lines(Theme.result_err, "<step limit exceeded>")
      /* too short for a grid (chrome alone takes 4 lines): plain text */
      | EvalTable(text, _, _) when height < 6 =>
        text_lines(Theme.result_ok, text)
      | EvalTable(_, headers, cells) =>
        /* data rows beyond the room left collapse into a ⋯ row */
        TermProjector.table_rows(
          ~max_data=max(1, height - 5),
          headers,
          cells,
        )
        |> List.map(Frame.clip_row(_, ~col_off=0, ~width))
      };
    [sep, ...Util.ListUtil.take(height - 1, lines)];
  };

/* How many frame rows the pane gets for this result on a [rows]-tall
   screen: at most a third of the screen; within that, up to 5 for text
   and up to the threshold-capped grid for tables. A table whose grid
   can't fit displays as text (see [rows] above) and is sized as text. */
let wanted_height = (~rows: int, result: t): int => {
  let cap = rows / 3;
  let text_height = text =>
    min(5, 1 + List.length(String.split_on_char('\n', text)));
  switch (result) {
  | Pending
  | TimedOut => min(2, cap)
  | EvalTable(text, _, cells) =>
    let grid =
      min(TableProj.scroll_threshold_rows + 5, 5 + List.length(cells));
    min(grid, cap) < 6 ? min(text_height(text), cap) : min(grid, cap);
  | EvalOk(text)
  | EvalErr(text) => min(text_height(text), cap)
  };
};
