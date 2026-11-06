/* Work counts for the performance report (.github/workflows/perf-report.yml,
   docs/performance.md): the steps the evaluator's trampoline takes
   (Evaluator.evaluate_counting_work) on each program named on the command
   line, as JSON on stdout. A count, not a time: the same on every run, so
   the report can compare two builds program by program without noise.

   Programs are evaluated as Test_PerfBudgets evaluates them: statics and
   elaboration first (not counted), then a cold run with the eval info the
   editor's first run would have. Parsing follows
   Test_Evaluator_IncrCorpus.

   CI compiles this file against both the pull request and its merge base,
   so it should only use APIs that change rarely. If it stops compiling
   against an older base, the report says the work counts are unavailable
   for that base rather than failing.

   Usage: node perf_work.bc.js [--step-limit N] [--out FILE] FILE_OR_DIR...
   Directories are searched recursively for .hz files. */

open Haz3lcore;
open Language;

let rec find_hz = (path: string): list(string) =>
  switch (Sys.is_directory(path)) {
  | true =>
    Sys.readdir(path)
    |> Array.to_list
    |> List.sort(compare)
    |> List.concat_map(entry => find_hz(Filename.concat(path, entry)))
  | false => Filename.check_suffix(path, ".hz") ? [path] : []
  | exception _ => []
  };

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

type outcome =
  | Steps(int)
  /* Did not finish within the step limit: likely a program that is meant
     to diverge or wait on input. Compared as a status, not a count. */
  | OverLimit
  | Unparsed
  | Failed(string);

let work_or_raise = (~step_limit: int, path: string): outcome =>
  switch (
    read_file(path)
    |> Util.StringUtil.trim_leading
    /* as a slide or the CLI loads it: the fast parser, falling back to
       typing the text in */
    |> PersistentZipper.parse_text(~source="perf_work", ~root=Exp)
  ) {
  | None => Unparsed
  | Some(z) =>
    let exp = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let (info_map, elab) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        exp,
      );
    let eval_info =
      EvalInfo.of_info_map(
        ~probe_all=CoreSettings.on.probe_all,
        ~targets=Id.Map.empty,
        info_map,
      );
    /* evaluate_counting_work has no limit, so a divergent program would
       hang the report: try it under one first. */
    switch (
      Evaluator.evaluate_and_limit(
        ~step_limit,
        ~eval_info,
        ~env=Builtins.env_init,
        elab,
      )
    ) {
    | StepLimitExceeded => OverLimit
    | LimitedCompleted(_) =>
      let (_, steps) =
        Evaluator.evaluate_counting_work(
          ~eval_info,
          ~env=Builtins.env_init,
          elab,
        );
      Steps(steps);
    };
  };

let work = (~step_limit: int, path: string): outcome =>
  try(work_or_raise(~step_limit, path)) {
  | e => Failed(Printexc.to_string(e))
  };

let json_of_outcome = (path: string, outcome: outcome): Yojson.Safe.t => {
  let (status, steps) =
    switch (outcome) {
    | Steps(n) => ("ok", `Int(n))
    | OverLimit => ("over-limit", `Null)
    | Unparsed => ("unparsed", `Null)
    | Failed(msg) => ("error: " ++ msg, `Null)
    };
  `Assoc([
    ("program", `String(path)),
    ("status", `String(status)),
    ("steps", steps),
  ]);
};

/* stdout is not ours alone (the slow parser logs to it), so the JSON goes
   to a file when one is given */
let write_json = (out: option(string), json: Yojson.Safe.t): unit =>
  switch (out) {
  | None => print_endline(Yojson.Safe.pretty_to_string(json))
  | Some(path) =>
    let oc = open_out_bin(path);
    output_string(oc, Yojson.Safe.pretty_to_string(json));
    close_out(oc);
  };

let () = {
  let rec parse_args = ((step_limit, out, paths)) =>
    fun
    | [] => (step_limit, out, List.rev(paths))
    | ["--step-limit", n, ...rest] =>
      parse_args((int_of_string(n), out, paths), rest)
    | ["--out", path, ...rest] =>
      parse_args((step_limit, Some(path), paths), rest)
    | [path, ...rest] =>
      parse_args((step_limit, out, [path, ...paths]), rest);
  let (step_limit, out, roots) =
    parse_args((20_000_000, None, []), List.tl(Array.to_list(Sys.argv)));
  let programs = List.concat_map(find_hz, roots);
  let results =
    List.map(
      path => {
        let outcome = work(~step_limit, path);
        Printf.eprintf(
          "%s: %s\n%!",
          path,
          switch (outcome) {
          | Steps(n) => string_of_int(n) ++ " steps"
          | OverLimit => "over the step limit"
          | Unparsed => "did not parse"
          | Failed(msg) => msg
          },
        );
        json_of_outcome(path, outcome);
      },
      programs,
    );
  write_json(
    out,
    `Assoc([
      ("step_limit", `Int(step_limit)),
      ("programs", `List(results)),
    ]),
  );
};
