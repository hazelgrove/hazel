open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  init_ctx: Ctx.t,
  check: LSActions.check,
  data: LSActions.data,
};

let get_or_init_zip = (~db, s: option(string)) =>
  switch (s) {
  | None => Zipper.init()
  | Some(prelude) => LSFiles.process_zipper(~db, prelude)
  };

let get_zips = (data: LSActions.data, ~db): list(Zipper.t) => {
  let common_term = get_or_init_zip(~db, data.common);
  let prelude_term = get_or_init_zip(~db, data.prelude);
  let epilogue_term = get_or_init_zip(~db, data.epilogue);
  let main_term = LSFiles.get_zipper(~db, data.program, data.new_token);
  [common_term, prelude_term, main_term, epilogue_term];
};

let splice_terms = (terms: list(TermBase.UExp.t)): TermBase.UExp.t =>
  List.fold_left(
    EditorUtil.append_exp,
    Term.UExp.{ids: [Id.mk()], term: Triv},
    terms,
  );

let zs_to_term = (zs: list(Zipper.t)): TermBase.UExp.t =>
  zs |> List.map(MakeTerm.from_zip_for_sem) |> List.map(fst) |> splice_terms;

let mk_combined_term = (settings, ~db) =>
  settings.data |> get_zips(~db) |> zs_to_term;

let syntax_error_report = (~db, data: LSActions.data) =>
  switch (
    try(Some(LSFiles.get_zipper(~db, data.program, data.new_token))) {
    | _ => None
    }
  ) {
  | None =>
    print_endline(
      "LS: Check Syntax: Incorrect syntax: Unknown exception in parse",
    )
  | Some(z) =>
    //TODO(andrew): look for holes in the syntax, Invalids, etc.
    switch (Printer.of_backpack(z)) {
    | [] => print_endline("LSP: Check Syntax: Syntax OK")
    | orphans =>
      print_endline(
        "LS: Check Syntax: Incorrect syntax: Unmatched delimiters:"
        ++ String.concat(", ", orphans),
      )
    }
  };

let get_info_map = (~init_ctx, z: Zipper.t) =>
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx);

let get_static_errors = (~db, ~settings): list(string) => {
  let term = mk_combined_term(settings, ~db);
  let info_map =
    Interface.Statics.mk_map_ctx(CoreSettings.on, settings.init_ctx, term);
  ErrorPrint.collect_static(info_map);
};

let static_error_report = (~db, ~settings: settings) =>
  switch (get_static_errors(~db, ~settings)) {
  | [] => print_endline("LS: Check Statics: No static errors")
  | errs =>
    let num_errs = errs |> List.length |> string_of_int;
    print_endline(
      "LS: Check Statics: " ++ num_errs ++ " static error(s) found: ",
    );
    print_endline(errs |> String.concat("\n"));
  };

let eval = (~init_ctx, combined_term) => {
  let info_map =
    Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx, combined_term);
  combined_term
  |> Interface.elaborate(~settings=CoreSettings.on, info_map)
  |> Interface.evaluate(~settings=CoreSettings.on, ~env=Builtins.env_init);
};

let test_results = (res: ProgramResult.t): list(TestStatus.t) =>
  res
  |> ProgramResult.get_state
  |> EvaluatorState.get_tests
  |> List.map(((_id, instance_report)) =>
       switch (instance_report) {
       /* Assumes tests not in function literal */
       | [] => TestStatus.Indet
       | [(_, status, _), ..._] => status
       }
     );

type test_counts = {
  total: int,
  pass: int,
  fail: int,
  indet: int,
};

let collate_test_counts = (res: list(TestStatus.t)): test_counts => {
  total: res |> List.length,
  pass: res |> List.filter((==)(TestStatus.Pass)) |> List.length,
  fail: res |> List.filter((==)(TestStatus.Fail)) |> List.length,
  indet: res |> List.filter((==)(TestStatus.Indet)) |> List.length,
};

let test_combined = (settings, ~db) =>
  mk_combined_term(settings, ~db)
  |> eval(~init_ctx=settings.init_ctx)
  |> test_results;

let dynamic_error_report = (~db, ~settings) =>
  switch (test_combined(settings, ~db)) {
  | [] => print_endline("LS: Check Dynamics: No tests found")
  | results =>
    print_endline("LS: Check Dynamics: Test results:");
    print_endline(
      String.concat("\n", results |> List.map(TestStatus.to_string)),
    );
  };

let go = (~db, ~settings): unit =>
  switch (settings.check) {
  | Syntax => syntax_error_report(~db, settings.data)
  | Static => static_error_report(~db, ~settings)
  | Dynamic => dynamic_error_report(~db, ~settings)
  };
