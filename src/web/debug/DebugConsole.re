open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics, _}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let map = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Language.Exp.show |> print
  | "F4" => map |> Language.Statics.Map.show |> print
  | "F5" when settings.core.dynamics =>
    let env_init = Language.Builtins.env_init;
    statics.elaborated
    |> Language.Evaluator.evaluate(~env=env_init)
    |> fst
    |> Language.DHExp.show
    |> print;
  | "F5" => print("Dynamics disabled, cannot show evaluation.")
  | "F6" =>
    let index = Indicated.index(zipper);
    switch (index) {
    | Some(index) =>
      print("id:" ++ Id.to_string(index));
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Language.Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F7" => print(Haz3lcore.BuiltinsPrinter.builtin_value_signatures())
  | "F8" =>
    let collect_print_samples =
        ({probes, _}: Language.EvaluatorState.t): list(Language.Sample.t) =>
      Id.Map.fold(
        (_, samples, acc) =>
          List.fold_left(
            (acc, sample) =>
              sample.Language.Sample.origin == Language.Sample.Print
                ? [sample, ...acc] : acc,
            acc,
            samples,
          ),
        probes,
        [],
      );

    let collect_print_outputs =
        (state: Language.EvaluatorState.t): list(string) =>
      collect_print_samples(state)
      |> List.sort((a, b) =>
           Int.compare(a.Language.Sample.seq, b.Language.Sample.seq)
         )
      |> List.map(sample =>
           sample.Language.Sample.value
           |> ExpToSegment.exp_to_segment(
                ~settings=
                  ExpToSegment.Settings.of_core(
                    ~inline=true,
                    Language.CoreSettings.off,
                  ),
              )
           |> Printer.of_segment(~holes="")
         );

    let print_summary = (state: Language.EvaluatorState.t): option(string) =>
      switch (collect_print_outputs(state)) {
      | [] => None
      | outputs => Some(String.concat("\n", outputs))
      };

    let env_init = Language.Builtins.env_init;
    let res =
      statics.elaborated
      |> Language.Evaluator.evaluate(~env=env_init)
      |> snd
      |> print_summary;
    switch (res) {
    | Some(summary) => print(summary)
    | None => print("No print outputs")
    };
  | "F9" =>
    /* Print program with probes in text-only format */
    let env_init = Language.Builtins.env_init;
    let (_, state) =
      statics.elaborated |> Language.Evaluator.evaluate(~env=env_init);
    let probe_map = state.probes;
    let text =
      ProbeText.of_zipper(
        ~window=ProbeProj.Settings.s^.window,
        ~probe_map,
        zipper,
      );
    print("=== Program with Probes ===");
    print(text);
    print("===========================");
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
