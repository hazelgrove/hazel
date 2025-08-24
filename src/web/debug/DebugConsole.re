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
  | "F8" =>
    print_endline("--------------------------------");
    // let _measured_old =
    //   Util.TimeUtil.measure_time("MeasuredOld.of_segment", true, () =>
    //     for (_i in 0 to 190) {
    //       MeasuredOld.of_segment(
    //         editor.editor.syntax.segment,
    //         editor.editor.syntax.shape_map,
    //       )
    //       |> ignore;
    //     }
    //   );
    let _measured =
      Util.TimeUtil.measure_time("Measured.of_segment", true, () =>
        for (_i in 0 to 190) {
          Measured.of_segment(
            editor.editor.syntax.segment,
            editor.editor.syntax.shape_map,
          )
          |> ignore;
        }
      );
    print_endline("--------------------------------");
    ();
  | "F9" =>
    print_endline("piece rows");
    editor.editor.syntax.measured.piece_rows
    // sort by index
    |> List.rev
    |> List.iteri((row, seg: Segment.t) => {
         print_endline(
           "row: "
           ++ string_of_int(row)
           ++ " pieces: "
           ++ Printer.of_segment(List.rev(seg)),
         )
       });
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
