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
            Id.Map.empty,
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
  | "F10" =>
    print_endline("--------------------------------");
    let _ =
      Util.TimeUtil.measure_time("OLD code", true, () =>
        for (_i in 0 to 100) {
          Code.view(
            ~measured=editor.editor.syntax.measured,
            ~settings=Settings.Model.init,
            ~shape_map=editor.editor.syntax.shape_map,
            ~font_metrics=FontMetrics.init,
            ~term_data=Id.Map.empty,
            ~buffer_ids=[],
            ~refractor_shape_map=Id.Map.empty,
            editor.editor.syntax.segment,
          )
          |> ignore;
        }
      );
    // let _ =
    //   Util.TimeUtil.measure_time("NEW code", true, () =>
    //     for (_i in 0 to 100) {
    //       CodeNew.view(
    //         ~measured=editor.editor.syntax.measured,
    //         ~settings=Settings.Model.init,
    //         ~shape_map=editor.editor.syntax.shape_map,
    //         ~font_metrics=FontMetrics.init,
    //         ~term_data=Id.Map.empty,
    //         ~buffer_ids=[],
    //         editor.editor.syntax.segment,
    //       )
    //       |> ignore;
    //     }
    //   );
    print_endline("--------------------------------");
    ();
  | "F11" =>
    let _ = {
      let pad3 = (n: int): string => {
        let s = string_of_int(n);
        let len = String.length(s);
        if (len >= 3) {
          s;
        } else {
          String.make(3 - len, '0') ++ s;
        };
      };

      let term_data = editor.editor.syntax.term_data;
      let measured = editor.editor.syntax.measured;
      let get_term_rows = TermData.get_term_rows(_, term_data, measured);
      // let get_terminal_term_ids =
      //   TermData.get_terminal_term_ids(_, term_data, measured);
      let get_largest_terminal_term_ids =
        TermData.get_largest_terminal_term_ids(_, term_data, measured);
      open Util.OptUtil.Syntax;

      //let piece_rows = measured.piece_rows |> List.rev;

      let* indicated_id = Indicated.index(zipper);
      let* (_, term_rows) = get_term_rows(indicated_id);
      let* terminal_term_ids = get_largest_terminal_term_ids(indicated_id);
      print_endline("--------------------------------");
      List.iteri(
        (row, seg: Segment.t) => {
          print_endline(pad3(row) ++ "|  " ++ Printer.of_segment(seg))
        },
        term_rows,
      );
      print_endline("--------------------------------");
      List.iteri(
        (row_index, id: option(Id.t)) => {
          print_endline(
            pad3(row_index)
            ++ "|  "
            ++ (
              switch (id) {
              | Some(id) =>
                switch (TermData.segment(id, editor.editor.syntax.term_data)) {
                | Some(seg) => Printer.of_segment(seg)
                | None => "None"
                }
              | None => "None"
              }
            ),
          )
        },
        terminal_term_ids,
      );
      print_endline("--------------------------------");
      Some();
    };
    ();
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
