open Haz3lcore;
open Language;

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
    let info_map = editor.statics.info_map;
    let zipper = editor.editor.state.zipper;
    let cursor = Indicated.ci_of(zipper, info_map);
    switch (cursor) {
    | Some(ci) =>
      print_endline("Curr ID: " ++ Id.to_string(Info.id_of(ci)) ++ "\n");
      let ancestors = Info.ancestors_of(ci);
      List.iter(
        (ancestor: Uuidm.t) => {
          print_endline("Ancestor ID: " ++ Uuidm.to_string(ancestor))
        },
        ancestors,
      );
    | None => print("DEBUG: No cursor found")
    };
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
