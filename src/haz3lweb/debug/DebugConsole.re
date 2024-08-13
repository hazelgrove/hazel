open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let print = (~settings: Settings.t, editor: Editor.t, key: string): unit => {
  let {state: {zipper, meta, _}, _}: Editor.t = editor;
  let term = meta.statics.term;
  let map = meta.statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> UExp.show |> print
  | "F4" => map |> Statics.Map.show |> print
  | "F5" =>
    let env_init = Builtins.env_init;
    Interface.elaborate(~settings=settings.core, map, term)
    |> Interface.eval_term(~settings=settings.core, ~env_init)
    |> ProgramResult.show(ProgramResult.pp_inner)
    |> print;
  | "F6" =>
    let index = Indicated.index(zipper);
    switch (index) {
    | Some(index) =>
      print("id:" ++ Id.to_string(index));
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
