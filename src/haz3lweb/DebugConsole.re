open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let print = ({settings, editors, _}: Model.t, key: string): unit => {
  let {state: {zipper, meta, _}, _}: Editor.t = Editors.get_editor(editors);
  let term = meta.statics.term;
  let map = meta.statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" =>
    zipper
    |> Zipper.unselect_and_zip
    |> ((seg: Segment.t(Uuidm.t)) => [%derive.show: Segment.t(Id.t)](seg))
    |> print
  | "F3" => term |> [%derive.show: UExp.t(list(Id.t))] |> print
  | "F4" => map |> Statics.Map.show |> print
  | "F5" =>
    let env = Editors.get_env_init(~settings, editors);
    Interface.elaborate(~settings=settings.core, map, term)
    |> Interface.evaluate(~settings=settings.core, ~env)
    |> ProgramResult.show
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
