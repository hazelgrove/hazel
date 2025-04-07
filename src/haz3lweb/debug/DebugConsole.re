open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

open IndetEvaluator.Make(Nondeterminism.DFS);
let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let map = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Exp.show |> print
  | "F4" => map |> Statics.Map.show |> print
  | "F5" =>
    let env_init = Builtins.env_init;
    statics.elaborated
    |> Evaluator.evaluate(~settings=settings.core, ~env=env_init)
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
  | "F8" => statics.elaborated |> Exp.show |> print
  | "F9" =>
    let results = statics.elaborated |> values(Builtins.env_init);
    let _ =
      results
      |> Nondeterminism.DFS.run_n(~solutions=30)
      |> List.mapi((i, d) =>
           print(
             "Instantiation "
             ++ Int.to_string(i)
             ++ ": "
             ++ Exp.show(d)
             ++ "\n",
           )
         );
    ();
  | "F12" =>
    let inst =
      statics.elaborated
      |> Evaluator.evaluate''(Builtins.env_init)
      |> RedexHoleType.find(Builtins.env_init);
    (
      switch (inst) {
      | None => "No Hole"
      | Hole(id) => "Hole with no cast"
      | HoleCast(id, slc) => "Cast Hole"
      | Match(_) => "Match Hole"
      }
    )
    |> print;
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
