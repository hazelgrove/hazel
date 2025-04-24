open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

module BoundedDFS =
  Nondeterminism.Bounded(
    (val Nondeterminism.const_incr_config(~init=5, ~inc=5)),
  );
module BFS = Nondeterminism.BFS;
module DFS = Nondeterminism.DFS;
module SearchBoundedDFS = IndetEvaluator.Make(BoundedDFS);
module SearchBFS = IndetEvaluator.Make(Nondeterminism.BFS);
module SearchDFS = IndetEvaluator.Make(Nondeterminism.DFS);
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
  | "F3" => term |> Exp.show |> print
  | "F4" => map |> Statics.Map.show |> print
  | "F5" when settings.core.dynamics =>
    let env_init = Builtins.env_init;
    statics.elaborated
    |> Evaluator.evaluate(~env=env_init)
    |> fst
    |> DHExp.show
    |> print;
  | "F5" => print("Dynamics disabled, cannot show evaluation.")
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
    let results =
      statics.elaborated
      |> SearchBFS.values(
           ~env=Builtins.env_init,
           ~state=IndetEvaluatorState.init,
         );
    let _ =
      results
      |> BFS.run_n(~solutions=30)
      |> List.mapi((i, (state, d)) =>
           print(
             "---Result: "
             ++ Int.to_string(i)
             ++ "\n# of Instantiations: "
             ++ Int.to_string(IndetEvaluatorState.get_instantiations(state))
             ++ "\nTrace Length: "
             ++ Int.to_string(IndetEvaluatorState.get_trace_length(state))
             ++ "\n"
             ++ Exp.show(d)
             ++ "\n",
           )
         );
    ();
  | "F12" =>
    let inst =
      statics.elaborated
      |> Evaluator.evaluate(~env=Builtins.env_init)
      |> fst
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
