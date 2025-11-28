open Haz3lcore;
open Util;

let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics, _}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let info = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Language.Exp.show |> print
  | "F4" => info |> Language.Statics.Map.show |> print
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
      switch (Id.Map.find_opt(index, info)) {
      | Some(ci) => print(Language.Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F7" => ()
  | "F8" => ()
  | "F9" =>
    open HighLevelNodeMap.Public;
    let node_map = build(zipper, info);
    // Print all nodes and their paths
    switch (node_map) {
    | Some(node_map) =>
      node_map
      |> Id.Map.bindings
      |> List.iter(((id: Id.t, node: HighLevelNodeMap.node)) => {
           let path_str =
             node.path
             |> List.map((path_id: Id.t) => id_to_name(node_map, path_id))
             |> String.concat("/");
           print(
             "Node: "
             ++ node.name
             ++ " (id: "
             ++ Id.to_string(id)
             ++ ", path: "
             ++ path_str
             ++ ")",
           );
         })
    | None => print("DEBUG: No node map found")
    };
  | "F10" => ()
  | "F11" => ()
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
