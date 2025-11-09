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
  | "F8" =>
    open AssistantTreeHelper.HighLevelNode;
    let node_info = build(zipper, info);
    switch (node_info) {
    | Some(node_info) =>
      print("Success!");
      let node_map = node_info.node_map;
      // print each item in the map, just their name and path
      node_map
      |> Id.Map.bindings
      |> List.iter(
           ((id: Id.t, node: AssistantTreeHelper.HighLevelNode.node)) => {
           let path_str =
             node.path |> List.map(Id.to_string) |> String.concat(" -> ");
           print(
             "Node: "
             ++ node.name
             ++ " (id: "
             ++ Id.to_string(id)
             ++ ", path: "
             ++ path_str
             ++ ")",
           );
         });
    | None => print("Failed to derive full definition")
    };

  | "F9" =>
    open AssistantTreeHelper.HighLevelNode.Public;
    let node_info = build(zipper, info);
    switch (node_info) {
    | Some(node_info) =>
      print("=== TREE PRINTING TESTS ===");
      let node_map = node_info.node_map;
      let root_node = current_of(node_info);

      print("\n1. Basic Tree Structure:");
      print("----------------------");
      print(print_tree(node_map, root_node));

      print("\n2. Tree with Full Paths:");
      print("------------------------");
      print(print_tree_with_paths(node_map, root_node));

      print("\n3. Tree with Level/Sibling Indices:");
      print("-----------------------------------");
      print(print_tree_with_indices(node_map, root_node));

      print("\n4. Tree Navigation Tests:");
      print("------------------------");

      // Test finding nodes by level and sibling index
      let descendants = descendants_of(node_map, root_node);
      print("Descendants by level:");
      List.iteri(
        (level, level_nodes) => {
          print("Level " ++ string_of_int(level) ++ ":");
          List.iteri(
            (sibling_idx, node_id) => {
              let node = find(node_map, node_id);
              print(
                "  L"
                ++ string_of_int(level)
                ++ "S"
                ++ string_of_int(sibling_idx)
                ++ ": "
                ++ node.name,
              );
            },
            level_nodes,
          );
        },
        descendants,
      );

      print("\n5. Sibling Navigation Tests:");
      print("----------------------------");
      let all_nodes = node_map |> Id.Map.bindings |> List.map(snd);

      List.iter(
        node => {
          let siblings = siblings_of(node_map, node);
          let sibling_count = List.length(siblings);
          if (sibling_count > 0) {
            print(
              "Node '"
              ++ node.name
              ++ "' has "
              ++ string_of_int(sibling_count)
              ++ " siblings:",
            );
            List.iteri(
              (idx: int, sibling: AssistantTreeHelper.HighLevelNode.node) => {
                let marker = idx == node.sibling_idx ? " <-- CURRENT" : "";
                print(
                  "  ["
                  ++ string_of_int(idx)
                  ++ "] "
                  ++ sibling.name
                  ++ marker,
                );
              },
              siblings,
            );
          } else {
            print("Node '" ++ node.name ++ "' has no siblings");
          };
        },
        all_nodes,
      );

      print("\n6. Parent-Child Relationships:");
      print("------------------------------");
      List.iter(
        node => {
          switch (parent_of(node_map, node)) {
          | Some(parent) =>
            print(
              "Node '" ++ node.name ++ "' is child of '" ++ parent.name ++ "'",
            )
          | None => print("Node '" ++ node.name ++ "' is a root node")
          }
        },
        all_nodes,
      );

      print("\n=== END TREE TESTS ===");
    | None => print("Failed to build tree - cannot run tree printing tests")
    };
  | "F10" => ()
  | "F11" => ()
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
