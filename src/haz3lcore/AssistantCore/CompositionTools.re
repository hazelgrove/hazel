open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(API.Json.t);

let tools = [
  NavTools.go_to_parent,
  NavTools.go_to_child,
  NavTools.go_to_sibling,
  EditTools.update_definition,
  EditTools.update_body,
  EditTools.update_pattern,
  EditTools.update_binding_clause,
  EditTools.delete_binding_clause,
  EditTools.delete_body,
  EditTools.insert_after,
  EditTools.insert_before,
  //ViewTools.view_definition,
];

type action = Action.action;

let action_of = (~tool_name: string, ~args: Maps.StringMap.t(string)): action => {
  /* Possible arguments */
  /* Parsing here to avoid redundancy */
  /* Argument(s) may or may not be provided depending on the tool called */
  let name = Maps.StringMap.find_opt("name", args);
  let index =
    Option.map(int_of_string, Maps.StringMap.find_opt("index", args));
  let code = Maps.StringMap.find_opt("code", args);

  switch (tool_name) {
  | "go_to_parent" => Nav(GoToParent)
  | "go_to_child" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the child you wish to navigate to",
          ),
        )
      };
    Nav(GoToChild(name, index));
  | "go_to_sibling" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the sibling you wish to navigate to",
          ),
        )
      };
    Nav(GoToSibling(NameAndIdx(name, index)));
  | "view_definition" => Read(ViewDefinition)
  | "update_definition" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the definition you wish to update",
          ),
        )
      };
    Edit(UpdateDefinition(code));
  | "update_body" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure("You must specify a code for the body you wish to update"),
        )
      };
    Edit(UpdateBody(code));
  | "update_pattern" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the pattern you wish to update",
          ),
        )
      };
    Edit(UpdatePattern(code));
  | "update_binding_clause" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to update",
          ),
        )
      };
    Edit(UpdateBindingClause(code));
  | "insert_after" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert after",
          ),
        )
      };
    Edit(InsertAfter(code));
  | "insert_before" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert before",
          ),
        )
      };
    Edit(InsertBefore(code));
  | "delete_binding_clause" => Edit(DeleteBindingClause)
  | "delete_body" => Edit(DeleteBody)
  | _ => Nav(GoToParent) // default fallback
  };
};

let string_of = (action: action) => {
  switch (action) {
  | Nav(GoToParent) => "go_to_parent"
  | Nav(GoToChild(name, index)) =>
    "go_to_child(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Nav(GoToSibling(NameAndIdx(name, index))) =>
    "go_to_sibling(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Nav(GoToSibling(Stepwise(d))) =>
    "go_to_sibling("
    ++ (
      switch (d) {
      | Left => "Prev"
      | Right => "Next"
      }
    )
    ++ ")"
  | Read(ViewDefinition) => "view_definition"
  | Edit(UpdateDefinition(code)) => "update_definition(\"" ++ code ++ "\")"
  | Edit(UpdateBody(code)) => "update_body(\"" ++ code ++ "\")"
  | Edit(UpdatePattern(code)) => "update_pattern(\"" ++ code ++ "\")"
  | Edit(UpdateBindingClause(code)) =>
    "update_binding_clause(\"" ++ code ++ "\")"
  | Edit(DeleteBindingClause) => "delete_binding_clause"
  | Edit(DeleteBody) => "delete_body"
  | Edit(InsertAfter(code)) => "insert_after(\"" ++ code ++ "\")"
  | Edit(InsertBefore(code)) => "insert_before(\"" ++ code ++ "\")"
  };
};

// let derive_actions =
//     (z: Zipper.t, info_map: Statics.Map.t, action: action)
//     : (string, list(Action.t)) => {
//   let curr_node_info = AssistantTreeHelper.build_curr_node_info(z, info_map);
//   switch (curr_node_info) {
//   | None =>
//     // Special case: if the program is empty/no let/type alias exprs exist, we can only update the entire program.
//     switch (action) {
//     | Edit(UpdateBindingClause(code)) => (
//         "Your edits have been applied to the sketch.",
//         [Action.Select(All), Action.Paste(Assistant(code))],
//       )
//     | _ =>
//       raise(
//         Failure(
//           "No let or type alias expressions found in the program, unable to derive any meaningful AST information. Please call update_expression to initialize/update the entire program. Unable to apply any other actions.",
//         ),
//       )
//     }
//   | Some(curr_node_info) =>
//     switch (action) {
//     // Navigate to the parent node of the current node
//     | Nav(nav_action) =>
//       switch (nav_action) {
//       | GoToParent =>
//         switch (curr_node_info.parent) {
//         | None => raise(Failure("This node does not have a parent"))
//         | Some(parent) => (
//             "Cursor moved from \""
//             ++ curr_node_info.name
//             ++ "\" to its parent \""
//             ++ parent.name
//             ++ "\"",
//             [
//               Action.Select(
//                 Tile(Id(Info.id_of(parent.info), Direction.Right)),
//               ),
//             ],
//           )
//         }
//       | GoToChild(who, where) =>
//         // todo/idea: move candidates out here, maybe change indexing method?
//         // to assert referencing by both name and index...
//         // note: llms tend to be poor at logical/mathematical reasoning, and working with
//         //       numbers in general. Unfortunately, the nature of the indexing fallback
//         //       requires each variable to be unique, thus, I'd surmise that this pitfall
//         //       is unavoidable, nevertheless mitigatable via making the fallback method optional
//         // * applies to GoToSibling as well
//         let child =
//           switch (where) {
//           | None =>
//             // the llm provided no index, thus, use the name
//             let candidates =
//               List.filter(
//                 (child: AssistantTreeHelper.node) => child.name == who,
//                 curr_node_info.children,
//               );
//             if (List.length(candidates) > 1) {
//               raise(
//                 Failure(
//                   "Multiple children found, not sure how to resolve ambiguity. Please specify which child to reference via using the index associated with that child.",
//                 ),
//               );
//             };
//             switch (ListUtil.hd_opt(candidates)) {
//             | None =>
//               raise(
//                 Failure(
//                   "Child not found. Make sure the current node has children, and that the child you're referencing exists.",
//                 ),
//               )
//             | Some(child) => child
//             };
//           | Some(here) =>
//             // this means the llm provided an index to move to, in which case
//             // we default on using that as opposed to the name

//             switch (List.nth_opt(curr_node_info.children, here)) {
//             | None =>
//               raise(
//                 Failure(
//                   "Child index out of bounds. Make sure the current node has children, and that your given index is within bounds.",
//                 ),
//               )
//             | Some(child) => child
//             }
//           };
//         (
//           "Cursor moved from \""
//           ++ curr_node_info.name
//           ++ "\" to its child \""
//           ++ child.name
//           ++ "\"",
//           [
//             Action.Select(
//               Tile(Id(Info.id_of(child.info), Direction.Right)),
//             ),
//           ],
//         );
//       | GoToSibling(who, where) =>
//         let sibling =
//           switch (where) {
//           | None =>
//             let candidates =
//               List.filter(
//                 (sibling: AssistantTreeHelper.node) => sibling.name == who,
//                 curr_node_info.siblings,
//               );
//             if (List.length(candidates) > 1) {
//               raise(
//                 Failure(
//                   "Multiple siblings found, not sure how to resolve ambiguity. Please specify which sibling to reference via using the index associated with that sibling.",
//                 ),
//               );
//             };
//             switch (ListUtil.hd_opt(candidates)) {
//             | None =>
//               raise(
//                 Failure(
//                   "Sibling not found. Make sure the current node has siblings, and that the sibling you're referencing exists.",
//                 ),
//               )
//             | Some(sibling) => sibling
//             };
//           | Some(here) =>
//             switch (List.nth_opt(curr_node_info.siblings, here)) {
//             | None =>
//               raise(
//                 Failure(
//                   "Sibling index out of bounds. Make sure the current node has siblings, and that your given index is within bounds.",
//                 ),
//               )
//             | Some(sibling) => sibling
//             }
//           };
//         (
//           "Cursor moved from \""
//           ++ curr_node_info.name
//           ++ "\" to its sibling \""
//           ++ sibling.name
//           ++ "\"",
//           [
//             Action.Select(
//               Tile(Id(Info.id_of(sibling.info), Direction.Right)),
//             ),
//           ],
//         );
//       }
//     | Read(read_action) =>
//       switch (read_action) {
//       | ViewDefinition => (
//           "Definition of \""
//           ++ curr_node_info.name
//           ++ "\":\n```"
//           ++ Printer.of_segment(
//                ~holes="?",
//                ~special_folds=true,
//                CompositionUtil.View.definition(z, curr_node_info),
//              )
//           ++ "```",
//           [],
//         )
//       }
//     | Edit(action) =>
//       switch (action) {
//       | UpdateDefinition(code) =>
//         let target_id = get_inner_term_id(curr_node_info, Def);
//         (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(Tile(Id(target_id, Direction.Right))),
//             Action.Paste(Assistant(code)),
//           ],
//         );
//       | UpdateBody(code) =>
//         let target_id = get_inner_term_id(curr_node_info, Body);
//         (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(Tile(Id(target_id, Direction.Right))),
//             Action.Paste(Assistant(code)),
//           ],
//         );
//       | UpdatePattern(code) =>
//         let target_id = get_inner_term_id(curr_node_info, Pat);
//         (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(Tile(Id(target_id, Direction.Right))),
//             Action.Paste(Assistant(code)),
//           ],
//         );
//       | UpdateBindingClause(code) => (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(
//               Tile(Id(Info.id_of(curr_node_info.info), Direction.Right)),
//             ),
//             Action.Paste(Assistant(code)),
//           ],
//         )
//       | DeleteBindingClause => (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(
//               Tile(Id(Info.id_of(curr_node_info.info), Direction.Right)),
//             ),
//             Action.Destruct(Left),
//           ],
//         )
//       | DeleteBody =>
//         let target_id = get_inner_term_id(curr_node_info, Body);
//         (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Select(Tile(Id(target_id, Direction.Right))),
//             Action.Destruct(Left),
//           ],
//         );
//       | InsertBefore(code) => (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Move(Extreme(Left(ByToken))),
//             Action.Paste(Assistant(code)),
//           ],
//         )
//       | InsertAfter(code) => (
//           "Your edits have been applied to the sketch.",
//           [
//             Action.Move(Extreme(Right(ByToken))),
//             Action.Paste(Assistant(code)),
//           ],
//         )
//       }
//     }
//   };
// };
