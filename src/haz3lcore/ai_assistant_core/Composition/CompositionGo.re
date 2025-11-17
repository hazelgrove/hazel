open Util;
open AssistantTreeHelper.HighLevelNodeMap.Public;
open Language;

type node_map = AssistantTreeHelper.HighLevelNodeMap.t;
type node = AssistantTreeHelper.HighLevelNodeMap.node;

module Local = {
  type inner_term =
    | Pat
    | Def
    | Body;

  module Utils = {
    let get_inner_term_id = (inner_term: inner_term, node: node): Id.t => {
      /*
       Returns the specified "inner_term" from the "curr_node_info"

       E.g. If current node is "x" in a program "let x : Int = 2 + 3 in 100 + 200",
       calling get_inner_term_id(curr_node_info, Pat) will return the id of the pattern "x : Int",
       calling get_inner_term_id(curr_node_info, Def) will return the id of the definition "2 + 3",
       calling get_inner_term_id(curr_node_info, Body) will return the id of the body "100 + 200".
       */
      switch (node.info) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(pat, def, body) =>
          switch (inner_term) {
          | Pat => Pat.rep_id(pat)
          | Def => Exp.rep_id(def)
          | Body => Exp.rep_id(body)
          }
        | TyAlias(tpat, tdef, body) =>
          switch (inner_term) {
          | Pat => TPat.rep_id(tpat)
          | Def => Typ.rep_id(tdef)
          | Body => Exp.rep_id(body)
          }
        | _ =>
          raise(
            Failure(
              "UNIMPLEMENTED_NODE_TYPE: Only let and type alias expressions are currently supported as nodes",
            ),
          )
        }
      | _ =>
        raise(
          Failure(
            "Current node is not a let or type alias expression, so no pattern to update",
          ),
        )
      };
    };
  };

  module PerformUtils = {
    let static_error_check =
        (
          ~old_z: option(Zipper.t), // optional arg -- if not provided, assume no errors existed
          ~old_node: option(node),
          ~new_z: Zipper.t,
          ~new_node: node,
          ~mk_statics: Zipper.t => StaticsBase.Map.t,
          ~of_pat: bool,
          ~of_def: bool,
          ~of_body: bool,
        ) => {
      /*
       A localized static error check to ensure that newly inserted segments do not introduce any errors.

       This is a localized check, as obligations occuring elsewhere in the program are inevitable for
       many types of edits.

       of_pat, of_def, and of_body are used to specify which parts of the program to check for errors.
       */
      let old_errors =
        switch (old_z, old_node) {
        | (None, _)
        | (_, None) => []
        | (Some(old_z), Some(old_node)) =>
          let old_info_map = mk_statics(old_z);
          let old_subtree =
            GeneralTreeUtils.subtree_of(
              ~info=old_node.info,
              ~orig_info_map=old_info_map,
              ~of_pat,
              ~of_def,
              ~of_body,
            );
          ErrorPrint.all(old_subtree);
        };
      let new_info_map = mk_statics(new_z);
      let new_subtree =
        GeneralTreeUtils.subtree_of(
          ~info=new_node.info,
          ~orig_info_map=new_info_map,
          ~of_pat,
          ~of_def,
          ~of_body,
        );
      let new_errors = ErrorPrint.all(new_subtree);
      if (List.length(new_errors) > List.length(old_errors)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Not applying the action you requested as it would have the following static error(s): "
            ++ String.concat(", ", new_errors),
          ),
        );
      } else {
        Ok(new_z);
      };
    };

    let statics_map_new_ids =
        (old_statics: StaticsBase.Map.t, new_statics: StaticsBase.Map.t) => {
      // Returns only the IDs of the new statics map that are not in the old statics map
      // This is useful to identify which new static information was added
      Id.Map.fold(
        (id, _info, acc) =>
          // Check if the ID exists in the old statics map
          switch (StaticsBase.Map.lookup(id, old_statics)) {
          | Some(_) => acc // ID exists in old map, don't include it
          | None => [id, ...acc] // ID doesn't exist in old map, include it
          },
        new_statics,
        [],
      );
    };

    let introduce =
        (
          z: Zipper.t,
          code: string,
          return:
            (Action.Failure.t, option(Zipper.t)) =>
            result(Zipper.t, Action.Failure.t),
        ) => {
      // A wrapper function for trying to paste code into the zipper
      // Note that we paste a segment; so, we convert the string to a segment
      // first, and then insert the segment into the zipper. This helps to
      // avoid potential current buggy parsing issues.
      Parser.to_segment(code)
      |> OptUtil.and_then((segment: Segment.t) =>
           Some(Zipper.insert_segment(z, segment))
         )
      |> return(CantPaste);
    };

    let destruct =
        (
          ~defs_exclude_bodies: bool,
          z: Zipper.t,
          target_id: Id.t,
          syntax: CachedSyntax.t,
        ) => {
      switch (
        Select.term(
          ~defs_exclude_bodies,
          ~case_rules=false,
          syntax.term_data,
          target_id,
          z,
        )
      ) {
      | Some(z') =>
        switch (Destruct.go(Left, z')) {
        | None => Error(Action.Failure.Cant_destruct)
        | Some(z'') => Ok(z'')
        }
      | None => Error(Action.Failure.Cant_select)
      };
    };

    let overwrite_term =
        (
          z: Zipper.t,
          target_id: Id.t,
          code: string,
          defs_exclude_bodies: bool,
          syntax: CachedSyntax.t,
          return:
            (Action.Failure.t, option(Zipper.t)) =>
            result(Zipper.t, Action.Failure.t),
        ) => {
      // Select the respective term (in this case the definition term)
      switch (
        Select.term(
          ~defs_exclude_bodies,
          ~case_rules=false,
          syntax.term_data, // todo: not sure about this arg
          target_id,
          z,
        )
      ) {
      | Some(z') =>
        // Paste the code over the selected tile
        introduce(z', code, return)
      | None => Error(Action.Failure.Cant_select)
      };
    };
    let insert_term =
        (
          z: Zipper.t,
          target_id: Id.t,
          code: string,
          d: Direction.t,
          syntax: CachedSyntax.t,
          return:
            (Action.Failure.t, option(Zipper.t)) =>
            result(Zipper.t, Action.Failure.t),
        ) => {
      switch (
        // ' let a = 0 in'
        Select.term(
          ~defs_exclude_bodies=true,
          ~case_rules=false,
          syntax.term_data, // todo: not sure about this arg, is it right?
          target_id,
          z,
        )
      ) {
      | Some(z') =>
        switch (Move.by_token(d, z')) {
        | Some(z'') => introduce(z'', code, return)
        | None => Error(Action.Failure.Cant_move)
        }
      | None => Error(Action.Failure.Cant_select)
      };
    };
  };

  let initialize_dispatch =
      (
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
        code: string,
      ) => {
    switch (PerformUtils.introduce(Select.all(z), code, return)) {
    | Ok(new_z) =>
      let new_statics = mk_statics(new_z);
      // For initialization, check the entire program for errors
      let new_errors = ErrorPrint.all(new_statics);
      if (List.length(new_errors) > 0) {
        Error(
          Action.Failure.Composition_action_failure(
            "Not applying the action you requested as it would have the following static error(s): "
            ++ String.concat(", ", new_errors),
          ),
        );
      } else {
        Ok(new_z);
      };
    | Error(e) => Error(e)
    };
  };

  let view_dispatch = (a: CompositionActions.view_action, z: Zipper.t) => {
    switch (a) {
    | Expand(paths) =>
      let z' = {
        ...z,
        agent_view: AgentState.add_paths(paths, z.agent_view),
      };
      Ok(z');
    | Collapse(paths) =>
      let z' = {
        ...z,
        agent_view: AgentState.remove_paths(paths, z.agent_view),
      };
      Ok(z');
    };
  };

  // Tempory wrapper that helps me localize myself while implementing (remove)
  let composition_dispatch =
      (
        a: CompositionActions.composition_action,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
        node_map: option(node_map),
      ) => {
    switch (node_map) {
    | None =>
      switch (a) {
      | Edit(Initialize(code)) =>
        initialize_dispatch(z, mk_statics, return, code)
      | _ => Error(Action.Failure.Cant_derive_local_AST_information)
      }
    | Some(node_map) =>
      switch (a) {
      | View(a) => view_dispatch(a, z)
      | Read(ShowUseSites(_path))
      | Read(ShowReferences(_path)) => Ok(z) // TODO: Implement
      | Edit(e) =>
        switch (e) {
        | UpdateDefinition(path, code) =>
          print_endline("here #16, path is: " ++ path);
          let node = path_to_node(node_map, path);
          print_endline("here #17, node is: " ++ node.name);
          let target_id = Utils.get_inner_term_id(Def, node);
          switch (
            PerformUtils.overwrite_term(
              z,
              target_id,
              code,
              false,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | UpdateBody(path, code) =>
          let node = path_to_node(node_map, path);
          let target_id = Utils.get_inner_term_id(Body, node);
          switch (
            PerformUtils.overwrite_term(
              z,
              target_id,
              code,
              false,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | UpdatePattern(path, code) =>
          let node = path_to_node(node_map, path);
          let target_id = Utils.get_inner_term_id(Pat, node);
          switch (
            PerformUtils.overwrite_term(
              z,
              target_id,
              code,
              false,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | UpdateBindingClause(path, code) =>
          let target_id = path_to_id(node_map, path);
          switch (
            PerformUtils.overwrite_term(
              z,
              target_id,
              code,
              true,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | InsertBefore(path, code) =>
          // todo: figure out a better method than magic space
          let target_id = path_to_id(node_map, path);
          switch (
            {
              PerformUtils.insert_term(
                z,
                target_id,
                code ++ " ",
                Direction.Left,
                syntax,
                return,
              );
            }
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | InsertAfter(path, code) =>
          // todo: figure out a better method than magic space
          let target_id = path_to_id(node_map, path);
          switch (
            PerformUtils.insert_term(
              z,
              target_id,
              " " ++ code,
              Direction.Right,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) => Ok(new_z)
          };
        | DeleteBindingClause(path) =>
          let target_id = path_to_id(node_map, path);
          PerformUtils.destruct(
            ~defs_exclude_bodies=true,
            z,
            target_id,
            syntax,
          );
        | DeleteBody(path) =>
          let node = path_to_node(node_map, path);
          let target_id = Utils.get_inner_term_id(Body, node);
          PerformUtils.destruct(
            ~defs_exclude_bodies=false,
            z,
            target_id,
            syntax,
          );
        | Initialize(_) =>
          Error(
            Action.Failure.Composition_action_failure(
              "Once a program has let/type alias expressions, you can never use initialize on it ever again.",
            ),
          )
        }
      }
    };
  };

  let get_initial_cursor_position = (z: Zipper.t, info_map: Id.Map.t(Info.t)) => {
    switch (Indicated.ci_of(z, info_map)) {
    | Some(ci) => Info.id_of(ci)
    | None => raise(Failure("No indicated piece found"))
    };
  };

  let reposition_cursor = (z: Zipper.t, target_id: Id.t) => {
    switch (Move.jump_to_id_indicated(z, target_id)) {
    | Some(z) => Ok(z)
    | None => Error(Action.Failure.Cant_move)
    };
  };

  let freshen_paths =
      (z: Zipper.t, mk_statics: Zipper.t => StaticsBase.Map.t)
      : result(Zipper.t, Action.Failure.t) => {
    // This function removes any stale paths from the agent view list
    // This can happen if variables are changed or deleted from the editor itself
    let node_map = build(z, mk_statics(z));
    switch (node_map) {
    | None =>
      Ok({
        ...z,
        agent_view: AgentState.init,
      })
    | Some(node_map) =>
      Ok({
        ...z,
        agent_view: {
          expanded_paths:
            List.filter(
              (path: string) => {
                switch (
                  Id.Map.find_opt(path_to_id(node_map, path), node_map)
                ) {
                | Some(_) => true
                | None => false
                }
              },
              z.agent_view.expanded_paths,
            ),
        },
      })
    };
  };

  let go = (~syntax, ~z, ~a, ~mk_statics, ~return, ~schedule_tool_response) => {
    let node_map = build(z, mk_statics(z));

    let res =
      switch (
        composition_dispatch(a, syntax, z, mk_statics, return, node_map)
      ) {
      | Ok(new_z) =>
        // TODO: Add repositioning of cursor here. ONLY possible if we have a separate editor state for the agent.
        switch (freshen_paths(new_z, mk_statics)) {
        | Ok(new_z) => Ok(new_z)
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      };

    //todo: handle res and use schedule_assistant_action to send the result to the assistant and loop
    switch (schedule_tool_response) {
    | Some(schedule_tool_response) =>
      switch (res) {
      | Ok(_) =>
        schedule_tool_response(
          AssistantUpdateAction.Success(
            "Action has been applied to the editor -- TODO: make more informative",
          ),
        )
      | Error(Composition_action_failure(e)) =>
        schedule_tool_response(AssistantUpdateAction.Failure(e))
      | _ =>
        schedule_tool_response(
          AssistantUpdateAction.Failure(
            "An error occured when applying your changes to the editor",
          ),
        )
      }
    | None =>
      // Composition action was not sourced from an ai assistant tool call,
      // just return the action performed on the editor (no feedback to send)
      ()
    };
    res;
  };
};
module Public = {
  let go = (~syntax, ~z, ~a, ~mk_statics, ~return, ~schedule_tool_response) => {
    Local.go(~syntax, ~z, ~a, ~mk_statics, ~return, ~schedule_tool_response);
  };
};
