open Util;
open HighLevelNodeMap.Public;
open Language;

type node_map = HighLevelNodeMap.t;
type node = HighLevelNodeMap.node;

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
    let edit_action_to_static_error_scrutiny =
        (~edit_action: CompositionActions.edit_action): (bool, bool, bool) => {
      // Returns (of_pat, of_def, of_body), i.e. which parts of the program to check for static errors.
      switch (edit_action) {
      | Initialize(_) =>
        raise(
          Failure(
            "Initialize action handles static error checking on its own.",
          ),
        )
      | UpdateDefinition(_) => (true, true, false)
      | UpdateBody(_) => (true, true, true)
      | UpdatePattern(_) => (true, false, false)
      | UpdateBindingClause(_) => (false, true, false)
      | InsertBefore(_) => (false, false, false)
      | InsertAfter(_) => (false, false, false)
      | DeleteBindingClause(_) => (false, true, false)
      | DeleteBody(_) => (false, false, true)
      };
    };

    let static_error_check =
        (
          ~edit_action: CompositionActions.edit_action,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_node: node,
          ~new_info_map: Id.Map.t(Info.t),
        )
        : option(string) => {
      /*
       A localized static error check to ensure that newly inserted segments do not introduce any errors.

       This is a localized check, as obligations occuring elsewhere in the program are inevitable for
       many types of edits.

       of_pat, of_def, and of_body are used to specify which parts of the program to check for errors.
       */
      let (of_pat, of_def, of_body) =
        edit_action_to_static_error_scrutiny(~edit_action);
      let initial_errors =
        switch (initial_node) {
        | None => []
        | Some(initial_node) =>
          let initial_subtree =
            GeneralTreeUtils.subtree_of(
              ~info=initial_node.info,
              ~orig_info_map=initial_info_map,
              ~of_pat,
              ~of_def,
              ~of_body,
            );
          ErrorPrint.all(initial_subtree);
        };
      let new_subtree =
        GeneralTreeUtils.subtree_of(
          ~info=new_node.info,
          ~orig_info_map=new_info_map,
          ~of_pat,
          ~of_def,
          ~of_body,
        );
      let new_errors = ErrorPrint.all(new_subtree);
      if (List.length(new_errors) > List.length(initial_errors)) {
        Some(
          "Not applying the action you requested as it would have the following static error(s): "
          ++ String.concat(", ", new_errors),
        );
      } else {
        None;
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

  let edit_dispatch =
      (
        ~e: CompositionActions.edit_action,
        ~initial_z: Zipper.t,
        ~initial_node_map: node_map,
        ~initial_info_map: Id.Map.t(Info.t),
        ~syntax: CachedSyntax.t,
        ~return:
           (Action.Failure.t, option(Zipper.t)) =>
           result(Zipper.t, Action.Failure.t),
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
      ) => {
    switch (e) {
    | UpdateDefinition(path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Def, initial_node);
      switch (
        PerformUtils.overwrite_term(
          initial_z,
          target_id,
          code,
          false,
          syntax,
          return,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_node=Some(initial_node),
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            )
          ) {
          | Some(e) => Error(Action.Failure.Composition_action_failure(e))
          | None => Ok(new_z)
          }
        };
      };
    | UpdateBody(path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, initial_node);
      switch (
        PerformUtils.overwrite_term(
          initial_z,
          target_id,
          code,
          false,
          syntax,
          return,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_node=Some(initial_node),
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            )
          ) {
          | Some(e) => Error(Action.Failure.Composition_action_failure(e))
          | None => Ok(new_z)
          }
        };
      };
    | UpdatePattern(path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Pat, initial_node);
      let old_pat =
        StaticsBase.Map.lookup(target_id, initial_info_map)
        |> OptUtil.get_or_fail(
             "Failed trying to rename all occurences of the pattern. Could not find the old pattern in the statics map.",
           );
      switch (
        PerformUtils.overwrite_term(
          initial_z,
          target_id,
          code,
          false,
          syntax,
          return,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_info_map,
              ~initial_node=Some(initial_node),
              ~new_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
            )
          ) {
          | Some(e) => Error(Action.Failure.Composition_action_failure(e))
          | None =>
            let new_node = node_of_cursor(new_node_map, new_z, new_info_map);
            let new_target_id = Utils.get_inner_term_id(Pat, new_node);
            let new_pat =
              StaticsBase.Map.lookup(new_target_id, new_info_map)
              |> OptUtil.get_or_fail(
                   "Failed trying to rename all occurences of the pattern. Could not find the new pattern in the statics map.",
                 );
            Ok(
              GeneralTreeUtils.update_use_sites_of_pat(
                ~z=new_z,
                ~co_ctx=
                  GeneralTreeUtils.get_refs_to(
                    initial_node.info,
                    new_info_map,
                  ),
                ~old_names=GeneralTreeUtils.get_var_names_from_pat(old_pat),
                ~new_names=GeneralTreeUtils.get_var_names_from_pat(new_pat),
              ),
            );
          }
        };
      };
    | UpdateBindingClause(path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      switch (
        PerformUtils.overwrite_term(
          initial_z,
          target_id,
          code,
          true,
          syntax,
          return,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_info_map,
              ~initial_node=Some(initial_node),
              ~new_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
            )
          ) {
          | Some(e) => Error(Action.Failure.Composition_action_failure(e))
          | None => Ok(new_z)
          }
        };
      };
    | InsertBefore(path, code) =>
      // todo: figure out a better method than magic space
      let target_id = path_to_id(initial_node_map, path);
      switch (
        {
          PerformUtils.insert_term(
            initial_z,
            target_id,
            "\n" ++ code ++ "\n",
            Direction.Left,
            syntax,
            return,
          );
        }
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        let old_errors = ErrorPrint.all(initial_info_map);
        let new_errors = ErrorPrint.all(new_info_map);
        if (List.length(new_errors) > List.length(old_errors)) {
          Error(
            Action.Failure.Composition_action_failure(
              "Not applying the action you requested as it would introduce new static error(s): "
              ++ String.concat(", ", new_errors),
            ),
          );
        } else {
          Ok(new_z);
        };
      };
    | InsertAfter(path, code) =>
      // todo: figure out a better method than magic space
      let target_id = path_to_id(initial_node_map, path);
      switch (
        PerformUtils.insert_term(
          initial_z,
          target_id,
          "\n" ++ code ++ "\n",
          Direction.Right,
          syntax,
          return,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        let old_errors = ErrorPrint.all(initial_info_map);
        let new_errors = ErrorPrint.all(new_info_map);
        if (List.length(new_errors) > List.length(old_errors)) {
          Error(
            Action.Failure.Composition_action_failure(
              "Not applying the action you requested as it would introduce new static error(s): "
              ++ String.concat(", ", new_errors),
            ),
          );
        } else {
          Ok(new_z);
        };
      };
    | DeleteBindingClause(path) =>
      let target_id = path_to_id(initial_node_map, path);
      PerformUtils.destruct(
        ~defs_exclude_bodies=true,
        initial_z,
        target_id,
        syntax,
      );
    | DeleteBody(path) =>
      let node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, node);
      PerformUtils.destruct(
        ~defs_exclude_bodies=false,
        initial_z,
        target_id,
        syntax,
      );
    | Initialize(_) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Once a program has let/type alias expressions, you can never use initialize on it ever again.",
        ),
      )
    };
  };

  // Tempory wrapper that helps me localize myself while implementing (remove)
  let composition_dispatch =
      (
        a: CompositionActions.editor_action,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    let initial_info_map = mk_statics(z);
    switch (build(z, initial_info_map)) {
    | None =>
      switch (a) {
      | Edit(Initialize(code)) =>
        initialize_dispatch(z, mk_statics, return, code)
      | _ => Error(Action.Failure.Cant_derive_local_AST_information)
      }
    | Some(initial_node_map) =>
      switch (a) {
      | View(a) => view_dispatch(a, z)
      | Read(ShowUseSites(_path))
      | Read(ShowReferences(_path)) => Ok(z) // TODO: Implement
      | Edit(e) =>
        edit_dispatch(
          ~e,
          ~initial_z=z,
          ~initial_node_map,
          ~initial_info_map,
          ~syntax,
          ~return,
          ~mk_statics,
        )
      }
    };
  };

  let get_initial_cursor_position = (z: Zipper.t, info_map: Id.Map.t(Info.t)) => {
    switch (Indicated.ci_of(z, info_map)) {
    | Some(ci) => Info.id_of(ci)
    | None =>
      raise(
        Failure(
          "No indicated piece found when getting initial cursor position.",
        ),
      )
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
                switch (path_to_id_opt(node_map, path)) {
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

  let go =
      (
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: CompositionActions.editor_action,
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~return:
           (Action.Failure.t, option(Zipper.t)) =>
           result(Zipper.t, Action.Failure.t),
        ~schedule_tool_response: AssistantUpdateAction.status => unit,
      ) => {
    let res =
      try(
        switch (composition_dispatch(a, syntax, z, mk_statics, return)) {
        | Ok(new_z) =>
          switch (freshen_paths(new_z, mk_statics)) {
          | Ok(new_z) => Ok(Dump.to_zipper(new_z))
          | Error(e) => Error(e)
          }
        | Error(e) => Error(e)
        }
      ) {
      | Failure(e) => Error(Action.Failure.Composition_action_failure(e))
      };

    //todo: handle res and use schedule_assistant_action to send the result to the assistant and loop
    switch (res) {
    | Ok(_) =>
      schedule_tool_response(
        AssistantUpdateAction.Success(
          "Action has been applied to the editor -- TODO: make more informative",
        ),
      )
    | Error(Composition_action_failure(e)) =>
      schedule_tool_response(AssistantUpdateAction.Failure(e))
    | Error(Action.Failure.Cant_derive_local_AST_information) =>
      schedule_tool_response(
        AssistantUpdateAction.Failure(
          "Could not derive an AST with definition nodes for the program. Maybe you meant to call initialize? Make sure to call initialize to overwrite the current program, and introduce a program with definition-based nodes. i.e. You tried calling a tool that requres deinitions to exist in the program.",
        ),
      )
    | _ =>
      schedule_tool_response(
        AssistantUpdateAction.Failure(
          "An error occured when applying your changes to the editor",
        ),
      )
    };

    res;
  };
};
module Public = {
  let go = Local.go;
};
