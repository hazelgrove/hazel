open Util;
open HighLevelNodeMap.Public;
open Language;
open OptUtil.Syntax;

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
        | ModuleExp(mp, def, body) =>
          switch (inner_term) {
          | Pat => MPat.rep_id(mp)
          | Def => Exp.rep_id(def)
          | Body => Exp.rep_id(body)
          }
        | _ =>
          raise(
            Failure(
              "UNIMPLEMENTED_NODE_TYPE: Only let, type alias, and module expressions are currently supported as nodes",
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

  let segment_of_term =
      (zipper: Zipper.t, target_id: option(Id.t), syntax: CachedSyntax.t)
      : option(Segment.t) => {
    switch (target_id) {
    | Some(target_id) =>
      let* zipper =
        Select.term(
          ~defs_exclude_bodies=true,
          ~case_rules=false,
          syntax.term_data,
          target_id,
          zipper,
        );
      Some(zipper.selection.content);
    | _ =>
      let zipper = Select.all(zipper);
      Some(zipper.selection.content);
    };
  };

  let get_diff =
      (
        old_zipper: Zipper.t,
        new_zipper: Zipper.t,
        action: Action.Structural.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        syntax: CachedSyntax.t,
      )
      : option((Segment.t, option(Segment.t))) => {
    switch (action) {
    | Insert(_, _, _) =>
      let* old_segment = segment_of_term(old_zipper, None, syntax);
      let new_segment = segment_of_term(new_zipper, None, syntax);
      Some((old_segment, new_segment));
    | Update(_, path, _)
    | Delete(_, path) =>
      /* Delete (especially BindingClause) removes the target from the new
         program; Delete(Pattern) replaces the naming pat with a hole so the
         path no longer resolves by name. Resolve the new-side target with
         [[path_to_id_opt]] and treat non-resolution as "no replacement
         segment" — the diff becomes "old segment deleted" instead of
         raising. Historical bug: using [[path_to_id]] here raised
         "Path X not found in node map" after every successful delete,
         surfacing to the agent as a spurious tool-call failure. */
      let* old_node_map =
        HighLevelNodeMap.build(old_zipper, mk_statics(old_zipper));
      let* new_node_map =
        HighLevelNodeMap.build(new_zipper, mk_statics(new_zipper));
      let old_target_id = path_to_id(old_node_map, path);
      let* old_segment =
        segment_of_term(old_zipper, Some(old_target_id), syntax);
      let new_segment =
        switch (path_to_id_opt(new_node_map, path)) {
        | Some(new_target_id) =>
          segment_of_term(new_zipper, Some(new_target_id), syntax)
        | None => None
        };
      Some((old_segment, new_segment));
    };
  };

  module PerformUtils = {
    let edit_action_to_static_error_scrutiny =
        (~edit_action: Action.Structural.t): (bool, bool, bool) => {
      // Returns (of_pat, of_def, of_body), i.e. which parts of the program to check for static errors.
      switch (edit_action) {
      | Update(Definition, _, _) => (true, true, false)
      | Update(Body, _, _) => (true, true, true)
      | Update(Pattern, _, _) => (true, false, false)
      | Update(BindingClause, _, _) => (false, true, false)
      | Insert(_, _, _) => (false, false, false)
      | Delete(BindingClause, _) => (false, true, false)
      | Delete(Body, _) => (false, false, true)
      | Delete(Definition | Pattern, _) => (false, false, false)
      };
    };

    let static_error_check =
        (
          ~edit_action: Action.Structural.t,
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

  let edit_dispatch =
      (
        ~e: Action.Structural.t,
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
    | Update(Definition, path, code) =>
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
          | None =>
            let z_after_projectors =
              try({
                let fresh_syn = CachedSyntax.init(new_z);
                let binding_node = path_to_node(new_node_map, path);
                let def_id = Utils.get_inner_term_id(Def, binding_node);
                switch (
                  Select.term(
                    ~defs_exclude_bodies=false,
                    ~case_rules=false,
                    fresh_syn.term_data,
                    def_id,
                    new_z,
                  )
                ) {
                | None => new_z
                | Some(z_sel) =>
                  let seg = z_sel.selection.content;
                  let focus = z_sel.selection.focus;
                  let (z_sel', new_seg, did_change) =
                    ProjectorPerform.sanitize_projectors_in_segment(
                      z_sel,
                      seg,
                    );
                  did_change
                    ? Zipper.replace_selection(focus, new_seg, z_sel') : new_z;
                };
              }) {
              | Failure(_) => new_z
              };
            Ok(z_after_projectors);
          }
        };
      };
    | Update(Body, path, code) =>
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
    | Update(Pattern, path, code) =>
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
            /* Hybrid refs: pre-edit [[co_ctx]] for the filter, post-edit term +
               [[new_info_map]] for the body, so renames see stale spellings but
               paths stay consistent after follow-up edits (see
               [[GeneralTreeUtils.get_refs_to_after_pattern_edit]]). */
            Ok(
              GeneralTreeUtils.update_use_sites_of_pat(
                ~z=new_z,
                ~co_ctx=
                  GeneralTreeUtils.get_refs_to_after_pattern_edit(
                    ~pre_edit_let_info=initial_node.info,
                    ~post_edit_let_info=new_node.info,
                    new_info_map,
                  ),
                ~old_names=GeneralTreeUtils.get_var_names_from_pat(old_pat),
                ~new_names=GeneralTreeUtils.get_var_names_from_pat(new_pat),
              ),
            );
          }
        };
      };
    | Update(BindingClause, path, code) =>
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
    | Insert(Before, path, code) =>
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
    | Insert(After, path, code) =>
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
    | Delete(BindingClause, path) =>
      let target_id = path_to_id(initial_node_map, path);
      PerformUtils.destruct(
        ~defs_exclude_bodies=true,
        initial_z,
        target_id,
        syntax,
      );
    | Delete(Body, path) =>
      let node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, node);
      PerformUtils.destruct(
        ~defs_exclude_bodies=false,
        initial_z,
        target_id,
        syntax,
      );
    | Delete(Definition | Pattern, _) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Deleting a definition or pattern is not yet implemented.",
        ),
      )
    };
  };

  let composition_dispatch =
      (
        a: Action.Structural.t,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    let initial_info_map = mk_statics(z);
    switch (build(z, initial_info_map)) {
    | None => Error(Action.Failure.Cant_derive_local_AST_information)
    | Some(initial_node_map) =>
      edit_dispatch(
        ~e=a,
        ~initial_z=z,
        ~initial_node_map,
        ~initial_info_map,
        ~syntax,
        ~return,
        ~mk_statics,
      )
    };
  };

  let go =
      (
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
        ~return:
           (Action.Failure.t, option(Zipper.t)) =>
           result(Zipper.t, Action.Failure.t),
      )
      : result(Zipper.t, Action.Failure.t) => {
    let res =
      try(
        switch (composition_dispatch(a, syntax, z, mk_statics, return)) {
        | Ok(new_z) => Ok(Dump.to_zipper(new_z))
        | Error(e) => Error(e)
        }
      ) {
      | Failure(e) => Error(Action.Failure.Composition_action_failure(e))
      };

    res;
  };
};

module Public = {
  let mk_statics = (z: Zipper.t): Language.StaticsBase.Map.t =>
    Language.(
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.from_zip_for_sem(z).term,
      )
    );
  let go = Local.go(~mk_statics);
};
