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
    | Body
    | TypeAnn;

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
          | TypeAnn =>
            switch (Pat.term_of(pat)) {
            | Asc(_, typ) => Typ.rep_id(typ)
            | _ =>
              raise(
                Failure("No type annotation found on this binding's pattern"),
              )
            }
          }
        | TyAlias(tpat, tdef, body) =>
          switch (inner_term) {
          | Pat => TPat.rep_id(tpat)
          | Def => Typ.rep_id(tdef)
          | Body => Exp.rep_id(body)
          | TypeAnn => Typ.rep_id(tdef) /* For type alias, the "annotation" is the definition */
          }
        | ModuleExp(mp, def, body) =>
          switch (inner_term) {
          | Pat => MPat.rep_id(mp)
          | Def => Exp.rep_id(def)
          | Body => Exp.rep_id(body)
          | TypeAnn =>
            switch (mp.term) {
            | Asc(_, sig_typ) => Typ.rep_id(sig_typ)
            | _ =>
              raise(
                Failure("No type annotation found on this module binding's pattern"),
              )
            }
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
      let* old_node_map =
        HighLevelNodeMap.build(old_zipper, mk_statics(old_zipper));
      let* new_node_map =
        HighLevelNodeMap.build(new_zipper, mk_statics(new_zipper));
      let old_target_id = path_to_id(old_node_map, path);
      let new_target_id = path_to_id(new_node_map, path);
      let* old_segment =
        segment_of_term(old_zipper, Some(old_target_id), syntax);
      let new_segment =
        segment_of_term(new_zipper, Some(new_target_id), syntax);
      Some((old_segment, new_segment));
    | SelectorUpdate(_, _)
    | SelectorDelete(_)
    | SelectorInsertBefore(_, _)
    | SelectorInsertAfter(_, _) =>
      /* Selector-driven edits: diff the whole program */
      let* old_segment = segment_of_term(old_zipper, None, syntax);
      let new_segment = segment_of_term(new_zipper, None, syntax);
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
      | Update(TypeAnnotation, _, _) => (true, true, false)
      | Update(BindingClause, _, _) => (false, true, false)
      | Insert(_, _, _) => (false, false, false)
      | Delete(BindingClause, _) => (false, true, false)
      | Delete(Body, _) => (false, false, true)
      | Delete(Definition | Pattern | TypeAnnotation, _) => (
          false,
          false,
          false,
        )
      /* Selector-driven: check everything since we don't know the target */
      | SelectorUpdate(_, _) => (true, true, true)
      | SelectorDelete(_) => (true, true, true)
      | SelectorInsertBefore(_, _) => (false, false, false)
      | SelectorInsertAfter(_, _) => (false, false, false)
      };
    };

    let parse_error_check = (z: Zipper.t): option(string) => {
      /* Check for parse errors:
         1. Unmatched delimiters (orphaned shards in the backpack)
         2. Invalid nodes in the term tree
         3. MultiHole nodes in the term tree */
      let backpack = Zipper.local_backpack(z);
      switch (backpack) {
      | [_, ..._] as tiles =>
        let labels =
          tiles
          |> List.map((t: Tile.t) =>
               String.concat(" ", Tile.effective_label(t))
             );
        Some(
          "Parse error: unmatched delimiter(s) ["
          ++ String.concat(", ", labels)
          ++ "]. Check for missing or extra delimiters in your code.",
        );
      | [] =>
        /* Check for Invalid/MultiHole nodes in the parsed term */
        let term = MakeTerm.from_zip_for_sem(z).term;
        let errors = ref([]);
        let _ =
          Exp.map_term(
            ~f_exp=
              (continue, e) => {
                switch (Exp.term_of(e)) {
                | Invalid(token) =>
                  errors := ["Invalid token: \"" ++ token ++ "\"", ...errors^];
                  e;
                | MultiHole(_) =>
                  errors := ["MultiHole (malformed expression)", ...errors^];
                  e;
                | _ => continue(e)
                }
              },
            term,
          );
        switch (errors^) {
        | [] => None
        | errs =>
          Some(
            "Parse error: "
            ++ String.concat("; ", List.rev(errs))
            ++ ". Check your syntax.",
          )
        };
      };
    };

    /* Count empty holes in the program. Returns the number of
       expression, pattern, and type holes. Useful for the agent to
       know if the program is "complete" (no unfilled holes). */
    let count_holes = (z: Zipper.t): (int, int, int) => {
      let term = MakeTerm.from_zip_for_sem(z).term;
      let exp_holes = ref(0);
      let pat_holes = ref(0);
      let typ_holes = ref(0);
      let _ =
        Exp.map_term(
          ~f_exp=
            (continue, e) => {
              switch (Exp.term_of(e)) {
              | EmptyHole =>
                exp_holes := exp_holes^ + 1;
                e;
              | _ => continue(e)
              }
            },
          ~f_pat=
            (continue, p) => {
              switch (Pat.term_of(p)) {
              | EmptyHole =>
                pat_holes := pat_holes^ + 1;
                p;
              | _ => continue(p)
              }
            },
          ~f_typ=
            (continue, t) => {
              switch (Typ.term_of(t)) {
              | Unknown(Hole(EmptyHole)) =>
                typ_holes := typ_holes^ + 1;
                t;
              | _ => continue(t)
              }
            },
          term,
        );
      (exp_holes^, pat_holes^, typ_holes^);
    };

    /* Compute a static error warning for an edit. Returns Some(warning)
       if the edit introduces new errors, None otherwise. This is purely
       informational — it does NOT block the edit. The agent can use
       get_statics to investigate and fix any errors. */
    let static_error_warning =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_node: node,
          ~new_info_map: Id.Map.t(Info.t),
        )
        : option(string) => {
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
          "Warning: this edit introduced new static error(s): "
          ++ String.concat(", ", new_errors)
          ++ ". Use get_statics to investigate.",
        );
      } else {
        None;
      };
    };

    /* Combined check: parse errors are hard failures (unmatched delimiters,
       invalid tokens). Static errors produce warnings but do NOT block the
       edit, allowing multi-step refactoring where intermediate states have
       type errors. validate_edit_full returns intermediate results
       (info_map, node_map) for callers that need them (e.g. Pattern rename). */
    let validate_edit_full =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_z: Zipper.t,
          ~mk_statics: Zipper.t => StaticsBase.Map.t,
        )
        : result(
            (Zipper.t, Id.Map.t(Info.t), node_map, option(string)),
            Action.Failure.t,
          ) => {
      switch (parse_error_check(new_z)) {
      | Some(parse_err) =>
        Error(Action.Failure.Composition_action_failure(parse_err))
      | None =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          let warning =
            static_error_warning(
              ~edit_action,
              ~initial_node,
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            );
          Ok((new_z, new_info_map, new_node_map, warning));
        };
      };
    };

    let validate_edit =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_z: Zipper.t,
          ~mk_statics: Zipper.t => StaticsBase.Map.t,
        )
        : result((Zipper.t, option(string)), Action.Failure.t) =>
      switch (
        validate_edit_full(
          ~edit_action,
          ~initial_node,
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
        )
      ) {
      | Ok((z, _, _, warning)) => Ok((z, warning))
      | Error(e) => Error(e)
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
  };

  let edit_dispatch =
      (
        ~e: Action.Structural.t,
        ~initial_z: Zipper.t,
        ~initial_node_map: node_map,
        ~initial_info_map: Id.Map.t(Info.t),
        ~syntax as _syntax: CachedSyntax.t,
        ~return as
          _return:
            (Action.Failure.t, option(Zipper.t)) =>
            result(Zipper.t, Action.Failure.t),
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    switch (e) {
    | Update(Definition, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Def, initial_node);
      /* For TyAlias nodes, the definition is a type (not an expression),
         so we need to use update_type_annotation which uses parse_typ
         and replace_typ_by_id. */
      let is_type_alias =
        switch (initial_node.info) {
        | InfoExp({term, _}) =>
          switch (Exp.term_of(term)) {
          | TyAlias(_, _, _) => true
          | _ => false
          }
        | _ => false
        };
      let (term_edit_result, kind) =
        if (is_type_alias) {
          (
            TermEdit.update_type_annotation(initial_z, target_id, code),
            "type alias definition",
          );
        } else {
          (
            TermEdit.update_definition(initial_z, target_id, code),
            "definition",
          );
        };
      switch (term_edit_result) {
      | Some(new_z) =>
        PerformUtils.validate_edit(
          ~edit_action=e,
          ~initial_node=Some(initial_node),
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
        )
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to update "
            ++ kind
            ++ ": could not parse \""
            ++ code
            ++ "\" as valid code.",
          ),
        )
      };
    | Update(Body, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* For sequence element nodes (case arms, list/tuple elements), the
         path points directly to the element expression. For Let/TyAlias
         nodes, extract the body sub-expression. */
      let target_id =
        if (TermEdit.is_sequence_element(initial_z, target_id)) {
          target_id;
        } else {
          Utils.get_inner_term_id(Body, initial_node);
        };
      let (term_edit_result, kind) =
        if (TermEdit.is_case_arm(initial_z, target_id)) {
          (
            TermEdit.case_update_arm_body(initial_z, target_id, code),
            "case arm body",
          );
        } else if (TermEdit.is_list_element(initial_z, target_id)) {
          (
            TermEdit.list_update_element(initial_z, target_id, code),
            "list element",
          );
        } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
          (
            TermEdit.tuple_update_element(initial_z, target_id, code),
            "tuple element",
          );
        } else {
          (TermEdit.update_body(initial_z, target_id, code), "body");
        };
      switch (term_edit_result) {
      | Some(new_z) =>
        PerformUtils.validate_edit(
          ~edit_action=e,
          ~initial_node=Some(initial_node),
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
        )
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to update "
            ++ kind
            ++ ": could not parse \""
            ++ code
            ++ "\" as valid code.",
          ),
        )
      };
    | Update(Pattern, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* For case arm nodes, use case_update_arm_pattern which finds
         the arm by its body ID. For list/tuple elements, Pattern update
         doesn't apply — return an explicit inapplicability error.
         For Let/TyAlias, use update_pattern which handles
         pattern replacement and variable renaming. */
      if (TermEdit.is_list_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(Pattern) is not applicable to list elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(Pattern) is not applicable to tuple elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else {
        let (term_edit_result, kind) =
          if (TermEdit.is_case_arm(initial_z, target_id)) {
            (
              TermEdit.case_update_arm_pattern(initial_z, target_id, code),
              "case arm pattern",
            );
          } else {
            let pat_id = Utils.get_inner_term_id(Pat, initial_node);
            (TermEdit.update_pattern(initial_z, pat_id, code), "pattern");
          };
        switch (term_edit_result) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update "
              ++ kind
              ++ ": could not parse \""
              ++ code
              ++ "\" as a valid pattern.",
            ),
          )
        };
      };
    | Update(BindingClause, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* Case arms, list elements, and tuple elements don't have
         "binding clauses" — use Update(Body) for those. */
      if (TermEdit.is_case_arm(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(BindingClause) is not applicable to case arms. "
            ++ "Use Update(Body, ...) or Update(Pattern, ...) instead.",
          ),
        );
      } else if (TermEdit.is_list_element(initial_z, target_id)
                 || TermEdit.is_tuple_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(BindingClause) is not applicable to list/tuple elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else {
        let (term_edit_result, kind) =
          if (TermEdit.is_module_item(initial_z, target_id)) {
            (
              TermEdit.module_update_binding(initial_z, target_id, code),
              "module item",
            );
          } else {
            (
              TermEdit.update_binding_clause(initial_z, target_id, code),
              "binding clause",
            );
          };
        switch (term_edit_result) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update "
              ++ kind
              ++ ": could not parse \""
              ++ code
              ++ "\" as a valid binding.",
            ),
          )
        };
      };
    | Update(TypeAnnotation, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(TypeAnn, initial_node);
      switch (TermEdit.update_type_annotation(initial_z, target_id, code)) {
      | Some(new_z) =>
        PerformUtils.validate_edit(
          ~edit_action=e,
          ~initial_node=Some(initial_node),
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
        )
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to update type annotation: could not parse \""
            ++ code
            ++ "\" as a valid type.",
          ),
        )
      };
    | Insert(Before, path, code) =>
      let target_id = path_to_id(initial_node_map, path);
      let (term_edit_result, kind) =
        if (TermEdit.is_case_arm(initial_z, target_id)) {
          (
            TermEdit.case_insert_arm(
              initial_z,
              target_id,
              code,
              Direction.Left,
            ),
            "case arm",
          );
        } else if (TermEdit.is_list_element(initial_z, target_id)) {
          (
            TermEdit.list_insert_element(
              initial_z,
              target_id,
              code,
              Direction.Left,
            ),
            "list element",
          );
        } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
          (
            TermEdit.tuple_insert_element(
              initial_z,
              target_id,
              code,
              Direction.Left,
            ),
            "tuple element",
          );
        } else if (TermEdit.is_module_item(initial_z, target_id)) {
          (
            TermEdit.module_insert(initial_z, target_id, code, Direction.Left),
            "module item",
          );
        } else {
          (
            TermEdit.insert_binding(
              initial_z,
              target_id,
              code,
              Direction.Left,
            ),
            "binding",
          );
        };
      switch (term_edit_result) {
      | Some(new_z) =>
        switch (PerformUtils.parse_error_check(new_z)) {
        | Some(parse_err) =>
          Error(Action.Failure.Composition_action_failure(parse_err))
        | None =>
          let new_info_map = mk_statics(new_z);
          let old_errors = ErrorPrint.all(initial_info_map);
          let new_errors = ErrorPrint.all(new_info_map);
          let warning =
            if (List.length(new_errors) > List.length(old_errors)) {
              Some(
                "Warning: this edit introduced new static error(s): "
                ++ String.concat(", ", new_errors)
                ++ ". Use get_statics to investigate.",
              );
            } else {
              None;
            };
          Ok((new_z, warning));
        }
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to insert "
            ++ kind
            ++ " before \""
            ++ path
            ++ "\": could not parse \""
            ++ code
            ++ "\" as valid code.",
          ),
        )
      };
    | Insert(After, path, code) =>
      let target_id = path_to_id(initial_node_map, path);
      let (term_edit_result, kind) =
        if (TermEdit.is_case_arm(initial_z, target_id)) {
          (
            TermEdit.case_insert_arm(
              initial_z,
              target_id,
              code,
              Direction.Right,
            ),
            "case arm",
          );
        } else if (TermEdit.is_list_element(initial_z, target_id)) {
          (
            TermEdit.list_insert_element(
              initial_z,
              target_id,
              code,
              Direction.Right,
            ),
            "list element",
          );
        } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
          (
            TermEdit.tuple_insert_element(
              initial_z,
              target_id,
              code,
              Direction.Right,
            ),
            "tuple element",
          );
        } else if (TermEdit.is_module_item(initial_z, target_id)) {
          (
            TermEdit.module_insert(
              initial_z,
              target_id,
              code,
              Direction.Right,
            ),
            "module item",
          );
        } else {
          (
            TermEdit.insert_binding(
              initial_z,
              target_id,
              code,
              Direction.Right,
            ),
            "binding",
          );
        };
      switch (term_edit_result) {
      | Some(new_z) =>
        switch (PerformUtils.parse_error_check(new_z)) {
        | Some(parse_err) =>
          Error(Action.Failure.Composition_action_failure(parse_err))
        | None =>
          let new_info_map = mk_statics(new_z);
          let old_errors = ErrorPrint.all(initial_info_map);
          let new_errors = ErrorPrint.all(new_info_map);
          let warning =
            if (List.length(new_errors) > List.length(old_errors)) {
              Some(
                "Warning: this edit introduced new static error(s): "
                ++ String.concat(", ", new_errors)
                ++ ". Use get_statics to investigate.",
              );
            } else {
              None;
            };
          Ok((new_z, warning));
        }
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to insert "
            ++ kind
            ++ " after \""
            ++ path
            ++ "\": could not parse \""
            ++ code
            ++ "\" as valid code.",
          ),
        )
      };
    | Delete(BindingClause, path) =>
      let target_id = path_to_id(initial_node_map, path);
      let (term_edit_result, kind) =
        if (TermEdit.is_case_arm(initial_z, target_id)) {
          (TermEdit.case_delete_arm(initial_z, target_id), "case arm");
        } else if (TermEdit.is_list_element(initial_z, target_id)) {
          (
            TermEdit.list_delete_element(initial_z, target_id),
            "list element",
          );
        } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
          (
            TermEdit.tuple_delete_element(initial_z, target_id),
            "tuple element",
          );
        } else if (TermEdit.is_module_item(initial_z, target_id)) {
          (TermEdit.module_delete(initial_z, target_id), "module item");
        } else {
          (TermEdit.delete_binding(initial_z, target_id), "binding");
        };
      switch (term_edit_result) {
      | Some(new_z) => Ok((new_z, None))
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to delete "
            ++ kind
            ++ " at \""
            ++ path
            ++ "\": the element could not be found in the term tree.",
          ),
        )
      };
    | Delete(Body, path) =>
      let node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, node);
      switch (TermEdit.delete_body(initial_z, target_id)) {
      | Some(new_z) => Ok((new_z, None))
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to delete body at \""
            ++ path
            ++ "\": the body expression could not be found in the term tree.",
          ),
        )
      };
    | Delete(Definition | Pattern | TypeAnnotation, _) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Deleting a definition, pattern, or type annotation is not yet implemented.",
        ),
      )

    /* --- Selector-driven edits --- */

    | SelectorUpdate(selector, code) =>
      let term = MakeTerm.from_zip_for_sem(initial_z).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused, focused_id, _}) =>
        let parse_err = msg =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to parse replacement code: " ++ msg,
            ),
          );
        switch (focused) {
        | FocusExp(_) =>
          switch (TermEdit.parse_exp(code)) {
          | None => parse_err(code)
          | Some(new_exp) =>
            let new_term =
              TermEdit.replace_exp_by_id(focused_id, _ => new_exp, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusPat(_) =>
          switch (TermEdit.parse_pat(code)) {
          | None => parse_err(code ++ " (as pattern)")
          | Some(new_pat) =>
            let new_term =
              TermEdit.replace_pat_by_id(focused_id, new_pat, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusTyp(_) =>
          switch (TermEdit.parse_typ(code)) {
          | None => parse_err(code ++ " (as type)")
          | Some(new_typ) =>
            let new_term =
              TermEdit.replace_typ_by_id(focused_id, new_typ, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusMod(_) =>
          switch (TermEdit.find_module_containing_item(focused_id, term)) {
          | None =>
            Error(
              Action.Failure.Composition_action_failure(
                "Module item not found for id",
              ),
            )
          | Some((module_id, items, idx)) =>
            switch (TermEdit.exp_to_mod_item(code)) {
            | None => parse_err(code ++ " (as module item)")
            | Some(new_item) =>
              let new_items = TermEdit.replace_item(items, idx, new_item);
              let new_term =
                TermEdit.replace_module_items(module_id, new_items, term);
              let new_z = TermEdit.term_to_zipper(new_term);
              Ok((new_z, None));
            }
          }
        | FocusSig(_)
        | FocusTPat(_)
        | FocusMPat(_)
        | FocusRule(_, _) =>
          Error(
            Action.Failure.Composition_action_failure(
              "Cannot update this focus type via selector",
            ),
          )
        };
      };

    | SelectorDelete(selector) =>
      let term = MakeTerm.from_zip_for_sem(initial_z).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused, focused_id, _}) =>
        let new_term =
          switch (focused) {
          | FocusExp(_) =>
            let hole = Exp.fresh(EmptyHole);
            TermEdit.replace_exp_by_id(focused_id, _ => hole, term);
          | FocusMod(_) =>
            switch (TermEdit.find_module_containing_item(focused_id, term)) {
            | None => term
            | Some((module_id, items, idx)) =>
              let new_items = TermEdit.delete_item(items, idx);
              TermEdit.replace_module_items(module_id, new_items, term);
            }
          | FocusPat(_) =>
            let hole = Pat.fresh(EmptyHole);
            TermEdit.replace_pat_by_id(focused_id, hole, term);
          | FocusTyp(_) =>
            let hole = Typ.fresh(Unknown(Hole(EmptyHole)));
            TermEdit.replace_typ_by_id(focused_id, hole, term);
          | FocusSig(_)
          | FocusTPat(_)
          | FocusMPat(_)
          | FocusRule(_, _) => term
          };
        let new_z = TermEdit.term_to_zipper(new_term);
        Ok((new_z, None));
      };

    | SelectorInsertBefore(selector, code)
    | SelectorInsertAfter(selector, code) =>
      let dir =
        switch (e) {
        | SelectorInsertBefore(_, _) => Direction.Left
        | _ => Direction.Right
        };
      let term = MakeTerm.from_zip_for_sem(initial_z).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused_id, _}) =>
        let (term_edit_result, kind) =
          if (TermEdit.is_case_arm(initial_z, focused_id)) {
            (
              TermEdit.case_insert_arm(initial_z, focused_id, code, dir),
              "case arm",
            );
          } else if (TermEdit.is_list_element(initial_z, focused_id)) {
            (
              TermEdit.list_insert_element(initial_z, focused_id, code, dir),
              "list element",
            );
          } else if (TermEdit.is_tuple_element(initial_z, focused_id)) {
            (
              TermEdit.tuple_insert_element(initial_z, focused_id, code, dir),
              "tuple element",
            );
          } else {
            /* Check if focused_id is inside a module item (directly or
               as a sub-expression like the definition). If so, use
               module_insert with the item's ID. */
            switch (TermEdit.find_module_item_id(initial_z, focused_id)) {
            | Some(item_id) => (
                TermEdit.module_insert(initial_z, item_id, code, dir),
                "module item",
              )
            | None => (
                TermEdit.insert_binding(initial_z, focused_id, code, dir),
                "binding",
              )
            };
          };
        switch (term_edit_result) {
        | Some(new_z) =>
          switch (PerformUtils.parse_error_check(new_z)) {
          | Some(parse_err) =>
            Error(Action.Failure.Composition_action_failure(parse_err))
          | None => Ok((new_z, None))
          }
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to insert "
              ++ kind
              ++ " via selector \""
              ++ selector
              ++ "\": could not parse \""
              ++ code
              ++ "\" as valid code.",
            ),
          )
        };
      };
    };
  };

  let format_typ = (ty: Typ.t): string => ErrorPrint.Print.typ(ty);

  let format_status_exp = (status: Info.status_exp): string =>
    switch (status) {
    | NotInHole(Common(Syn(ty))) => "Synthesized type: " ++ format_typ(ty)
    | NotInHole(Common(Ana(Consistent({ana, syn, _})))) =>
      "Expected type: "
      ++ format_typ(ana)
      ++ "\nSynthesized type: "
      ++ format_typ(syn)
      ++ "\nStatus: consistent"
    | NotInHole(Common(Ana(InternallyInconsistent({ana, _})))) =>
      "Expected type: "
      ++ format_typ(ana)
      ++ "\nStatus: internally inconsistent (ok in analytic position)"
    | NotInHole(AnaDeferralConsistent(ty)) =>
      "Deferral consistent with type: " ++ format_typ(ty)
    | InHole(err) => "Status: error\nError: " ++ ErrorPrint.exp_error(err)
    };

  let read_dispatch =
      (
        ~action: CompositionActions.read_action,
        ~z: Zipper.t,
        ~info_map: Id.Map.t(Info.t),
        ~syntax: CachedSyntax.t,
      )
      : result(string, Action.Failure.t) => {
    /* Select uses the selector language directly on the term tree,
       bypassing the HighLevelNodeMap path system */
    switch (action) {
    | Select(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z).term;
      switch (Selector.query(selector_str, term)) {
      | [] =>
        Error(
          Composition_action_failure(
            "No match for selector: " ++ selector_str,
          ),
        )
      | matches =>
        let results = matches |> List.map(Selector.print_match);
        Ok(String.concat("\n", results));
      };
    | GetCanonical(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        let num =
          switch (Selector.canonical_numeric(id, term)) {
          | Some(path) => Selector.deparse(path)
          | None => "(not found)"
          };
        let named =
          switch (Selector.canonical_named(id, term)) {
          | Some(path) => Selector.deparse(path)
          | None => "(not found)"
          };
        Ok("numeric: " ++ num ++ "\nnamed: " ++ named);
      };
    | GetCompleteness =>
      let (exp_holes, pat_holes, typ_holes) = PerformUtils.count_holes(z);
      let total = exp_holes + pat_holes + typ_holes;
      if (total == 0) {
        Ok("Complete: no unfilled holes.");
      } else {
        let parts = ref([]);
        if (typ_holes > 0) {
          parts := [string_of_int(typ_holes) ++ " type", ...parts^];
        };
        if (pat_holes > 0) {
          parts := [string_of_int(pat_holes) ++ " pattern", ...parts^];
        };
        if (exp_holes > 0) {
          parts := [string_of_int(exp_holes) ++ " expression", ...parts^];
        };
        Ok(
          "Incomplete: "
          ++ string_of_int(total)
          ++ " unfilled hole(s) ("
          ++ String.concat(", ", parts^)
          ++ ").",
        );
      };
    | SelectorGetStatics(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        switch (Id.Map.find_opt(id, info_map)) {
        | None =>
          Error(
            Composition_action_failure(
              "No statics info for selector-resolved node",
            ),
          )
        | Some(info) =>
          let result =
            switch (info) {
            | InfoExp({ana, status, _}) =>
              "Selector: "
              ++ selector_str
              ++ "\nAnalytic (expected) type: "
              ++ format_typ(ana)
              ++ "\n"
              ++ format_status_exp(status)
            | InfoPat({ana, status, _}) =>
              "Selector: "
              ++ selector_str
              ++ "\nAnalytic type: "
              ++ format_typ(ana)
              ++ "\nStatus: "
              ++ (
                switch (status) {
                | NotInHole(_) => "ok"
                | InHole(err) => "error: " ++ ErrorPrint.pat_error(err)
                }
              )
            | info =>
              "Selector: "
              ++ selector_str
              ++ "\nClass: "
              ++ Cls.show(Info.cls_of(info))
              ++ (
                switch (Info.error_of(info)) {
                | None => "\nStatus: ok"
                | Some(err) =>
                  "\nStatus: error: " ++ ErrorPrint.string_of(err)
                }
              )
            };
          /* Also gather errors from the subtree */
          let subtree =
            GeneralTreeUtils.subtree_of(
              ~info,
              ~orig_info_map=info_map,
              ~of_pat=true,
              ~of_def=true,
              ~of_body=true,
            );
          let errors = ErrorPrint.all(subtree);
          let result =
            switch (errors) {
            | [] => result
            | _ =>
              result
              ++ "\nErrors in subtree:\n"
              ++ String.concat("\n", errors)
            };
          Ok(result);
        };
      };
    | SelectorGetContext(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        switch (Id.Map.find_opt(id, info_map)) {
        | None =>
          Error(
            Composition_action_failure(
              "No statics info for selector-resolved node",
            ),
          )
        | Some(info) =>
          let ctx = Info.ctx_of(info) |> Ctx.filter_shadowed;
          let vars =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.VarEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let constructors =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.ConstructorEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let type_aliases =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.TVarEntry(te) => Some(te)
                 | _ => None
                 }
               );
          let fmt_var = (ve: Ctx.var_entry) =>
            "  " ++ ve.name ++ " : " ++ format_typ(ve.typ);
          let fmt_tvar = (te: Ctx.tvar_entry) =>
            switch (te.kind) {
            | Singleton(ty) => "  " ++ te.name ++ " = " ++ format_typ(ty)
            | Abstract => "  " ++ te.name ++ " (abstract)"
            };
          let result = "Context at selector: " ++ selector_str;
          let result =
            switch (vars) {
            | [] => result
            | _ =>
              result
              ++ "\nVariables:\n"
              ++ String.concat("\n", List.map(fmt_var, vars))
            };
          let result =
            switch (type_aliases) {
            | [] => result
            | _ =>
              result
              ++ "\nType aliases:\n"
              ++ String.concat("\n", List.map(fmt_tvar, type_aliases))
            };
          let result =
            switch (constructors) {
            | [] => result
            | _ =>
              result
              ++ "\nConstructors:\n"
              ++ String.concat("\n", List.map(fmt_var, constructors))
            };
          Ok(result);
        };
      };
    | _ =>
      switch (build(z, info_map)) {
      | None => Error(Cant_derive_local_AST_information)
      | Some(node_map) =>
        switch (action) {
        | GetSyntax(path) =>
          let node = path_to_node(node_map, path);
          let target_id = Info.id_of(node.info);
          switch (segment_of_term(z, Some(target_id), syntax)) {
          | Some(segment) =>
            let code = Printer.of_segment(~holes="?", segment);
            Ok(code);
          | None =>
            Error(
              Composition_action_failure(
                "Could not select the term at path: " ++ path,
              ),
            )
          };
        | GetStatics(path) =>
          let node = path_to_node(node_map, path);
          let result =
            switch (node.info) {
            | InfoExp({ana, status, _}) =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nAnalytic (expected) type: "
              ++ format_typ(ana)
              ++ "\n"
              ++ format_status_exp(status)
            | InfoPat({ana, status, _}) =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nAnalytic type: "
              ++ format_typ(ana)
              ++ "\nStatus: "
              ++ (
                switch (status) {
                | NotInHole(_) => "ok"
                | InHole(err) => "error: " ++ ErrorPrint.pat_error(err)
                }
              )
            | info =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nClass: "
              ++ Cls.show(Info.cls_of(info))
              ++ (
                switch (Info.error_of(info)) {
                | None => "\nStatus: ok"
                | Some(err) =>
                  "\nStatus: error: " ++ ErrorPrint.string_of(err)
                }
              )
            };
          /* Also gather errors from the node's subtree */
          let subtree =
            GeneralTreeUtils.subtree_of(
              ~info=node.info,
              ~orig_info_map=info_map,
              ~of_pat=true,
              ~of_def=true,
              ~of_body=true,
            );
          let errors = ErrorPrint.all(subtree);
          let result =
            switch (errors) {
            | [] => result
            | _ =>
              result
              ++ "\nErrors in subtree:\n"
              ++ String.concat("\n", errors)
            };
          Ok(result);
        | GetContext(path) =>
          let node = path_to_node(node_map, path);
          let ctx = Info.ctx_of(node.info) |> Ctx.filter_shadowed;
          let vars =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.VarEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let constructors =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.ConstructorEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let type_aliases =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.TVarEntry(te) => Some(te)
                 | _ => None
                 }
               );
          let fmt_var = (ve: Ctx.var_entry) =>
            "  " ++ ve.name ++ " : " ++ format_typ(ve.typ);
          let fmt_tvar = (te: Ctx.tvar_entry) =>
            switch (te.kind) {
            | Singleton(ty) => "  " ++ te.name ++ " = " ++ format_typ(ty)
            | Abstract => "  " ++ te.name ++ " (abstract)"
            };
          let result = "Context at path: " ++ path;
          let result =
            switch (vars) {
            | [] => result
            | _ =>
              result
              ++ "\nVariables:\n"
              ++ String.concat("\n", List.map(fmt_var, vars))
            };
          let result =
            switch (type_aliases) {
            | [] => result
            | _ =>
              result
              ++ "\nType aliases:\n"
              ++ String.concat("\n", List.map(fmt_tvar, type_aliases))
            };
          let result =
            switch (constructors) {
            | [] => result
            | _ =>
              result
              ++ "\nConstructors:\n"
              ++ String.concat("\n", List.map(fmt_var, constructors))
            };
          Ok(result);
        | Select(_)
        | GetCanonical(_)
        | GetCompleteness
        | SelectorGetStatics(_)
        | SelectorGetContext(_) => assert(false) /* handled above */
        }
      }
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
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    /* Selector-driven edits bypass the HighLevelNodeMap entirely —
       they resolve against the term tree directly via Selector */
    switch (a) {
    | SelectorUpdate(_)
    | SelectorDelete(_)
    | SelectorInsertBefore(_)
    | SelectorInsertAfter(_) =>
      let initial_info_map = mk_statics(z);
      /* Pass an empty node map — selector edits don't use it */
      edit_dispatch(
        ~e=a,
        ~initial_z=z,
        ~initial_node_map=Id.Map.empty,
        ~initial_info_map,
        ~syntax,
        ~return,
        ~mk_statics,
      );
    | _ =>
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
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    let res =
      try(
        switch (composition_dispatch(a, syntax, z, mk_statics, return)) {
        | Ok((new_z, warning)) => Ok((Dump.to_zipper(new_z), warning))
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

  /* Stores the warning from the most recent structural edit.
     Set by go(), read by Agent.re to include in success messages.
     Cleared on each call to go(). */
  let last_warning: ref(option(string)) = ref(None);

  let go =
      (
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
        ~return:
           (Action.Failure.t, option(Zipper.t)) =>
           result(Zipper.t, Action.Failure.t),
      ) => {
    last_warning := None;
    let result = Local.go(~mk_statics, ~syntax, ~z, ~a, ~return);
    switch (result) {
    | Ok((z, warning)) =>
      last_warning := warning;
      Ok((z, warning));
    | Error(_) => result
    };
  };

  let read_dispatch =
      (~action: CompositionActions.read_action, ~z: Zipper.t)
      : result(string, Action.Failure.t) => {
    let info_map = mk_statics(z);
    let syntax = CachedSyntax.init(z);
    Local.read_dispatch(~action, ~z, ~info_map, ~syntax);
  };
};
