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
                Failure(
                  "No type annotation found on this binding's pattern",
                ),
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
      | Delete(Definition | Pattern | TypeAnnotation, _) => (false, false, false)
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
                  errors :=
                    ["Invalid token: \"" ++ token ++ "\"", ...errors^];
                  e;
                | MultiHole(_) =>
                  errors := ["MultiHole (malformed expression)", ...errors^];
                  e;
                | _ => continue(e)
                };
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
              };
            },
          ~f_pat=
            (continue, p) => {
              switch (Pat.term_of(p)) {
              | EmptyHole =>
                pat_holes := pat_holes^ + 1;
                p;
              | _ => continue(p)
              };
            },
          ~f_typ=
            (continue, t) => {
              switch (Typ.term_of(t)) {
              | Unknown(Hole(EmptyHole)) =>
                typ_holes := typ_holes^ + 1;
                t;
              | _ => continue(t)
              };
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
        ~return as _return:
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
      let term_edit_result =
        if (is_type_alias) {
          TermEdit.update_type_annotation(initial_z, target_id, code);
        } else {
          TermEdit.update_definition(initial_z, target_id, code);
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
            "Failed to parse the new definition code.",
          ),
        )
      };
    | Update(Body, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, initial_node);
      switch (TermEdit.update_body(initial_z, target_id, code)) {
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
            "Failed to parse the new body code.",
          ),
        )
      };
    | Update(Pattern, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Pat, initial_node);
      /* TermEdit.update_pattern handles both pattern replacement and
         variable renaming at the term level (before round-trip), so
         we don't need the old statics-based use-site renaming. */
      switch (TermEdit.update_pattern(initial_z, target_id, code)) {
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
            "Failed to parse the new pattern code.",
          ),
        )
      };
    | Update(BindingClause, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      let term_edit_result =
        if (TermEdit.is_module_item(initial_z, target_id)) {
          TermEdit.module_update_binding(initial_z, target_id, code);
        } else {
          TermEdit.update_binding_clause(initial_z, target_id, code);
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
            "Failed to update binding clause.",
          ),
        )
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
            "Failed to parse the new type annotation code.",
          ),
        )
      };
    | Insert(Before, path, code) =>
      let target_id = path_to_id(initial_node_map, path);
      let term_edit_result =
        if (TermEdit.is_module_item(initial_z, target_id)) {
          TermEdit.module_insert(initial_z, target_id, code, Direction.Left);
        } else {
          TermEdit.insert_binding(
            initial_z, target_id, code, Direction.Left,
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
            "Failed to insert binding.",
          ),
        )
      };
    | Insert(After, path, code) =>
      let target_id = path_to_id(initial_node_map, path);
      let term_edit_result =
        if (TermEdit.is_module_item(initial_z, target_id)) {
          TermEdit.module_insert(
            initial_z, target_id, code, Direction.Right,
          );
        } else {
          TermEdit.insert_binding(
            initial_z, target_id, code, Direction.Right,
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
            "Failed to insert binding.",
          ),
        )
      };
    | Delete(BindingClause, path) =>
      let target_id = path_to_id(initial_node_map, path);
      let term_edit_result =
        if (TermEdit.is_module_item(initial_z, target_id)) {
          TermEdit.module_delete(initial_z, target_id);
        } else {
          TermEdit.delete_binding(initial_z, target_id);
        };
      switch (term_edit_result) {
      | Some(new_z) => Ok((new_z, None))
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to delete binding.",
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
            "Failed to delete body.",
          ),
        )
      };
    | Delete(Definition | Pattern | TypeAnnotation, _) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Deleting a definition, pattern, or type annotation is not yet implemented.",
        ),
      )
    };
  };

  let format_typ = (ty: Typ.t): string => ErrorPrint.Print.typ(ty);

  let format_status_exp = (status: Info.status_exp): string =>
    switch (status) {
    | NotInHole(Common(Syn(ty))) =>
      "Synthesized type: " ++ format_typ(ty)
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
    | InHole(err) =>
      "Status: error\nError: " ++ ErrorPrint.exp_error(err)
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
        let results =
          matches |> List.map(Selector.print_match);
        Ok(String.concat("\n", results));
      };
    | GetCompleteness =>
      let (exp_holes, pat_holes, typ_holes) =
        PerformUtils.count_holes(z);
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
          | Singleton(ty) =>
            "  " ++ te.name ++ " = " ++ format_typ(ty)
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
            ++ String.concat(
                 "\n",
                 List.map(fmt_var, constructors),
               )
          };
        Ok(result);
      | Select(_)
      | GetCompleteness => assert(false) /* handled above */
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
      (
        ~action: CompositionActions.read_action,
        ~z: Zipper.t,
      )
      : result(string, Action.Failure.t) => {
    let info_map = mk_statics(z);
    let syntax = CachedSyntax.init(z);
    Local.read_dispatch(~action, ~z, ~info_map, ~syntax);
  };
};
