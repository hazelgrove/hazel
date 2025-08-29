open Util;
open Language;
open Language.Statics;

module View = {
  // The following functions are to help with viewing the AST
  // as a modified version of the editor.
  // This allows use to modify the term-base of the editor itself,
  // covering up inner child definitions with folds,
  // and any other modifications we might want to make to the editor
  // before displaying a snippet of it in string form to the LLM.
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go(
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      a,
      {
        zipper: z,
        col_target: None,
      },
    );

  let caret_char = "¦"; /* Note this is two bytes */
  let convex_char = "?";
  let concave_char = "~";
  let selection_char = "§"; /* Note this is two bytes */
  let caret_regexp = StringUtil.regexp(caret_char);

  let printer = (z: Zipper.t): string => {
    Printer.of_zipper(
      ~holes=convex_char,
      ~concave_holes=concave_char,
      ~special_folds=true,
      ~caret=caret_char,
      ~selection_anchor=selection_char,
      z,
    );
  };

  let prepare_definition = (z: Zipper.t, curr_node: AssistantTreeHelper.node) => {
    let rec fold_terms = (z: Zipper.t, ids: list(Id.t)) => {
      switch (ids) {
      | [] => z
      | [id, ...rest] =>
        // Fold the *term* of the definition
        let z' = perform(Action.Select(Term(Id(id, Direction.Right))), z);
        switch (z') {
        | Ok(z') =>
          let z'' =
            perform(Action.Project(SetIndicated(Specific(Fold))), z');
          switch (z'') {
          | Ok(z'') => fold_terms(z'', rest)
          | _ => fold_terms(z', rest)
          };
        | _ => fold_terms(z, rest)
        };
      };
    };
    let remove_body = (z: Zipper.t, term: Info.t) => {
      let id =
        switch (term) {
        | InfoExp({term, _}) =>
          switch (Exp.term_of(term)) {
          | Let(_, _, body) => Exp.rep_id(body)
          | TyAlias(_, _, body) => Exp.rep_id(body)
          | _ => Id.invalid
          }
        | _ => Id.invalid
        };
      let z' = perform(Action.Select(Term(Id(id, Direction.Right))), z);
      switch (z') {
      | Ok(z') =>
        let z'' = perform(Action.Paste(String("")), z');
        z'';
      | _ => Ok(z)
      };
    };
    let get_def_id_of_let = (term: Info.t): Id.t => {
      switch (term) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_, def, _) => Exp.rep_id(def)
        // We won't fold/abstract away type definitions.
        | _ => Id.invalid
        }
      | _ => Id.invalid
      };
    };
    print_endline(
      "curr_node.id: " ++ Uuidm.to_string(Info.id_of(curr_node.info)),
    );
    print_endline(
      "curr_node.children: "
      ++ String.concat(
           ", ",
           List.map(
             Uuidm.to_string,
             List.map(AssistantTreeHelper.id_of, curr_node.children),
           ),
         ),
    );
    print_endline(
      "curr_node.siblings: "
      ++ String.concat(
           ", ",
           List.map(
             Uuidm.to_string,
             List.map(AssistantTreeHelper.id_of, curr_node.siblings),
           ),
         ),
    );
    let children_def_ids =
      List.map(
        (c: AssistantTreeHelper.node) => get_def_id_of_let(c.info),
        curr_node.children,
      );
    let siblings_def_ids =
      List.map(
        (c: AssistantTreeHelper.node) => get_def_id_of_let(c.info),
        curr_node.siblings,
      );

    let z = fold_terms(z, children_def_ids);
    let z' = fold_terms(z, siblings_def_ids);

    let z'' =
      switch (curr_node.parent) {
      | Some(parent) =>
        // this switch is a temporary workaround for below mentioned bug
        switch (remove_body(z', parent.info)) {
        | Ok(z'') =>
          let syntax = CachedSyntax.init(z'');
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data,
              AssistantTreeHelper.id_of(parent),
              z'',
            )
          ) {
          | Some(z''') => z'''
          | None => z''
          };
        | _ => z
        }
      | None =>
        switch (perform(Action.Select(All), z')) {
        | Ok(z'') => z''
        | _ => z
        }
      };

    // Todo @andrew: Not sure of the perf effects of the below
    // What this does is effectively display the local code map from the parent of the current node,
    // down, along with the current selection (the current node the cursor is at, using the same
    // characters test_editing uses).
    // This effectively Cuts out the def of the parent, pastes it as it's own thing, and then
    // selects the def of the current node.
    let seg = z''.selection.content;
    let z = Zipper.init();
    let z' = Zipper.insert_segment(z, seg);
    let z'' =
      switch (
        Select.term(
          ~defs_exclude_bodies=true,
          ~case_rules=false,
          CachedSyntax.init(z').term_data,
          AssistantTreeHelper.id_of(curr_node),
          z',
        )
      ) {
      | Some(z'') => z''
      | None => raise(Failure("Failed to select term"))
      };
    print_endline(printer(z''));
    z'';
  };

  let context = (local_information: AssistantTreeHelper.node): string => {
    let info = local_information.info;
    switch (info) {
    | InfoExp(info) =>
      let ctx = info.ctx;
      let bindings: Binding.s =
        List.filter_map(
          (entry: Ctx.entry) => {
            let b =
              switch (entry) {
              | Ctx.VarEntry(entry) => Ctx.binding_of(ctx, entry.name)
              | Ctx.TVarEntry(entry) => Ctx.binding_of(ctx, entry.name)
              | Ctx.ConstructorEntry(entry) =>
                Ctx.binding_of(ctx, entry.name)
              | _ => Ctx.binding_of(ctx, "") // invalid
              };
            if (b.id == Id.invalid) {
              None;
            } else {
              Some(b);
            };
          },
          ctx.entries,
        );
      "Typing Context: ["
      ++ String.concat(
           "\n",
           List.mapi(
             (i: int, b: Binding.t) =>
               b.name ++ "(Index: " ++ string_of_int(i) ++ ")",
             bindings,
           ),
         )
      ++ "]";
    | _ => ""
    };
  };

  let references_in =
      (
        local_information: AssistantTreeHelper.node,
        info_map: Id.Map.t(Info.t),
      )
      : string => {
    let id = AssistantTreeHelper.id_of(local_information);
    let references = Statics.Map.refs_in(info_map, id);
    "References: ["
    ++ String.concat(
         ", ",
         List.mapi(
           (i: int, b: Binding.t) =>
             b.name ++ " (Index: " ++ string_of_int(i) ++ ")",
           references,
         ),
       )
    ++ "]";
  };
};
