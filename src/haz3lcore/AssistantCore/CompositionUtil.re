open Util;
open Language;
open Language.Statics;

module View = {
  // The following functions are to help with viewing the AST
  // as a modified version of the editor.
  // This allows use to modify the term-base of the editor itself,
  // covering up inner child definitions with folds,
  // and any other modifications we might want to make to the editor
  // before displaying to the LLM.
  let mk_syntax: Zipper.t => Editor.CachedSyntax.t =
    Editor.CachedSyntax.init(
      ~info_map=Language.Statics.Map.empty,
      ~dyn_map=Language.Dynamics.Map.empty,
    );
  let mk_state: Zipper.t => Editor.State.t =
    z => {
      zipper: z,
      col_target: None,
    };
  let mk_move = (z: Zipper.t): (module Move.S) =>
    Editor.Model.to_move_s({
      state: mk_state(z),
      syntax: mk_syntax(z),
    });
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go_z(
      ~settings=Language.CoreSettings.off,
      CachedStatics.empty,
      a,
      mk_move(z),
      z,
    );

  let definition = (z: Zipper.t, curr_node: AssistantTreeHelper.node) => {
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
          switch (
            perform(
              // Select the *tile* of the parent
              // TODO: throws failure when trying to select tile
              // note: this bug only started happening after passing the segment
              // directly for view
              // I suspect this is because the body expression is missing when selecting term
              Action.Select(
                Term(
                  Id(AssistantTreeHelper.id_of(parent), Direction.Right),
                ),
              ),
              z'',
            )
          ) {
          | Ok(z''') => z'''
          | _ => z''
          }
        | _ => z
        }
      | None =>
        switch (perform(Action.Select(All), z')) {
        | Ok(z'') => z''
        | _ => z
        }
      };

    let seg = z''.selection.content;
    seg;
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
