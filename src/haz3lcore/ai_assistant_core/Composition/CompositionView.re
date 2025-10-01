open Util;
open Language;
open Language.Statics;

// The following functions are to help with viewing the AST
// as a modified version of the editor.
// This allows use to modify the term-base of the editor itself,
// covering up inner child definitions with folds,
// and any other modifications we might want to make to the editor
// before displaying a snippet of it in string form to the LLM.

let caret_char = "¦"; /* Note this is two bytes */
let convex_char = "?";
let concave_char = "~";
let selection_char = "§"; /* Note this is two bytes */
let caret_regexp = StringUtil.regexp(caret_char);

// let printer = (z: Zipper.t): string => {
//   Printer.of_zipper(
//     ~holes=convex_char,
//     ~concave_holes=concave_char,
//     ~special_folds=true,
//     ~caret=caret_char,
//     ~selection_anchor=selection_char,
//     z,
//   );
// };

let get_individual_ids_of_let = (term: Info.t): (Id.t, Id.t, Id.t) => {
  switch (term) {
  | InfoExp({term, _}) =>
    switch (Exp.term_of(term)) {
    | Let(pat, def, body) => (
        Pat.rep_id(pat),
        Exp.rep_id(def),
        Exp.rep_id(body),
      )
    // We won't fold/abstract away type definitions.
    | _ => (Id.invalid, Id.invalid, Id.invalid)
    }
  | _ => (Id.invalid, Id.invalid, Id.invalid)
  };
};

let get_def_id_of_let = (term: Info.t): Id.t => {
  let (_, def, _) = get_individual_ids_of_let(term);
  def;
};

let printer = (seg: Segment.t): string => {
  Printer.of_segment(~special_folds=true, seg);
};

let prepare_definition = (z: Zipper.t, curr_node: AssistantTreeHelper.node) => {
  let rec fold_terms = (z: Zipper.t, ids: list(Id.t)) => {
    switch (ids) {
    | [] => z
    | [id, ...rest] =>
      // Fold the *term* of the definition
      switch (
        Select.term(
          ~defs_exclude_bodies=false,
          ~case_rules=false,
          CachedSyntax.init(z).term_data,
          id,
          z,
        )
      ) {
      | Some(z') =>
        switch (
          ProjectorPerform.go(
            CachedSyntax.init(z').term_data,
            SetIndicated(Specific(Fold)),
            z',
          )
        ) {
        | Ok(z'') => fold_terms(z'', rest)
        | _ => fold_terms(z', rest)
        }
      | None => fold_terms(z, rest)
      }
    };
  };
  let fold_body = (z: Zipper.t, term: Info.t) => {
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
    switch (
      Select.term(
        ~defs_exclude_bodies=false,
        ~case_rules=false,
        CachedSyntax.init(z).term_data,
        id,
        z,
      )
    ) {
    | Some(z') =>
      let z'' =
        ProjectorPerform.go(
          CachedSyntax.init(z').term_data,
          SetIndicated(Specific(Fold)),
          z',
        );
      z'';
    | _ => Ok(z)
    };
  };
  print_endline(
    "curr_node.id: " ++ Uuidm.to_string(Info.id_of(curr_node.info)),
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
  print_endline(
    "def ids of children: "
    ++ String.concat(", ", List.map(Uuidm.to_string, children_def_ids)),
  );
  print_endline(
    "def ids of siblings: "
    ++ String.concat(", ", List.map(Uuidm.to_string, siblings_def_ids)),
  );
  let z = fold_terms(z, children_def_ids);
  let z' = fold_terms(z, siblings_def_ids);

  let z'' =
    switch (curr_node.parent) {
    | Some(parent) =>
      // this switch is a temporary workaround for below mentioned bug
      switch (fold_body(z', parent.info)) {
      | Ok(z'') =>
        switch (
          // Selects the parent node, displaying local code context/map.
          Select.term(
            ~defs_exclude_bodies=false,
            ~case_rules=false,
            CachedSyntax.init(z'').term_data,
            AssistantTreeHelper.id_of(parent),
            z'',
          )
        ) {
        | Some(z''') => z'''
        | None => z''
        }
      | _ => z
      }
    | None => Select.all(z')
    };

  let seg = z''.selection.content;
  print_endline(printer(seg));
  seg;
  // Todo @andrew: Not sure of the perf effects of the below
  // What this does is effectively display the local code map from the parent of the current node,
  // down, along with the current selection (the current node the cursor is at, using the same
  // characters test_editing uses).
  // This effectively Cuts out the def of the parent, pastes it as it's own thing, and then
  // selects the def of the current node.
  // let z = Zipper.init();
  // let z' = Zipper.insert_segment(z, seg);
  // let z'' =
  //   switch (
  //     // Selects the current node, displaying where the cursor selection is.
  //     Select.term(
  //       ~defs_exclude_bodies=true,
  //       ~case_rules=false,
  //       CachedSyntax.init(z').term_data,
  //       AssistantTreeHelper.id_of(curr_node),
  //       z',
  //     )
  //   ) {
  //   | Some(z'') => z''
  //   | None => raise(Failure("Failed to select term"))
  //   };
  // print_endline(printer(z''));
  // z'';
};

let full_definition =
    (z: Zipper.t, curr_node: AssistantTreeHelper.node): string => {
  switch (
    Select.term(
      ~defs_exclude_bodies=true,
      ~case_rules=false,
      CachedSyntax.init(z).term_data,
      AssistantTreeHelper.id_of(curr_node),
      z,
    )
  ) {
  | Some(z'') => printer(z''.selection.content)
  | None => "Failed to derive full definition"
  };
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
            | Ctx.ConstructorEntry(entry) => Ctx.binding_of(ctx, entry.name)
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

let refs_in =
    (
      ~exclude_rec_refs: bool=false,
      ~exclude_body_refs: bool=false,
      node: AssistantTreeHelper.node,
      info_map: Id.Map.t(Info.t),
    )
    : list(Binding.t) => {
  let id = get_def_id_of_let(node.info);
  let refs_of_def = Statics.Map.refs_in(info_map, id);
  let refs_of_node =
    Statics.Map.refs_in(info_map, AssistantTreeHelper.id_of(node));

  // Intersect based on binding IDs
  // This allows us to ignore references in the body AND recursive references in the def
  // (refs_of_def will not have body refs)
  // (refs_of_node will not have recursive refs)
  let intersected_refs =
    ListUtil.intersection_f(
      (b: Binding.t) => b.id,
      refs_of_def,
      refs_of_node,
    );

  let refs =
    switch (exclude_rec_refs, exclude_body_refs) {
    | (true, true) => intersected_refs
    | (true, false) => refs_of_node
    | (false, true) => refs_of_def
    | (false, false) => refs_of_def @ refs_of_node
    };

  // remove duplicates
  let refs' =
    ListUtil.dedup_f((b1: Binding.t, b2: Binding.t) => b1.id == b2.id, refs);

  let refs'' = List.filter((b: Binding.t) => b.id != Id.invalid, refs');

  refs'';
};

let refs_in_str = (refs: list(Binding.t)): string => {
  String.concat(" ", List.map((b: Binding.t) => b.name, refs));
};

let str_of_refs_in = (references: list(Binding.t)): string => {
  "Variables referenced: ["
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

let str_refs_in =
    (
      ~exclude_rec_refs: bool=false,
      ~exclude_body_refs: bool=false,
      node: AssistantTreeHelper.node,
      info_map: Id.Map.t(Info.t),
    )
    : string => {
  refs_in(~exclude_rec_refs, ~exclude_body_refs, node, info_map)
  |> str_of_refs_in;
};
