open Language;
open Haz3lcore;
open Util;
open AssistantTreeHelper;

let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics, _}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let map = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Language.Exp.show |> print
  | "F4" => map |> Language.Statics.Map.show |> print
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
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Language.Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F9" =>
    let is_on_whitespace = (z: Zipper.t): bool => {
      // Use for_index which only ignores secondary pieces, not grout pieces
      switch (Indicated.for_index(z)) {
      | Some((piece, _, _)) =>
        Piece.is_secondary(piece)
        || Piece.is_grout(piece)
        || Piece.is_convex(piece)
      | None => false
      };
    };
    print_endline(
      "is on whitespace: " ++ string_of_bool(is_on_whitespace(zipper)),
    );

    // switch (info) {
    // | InfoExp(info) =>
    //   let ctx = info.ctx;
    //   let entries =
    //     List.map(
    //       (entry: Ctx.entry) => {
    //         switch (entry) {
    //         | VarEntry(entry) => entry.name
    //         | TVarEntry(entry) => entry.name
    //         | ConstructorEntry(entry) => entry.name
    //         | _ => "Unknown entry"
    //         }
    //       },
    //       ctx.entries,
    //     );
    //   print("ctx: " ++ String.concat("\n", entries));
    // | _ => print("DEBUG: No context found for info")
    // };
    ();
  | "F10" =>
    let context = (local_information: node): string => {
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
             ", ",
             List.mapi(
               (i: int, b: Binding.t) =>
                 b.name ++ " (Index: " ++ string_of_int(i) ++ ")",
               bindings,
             ),
           )
        ++ "]";
      | _ => ""
      };
    };
    print(
      context(
        get_node(
          build_sub_AST(editor.editor.state.zipper, editor.statics.info_map),
        ),
      ),
    );
  | "F11" =>
    //simple curr node id print
    let curr_node =
      get_node(
        AssistantTreeHelper.build_sub_AST(
          editor.editor.state.zipper,
          editor.statics.info_map,
        ),
      );
    print("curr node id: " ++ Id.to_string(Info.id_of(curr_node.info)));
  | "F12" =>
    let rec curr_node_of =
            (candidate: option(Info.t), info_map: Id.Map.t(Info.t))
            : option(node) => {
      switch (candidate) {
      | Some(candidate) =>
        switch (candidate) {
        | InfoExp({term, _}) =>
          switch (Exp.term_of(term)) {
          | Let(pat, _, _) =>
            Some({
              info: candidate,
              parent: None,
              siblings: [],
              children: [],
              sibling_idx: 0,
              name: mk_name_from_pat(pat),
            })
          | TyAlias(tpat, _, _) =>
            Some({
              info: candidate,
              parent: None,
              siblings: [],
              children: [],
              sibling_idx: 0,
              name: mk_name_from_tpat(tpat),
            })
          | _ =>
            switch (Info.ancestors_of(candidate)) {
            | [ancestor, ..._] =>
              curr_node_of(Id.Map.find_opt(ancestor, info_map), info_map)
            | _ => None
            }
          }
        | _ =>
          switch (Info.ancestors_of(candidate)) {
          | [ancestor, ..._] =>
            curr_node_of(Id.Map.find_opt(ancestor, info_map), info_map)
          | _ => None
          }
        }
      | None => None
      };
    };
    let info_map = editor.statics.info_map;
    let zipper = move_to_non_whitespace(zipper);
    let curr_term = Indicated.ci_of(zipper, info_map);
    // Test the curr_node_of function
    print_endline("Testing curr_node_of function:");
    switch (curr_node_of(curr_term, info_map)) {
    | Some(node) => print_endline("Found node: " ++ node.name)
    | None => print_endline("No node found")
    };

  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
