open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type state = {
  zipper: Zipper.t,
  col_target: option(int),
};

type inner_term =
  | Pat
  | Def
  | Body;

let mk_statics = (z: Zipper.t) =>
  Language.(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z).term,
    )
  );

let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
  Result.of_option(~error, z);

let go =
    (
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.t,
      {zipper: z, col_target}: state,
    )
    : Action.Result.t(Zipper.t) =>
  switch (a) {
  | Introduce =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      z,
    )
    |> OptUtil.and_then(
         Introduce.introduce(Indicated.ci_of(z, statics.info_map)),
       )
    |> return(CantIntroduce)
  | Paste(String(clipboard)) =>
    Parser.to_zipper(~zipper_init=z, clipboard) |> return(CantPaste)
  | Paste(Segment(segment)) => Ok(Zipper.insert_segment(z, segment))
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    Destruct.go(Left, z) |> return(Cant_destruct)
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird backpack states */
    Parser.to_zipper(
      ~zipper_init=Zipper.init(),
      Printer.of_zipper(~holes="", ~indent="", z),
    )
    |> return(CantReparse)
  | Buffer(a) => Buffer.go(~ci=Indicated.ci_of(z, statics.info_map), a, z)
  | Project(a) => ProjectorPerform.go(syntax.term_data, a, z)
  | Move(d) =>
    Move.go(
      ~ci=Indicated.ci_of(z, statics.info_map),
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> return(Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(Resize(Local(d, _))) =>
    Select.local(d, z) |> return(Cant_select)
  | Select(Resize(Vertical(d))) =>
    Select.vertical(
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> return(Cant_select)
  | Select(Resize(Start)) => Ok(Select.to_start(z))
  | Select(Resize(End)) => Ok(Select.to_end(z))
  | Select(Resize(Line(d))) =>
    Select.to_linebreak(d, z) |> return(Cant_select)
  | Select(Resize(Point(goal))) =>
    Select.to_point(~measured=syntax.measured, ~goal, z)
    |> return(Cant_select)
  | Select(Resize(Goal(_))) => failwith("Select not implemented for goals")
  | Select(All) => Ok(Select.all(z))
  | Select(Term(Current)) =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=true,
      ~case_rules=true,
      z,
    )
    |> return(Cant_select)
  | Select(Smart(n)) =>
    Select.smart(syntax.term_data, statics.info_map, n, z)
    |> return(Cant_select)
  | Select(Term(Id(id, d))) =>
    switch (
      Select.term(
        ~defs_exclude_bodies=false,
        ~case_rules=false,
        syntax.term_data,
        id,
        z,
      )
    ) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(Tile(Current)) => Select.current_tile(z) |> return(Cant_select)
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(ToggleFocus) => Ok(Zipper.toggle_focus(z))
  | Select(SetFocus(d)) => Ok(Zipper.set_focus(z, d))
  | Destruct(d) => Destruct.go(d, z) |> return(Cant_destruct)
  | Insert(char) =>
    z
    |> Insert.go(char, ~ci=Indicated.ci_of(z, statics.info_map))
    |> return(Cant_insert)
  | Put_down => Zipper.put_down_glom(z) |> return(Cant_put_down)
  | Dump => Ok(Zipper.try_to_dump_backpack(z))
  | AssistantComposition(a) =>
    let get_inner_term_id =
        (curr_node_info: AssistantTreeHelper.node, inner_term: inner_term)
        : Id.t => {
      switch (curr_node_info.info) {
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
            Failure("Current node is not a let or type alias expression"),
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
    // Tempory wrapper that helps me localize myself while implementing (remove)
    let handle_composition_action = (node: AssistantTreeHelper.node) => {
      switch (a) {
      | Nav(n) =>
        switch (n) {
        | GoToParent =>
          switch (node.parent) {
          | None => Error(Action.Failure.Cant_move)
          | Some(parent) =>
            switch (Select.tile(Info.id_of(parent.info), z)) {
            | Some(z) => Ok(z)
            | None => Error(Action.Failure.Cant_select)
            }
          }
        | GoToChild(who, which) =>
          switch (which) {
          | None =>
            // the llm provided no index, thus, use the name
            let cands =
              List.filter(
                (child: AssistantTreeHelper.node) => child.name == who,
                node.children,
              );
            if (List.length(cands) > 1) {
              Error(Action.Failure.Cant_move);
            } else {
              switch (ListUtil.hd_opt(cands)) {
              | None => Error(Action.Failure.Cant_move)
              | Some(child) =>
                switch (Select.tile(Info.id_of(child.info), z)) {
                | Some(z) => Ok(z)
                | None => Error(Action.Failure.Cant_select)
                }
              };
            };
          | Some(nth) =>
            // this means the llm provided an index to move to, in which case
            // we default on using that as opposed to the name
            switch (List.nth_opt(node.children, nth)) {
            | None => Error(Action.Failure.Cant_move)
            | Some(child) =>
              switch (Select.tile(Info.id_of(child.info), z)) {
              | Some(z) => Ok(z)
              | None => Error(Action.Failure.Cant_select)
              }
            }
          }
        | GoToSibling(via) =>
          switch (via) {
          | NameAndIdx(who, which) =>
            switch (which) {
            | None =>
              // the llm provided no index, thus, use the name
              let cands =
                List.filter(
                  (sibling: AssistantTreeHelper.node) => sibling.name == who,
                  node.siblings,
                );
              if (List.length(cands) > 1) {
                Error(Action.Failure.Cant_move);
              } else {
                switch (ListUtil.hd_opt(cands)) {
                | None => Error(Action.Failure.Cant_move)
                | Some(sibling) =>
                  switch (Select.tile(Info.id_of(sibling.info), z)) {
                  | Some(z) => Ok(z)
                  | None => Error(Action.Failure.Cant_select)
                  }
                };
              };
            | Some(nth) =>
              // this means the llm provided an index to move to, in which case
              // we default on using that as opposed to the name
              switch (List.nth_opt(node.siblings, nth)) {
              | None => Error(Action.Failure.Cant_move)
              | Some(sibling) =>
                switch (Select.tile(Info.id_of(sibling.info), z)) {
                | Some(z) => Ok(z)
                | None => Error(Action.Failure.Cant_select)
                }
              }
            }
          | Stepwise(d) =>
            let len = List.length(node.siblings);
            print_endline(
              "node.sibling_idx: "
              ++ string_of_int(node.sibling_idx)
              ++ " len: "
              ++ string_of_int(len),
            );
            let target_id =
              switch (d) {
              | Left =>
                List.nth(node.siblings, (node.sibling_idx - 1 + len) mod len).
                  info
                |> Info.id_of
              | Right =>
                // Don't add 1 here because we filtered out the current node
                List.nth(node.siblings, (node.sibling_idx + len) mod len).info
                |> Info.id_of
              };
            switch (Select.tile(target_id, z)) {
            | Some(z) => Ok(z)
            | None => Error(Action.Failure.Cant_select)
            };
          }
        }
      | Read(_r) => Ok(z) // todo
      | Edit(e) =>
        // let select_curr_node = z => {
        //   // Recalculate info map with new statics and measurements to assert fresh, non-stale state
        //   let curr_node_info =
        //     AssistantTreeHelper.build_curr_node_info(z, mk_statics(z));
        //   switch (curr_node_info) {
        //   | Some(node) =>
        //     switch (Select.tile(Info.id_of(node.info), z)) {
        //     | Some(z) => Ok(z)
        //     | None => Error(Action.Failure.Cant_select)
        //     }
        //   | None => Error(Action.Failure.Cant_derive_local_AST_information)
        //   };
        // };
        let overwrite_term = (z, target_id, code) => {
          // Select the respective tile (in this case the definition tile)
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data, // todo: not sure about this arg
              target_id,
              z,
            )
          ) {
          | Some(z') =>
            // Paste the code over the selected tile
            Parser.to_zipper(~zipper_init=z', code) |> return(CantPaste)
          | None => Error(Action.Failure.Cant_select)
          };
        };
        let insert_term = (z, target_id, code, direction) => {
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data, // todo: not sure about this arg
              target_id,
              z,
            )
          ) {
          | Some(z') =>
            switch (Move.by_token(direction, z')) {
            | Some(z'') =>
              Parser.to_zipper(~zipper_init=z'', code) |> return(CantPaste)
            | None => Error(Action.Failure.Cant_move)
            }
          | None => Error(Action.Failure.Cant_select)
          };
        };
        let rec destruct_term = (~defs_exclude_bodies, z, target_id) => {
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
        switch (e) {
        | UpdateDefinition(code) =>
          let target_id = get_inner_term_id(node, Def);
          overwrite_term(z, target_id, code);
        | UpdateBody(code) =>
          let target_id = get_inner_term_id(node, Body);
          overwrite_term(z, target_id, code);
        | UpdatePattern(code) =>
          let target_id = get_inner_term_id(node, Pat);
          overwrite_term(z, target_id, code);
        | UpdateBindingClause(code) =>
          let target_id = Info.id_of(node.info);
          overwrite_term(z, target_id, code);
        | DeleteBindingClause =>
          destruct_term(~defs_exclude_bodies=true, z, Info.id_of(node.info))
        | DeleteBody =>
          let target_id = get_inner_term_id(node, Body);
          destruct_term(~defs_exclude_bodies=false, z, target_id);
        | InsertBefore(code) =>
          insert_term(z, Info.id_of(node.info), code ++ "\n", Direction.Left)
        | InsertAfter(code) =>
          insert_term(
            z,
            Info.id_of(node.info),
            "\n" ++ code,
            Direction.Right,
          )
        };
      };
    };
    let curr_node_info =
      AssistantTreeHelper.build_curr_node_info(z, mk_statics(z));
    switch (curr_node_info) {
    | Some(node) => handle_composition_action(node)
    | None => Error(Action.Failure.Cant_derive_local_AST_information) //todo, add failure case
    };
  };
