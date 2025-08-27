open Util;
open Zipper;
open Language;

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

let buffer_clear = (z: t): t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.mk([]),
    }

  | Buffer(Parsed) => z |> Zipper.destruct |> Zipper.regrout(Left)
  | Normal => z
  };

let set_tydi_buffer = (info_map: Language.Statics.Map.t, z: t): t =>
  switch (TyDi.set_buffer(~info_map, z)) {
  | None => z
  | Some(z) => z
  };

let set_llm_buffer = (z: t, response: string): t =>
  switch (
    {
      open OptUtil.Syntax;
      //TODO: Error feedback on below
      let* content = Parser.to_zipper(response);
      let+ _ = [] == content.backpack ? Some() : None;
      Zipper.set_buffer(z, ~content=Zipper.zip(content), ~mode=Parsed);
    }
  ) {
  | None => z
  | Some(z) => z
  };

let paste = (z: Zipper.t, str: string): option(Zipper.t) => {
  open Util.OptUtil.Syntax;
  let* z = Parser.to_zipper(~zipper_init=z, str);
  /* HACK(andrew): Insert/Destruct below is a hack to deal
     with the fact that pasting something like "let a = b in"
     won't trigger the barfing of the "in"; to trigger this,
     we insert a space, and then we immediately delete it */
  let* z = Insert.go(" ", z);
  let+ z = Destruct.go(Left, z);
  remold_regrout(Left, z);
};

let paste_segment = (z: Zipper.t, segment: Segment.t): Zipper.t => {
  let replace_selection = (z, focus, segment): Zipper.t =>
    {
      ...z,
      selection: Selection.mk(~focus, segment),
    }
    |> Zipper.unselect
    |> Zipper.remold_regrout(Util.Direction.Right)
    |> Zipper.remold_regrout(Util.Direction.Left);
  replace_selection(z, z.selection.focus, segment);
};

let go_z =
    (
      ~settings as _: Language.CoreSettings.t,
      statics: CachedStatics.t,
      a: Action.t,
      module M: Move.S,
      z: Zipper.t,
    )
    : Action.Result.t(Zipper.t) => {
  module Move = Move.Make(M);
  module Select = Select.Make(M);

  let buffer_accept = (z): option(Zipper.t) =>
    switch (z.selection.mode) {
    | Normal => None
    | Buffer(Parsed) =>
      let z = Zipper.directional_unselect(Right, z);
      Some(z);
    | Buffer(Unparsed) =>
      switch (TyDi.get_unparsed_buffer(z)) {
      | None => None
      | Some(completion)
          when StringUtil.match(StringUtil.regexp(".*\\)::$"), completion) =>
        /* Slightly hacky. There's currently only one genre of completion
         * that creates more than one hole on intial expansion: when on eg
         * 1 :: a|, we suggest "abs( )::" via lookahead. In such a case we
         * want the caret to end up to the left of the first hole, whereas
         * pasting would leave it to the left of the second. Thus we move
         * left to the previous hole. */
        let z = {
          open OptUtil.Syntax;
          let* z = paste(z, completion);
          let* z = Move.go(Goal(Piece(Grout, Left)), z);
          Move.go(Local(Left(ByToken)), z);
        };
        z;
      | Some(completion) => paste(z, completion)
      }
    };

  let smart_select = (n, z: t): option(Zipper.t) => {
    switch (n) {
    | 2 => Select.indicated_token(z)
    | 3 =>
      open OptUtil.Syntax;
      /* For things where triple-clicking would otherwise have
       * no additional effect, select the parent term instead */
      let* (p, _, _) = Indicated.piece''(z);
      Piece.is_term(p)
        ? Select.parent_of_indicated(z, statics.info_map)
        : Select.current_term(~defs_exclude_bodies=true, ~case_rules=true, z);
    | _ => None
    };
  };

  switch (a) {
  | Paste(String(clipboard)) =>
    print_endline("Here #3 : Pasting Sketch");
    switch (paste(z, clipboard)) {
    | None => Error(CantPaste)
    | Some(z) => Ok(z)
    };
  | Introduce =>
    Select.current_term(~defs_exclude_bodies=false, ~case_rules=false, z)
    |> Option.bind(_, Introduce.introduce(statics.info_map, _))
    |> Result.of_option(~error=Action.Failure.CantIntroduce)
  | Paste(Segment(segment)) => Ok(paste_segment(z, segment))
  | Paste(Assistant(code)) =>
    // trim leading whitespace in assistant code
    let code' = code |> StringUtil.trim_leading;
    switch (paste(z, code')) {
    | None => Error(CantPaste)
    | Some(z') => Ok(Zipper.try_to_dump_backpack(z'))
    };
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    switch (Destruct.go(Left, z)) {
    | None => Error(Cant_destruct)
    | Some(z) => Ok(z)
    }
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird backpack states */
    let reparse = z =>
      Parser.to_zipper(
        ~zipper_init=Zipper.init(),
        Printer.of_zipper(~holes="", ~indent="", z),
      );
    switch (reparse(z)) {
    | None => Error(CantReparse)
    | Some(z) => Ok(z)
    };
  | Buffer(Set(TyDi)) => Ok(set_tydi_buffer(statics.info_map, z))
  | Buffer(Set(LLM(response))) => Ok(set_llm_buffer(z, response))
  | Buffer(Accept) =>
    switch (buffer_accept(z)) {
    | None => Error(CantAccept)
    | Some(z) => Ok(z)
    }
  | Buffer(Clear) => Ok(buffer_clear(z))
  | Project(a) =>
    ProjectorPerform.go(
      Move.jump_to_id_indicated,
      Move.jump_to_side_of_id,
      Select.current_term(~defs_exclude_bodies=false, ~case_rules=false),
      a,
      z,
    )
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    (
      switch (jump_target) {
      | BindingSiteOfIndicatedVar =>
        open OptUtil.Syntax;
        let* idx = Indicated.index(z);
        let* ci = Id.Map.find_opt(idx, statics.info_map);
        let* binding_id = Language.Info.get_binding_site(ci);
        Move.jump_to_id_indicated(z, binding_id);
      | TileId(id) => Move.jump_to_id_indicated(z, id)
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(All) =>
    let z =
      switch (Move.do_extreme(Move.primary(ByToken), Up, z)) {
      | Some(z) => z
      | None => z
      };
    switch (Select.go(Extreme(Down), z)) {
    | Some(z) => Ok(z)
    | None => Error(Action.Failure.Cant_select)
    };
  | Select(Term(Current)) =>
    switch (
      Select.current_term(~defs_exclude_bodies=true, ~case_rules=true, z)
    ) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Smart(n)) =>
    switch (smart_select(n, z)) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Term(Id(id, d))) =>
    switch (Select.term(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Tile(Current)) =>
    switch (Select.current_tile(z)) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Resize(d)) =>
    switch (Select.go(d, z)) {
    | None => Ok(z)
    | Some(z) => Ok(z)
    }
  | Restore(sketch) => Ok(sketch)
  | Destruct(d) =>
    z
    |> Destruct.go(d)
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    let id =
      switch (Indicated.index(z)) {
      | Some(id) => id
      | None => Id.invalid
      };

    let ctx =
      switch (Id.Map.find_opt(id, statics.info_map)) {
      | Some(ci) => Info.ctx_of(ci)
      | None => Ctx.empty
      };

    z
    |> Insert.go(char, ~ctx)
    /* note: remolding here is done case-by-case */
    |> Result.of_option(~error=Action.Failure.Cant_insert);
  | Pick_up => Ok(remold_regrout(Left, Zipper.pick_up(z)))
  | Put_down =>
    let z =
      /* Alternatively, putting down inside token could eiter merge-in or split */
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.put_down_regrout_remold(Left, z)
      };
    z |> Result.of_option(~error=Action.Failure.Cant_put_down);
  | RotateBackpack =>
    let z = {
      ...z,
      backpack: Util.ListUtil.rotate(z.backpack),
    };
    Ok(z);
  | MoveToBackpackTarget((Left(_) | Right(_)) as d) =>
    if (Backpack.restricted(z.backpack)) {
      Move.to_backpack_target(d, z)
      |> Result.of_option(~error=Action.Failure.Cant_move);
    } else {
      Move.go(Local(d), z)
      |> Result.of_option(~error=Action.Failure.Cant_move);
    }
  | MoveToBackpackTarget((Up | Down) as d) =>
    Move.to_backpack_target(d, z)
    |> Result.of_option(~error=Action.Failure.Cant_move)
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
            let self_idx =
              List.find_index(
                (sibling: AssistantTreeHelper.node) =>
                  Info.id_of(sibling.info) == Info.id_of(node.info),
                node.siblings,
              );
            let target_id =
              switch (self_idx) {
              | None => Id.invalid
              | Some(idx) =>
                switch (d) {
                | Left =>
                  List.nth(node.siblings, (idx - 1 + len) mod len).info
                  |> Info.id_of
                | Right =>
                  List.nth(node.siblings, (idx + 1 + len) mod len).info
                  |> Info.id_of
                }
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
        let overwrite_tile = (z, target_id, code) => {
          // Select the respective tile (in this case the definition tile)
          switch (Select.tile(target_id, z)) {
          | Some(z') =>
            // Paste the code over the selected tile
            switch (paste(z', code)) {
            | Some(z'') => Ok(z'')
            | None => Error(Action.Failure.CantPaste)
            }
          | None => Error(Action.Failure.Cant_select)
          };
        };
        let destruct_tile = (z, target_id) => {
          switch (Select.tile(target_id, z)) {
          | Some(z') =>
            switch (Destruct.go(Left, z')) {
            | None => Error(Action.Failure.Cant_destruct)
            | Some(z'') => Ok(z'')
            }
          | None => Error(Action.Failure.Cant_destruct)
          };
        };
        switch (e) {
        | UpdateDefinition(code) =>
          let target_id = get_inner_term_id(node, Def);
          overwrite_tile(z, target_id, code);
        | UpdateBody(code) =>
          let target_id = get_inner_term_id(node, Body);
          overwrite_tile(z, target_id, code);
        | UpdatePattern(code) =>
          let target_id = get_inner_term_id(node, Pat);
          overwrite_tile(z, target_id, code);
        | UpdateExpression(code) =>
          let target_id = Info.id_of(node.info);
          overwrite_tile(z, target_id, code);
        | DeleteExpression => destruct_tile(z, Info.id_of(node.info))
        | DeleteBody =>
          let target_id = get_inner_term_id(node, Body);
          destruct_tile(z, target_id);
        | InsertBefore(code) =>
          switch (Move.go(Extreme(Left(ByToken)), z)) {
          | Some(z') =>
            switch (paste(z', code)) {
            | Some(z'') => Ok(z'')
            | None => Error(Action.Failure.CantPaste)
            }
          | None => Error(Action.Failure.Cant_move)
          }
        | InsertAfter(code) =>
          switch (Move.go(Extreme(Right(ByToken)), z)) {
          | Some(z') =>
            switch (paste(z', code)) {
            | Some(z'') => Ok(z'')
            | None => Error(Action.Failure.CantPaste)
            }
          | None => Error(Action.Failure.Cant_move)
          }
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
};
