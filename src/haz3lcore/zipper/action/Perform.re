open Util;
open Zipper;
open Language;

let buffer_clear = (z: t): t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.mk([]),
    }

  | Buffer(Parsed) => z |> Zipper.destroy_selection |> Zipper.regrout(Left)
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
      let* rz = Parser.to_zipper(response);
      switch (Zipper.local_backpack(rz)) {
      | [] =>
        Some(Zipper.set_buffer(z, ~content=Zipper.zip(rz), ~mode=Parsed))
      | _ => None
      };
    }
  ) {
  | None => z
  | Some(z) => z
  };

let paste = (z: Zipper.t, str: string): option(Zipper.t) =>
  Parser.to_zipper(~zipper_init=z, str);

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
      syntax: CachedSyntax.t,
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
          when Token.match(Token.regexp(".*\\)::$"), completion) =>
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
    switch (paste(z, clipboard)) {
    | None => Error(CantPaste)
    | Some(z) => Ok(z)
    }
  | Introduce =>
    Select.current_term(~defs_exclude_bodies=false, ~case_rules=false, z)
    |> Option.bind(_, Introduce.introduce(statics.info_map, _))
    |> Result.of_option(~error=Action.Failure.CantIntroduce)
  | Paste(Segment(segment)) => Ok(paste_segment(z, segment))
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
  | Select(ToggleFocus) => Ok(Zipper.toggle_focus(z))
  | Select(SetFocus(d)) => Ok(Zipper.set_focus(z, d))
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
  | Put_down =>
    (
      switch (z.caret) {
      | Inner(_) => None
      | Outer =>
        switch (Zipper.glom_prev(z)) {
        | Some(z) => Some(z)
        | None => Zipper.put_down_remold_regrout(Left, z)
        }
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | Refractor(SetRefProbe) =>
    switch (z.selection.content) {
    | [] => Ok(Refractors.add(z))
    | _ =>
      let selection_ids = Selection.selection_ids(z.selection);
      let refractors =
        Id.Map.filter(
          (id, _) => !List.mem(id, selection_ids),
          z.refractors,
        );
      Ok({
        ...z,
        refractors,
      });
    }
  | Refractor(InstrumentTerm) =>
    let selection_ids = Selection.selection_ids(z.selection);
    let _selection_ids_with_refractors_on_them =
      Id.Map.filter((id, _) => List.mem(id, selection_ids), z.refractors)
      |> Id.Map.bindings
      |> List.map(((id, _)) => id);
    switch (Indicated.index(z)) {
    | Some(id) =>
      let ids =
        TermData.get_largest_terminal_term_ids(
          id,
          syntax.term_data,
          syntax.measured,
        )
        |> Option.to_list
        |> List.flatten
        |> List.filter_map(Fun.id);
      Ok(List.fold_left((z, id) => Refractors.add'(id, z), z, ids));
    | None => Ok(z)
    };

  | Dump => Ok(Zipper.try_to_dump_backpack(z))
  };
};
