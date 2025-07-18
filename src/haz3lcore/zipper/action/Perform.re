open Util;
open Zipper;
open Language;

let buffer_clear = (z: t('p)): t('p) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.mk([]),
    }

  | Buffer(Parsed) => z |> Zipper.destruct |> Zipper.regrout(Left)
  | Normal => z
  };

let set_tydi_buffer = (info_map: Language.Statics.Map.t, z: t('p)): t('p) =>
  switch (TyDi.set_buffer(~info_map, z)) {
  | None => z
  | Some(z) => z
  };

let set_llm_buffer = (z: t('p), response: string): t('p) =>
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

let paste = (z: Zipper.t('p), str: string): option(Zipper.t('p)) => {
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

let paste_segment = (z: Zipper.t('p), segment: Segment.t('p)): Zipper.t('p) => {
  let replace_selection = (z, focus, segment): Zipper.t('p) =>
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
      type p',
      type p_kind,
      type p_a,
      ~settings as _: Language.CoreSettings.t,
      ~seg_to_ed,
      ~projector_init,
      ~seg_of_projector as seg_of_pr,
      ~update_projector,
      ~livelit_projectors,
      statics: CachedStatics.t,
      a: Action.t(p_kind, p', p_a),
      module M: Move.S with type p = p',
      z: Zipper.t(p'),
    )
    : Action.Result.t(Zipper.t(p')) => {
  module Move = Move.Make(M);
  module Select = Select.Make(M);

  let buffer_accept = (z): option(Zipper.t(p')) =>
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

  let smart_select = (type p, n, z: t(p)): option(t(p)) => {
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
      ~seg_to_ed,
      ~projector_init,
      ~seg_of_pr,
      ~update_projector,
      ~livelit_projectors,
      ~jump_to_side_of_id=Move.jump_to_side_of_id,
      ~select_term=
        Select.current_term(~defs_exclude_bodies=false, ~case_rules=false),
      a,
      z,
    )
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    //TODO(andrew): cleanup debugging code below
    // let segment =
    //   Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
    // let id = Indicated.index(z);
    // print_endline("LOOKING FORID");
    // switch (id) {
    // | Some(id) => print_endline(Id.to_string(id))
    // | None => print_endline("NONE")
    // };
    // print_endline("INITIAL SEGMENT:");
    // Segment.show((_, _) => (), segment) |> print_endline;
    // switch (id) {
    // | Some(id) =>
    //   let segs = TermRanges.split([id], segment);
    //   let _ =
    //     switch (Id.Map.find_opt(id, segs)) {
    //     | Some(seg) =>
    //       print_endline("FOUND SEGMENT:");
    //       Segment.show((_, _) => (), seg) |> print_endline;
    //       ();
    //     | None =>
    //       print_endline("NO SEGMENT FOUND");
    //       ();
    //     };
    //   ();
    // | None => ()
    // };
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
  };
};
