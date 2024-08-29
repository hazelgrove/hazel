open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | Project(_) => false //TODO(andrew): revisit
  | Copy
  | Move(_)
  | Unselect(_)
  | Jump(_)
  | Select(_) => false
  | Buffer(Set(_) | Accept | Clear)
  | Cut
  | Paste(_)
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Pick_up
  | Put_down
  | RotateBackpack
  | MoveToBackpackTarget(_) => true
  };
};

let buffer_clear = (z: t): t =>
  switch (z.selection.mode) {
  | Buffer(_) => {...z, selection: Selection.mk([])}
  | _ => z
  };

let set_buffer = (info_map: Statics.Map.t, z: t): t =>
  switch (TyDi.set_buffer(~info_map, z)) {
  | None => z
  | Some(z) => z
  };

let go_z =
    (
      ~meta: option(Editor.Meta.t)=?,
      ~settings: CoreSettings.t,
      a: Action.t,
      z: Zipper.t,
    )
    : Action.Result.t(Zipper.t) => {
  let meta =
    switch (meta) {
    | Some(m) => m
    | None => Editor.Meta.init(z, ~settings)
    };
  module M = (val Editor.Meta.module_of_t(meta));
  module Move = Move.Make(M);
  module Select = Select.Make(M);

  let select_term_current = z =>
    switch (Indicated.index(z)) {
    | None => Error(Action.Failure.Cant_select)
    | Some(id) =>
      switch (Select.term(id, z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    };

  let paste = (z: Zipper.t, str: string): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* z = Printer.zipper_of_string(~zipper_init=z, str);
    /* HACK(andrew): Insert/Destruct below is a hack to deal
       with the fact that pasting something like "let a = b in"
       won't trigger the barfing of the "in"; to trigger this,
       we insert a space, and then we immediately delete it */
    let* z = Insert.go(" ", z);
    let+ z = Destruct.go(Left, z);
    remold_regrout(Left, z);
  };

  let buffer_accept = (z): option(Zipper.t) =>
    switch (z.selection.mode) {
    | Normal => None
    | Buffer(Unparsed) =>
      switch (TyDi.get_buffer(z)) {
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

  switch (a) {
  | Paste(clipboard) =>
    switch (paste(z, clipboard)) {
    | None => Error(CantPaste)
    | Some(z) => Ok(z)
    }
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
    switch (Printer.reparse(z)) {
    | None => Error(CantReparse)
    | Some(z) => Ok(z)
    }
  | Buffer(Set(TyDi)) => Ok(set_buffer(meta.statics.info_map, z))
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
      a,
      z,
    )
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    let idx = Indicated.index(z);
    let statics = meta.statics.info_map;
    (
      switch (jump_target) {
      | BindingSiteOfIndicatedVar =>
        open OptUtil.Syntax;
        let* idx = idx;
        let* ci = Id.Map.find_opt(idx, statics);
        let* binding_id = Info.get_binding_site(ci);
        Move.jump_to_id(z, binding_id);
      | TileId(id) => Move.jump_to_id(z, id)
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_move);
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) =>
    let z = Zipper.directional_unselect(z.selection.focus, z);
    Ok(z);
  | Select(All) =>
    switch (Move.do_extreme(Move.primary(ByToken), Up, z)) {
    | Some(z) =>
      switch (Select.go(Extreme(Down), z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Term(Current)) => select_term_current(z)
  | Select(Smart) =>
    /* If the current tile is not coincident with the term,
       select the term. Otherwise, select the parent term. */
    let tile_is_term =
      switch (Indicated.index(z)) {
      | None => false
      | Some(id) => Select.tile(id, z) == Select.term(id, z)
      };
    if (!tile_is_term) {
      select_term_current(z);
    } else {
      let statics = meta.statics.info_map;
      let target =
        switch (
          Indicated.index(z)
          |> OptUtil.and_then(idx => Id.Map.find_opt(idx, statics))
        ) {
        | Some(ci) =>
          switch (Info.ancestors_of(ci)) {
          | [] => None
          | [parent, ..._] => Some(parent)
          }
        | None => None
        };
      switch (target) {
      | None => Error(Action.Failure.Cant_select)
      | Some(id) =>
        switch (Select.term(id, z)) {
        | Some(z) => Ok(z)
        | None => Error(Action.Failure.Cant_select)
        }
      };
    };
  | Select(Term(Id(id, d))) =>
    switch (Select.term(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Tile(Current)) =>
    switch (Indicated.index(z)) {
    | None => Error(Action.Failure.Cant_select)
    | Some(id) =>
      switch (Select.tile(id, z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    }
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Resize(d)) =>
    Select.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_select)
  | Destruct(d) =>
    z
    |> Destruct.go(d)
    |> Option.map(remold_regrout(d))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    z
    |> Insert.go(char)
    /* note: remolding here is done case-by-case */
    //|> Option.map((z) => remold_regrout(Right, z))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Zipper.pick_up(z)))
  | Put_down =>
    let z =
      /* Alternatively, putting down inside token could eiter merge-in or split */
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.put_down(Left, z)
      };
    z
    |> Option.map(remold_regrout(Left))
    |> Result.of_option(~error=Action.Failure.Cant_put_down);
  | RotateBackpack =>
    let z = {...z, backpack: Util.ListUtil.rotate(z.backpack)};
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

let go_history =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) => {
  open Result.Syntax;
  /* This function is responsible for the action history */
  let Editor.State.{zipper, meta} = ed.state;
  //Effect.s_clear();
  let+ z = go_z(~settings, ~meta, a, zipper);
  Editor.new_state(
    /*~effects=Effect.s^,*/
    ~settings,
    a,
    z,
    ed,
  );
};

let go =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) =>
  /* This function wraps assistant completions. If completions are enabled,
   * then beginning any action (other than accepting a completion) clears
   * the completion buffer before performing the action. Conversely,
   * after any edit action, a new completion is set in the buffer */
  if (ed.read_only && is_write_action(a)) {
    Ok(ed);
  } else if (settings.assist && settings.statics) {
    open Result.Syntax;
    let ed =
      a == Buffer(Accept)
        ? ed
        : (
          switch (go_history(~settings, Buffer(Clear), ed)) {
          | Ok(ed) => ed
          | Error(_) => ed
          }
        );
    let* ed = go_history(~settings, a, ed);
    Action.is_edit(a)
      ? {
        switch (go_history(~settings, Buffer(Set(TyDi)), ed)) {
        | Error(err) => Error(err)
        | Ok(ed) => Ok(ed)
        };
      }
      : Ok(ed);
  } else {
    go_history(~settings, a, ed);
  };
