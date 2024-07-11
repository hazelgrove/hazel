open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | RecalcStatics
  | Project(_) => false //TODO(andrew): revisit
  | Copy
  | Move(_)
  | Unselect(_)
  | Jump(_)
  | Select(_) => false
  | Buffer(Set(_) | Clear | Accept)
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

let rec go_z =
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
    /* HACK(andrew): The two perform calls are a hack to
       deal with the fact that pasting something like "let a = b in"
       won't trigger the barfing of the "in"; to trigger this, we
       insert a space, and then we immediately delete it. Note
       that in general it's not safe to call go_z recursively
       (the meta may change) but as long as we're not relying
       on anything but the raw zipper we're fine. */
    let settings = CoreSettings.off;
    let* z = Printer.zipper_of_string(~zipper_init=z, str);
    switch (go_z(~settings, Insert(" "), z)) {
    | Error(_) => None
    | Ok(z) =>
      switch (go_z(~settings, Destruct(Left), z)) {
      | Error(_) => None
      | Ok(z) => Some(z)
      }
    };
  };

  switch (a) {
  | Paste(clipboard) =>
    switch (paste(z, clipboard)) {
    | None => Error(CantPaste)
    | Some(z) => Ok(z)
    }
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    go_z(~settings, ~meta, Destruct(Left), z)
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    switch (Printer.reparse(z)) {
    | None => Error(CantReparse)
    | Some(z) => Ok(z)
    }
  | Buffer(Clear) =>
    switch (z.selection.mode) {
    | Buffer(_) => Ok({...z, selection: Selection.mk([])})
    | _ => Ok(z)
    }
  | Buffer(Set(TyDi)) =>
    let info_map = meta.statics.info_map;
    switch (TyDi.set_buffer(~info_map, z)) {
    | None => Ok(z)
    | Some(z) => Ok(z)
    };
  | Buffer(Accept) =>
    switch (z.selection.mode) {
    | Normal => Ok(z)
    | Buffer(Unparsed) =>
      switch (TyDi.get_buffer(z)) {
      | None => Error(CantAccept)
      | Some(completion) when String.contains(completion, ' ') =>
        /* Slightly hacky. We assume that if a completion string has
         * spaces in it, that means it will have a hole in it. This
         * is a non-essential invariant currently maintained in TyDi.
         * In such a case, we insert the completion as normal by
         * pasting, then return to the beginning and advance to the
         * first hole. This should be revisited if completions are
         * refactored to use a more structured buffer format.
         *
         * Note that calling go_z like this without recalculating
         * the meta is iffy; the goal movement only works because
         * the paste only changes measurements south of the start */
        let start = Zipper.caret_point(M.measured, meta.projected.z);
        let rec do_actions = (z, actions: list(Action.t)) =>
          switch (actions) {
          | [] => Ok(z)
          | [a, ...tl] =>
            switch (go_z(~meta, ~settings, a, z)) {
            | Error(err) => Error(err)
            | Ok(z) => do_actions(z, tl)
            }
          };
        do_actions(
          z,
          [
            Paste(AssistantExpander.trim(completion)),
            Move(Goal(Point(start))),
            Move(Goal(Piece(Grout, Right))),
            Move(Local(Left(ByToken))),
          ],
        );
      | Some(completion) =>
        go_z(~meta, ~settings, Paste(AssistantExpander.trim(completion)), z)
      }
    }
  | RecalcStatics =>
    print_endline("RecalcStatics action called");
    Ok(z);
  | Project(a) =>
    ProjectorPerform.go(
      Move.jump_to_id,
      Move.primary,
      a,
      meta.projected.syntax_map,
      z,
    )
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    open OptUtil.Syntax;

    let idx = Indicated.index(z);
    let statics = meta.statics.info_map;

    (
      switch (jump_target) {
      | BindingSiteOfIndicatedVar =>
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

let go_base =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) =>
  /* This function enforces read-only editors and
   * is responsible for mainting the action history */
  if (ed.read_only && is_write_action(a)) {
    Result.Ok(ed);
  } else {
    open Result.Syntax;
    let Editor.State.{zipper, meta} = ed.state;
    Effect.s_clear();
    let+ z = go_z(~settings, ~meta, a, zipper);
    Editor.new_state(~effects=Effect.s^, ~settings, a, z, ed);
  };

let go =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) =>
  /* This function wraps assistant completions. If completions are enabled,
   * then beginning any action (other than accepting a completion) clears
   * the completion buffer before performing the action. Conversely,
   * after any edit action, a new completion is set in the buffer */
  if (settings.assist && settings.statics) {
    open Result.Syntax;
    let* ed =
      a == Buffer(Accept) ? Ok(ed) : go_base(~settings, Buffer(Clear), ed);
    let* ed = go_base(~settings, a, ed);
    Action.is_edit(a)
      ? go_base(~settings, Buffer(Set(TyDi)), ed) : Ok(ed);
  } else {
    go_base(~settings, a, ed);
  };
