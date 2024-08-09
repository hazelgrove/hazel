open Util;
open Zipper;

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

  let smart_select = (n, z): option(Zipper.t) => {
    open OptUtil.Syntax;
    let statics_of = Id.Map.find_opt(_, meta.statics.info_map);
    let select_parent = z => {
      let* ci = Indicated.index(z) |> OptUtil.and_then(statics_of);
      let* id =
        switch (Info.ancestors_of(ci)) {
        | [] => None
        | [parent, ..._] => Some(parent)
        };
      let* ci_parent = statics_of(id);
      switch (Info.cls_of(ci_parent)) {
      | Exp(Let | TyAlias) =>
        /* For definition-type forms, don't select the body */
        Select.tile(id, z)
      | Exp(Match) =>
        /* Case rules aren't terms in the syntax model,
         * but here we pretend they are */
        let* z = Move.left_until_case_or_rule(z);
        switch (Indicated.piece''(z)) {
        | Some((p, _, _)) =>
          switch (p) {
          | Tile({label: ["|", "=>"], _}) => Select.containing_rule(z)
          | Tile({label: ["case", "end"], _}) => Select.term(id, z)
          | _ => None
          }
        | _ => None
        };
      | _ =>
        switch (Info.ancestors_of(ci_parent)) {
        | [] => Select.term(id, z)
        | [gp, ..._] =>
          let* ci_gp = statics_of(gp);
          switch (Info.cls_of(ci_parent), Info.cls_of(ci_gp)) {
          | (
              Exp(Tuple) | Pat(Tuple) | Typ(Prod),
              Exp(Parens) | Pat(Parens) | Typ(Parens),
            ) =>
            /* If parent is tuple, check if it's in parens,
             * and if so, select the parens as well */
            Select.term(gp, z)
          | _ => Select.term(id, z)
          };
        }
      };
    };
    switch (n) {
    | 2 =>
      switch (Indicated.piece'(~no_ws=false, ~ign=Piece.is_secondary, z)) {
      | Some((p, Left, _)) when !Piece.is_secondary(p) && z.caret == Outer =>
        /* If we're on the far right side of something, we
         * still want to select it versus secondary to the right */
        let* z = Move.go(Local(Left(ByToken)), z);
        Select.go(Local(Right(ByToken)), z);
      | Some((p, _, _)) when !Piece.is_secondary(p) =>
        Select.go(Local(Right(ByToken)), z)
      | Some((Secondary(_), _, _)) =>
        /* If there is secondary on both sides, select the
         * largest contiguous run of non-linebreak secondary */
        Select.containing_secondary_run(z)
      | _ => None
      }
    | 3 =>
      switch (Indicated.piece''(z)) {
      | Some((p, _, _)) =>
        switch (p) {
        | Secondary(_) => failwith("Perform.Select Smart impossible")
        | Grout(_)
        | Projector(_)
        | Tile({
            label: [_],
            mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
            _,
          }) =>
          /* For things where triple-clicking would otherwise have
           * no additional effect, select the parent term instead */
          select_parent(z)
        | Tile({label: ["let" | "type", ..._], _}) => Select.current_tile(z)
        | Tile({label: ["|", "=>"], _}) => Select.containing_rule(z)
        | Tile(t) =>
          switch (t.label, Zipper.parent(z)) {
          | ([","], Some(Tile({label: ["[", "]"] | ["(", ")"], id, _}))) =>
            Select.term(id, z)
          | _ => Select.current_term(z)
          }
        }
      | _ => None
      }
    | _ => None
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
  | Select(Term(Current)) =>
    switch (Select.current_term(z)) {
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
  /* This function records action history */
  let Editor.State.{zipper, meta} = ed.state;
  let+ z = go_z(~settings, ~meta, a, zipper);
  Editor.new_state(~settings, a, z, ed);
};

let go =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) =>
  /* This function wraps assistant completions. If completions are enabled,
   * then beginning any action (other than accepting a completion) clears
   * the completion buffer before performing the action. Conversely,
   * after any edit action, a new completion is set in the buffer */
  if (ed.read_only && Action.prevent_in_read_only_editor(a)) {
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
