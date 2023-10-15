open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | Move(_)
  | MoveToNextHole(_)
  | Click(_)
  | Unselect
  | Jump(_)
  | Select(_) => false
  | Destruct(_)
  | Insert(_)
  | Pick_up
  | Put_down
  | RotateBackpack
  | MoveToBackpackTarget(_) => true
  };
};

let go_z =
    (~meta: option(Editor.Meta.t)=?, a: Action.t, z: Zipper.t)
    : Action.Result.t(Zipper.t) => {
  let meta =
    switch (meta) {
    | Some(m) => m
    | None => Editor.Meta.init(z)
    };
  module M = (val Editor.Meta.module_of_t(meta));
  module Move = Move.Make(M);
  module Select = Select.Make(M);
  switch (a) {
  | Click(_) => Ok(z)
  | Move(d) =>
    Move.go(d, z, ~folded=meta.folded)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | MoveToNextHole(d) =>
    let p: Piece.t => bool = (
      fun
      | Grout(_) => true
      | _ => false
    );
    Move.go(Goal(Piece(p, d)), z, ~folded=meta.folded)
    |> Result.of_option(~error=Action.Failure.Cant_move);
  | Jump(jump_target) =>
    open OptUtil.Syntax;

    let idx = Indicated.index(z);
    let (term, _) =
      Util.TimeUtil.measure_time("Perform.go_z => MakeTerm.go", true, () =>
        MakeTerm.go(Zipper.unselect_and_zip(z))
      );
    let statics = Statics.mk_map(term);

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
  | Unselect =>
    let z = Zipper.directional_unselect(z.selection.focus, z);
    Ok(z);
  | Select(Term(Current)) =>
    switch (Indicated.index(z)) {
    | None => Error(Action.Failure.Cant_select)
    | Some(id) =>
      switch (Select.term(id, z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    }
  | Select(All) =>
    switch (Move.do_extreme(Move.primary(ByToken), Up, z)) {
    | Some(z) =>
      switch (Select.go(Extreme(Down), z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Term(Id(id))) =>
    switch (Select.term(id, z)) {
    | Some(z) => Ok(z)
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
  | MoveToBackpackTarget(d) =>
    Move.to_backpack_target(d, z)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  };
};

let go = (a: Action.t, ed: Editor.t): Action.Result.t(Editor.t) =>
  if (ed.read_only && is_write_action(a)) {
    Result.Ok(ed);
  } else {
    open Result.Syntax;
    let Editor.State.{zipper, meta} = ed.state;
    Effect.s_clear();
    let+ z = go_z(~meta, a, zipper);
    Editor.new_state(~effects=Effect.s^, a, z, ed);
  };
