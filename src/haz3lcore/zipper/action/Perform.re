open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | Move(_)
  | MoveToNextHole(_)
  | Unselect(_)
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
    | None => Editor.Meta.init(z)
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

  switch (a) {
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | MoveToNextHole(d) =>
    Move.go(Goal(Piece(Grout, d)), z)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    open OptUtil.Syntax;

    let idx = Indicated.index(z);
    let (term, _) =
      Util.TimeUtil.measure_time("Perform.go_z => MakeTerm.from_zip", true, () =>
        MakeTerm.from_zip_for_view(z)
      );
    let statics = Interface.Statics.mk_map(settings, term);

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
      //PERF: this is expensive
      let (term, _) = MakeTerm.from_zip_for_view(z);
      let statics = Interface.Statics.mk_map(settings, term);
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

let go =
    (~settings: CoreSettings.t, a: Action.t, ed: Editor.t)
    : Action.Result.t(Editor.t) =>
  if (ed.read_only && is_write_action(a)) {
    Result.Ok(ed);
  } else {
    open Result.Syntax;
    let Editor.State.{zipper, meta, tylr} = ed.state;
    Effect.s_clear();
    let+ z = go_z(~settings, ~meta, a, zipper);
    let tylr =
      switch (Action.to_tylr(a)) {
      | None => tylr
      | Some(a) =>
        print_endline("Perform.go a = " ++ Tylr.Edit.Action.show(a));
        print_endline("Perform.go z = " ++ Tylr.Zipper.show(tylr));
        switch (Tylr.Edit.perform(a, tylr)) {
        | None =>
          print_endline("Perform.go failed");
          tylr;
        | Some(t) =>
          print_endline("Perform.go succeeded z = " ++ Tylr.Zipper.show(t));
          t;
        };
      };
    Editor.new_state(~effects=Effect.s^, ~tylr, a, z, ed);
  };
