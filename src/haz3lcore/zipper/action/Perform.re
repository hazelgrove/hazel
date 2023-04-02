open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | Move(_)
  | Unselect(_)
  | SetSelectionFocus(_) //TODO(andrew): remove
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
      a: Action.t,
      z: Zipper.t,
      id_gen: IdGen.state,
    )
    : Action.Result.t((Zipper.t, IdGen.state)) => {
  let meta =
    switch (meta) {
    | Some(m) => m
    | None => Editor.Meta.init(z)
    };
  module M = (val Editor.Meta.module_of_t(meta));
  module Move = Move.Make(M);
  module Select = Select.Make(M);
  switch (a) {
  | Move(d) =>
    Move.go(d, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    open OptUtil.Syntax;

    let idx = Indicated.index(z);
    let (term, _) =
      Util.TimeUtil.measure_time("Perform.go_z => MakeTerm.go", true, () =>
        MakeTerm.go(Zipper.unselect_and_zip(~ignore_selection=true, z))
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
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  | SetSelectionFocus(d) =>
    let z = {
      ...z,
      selection: {
        ...z.selection,
        focus: d,
      },
    };
    Ok((z, id_gen));
  | Unselect(Some(d)) =>
    let z = Zipper.directional_unselect(d, z);
    Ok((z, id_gen));
  | Unselect(None) =>
    let z = Zipper.directional_unselect(z.selection.focus, z);
    Ok((z, id_gen));
  | Select(Term(Current)) =>
    switch (Indicated.index(z)) {
    | None => Error(Action.Failure.Cant_select)
    | Some(id) =>
      switch (Select.term(id, z)) {
      | Some(z) => Ok((z, id_gen))
      | None => Error(Action.Failure.Cant_select)
      }
    }
  | Select(Term(Id(id))) =>
    switch (Select.term(id, z)) {
    | Some(z) => Ok((z, id_gen))
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Resize(d)) =>
    Select.go(d, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Destruct(d) =>
    (z, id_gen)
    |> Destruct.go(d)
    |> Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    (z, id_gen)
    |> Insert.go(char)
    /* note: remolding here is done case-by-case */
    //|> Option.map(((z, id_gen)) => remold_regrout(Right, z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Zipper.pick_up(z), id_gen))
  | Put_down =>
    let z =
      /* Alternatively, putting down inside token could eiter merge-in or split */
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.put_down(Left, z)
      };
    z
    |> Option.map(z => remold_regrout(Left, z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_put_down);
  | RotateBackpack =>
    let z = {...z, backpack: Util.ListUtil.rotate(z.backpack)};
    Ok((z, id_gen));
  | MoveToBackpackTarget(d) =>
    Move.to_backpack_target(d, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  };
};

let go =
    (a: Action.t, ed: Editor.t, id_gen: IdGen.state)
    : Action.Result.t((Editor.t, IdGen.state)) =>
  if (ed.read_only && is_write_action(a)) {
    Result.Ok((ed, id_gen));
  } else {
    open Result.Syntax;
    let Editor.State.{zipper, meta} = ed.state;
    Effect.s_clear();
    let+ (z, id_gen) = go_z(~meta, a, zipper, id_gen);
    let ed = Editor.new_state(~effects=Effect.s^, a, z, ed);
    (ed, id_gen);
  };
