open Util;
open Zipper;

let is_write_action = (a: Action.t) => {
  switch (a) {
  | Move(_)
  | JumpToId(_)
  | Unselect
  | Select(_) => false
  | Destruct(_)
  | Insert(_)
  // | Pick_up
  // | Put_down
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
    Move.go(d, z, id_gen)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | JumpToId(id) =>
    Move.jump_to_id(z, id_gen, id)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Unselect =>
    let z = Zipper.directional_unselect(z.selection.focus, z);
    Ok((z, id_gen));
  | Select(d) =>
    Select.go(d, z, id_gen)
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Destruct(d) =>
    Zipper.delete(d, z, id_gen)
    |> Option.map(((z, id_gen)) => {
         let (z, id_gen) = Zipper.regrold(z, id_gen);
         (reassemble(z), id_gen);
       })
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    Zipper.insert(Left, char, z, id_gen)
    |> Option.map(((z, id_gen)) => {
         let (z, id_gen) = Zipper.regrold(z, id_gen);
         (reassemble(z), id_gen);
       })
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | MoveToBackpackTarget(d) =>
    Move.to_backpack_target(d, z, id_gen)
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
