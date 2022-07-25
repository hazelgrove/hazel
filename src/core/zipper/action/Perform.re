open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Extreme(planar)
  | Local(planar);

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(move)
    | Select(move)
    | Unselect
    | Destruct(Direction.t)
    | Insert(string)
    | RotateBackpack
    | MoveToBackpackTarget(planar)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Cant_move
      | Cant_insert
      | Cant_destruct
      | Cant_select
      | Cant_put_down;
  };

  module Result = {
    include Result;
    type t('success) = Result.t('success, Failure.t);
  };
};

let update_target = Caret.update_target;

let put_down = (z: t): option(t) =>
  /* Alternatively, putting down inside token could eiter merge-in or split */
  switch (z.caret) {
  | Inner(_) => None
  | Outer => Outer.put_down(z)
  };

let move = (d, z, id_gen) =>
  switch (d) {
  | Extreme(d) =>
    Caret.do_extreme(Move.primary(ByToken, Zipper.from_plane(d)), d, z)
    |> Option.map(update_target)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Local(d) =>
    /* Note: Don't update target on vertical movement */
    z
    |> (
      z =>
        switch (d) {
        | Left(chunk) =>
          Move.primary(chunk, Left, z) |> Option.map(update_target)
        | Right(chunk) =>
          Move.primary(chunk, Right, z) |> Option.map(update_target)
        | Up => Move.vertical(Left, z)
        | Down => Move.vertical(Right, z)
        }
    )
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  };

let select_primary = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    Outer.select(d, z);
  } else if (d == Left) {
    z
    |> Caret.set(Outer)
    |> Outer.move(Right)
    |> OptUtil.and_then(Outer.select(d));
  } else {
    z |> Caret.set(Outer) |> Outer.select(d);
  };

let select_vertical = (d: Direction.t, z: t): option(t) =>
  Caret.do_vertical(select_primary(d), d, z);

let select = (d, z, id_gen) =>
  (
    switch (d) {
    | Extreme(d) =>
      Caret.do_extreme(select_primary(Zipper.from_plane(d)), d, z)
      |> Option.map(update_target)
    | Local(d) =>
      /* Note: Don't update target on vertical selection */
      switch (d) {
      | Left(_) => select_primary(Left, z) |> Option.map(update_target)
      | Right(_) => select_primary(Right, z) |> Option.map(update_target)
      | Up => select_vertical(Left, z)
      | Down => select_vertical(Right, z)
      }
    }
  )
  |> Option.map(IdGen.id(id_gen))
  |> Result.of_option(~error=Action.Failure.Cant_select);

let go = (a: Action.t, (z, id_gen): state): Action.Result.t(state) => {
  IncompleteBidelim.clear();
  switch (a) {
  | Move(d) => move(d, z, id_gen)
  | Unselect =>
    Ok((Outer.directional_unselect(z.selection.focus, z), id_gen))
  | Select(d) => select(d, z, id_gen)
  | Destruct(d) =>
    (z, id_gen)
    |> Destruct.go(d)
    |> Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    (z, id_gen)
    |> Insert.go(char)
    /* note: remolding here is done case-by-case */
    //|> Option.map(((z, id_gen)) => remold_regrout(Right, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Outer.pick_up(z), id_gen))
  | Put_down =>
    z
    |> put_down
    |> Option.map(z => remold_regrout(Left, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | RotateBackpack =>
    Ok(({...z, backpack: Util.ListUtil.rotate(z.backpack)}, id_gen))
  | MoveToBackpackTarget(d) =>
    let map = Measured.of_segment(unselect_and_zip(z));
    Move.to_backpack_target(d, map, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  };
};
