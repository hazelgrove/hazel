open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type goal =
  | Point(Measured.Point.t)
  | Piece(Piece.t => bool, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Extreme(planar)
  | Local(planar)
  | Goal(goal);

[@deriving (show({with_path: false}), sexp, yojson)]
type jump_target =
  | TileId(Id.t)
  | BindingSiteOfIndicatedVar;

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Current
  | Id(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type select =
  | Resize(move)
  | Term(term);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Move(move)
  | MoveToNextHole(Direction.t)
  | Jump(jump_target)
  | RemoteAction(move, (Zipper.t, Id.t) => option((Zipper.t, Id.t)))
  | Select(select)
  | Unselect(option(Direction.t))
  | SetSelectionFocus(Direction.t)
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
