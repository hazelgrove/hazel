open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Extreme(planar)
  | Local(planar)
  | Goal(Measured.Point.t);

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
  | Jump(jump_target)
  | Select(select)
  | Unselect
  | Destruct(Direction.t)
  | Insert(string)
  | RotateBackpack
  | MoveToBackpackTarget(planar)
  | Pick_up
  | Put_down;

let to_tylr: t => option(Tylr.Zipper.Action.t) =
  fun
  | Move(Local(Left(ByChar))) => Some(Move(L))
  | Move(Local(Right(ByChar))) => Some(Move(R))
  | Insert(s) => Some(Insert(s))
  | Destruct(Left) => Some(Delete(L))
  | Destruct(Right) => Some(Delete(R))
  | _ => None;

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
