open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Extreme(planar)
  | Local(planar)
  | Goal(Measured.Point.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Move(move)
  | JumpToId(Id.t)
  | Select(move)
  | Unselect
  | Destruct(Direction.t)
  | Insert(string)
  | RotateBackpack
  | MoveToBackpackTarget(planar)
  | Pick_up
  | Put_down
  | Comment_out;

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cant_move
    | Cant_insert
    | Cant_destruct
    | Cant_select
    | Cant_put_down
    | Cant_comment_out;
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};
