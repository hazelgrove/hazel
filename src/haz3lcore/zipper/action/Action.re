open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type piece_goal =
  | Grout;

let of_piece_goal =
  fun
  | Grout => (
      fun
      | Piece.Grout(_) => true
      | _ => false
    );

[@deriving (show({with_path: false}), sexp, yojson)]
type goal =
  | Point(Measured.Point.t)
  | Piece(piece_goal, Direction.t);

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
type rel =
  | Current
  | Id(Id.t, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type select =
  | All
  | Resize(move)
  | Smart
  | Tile(rel)
  | Term(rel);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Move(move)
  | MoveToNextHole(Direction.t)
  | Jump(jump_target)
  | Select(select)
  | Unselect(option(Direction.t))
  | Destruct(Direction.t)
  | Insert(string)
  | RotateBackpack
  | MoveToBackpackTarget(planar)
  | Pick_up
  | Put_down;

let to_tylr: t => option(Tylr.Edit.Action.t) =
  fun
  | Move(Local(Left(ByChar))) => Some(Move(Step(H(L))))
  | Move(Local(Right(ByChar))) => Some(Move(Step(H(R))))
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

let is_edit: t => bool =
  fun
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true
  | _ => false;
