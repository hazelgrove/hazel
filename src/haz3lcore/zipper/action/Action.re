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
type project =
  | UpdateSyntax(Id.t, Piece.t => Piece.t)
  | ToggleIndicated(Projector.t)
  | Remove(Id.t);
//| UpdateModel(Id.t, projector_type(X)_action)

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Project(project)
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

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cant_move
    | Cant_insert
    | Cant_destruct
    | Cant_select
    | Cant_put_down
    | Cant_project;
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t => bool =
  fun
  | Project(_) => true //TODO(andrew): revisit
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true
  | _ => false;
