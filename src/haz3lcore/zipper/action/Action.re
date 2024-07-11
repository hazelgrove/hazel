open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
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
  | UpdateModel(Id.t, Projector.t => Projector.t)
  | SetIndicated(Projector.kind)
  | ToggleIndicated(Projector.kind)
  | FocusInternal(Id.t)
  | Escape(Id.t, Direction.t)
  | Remove(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | RecalcStatics /* Could refactor to SetInitCtx(ctx) */
  | SetBuffer(agent)
  | ResetBuffer
  | Paste(string)
  | Reparse
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
    | Cant_project
    | CantPaste
    | CantReparse;
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t => bool =
  fun
  | RecalcStatics => true
  | Project(_) => true //TODO(andrew): revisit
  | SetBuffer(_) => true //TODO(andrew): revisit
  | ResetBuffer => true //TODO(andrew): revisit
  | Paste(_)
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true
  | Move(_)
  | MoveToNextHole(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false;

/* Determines whether undo/redo skips action */
let is_historic: t => bool =
  fun
  | RecalcStatics =>
    /* If we add this to history, we can basically never undo */
    false
  | SetBuffer(_)
  | ResetBuffer
  | Move(_)
  | MoveToNextHole(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Paste(_)
  | Reparse
  | Project(_)
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true;
