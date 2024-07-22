open Util;

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

/* This type defines the top-level actions used to manage
 * projectors,as distinguished from ProjectorBase.external_action,
 * which defines the actions available internally to all projectors,
 * and from each projector's own internal action type */
[@deriving (show({with_path: false}), sexp, yojson)]
type project =
  | SetIndicated(Projector.kind) /* Project syntax at caret */
  | ToggleIndicated(Projector.kind) /* Un/Project syntax at caret */
  | Remove(Id.t) /* Remove projector at Id */
  | SetSyntax(Id.t, Piece.t) /* Set underlying syntax */
  | SetModel(Id.t, string) /* Set serialized projector model */
  | Focus(Id.t, option(Util.Direction.t)) /* Pass control to projector */
  | Escape(Id.t, Direction.t); /* Pass control to parent editor */

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi;

[@deriving (show({with_path: false}), sexp, yojson)]
type buffer =
  | Set(agent)
  | Accept;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Reparse
  | Buffer(buffer)
  | Paste(string)
  | Copy
  | Cut
  | Project(project)
  | Move(move)
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
    | CantReparse
    | CantAccept;
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t => bool =
  fun
  | Project(Focus(_) | Escape(_)) => false
  | Project(_) => true //TODO(andrew): revisit
  | Buffer(Accept)
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true
  | Buffer(Set(_))
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false;

/* Determines whether undo/redo skips action */
let is_historic: t => bool =
  fun
  | Buffer(Set(_))
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Cut
  | Buffer(Accept)
  | Paste(_)
  | Reparse
  | Project(_)
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true;
