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
  | SetSyntax(Id.t, Piece.t)
  | UpdateModel(Id.t, Projector.entry => Projector.entry)
  | SetIndicated(Projector.kind)
  | ToggleIndicated(Projector.kind)
  | FocusInternal(Id.t, Util.Direction.t)
  | Escape(Id.t, Direction.t)
  | Remove(Id.t);

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
  | Project(FocusInternal(_) | Escape(_)) => false
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
