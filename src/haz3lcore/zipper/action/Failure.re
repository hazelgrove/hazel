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
  | CantAccept
  | Cant_undo
  | Cant_redo
  | CantIntroduce
  | Wrong_projector;

exception Exception(t);
