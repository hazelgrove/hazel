open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Editor(Exercise.pos, CellEditor.Update.t)
  | ResetEditor(Exercise.pos)
  | ResetExercise;
