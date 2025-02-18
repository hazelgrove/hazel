open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  spec: Exercise.spec, // The spec that the model will be reset to on ResetExercise
  /* We keep a separate editors field below (even though each cell technically also has its own editor)
     for two reasons:
        1. There are two synced cells that have the same internal `editor` model
        2. The editors need to be `stitched` together before any cell calculations can be done */
  editors: Exercise.p(Editor.t),
  cells: Exercise.stitched(CellEditor.Model.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = Exercise.persistent_exercise_mode;
