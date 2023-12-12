open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.persistent_state));

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = (string, list((string, ScratchSlide.persistent_state)));

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  settings: Settings.t,
  scratch,
  examples,
};
