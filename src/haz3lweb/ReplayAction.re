open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | LoadReplay([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | EnableReplay(option(string))
  | ForwardReplay
  | BackwardReplay
  | TogglePlayReplay
  | StepReplay
  | DisableReplay;
