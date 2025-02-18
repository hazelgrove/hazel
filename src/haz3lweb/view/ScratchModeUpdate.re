open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | CellAction(CellEditor.Update.t)
  | SwitchSlide(int)
  | ResetCurrent
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  | Export;
