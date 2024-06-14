open Sexplib.Std;
module Sexp = Sexplib.Sexp;
open Haz3lcore;
module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    enable: bool,
    is_editing: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleEnable
    | ToggleIsEditing;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  input: string,
  query_result: option(string),
  colorings: list((Id.t, string)),
};

let init = {input: "", query_result: None, colorings: []};
