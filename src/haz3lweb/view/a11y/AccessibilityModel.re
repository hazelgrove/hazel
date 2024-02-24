open Sexplib.Std;
module Sexp = Sexplib.Sexp;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {enable: bool};
};
