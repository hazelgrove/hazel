open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  refs: Binding.s,
  stem: Binding.stem,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tag =
  | Paren
  | Probe(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type frame = {
  env_id: Id.t,
  frame_id: Id.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type stack = list(frame);

let empty = {refs: [], stem: []};
