open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (
  int,
  list(ScratchSlide.persistent_state),
  list((string, ModelResult.persistent)),
);

[@deriving (show({with_path: false}), sexp, yojson)]
type documentation = (
  string,
  list((string, ScratchSlide.persistent_state)),
  [@default []] list((string, ModelResult.persistent)),
);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  settings: Settings.t,
  scratch,
  documentation,
};
