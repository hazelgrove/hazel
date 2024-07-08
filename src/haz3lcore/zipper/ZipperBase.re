open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);

  let decrement: t => t =
    fun
    | Outer
    | Inner(_, 0) => Outer
    | Inner(d, c) => Inner(d, c - 1);

  let offset: t => int =
    fun
    | Outer => 0
    | Inner(_, c) => c + 1;
};

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret: Caret.t,
  [@opaque]
  projectors: ProjectorBase.Map.t,
};

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let put_siblings = (siblings, z: t): t => update_siblings(_ => siblings, z);

let put_selection_content = (content: Segment.t, z): t => {
  ...z,
  selection: {
    ...z.selection,
    content,
  },
};
