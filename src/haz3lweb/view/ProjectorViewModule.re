open Haz3lcore;
open Virtual_dom.Vdom;

//TODO(andrew): cleanup/rm

[@deriving (show({with_path: false}), sexp, yojson)]
type accent =
  | Indicated(Util.Direction.t)
  | Selected;

let cls = (indicated: option(accent)) =>
  switch (indicated) {
  | Some(Indicated(Left)) => ["indicated", "left"]
  | Some(Indicated(Right)) => ["indicated", "right"]
  | Some(Selected) => ["selected"]
  | None => []
  };

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;

  let view: option(accent) => Node.t;
  let keymap: (Util.Direction.t, Key.t) => option(ProjectorsUpdate.t);
};

type t = (module ProjectorView);
