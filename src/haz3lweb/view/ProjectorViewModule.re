open Haz3lcore;
open Virtual_dom.Vdom;

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;

  let view: Node.t;
  let keymap: Key.t => option(Projector.action(action));
};

type t = (module ProjectorView);
