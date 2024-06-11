open Haz3lcore;
open Virtual_dom.Vdom;

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;

  let model: model;
  let view: Node.t;
  let keymap: Key.t => option(Projector.action(action));
};

type t = (module ProjectorView);
