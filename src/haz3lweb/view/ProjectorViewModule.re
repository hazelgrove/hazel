open Haz3lcore;
open Virtual_dom.Vdom;

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;

  let syntax: Piece.t;
  let model: model;
  let view: Node.t;
  let inject: Projector.action(action) => Ui_effect.t(unit);
  let key_handler: Key.t => option(Projector.action(action));
};

type t = (module ProjectorView);
