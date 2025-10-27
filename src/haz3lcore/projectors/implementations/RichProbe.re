open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
// Signature for domain specific representations

module type RichProbe = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;

  let update: (model, action) => model;
  let init: Exp.t => option(model);

  let badge: Node.t;

  let render:
    (
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      unit
    ) =>
    Node.t;
};
