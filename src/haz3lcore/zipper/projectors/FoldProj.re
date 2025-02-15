open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = {text: "⋱"};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (m, _) =>
    Inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _) => m;
  let view = (m: model, ~info as _, ~local as _, ~parent) =>
    div(
      ~attrs=[Attr.on_double_click(_ => parent(Remove))],
      [text(m.text)],
    );
  let focus = _ => ();
};
