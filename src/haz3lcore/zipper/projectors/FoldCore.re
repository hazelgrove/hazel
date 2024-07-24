open Util;
open ProjNew;
open Virtual_dom.Vdom;
open Node;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (_, _) => Inline(2);
  let update = (_, _) => ();
  let view = (_, ~info as _, ~local as _, ~parent) =>
    div(
      ~attrs=[Attr.on_double_click(_ => parent(Remove))],
      [text("⋱")],
    );
  let focus = _ => ();
};
