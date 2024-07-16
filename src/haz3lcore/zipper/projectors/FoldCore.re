open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

module M: CoreInner = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  let init = ();
  let can_project = _ => true;
  let placeholder = (_, _) => Inline(2);
  let update = (_, _) => ();
  let view = (_, ~info as _, ~inject) =>
    div(
      ~attrs=[Attr.on_double_click(_ => inject(Remove))],
      [text("⋱")],
    );
  let keymap = (_, _, _): option(action) => None;
};
