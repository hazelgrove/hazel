open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

let mk = (_model: fold): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     let model = ();
     let can_project = _ => true;
     let placeholder = _ => Inline(2);
     let auto_update = _ => Fold();
     let update = _ => Fold();
     let view = (~status as _, ~syntax as _, ~info as _, ~inject) =>
       div(
         ~attrs=[Attr.on_double_click(_ => inject(Remove))],
         [text("⋱")],
       );
     let keymap = (_, _): option(ProjectorBase.action) => None;
   });
