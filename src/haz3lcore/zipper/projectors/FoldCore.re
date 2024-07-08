open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open ZipperBase;
open Virtual_dom.Vdom;
open Node;

let mk = (_model: ZipperBase.fold, ~syntax as _): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = ();
     let projector: ZipperBase.projector = Fold();
     let can_project = _ => true;
     let placeholder = () => Inline(2);
     let auto_update = _: ZipperBase.projector => Fold();
     let update = (_action): ZipperBase.projector => Fold();
     let view = (~inject, _) =>
       div(
         ~attrs=[Attr.on_double_click(_ => inject(ProjectorsUpdate.Remove))],
         [text("⋱")],
       );
     let keymap = (_, _): option(ProjectorsUpdate.t) => None;
   });
