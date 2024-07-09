open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Unknown(Internal)
  };

let expected_ty = (info: option(Info.t)) =>
  switch (info) {
  | Some(InfoExp({mode, _}) | InfoPat({mode, _})) => Mode.ty_of(mode)
  | _ => Unknown(Internal)
  };

let display = expected_ty =>
  "⋱ ⇐ " ++ (expected_ty |> display_ty |> Typ.pretty_print);

let mk = (model: infer, ~syntax as _): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = infer;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let projector = Infer(model);

     let can_project = (p: Piece.t): bool =>
       switch (p) {
       | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
       | _ => false
       };

     let placeholder = () =>
       Inline((model.expected_ty |> display |> String.length) - 2);

     let auto_update = ({info, _}) => {
       print_endline("updating infer projector");
       Infer({expected_ty: Some(expected_ty(info))});
     };
     let update = _ => Infer(model);

     let view = (~inject, _) =>
       div(
         ~attrs=[Attr.on_double_click(_ => inject(Remove))],
         [text(display(model.expected_ty))],
       );

     let keymap = (_, key: Key.t): option(ProjectorBase.action) =>
       switch (key) {
       | {key: D("Escape"), _} => Some(Remove)
       | _ => None
       };
   });
