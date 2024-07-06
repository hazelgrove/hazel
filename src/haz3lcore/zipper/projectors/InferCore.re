open ZipperBase;
open Virtual_dom.Vdom;
open Node;

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

let mk = (model: ZipperBase.infer, ~syntax as _): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = infer;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = infer_action;
     let model = model;
     let projector: projector = Infer(model);

     let can_project = (p: Piece.t): bool =>
       switch (p) {
       | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
       | _ => false
       };

     let placeholder = () =>
       Inline((model.expected_ty |> display |> String.length) - 2);

     let auto_update = ({info, _}): projector => {
       print_endline("updating infer projector");
       Infer({expected_ty: Some(expected_ty(info))});
     };
     let update = (_action): projector => Infer(model);

     let view = (~inject, _) =>
       div(
         ~attr=Attr.on_double_click(_ => inject(ProjectorsUpdate.Remove)),
         [text(display(model.expected_ty))],
       );

     let keymap = (_, key: Key.t): option(ProjectorsUpdate.t) =>
       switch (key) {
       | {key: D("Escape"), _} => Some(Remove)
       | _ => None
       };
   });
