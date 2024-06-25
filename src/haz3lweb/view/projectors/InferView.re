open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let view = (~inject, expected_ty: option(Typ.t), _) =>
  div(
    ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
    [text(InferCore.display(expected_ty))],
  );

let keymap = (_, key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (_syntax: Piece.t, model: ZipperBase.infer, ~inject)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.infer_action;

     let view = view(~inject, model.expected_ty);
     let keymap = keymap;
   });
