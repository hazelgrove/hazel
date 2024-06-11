open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Sexplib.Std;

let view = (~inject, expected_ty: option(Typ.t)) =>
  div(
    ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
    [text(expected_ty |> InferCore.display_ty |> Typ.pretty_print)],
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, model: Projector.infer, ~inject): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.infer;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let view = view(~inject, model.expected_ty);
     let key_handler = key_handler;
   });
