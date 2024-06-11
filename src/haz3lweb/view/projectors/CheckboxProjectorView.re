open Haz3lcore;
open Virtual_dom.Vdom;
open Sexplib.Std;

let put = (bool: bool) =>
  Example.mk_monotile(Form.mk_atomic(Exp, string_of_bool(bool)));

let get = piece =>
  switch (CheckboxProjectorCore.state_of(piece)) {
  | None => failwith("toggle: not boolean literal")
  | Some(s) => s
  };

let toggle = (piece: Piece.t) => {
  let cur =
    switch (CheckboxProjectorCore.state_of(piece)) {
    | None => failwith("toggle: not boolean literal")
    | Some(s) => s
    };
  put(!cur);
};

let view = (~inject, syntax) =>
  Node.input(
    ~attr=
      Attr.many(
        [
          Attr.create("type", "checkbox"),
          Attr.on_input((_evt, _str) =>
            inject(Projector.UpdateSyntax(toggle))
          ),
          JsUtil.stop_mousedown_propagation,
        ]
        @ (get(syntax) ? [Attr.checked] : []),
      ),
    [],
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, ~inject, model: Projector.fold): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.checkbox;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let view = view(syntax, ~inject);
     let key_handler = key_handler;
   });
