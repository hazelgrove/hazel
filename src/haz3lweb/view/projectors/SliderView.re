open Haz3lcore;
open Virtual_dom.Vdom;

let view =
    (~inject: Projector.action(_) => Ui_effect.t(unit), value: int, _) =>
  Node.input(
    ~attr=
      Attr.many([
        Attr.create("type", "range"),
        Attr.create("value", string_of_int(value)),
        Attr.on_input((_evt, new_val) =>
          inject(UpdateSyntax(_ => SliderCore.put(new_val)))
        ),
      ]),
    [],
  );

let keymap = (_, key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, ~inject, _model: ZipperBase.slider)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;

     let value = SliderCore.get(syntax);
     let view = view(value, ~inject);
     let keymap = keymap;
   });
