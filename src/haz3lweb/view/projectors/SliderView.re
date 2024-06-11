open Haz3lcore;
open Virtual_dom.Vdom;

let view =
    (
      ~inject:
         Projector.action(ZipperBase.slider_action) => Ui_effect.t(unit),
      value: int,
    ) =>
  Node.input(
    ~attr=
      Attr.many([
        Attr.create("type", "range"),
        Attr.create("value", string_of_int(value)),
        Attr.on_input((_evt, new_val) =>
          inject(UpdateModel(Set(int_of_string(new_val))))
        ),
      ]),
    [],
  );

let keymap =
    (key: Key.t): option(Projector.action(ZipperBase.slider_action)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (_syntax: Piece.t, ~inject, model: ZipperBase.slider)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;

     let view = view(model.value, ~inject);
     let keymap = keymap;
   });
