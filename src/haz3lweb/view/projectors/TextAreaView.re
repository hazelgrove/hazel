open Haz3lcore;
open Virtual_dom.Vdom;

let view =
    (~inject: Projector.action(_) => Ui_effect.t(unit), value: string) =>
  Node.textarea(
    ~attr=
      Attr.many([
        Attr.create("rows", "4"),
        Attr.create("cols", "30"),
        Attr.on_mousedown(_ => inject(Focus)),
        Attr.on_input((_evt, new_val) =>
          inject(
            UpdateSyntax(
              _ => new_val |> Form.string_quote |> TextAreaCore.put,
            ),
          )
        ),
      ]),
    [Node.text(value)],
  );

let keymap = (key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | _ => Some(Default)
  };

let mk =
    (~inject, syntax: Piece.t, _model: ZipperBase.textarea)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;

     let value = syntax |> TextAreaCore.get |> Form.strip_quotes;
     let view = view(value, ~inject);
     let keymap = keymap;
   });
