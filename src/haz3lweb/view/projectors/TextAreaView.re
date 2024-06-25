open Haz3lcore;
open Virtual_dom.Vdom;
open Util.Web;

let put = (str: string): Projector.action(_) =>
  Projector.UpdateSyntax(_ => str |> Form.string_quote |> TextAreaCore.put);

let textarea2 =
    (~inject: Projector.action(_) => Ui_effect.t(unit), text: string) =>
  Node.textarea(
    ~attr=
      Attr.many([
        //TODO(andrew): fudge factors below
        // Attr.create("rows", "4"),
        // Attr.create("cols", "21"),
        Attr.on_mousedown(_ => inject(Focus)),
        Attr.on_input((_, new_text) => inject(put(new_text))),
      ]),
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text(".")]
  @ (List.init(n, _ => [Node.br(), Node.text(".")]) |> List.flatten);

let view = (~inject, text, indicated: option(ProjectorViewModule.accent)) =>
  Node.div(
    ~attr=clss(["cols"] @ ProjectorViewModule.cls(indicated)),
    n_of(3)  //TODO(andrew): magic number
    @ [textarea2(~inject, text)],
  );

let keymap = (_direction, key: Key.t): option(Projector.action(string)) =>
  switch (key.key) {
  | D(
      "ArrowLeft" | "ArrowRight" | "ArrowUp" | "ArrowDown" | "Enter" |
      "Backspace" |
      " ",
    ) =>
    None
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
