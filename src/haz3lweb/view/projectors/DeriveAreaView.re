open Haz3lcore;
open Virtual_dom.Vdom;
open Util.Web;

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//TODO(andrew): unhardcode element !!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
let selector = ".projector.text textarea";

let put = (str: string): Projector.action(_) =>
  Projector.UpdateSyntax(_ => str |> Form.string_quote |> TextAreaCore.put);

let textarea =
    (
      ~selector,
      ~inject: Projector.action(string) => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attr=
      Attr.many([
        // Attr.create("rows", "4"),
        // Attr.create("cols", "21"),
        Attr.on_blur(_ =>
          inject(UpdateModel(TextAreaCore.serialize(SetInside(false))))
        ),
        Attr.on_click(_ => inject(FocusInternal(selector))),
        Attr.on_input((_, new_text) => inject(put(new_text))),
      ]),
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text("·")]
  @ (List.init(n, _ => [Node.br(), Node.text("·")]) |> List.flatten);

let view =
    (
      ~inject,
      ~selector,
      model: ZipperBase.textarea,
      text: string,
      indicated: option(ProjectorViewModule.accent),
    ) =>
  Node.div(
    ~attr=
      clss(
        ["cols"] @ (model.inside ? [] : ProjectorViewModule.cls(indicated)),
      ),
    n_of(1 + Util.StringUtil.num_linebreaks(text))
    @ [textarea(~inject, ~selector, text)],
  );

let keymap =
    (
      ~selector,
      model: ZipperBase.textarea,
      direction: Util.Direction.t,
      key: Key.t,
    )
    : option(Projector.action(string)) => {
  let textarea = JsUtil.TextArea.get(selector);
  let focussed = model.inside;
  //TODO(andrew): make actual focus king?
  //IE query focus state FocusInternal side?
  // but what if gets unfocussed due to eg refresh?
  let pos = JsUtil.TextArea.caret_rel_pos(textarea);
  let is_last_pos = pos.rows == Last && pos.cols == Last;
  let is_first_pos = pos.rows == First && pos.cols == First;
  // print_endline("is_focus:" ++ string_of_bool(is_focus));
  // let rel_pos = JsUtil.TextArea.caret_rel_pos(textarea);
  // let pos = JsUtil.TextArea.caret_pos(textarea);
  // print_endline("pos: " ++ JsUtil.TextArea.show_rel_pos(rel_pos));
  // print_endline("pos': " ++ JsUtil.TextArea.show_pos(pos));
  switch (key.key, direction) {
  | (D("ArrowRight"), Right) when !focussed =>
    Some(FocusInternal(selector))
  | (D("ArrowLeft"), Left) when !focussed =>
    JsUtil.TextArea.set_caret_to_end(textarea);
    Some(FocusInternal(selector));
  | (D("ArrowRight" | "ArrowDown"), _) when focussed && is_last_pos =>
    Some(Escape(selector, Left))
  | (D("ArrowLeft" | "ArrowUp"), _) when focussed && is_first_pos =>
    Some(Escape(selector, Right))
  | _ when focussed => Some(Default)
  | _ =>
    print_endline("Warning: Not focussed");
    None;
  };
};

let mk =
    (~inject, syntax: Piece.t, model: ZipperBase.textarea)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.textarea_action;

     let value = syntax |> TextAreaCore.get |> Form.strip_quotes;
     let view = view(model, value, ~selector, ~inject);
     let keymap = keymap(~selector, model);
   });
