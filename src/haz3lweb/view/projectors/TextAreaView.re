open Haz3lcore;
open Virtual_dom.Vdom;
open Util.Web;

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
        //TODO(andrew): fudge factors below
        // Attr.create("rows", "4"),
        // Attr.create("cols", "21"),
        Attr.on_blur(_ =>
          inject(UpdateModel(TextAreaCore.serialize(SetInside(false))))
        ),
        Attr.on_mousedown(_ =>
          Effect.Many([
            // inject(JumpTo),
            // inject(UpdateModel(TextAreaCore.serialize(SetInside(true)))),
            inject(FocusInternal(selector)),
            //Effect.Stop_propagation,
          ])
        ),
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
    n_of(1 + Util.StringUtil.num_linebreaks(text))  //TODO(andrew): magic number
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
  let JsUtil.{rows, cols} = JsUtil.textarea_caret_rel_pos(selector);
  print_endline("model-inside:" ++ string_of_bool(model.inside));
  let pos = JsUtil.textarea_caret_rel_pos(selector);
  let pos' = JsUtil.textarea_caret_position(selector);
  print_endline("pos: " ++ JsUtil.show_caret_position(pos));
  print_endline("pos': " ++ JsUtil.show_text_position(pos'));
  switch (key.key, direction) {
  | (D("ArrowRight"), Right) when !model.inside =>
    Some(FocusInternal(selector))
  | (D("ArrowLeft"), Left) when !model.inside =>
    JsUtil.set_textarea_caret_to_end(selector);
    Some(FocusInternal(selector));
  | (D("ArrowRight"), _) when model.inside && rows == Last && cols == Last =>
    Some(Escape(selector, Left))
  | (D("ArrowLeft"), _) when model.inside && rows == First && cols == First =>
    Some(Escape(selector, Right))
  | _ when model.inside => Some(Default)
  | _ =>
    print_endline("Not model.inside");
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
     // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     //TODO(andrew): unhardcode element !!!!!!!!!!
     // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     let selector = ".projector.text textarea";

     let value = syntax |> TextAreaCore.get |> Form.strip_quotes;
     let view = view(model, value, ~selector, ~inject);
     let keymap = keymap(~selector, model);
   });
