open Haz3lcore;
open Virtual_dom.Vdom;
open Util.Web;

let put = (str: string): Projector.action(_) =>
  Projector.UpdateSyntax(_ => str |> Form.string_quote |> TextAreaCore.put);

let textarea =
    (~inject: Projector.action(string) => Ui_effect.t(unit), text: string) =>
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
            inject(JumpTo),
            inject(UpdateModel(TextAreaCore.serialize(SetInside(true)))),
            Effect.Stop_propagation,
          ])
        ),
        Attr.on_input((_, new_text) => inject(put(new_text))),
      ]),
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text(".")]
  @ (List.init(n, _ => [Node.br(), Node.text(".")]) |> List.flatten);

let view =
    (
      ~inject,
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
    @ [textarea(~inject, text)],
  );

let keymap =
    (model: ZipperBase.textarea, direction: Util.Direction.t, key: Key.t)
    : option(Projector.action(string)) => {
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //TODO(andrew): unhardcoded element !!!!!!!!!!
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  let selector = ".projector.text textarea";
  let JsUtil.{rows, cols} = JsUtil.get_caret_position_type(selector);
  print_endline("model-inside:" ++ string_of_bool(model.inside));
  switch (key.key, direction) {
  | (D("ArrowRight"), Right) when !model.inside => Some(FocusInternal)
  | (D("ArrowLeft"), Left) when !model.inside => Some(FocusInternal)
  | (D("ArrowRight"), _) when model.inside && rows == Last && cols == Last =>
    //TODO(andrew): need to move zipper caret to right side
    //TODO(andrew): focus specific element id
    JsUtil.get_elem_by_selector(selector)##blur;
    print_endline("escape to right");
    Some(UpdateModel(TextAreaCore.serialize(SetInside(false))));
  | (D("ArrowLeft"), _) when model.inside && rows == First && cols == First =>
    //TODO(andrew): need to move zipper caret to left side
    //TODO(andrew): focus specific element id
    JsUtil.get_elem_by_selector(selector)##blur;
    print_endline("escape to left");
    Some(UpdateModel(TextAreaCore.serialize(SetInside(false))));
  | _ when model.inside =>
    let pos = JsUtil.get_caret_position_type(selector);
    let pos' = JsUtil.textarea_caret_position(selector);
    print_endline("pos: " ++ JsUtil.show_caret_position(pos));
    print_endline("pos': " ++ JsUtil.show_text_position(pos'));
    Some(Default);
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

     let value = syntax |> TextAreaCore.get |> Form.strip_quotes;
     let view = view(model, value, ~inject);
     let keymap = keymap(model);
   });
