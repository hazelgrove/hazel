open Incr_dom;
module Js = Js_of_ocaml.Js;
module Parsing = Hazeltext.Parsing;
module Print = Hazeltext.Print;

let ast_from_text = Parsing.ast_of_string;

let switch_button_clickhandler =
    (inject: ModelAction.t => Ui_event.t, te_model: TextEditorModel.t, _) => {
  Vdom.Event.Many([
    inject(ModelAction.UpdateTextEditor(TextEditorModel.CloseEditor)),
    inject(
      ModelAction.Import(
        ast_from_text(te_model.current_text) |> Stdlib.Result.get_ok,
      ),
    ),
  ]);
};

let textbox_keyhandlers =
    (inject: ModelAction.t => Ui_event.t, model: Model.t) => {
  Vdom.[
    Attr.on_input((_, s) =>
      inject(
        ModelAction.UpdateTextEditor(TextEditorModel.SetCurrentText(s)),
      )
    ),
    Attr.on_keyup(_ => {
      let result = ast_from_text(model.text_editor.current_text);
      inject(
        ModelAction.UpdateTextEditor(
          Stdlib.Result.(
            is_error(result)
              ? TextEditorModel.SetErrorText(get_error(result))
              : TextEditorModel.ClearError
          ),
        ),
      );
    }),
  ];
};

let editor_switch_button =
    (inject: ModelAction.t => Ui_event.t, te_model: TextEditorModel.t) => {
  Vdom.(
    Node.span(
      [Attr.id("button-span")],
      [
        Node.button(
          [
            Attr.id("editor-switch-button"),
            Attr.on_click(
              switch_button_clickhandler(
                inject: ModelAction.t => Ui_event.t,
                te_model,
              ),
            ),
            ...te_model.valid_text ? [] : [Attr.disabled],
          ],
          [Node.text("Switch to Structural Editor")],
        ),
      ],
    )
  );
};

let text_box_height_css = (model: Model.t) => {
  let row_height = model.font_metrics.row_height;
  Css_gen.height(
    `Px(
      (TextEditorModel.line_count(model.text_editor) + 1)
      * Float.to_int(row_height),
    ),
  );
};

let text_editor_body = (inject: ModelAction.t => Ui_event.t, model: Model.t) => {
  Vdom.(
    Node.div(
      [Attr.classes(["page"])],
      [
        editor_switch_button(inject, model.text_editor),
        Node.textarea(
          [
            Attr.id("text-editor-text-box"),
            Attr.style(text_box_height_css(model)),
            ...textbox_keyhandlers(inject, model),
          ],
          [Node.text(model.text_editor.current_text)],
        ),
        Node.textarea(
          [Attr.id("text-editor-errors"), Attr.create("readonly", "")],
          [Node.text(model.text_editor.error_text)],
        ),
      ],
    )
  );
};

let view = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  Vdom.(
    Node.div([Attr.id("page-area")], [text_editor_body(inject, model)])
  );
};
