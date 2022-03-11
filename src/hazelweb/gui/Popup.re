open Incr_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;
module Parsing = Hazeltext.Parsing;
module Print = Hazeltext.Print;

let extract_program_string = model => {
  let prog = Model.get_program(model);
  let lay = Program.get_layout(~settings=Settings.init, prog);
  Print.string_of_layout(lay);
};

let update_textbox_value = (text_box_id, str) => {
  let elem = JSUtil.force_get_elem_by_id(text_box_id);
  elem##setAttribute(Js_of_ocaml.Js.string("value"), JSUtil.Js.string(str));
  // Additionally set the textarea value, so the box
  // visually updates
  let text_area =
    Js.Opt.get(Dom_html.CoerceTo.textarea(elem), _ => assert(false));
  text_area##.value := JSUtil.Js.string(str);
  Vdom.Event.Ignore;
};

let ast_from_textbox = text_box_id => {
  let elem = JSUtil.force_get_elem_by_id(text_box_id);
  let cur_text = JSUtil.force_get_attr("value", elem);
  Parsing.ast_of_string(cur_text);
};

let set_errors_text = str => {
  let errs_elem = JSUtil.force_get_elem_by_id("text-editor-errors");
  errs_elem##.innerHTML := JSUtil.Js.string(str);
};

let console_button =
  Vdom.Node.button(
    [
      Vdom.Attr.id("parse-console-button"),
      Vdom.Attr.on_click(_ => {
        switch (ast_from_textbox("text-editor-text-box")) {
        | Ok(ast) =>
          JSUtil.log(Js.string(Serialization.string_of_exp(ast)));
          Vdom.Event.Ignore;
        | _ => Vdom.Event.Ignore
        }
      }),
    ],
    [Vdom.Node.text("Parse to Console")],
  );

let load_program_button = inject =>
  Vdom.Node.button(
    [
      Vdom.Attr.id("load-program-button"),
      Vdom.Attr.on_click(_ => {
        switch (ast_from_textbox("text-editor-text-box")) {
        | Ok(ast) =>
          set_errors_text("");
          inject(ModelAction.Import(ast));
        | Error(err_string) =>
          set_errors_text(err_string);
          Vdom.Event.Ignore;
        }
      }),
    ],
    [Vdom.Node.text("Load program")],
  );

let text_editor_body = (inject, model) => {
  Vdom.(
    Node.div(
      [Attr.id("popup-page")],
      [
        Node.textarea(
          [
            Attr.id("text-editor-text-box"),
            Attr.value(""),
            Attr.on_change((_, s) =>
              update_textbox_value("text-editor-text-box", s)
            ),
          ],
          [],
        ),
        Node.textarea([Attr.id("text-editor-errors")], []),
        Node.div(
          [Attr.id("text-editor-button-bar")],
          [
            Node.button(
              [
                Attr.id("edit-program-button"),
                Attr.on_click(_ =>
                  update_textbox_value(
                    "text-editor-text-box",
                    extract_program_string(model),
                  )
                ),
              ],
              [Node.text("Edit current program")],
            ),
            load_program_button(inject),
            console_button,
          ],
        ),
      ],
    )
  );
};

let mk_popup =
    (
      popup_open: bool,
      ~inject,
      ~on_toggle: Js.t(Dom_html.mouseEvent) => Vdom.Event.t,
      ~popup_body,
      ~model,
    ) => {
  let pop_body = popup_open ? [popup_body(inject, model)] : [];
  Vdom.(
    Node.div(
      [
        Attr.id("popup"),
        Attr.classes(popup_open ? ["popup"] : []),
        Attr.on_click(on_toggle),
      ],
      [
        Node.div(
          [
            Attr.id("popup-page-area"),
            Attr.classes(popup_open ? ["popup-page-area"] : []),
            Attr.on_click(_ => Event.Stop_propagation),
          ],
          pop_body,
        ),
      ],
    )
  );
};

let text_editor = (~inject, ~is_open, ~model) =>
  mk_popup(
    is_open,
    ~inject,
    ~on_toggle=_ => inject(ModelAction.ToggleTextEditorPopup),
    ~popup_body=text_editor_body,
    ~model,
  );
