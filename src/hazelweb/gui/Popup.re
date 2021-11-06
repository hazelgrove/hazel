open Incr_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;
module Parsing = Parser.Parsing;
module Print = Parser.Print;

let extract_program_string = model => {
  let prog = Model.get_program(model);
  let lay = Program.get_layout(~settings=Settings.init, prog);
  Print.string_of_layout(lay);
};

let update_textbox_value = (_, text_box_id, str) => {
  let text_box = JSUtil.force_get_elem_by_id(text_box_id);
  text_box##setAttribute(
    Js_of_ocaml.Js.string("value"),
    JSUtil.Js.string(str),
  );
  Vdom.Event.Ignore;
};

let set_textbox_text = (model, text_box_id, _) => {
  let program_string = extract_program_string(model);
  let elem = JSUtil.force_get_elem_by_id(text_box_id);
  elem##.innerHTML := JSUtil.Js.string(program_string);
  elem##setAttribute(
    Js_of_ocaml.Js.string("value"),
    JSUtil.Js.string(program_string),
  );
  Vdom.Event.Ignore;
};

let export_body = (_, model) => {
  Vdom.(
    Node.div(
      [],
      [
        Node.textarea(
          [
            Attr.id("parse-text-box"),
            Attr.value(""),
            Attr.classes(["import-text-area"]),
            Attr.on_change((_, s) =>
              update_textbox_value(model, "parse-text-box", s)
            ),
          ],
          [],
        ),
        Node.button(
          [Attr.on_click(set_textbox_text(model, "parse-text-box"))],
          [Node.text("Export")],
        ),
      ],
    )
  );
};

let import_body = (inject, model) => {
  Vdom.(
    Node.div(
      [],
      [
        Node.textarea(
          [
            Attr.id("parse-text-box"),
            Attr.value(""),
            Attr.classes(["import-text-area"]),
            Attr.on_change((_, s) =>
              update_textbox_value(model, "parse-text-box", s)
            ),
          ],
          [],
        ),
        Node.textarea(
          [Attr.id("parse-errors"), Attr.classes(["import-errors"])],
          [],
        ),
        Node.button(
          [
            Attr.on_click(_ => {
              let elem = JSUtil.force_get_elem_by_id("parse-text-box");
              let cur_text = JSUtil.force_get_attr("value", elem);
              let lexbuf = Lexing.from_string(cur_text);
              switch (Parsing.ast_of_lexbuf(lexbuf)) {
              | (Some(ast), None) =>
                let (ast, _, _) =
                  Statics_Exp.syn_fix_holes(
                    Contexts.empty,
                    MetaVarGen.init,
                    ast,
                  );
                JSUtil.log(Js.string(Serialization.string_of_exp(ast)));
                Event.Ignore;
              | _ => Event.Ignore
              };
            }),
          ],
          [Node.text("Parse to Console")],
        ),
        Node.button(
          [Attr.on_click(set_textbox_text(model, "parse-text-box"))],
          [Node.text("Load current program")],
        ),
        Node.button(
          [
            Attr.on_click(_ => {
              let elem = JSUtil.force_get_elem_by_id("parse-text-box");
              let cur_text = JSUtil.force_get_attr("value", elem);
              let lexbuf = Lexing.from_string(cur_text);
              switch (Parsing.ast_of_lexbuf(lexbuf)) {
              | (Some(ast), _) =>
                let (ast, _, _) =
                  Statics_Exp.syn_fix_holes(
                    Contexts.empty,
                    MetaVarGen.init,
                    ast,
                  );
                let errs_elem = JSUtil.force_get_elem_by_id("parse-errors");
                errs_elem##.innerHTML := JSUtil.Js.string("");
                inject(ModelAction.Import(ast));
              | (_, Some(err_string)) =>
                let errs_elem = JSUtil.force_get_elem_by_id("parse-errors");
                errs_elem##.innerHTML := JSUtil.Js.string(err_string);
                Event.Ignore;
              | (None, None) => Event.Ignore
              };
            }),
          ],
          [Node.text("Import")],
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
            Attr.id("popup-body"),
            Attr.classes(popup_open ? ["popup-body"] : []),
            Attr.on_click(_ => Event.Stop_propagation),
          ],
          [
            Node.div(
              [
                Attr.id("popup-text-block"),
                Attr.classes(popup_open ? ["popup-text-block"] : []),
              ],
              pop_body,
            ),
          ],
        ),
      ],
    )
  );
};

let import = (~inject, ~is_open, ~model) =>
  mk_popup(
    is_open,
    ~inject,
    ~on_toggle=_ => inject(ModelAction.ToggleImportPopup),
    ~popup_body=import_body,
    ~model,
  );

let export = (~inject, ~is_open, ~model) => {
  mk_popup(
    is_open,
    ~inject,
    ~on_toggle=_ => inject(ModelAction.ToggleExportPopup),
    ~popup_body=export_body,
    ~model,
  );
};
