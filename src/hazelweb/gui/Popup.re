open Incr_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;
module Parsing = Parser.Parsing;
module Print = Parser.Print;

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
            Attr.on_change((_, s) => {
              let t = JSUtil.force_get_elem_by_id("parse-text-box");
              t##setAttribute(
                Js_of_ocaml.Js.string("value"),
                JSUtil.Js.string(s),
              );
              Event.Ignore;
            }),
          ],
          [],
        ),
        Node.button(
          [
            Attr.on_click(_ => {
              let p = Model.get_program(model);
              let l = Program.get_layout(~settings=Settings.init, p);
              // Place the text into the text area
              let s = Print.string_of_layout(l);
              let e = JSUtil.force_get_elem_by_id("parse-text-box");
              e##.innerHTML := JSUtil.Js.string(s);
              e##setAttribute(
                Js_of_ocaml.Js.string("value"),
                JSUtil.Js.string(s),
              );
              Event.Ignore;
            }),
          ],
          [Node.text("Export")],
        ),
      ],
    )
  );
};

let import_body = (inject, _) => {
  Vdom.(
    Node.div(
      [],
      [
        Node.textarea(
          [
            Attr.id("parse-text-box"),
            Attr.value(""),
            Attr.classes(["import-text-area"]),
            Attr.on_change((_, s) => {
              let t = JSUtil.force_get_elem_by_id("parse-text-box");
              t##setAttribute(
                Js_of_ocaml.Js.string("value"),
                JSUtil.Js.string(s),
              );
              Event.Ignore;
            }),
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
              let e = JSUtil.force_get_elem_by_id("parse-text-box");
              let s = JSUtil.force_get_attr("value", e);

              let l = Lexing.from_string(s);
              switch (Parsing.ast_of_layout(l)) {
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
          [
            Attr.on_click(_ => {
              let e = JSUtil.force_get_elem_by_id("parse-text-box");
              let s = JSUtil.force_get_attr("value", e);
              let l = Lexing.from_string(s);
              switch (Parsing.ast_of_layout(l)) {
              | (Some(ast), _) =>
                let (ast, _, _) =
                  Statics_Exp.syn_fix_holes(
                    Contexts.empty,
                    MetaVarGen.init,
                    ast,
                  );
                let e = JSUtil.force_get_elem_by_id("parse-errors");
                e##.innerHTML := JSUtil.Js.string("");
                inject(ModelAction.Import(ast));
              | (_, Some(err_string)) =>
                let e = JSUtil.force_get_elem_by_id("parse-errors");
                e##.innerHTML := JSUtil.Js.string(err_string);
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
