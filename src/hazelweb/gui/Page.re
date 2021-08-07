open Virtual_dom.Vdom;
open Node;
module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;
module Parse = Parser.Parse;
module Hazel_parser = Parser.Hazel_parser;
module Print = Parser.Print;
let logo_panel =
  a(
    [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
    [text("Hazel")],
  );

let branch_panel =
  span(
    [Attr.classes(["branch-panel"])],
    [
      text("["),
      text(Version_autogenerated.branch),
      text(" @ "),
      text(Version_autogenerated.commit_hash_short),
      text(" ("),
      text(Version_autogenerated.commit_time),
      text(")]"),
    ],
  );

let top_bar = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  div(
    [Attr.classes(["top-bar"])],
    [
      logo_panel,
      CardsPanel.view(~inject, ~model),
      ActionMenu.view(~inject),
    ],
  );
};

let cell_status_panel = (~settings: Settings.t, ~model: Model.t, ~inject) => {
  let program = Model.get_program(model);
  let selected_instance = Model.get_selected_hole_instance(model);
  let (_, ty, _) = program.edit_state;
  let result =
    settings.evaluation.show_unevaluated_elaboration
      ? program |> Program.get_elaboration
      : program |> Program.get_result |> Result.get_dhexp;
  div(
    [],
    [
      div(
        [Attr.classes(["cell-status"])],
        [
          div(
            [Attr.classes(["type-indicator"])],
            [
              div(
                [Attr.classes(["type-label"])],
                [text("Result of type: ")],
              ),
              div([Attr.classes(["htype-view"])], [HTypCode.view(ty)]),
            ],
          ),
        ],
      ),
      div(
        [Attr.classes(["result-view"])],
        [
          DHCode.view(
            ~inject,
            ~selected_instance,
            ~settings=settings.evaluation,
            ~width=80,
            ~font_metrics=model.font_metrics,
            result,
          ),
        ],
      ),
    ],
  );
};

let left_sidebar = (~inject: ModelAction.t => Event.t, ~model: Model.t) =>
  Sidebar.left(~inject, ~is_open=model.left_sidebar_open, () =>
    [ActionPanel.view(~inject, model)]
  );

let right_sidebar = (~inject: ModelAction.t => Event.t, ~model: Model.t) => {
  let settings = model.settings;
  let program = Model.get_program(model);
  let selected_instance = Model.get_selected_hole_instance(model);
  Sidebar.right(~inject, ~is_open=model.right_sidebar_open, () =>
    [
      ContextInspector.view(
        ~inject,
        ~selected_instance,
        ~settings=settings.evaluation,
        ~font_metrics=model.font_metrics,
        program,
      ),
      UndoHistoryPanel.view(~inject, model),
      SettingsPanel.view(~inject, settings),
    ]
  );
};

let get_ast = l =>
  try(Some(Parse.parse(l, Hazel_parser.Incremental.main(l.lex_curr_p)))) {
  | Parse.SyntaxError((pos, tok)) =>
    switch (pos) {
    | Some((line, col)) =>
      JSUtil.log(
        Printf.sprintf(
          "ERROR on line %d, column %d. Token: %s",
          line,
          col,
          tok,
        ),
      );
      None;
    | None => None
    }
  };

let parse_test = (~inject: ModelAction.t => Event.t) => {
  div(
    [],
    [
      Node.textarea(
        [
          Attr.id("parse_test"),
          Attr.classes(["page"]),
          Attr.value(""),
          Attr.on_change((_, s) => {
            let t = JSUtil.force_get_elem_by_id("parse_test");
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
            let e = JSUtil.force_get_elem_by_id("parse_test");
            let s = JSUtil.force_get_attr("value", e);

            let l = Lexing.from_string(s);
            switch (get_ast(l)) {
            | Some(ast) =>
              let (ast, _, _) =
                Statics_Exp.syn_fix_holes(
                  Contexts.empty,
                  MetaVarGen.init,
                  ast,
                );
              JSUtil.log(Js.string(Serialization.string_of_exp(ast)));
              Event.Ignore;
            | None => Event.Ignore
            };
          }),
        ],
        [Node.text("Parse to Console")],
      ),
      Node.button(
        [
          Attr.on_click(_ => {
            let e = JSUtil.force_get_elem_by_id("parse_test");
            let s = JSUtil.force_get_attr("value", e);

            inject(ModelAction.Import(s));
          }),
        ],
        [Node.text("Import")],
      ),
    ],
  );
};

let view = (~inject: ModelAction.t => Event.t, model: Model.t) => {
  let settings = model.settings;
  TimeUtil.measure_time(
    "Page.view",
    settings.performance.measure && settings.performance.page_view,
    () => {
      let card_caption = Model.get_card(model).info.caption;
      let cell_status =
        !settings.evaluation.evaluate
          ? div([], []) : cell_status_panel(~settings, ~model, ~inject);
      div(
        [Attr.id("root")],
        [
          top_bar(~inject, ~model),
          div(
            [Attr.classes(["main-area"])],
            [
              left_sidebar(~inject, ~model),
              div(
                [Attr.classes(["flex-wrapper"])],
                [
                  div(
                    [Attr.id("page-area")],
                    [
                      div(
                        [Attr.classes(["page"])],
                        [
                          div(
                            [Attr.classes(["card-caption"])],
                            [card_caption],
                          ),
                          Cell.view(~inject, model),
                          cell_status,
                        ],
                      ),
                      Node.button(
                        [
                          Attr.on_click(_ => {
                            let program = Model.get_program(model);
                            let e = program |> Program.get_uhexp;
                            JSUtil.log(
                              Js.string(Serialization.string_of_exp(e)),
                            );
                            Event.Ignore;
                          }),
                        ],
                        [Node.text("Serialize to console")],
                      ),
                      Node.button(
                        [
                          Attr.on_click(_ => {
                            let p = Model.get_program(model);
                            let l =
                              Program.get_layout(~settings=Settings.init, p);
                            // Place the text into the text area
                            let s = Print.string_of_layout(l);
                            let e = JSUtil.force_get_elem_by_id("parse_test");
                            e##.innerHTML := JSUtil.Js.string(s);
                            e##setAttribute(
                              Js_of_ocaml.Js.string("value"),
                              JSUtil.Js.string(s),
                            );
                            Event.Ignore;
                          }),
                        ],
                        [Node.text("Print")],
                      ),
                      parse_test(~inject),
                      div(
                        [
                          Attr.style(
                            Css_gen.(
                              white_space(`Pre) @> font_family(["monospace"])
                            ),
                          ),
                        ],
                        [branch_panel],
                      ),
                    ],
                  ),
                ],
              ),
              right_sidebar(~inject, ~model),
            ],
          ),
        ],
      );
    },
  );
};
