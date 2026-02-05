open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;
open Language;

// App View sidebar panel - renders HTML evaluation results

// Extract the evaluated DHExp from a CellEditor
let get_evaluated_exp = (cell_editor: CellEditor.Model.t): option(DHExp.t) => {
  let result = cell_editor.result.result |> Calc.get_value;
  switch (result) {
  | ProgramResult.ResultOk(inner) => Some(inner.result)
  | ProgramResult.ResultFail(_)
  | ProgramResult.ResultPending => None
  };
};

// Check if an expression looks like HTML
let looks_like_html = (d: DHExp.t): bool =>
  switch (d.term) {
  | Ap(_, {term: Constructor(name, _), _}, _) =>
    HazelDOM.of_constructor(d)
    |> Option.is_some
    && List.mem(
         name,
         [
           "Text",
           "Bool",
           "Int",
           "Float",
           "Div",
           "Span",
           "P",
           "Pre",
           "Code",
           "Blockquote",
           "H1",
           "H2",
           "H3",
           "H4",
           "H5",
           "H6",
           "Ul",
           "Ol",
           "Li",
           "Form",
           "Label",
           "Input",
           "TextArea",
           "Button",
           "Select",
           "Option",
           "Checkbox",
           "Radio",
           "Range",
           "A",
           "Img",
           "Table",
           "Thead",
           "Tbody",
           "Tr",
           "Th",
           "Td",
           "Header",
           "Footer",
           "Nav",
           "Main",
           "Section",
           "Article",
           "Aside",
           "Br",
           "Hr",
           "Node",
         ],
       )
  | Constructor("Br", _) => true
  | _ => false
  };

// Render the placeholder/instructions view
let render_instructions = (): Node.t =>
  div(
    ~attrs=[
      Attr.create("style", "text-align: center; padding: 40px; color: #666;"),
    ],
    [
      div(
        ~attrs=[
          Attr.create("style", "font-size: 48px; margin-bottom: 20px;"),
        ],
        [text("\xF0\x9F\x96\xA5")],
      ),
      div(
        ~attrs=[
          Attr.create(
            "style",
            "font-size: 18px; margin-bottom: 15px; color: #333;",
          ),
        ],
        [text("HazelHtml Apps")],
      ),
      div(
        ~attrs=[
          Attr.create(
            "style",
            "font-size: 14px; line-height: 1.8; text-align: left; max-width: 350px; margin: 0 auto;",
          ),
        ],
        [
          div(
            ~attrs=[
              Attr.create(
                "style",
                "margin-bottom: 15px; padding: 10px; background: #f8f8f8; border-radius: 4px;",
              ),
            ],
            [
              div(
                ~attrs=[
                  Attr.create(
                    "style",
                    "font-weight: bold; margin-bottom: 5px;",
                  ),
                ],
                [text("To view HTML inline:")],
              ),
              text("1. Write an HTML expression"),
              Node.create("br", []),
              text("2. Right-click on the expression"),
              Node.create("br", []),
              text("3. Select \"Add HTML\" from the menu"),
            ],
          ),
          div(
            ~attrs=[
              Attr.create(
                "style",
                "margin-bottom: 15px; padding: 10px; background: #f0f8ff; border-radius: 4px;",
              ),
            ],
            [
              div(
                ~attrs=[
                  Attr.create(
                    "style",
                    "font-weight: bold; margin-bottom: 5px;",
                  ),
                ],
                [text("Example expressions:")],
              ),
              Node.create(
                "code",
                ~attrs=[
                  Attr.create(
                    "style",
                    "display: block; padding: 5px; background: #fff; margin: 5px 0; font-size: 12px;",
                  ),
                ],
                [text("Div([], [Text(\"Hello\")])")],
              ),
              Node.create(
                "code",
                ~attrs=[
                  Attr.create(
                    "style",
                    "display: block; padding: 5px; background: #fff; margin: 5px 0; font-size: 12px;",
                  ),
                ],
                [text("Button([OnClick(...)], [Text(\"Click\")])")],
              ),
            ],
          ),
          div(
            ~attrs=[Attr.create("style", "color: #888; font-size: 12px;")],
            [
              text(
                "The evaluated result will appear here when it's valid HTML.",
              ),
            ],
          ),
        ],
      ),
    ],
  );

// Render an error message
let render_error = (msg: string): Node.t =>
  div(
    ~attrs=[
      Attr.create(
        "style",
        "padding: 20px; color: #c00; background: #fff0f0; border-radius: 4px; margin: 10px;",
      ),
    ],
    [text(msg)],
  );

// Render the "not HTML" message with the actual type/value
let render_not_html = (): Node.t =>
  div(
    ~attrs=[
      Attr.create("style", "padding: 20px; color: #666; text-align: center;"),
    ],
    [
      div(
        ~attrs=[
          Attr.create("style", "font-size: 14px; margin-bottom: 10px;"),
        ],
        [text("The evaluated result is not an HTML expression.")],
      ),
      div(
        ~attrs=[Attr.create("style", "font-size: 12px; color: #888;")],
        [
          text(
            "Write an expression that evaluates to an HTML type (like Div, Span, Text, etc.)",
          ),
        ],
      ),
    ],
  );

let view =
    (
      ~globals as _: Globals.t,
      ~cell_editor: option(CellEditor.Model.t),
      ~inject: DHExp.t => Ui_effect.t(unit),
    )
    : Node.t => {
  // Get the content to render
  let content =
    switch (cell_editor) {
    | None => render_instructions()
    | Some(editor) =>
      switch (get_evaluated_exp(editor)) {
      | None => render_error("Evaluation pending or failed")
      | Some(exp) when looks_like_html(exp) =>
        // Create the MVU context for rendering
        let mvu: HazelDOM.t = {
          model: exp,
          inject,
          view_term: d => {
            // Fallback for unknown terms - render as placeholder
            Node.span(
              ~attrs=[
                Attr.create(
                  "style",
                  "background: #ffe0e0; padding: 2px 4px; border-radius: 2px;",
                ),
              ],
              [text("[" ++ DHExp.show(d) ++ "]")],
            );
          },
          projector_id: None,
          subscriptions: None,
        };
        // Render the HTML
        div(
          ~attrs=[
            clss(["app-view-content"]),
            Attr.create(
              "style",
              "padding: 15px; background: white; min-height: 100px;",
            ),
          ],
          [HazelDOM.render_elem(mvu, exp)],
        );
      | Some(_) => render_not_html()
      }
    };

  div(
    ~attrs=[clss(["app-view-panel"])],
    [
      // Header
      div(
        ~attrs=[
          clss(["app-view-header"]),
          Attr.create(
            "style",
            "padding: 10px 15px; background: #f5f5f5; border-bottom: 1px solid #ddd; font-weight: bold;",
          ),
        ],
        [text("App View")],
      ),
      // Content area
      div(
        ~attrs=[
          clss(["app-view-body"]),
          Attr.create("style", "flex: 1; overflow: auto;"),
        ],
        [content],
      ),
    ],
  );
};
