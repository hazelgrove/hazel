open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;
open Language;
open IdTagged.FreshGrammar;

// App View sidebar panel - renders HTML evaluation results with MVU architecture
//
// Supports two app types:
// 1. MVU App: (init_model, view: model -> Html, subs: model -> Sub)
//    - Handlers are: model -> model
//    - Sidebar manages model state, calls view_fn(model) to get HTML
//
// 2. Self-modifying App (legacy): ((HTML, Cmd), HTML -> Sub)
//    - Handlers are: Html -> Html
//    - The HTML tree IS the model

// Evaluate a Hazel expression
let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );

// Detect MVU App type: (init_model, view_fn, subs_fn) - a 3-tuple
// Returns Some((init_model, view_fn, subs_fn)) or None
let detect_mvu_app =
    (exp: DHExp.t)
    : option((DHExp.t, DHExp.t, DHExp.t)) => {
  switch (exp.term) {
  | Tuple([init_model, view_fn, subs_fn])
  | Parens({term: Tuple([init_model, view_fn, subs_fn]), _}) =>
    // Check that view_fn looks like a function (Fun or fix)
    switch (view_fn.term) {
    | Fun(_)
    | FixF(_) => Some((init_model, view_fn, subs_fn))
    | _ => None
    }
  | _ => None
  };
};

// Check if expression looks like an MVU App type (3-tuple with function)
let looks_like_mvu_app = (exp: DHExp.t): bool =>
  Option.is_some(detect_mvu_app(exp));

// Detect legacy self-modifying App type: ((HTML, Cmd), HTML -> Sub)
// Returns Some((html_model, init_cmd, subscriptions_fn)) or None
let detect_legacy_app =
    (exp: DHExp.t): option((DHExp.t, option(DHExp.t), option(DHExp.t))) => {
  switch (exp.term) {
  | Tuple([init, subs_fn])
  | Parens({term: Tuple([init, subs_fn]), _}) =>
    switch (init.term) {
    | Tuple([html_model, init_cmd])
    | Parens({term: Tuple([html_model, init_cmd]), _}) =>
      Some((html_model, Some(init_cmd), Some(subs_fn)))
    | _ => None
    }
  | _ => None
  };
};

// Check if expression looks like a legacy App type
let looks_like_legacy_app = (exp: DHExp.t): bool =>
  switch (exp.term) {
  | Tuple([init, _subs_fn])
  | Parens({term: Tuple([init, _subs_fn]), _}) =>
    switch (init.term) {
    | Tuple([_html, _cmd])
    | Parens({term: Tuple([_html, _cmd]), _}) => true
    | _ => false
    }
  | _ => false
  };

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
      ~globals: Globals.t,
      ~cell_editor: option(CellEditor.Model.t),
      ~inject: DHExp.t => Ui_effect.t(unit),
    )
    : Node.t => {
  // Get the evaluation result (for detecting app type)
  let eval_result: option(DHExp.t) =
    switch (cell_editor) {
    | None => None
    | Some(editor) => get_evaluated_exp(editor)
    };

  // Check if we have active MVU state
  let is_showing_state = Option.is_some(globals.app_view_state);

  // Fallback view_term for unknown terms - render as placeholder
  let fallback_view_term = (d: DHExp.t) =>
    Node.span(
      ~attrs=[
        Attr.create(
          "style",
          "background: #ffe0e0; padding: 2px 4px; border-radius: 2px;",
        ),
      ],
      [text("[" ++ DHExp.show(d) ++ "]")],
    );

  // Helper to render HTML with error boundary
  // For MVU apps: model is the user's model, html is view_fn(model)
  // For legacy/plain: model IS the html
  let render_html_content =
      (
        ~model: DHExp.t,
        ~html: DHExp.t,
        ~inject: DHExp.t => Ui_effect.t(unit),
        ~subscriptions: option(DHExp.t),
      ) =>
    try({
      let mvu: HazelDOM.t = {
        model,
        inject,
        view_term: fallback_view_term,
        projector_id: None, // TODO: Use a stable ID for subscription tracking
        subscriptions,
      };
      div(
        ~attrs=[
          clss(["app-view-content"]),
          Attr.create(
            "style",
            "padding: 15px; background: white; min-height: 100px;",
          ),
        ],
        [HazelDOM.render_elem(mvu, html)],
      );
    }) {
    | exn =>
      let msg = Printexc.to_string(exn);
      render_error("Render error: " ++ msg);
    };

  // Render MVU app from pre-computed state (no evaluation here!)
  let render_mvu_app = (state: Globals.AppViewState.t) =>
    // Use pre-computed html and subs from state - evaluation happened in update handler
    render_html_content(
      ~model=state.model,
      ~html=state.html,
      ~inject,
      ~subscriptions=Some(state.subs),
    );

  // Render legacy self-modifying app
  let render_legacy_app = (exp: DHExp.t) =>
    try(
      switch (detect_legacy_app(exp)) {
      | Some((html, Some(init_cmd), Some(subs_fn))) =>
        // Run the init command
        let cmd_ctx: CmdRunner.context = {
          model: html,
          inject,
        };
        let cmd_effect = CmdRunner.run(cmd_ctx, init_cmd);
        Bonsai.Effect.Expert.handle(cmd_effect);
        // Evaluate subscriptions
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        render_html_content(
          ~model=html,
          ~html,
          ~inject,
          ~subscriptions=Some(subs),
        );
      | Some((html, None, Some(subs_fn))) =>
        // No init command, just subscriptions
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        render_html_content(
          ~model=html,
          ~html,
          ~inject,
          ~subscriptions=Some(subs),
        );
      | Some((html, _, None)) =>
        // No subscriptions
        render_html_content(~model=html, ~html, ~inject, ~subscriptions=None)
      | None =>
        // Failed to detect app structure
        render_error("Invalid App structure")
      }
    ) {
    | exn =>
      let msg = Printexc.to_string(exn);
      render_error("Legacy app error: " ++ msg);
    };

  // Get the content to render
  let content =
    switch (globals.app_view_state) {
    | Some(state) =>
      // We have MVU state - render from it
      render_mvu_app(state)
    | None =>
      // No state - check evaluation result
      switch (eval_result) {
      | None =>
        switch (cell_editor) {
        | None => render_instructions()
        | Some(_) => render_error("Evaluation pending or failed")
        }
      | Some(exp) when looks_like_mvu_app(exp) =>
        // It's an MVU App - trigger initialization (evaluation happens in update handler)
        switch (detect_mvu_app(exp)) {
        | Some((init_model, view_fn, subs_fn)) =>
          // Dispatch action - evaluation will happen in Page.re update handler
          Bonsai.Effect.Expert.handle(
            globals.inject_global(InitAppView(init_model, view_fn, subs_fn)),
          );
          // Show loading state while waiting for evaluation to complete
          div(
            ~attrs=[
              Attr.create(
                "style",
                "padding: 20px; text-align: center; color: #666;",
              ),
            ],
            [text("Initializing app...")],
          )
        | None => render_error("Invalid MVU App structure")
        }
      | Some(exp) when looks_like_legacy_app(exp) =>
        // Legacy self-modifying app
        render_legacy_app(exp)
      | Some(exp) when looks_like_html(exp) =>
        // Plain HTML - no subscriptions, model IS html
        render_html_content(~model=exp, ~html=exp, ~inject, ~subscriptions=None)
      | Some(_) => render_not_html()
      }
    };

  // Reset button (only shown when we have state)
  let reset_button =
    is_showing_state
      ? Node.button(
          ~attrs=[
            Attr.create(
              "style",
              "margin-left: auto; padding: 4px 8px; font-size: 12px; cursor: pointer;",
            ),
            Attr.on_click(_ => globals.inject_global(ResetAppView)),
          ],
          [text("Reset")],
        )
      : Node.none;

  div(
    ~attrs=[clss(["app-view-panel"])],
    [
      // Header with reset button
      div(
        ~attrs=[
          clss(["app-view-header"]),
          Attr.create(
            "style",
            "padding: 10px 15px; background: #f5f5f5; border-bottom: 1px solid #ddd; font-weight: bold; display: flex; align-items: center;",
          ),
        ],
        [text("App View"), reset_button],
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
