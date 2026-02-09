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

// Stable projector ID for the sidebar app view (deterministic UUID from string)
let sidebar_projector_id = Id.mk_str("app-view-sidebar");

// Evaluate directly (skip elaboration/statics). Expressions from
// MVU runtime contain Closures which the elaborator can't handle.
let evaluate = exp => fst(Evaluator.evaluate(~env=Builtins.env_init, exp));

// Check if an expression is a function (possibly wrapped in a Closure from evaluation)
let is_function = (exp: DHExp.t): bool =>
  switch (exp.term) {
  | Fun(_)
  | FixF(_)
  | Closure(_, {term: Fun(_), _})
  | Closure(_, {term: FixF(_), _}) => true
  | _ => false
  };

// App kind: Elm-style 4-tuple or legacy 3-tuple
type app_kind =
  | ElmApp(DHExp.t, DHExp.t, DHExp.t, DHExp.t) // init_model, update, view, subs
  | LegacyMvuApp(DHExp.t, DHExp.t, DHExp.t); // init_model, view, subs

// Detect app kind: 4-tuple first (more specific), then 3-tuple
let detect_app_kind = (exp: DHExp.t): option(app_kind) => {
  switch (exp.term) {
  // 4-tuple: Elm-style (init_model, update_fn, view_fn, subs_fn)
  | Tuple([init_model, update_fn, view_fn, subs_fn])
  | Parens({term: Tuple([init_model, update_fn, view_fn, subs_fn]), _})
      when is_function(update_fn) && is_function(view_fn) =>
    Some(ElmApp(init_model, update_fn, view_fn, subs_fn))
  // 3-tuple: Legacy MVU (init_model, view_fn, subs_fn)
  | Tuple([init_model, view_fn, subs_fn])
  | Parens({term: Tuple([init_model, view_fn, subs_fn]), _})
      when is_function(view_fn) =>
    Some(LegacyMvuApp(init_model, view_fn, subs_fn))
  | _ => None
  };
};

// Detect MVU App type: returns legacy 3-tuple format for backwards compat
let detect_mvu_app = (exp: DHExp.t): option((DHExp.t, DHExp.t, DHExp.t)) => {
  switch (detect_app_kind(exp)) {
  | Some(LegacyMvuApp(init_model, view_fn, subs_fn)) =>
    Some((init_model, view_fn, subs_fn))
  | Some(ElmApp(init_model, _update_fn, view_fn, subs_fn)) =>
    Some((init_model, view_fn, subs_fn))
  | None => None
  };
};

// Check if expression looks like an MVU App type (3 or 4-tuple with function)
let looks_like_mvu_app = (exp: DHExp.t): bool =>
  Option.is_some(detect_app_kind(exp));

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
let get_evaluated_exp = (cell_editor: CellEditor.Model.t): option(DHExp.t) =>
  switch (cell_editor.result.result |> Calc.get_value) {
  | ProgramResult.ResultOk(inner) => Some(inner.result)
  | ProgramResult.ResultFail(_)
  | ProgramResult.ResultPending => None
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

  // Fallback view_term for unknown terms - render as abbreviated Hazel code
  let fallback_view_term = (d: DHExp.t) => {
    let segment =
      d
      |> DHExp.strip_ascriptions
      |> Abbreviate.abbreviate_exp(~available=40)
      |> fst
      |> ExpToSegment.exp_to_segment(
           ~settings=
             ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
         );
    Node.div(
      ~attrs=[
        Attr.create(
          "style",
          "display: inline-block; "
          ++ "border: 1px solid #feb2b2; border-radius: 4px; "
          ++ "padding: 2px 6px; background: #fff5f5; "
          ++ "max-width: 300px; overflow: hidden;",
        ),
      ],
      [
        ProjectorView.flex_code(
          ~font_metrics=globals.font_metrics,
          ~single_line=true,
          Language.Sort.Exp,
          segment,
        ),
      ],
    );
  };

  // Helper to render HTML with error boundary
  // For MVU apps: model is the user's model, html is view_fn(model)
  // For legacy/plain: model IS the html
  let render_html_content =
      (
        ~model: DHExp.t,
        ~html: DHExp.t,
        ~inject: DHExp.t => Ui_effect.t(unit),
        ~subscriptions: option(DHExp.t),
        ~update_fn: option(DHExp.t)=None,
        (),
      ) =>
    try({
      let mvu: HazelDOM.t = {
        model,
        inject,
        view_term: fallback_view_term,
        projector_id: Some(sidebar_projector_id),
        subscriptions,
        update_fn,
      };
      HazelDOM.manage_subscriptions(mvu);
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
      ~update_fn=state.update_fn,
      (),
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
          update_fn: None,
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
          (),
        );
      | Some((html, None, Some(subs_fn))) =>
        // No init command, just subscriptions
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        render_html_content(
          ~model=html,
          ~html,
          ~inject,
          ~subscriptions=Some(subs),
          (),
        );
      | Some((html, _, None)) =>
        // No subscriptions
        render_html_content(
          ~model=html,
          ~html,
          ~inject,
          ~subscriptions=None,
          (),
        )
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
    | Some(_state) =>
      // We have MVU state - check if eval result changed (auto-refresh)
      switch (eval_result) {
      | Some(exp) when exp !== _state.source_result =>
        // Eval result changed! Check if it's still an MVU app
        switch (detect_app_kind(exp)) {
        | Some(ElmApp(init_model, update_fn, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              RefreshAppView(
                exp,
                init_model,
                Some(update_fn),
                view_fn,
                subs_fn,
              ),
            ),
          );
          render_mvu_app(_state);
        | Some(LegacyMvuApp(init_model, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              RefreshAppView(exp, init_model, None, view_fn, subs_fn),
            ),
          );
          render_mvu_app(_state);
        | None =>
          // Result changed but isn't MVU (intermediate edit state, holes, etc.)
          // Keep showing current state - user can hit Reset for intentional clear
          render_mvu_app(_state)
        }
      | Some(_) => render_mvu_app(_state)
      | None => render_mvu_app(_state)
      }
    | None =>
      // No state - check evaluation result
      switch (eval_result) {
      | None =>
        switch (cell_editor) {
        | None => render_instructions()
        | Some(_) => render_error("Evaluation pending or failed")
        }
      | Some(exp) when looks_like_mvu_app(exp) =>
        switch (detect_app_kind(exp)) {
        | Some(ElmApp(init_model, update_fn, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              InitAppView(
                exp,
                init_model,
                Some(update_fn),
                view_fn,
                subs_fn,
              ),
            ),
          );
          div(
            ~attrs=[
              Attr.create(
                "style",
                "padding: 20px; text-align: center; color: #666;",
              ),
            ],
            [text("Initializing app...")],
          );
        | Some(LegacyMvuApp(init_model, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              InitAppView(exp, init_model, None, view_fn, subs_fn),
            ),
          );
          div(
            ~attrs=[
              Attr.create(
                "style",
                "padding: 20px; text-align: center; color: #666;",
              ),
            ],
            [text("Initializing app...")],
          );
        | None => render_error("Invalid MVU App structure")
        }
      | Some(exp) when looks_like_legacy_app(exp) => render_legacy_app(exp)
      | Some(exp) when looks_like_html(exp) =>
        render_html_content(
          ~model=exp,
          ~html=exp,
          ~inject,
          ~subscriptions=None,
          (),
        )
      | Some(_exp) => render_not_html()
      }
    };

  // Reset button (only shown when we have state)
  let reset_button =
    is_showing_state
      ? Node.button(
          ~attrs=[
            Attr.create(
              "style",
              "margin-left: auto; padding: 4px 8px; font-size: 12px; cursor: pointer; background-color: var(--T1); border: 1px solid var(--BR3); color: var(--BR3); border-radius: 0.3em;",
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
            "padding: 6px 8px; color: var(--BR3); border-bottom: 1px solid #ddd; font-weight: bold; display: flex; align-items: center;",
          ),
        ],
        [text("MVU"), reset_button],
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
