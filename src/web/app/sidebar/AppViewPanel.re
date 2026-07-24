open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;
open Language;

// App View sidebar panel - renders MVU apps (the Elm-style 4-tuple
// (init, update, view, subs)) plus bare HTML values. Handlers produce
// msgs; the AppStore commits them by evaluating update(msg, model).

// The sidebar app lives in the AppStore under a stable synthetic id
// (deterministic UUID from string; see AppStore.sidebar_id).

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
    MvuShape.of_constructor(d)
    |> Option.is_some
    && MvuShape.is_html_constructor(name)
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

  // Check if we have active MVU state for the sidebar app
  let sidebar_entry = AppStore.lookup(AppStore.sidebar_id, globals.apps);
  let is_showing_state = Option.is_some(sidebar_entry);

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

  // Helper to render HTML with error boundary.
  // Render-only: subscriptions are owned by the AppStore update path.
  let render_html_content =
      (~html: DHExp.t, ~inject: DHExp.t => Ui_effect.t(unit)) =>
    try({
      let mvu: HazelDOM.t = {
        inject,
        view_term: fallback_view_term,
        commit: HazelDOM.State,
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

  // Render MVU app from pre-computed store entry (no evaluation here!)
  let render_mvu_app = (entry: AppStore.Entry.t) =>
    render_html_content(~html=entry.html, ~inject);

  // Get the content to render
  let content =
    switch (sidebar_entry) {
    | Some(entry) =>
      // We have MVU state - check if eval result changed (auto-refresh)
      switch (eval_result) {
      | Some(exp) when exp !== entry.source_result =>
        // Eval result changed! Check if it's still an MVU app
        switch (MvuShape.detect_app_kind(exp)) {
        | Some(MvuShape.ElmApp(init_model, update_fn, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              RefreshAppView(
                AppStore.sidebar_id,
                exp,
                init_model,
                update_fn,
                view_fn,
                subs_fn,
              ),
            ),
          );
          render_mvu_app(entry);
        | None =>
          // Result changed but isn't MVU (intermediate edit state, holes, etc.)
          // Keep showing current state - user can hit Reset for intentional clear
          render_mvu_app(entry)
        }
      | Some(_) => render_mvu_app(entry)
      | None => render_mvu_app(entry)
      }
    | None =>
      // No state - check evaluation result
      switch (eval_result) {
      | None =>
        switch (cell_editor) {
        | None => render_instructions()
        | Some(_) => render_error("Evaluation pending or failed")
        }
      | Some(exp) when MvuShape.looks_like_mvu_app(exp) =>
        switch (MvuShape.detect_app_kind(exp)) {
        | Some(MvuShape.ElmApp(init_model, update_fn, view_fn, subs_fn)) =>
          Bonsai.Effect.Expert.handle(
            globals.inject_global(
              InitAppView(
                AppStore.sidebar_id,
                exp,
                init_model,
                update_fn,
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
        | None => render_error("Invalid MVU App structure")
        }
      | Some(exp) when looks_like_html(exp) =>
        render_html_content(~html=exp, ~inject)
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
            Attr.on_click(_ =>
              globals.inject_global(ResetAppView(AppStore.sidebar_id))
            ),
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
