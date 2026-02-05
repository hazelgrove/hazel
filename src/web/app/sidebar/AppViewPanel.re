open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

// App View sidebar panel
// NOTE: This is currently a placeholder. To properly show the app view,
// we need architectural changes to pass the evaluation result to the sidebar.
// The CellEditor.Model.t has the result, but Sidebar.view only receives
// CodeEditable.Model.t which doesn't include evaluation results.
//
// For now, this panel shows instructions on how to use the HTML projector
// inline (by right-clicking on an HTML expression and selecting "Add HTML").

let view =
    (
      ~globals as _: Globals.t,
      ~editor as _: CodeEditable.Model.t,
      ~inject as _: Language.DHExp.t => Ui_effect.t(unit),
    )
    : Node.t => {
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
          Attr.create("style", "flex: 1; padding: 20px;"),
        ],
        [
          div(
            ~attrs=[
              Attr.create(
                "style",
                "text-align: center; padding: 40px; color: #666;",
              ),
            ],
            [
              div(
                ~attrs=[
                  Attr.create(
                    "style",
                    "font-size: 48px; margin-bottom: 20px;",
                  ),
                ],
                [text("🖥")],
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
                    ~attrs=[
                      Attr.create("style", "color: #888; font-size: 12px;"),
                    ],
                    [
                      text(
                        "Note: A dedicated sidebar app viewer is planned for future versions.",
                      ),
                    ],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ],
  );
};
