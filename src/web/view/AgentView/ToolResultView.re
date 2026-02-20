open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open CodeViewable;

// Shared component for rendering tool results
// Used by both ChatMessagesView and WorkbenchView

// Helper to render a segment safely
let render_segment = (~globals: Globals.t, segment: Segment.t): Node.t => {
  // Complete the segment to fix any structural issues
  let completed_segment = Indentation.shallow_complete_segment(segment);
  // Use CodeViewable which is safer for incomplete segments
  view_segment(~globals, completed_segment);
};

let view =
    (
      ~globals: Globals.t,
      ~tool_result: OpenRouter.Reply.Model.tool_result,
      ~toggle_expanded: 'a => Effect.t(unit),
    )
    : Node.t => {
  let status_icon = tool_result.success ? Icons.confirm : Icons.cancel;
  let status_class =
    tool_result.success ? "tool-call-success" : "tool-call-failure";
  div(
    ~attrs=[clss(["agent-tool-call-inline"])],
    [
      div(
        ~attrs=[
          clss(["tool-call-header", tool_result.expanded ? "expanded" : ""]),
          Attr.on_click(toggle_expanded),
        ],
        [
          div(
            ~attrs=[clss(["tool-call-status-icon", status_class])],
            [status_icon],
          ),
          div(
            ~attrs=[clss(["tool-call-name"])],
            [text(tool_result.tool_call.name)],
          ),
        ],
      ),
      if (tool_result.expanded) {
        div(
          ~attrs=[clss(["tool-call-content"])],
          [
            div(
              ~attrs=[clss(["tool-call-args"])],
              [
                div(
                  ~attrs=[clss(["tool-call-args-label"])],
                  [text("Arguments:")],
                ),
                div(
                  ~attrs=[clss(["tool-call-args-value"])],
                  [
                    text(
                      Yojson.Safe.pretty_to_string(
                        tool_result.tool_call.args,
                      ),
                    ),
                  ],
                ),
              ],
            ),
            div(
              ~attrs=[clss(["tool-call-result"])],
              [
                div(
                  ~attrs=[clss(["tool-call-result-label"])],
                  [text("Result:")],
                ),
                div(
                  ~attrs=[clss(["tool-call-result-value"])],
                  [text(tool_result.content)],
                ),
              ],
            ),
            // Display diff if available
            switch (tool_result.diff) {
            | Some(diff) =>
              div(
                ~attrs=[clss(["tool-call-diff-container"])],
                [
                  div(
                    ~attrs=[clss(["tool-call-diff-scrollable"])],
                    [
                      div(
                        ~attrs=[
                          clss(["tool-call-diff-segment", "old-segment"]),
                        ],
                        [
                          div(
                            ~attrs=[clss(["tool-call-diff-label"])],
                            [text("Before:")],
                          ),
                          render_segment(~globals, diff.old_segment),
                        ],
                      ),
                      switch (diff.new_segment) {
                      | Some(new_segment) =>
                        div(
                          ~attrs=[
                            clss(["tool-call-diff-segment", "new-segment"]),
                          ],
                          [
                            div(
                              ~attrs=[clss(["tool-call-diff-label"])],
                              [text("After:")],
                            ),
                            render_segment(~globals, new_segment),
                          ],
                        )
                      | None => div(~attrs=[], [])
                      },
                    ],
                  ),
                ],
              )
            | None =>
              div(
                ~attrs=[clss(["tool-call-diff-empty"])],
                [text("no diff to display")],
              )
            },
          ],
        );
      } else {
        div(~attrs=[], []);
      },
    ],
  );
};
