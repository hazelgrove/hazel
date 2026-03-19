open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;
open Haz3lcore;
open CodeViewable;

let render_segment = (~globals: Globals.t, segment: Segment.t): Node.t => {
  let completed_segment = Indentation.shallow_complete_segment(segment);
  view_segment(~globals, completed_segment);
};

let render_pretty_args = (args: API.Json.t): Node.t => {
  let rec render_value = (json: API.Json.t): Node.t =>
    switch (json) {
    | `String(s) => span(~attrs=[clss(["arg-string"])], [text(s)])
    | `Int(i) =>
      span(~attrs=[clss(["arg-number"])], [text(string_of_int(i))])
    | `Float(f) =>
      span(~attrs=[clss(["arg-number"])], [text(string_of_float(f))])
    | `Bool(b) =>
      span(~attrs=[clss(["arg-bool"])], [text(b ? "true" : "false")])
    | `Null => span(~attrs=[clss(["arg-null"])], [text("null")])
    | `List(items) =>
      div(
        ~attrs=[clss(["arg-list"])],
        List.mapi(
          (i, item) =>
            div(
              ~attrs=[clss(["arg-list-item"])],
              [
                span(
                  ~attrs=[clss(["arg-list-index"])],
                  [text(string_of_int(i + 1) ++ ".")],
                ),
                render_value(item),
              ],
            ),
          items,
        ),
      )
    | `Assoc(pairs) =>
      div(
        ~attrs=[clss(["arg-object"])],
        List.map(
          ((key, value)) =>
            div(
              ~attrs=[clss(["arg-field"])],
              [
                span(~attrs=[clss(["arg-field-key"])], [text(key)]),
                render_value(value),
              ],
            ),
          pairs,
        ),
      )
    | _ => span(~attrs=[], [text(Yojson.Safe.to_string(json))])
    };

  switch (args) {
  | `Assoc(pairs) =>
    div(
      ~attrs=[clss(["tool-call-args-pretty"])],
      List.map(
        ((key, value)) =>
          div(
            ~attrs=[clss(["arg-field"])],
            [
              span(~attrs=[clss(["arg-field-key"])], [text(key)]),
              render_value(value),
            ],
          ),
        pairs,
      ),
    )
  | _ =>
    div(
      ~attrs=[clss(["tool-call-args-value"])],
      [text(Yojson.Safe.pretty_to_string(args))],
    )
  };
};

let view =
    (
      ~globals: Globals.t,
      ~tool_result: AgentToolResult.tool_result,
      ~toggle_expanded: 'a => Effect.t(unit),
    )
    : Node.t => {
  let status_icon = tool_result.success ? Icons.confirm : Icons.cancel;
  let status_class =
    tool_result.success ? "tool-call-success" : "tool-call-failure";
  let dom_id = "tool-call-" ++ tool_result.tool_call.id;
  div(
    ~attrs=[clss(["agent-tool-call-inline"]), Attr.id(dom_id)],
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
                render_pretty_args(tool_result.tool_call.args),
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
