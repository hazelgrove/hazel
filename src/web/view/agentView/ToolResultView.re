open Virtual_dom.Vdom;
open Node;
open Util_web;
open Util_web.WebUtil;
open Haz3lcore;
open CodeViewable;
open Icons;

let render_segment =
    (~globals: Globals.t, ~shallow_complete: bool=true, segment: Segment.t)
    : Node.t => {
  let segment' =
    shallow_complete
      ? Indentation.shallow_complete_segment(segment) : segment;
  view_segment(~globals, segment');
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

let category_icon = (c: ToolCallSummary.category): Node.t =>
  switch (c) {
  | Edit => cat_edit
  | Read => cat_read
  | View => cat_view
  | Projector => cat_projector
  | Probe => cat_probe
  | Statics => cat_statics
  | Workbench => cat_workbench
  };

/** Returns [[Some(id)]] for the first path in [[jump_paths]] that resolves in
    [[node_map]]; [[None]] otherwise (including when [[node_map]] is [[None]]). */
let first_resolving_id =
    (~node_map: option(HighLevelNodeMap.t), jump_paths: list(string))
    : option(Id.t) =>
  switch (node_map) {
  | None => None
  | Some(nm) =>
    List.find_map(
      (p: string) => HighLevelNodeMap.Public.path_to_id_opt(nm, p),
      jump_paths,
    )
  };

let view =
    (
      ~globals: Globals.t,
      ~node_map: option(HighLevelNodeMap.t)=?,
      ~tool_result: AgentToolResult.tool_result,
      ~toggle_expanded: 'a => Effect.t(unit),
      (),
    )
    : Node.t => {
  let (status_icon, status_class) =
    if (tool_result.skipped) {
      (circle_with_minus, "tool-call-skipped");
    } else if (tool_result.success) {
      (confirm, "tool-call-success");
    } else {
      (cancel, "tool-call-failure");
    };
  let summary_opt = ToolCallSummary.of_tool_call(tool_result.tool_call);
  let resolved_id: option(Id.t) =
    switch (summary_opt) {
    | Some(s) when s.jump_paths != [] =>
      first_resolving_id(~node_map, s.jump_paths)
    | _ => None
    };
  let is_stale: bool =
    switch (summary_opt) {
    | Some(s) when s.persists && s.jump_paths != [] =>
      resolved_id == None && node_map != None
    | _ => false
    };
  let category_node: Node.t =
    switch (summary_opt) {
    | Some(s) =>
      div(
        ~attrs=[
          clss([
            "tool-call-category-icon",
            ToolCallSummary.category_class(s.category),
          ]),
        ],
        [category_icon(s.category)],
      )
    | None => Node.none
    };
  let signifier_node: Node.t =
    switch (summary_opt) {
    | Some({signifier: Some(s), _}) =>
      span(~attrs=[clss(["tool-call-signifier"])], [text(s)])
    | _ => Node.none
    };
  let on_header_click = (evt): Effect.t(unit) => {
    let is_meta_click = Key.ctrl_held(evt) || Key.meta_held(evt);
    switch (is_meta_click, resolved_id) {
    | (true, Some(id)) =>
      Effect.Many([
        globals.inject_global(Globals.Update.JumpToTile(id)),
        Effect.Stop_propagation,
      ])
    | _ => toggle_expanded(evt)
    };
  };
  let dom_id = "tool-call-" ++ tool_result.tool_call.id;
  let root_classes =
    ["agent-tool-call-inline"] @ (is_stale ? ["stale"] : []);
  div(
    ~attrs=[clss(root_classes), Attr.id(dom_id)],
    [
      div(
        ~attrs=[
          clss(["tool-call-header", tool_result.expanded ? "expanded" : ""]),
          Attr.on_click(on_header_click),
        ],
        [
          category_node,
          div(
            ~attrs=[clss(["tool-call-name"])],
            [text(ToolCallSummary.display_name_for(tool_result.tool_call))],
          ),
          signifier_node,
          div(
            ~attrs=[clss(["tool-call-status-icon", status_class])],
            [status_icon],
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
