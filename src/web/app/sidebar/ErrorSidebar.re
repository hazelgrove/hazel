open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;
open ErrorCollection;

/* ---------- View helpers ---------- */

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

let error_status_view = (~globals, ci: Language.Info.t): Node.t =>
  switch (ci) {
  | InfoExp({cls, status, _} as ie) =>
    CursorInspector.exp_view(~globals, cls, status, ie)
  | InfoPat({cls, status, _} as ip) =>
    CursorInspector.pat_view(~globals, cls, status, ip)
  | InfoTyp({cls, status, _}) =>
    CursorInspector.typ_view(~globals, cls, status)
  | InfoTPat({cls, status, _}) =>
    CursorInspector.tpat_view(~globals, cls, status)
  | Secondary(_)
  | InfoMod(_)
  | InfoSig(_)
  | InfoMPat(_) => div([])
  };

let line_num_view =
    (id: Id.t, measured: Haz3lcore.Measured.t, row_to_line: int => int)
    : Node.t =>
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, _}) =>
    let line = row_to_line(origin.row);
    line > 0
      ? span(
          ~attrs=[clss(["error-line-num"])],
          [text("L" ++ string_of_int(line))],
        )
      : span([]);
  | None => span([])
  };

let error_row =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~expanded: list(Id.t),
      ~measured: Haz3lcore.Measured.t,
      ~row_to_line: int => int,
      ~cls: string,
      id: Id.t,
      content: Node.t,
    )
    : Node.t => {
  let is_active =
    Option.map(Id.equal(id), cursor_id) |> Option.value(~default=false);
  let is_expanded = is_active || List.mem(id, expanded);
  let classes =
    ["error-row", cls]
    @ (is_active ? ["active"] : [])
    @ (is_expanded ? ["expanded"] : []);
  let chevron =
    span(
      ~attrs=[
        clss(["error-row-chevron"]),
        Attr.on_pointerdown(evt => {
          Js_of_ocaml.Dom.preventDefault(evt);
          Js_of_ocaml.Dom_html.stopPropagation(evt);
          globals.inject_global(Set(Sidebar(Errors(ToggleExpanded(id)))));
        }),
      ],
      [text(is_expanded ? "▾" : "▸")],
    );
  div(
    ~attrs=[clss(classes), Attr.on_pointerdown(jump_to(~globals, id))],
    [chevron, line_num_view(id, measured, row_to_line), content],
  );
};

let legend_view =
    (categories: list((SidebarModel.Settings.error_category, list('a))))
    : Node.t => {
  let items =
    List.filter_map(
      ((cat, rows)) =>
        rows != []
          ? Some(
              span(
                ~attrs=[clss(["legend-item"])],
                [
                  span(
                    ~attrs=[
                      clss([
                        "legend-swatch",
                        SidebarModel.Settings.category_cls(cat),
                      ]),
                    ],
                    [],
                  ),
                  text(SidebarModel.Settings.category_short_label(cat)),
                ],
              ),
            )
          : None,
      categories,
    );
  div(~attrs=[clss(["error-legend"])], items);
};

let section_view =
    (~title: string, ~cls: string, ~collapsed: bool, ~on_toggle, items) =>
  div(
    ~attrs=[clss(["error-section", cls])],
    [
      div(
        ~attrs=[clss(["error-section-header"]), Attr.on_click(on_toggle)],
        [
          text(collapsed ? "▶ " : "▼ "),
          text(title ++ " (" ++ string_of_int(List.length(items)) ++ ")"),
        ],
      ),
    ]
    @ (collapsed ? [] : items),
  );

/* ---------- Problem → row rendering ---------- */

let problem_row =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~expanded: list(Id.t),
      ~measured: Haz3lcore.Measured.t,
      ~row_to_line: int => int,
      problem: problem,
    )
    : Node.t => {
  let cls = SidebarModel.Settings.category_row_cls(problem.category);
  let content =
    switch (problem.source) {
    | Structural(desc) =>
      span(~attrs=[clss(["error-description"])], [text(desc)])
    | FromInfo(ci) => error_status_view(~globals, ci)
    };
  error_row(
    ~globals,
    ~cursor_id,
    ~expanded,
    ~measured,
    ~row_to_line,
    ~cls,
    problem.id,
    content,
  );
};

/* ---------- Main view ---------- */

let view =
    (
      ~globals: Globals.t,
      ~cursor: Cursor.cursor(Editors.Update.t),
      ~editor: CodeWithStatics.Model.t,
    )
    : Node.t => {
  let cursor_id =
    switch (cursor.info) {
    | Some(ci) => Some(Language.Info.id_of(ci))
    | None => None
    };
  let ctx = make_error_context(~settings=globals.settings, ~editor);
  let categories:
    list((SidebarModel.Settings.error_category, list(problem))) =
    List.map(
      cat => (cat, collect_category(ctx, cat) |> List.of_seq),
      [Syntax, Hole, Static, Warning],
    );
  let errors_settings = globals.settings.sidebar.errors;
  let has_any_errors = List.exists(((_, rows)) => rows != [], categories);
  let toggle_view =
    div(
      ~attrs=[clss(["error-view-toggle"])],
      [
        span(
          ~attrs=[
            clss(
              ["toggle-option"] @ (errors_settings.flat ? [] : ["active"]),
            ),
            Attr.on_click(_ =>
              if (errors_settings.flat) {
                globals.inject_global(Set(Sidebar(Errors(ToggleFlat))));
              } else {
                Virtual_dom.Vdom.Effect.Ignore;
              }
            ),
          ],
          [text("Grouped")],
        ),
        span(
          ~attrs=[
            clss(
              ["toggle-option"] @ (errors_settings.flat ? ["active"] : []),
            ),
            Attr.on_click(_ =>
              if (!errors_settings.flat) {
                globals.inject_global(Set(Sidebar(Errors(ToggleFlat))));
              } else {
                Virtual_dom.Vdom.Effect.Ignore;
              }
            ),
          ],
          [text("Flat")],
        ),
      ],
    );
  let render_row = problem =>
    problem_row(
      ~globals,
      ~cursor_id,
      ~expanded=errors_settings.expanded,
      ~measured=ctx.measured,
      ~row_to_line=ctx.row_to_line,
      problem,
    );
  div(
    ~attrs=[clss(["errors-panel"])],
    if (!has_any_errors) {
      [
        div(
          ~attrs=[clss(["no-errors-message"])],
          [text("No errors or warnings")],
        ),
      ];
    } else {
      [legend_view(categories), toggle_view]
      @ (
        if (errors_settings.flat) {
          let all_problems =
            List.concat_map(snd, categories)
            |> List.sort((a, b) => compare(ctx.pos(a.id), ctx.pos(b.id)));
          List.map(render_row, all_problems);
        } else {
          List.filter_map(
            ((cat, problems)) =>
              problems != []
                ? Some(
                    section_view(
                      ~title=SidebarModel.Settings.category_label(cat),
                      ~cls=SidebarModel.Settings.category_section_cls(cat),
                      ~collapsed=
                        SidebarModel.Settings.is_collapsed(
                          cat,
                          errors_settings,
                        ),
                      ~on_toggle=
                        _ =>
                          globals.inject_global(
                            Set(Sidebar(Errors(ToggleCollapsed(cat)))),
                          ),
                      List.map(render_row, problems),
                    ),
                  )
                : None,
            categories,
          );
        }
      );
    },
  );
};
