open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;

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

let line_num_view = (id: Id.t, measured: Haz3lcore.Measured.t): Node.t =>
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, _}) =>
    span(
      ~attrs=[clss(["error-line-num"])],
      [text("L" ++ string_of_int(origin.row + 1))],
    )
  | None => span([])
  };

let error_row =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~measured: Haz3lcore.Measured.t,
      ~cls: string,
      id: Id.t,
      content: Node.t,
    )
    : Node.t => {
  let is_active =
    Option.map(Id.equal(id), cursor_id) |> Option.value(~default=false);
  let classes = ["error-row", cls] @ (is_active ? ["active"] : []);
  div(
    ~attrs=[clss(classes), Attr.on_pointerdown(jump_to(~globals, id))],
    [line_num_view(id, measured), content],
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

type categorized_errors =
  list((SidebarModel.Settings.error_category, list((int, Node.t))));

let collect_errors =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~editor: CodeWithStatics.Model.t,
    )
    : categorized_errors => {
  let info_map = editor.statics.info_map;
  let error_ids = editor.statics.error_ids;
  let segment = editor.editor.syntax.segment;
  let measured = editor.editor.syntax.measured;
  /* Build position map for document-order sorting */
  let position_map = {
    let piece_ids =
      measured.piece_rows
      |> List.rev
      |> List.concat_map(row => List.rev_map(Haz3lcore.Piece.id, row));
    List.fold_left(
      (map, id) =>
        Id.Map.mem(id, map)
          ? map : Id.Map.add(id, Id.Map.cardinal(map), map),
      Id.Map.empty,
      piece_ids,
    );
  };
  let pos = id =>
    switch (Id.Map.find_opt(id, position_map)) {
    | Some(i) => i
    | None => max_int
    };
  let error_ids = List.sort((a, b) => compare(pos(a), pos(b)), error_ids);
  let mk_row = (~cls, id, content) => (
    pos(id),
    error_row(~globals, ~cursor_id, ~measured, ~cls, id, content),
  );
  /* Collect concave grout (missing operators) */
  let grout_rows =
    Haz3lcore.Segment.holes(segment)
    |> List.filter((g: Haz3lcore.Grout.t) => g.shape == Concave)
    |> List.map((g: Haz3lcore.Grout.t) =>
         mk_row(
           ~cls="error-syntax",
           g.id,
           span(
             ~attrs=[clss(["error-description"])],
             [text("Missing operator")],
           ),
         )
       );
  /* Collect incomplete tiles */
  let incomplete_rows =
    Haz3lcore.Segment.incomplete_tiles_deep(segment)
    |> List.map((t: Haz3lcore.Tile.t) => {
         let all_indices = List.init(List.length(t.label), Fun.id);
         let missing_labels =
           List.filter(i => !List.mem(i, t.shards), all_indices)
           |> List.map(i => List.nth(t.label, i));
         let description =
           "Incomplete: missing " ++ String.concat(", ", missing_labels);
         mk_row(
           ~cls="error-syntax",
           t.id,
           span(
             ~attrs=[clss(["error-description"])],
             [text(description)],
           ),
         );
       });
  /* Partition info_map errors into syntax and static */
  let (syntax_info_rows, static_rows) =
    List.fold_right(
      (id, (syn, stat)) =>
        switch (Language.Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Language.Info.is_error(ci) =>
          let cls =
            Language.Info.is_syntax_error(ci)
              ? "error-syntax" : "error-static";
          let row = mk_row(~cls, id, error_status_view(~globals, ci));
          if (Language.Info.is_syntax_error(ci)) {
            ([row, ...syn], stat);
          } else {
            (syn, [row, ...stat]);
          };
        | _ => (syn, stat)
        },
      error_ids,
      ([], []),
    );
  /* Collect warnings (respecting display_warnings setting) */
  let warning_ids =
    globals.settings.core.display_warnings ? editor.statics.warning_ids : [];
  let warning_ids =
    List.sort((a, b) => compare(pos(a), pos(b)), warning_ids);
  let warning_rows =
    List.filter_map(
      id =>
        switch (Language.Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Language.Info.is_warning(ci) =>
          Some(
            mk_row(
              ~cls="error-warning",
              id,
              error_status_view(~globals, ci),
            ),
          )
        | _ => None
        },
      warning_ids,
    );
  /* Combine and sort syntax rows */
  let sort_tagged = rows =>
    List.sort(((a, _), (b, _)) => compare(a, b), rows);
  [
    (
      SidebarModel.Settings.Syntax,
      grout_rows @ incomplete_rows @ syntax_info_rows |> sort_tagged,
    ),
    (Static, static_rows |> sort_tagged),
    (Warning, warning_rows |> sort_tagged),
  ];
};

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
  let categories = collect_errors(~globals, ~cursor_id, ~editor);
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
          let sort_tagged = rows =>
            List.sort(((a, _), (b, _)) => compare(a, b), rows);
          List.concat_map(snd, categories) |> sort_tagged |> List.map(snd);
        } else {
          List.filter_map(
            ((cat, tagged)) => {
              let rows = List.map(snd, tagged);
              rows != []
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
                      rows,
                    ),
                  )
                : None;
            },
            categories,
          );
        }
      );
    },
  );
};
