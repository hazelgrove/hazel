open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

let error_status_view = (~globals, ci: Language.Info.t): Node.t =>
  div(CursorInspector.view_of_info(~globals, ci));

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
      ~error_cls: string,
      id: Id.t,
      ci: Language.Info.t,
    )
    : Node.t => {
  let is_active =
    switch (cursor_id) {
    | Some(cid) => Id.equal(cid, id)
    | None => false
    };
  let classes = ["error-row", error_cls] @ (is_active ? ["active"] : []);
  div(
    ~attrs=[clss(classes), Attr.on_pointerdown(jump_to(~globals, id))],
    [line_num_view(id, measured), error_status_view(~globals, ci)],
  );
};

let syntax_error_row =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~measured: Haz3lcore.Measured.t,
      id: Id.t,
      description: string,
    )
    : Node.t => {
  let is_active =
    switch (cursor_id) {
    | Some(cid) => Id.equal(cid, id)
    | None => false
    };
  let classes =
    ["error-row", "error-syntax"] @ (is_active ? ["active"] : []);
  div(
    ~attrs=[clss(classes), Attr.on_pointerdown(jump_to(~globals, id))],
    [
      line_num_view(id, measured),
      span(~attrs=[clss(["error-description"])], [text(description)]),
    ],
  );
};

let is_syntax_error = (ci: Language.Info.t): bool =>
  switch (ci) {
  | InfoExp({status: InHole(Common(NoType(BadToken(_)))), _}) => true
  | InfoPat({status: InHole(Common(NoType(BadToken(_)))), _}) => true
  | InfoTyp({status: InHole(BadToken(_)), _}) => true
  | InfoTyp({status: InHole(ParseFailure), _}) => true
  | _ => false
  };

let legend_view: Node.t =
  div(
    ~attrs=[clss(["error-legend"])],
    [
      span(
        ~attrs=[clss(["legend-item"])],
        [
          span(~attrs=[clss(["legend-swatch", "syntax"])], []),
          text("Syntax"),
        ],
      ),
      span(
        ~attrs=[clss(["legend-item"])],
        [
          span(~attrs=[clss(["legend-swatch", "static"])], []),
          text("Static"),
        ],
      ),
    ],
  );

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

let view =
    (
      ~globals: Globals.t,
      ~cursor: Cursor.cursor(Editors.Update.t),
      ~editor: CodeWithStatics.Model.t,
    )
    : Node.t => {
  let info_map = editor.statics.info_map;
  let error_ids = editor.statics.error_ids;
  let segment = editor.editor.syntax.segment;
  let measured = editor.editor.syntax.measured;
  /* Sort error_ids by document order using piece_rows from measured layout */
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
  let cursor_id =
    switch (cursor.info) {
    | Some(ci) => Some(Language.Info.id_of(ci))
    | None => None
    };
  /* Collect concave grout (missing operators) */
  let grout_rows =
    Haz3lcore.Segment.holes(segment)
    |> List.filter((g: Haz3lcore.Grout.t) => g.shape == Concave)
    |> List.map((g: Haz3lcore.Grout.t) =>
         (
           pos(g.id),
           syntax_error_row(
             ~globals,
             ~cursor_id,
             ~measured,
             g.id,
             "Missing operator",
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
         (
           pos(t.id),
           syntax_error_row(
             ~globals,
             ~cursor_id,
             ~measured,
             t.id,
             description,
           ),
         );
       });
  /* Partition info_map errors into syntax and static */
  let (syntax_info_rows, static_rows) =
    List.fold_right(
      (id, (syn, stat)) =>
        switch (Language.Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Language.Info.is_error(ci) =>
          if (is_syntax_error(ci)) {
            (
              [
                (
                  pos(id),
                  error_row(
                    ~globals,
                    ~cursor_id,
                    ~measured,
                    ~error_cls="error-syntax",
                    id,
                    ci,
                  ),
                ),
                ...syn,
              ],
              stat,
            );
          } else {
            (
              syn,
              [
                (
                  pos(id),
                  error_row(
                    ~globals,
                    ~cursor_id,
                    ~measured,
                    ~error_cls="error-static",
                    id,
                    ci,
                  ),
                ),
                ...stat,
              ],
            );
          }
        | _ => (syn, stat)
        },
      error_ids,
      ([], []),
    );
  /* Combine and sort syntax rows */
  let sort_tagged = rows =>
    List.sort(((a, _), (b, _)) => compare(a, b), rows);
  let syntax_tagged =
    grout_rows @ incomplete_rows @ syntax_info_rows |> sort_tagged;
  let static_tagged = static_rows |> sort_tagged;
  let syntax_rows = List.map(snd, syntax_tagged);
  let static_rows = List.map(snd, static_tagged);
  let errors_flat = globals.settings.sidebar.errors_flat;
  let toggle_view =
    div(
      ~attrs=[clss(["error-view-toggle"])],
      [
        span(
          ~attrs=[
            clss(["toggle-option"] @ (errors_flat ? [] : ["active"])),
            Attr.on_click(_ =>
              if (errors_flat) {
                globals.inject_global(Set(Sidebar(ToggleErrorsFlat)));
              } else {
                Virtual_dom.Vdom.Effect.Ignore;
              }
            ),
          ],
          [text("Grouped")],
        ),
        span(
          ~attrs=[
            clss(["toggle-option"] @ (errors_flat ? ["active"] : [])),
            Attr.on_click(_ =>
              if (!errors_flat) {
                globals.inject_global(Set(Sidebar(ToggleErrorsFlat)));
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
    switch (syntax_rows, static_rows) {
    | ([], []) => [
        div(~attrs=[clss(["no-errors-message"])], [text("No errors")]),
      ]
    | _ =>
      [legend_view, toggle_view]
      @ (
        if (errors_flat) {
          syntax_tagged @ static_tagged |> sort_tagged |> List.map(snd);
        } else {
          (
            syntax_rows != []
              ? [
                section_view(
                  ~title="Syntax Errors",
                  ~cls="syntax-errors",
                  ~collapsed=globals.settings.sidebar.syntax_collapsed,
                  ~on_toggle=
                    _ =>
                      globals.inject_global(
                        Set(Sidebar(ToggleSyntaxCollapsed)),
                      ),
                  syntax_rows,
                ),
              ]
              : []
          )
          @ (
            static_rows != []
              ? [
                section_view(
                  ~title="Static Errors",
                  ~cls="static-errors",
                  ~collapsed=globals.settings.sidebar.static_collapsed,
                  ~on_toggle=
                    _ =>
                      globals.inject_global(
                        Set(Sidebar(ToggleStaticCollapsed)),
                      ),
                  static_rows,
                ),
              ]
              : []
          );
        }
      )
    },
  );
};
