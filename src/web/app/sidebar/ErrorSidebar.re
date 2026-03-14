open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;

/* ---------- Problem data types ---------- */

type problem_source =
  | Structural(string)
  | FromInfo(Language.Info.t);

type problem = {
  id: Id.t,
  category: SidebarModel.Settings.error_category,
  source: problem_source,
};

/* ---------- Error context ---------- */

type error_context = {
  info_map: Language.Statics.Map.t,
  syntax_error_ids: list((Id.t, Language.Info.t)),
  static_error_ids: list((Id.t, Language.Info.t)),
  warning_ids: list((Id.t, Language.Info.t)),
  segment: Haz3lcore.Segment.t,
  measured: Haz3lcore.Measured.t,
  pos: Id.t => int,
};

let make_error_context =
    (~settings: Settings.t, ~editor: CodeWithStatics.Model.t): error_context => {
  let measured = editor.editor.syntax.measured;
  let info_map = editor.statics.info_map;
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
  /* Partition error_ids into syntax and static in a single pass */
  let (syntax_error_ids, static_error_ids) =
    List.fold_right(
      (id, (syn, stat)) =>
        switch (Language.Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Language.Info.is_error(ci) =>
          if (Language.Info.is_syntax_error(ci)) {
            ([(id, ci), ...syn], stat);
          } else {
            (syn, [(id, ci), ...stat]);
          }
        | _ => (syn, stat)
        },
      editor.statics.error_ids,
      ([], []),
    );
  /* Collect warning ids with their info */
  let warning_ids =
    if (settings.core.display_warnings) {
      List.filter_map(
        id =>
          switch (Language.Statics.Map.lookup(id, info_map)) {
          | Some(ci) when Language.Info.is_warning(ci) => Some((id, ci))
          | _ => None
          },
        editor.statics.warning_ids,
      );
    } else {
      [];
    };
  {
    info_map,
    syntax_error_ids,
    static_error_ids,
    warning_ids,
    segment: editor.editor.syntax.segment,
    measured,
    pos,
  };
};

/* ---------- Per-category collection (lazy) ---------- */

let collect_category =
    (ctx: error_context, cat: SidebarModel.Settings.error_category)
    : Seq.t(problem) => {
  let sort_by_pos = problems =>
    List.sort((a, b) => compare(ctx.pos(a.id), ctx.pos(b.id)), problems);
  switch (cat) {
  | Syntax =>
    let grout_problems =
      Haz3lcore.Segment.holes(ctx.segment)
      |> List.filter((g: Haz3lcore.Grout.t) => g.shape == Concave)
      |> List.map((g: Haz3lcore.Grout.t) =>
           {
             id: g.id,
             category: Syntax,
             source: Structural("Missing operator"),
           }
         );
    let incomplete_problems =
      Haz3lcore.Segment.incomplete_tiles_deep(ctx.segment)
      |> List.map((t: Haz3lcore.Tile.t) => {
           let all_indices = List.init(List.length(t.label), Fun.id);
           let missing_labels =
             List.filter(i => !List.mem(i, t.shards), all_indices)
             |> List.map(i => List.nth(t.label, i));
           let description =
             "Incomplete: missing " ++ String.concat(", ", missing_labels);
           {
             id: t.id,
             category: Syntax,
             source: Structural(description),
           };
         });
    let syntax_info_problems =
      ctx.syntax_error_ids
      |> List.map(((id, ci)) => {
           {
             id,
             category: Syntax,
             source: FromInfo(ci),
           }
         });
    grout_problems
    @ incomplete_problems
    @ syntax_info_problems
    |> sort_by_pos
    |> List.to_seq;
  | Static =>
    ctx.static_error_ids
    |> List.map(((id, ci)) => {
         {
           id,
           category: Static,
           source: FromInfo(ci),
         }
       })
    |> sort_by_pos
    |> List.to_seq
  | Warning =>
    ctx.warning_ids
    |> List.map(((id, ci)) => {
         {
           id,
           category: Warning,
           source: FromInfo(ci),
         }
       })
    |> sort_by_pos
    |> List.to_seq
  };
};

/* ---------- View helpers (unchanged) ---------- */

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

/* ---------- Problem → row rendering ---------- */

let problem_row =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~measured: Haz3lcore.Measured.t,
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
  error_row(~globals, ~cursor_id, ~measured, ~cls, problem.id, content);
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
      [Syntax, Static, Warning],
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
    problem_row(~globals, ~cursor_id, ~measured=ctx.measured, problem);
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
