open Virtual_dom.Vdom;
open Node;
open Util;
open Util_web.WebUtil;
open Haz3lcore.ProblemCollection;

/* ---------- Scroll-into-view hook ---------- */

module ScrollIntoViewHook =
  Attr.Hooks.Make({
    module State = Unit;
    module Input = {
      type t = unit;
      let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_unit;
      let combine = ((), ()) => ();
    };
    let init = ((), _el) => ();
    let on_mount = ((), (), el) =>
      Js_of_ocaml.Js.Unsafe.coerce(el)##scrollIntoView(
        Js_of_ocaml.Js.Unsafe.obj([|
          (
            "block",
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("nearest")),
          ),
          (
            "inline",
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("nearest")),
          ),
        |]),
      );
    let update = (~old_input as (), ~new_input as (), (), _el) => ();
    let destroy = ((), (), _el) => ();
  });

let scroll_active_into_view: Attr.t =
  Attr.create_hook("scroll-active-problem", ScrollIntoViewHook.create());

/* ---------- View helpers ---------- */

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(JumpToTile(id));

let problem_status_view = (~globals, ci: Language.Info.t): Node.t =>
  switch (ci) {
  | InfoExp({cls, message, _} as ie) =>
    CursorInspector.exp_view(
      ~globals,
      ~show_type_colon=false,
      cls,
      message,
      ie,
    )
  | InfoPat({cls, message, _} as ip) =>
    CursorInspector.pat_view(
      ~globals,
      ~show_type_colon=false,
      cls,
      message,
      ip,
    )
  | InfoTyp({cls, marks, message, _}) =>
    CursorInspector.typ_view(~globals, cls, ~marks, ~message)
  | InfoTPat({cls, marks, message, _}) =>
    CursorInspector.tpat_view(~globals, cls, ~marks, ~message)
  /* InfoDrv has no marks and routes errors through its own renderer, so
     defer entirely to the cursor inspector's drv_view rather than building
     a generic problem-row from cls/marks/message like the cases above. */
  | InfoDrv(ci) => DrvCursorInspector.drv_view(~globals, ci)
  | InfoMod({cls, _})
  | InfoSig({cls, _})
  | InfoMPat({cls, _}) =>
    span(
      ~attrs=[clss(["problem-description"])],
      [text(Language.Cls.show(cls))],
    )
  | Secondary(_) => div([])
  };

let line_num_view =
    (
      id: Id.t,
      measured: Haz3lcore.Measured.t,
      row_to_line: int => option(int),
      nearest_measured_id: Id.t => option(Id.t),
    )
    : Node.t => {
  /* Common case: id is in measured directly; only walk ancestors on miss
     (e.g. ids inside a fold projector). */
  let resolved =
    switch (Haz3lcore.Measured.find_by_id(id, measured)) {
    | Some(_) as m => m
    | None =>
      Option.bind(nearest_measured_id(id), anc =>
        Haz3lcore.Measured.find_by_id(anc, measured)
      )
    };
  switch (resolved) {
  | Some({origin, _}) =>
    switch (row_to_line(origin.row)) {
    | Some(line) =>
      span(
        ~attrs=[clss(["problem-line-num"])],
        [text("L" ++ string_of_int(line))],
      )
    | None =>
      span(~attrs=[clss(["problem-line-num", "no-line"])], [text("L?")])
    }
  | None =>
    span(~attrs=[clss(["problem-line-num", "no-line"])], [text("L?")])
  };
};

let row_view =
    (
      ~globals: Globals.t,
      ~cursor_id: option(Id.t),
      ~expanded: list(Id.t),
      ~measured: Haz3lcore.Measured.t,
      ~row_to_line: int => option(int),
      ~nearest_measured_id: Id.t => option(Id.t),
      ~show_line_numbers: bool,
      ~cls: string,
      id: Id.t,
      content: Node.t,
    )
    : Node.t => {
  let is_active =
    Option.map(Id.equal(id), cursor_id) |> Option.value(~default=false);
  let is_expanded = is_active || List.mem(id, expanded);
  let classes =
    ["problem-row", cls]
    @ (is_active ? ["active"] : [])
    @ (is_expanded ? ["expanded"] : []);
  let chevron =
    span(
      ~attrs=[
        clss(["problem-row-chevron"]),
        Attr.on_pointerdown(evt => {
          Js_of_ocaml.Dom.preventDefault(evt);
          Js_of_ocaml.Dom_html.stopPropagation(evt);
          globals.inject_global(
            Set(Sidebar(Problems(ToggleExpanded(id)))),
          );
        }),
      ],
      [text(is_expanded ? "▾" : "▸")],
    );
  let scroll_attr = is_active ? scroll_active_into_view : Attr.empty;
  let jump_id = Option.value(nearest_measured_id(id), ~default=id);
  let line_num =
    show_line_numbers
      ? [line_num_view(id, measured, row_to_line, nearest_measured_id)] : [];
  div(
    ~attrs=[
      clss(classes),
      Attr.on_pointerdown(jump_to(~globals, jump_id)),
      scroll_attr,
    ],
    [chevron] @ line_num @ [content],
  );
};

let legend_view =
    (counts: list((SidebarModel.Settings.problem_category, int))): Node.t => {
  let items =
    List.filter_map(
      ((cat, n)) =>
        n > 0
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
      counts,
    );
  div(~attrs=[clss(["problem-legend"])], items);
};

let section_view =
    (~title: string, ~cls: string, ~collapsed: bool, ~on_toggle, items) =>
  div(
    ~attrs=[clss(["problem-section", cls])],
    [
      div(
        ~attrs=[clss(["problem-section-header"]), Attr.on_click(on_toggle)],
        [
          text(collapsed ? "▸ " : "▾ "),
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
      ~show_line_numbers: bool,
      lp: located_problem,
    )
    : Node.t => {
  let cls = SidebarModel.Settings.category_row_cls(lp.problem.category);
  let content =
    switch (lp.problem.source) {
    | Structural(desc) =>
      span(~attrs=[clss(["problem-description"])], [text(desc)])
    | FromInfo(ci) => problem_status_view(~globals, ci)
    | FromProjector(_, {message}) =>
      span(~attrs=[clss(["problem-description"])], [text(message)])
    };
  row_view(
    ~globals,
    ~cursor_id,
    ~expanded,
    ~measured=lp.measured,
    ~row_to_line=lp.row_to_line,
    ~nearest_measured_id=lp.nearest_measured_id,
    ~show_line_numbers,
    ~cls,
    lp.problem.id,
    content,
  );
};

/* ---------- Navigation buttons ---------- */

let nav_btn =
    (
      ~globals: Globals.t,
      ~direction: Util.Direction.t,
      ~label: string,
      ~tooltip: string,
    )
    : Node.t =>
  div(
    ~attrs=[
      clss(["problem-nav-btn"]),
      Attr.on_pointerdown(evt => {
        Js_of_ocaml.Dom.preventDefault(evt);
        globals.inject_global(
          ActiveEditor(Move(Goal(NextProblem(direction)))),
        );
      }),
      Attr.title(tooltip),
    ],
    [text(label)],
  );

/* ---------- Main view ---------- */

let view =
    (
      ~globals: Globals.t,
      ~cursor: Cursor.cursor(Editors.Update.t),
      ~collection: Haz3lcore.ProblemCollection.problem_collection,
    )
    : Node.t => {
  let cursor_id =
    switch (cursor.info) {
    | Some(ci) => Some(Language.Info.id_of(ci))
    | None => None
    };
  let problems_settings = globals.settings.sidebar.problems;
  let group_has_problems = (g: problem_group) =>
    List.exists(((_, ps)) => !List.is_empty(ps), g.problems_by_category);
  let has_any_problems = List.exists(group_has_problems, collection.groups);
  let toggle_view =
    div(
      ~attrs=[clss(["problem-view-toggle"])],
      [
        span(
          ~attrs=[
            clss(
              ["toggle-option"] @ (problems_settings.flat ? [] : ["active"]),
            ),
            Attr.on_click(_ =>
              if (problems_settings.flat) {
                globals.inject_global(Set(Sidebar(Problems(ToggleFlat))));
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
              ["toggle-option"] @ (problems_settings.flat ? ["active"] : []),
            ),
            Attr.on_click(_ =>
              if (!problems_settings.flat) {
                globals.inject_global(Set(Sidebar(Problems(ToggleFlat))));
              } else {
                Virtual_dom.Vdom.Effect.Ignore;
              }
            ),
          ],
          [text("Flat")],
        ),
        div(
          ~attrs=[clss(["problem-nav-buttons"])],
          [
            nav_btn(
              ~globals,
              ~direction=Left,
              ~label={|↑|},
              ~tooltip="Previous Problem (Shift+Tab)",
            ),
            nav_btn(
              ~globals,
              ~direction=Right,
              ~label={|↓|},
              ~tooltip="Next Problem (Tab)",
            ),
          ],
        ),
      ],
    );
  /* Renders the body of one problem_group. In flat mode: all categories
     concatenated and re-sorted by position. In grouped mode: one
     collapsible per-category subsection. L# labels are shown only when
     the group came from a single source editor — otherwise "L1" would
     mean different things across the constituent editors. */
  let render_group = (g: problem_group): list(Node.t) => {
    let show_line_numbers = g.single_source;
    let render = lp =>
      problem_row(
        ~globals,
        ~cursor_id,
        ~expanded=problems_settings.expanded,
        ~show_line_numbers,
        lp,
      );
    if (problems_settings.flat) {
      g.problems_by_category
      |> List.concat_map(snd)
      |> List.sort((a: located_problem, b) => compare(a.pos, b.pos))
      |> List.map(render);
    } else {
      let label = Option.value(g.label, ~default="");
      List.filter_map(
        ((cat, lps)) =>
          if (lps == []) {
            None;
          } else {
            Some(
              section_view(
                ~title=SidebarModel.Settings.category_label(cat),
                ~cls=SidebarModel.Settings.category_section_cls(cat),
                ~collapsed=
                  SidebarModel.Settings.is_collapsed(
                    label,
                    cat,
                    problems_settings,
                  ),
                ~on_toggle=
                  _ =>
                    globals.inject_global(
                      Set(Sidebar(Problems(ToggleCollapsed(label, cat)))),
                    ),
                List.map(render, lps),
              ),
            );
          },
        g.problems_by_category,
      );
    };
  };
  let non_empty_groups = List.filter(group_has_problems, collection.groups);
  let group_sections =
    switch (non_empty_groups) {
    | [] => []
    | [g] =>
      /* Single group: no header, just render its body directly. */
      render_group(g)
    | gs =>
      /* Multiple groups: each is a collapsible labelled section with a
         count. */
      List.map(
        (g: problem_group) => {
          let label = Option.value(g.label, ~default="");
          let total =
            g.counts |> List.to_seq |> Seq.map(snd) |> Seq.fold_left((+), 0);
          let collapsed =
            SidebarModel.Settings.is_editor_collapsed(
              label,
              problems_settings,
            );
          div(
            ~attrs=[clss(["problem-editor-group"])],
            [
              div(
                ~attrs=[
                  clss(["problem-editor-header"]),
                  Attr.on_click(_ =>
                    globals.inject_global(
                      Set(Sidebar(Problems(ToggleEditorCollapsed(label)))),
                    )
                  ),
                ],
                [
                  text(collapsed ? "▸ " : "▾ "),
                  text(label ++ " (" ++ string_of_int(total) ++ ")"),
                ],
              ),
            ]
            @ (collapsed ? [] : render_group(g)),
          );
        },
        gs,
      )
    };
  div(
    ~attrs=[clss(["problems-panel"])],
    if (!has_any_problems) {
      [
        div(~attrs=[clss(["no-problems-message"])], [text("No problems")]),
      ];
    } else {
      [legend_view(collection.counts), toggle_view] @ group_sections;
    },
  );
};
