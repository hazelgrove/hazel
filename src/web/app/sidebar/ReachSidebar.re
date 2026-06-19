open Virtual_dom.Vdom;
open Node;
open WebUtil;
open Haz3lcore;

/* Reach sidebar — a breakpoint-manager-style control panel for reach points.
 *
 * It lists every Reach refractor in the active editor, lets you switch between
 * a group view (default) and an order view, see each point's individual path
 * condition and each group's merged (total) condition, solve points/groups on
 * demand, enable/disable points like breakpoints, reassign a point's group via
 * a dropdown, and name groups.
 *
 * All the analysis is reused: per-point/merged conditions come from
 * RefractorView.mk_data / Reach.analyze, solving goes through
 * Reach.smtlib2 → Z3Wasm.solve → Reach.interpret (exactly as the offside
 * ReachProjView does), and model changes are dispatched as Project(SetModel(…))
 * — the same path the projector's own `local` uses. */

/* One reach point, resolved for display + dispatch. */
type point = {
  id: Id.t,
  /* index in the refractor list (= projector_list order), for SetModel */
  idx: int,
  /* serialized model decoded to {group, enabled, result} */
  model: ReachProj.t,
  /* carries the group-resolved reach condition + utility, and is the `info`
     the cooked update expects (ReachProj.update ignores it) */
  info: ProjectorBase.info,
  /* this node's own (solo) path condition, independent of grouping */
  individual: option(Reach.t),
  /* group-resolved condition (merged for grouped points) == info.reach */
  reach: option(Reach.t),
  /* 1-based line number, when measured */
  row: option(int),
};

/* Gather all reach points from the editor, mirroring how CodeEditable builds
   refractor data for rendering. */
let collect = (~editor: CodeEditable.Model.t): list(point) => {
  let z = editor.editor.state.zipper;
  let statics = editor.statics.info_map;
  let measured = editor.editor.syntax.measured;
  /* The flat refractor list, in the same order used as projector_list for
     SetModel indexing (CodeEditable.re). */
  let refractor_list =
    List.map(fst, z.refractors.manuals)
    @ List.map(fst, Id.Map.to_list(z.refractors.multis.ephemerals));
  let refractors_map =
    Id.Map.union(
      (_, _, b) => Some(b),
      z.refractors.manuals |> Id.Map.of_list,
      z.refractors.multis.ephemerals,
    );
  let data =
    RefractorView.mk_data(
      ~refractors=refractors_map,
      ~syntax=editor.editor.syntax,
      ~indicated=None,
      ~statics,
      ~dynamics=editor.dynamics,
      ~sample_focus=z.refractors.sample_focus,
      ~editor_active=false,
    );
  List.filter_map(
    (d: ProjectorView.Model.projector_data) =>
      switch (d.p.kind) {
      | Reach =>
        switch (
          List.find_index(rid => Id.equal(rid, d.p.id), refractor_list)
        ) {
        | None => None
        | Some(idx) =>
          let model = ReachProj.t_of_sexp(Sexplib.Sexp.of_string(d.p.model));
          let row =
            switch (Measured.find_by_id(d.p.id, measured)) {
            | Some(m) => Some(m.origin.row + 1)
            | None => None
            };
          Some({
            id: d.p.id,
            idx,
            model,
            info: d.info,
            individual: Reach.analyze(d.p.id, statics),
            reach: d.info.reach,
            row,
          });
        }
      | _ => None
      },
    data,
  );
};

/* Group points by their (non-zero) merge group, sorted by group id, members
   sorted by source position. */
let group_points = (points: list(point)): list((int, list(point))) => {
  let cmp_pos = (a: point, b: point) =>
    compare((a.row, a.idx), (b.row, b.idx));
  let grouped = List.filter(p => p.model.group != 0, points);
  List.fold_left(
    (acc, p: point) => {
      let g = p.model.group;
      switch (List.assoc_opt(g, acc)) {
      | Some(ms) => [(g, [p, ...ms]), ...List.remove_assoc(g, acc)]
      | None => [(g, [p]), ...acc]
      };
    },
    [],
    grouped,
  )
  |> List.map(((g, ms)) => (g, List.sort(cmp_pos, ms)))
  |> List.sort(((g1, _), (g2, _)) => compare(g1, g2));
};

let stop = evt => Js_of_ocaml.Dom_html.stopPropagation(evt);

/* Render a reach condition as Hazel text (reusing the offside's renderer),
   colored by its merge group (neutral if solo), with a trivial-condition
   placeholder and an "approximate" marker when the translation dropped an
   unsupported construct. */
let constraint_view = (~group: int, r: Reach.t): Node.t => {
  let nodes = ReachProjView.path_view(ProjectorInfo.utility, r);
  let body =
    switch (nodes) {
    | [] => [
        span(
          ~attrs=[clss(["reach-constraint-trivial"])],
          [text({js|⊤ always reached|js})],
        ),
      ]
    | ns => ns
    };
  let approx =
    r.complete
      ? []
      : [
        span(
          ~attrs=[
            clss(["reach-approx"]),
            Attr.title(
              "Uses an unsupported construct — a 'reachable' result is reported as unknown; 'unreachable' is still sound.",
            ),
          ],
          [text({js| ≈|js})],
        ),
      ];
  div(
    ~attrs=[
      clss(["reach-constraint"]),
      ...ReachProjView.group_text_attrs(group),
    ],
    body @ approx,
  );
};

let view =
    (
      ~globals: Globals.t,
      ~cursor as _: Cursor.cursor(Editors.Update.t),
      ~editor: CodeEditable.Model.t,
    )
    : Node.t => {
  let settings = globals.settings.sidebar.reach;
  let points = collect(~editor);
  let solo = List.filter((p: point) => p.model.group == 0, points);
  let groups = group_points(points);
  let groups_in_use = List.map(fst, groups);
  let fresh_group =
    groups_in_use == [] ? 1 : List.fold_left(max, 0, groups_in_use) + 1;

  /* Dispatch a model change to a specific reach refractor, exactly as the
     projector's own `local` does: update its model and re-serialize, then
     Project(SetModel(idx, Reach, …)). */
  let set_model = (p: point, action: ReachProj.reach_action) => {
    let new_model =
      ReachProj.M.update(p.model, p.info, action)
      |> ReachProj.M.sexp_of_model
      |> Sexplib.Sexp.to_string;
    globals.inject_global(
      ActiveEditor(Project(SetModel(p.idx, Reach, new_model))),
    );
  };

  /* Solve a condition asynchronously and feed the interpreted outcome to
     dispatch_result (which builds the effect that stores it). */
  let solve = (r: Reach.t, dispatch_result) =>
    Z3Wasm.solve(
      ~k=
        outcome =>
          Bonsai.Effect.Expert.handle(
            dispatch_result(
              Reach.interpret(
                ~complete=r.complete,
                ~inputs=r.inputs,
                outcome,
              ),
            ),
          ),
      Reach.smtlib2(r) |> fst,
    );

  /* Solve one point (its solo or group-resolved condition) and store the
     result on that point. Skips disabled points. */
  let solve_point = (p: point) =>
    if (p.model.enabled) {
      switch (p.reach) {
      | Some(r) => solve(r, o => set_model(p, SetResult(o)))
      | None => ()
      };
    };

  /* Solve a whole group once (all members share the merged condition) and store
     the verdict on every enabled member, so the group header and all members
     agree — consistent with the offside. */
  let solve_group = (members: list(point)) => {
    let enabled = List.filter((m: point) => m.model.enabled, members);
    switch (enabled) {
    | [] => ()
    | [rep, ..._] =>
      switch (rep.reach) {
      | Some(r) =>
        solve(r, o =>
          Effect.Many(List.map(m => set_model(m, SetResult(o)), enabled))
        )
      | None => ()
      }
    };
  };

  let solve_all = () => {
    List.iter(solve_point, solo);
    List.iter(((_, members)) => solve_group(members), groups);
  };

  /* A small clickable control that stops the row's jump-on-click and runs the
     effect returned by on_activate. Returning the effect (rather than building
     and discarding it) is what actually dispatches it. */
  let control = (~cls, ~tooltip, ~on_activate, label) =>
    span(
      ~attrs=[
        clss(cls),
        Attr.title(tooltip),
        Attr.on_pointerdown(evt => {
          Js_of_ocaml.Dom.preventDefault(evt);
          stop(evt);
          on_activate();
        }),
      ],
      [text(label)],
    );

  let enable_toggle = (p: point) =>
    control(
      ~cls=["reach-enable", p.model.enabled ? "on" : "off"],
      ~tooltip=
        p.model.enabled
          ? "Enabled — click to disable (excludes it from solving and group merges)"
          : "Disabled — click to enable",
      ~on_activate=() => set_model(p, SetEnabled(!p.model.enabled)),
      p.model.enabled ? {js|●|js} : {js|○|js},
    );

  /* Solving is fire-and-forget (it schedules its own dispatch in the async
     solver callback), so the button itself returns no effect. */
  let solve_btn = (~tooltip="Find reaching inputs", do_solve) =>
    control(
      ~cls=["reach-generate"],
      ~tooltip,
      ~on_activate=
        () => {
          do_solve();
          Effect.Ignore;
        },
      {js|🎯|js},
    );

  let line_num = (p: point) =>
    span(
      ~attrs=[clss(["reach-line"])],
      [
        text(
          switch (p.row) {
          | Some(n) => "L" ++ string_of_int(n)
          | None => "L?"
          },
        ),
      ],
    );

  /* The same chip the offside uses: a colored circle (ReachProjView.group_color)
     with the group number (• = solo), so a group reads identically in both
     views. */
  let group_chip = (g: int) =>
    span(
      ~attrs=[
        clss(["reach-group-chip"]),
        Attr.create(
          "style",
          "background-color: " ++ ReachProjView.group_color(g),
        ),
        Attr.title(g == 0 ? "Solo" : "Group " ++ string_of_int(g)),
      ],
      [text(g == 0 ? {js|•|js} : string_of_int(g))],
    );

  let group_dropdown = (p: point) => {
    let opt = (g: int, label: string) =>
      Node.option(
        ~attrs=
          [Attr.value(string_of_int(g))]
          @ (g == p.model.group ? [Attr.create("selected", "selected")] : []),
        [text(label)],
      );
    Node.select(
      ~attrs=[
        clss(["reach-group-select"]),
        Attr.on_pointerdown(evt => {
          stop(evt);
          Effect.Ignore;
        }),
        Attr.on_change((_, v) => set_model(p, SetGroup(int_of_string(v)))),
      ],
      [opt(0, {js|• Solo|js})]
      @ List.map(
          g => opt(g, SidebarModel.Settings.reach_group_name(g, settings)),
          groups_in_use,
        )
      @ [opt(fresh_group, "+ New group")],
    );
  };

  /* The solved result, listed one variable per line (the sidebar has vertical
     room and scrolls), on its own full-width line. */
  let result_block = (~group: int, result: option(TestGen.outcome)) =>
    switch (result) {
    | Some(o) => [ReachProjView.result_view(~group, ~multiline=true, o)]
    | None => []
    };

  /* One point row. In group view, members defer their result to the group's
     own result line (show_result=false); solo points and the order view show
     their result below the controls. */
  let point_row = (~show_result: bool, p: point) => {
    let condition =
      switch (p.individual) {
      | Some(r) => constraint_view(~group=p.model.group, r)
      | None =>
        div(
          ~attrs=[clss(["reach-constraint", "na"])],
          [text({js|—|js})],
        )
      };
    let main =
      div(
        ~attrs=[
          clss(["reach-row-main"]),
          Attr.on_pointerdown(_ => globals.inject_global(JumpToTile(p.id))),
        ],
        [
          enable_toggle(p),
          line_num(p),
          condition,
          group_chip(p.model.group),
          group_dropdown(p),
          solve_btn(() => solve_point(p)),
        ],
      );
    div(
      ~attrs=[clss(["reach-row"] @ (p.model.enabled ? [] : ["disabled"]))],
      [main]
      @ (
        show_result ? result_block(~group=p.model.group, p.model.result) : []
      ),
    );
  };

  let group_section = ((g, members): (int, list(point))) => {
    let collapsed =
      SidebarModel.Settings.is_reach_group_collapsed(g, settings);
    let name = SidebarModel.Settings.reach_group_name(g, settings);
    let enabled = List.filter((m: point) => m.model.enabled, members);
    let merged = List.find_map((m: point) => m.reach, enabled);
    let group_result = List.find_map((m: point) => m.model.result, enabled);
    let header =
      div(
        ~attrs=[clss(["reach-group-header"])],
        [
          control(
            ~cls=["reach-group-chevron"],
            ~tooltip=collapsed ? "Show members" : "Hide members",
            ~on_activate=
              () =>
                globals.inject_global(
                  Set(Sidebar(Reach(ToggleGroupCollapsed(g)))),
                ),
            collapsed ? {js|▸|js} : {js|▾|js},
          ),
          group_chip(g),
          Node.input(
            ~attrs=[
              clss(["reach-group-name"]),
              Attr.value(name),
              Attr.title("Group name"),
              Attr.on_pointerdown(evt => {
                stop(evt);
                Effect.Ignore;
              }),
              Attr.on_change((_, nm) =>
                globals.inject_global(
                  Set(Sidebar(Reach(SetGroupName(g, nm)))),
                )
              ),
            ],
            (),
          ),
          span(
            ~attrs=[clss(["reach-count"])],
            [text("(" ++ string_of_int(List.length(members)) ++ ")")],
          ),
          solve_btn(~tooltip="Find one input reaching all enabled members", () =>
            solve_group(members)
          ),
        ],
      );
    let merged_view =
      switch (merged) {
      | Some(r) => [constraint_view(~group=g, r)]
      | None => []
      };
    div(
      ~attrs=[
        clss(["reach-group"]),
        /* Carry the group's color onto the whole section, matching the offside
           chip. */
        Attr.create(
          "style",
          "border-left: 4px solid " ++ ReachProjView.group_color(g),
        ),
      ],
      [header]
      @ merged_view
      @ result_block(~group=g, group_result)
      @ (collapsed ? [] : List.map(point_row(~show_result=false), members)),
    );
  };

  let toggle_option = (label, ~is_active, ~on_pick) =>
    span(
      ~attrs=[
        clss(["toggle-option"] @ (is_active ? ["active"] : [])),
        Attr.on_click(_ => is_active ? Effect.Ignore : on_pick()),
      ],
      [text(label)],
    );

  let header =
    div(
      ~attrs=[clss(["reach-header"])],
      [
        div(~attrs=[clss(["main-title"])], [text("Reach Points")]),
        div(
          ~attrs=[clss(["reach-view-toggle"])],
          [
            toggle_option("Groups", ~is_active=!settings.flat, ~on_pick=() =>
              globals.inject_global(Set(Sidebar(Reach(ToggleReachView))))
            ),
            toggle_option("Order", ~is_active=settings.flat, ~on_pick=() =>
              globals.inject_global(Set(Sidebar(Reach(ToggleReachView))))
            ),
            span(
              ~attrs=[
                clss(["reach-solve-all"]),
                Attr.title("Solve all reach points and groups"),
                Attr.on_pointerdown(evt => {
                  Js_of_ocaml.Dom.preventDefault(evt);
                  stop(evt);
                  solve_all();
                  Effect.Ignore;
                }),
              ],
              [text("Solve all")],
            ),
          ],
        ),
      ],
    );

  let body =
    if (points == []) {
      [
        div(
          ~attrs=[clss(["reach-empty"])],
          [
            div([text("No reach points.")]),
            div(
              ~attrs=[clss(["reach-empty-hint"])],
              [
                text(
                  {js|Add one via right-click → "Find reaching inputs", or type ^^reach(…).|js},
                ),
              ],
            ),
          ],
        ),
      ];
    } else if (settings.flat) {
      let sorted =
        List.sort(
          (a: point, b) => compare((a.row, a.idx), (b.row, b.idx)),
          points,
        );
      [
        div(
          ~attrs=[clss(["reach-list"])],
          List.map(point_row(~show_result=true), sorted),
        ),
      ];
    } else {
      let solo_section =
        solo == []
          ? []
          : [
            div(
              ~attrs=[clss(["reach-solo"])],
              [
                div(
                  ~attrs=[clss(["reach-section-label"])],
                  [text("Solo")],
                ),
              ]
              @ List.map(
                  point_row(~show_result=true),
                  List.sort(
                    (a: point, b) =>
                      compare((a.row, a.idx), (b.row, b.idx)),
                    solo,
                  ),
                ),
            ),
          ];
      solo_section @ List.map(group_section, groups);
    };

  div(~attrs=[Attr.id("reach-sidebar")], [header] @ body);
};
