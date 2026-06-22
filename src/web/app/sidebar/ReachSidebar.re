open Virtual_dom.Vdom;
open Node;
open WebUtil;
open Haz3lcore;

/* Reach sidebar — a breakpoint-manager-style control panel for reach points.
 *
 * Group view (default): a Solo section plus one collapsible, named section per
 * merge group, each showing the group's merged (total) condition, a solve
 * button, and the merged solution. A reach point can belong to several groups
 * at once, so it appears under each of its groups' sections. Order view: a flat
 * list of every point with its own condition and per-group solutions.
 *
 * Group membership is edited on the reach point itself in the editor (the
 * offside chips); this panel reads/solves/names groups. Group merges are
 * assembled here (the sidebar sees every point) by conjoining members' solo
 * conditions via Reach.merge; solving runs Reach.smtlib2 → Z3Wasm.solve →
 * Reach.interpret and writes the outcomes back via Project(SetModel(…)). */

/* One reach point, resolved for display + dispatch. */
type point = {
  id: Id.t,
  /* index in the refractor list (= projector_list order), for SetModel */
  idx: int,
  /* serialized model decoded to {groups, enabled, results} */
  model: ReachProj.t,
  /* the `info` the cooked update expects (ReachProj.update ignores it) */
  info: ProjectorBase.info,
  /* this node's own (solo) path condition */
  individual: option(Reach.t),
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
     SetModel indexing (CodeEditable.re / Perform.re). */
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
            row,
          });
        }
      | _ => None
      },
    data,
  );
};

let cmp_pos = (a: point, b: point) =>
  compare((a.row, a.idx), (b.row, b.idx));

let stop = evt => Js_of_ocaml.Dom_html.stopPropagation(evt);

/* Render a reach condition as Hazel text (reusing the offside's renderer),
   colored by a merge group (neutral if group 0), with a trivial-condition
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

/* A colored numbered chip (matches the offside), one per group. */
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

/* Every group a point belongs to, as chips (so a point "includes all the
   colors of its groups"). Empty for an ungrouped point. */
let group_chips = (groups: list(int)) =>
  span(~attrs=[clss(["reach-group-chips"])], List.map(group_chip, groups));

let view =
    (
      ~globals: Globals.t,
      ~cursor as _: Cursor.cursor(Editors.Update.t),
      ~editor: CodeEditable.Model.t,
    )
    : Node.t => {
  let settings = globals.settings.sidebar.reach;
  let points = collect(~editor);
  let solo = List.filter((p: point) => p.model.groups == [], points);
  let all_groups =
    points
    |> List.concat_map((p: point) => p.model.groups)
    |> List.sort_uniq(compare);
  /* All points in group g (any enabled state), sorted by position. */
  let members = (g: int): list(point) =>
    points
    |> List.filter((p: point) => List.mem(g, p.model.groups))
    |> List.sort(cmp_pos);

  /* Dispatch a membership/enable change to a refractor. */
  let set_model = (p: point, action: ReachProj.reach_action) => {
    let new_model =
      ReachProj.M.update(p.model, p.info, action)
      |> ReachProj.M.sexp_of_model
      |> Sexplib.Sexp.to_string;
    globals.inject_global(
      ActiveEditor(Project(SetModel(p.idx, Reach, new_model))),
    );
  };

  /* A solve "job": (group, condition-to-solve, refractors-to-store-on). */
  let group_job = (g: int): option((int, Reach.t, list(point))) => {
    let ms = List.filter((p: point) => p.model.enabled, members(g));
    switch (List.filter_map((m: point) => m.individual, ms)) {
    | [] => None
    | rs => Some((g, Reach.merge(rs), ms))
    };
  };
  let solo_job = (p: point): option((int, Reach.t, list(point))) =>
    p.model.enabled ? Option.map(r => (0, r, [p]), p.individual) : None;

  /* When every job's outcome is known, write them all back — each refractor
     dispatched exactly once, with all of its (group, outcome) pairs folded into
     one model, so concurrent jobs touching the same point can't clobber each
     other (SetModel replaces the whole model). */
  let dispatch_all =
      (outs: list((int, list(point), TestGen.outcome))): Ui_effect.t(unit) => {
    let per_ref =
      List.fold_left(
        (acc, (g, ms, o)) =>
          List.fold_left(
            (acc, m: point) => {
              let prev =
                switch (Id.Map.find_opt(m.id, acc)) {
                | Some((_, rs)) => rs
                | None => []
                };
              Id.Map.add(m.id, (m, [(g, o), ...prev]), acc);
            },
            acc,
            ms,
          ),
        Id.Map.empty,
        outs,
      );
    Effect.Many(
      Id.Map.bindings(per_ref)
      |> List.map(((_, (m: point, results))) => {
           let new_model =
             List.fold_left(
               (mdl, (g, o)) =>
                 ReachProj.M.update(mdl, m.info, SetResult(g, o)),
               m.model,
               results,
             );
           /* Results are derived; store them off the undo history. */
           globals.inject_global(
             ActiveEditor(
               Project(
                 SetModelTransient(
                   m.idx,
                   Reach,
                   ReachProj.M.sexp_of_model(new_model)
                   |> Sexplib.Sexp.to_string,
                 ),
               ),
             ),
           );
         }),
    );
  };

  let solve_jobs = (jobs: list((int, Reach.t, list(point)))) =>
    switch (jobs) {
    | [] => ()
    | _ =>
      let total = List.length(jobs);
      let outs = ref([]);
      let done_ = ref(0);
      List.iter(
        ((g, cond: Reach.t, ms)) =>
          Z3Wasm.solve(
            ~k=
              outcome => {
                let o =
                  Reach.interpret(
                    ~complete=cond.complete,
                    ~inputs=cond.inputs,
                    outcome,
                  );
                outs := [(g, ms, o), ...outs^];
                incr(done_);
                if (done_^ >= total) {
                  Bonsai.Effect.Expert.handle(dispatch_all(outs^));
                };
              },
            Reach.smtlib2(cond) |> fst,
          ),
        jobs,
      );
    };

  let solve_group = (g: int) =>
    solve_jobs(List.filter_map(Fun.id, [group_job(g)]));
  let solve_point = (p: point) =>
    solve_jobs(
      List.filter_map(
        Fun.id,
        [solo_job(p)] @ List.map(group_job, p.model.groups),
      ),
    );
  let solve_all = () =>
    solve_jobs(
      List.filter_map(
        Fun.id,
        List.map(solo_job, points) @ List.map(group_job, all_groups),
      ),
    );

  /* A clickable control that stops the row's jump-on-click and runs the effect
     returned by on_activate. */
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

  /* Solving is fire-and-forget (it schedules its own dispatch), so the button
     returns no effect. */
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

  let result_block = (~group: int, result: option(TestGen.outcome)) =>
    switch (result) {
    | Some(o) => [ReachProjView.result_view(~group, ~multiline=true, o)]
    | None => []
    };

  /* A member row inside group g's section: the point's own condition (colored
     by g); the group's solution is shown once in the section header. */
  let member_row = (~group: int, p: point) => {
    let condition =
      switch (p.individual) {
      | Some(r) => constraint_view(~group, r)
      | None =>
        div(
          ~attrs=[clss(["reach-constraint", "na"])],
          [text({js|—|js})],
        )
      };
    div(
      ~attrs=[clss(["reach-row"] @ (p.model.enabled ? [] : ["disabled"]))],
      [
        div(
          ~attrs=[
            clss(["reach-row-main"]),
            Attr.on_pointerdown(_ =>
              globals.inject_global(JumpToTile(p.id))
            ),
          ],
          [enable_toggle(p), line_num(p), condition],
        ),
      ],
    );
  };

  /* A flat row (Solo section + Order view): the point's own condition (neutral),
     chips for every group it is in, a solve button, and one solution pill per
     solved group below. */
  let flat_row = (p: point) => {
    let condition =
      switch (p.individual) {
      | Some(r) => constraint_view(~group=0, r)
      | None =>
        div(
          ~attrs=[clss(["reach-constraint", "na"])],
          [text({js|—|js})],
        )
      };
    let results =
      p.model.results
      |> List.sort(((a, _), (b, _)) => compare(a, b))
      |> List.concat_map(((g, o)) => result_block(~group=g, Some(o)));
    div(
      ~attrs=[clss(["reach-row"] @ (p.model.enabled ? [] : ["disabled"]))],
      [
        div(
          ~attrs=[
            clss(["reach-row-main"]),
            Attr.on_pointerdown(_ =>
              globals.inject_global(JumpToTile(p.id))
            ),
          ],
          [
            enable_toggle(p),
            line_num(p),
            condition,
            group_chips(p.model.groups),
            solve_btn(() => solve_point(p)),
          ],
        ),
      ]
      @ results,
    );
  };

  let group_section = (g: int) => {
    let ms = members(g);
    let collapsed =
      SidebarModel.Settings.is_reach_group_collapsed(g, settings);
    let name = SidebarModel.Settings.reach_group_name(g, settings);
    let enabled = List.filter((m: point) => m.model.enabled, ms);
    let merged: option(Reach.t) =
      switch (List.filter_map((m: point) => m.individual, enabled)) {
      | [] => None
      | rs => Some(Reach.merge(rs))
      };
    let group_result =
      List.find_map((m: point) => List.assoc_opt(g, m.model.results), ms);
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
            [text("(" ++ string_of_int(List.length(ms)) ++ ")")],
          ),
          solve_btn(~tooltip="Find one input reaching all enabled members", () =>
            solve_group(g)
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
        Attr.create(
          "style",
          "border-left: 4px solid " ++ ReachProjView.group_color(g),
        ),
        /* Hovering a group highlights its reach points with a connecting line
           in the editor (imperative — no re-render; see ReachHover). */
        Attr.on_mouseenter(_ => {
          ReachHover.set(Some(g));
          Effect.Ignore;
        }),
        Attr.on_mouseleave(_ => {
          ReachHover.set(None);
          Effect.Ignore;
        }),
      ],
      [header]
      @ merged_view
      @ result_block(~group=g, group_result)
      @ (collapsed ? [] : List.map(member_row(~group=g), ms)),
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
      [
        div(
          ~attrs=[clss(["reach-list"])],
          List.map(flat_row, List.sort(cmp_pos, points)),
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
              @ List.map(flat_row, List.sort(cmp_pos, solo)),
            ),
          ];
      solo_section @ List.map(group_section, all_groups);
    };

  div(~attrs=[Attr.id("reach-sidebar")], [header] @ body);
};
