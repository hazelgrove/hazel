open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

module StaticsBase = StaticsBase;

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

let basic = (~globals: Globals.t, id: Id.t) =>
  div(
    ~attrs=[
      Attr.create("style", "cursor: pointer;"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [text(Id.str3(id))],
  );

let exp_view = (~available, term: Exp.t) =>
  Abbreviate.abbreviate_exp(~available, term)
  |> fst
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
     );

let pat_view = (~available, term: Pat.t) =>
  Abbreviate.abbreviate_pat(~available, term)
  |> fst
  |> (x => Grammar.Pat(x))
  |> ExpToSegment.any_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
     );

let segment_of = (~default, ~available=8, term: Any.t): option(Segment.t) =>
  switch (term) {
  | Exp(x) => Some(exp_view(~available, x))
  | Pat(x) => Some(pat_view(~available, x))
  | _ => default
  };

let term_view =
    (
      ~globals: Globals.t,
      ~default,
      ~background,
      ~text_only,
      ~available=8,
      term: Any.t,
    )
    : option(Node.t) => {
  open Util.OptUtil.Syntax;
  let+ segment = segment_of(~default, ~available, term);
  ProjectorView.flex_code(
    ~background,
    ~text_only,
    ~font_metrics=globals.font_metrics,
    Sort.Exp,
    segment,
  );
};

let probe_view = (font_metrics, refractor_data, id: Id.t) => {
  let inject = _ => Ui_effect.Ignore;
  let projector_data =
    List.find_opt(
      (p: ProjectorView.Model.projector_data) => p.p.id == id,
      refractor_data,
    );
  switch (projector_data) {
  | Some(projector_data) =>
    let views =
      ProjectorView.mk_view(inject, font_metrics, projector_data, [id]);
    let offside_view = views.offside |> Option.to_list;
    div(~attrs=[Attr.class_("probe-view")], offside_view);
  | None => div([] /*text("Not Probed")*/)
  };
};

let fancy =
    (
      ~refractor_data,
      ~info_map: Statics.Map.t,
      ~globals: Globals.t,
      ~default,
      id: Id.t,
    ) => {
  open Util.OptUtil.Syntax;
  let any =
    switch (Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({term, _})) => Grammar.Exp(term)
    | Some(InfoPat({term, _})) => Grammar.Pat(term)
    | _ => Grammar.Any()
    };
  let+ term_view =
    term_view(
      ~globals,
      ~default,
      ~background=false,
      ~text_only=true,
      ~available=12,
      any,
    );
  div(
    ~attrs=[
      Attr.class_("probe-entry"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [term_view, probe_view(globals.font_metrics, refractor_data, id)],
  );
};

let sort_ids_by_measurement = (~measured: Measured.t, ids: list((Id.t, _))) =>
  ids
  |> List.sort(((id1, _p1), (id2, _p2)) =>
       compare(
         switch (Measured.find_by_id(id1, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
         switch (Measured.find_by_id(id2, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
       )
     );

let div_cs = (cls, node) => div(~attrs=[Attr.classes(cls)], [node]);

let legend_sample_view =
    (
      ~indicated: bool,
      ~mode: Sample.Window.mode,
      ~font_metrics: FontMetrics.t,
      ~ap_id: option(Id.t),
      ~indicated_call: option(Id.t),
      ~cursor_stack: Sample.call_stack,
      ~sample_stack: Sample.call_stack,
      ~step_range: (int, int),
      ~focus_step_range: option((int, int)),
      ~caption: string,
    ) => {
  let (step_start, step_end) = step_range;
  let sample: Sample.t = {
    id: 0,
    syntax_id: Id.invalid,
    value: IdTagged.FreshGrammar.Exp.constructor(caption, None),
    env: Sample.Env.empty,
    call_stack: sample_stack,
    time: 0.0,
    seq: 0,
    origin: Sample.Probe,
    step_start,
    step_end,
  };
  let di: Dynamics.Info.t = {
    samples: [sample],
    sample_cursor: {
      call_stack: cursor_stack,
      index: List.length(cursor_stack) - 1,
      pinned_stack: None,
      indicated_call,
      time: None,
      seq: 0,
      step_range: focus_step_range,
      pending_focus: None,
    },
  };
  ProbeProj.sample_view(
    ~ap_id,
    ~hide_env=true,
    ~settings={
      ...ProbeProj.Settings.s^,
      window: mode,
    },
    ~num_total=1,
    di,
    ProjectorInfo.utility,
    (~text_only) =>
      ProjectorView.flex_code(
        ~font_metrics,
        ~single_line=true,
        ~background=false,
        ~text_only,
      ),
    _ => Effect.Ignore,
    _ => Effect.Ignore,
    (0, sample),
  )
  |> div_cs(["sample-group"])
  |> div_cs(["sample-groups"])
  |> div_cs(["live-offside", Sample.Window.show_mode(mode)])
  |> div_cs(["projector", "probe", indicated ? "indicated" : "not-indicated"]);
};

let legend_item = (~tooltip: string, sample_view: Node.t) =>
  div(
    ~attrs=[clss(["legend-item"])],
    [
      sample_view,
      div(~attrs=[clss(["legend-tooltip"])], [text(tooltip)]),
    ],
  );

let legend_view = (~font_metrics: FontMetrics.t) => {
  let mode = ProbeProj.Settings.s^.window;
  /* Focus step range for StepRange mode comparisons */
  let focus = Some((10, 20));
  let legend_sample_view = legend_sample_view(~mode, ~font_metrics);
  div(
    ~attrs=[clss(["legend", "panel"])],
    [
      div(
        ~attrs=[clss(["title"])],
        [text("Dynamic Cursor Sample Legend")],
      ),
      legend_item(
        ~tooltip=
          "This sample was collected before the cursor position in the call stack.",
        legend_sample_view(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[(Id.invalid, None), (Id.invalid, None)],
          ~sample_stack=[(Id.invalid, None)],
          ~step_range=(0, 5),
          ~focus_step_range=focus,
          ~caption="Before",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is at the current cursor position in the call stack.",
        legend_sample_view(
          ~indicated=true,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[(Id.invalid, None)],
          ~sample_stack=[(Id.invalid, None)],
          ~step_range=(10, 20),
          ~focus_step_range=None,
          ~caption="At Cursor",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample was collected after the cursor position in the call stack.",
        legend_sample_view(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[(Id.invalid, None)],
          ~sample_stack=[(Id.invalid, None), (Id.invalid, None)],
          ~step_range=(25, 30),
          ~focus_step_range=focus,
          ~caption="After",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is from a call site that contains the cursor position.",
        legend_sample_view(
          ~indicated=false,
          ~indicated_call=None,
          ~ap_id=Some(Id.invalid),
          ~cursor_stack=[(Id.invalid, None), (Id.invalid, None)],
          ~sample_stack=[(Id.invalid, None)],
          ~step_range=(5, 25),
          ~focus_step_range=focus,
          ~caption="Contains",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is from a different branch of the call stack than the cursor.",
        legend_sample_view(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[(Id.mk(), None)],
          ~sample_stack=[(Id.invalid, None)],
          ~step_range=(0, 0),
          ~focus_step_range=None,
          ~caption="Off Cursor",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is from inside a function call at the cursor position.",
        legend_sample_view(
          ~indicated=false,
          ~indicated_call=Some(Id.invalid),
          ~ap_id=None,
          ~cursor_stack=[(Id.invalid, None)],
          ~sample_stack=[(Id.invalid, None), (Id.invalid, None)],
          ~step_range=(12, 18),
          ~focus_step_range=focus,
          ~caption="Inside",
        ),
      ),
    ],
  );
};

let toggle =
    (~tooltip, ~explain_this_inject, ~label1, ~label2, ~active, ~action) =>
  Widgets.toggle_named(
    ~tooltip,
    active ? label1 : label2,
    active,
    _ => {
      ProbeProj.Settings.go(action);
      explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
    },
  );

let settings = (~globals: Globals.t, ~explain_this_inject) => {
  div(
    ~attrs=[clss(["settings"])],
    [
      Widgets.toggle_named(
        ~tooltip="Auto-probe mode (Cmd/Ctrl+Shift+P)",
        globals.settings.auto_probe_mode ? "A" : "M",
        globals.settings.auto_probe_mode,
        _ =>
        globals.inject_global(Set(AutoProbeMode))
      ),
      toggle(
        ~tooltip="One or Many Samples",
        ~explain_this_inject,
        ~label1="1",
        ~label2="∞",
        ~active=ProbeProj.Settings.s^.window == Single,
        ~action=ToggleWindow,
      ),
      {
        /* 3-way cycle toggle for sample coloring mode */
        let (icon, tooltip) =
          switch (ProbeProj.Settings.s^.sample_base) {
          | Calls => (
              "\xF0\x9F\x93\x9E",
              "Color by Calls (click to switch to Steps)",
            )
          | Steps => (
              "\xF0\x9F\x91\xA3",
              "Color by Steps (click to switch to StepRange)",
            )
          | StepRange => (
              "\xE2\x8F\xB1",
              "Color by StepRange (click to switch to Calls)",
            )
          };
        Widgets.toggle(
          ~tooltip,
          icon,
          false,
          _ => {
            ProbeProj.Settings.go(ToggleSampleBase);
            explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
          },
        );
      },
      toggle(
        ~tooltip="Samples Before/Above Cursor",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.before_cutoff == None,
        ~action=ToggleBeforeCutoff,
      ),
      toggle(
        ~tooltip="Samples After/Below Cursor",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.after_cutoff == None,
        ~action=ToggleAfterCutoff,
      ),
      toggle(
        ~tooltip="Callsites containing Cursor",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.caller_cutoff == None,
        ~action=ToggleCallerCutoff,
      ),
      toggle(
        ~tooltip="Samples Inside Call at Cursor",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.callee_cutoff == None,
        ~action=ToggleCalleeCutoff,
      ),
    ],
  );
};

let sketch_view = (~globals: Globals.t, ~explain_this_inject): Node.t =>
  details(
    ~attrs=[clss(["sketch"])],
    [
      settings(~globals, ~explain_this_inject),
      summary(
        ~attrs=[clss(["sketch-toggle"])],
        [div(~attrs=[clss(["sketch-toggle-image"])], [])],
      ),
      div(~attrs=[clss(["sketch-body"])], []),
    ],
  );

let call_cursor_view = (~sample_cursor: Sample.Cursor.t, ~fancyd) =>
  div(
    ~attrs=[clss(["panel", "call-cursor"])],
    [
      div(~attrs=[clss(["title"])], [text("Call Stack")]),
      div(
        ~attrs=[clss(["stack"])],
        List.mapi(
          (i, (id, _name)) =>
            div(
              ~attrs=[
                Attr.classes([
                  i == sample_cursor.index ? "is-index" : "not",
                  i > sample_cursor.index ? "after-index" : "not",
                  List.exists(
                    ((frame_id, _)) => frame_id == id,
                    sample_cursor.call_stack,
                  )
                  && Some(id) == sample_cursor.indicated_call
                    ? "indicated-call" : "not",
                ]),
              ],
              [fancyd(id)],
            ),
          sample_cursor.call_stack |> List.rev,
        ),
      ),
    ],
  );

/* probe_type tracks whether a probe is manual or auto.
 * Auto probes include the list of ephemeral IDs they expand to. */
type probe_type =
  | Manual
  | Auto(list(Id.t));

let prep_refractors =
    (~refractors: Zipper.Refractor.t, ~info_map, ~syntax: CachedSyntax.t) => {
  let manuals = refractors.manuals |> List.map(((id, _)) => (id, Manual));
  let autos =
    refractors.autos.ids
    |> Id.Map.bindings
    |> List.map(((id, ())) => {
         let ids = ProbePerform.ids_from_term(~syntax, ~info_map, id);
         let ephemeral_ids =
           List.filter(
             id => Id.Map.mem(id, refractors.autos.ephemerals),
             ids,
           );
         (id, Auto(ephemeral_ids));
       });
  List.concat([manuals, autos])
  |> sort_ids_by_measurement(~measured=syntax.measured);
};

type refractor_group = {
  top_pat: option(Pat.t),
  entries: list((Id.t, probe_type)),
};

let top_level_pattern = (~info_map: Statics.Map.t, ~id: Id.t): option(Pat.t) =>
  switch (StaticsBase.let_definition_path(~statics=info_map, ~id)) {
  | [pat, ..._] => Some(pat)
  | _ => None
  };

let same_top_level = (left: option(Pat.t), right: option(Pat.t)): bool =>
  switch (left, right) {
  | (Some(lpat), Some(rpat)) => Pat.equal(lpat, rpat)
  | (None, None) => true
  | _ => false
  };

let push_group =
    (
      ~label: option(Pat.t),
      ~entries: list((Id.t, probe_type)),
      groups: list(refractor_group),
    )
    : list(refractor_group) =>
  switch (entries) {
  | [] => groups
  | _ => [
      {
        top_pat: label,
        entries: List.rev(entries),
      },
      ...groups,
    ]
  };

let group_refractors =
    (~info_map: Statics.Map.t, entries: list((Id.t, probe_type)))
    : list(refractor_group) => {
  let rec loop =
          (
            remaining: list((Id.t, probe_type)),
            current_label: option(Pat.t),
            current_entries: list((Id.t, probe_type)),
            groups: list(refractor_group),
          )
          : list(refractor_group) =>
    switch (remaining) {
    | [] =>
      let final_groups =
        push_group(~label=current_label, ~entries=current_entries, groups);
      List.rev(final_groups);
    | [entry, ...rest] =>
      let (id: Id.t, _probe: probe_type) = entry;
      let label: option(Pat.t) = top_level_pattern(~info_map, ~id);
      if (same_top_level(label, current_label)) {
        loop(rest, current_label, [entry, ...current_entries], groups);
      } else {
        let updated_groups =
          push_group(~label=current_label, ~entries=current_entries, groups);
        loop(rest, label, [entry], updated_groups);
      };
    };
  loop(entries, None, [], []);
};

let render_entry =
    (~fancyd: Id.t => option(Node.t), entry: (Id.t, probe_type))
    : option(Node.t) =>
  switch (entry) {
  | (id, Manual) => fancyd(id)
  | (_id, Auto(ephemeral_ids)) =>
    let ephemerals = List.filter_map(fancyd, ephemeral_ids);
    ephemerals == []
      ? None : Some(div(~attrs=[clss(["auto"])], ephemerals));
  };

let render_group =
    (
      ~globals: Globals.t,
      ~fancyd: Id.t => option(Node.t),
      group: refractor_group,
    )
    : list(Node.t) => {
  let body_nodes: list(Node.t) =
    List.filter_map(
      (entry: (Id.t, probe_type)) => render_entry(~fancyd, entry),
      group.entries,
    );
  switch (group.top_pat) {
  | Some(pat) =>
    let title_option: option(Node.t) =
      term_view(
        ~globals,
        ~default=None,
        ~background=false,
        ~available=17,
        ~text_only=false,
        Grammar.Pat(pat),
      );
    let title_node: Node.t =
      Option.value(
        ~default=div([text("Untitled definition")]),
        title_option,
      );
    [
      div(
        ~attrs=[clss(["top-level-group"])],
        [
          div(~attrs=[clss(["top-level-title"])], [title_node]),
          div(~attrs=[clss(["top-level-body"])], body_nodes),
        ],
      ),
    ];
  | None => body_nodes
  };
};

let append_group_nodes =
    (
      ~globals: Globals.t,
      ~fancyd: Id.t => option(Node.t),
      groups: list(refractor_group),
    )
    : list(Node.t) => {
  let rec loop =
          (remaining: list(refractor_group), acc: list(Node.t))
          : list(Node.t) =>
    switch (remaining) {
    | [] => List.rev(acc)
    | [group, ...rest] =>
      let nodes_for_group: list(Node.t) =
        render_group(~globals, ~fancyd, group);
      loop(rest, List.rev_append(nodes_for_group, acc));
    };
  loop(groups, []);
};

let probes_panel_view =
    (
      ~globals: Globals.t,
      ~refractors: Zipper.Refractor.t,
      ~info_map: Statics.Map.t,
      ~syntax: CachedSyntax.t,
      ~fancyd: Id.t => option(Node.t),
    ) => {
  let grouped: list(refractor_group) =
    group_refractors(
      ~info_map,
      prep_refractors(~refractors, ~info_map, ~syntax),
    );
  let group_nodes: list(Node.t) =
    append_group_nodes(~globals, ~fancyd, grouped);
  group_nodes == []
    ? div([])
    : div(
        ~attrs=[clss(["panel", "probes"])],
        [div(~attrs=[clss(["title"])], [text("Probes")])] @ group_nodes,
      );
};

let print_string = (probes: Sample.Map.t) => {
  let collect_print_samples = (probes: Sample.Map.t): list(Sample.t) =>
    Id.Map.fold(
      (_, samples, acc) =>
        List.fold_left(
          (acc, sample) =>
            sample.Sample.origin == Sample.Print ? [sample, ...acc] : acc,
          acc,
          samples,
        ),
      probes,
      [],
    );

  let collect_print_outputs = (probes: Sample.Map.t): list(string) =>
    collect_print_samples(probes)
    |> List.sort((a, b) => Int.compare(a.Sample.seq, b.Sample.seq))
    |> List.map(sample =>
         sample.Sample.value
         |> ExpToSegment.exp_to_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
            )
         |> Printer.of_segment(~holes="")
       );

  let print_summary = (probes: Sample.Map.t): option(string) =>
    switch (collect_print_outputs(probes)) {
    | [] => None
    | outputs => Some(String.concat("\n", outputs))
    };

  probes |> print_summary;
};

type panel_mode =
  | Probes
  | Prints;

let mode = ref(Probes);

/* Eval mode for Printarium: Auto refreshes on every change, Manual requires clicking Run */
type eval_mode =
  | Auto
  | Manual;

let eval_mode_ref = ref(Auto);
let cached_print_output = ref(None: option(string));

let mode_toggle = (~explain_this_inject) =>
  Widgets.toggle(
    ~tooltip="Toggle between Probes and Prints",
    mode^ == Probes ? "🔍" : "🖨",
    mode^ == Probes,
    _ => {
      mode := mode^ == Probes ? Prints : Probes;
      explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
    },
  );

let eval_mode_button = (~explain_this_inject, ~label, ~is_active, ~action) =>
  div(
    ~attrs=[
      clss(["eval-mode-button", is_active ? "active" : "inactive"]),
      Attr.on_click(_ => {
        action();
        explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
      }),
    ],
    [text(label)],
  );

let run_button = (~explain_this_inject, ~editor: CodeEditable.Model.t) =>
  div(
    ~attrs=[
      clss(["run-button"]),
      Attr.title("Run and refresh print output"),
      Attr.on_click(_ => {
        cached_print_output := print_string(editor.dynamics);
        explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
      }),
    ],
    [text("Run")],
  );

let printarium = (~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  /* Determine what output to display */
  let output =
    switch (eval_mode_ref^) {
    | Auto => print_string(editor.dynamics)
    | Manual => cached_print_output^
    };
  [
    div(
      ~attrs=[clss(["header"])],
      [
        div(
          ~attrs=[clss(["main-title"])],
          [text("Console Log"), mode_toggle(~explain_this_inject)],
        ),
      ],
    ),
    div(
      ~attrs=[clss(["eval-controls"])],
      [
        eval_mode_button(
          ~explain_this_inject,
          ~label="",
          ~is_active=eval_mode_ref^ == Auto,
          ~action=() =>
          eval_mode_ref := Auto
        ),
        eval_mode_button(
          ~explain_this_inject,
          ~label="",
          ~is_active=eval_mode_ref^ == Manual,
          ~action=() =>
          eval_mode_ref := Manual
        ),
        ...eval_mode_ref^ == Manual
             ? [run_button(~explain_this_inject, ~editor)] : [],
      ],
    ),
    div(
      ~attrs=[clss(["panel", "prints"])],
      [
        div(
          ~attrs=[clss(["body", "code"])],
          [
            switch (output) {
            | Some(summary) => text(summary)
            | None =>
              text(
                eval_mode_ref^ == Manual
                  ? "Click Run to see print outputs" : "No print outputs",
              )
            },
          ],
        ),
      ],
    ),
  ];
};

let probearium =
    (~globals: Globals.t, ~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  let zipper = editor.editor.state.zipper;
  let refractor_data =
    RefractorView.mk_data(
      ~refractors=
        Id.Map.union(
          (_, _, b) => Some(b),
          zipper.refractors.manuals |> Id.Map.of_list,
          zipper.refractors.autos.ephemerals,
        ),
      ~syntax=editor.editor.syntax,
      ~indicated=Indicated.piece(zipper),
      ~statics=editor.statics.info_map,
      ~dynamics=editor.dynamics,
      ~sample_cursor=zipper.refractors.sample_cursor,
      ~editor_active=true,
    );
  let refractors = editor.editor.state.zipper.refractors;
  [
    div(
      ~attrs=[clss(["header"])],
      [
        div(
          ~attrs=[clss(["main-title"])],
          [text("Probearium"), mode_toggle(~explain_this_inject)],
        ),
      ],
    ),
    legend_view(~font_metrics=globals.font_metrics),
    sketch_view(~globals, ~explain_this_inject),
    // call_cursor_view(~sample_cursor=refractors.sample_cursor, ~fancyd=id =>
    //   fancy(
    //     ~refractor_data,
    //     ~info_map=editor.statics.info_map,
    //     ~default=None, /*Some([Example.exp("<In Builtin>")]),*/
    //     ~globals,
    //     id,
    //   )
    //   |> Option.value(~default=div([]))
    // ),
    //TODO(andrew): don't show autos here? or collapse them by default at least
    probes_panel_view(
      ~globals,
      ~refractors,
      ~info_map=editor.statics.info_map,
      ~syntax=editor.editor.syntax,
      ~fancyd=id =>
      fancy(
        ~refractor_data,
        ~info_map=editor.statics.info_map,
        ~default=None,
        ~globals,
        id,
      )
    ),
  ];
};

let view =
    (
      ~globals: Globals.t,
      ~cursor as _: Cursor.cursor(Editors.Update.t),
      ~explain_this_inject,
      ~editor: CodeEditable.Model.t,
    ) => {
  div(
    ~attrs=[Attr.id("probesys")],
    mode^ == Probes
      ? probearium(~globals, ~explain_this_inject, ~editor)
      : printarium(~explain_this_inject, ~editor),
  );
};
