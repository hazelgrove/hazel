open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

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

let legend_sample =
    (
      ~indicated: bool,
      ~mode: Sample.Window.mode,
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
    args: None,
    time: 0.0,
    seq: 0,
    origin: Sample.Probe,
    step_start,
    step_end,
  };
  let dynamics: Dynamics.Info.t = {
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
  let settings = {
    ...ProbeProj.Settings.s^,
    window: mode,
  };
  let cursor_clss =
    ProbeProj.cursor_clss(~settings, ~ap_id, dynamics, sample);
  let caption_node =
    div(
      ~attrs=[clss(["code"])],
      [
        span(
          ~attrs=[clss(["code-text"])],
          [span(~attrs=[clss(["token"])], [text(caption)])],
        ),
      ],
    );
  div(~attrs=[Attr.classes(["value"] @ cursor_clss)], [caption_node])
  |> div_cs(["sample"])
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

let legend_view = () => {
  let mode = ProbeProj.Settings.s^.window;
  let focus = Some((10, 20));
  let f: Sample.stack_frame = {
    id: Id.invalid,
    name: None,
    fn_def_id: None,
  };
  let legend_sample = legend_sample(~mode);
  div(
    ~attrs=[clss(["legend", "panel"])],
    [
      div(
        ~attrs=[clss(["title"])],
        [text("Dynamic Cursor Sample Legend")],
      ),
      legend_item(
        ~tooltip=
          "This sample was collected before/above the cursor position in the call stack.",
        legend_sample(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f, f],
          ~sample_stack=[f],
          ~step_range=(0, 5),
          ~focus_step_range=focus,
          ~caption="Before",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is at the current cursor position in the call stack.",
        legend_sample(
          ~indicated=true,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f],
          ~sample_stack=[f],
          ~step_range=(10, 20),
          ~focus_step_range=None,
          ~caption="At Cursor",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample was collected after/below the cursor position in the call stack.",
        legend_sample(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f],
          ~sample_stack=[f, f],
          ~step_range=(25, 30),
          ~focus_step_range=focus,
          ~caption="After",
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is from a function call site which is above the cursor position in the call stack.",
        legend_sample(
          ~indicated=false,
          ~indicated_call=None,
          ~ap_id=Some(Id.invalid),
          ~cursor_stack=[f, f],
          ~sample_stack=[f],
          ~step_range=(5, 25),
          ~focus_step_range=focus,
          ~caption="Contains",
        ),
      ),
      switch (mode) {
      | Single =>
        legend_item(
          ~tooltip=
            "Samples not shown as they are not aligned with dynamic cursor; click to align the cursor and show them.",
          div(~attrs=[clss(["legend-not-aligned"])], [text({js|⊖|js})]),
        )
      | Many =>
        legend_item(
          ~tooltip=
            "This sample is from a different branch of the call stack than the cursor.",
          legend_sample(
            ~indicated=false,
            ~ap_id=None,
            ~indicated_call=None,
            ~cursor_stack=[
              {
                ...f,
                id: Id.mk(),
              },
            ],
            ~sample_stack=[f],
            ~step_range=(0, 0),
            ~focus_step_range=None,
            ~caption="Off Cursor",
          ),
        )
      },
      legend_item(
        ~tooltip=
          "This sample was collected inside the function call at the cursor position, or in a call made from that call.",
        legend_sample(
          ~indicated=false,
          ~indicated_call=Some(Id.invalid),
          ~ap_id=None,
          ~cursor_stack=[f],
          ~sample_stack=[f, f],
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
        /* 2-way toggle for sample coloring mode */
        let (icon, tooltip) =
          switch (ProbeProj.Settings.s^.sample_base) {
          | Calls => (
              "\xF0\x9F\x93\x9E",
              "Color by Calls (click to switch to StepRange)",
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

/* probe_type tracks whether a probe is manual or auto.
 * Auto probes include the list of ephemeral IDs they expand to. */
type probe_type =
  | Manual
  | Auto(list(Id.t));

let prep_refractors =
    (~refractors: Zipper.Refractor.t, ~syntax: CachedSyntax.t) => {
  refractors.manuals
  |> List.map(((id, _)) => (id, Manual))
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
    | [(id, _) as entry, ...rest] =>
      let label = top_level_pattern(~info_map, ~id);
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

let render_entry = (~fancyd, entry) =>
  switch (entry) {
  | (id, Manual) => fancyd(id)
  | (_, Auto(ephemeral_ids)) =>
    let ephemerals = List.filter_map(fancyd, ephemeral_ids);
    ephemerals == []
      ? None : Some(div(~attrs=[clss(["auto"])], ephemerals));
  };

let render_group = (~globals: Globals.t, ~fancyd, group: refractor_group) => {
  let body_nodes = List.filter_map(render_entry(~fancyd), group.entries);
  switch (group.top_pat) {
  | Some(pat) =>
    let title_node =
      term_view(
        ~globals,
        ~default=None,
        ~background=false,
        ~available=17,
        ~text_only=false,
        Grammar.Pat(pat),
      )
      |> Option.value(~default=div([text("Untitled definition")]));
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

let probes_panel_view =
    (
      ~globals: Globals.t,
      ~refractors: Zipper.Refractor.t,
      ~info_map: Statics.Map.t,
      ~syntax: CachedSyntax.t,
      ~fancyd: Id.t => option(Node.t),
    ) => {
  let group_nodes =
    prep_refractors(~refractors, ~syntax)
    |> group_refractors(~info_map)
    |> List.concat_map(render_group(~globals, ~fancyd));
  group_nodes == []
    ? div([])
    : div(
        ~attrs=[clss(["panel", "probes"])],
        [div(~attrs=[clss(["title"])], [text("Probes")])] @ group_nodes,
      );
};

type print_entry = {
  seq: int,
  value_str: string,
  line: option(int),
};

let collect_print_entries =
    (probes: Sample.Map.t, measured: Measured.t): list(print_entry) => {
  let samples =
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
  samples
  |> List.sort((a, b) => Int.compare(a.Sample.seq, b.Sample.seq))
  |> List.map(sample => {
       let value_str =
         sample.Sample.value
         |> ExpToSegment.exp_to_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
            )
         |> Printer.of_segment(~holes="");
       let line =
         switch (Measured.find_by_id(sample.syntax_id, measured)) {
         | Some(m) => Some(m.origin.row + 1)
         | None => None
         };
       {
         seq: sample.seq + 1,
         value_str,
         line,
       };
     });
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
let cached_print_entries = ref(None: option(list(print_entry)));

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

let run_button = (~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  let measured = editor.editor.syntax.measured;
  div(
    ~attrs=[
      clss(["run-button"]),
      Attr.title("Run and refresh print output"),
      Attr.on_click(_ => {
        let entries = collect_print_entries(editor.dynamics, measured);
        cached_print_entries := List.is_empty(entries) ? None : Some(entries);
        explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
      }),
    ],
    [text("Run")],
  );
};

let render_print_entry = (entry: print_entry): Node.t =>
  div(
    ~attrs=[clss(["print-entry"])],
    [
      span(
        ~attrs=[clss(["print-seq"])],
        [text(string_of_int(entry.seq))],
      ),
      span(~attrs=[clss(["print-value"])], [text(entry.value_str)]),
      span(
        ~attrs=[clss(["print-line"])],
        [
          text(
            switch (entry.line) {
            | Some(n) => ":" ++ string_of_int(n)
            | None => ""
            },
          ),
        ],
      ),
    ],
  );

let printarium = (~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  let measured = editor.editor.syntax.measured;
  /* Determine what entries to display */
  let entries =
    switch (eval_mode_ref^) {
    | Auto =>
      let es = collect_print_entries(editor.dynamics, measured);
      List.is_empty(es) ? Option.none : Option.some(es);
    | Manual => cached_print_entries^
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
        Widgets.toggle_named(
          ~tooltip="Auto-eval",
          eval_mode_ref^ == Auto ? "A" : "M",
          eval_mode_ref^ == Auto,
          _ => {
            eval_mode_ref := eval_mode_ref^ == Auto ? Manual : Auto;
            explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
          },
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
          switch (entries) {
          | Some(es) => List.map(render_print_entry, es)
          | None => [
              text(
                eval_mode_ref^ == Manual
                  ? "Click Run to see print outputs" : "No print outputs",
              ),
            ]
          },
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
          Id.Map.of_list(zipper.refractors.manuals),
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
    legend_view(),
    sketch_view(~globals, ~explain_this_inject),
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
