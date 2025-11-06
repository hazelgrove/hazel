open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

module StaticsBase = Language.StaticsBase;

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

let exp_view = (~available, term: Language.Exp.t) =>
  Language.Abbreviate.abbreviate_exp(~available, term)
  |> fst
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(
           ~inline=true,
           Language.CoreSettings.off,
         ),
     );

let pat_view = (~available, term: Language.Pat.t) =>
  Language.Abbreviate.abbreviate_pat(~available, term)
  |> fst
  |> (x => Language.Grammar.Pat(x))
  |> ExpToSegment.any_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(
           ~inline=true,
           Language.CoreSettings.off,
         ),
     );

let segment_of =
    (~default, ~available=8, term: Language.Any.t): option(Segment.t) =>
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
      term: Language.Any.t,
    )
    : option(Node.t) => {
  open Util.OptUtil.Syntax;
  let+ segment = segment_of(~default, ~available, term);
  ProjectorView.flex_code(
    ~background,
    ~is_single_line=Some(),
    ~text_only,
    ~font_metrics=globals.font_metrics,
    Language.Sort.Exp,
    segment,
  );
};

let probe_view = (font_metrics, refractor_data, id: Id.t) => {
  let inject = (a: Action.t) => {
    print_endline("ProbeProj inject called" ++ Action.show(a));
    Ui_effect.Ignore;
  };
  let projector_data =
    List.find_opt(
      (p: ProjectorView.Model.projector_data) => p.p.id == id,
      refractor_data,
    );
  switch (projector_data) {
  | Some(projector_data) =>
    let views = ProjectorView.mk_view(inject, font_metrics, projector_data);
    let offside_view = views.offside |> Option.to_list;
    div(~attrs=[Attr.class_("probe-view")], offside_view);
  | None => div([] /*text("Not Probed")*/)
  };
};

let fancy =
    (
      ~refractor_data,
      ~info_map: Language.Statics.Map.t,
      ~globals: Globals.t,
      ~default,
      id: Id.t,
    ) => {
  open Util.OptUtil.Syntax;
  let any =
    switch (Language.Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({term, _})) => Language.Grammar.Exp(term)
    | Some(InfoPat({term, _})) => Language.Grammar.Pat(term)
    | _ => Language.Grammar.Any()
    };
  let+ term_view =
    term_view(
      ~globals,
      ~default,
      ~background=false,
      ~text_only=Some(),
      ~available=12,
      any,
    );
  div(
    ~attrs=[
      Attr.class_("probe-entry"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [
      term_view,
      probe_view(
        globals.font_metrics,
        refractor_data,
        Id.transform_variant(id),
      ),
    ],
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
      ~mode: ProbeProj.Settings.window,
      ~font_metrics: FontMetrics.t,
      ~ap_id: option(Id.t),
      ~indicated_call: option(Id.t),
      ~cursor_stack: list(Id.t),
      ~sample_stack: list(Id.t),
      ~caption: string,
    )
    : Node.t => {
  let sample: Language.Sample.t = {
    id: 0,
    syntax_id: Id.invalid,
    value: Language.IdTagged.FreshGrammar.Exp.constructor(caption, None),
    env: Language.Sample.Env.empty,
    call_stack: sample_stack,
    time: 0.0,
    iter: 0,
    origin: Language.Sample.Probe,
  };
  let di: Language.Dynamics.Info.t = {
    samples: [sample],
    dyn_cursor: {
      stack: cursor_stack,
      index: List.length(cursor_stack) - 1,
      pinned_stack: None,
      indicated_call,
      time: None,
      iter: 0,
    },
  };
  ProbeProj.sample_view(
    ~ap_id,
    ~hide_env=true,
    ~settings={
      ...ProbeProj.Settings.s^,
      window: mode,
    },
    ~sort=Sort.Exp, // TODO Talk to andrew about this
    di,
    ProjectorInfo.utility,
    (~text_only) =>
      ProjectorView.flex_code(
        ~font_metrics,
        ~background=false,
        ~is_single_line=Some(),
        ~text_only?,
      ),
    _ => Effect.Ignore,
    _ => Effect.Ignore,
    (0, sample),
  )
  |> div_cs(["sample-group"])
  |> div_cs(["sample-groups"])
  |> div_cs(["live-offside", ProbeProj.Settings.show_window(mode)])
  |> div_cs(["projector", "probe", indicated ? "indicated" : "not-indicated"]);
};

let legend_view = (~font_metrics: FontMetrics.t) => {
  let mode = ProbeProj.Settings.s^.window;
  let legend_sample_view = legend_sample_view(~mode, ~font_metrics);
  div(
    ~attrs=[clss(["legend", "panel"])],
    [
      div(~attrs=[clss(["title"])], [text("Sample Legend")]),
      legend_sample_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid, Id.invalid],
        ~sample_stack=[Id.invalid],
        ~caption="Before",
      ),
      legend_sample_view(
        ~indicated=true,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid],
        ~sample_stack=[Id.invalid],
        ~caption="At Cursor",
      ),
      legend_sample_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid],
        ~sample_stack=[Id.invalid, Id.invalid],
        ~caption="After",
      ),
      legend_sample_view(
        ~indicated=false,
        ~indicated_call=None,
        ~ap_id=Some(Id.invalid),
        ~cursor_stack=[Id.invalid, Id.invalid],
        ~sample_stack=[Id.invalid],
        ~caption="Contains",
      ),
      legend_sample_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.mk()],
        ~sample_stack=[Id.invalid],
        ~caption="Off Cursor",
      ),
      legend_sample_view(
        ~indicated=false,
        ~indicated_call=Some(Id.invalid),
        ~ap_id=None,
        ~cursor_stack=[Id.invalid],
        ~sample_stack=[Id.invalid, Id.invalid],
        ~caption="Inside",
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

let settings = (~explain_this_inject) => {
  div(
    ~attrs=[clss(["settings"])],
    [
      toggle(
        ~tooltip="One or Many Samples",
        ~explain_this_inject,
        ~label1="1",
        ~label2="∞",
        ~active=ProbeProj.Settings.s^.window == Single,
        ~action=ToggleWindow,
      ),
      toggle(
        ~tooltip="Color by Calls or Steps",
        ~explain_this_inject,
        ~label1="👣",
        ~label2="📞",
        ~active=ProbeProj.Settings.s^.sample_base == Steps,
        ~action=ToggleSampleBase,
      ),
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

let sketch_view = (~explain_this_inject): Node.t =>
  details(
    ~attrs=[clss(["sketch"])],
    [
      settings(~explain_this_inject),
      summary(
        ~attrs=[clss(["sketch-toggle"])],
        [div(~attrs=[clss(["sketch-toggle-image"])], [])],
      ),
      div(~attrs=[clss(["sketch-body"])], []),
    ],
  );

let call_cursor_view = (~dyn_cursor: Language.DynCursor.t, ~fancyd) =>
  div(
    ~attrs=[clss(["panel", "call-cursor"])],
    [
      div(~attrs=[clss(["title"])], [text("Dynamic Cursor")]),
      switch (dyn_cursor.indicated_call) {
      | Some(id) when !List.mem(id, dyn_cursor.stack) =>
        div(
          ~attrs=[Attr.classes(["indicated-call", "not-in-stack"])],
          [fancyd(id)],
        )
      | _ => div([])
      },
      div(
        ~attrs=[clss(["stack"])],
        List.mapi(
          (i, id) =>
            div(
              ~attrs=[
                Attr.classes([
                  i == dyn_cursor.index ? "is-index" : "not",
                  i > dyn_cursor.index ? "after-index" : "not",
                  List.mem(id, dyn_cursor.stack)
                  && Some(id) == dyn_cursor.indicated_call
                    ? "indicated-call" : "not",
                ]),
              ],
              [fancyd(id)],
            ),
          dyn_cursor.stack |> List.rev,
        ),
      ),
    ],
  );

type probe_type =
  | Manual(Base.projector)
  | Auto(list((Id.t, Base.projector)));

let prep_refractors =
    (~refractors: Zipper.Refractor.t, ~info_map, ~syntax: CachedSyntax.t) => {
  let manuals =
    refractors.manuals
    |> Id.Map.to_list
    |> List.map(((id, p)) => (id, Manual(p)));
  let autos =
    refractors.autos
    |> List.map(id => {
         let ids = Refractors.ids_from_term(~syntax, ~info_map, id);
         let pairs =
           List.filter_map(
             id =>
               switch (Id.Map.find_opt(id, refractors.ephemerals)) {
               | Some(p) => Some((id, p))
               | None => None
               },
             ids,
           );
         (id, Auto(pairs));
       });
  List.concat([manuals, autos])
  |> sort_ids_by_measurement(~measured=syntax.measured);
};

type refractor_group = {
  top_pat: option(Language.Pat.t),
  entries: list((Id.t, probe_type)),
};

let top_level_pattern =
    (~info_map: Language.Statics.Map.t, ~id: Id.t): option(Language.Pat.t) =>
  switch (StaticsBase.let_definition_path(~statics=info_map, ~id)) {
  | [pat, ..._] => Some(pat)
  | _ => None
  };

let same_top_level =
    (left: option(Language.Pat.t), right: option(Language.Pat.t)): bool =>
  switch (left, right) {
  | (Some(lpat), Some(rpat)) => Language.Pat.equal(lpat, rpat)
  | (None, None) => true
  | _ => false
  };

let push_group =
    (
      ~label: option(Language.Pat.t),
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
    (~info_map: Language.Statics.Map.t, entries: list((Id.t, probe_type)))
    : list(refractor_group) => {
  let rec loop =
          (
            remaining: list((Id.t, probe_type)),
            current_label: option(Language.Pat.t),
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
      let label: option(Language.Pat.t) = top_level_pattern(~info_map, ~id);
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
  | (id, Manual(_projector)) => fancyd(id)
  | (_id, Auto(pairs)) =>
    let ephemerals =
      List.filter_map(
        ((pair_id: Id.t, _projector: Base.projector)) => fancyd(pair_id),
        pairs,
      );
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
        ~text_only=None,
        Language.Grammar.Pat(pat),
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
      ~info_map: Language.Statics.Map.t,
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

let print_string = (probes: Language.Sample.Map.t) => {
  let collect_print_samples =
      (probes: Language.Sample.Map.t): list(Language.Sample.t) =>
    Id.Map.fold(
      (_, samples, acc) =>
        List.fold_left(
          (acc, sample) =>
            sample.Language.Sample.origin == Language.Sample.Print
              ? [sample, ...acc] : acc,
          acc,
          samples,
        ),
      probes,
      [],
    );

  let collect_print_outputs = (probes: Language.Sample.Map.t): list(string) =>
    collect_print_samples(probes)
    |> List.sort((a, b) =>
         Int.compare(a.Language.Sample.iter, b.Language.Sample.iter)
       )
    |> List.map(sample =>
         sample.Language.Sample.value
         |> ExpToSegment.exp_to_segment(
              ~settings=
                ExpToSegment.Settings.of_core(
                  ~inline=true,
                  Language.CoreSettings.off,
                ),
            )
         |> Printer.of_segment(~holes="")
       );

  let print_summary = (probes: Language.Sample.Map.t): option(string) =>
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

let printarium = (~explain_this_inject, ~editor: CodeEditable.Model.t) => [
  div(
    ~attrs=[clss(["header"])],
    [
      div(
        ~attrs=[clss(["main-title"])],
        [text("Printarium"), mode_toggle(~explain_this_inject)],
      ),
    ],
  ),
  div(
    ~attrs=[clss(["panel", "prints"])],
    [
      //div(~attrs=[clss(["title"])], [text("Prints")]),
      div(
        ~attrs=[clss(["body", "code"])],
        [
          switch (print_string(editor.dynamics)) {
          | Some(summary) => text(summary)
          | None => text("No print outputs")
          },
        ],
      ),
    ],
  ),
];

let probearium =
    (~globals: Globals.t, ~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  let refractor_data =
    ProjectorView.Model.mk(
      Id.Map.union(
        (_, _, b) => Some(b),
        editor.editor.state.zipper.refractors.manuals,
        editor.editor.state.zipper.refractors.ephemerals,
      ),
      editor.editor.syntax.measured,
      editor.editor.syntax.term_data,
      editor.editor.syntax.selection_ids,
      Indicated.piece(editor.editor.state.zipper),
      editor.statics.info_map,
      editor.dynamics,
      editor.editor.state.zipper.refractors.dyn_cursor,
      true,
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
    sketch_view(~explain_this_inject),
    call_cursor_view(~dyn_cursor=refractors.dyn_cursor, ~fancyd=id =>
      fancy(
        ~refractor_data,
        ~info_map=editor.statics.info_map,
        ~default=None, /*Some([Example.exp("<In Builtin>")]),*/
        ~globals,
        id,
      )
      |> Option.value(~default=div([]))
    ),
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
