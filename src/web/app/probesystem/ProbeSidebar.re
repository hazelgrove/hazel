open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;
open Haz3lcore;
open Language;

let div_cs = (cls, node) => div(~attrs=[Attr.classes(cls)], [node]);

let legend_sample =
    (
      ~indicated: bool,
      ~mode: Sample.Window.mode,
      ~ap_id: option(Id.t),
      ~indicated_call: option(Id.t),
      ~cursor_stack: CallStack.t,
      ~sample_stack: CallStack.t,
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
    sample_focus: {
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

let kbd = (shortcut: string) =>
  span(~attrs=[clss(["kbd-badge"])], [text(shortcut)]);

let click_kbd = kbd;

let legend_view = (~globals as _: Globals.t, ~explain_this_inject) => {
  let mode = ProbeProj.Settings.s^.window;
  let color_scheme = ProbeProj.Settings.s^.sample_base;
  let focus = Some((10, 20));
  let f: CallStack.frame = {
    id: Id.invalid,
    name: None,
    fn_def_id: None,
  };
  let legend_sample = legend_sample(~mode);
  /* Labels vary by color scheme */
  let (before_label, after_label, contains_label, inside_label) =
    switch (color_scheme) {
    | Calls => ("Above", "Below", "Caller", "Callee")
    | Hybrid
    | StepRange => ("Before", "After", "Contains", "Inside")
    };
  div(
    ~attrs=[clss(["legend", "panel"])],
    [
      div(~attrs=[clss(["title"])], [text("Sample Focus Legend")]),
      legend_item(
        ~tooltip=
          switch (color_scheme) {
          | Calls => "This sample is from a shallower call stack depth than the focus."
          | Hybrid
          | StepRange => "This sample's step range ends before the focus starts."
          },
        legend_sample(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f, f],
          ~sample_stack=[f],
          ~step_range=(0, 5),
          ~focus_step_range=focus,
          ~caption=before_label,
        ),
      ),
      legend_item(
        ~tooltip=
          "This sample is at the current focal position in the call stack.",
        legend_sample(
          ~indicated=true,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f],
          ~sample_stack=[f],
          ~step_range=(10, 20),
          ~focus_step_range=Some((10, 20)),
          ~caption="Focused",
        ),
      ),
      legend_item(
        ~tooltip=
          switch (color_scheme) {
          | Calls => "This sample is from a deeper call stack depth than the focus."
          | Hybrid
          | StepRange => "This sample's step range starts after the focus ends."
          },
        legend_sample(
          ~indicated=false,
          ~ap_id=None,
          ~indicated_call=None,
          ~cursor_stack=[f],
          ~sample_stack=[f, f],
          ~step_range=(25, 30),
          ~focus_step_range=focus,
          ~caption=after_label,
        ),
      ),
      legend_item(
        ~tooltip=
          switch (color_scheme) {
          | Calls => "This sample is from a call site on the focus's call chain (a direct caller)."
          | Hybrid
          | StepRange => "This sample's step range strictly contains the focal range."
          },
        legend_sample(
          ~indicated=false,
          ~indicated_call=None,
          ~ap_id=Some(Id.invalid),
          ~cursor_stack=[f, f],
          ~sample_stack=[f],
          ~step_range=(5, 25),
          ~focus_step_range=focus,
          ~caption=contains_label,
        ),
      ),
      switch (mode) {
      | Single =>
        legend_item(
          ~tooltip=
            "Samples not shown as they are not within the probe focus; click to realign the focus and show them.",
          div(~attrs=[clss(["legend-not-aligned"])], [text({js|⊖|js})]),
        )
      | Many =>
        legend_item(
          ~tooltip=
            "This sample is from a different branch of the call stack than the focus.",
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
            ~caption="Unfocused",
          ),
        )
      },
      legend_item(
        ~tooltip=
          switch (color_scheme) {
          | Calls => "This sample is from a function called from the focal sample (a direct callee)."
          | Hybrid
          | StepRange => "This sample's step range is strictly inside the focus."
          },
        legend_sample(
          ~indicated=false,
          ~indicated_call=Some(Id.invalid),
          ~ap_id=None,
          ~cursor_stack=[f],
          ~sample_stack=[f, f],
          ~step_range=(12, 18),
          ~focus_step_range=focus,
          ~caption=inside_label,
        ),
      ),
      div(~attrs=[clss(["legend-divider"])], []),
      div(~attrs=[clss(["title"])], [text("Sample Color Scheme")]),
      {
        let next_mode: ProbeProj.Settings.sample_base =
          switch (color_scheme) {
          | Calls => Hybrid
          | Hybrid => StepRange
          | StepRange => Calls
          };
        let segment = (label, tooltip, mode) =>
          div(
            ~attrs=[
              clss(["segment"] @ (color_scheme == mode ? ["active"] : [])),
              Attr.on_pointerdown(_ => {
                let target = color_scheme == mode ? next_mode : mode;
                ProbeProj.Settings.go(SetSampleBase(target));
                explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
              }),
            ],
            [
              text(label),
              div(~attrs=[clss(["legend-tooltip"])], [text(tooltip)]),
            ],
          );
        div(
          ~attrs=[clss(["segmented-control"])],
          [
            segment(
              "Calls",
              "Color by call stack relations: relative call depth, callers, callees.",
              Calls,
            ),
            segment(
              "Hybrid",
              "Focus from call stack (green for matching call stack), everything else by evaluation order.",
              Hybrid,
            ),
            segment(
              "Steps",
              "Color by evaluation order: which expressions were evaluated before, after, or around the focus.",
              StepRange,
            ),
          ],
        );
      },
    ],
  );
};

let toggle_controls_view = (~globals: Globals.t, ~explain_this_inject) => {
  let mode = ProbeProj.Settings.s^.window;
  div(
    ~attrs=[clss(["toggle-controls", "panel"])],
    [
      {
        /* Auto Probe toggle */

        let is_on = globals.settings.autoprobe_mode;
        let segment = (label, active) =>
          div(
            ~attrs=[
              clss(["segment"] @ (active ? ["active"] : [])),
              Attr.on_pointerdown(_ =>
                globals.inject_global(Set(AutoprobeMode))
              ),
            ],
            [text(label)],
          );
        div(
          ~attrs=[clss(["toggle-group"])],
          [
            div(
              ~attrs=[clss(["toggle-label"])],
              [
                text("Auto Probe"),
                kbd(Util.Os.is_mac^ ? {js|⌘P|js} : "Ctrl+P"),
              ],
            ),
            div(
              ~attrs=[clss(["segmented-control"])],
              [segment("Off", !is_on), segment("On", is_on)],
            ),
            div(
              ~attrs=[clss(["legend-tooltip"])],
              [
                text(
                  "Automatically probe the definition at the cursor, following as you navigate.",
                ),
              ],
            ),
          ],
        );
      },
      {
        /* Samples toggle */

        let is_single = mode == Single;
        let segment = (label, active) =>
          div(
            ~attrs=[
              clss(["segment"] @ (active ? ["active"] : [])),
              Attr.on_pointerdown(_ => {
                ProbeProj.Settings.go(ToggleWindow);
                explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
              }),
            ],
            [text(label)],
          );
        div(
          ~attrs=[clss(["toggle-group"])],
          [
            div(
              ~attrs=[clss(["toggle-label"])],
              [
                text("Samples"),
                span(
                  ~attrs=[clss(["qr-when-focused", "kbd-badge"])],
                  [text({js|␣|js})],
                ),
              ],
            ),
            div(
              ~attrs=[clss(["segmented-control"])],
              [segment("One", is_single), segment("Many", !is_single)],
            ),
            div(
              ~attrs=[clss(["legend-tooltip"])],
              [text("Show at most one sample per probe, or all at once.")],
            ),
          ],
        );
      },
    ],
  );
};

let toggle =
    (~tooltip, ~explain_this_inject, ~label1, ~label2, ~active, ~action) =>
  Widgets.toggle_named(
    ~name=tooltip,
    active ? label1 : label2,
    active,
    _ => {
      ProbeProj.Settings.go(action);
      explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
    },
  );

let settings = (~globals as _: Globals.t, ~explain_this_inject) => {
  div(
    ~attrs=[clss(["settings"])],
    [
      toggle(
        ~tooltip="Samples Before/Above Focus",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.before_cutoff == None,
        ~action=ToggleBeforeCutoff,
      ),
      toggle(
        ~tooltip="Samples After/Below Focus",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.after_cutoff == None,
        ~action=ToggleAfterCutoff,
      ),
      toggle(
        ~tooltip="Callsites containing Focus",
        ~explain_this_inject,
        ~label1="∞",
        ~label2="1",
        ~active=ProbeProj.Settings.s^.caller_cutoff == None,
        ~action=ToggleCallerCutoff,
      ),
      toggle(
        ~tooltip="Samples Inside Call at Focus",
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

let eval_mode_ref = ref(Manual);
let cached_print_entries = ref(None: option(list(print_entry)));

let mode_title = (~explain_this_inject) => {
  let switch_mode = _ => {
    mode := mode^ == Probes ? Prints : Probes;
    explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
  };
  let is_probes = mode^ == Probes;
  div(
    ~attrs=[clss(["main-title"])],
    [
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_probes ? ["active"] : ["inactive"]))]
          @ (is_probes ? [] : [Attr.on_pointerdown(switch_mode)]),
        [text("Probearium")],
      ),
      span(
        ~attrs=[clss(["mode-separator"] @ (is_probes ? [] : ["prints"]))],
        [text(" / ")],
      ),
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_probes ? ["inactive"] : ["active"]))]
          @ (is_probes ? [Attr.on_pointerdown(switch_mode)] : []),
        [text("Printarium")],
      ),
    ],
  );
};

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
    div(~attrs=[clss(["header"])], [mode_title(~explain_this_inject)]),
    div(
      ~attrs=[clss(["eval-controls"])],
      [run_button(~explain_this_inject, ~editor)],
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

let quick_ref_row =
    (
      ~shortcut=?,
      ~click_shortcut=?,
      ~click_shortcut2=?,
      ~badge_cls=?,
      action: string,
      how: string,
    ) => {
  let wrap_cls = (nodes: list(Node.t)) =>
    switch (badge_cls) {
    | Some(cls) => [span(~attrs=[clss([cls])], nodes)]
    | None => nodes
    };
  let badge_nodes =
    switch (shortcut, click_shortcut) {
    | (Some(s), _) => wrap_cls([kbd(s)])
    | (_, Some(s)) =>
      wrap_cls(
        [click_kbd(s)]
        @ (
          switch (click_shortcut2) {
          | Some(s2) => [click_kbd(s2)]
          | None => []
          }
        ),
      )
    | _ => []
    };
  Node.tr([
    Node.td(~attrs=[clss(["qr-action"])], [text(action)]),
    Node.td(
      ~attrs=[clss(["qr-how"])],
      [span(~attrs=[clss(["qr-how-text"])], [text(how)])] @ badge_nodes,
    ),
  ]);
};

let quick_ref_divider =
  Node.tr([
    Node.td(
      ~attrs=[Attr.create("colspan", "2"), clss(["qr-divider"])],
      [],
    ),
  ]);

let quick_ref_view =
    (
      ~indicated_can_probe: bool,
      ~indicated_has_probe: bool,
      ~indicated_has_manual: bool,
    ) => {
  let meta = Util.Os.is_mac^ ? {js|⌘|js} : "Ctrl+";
  div(
    ~attrs=[
      clss(
        ["quick-ref", "panel"]
        @ (indicated_can_probe ? ["can-probe"] : [])
        @ (indicated_has_probe ? ["has-probe"] : [])
        @ (indicated_has_manual ? ["has-manual"] : []),
      ),
    ],
    [
      div(~attrs=[clss(["title"])], [text("Quick Reference")]),
      Node.table(
        ~attrs=[clss(["qr-table"])],
        [
          /* Group 1: Actions */
          quick_ref_row(
            ~shortcut=meta ++ "E",
            ~badge_cls="qr-cmd-e",
            "Add/remove probe",
            "Right-click term",
          ),
          quick_ref_row(
            ~click_shortcut="/",
            ~badge_cls="qr-when-focused",
            "See env/args",
            "Hover over sample",
          ),
          quick_ref_row(
            ~click_shortcut="P",
            ~badge_cls="qr-when-focused",
            "Pin call",
            {js|Click sample › Pin|js},
          ),
          quick_ref_row(
            ~click_shortcut={js|↩|js},
            ~badge_cls="qr-when-focused",
            "Step into call",
            {js|Click sample › Step|js},
          ),
          /* Group 2: Navigation */
          quick_ref_divider,
          quick_ref_row(
            ~click_shortcut={js|←|js},
            ~click_shortcut2={js|→|js},
            ~badge_cls="qr-when-focused",
            "Navigate samples",
            {js|Click ◀▶ sample|js},
          ),
          quick_ref_row(
            ~click_shortcut={js|↑|js},
            ~click_shortcut2={js|↓|js},
            ~badge_cls="qr-when-focused",
            "Navigate probes",
            "Click sample",
          ),
          quick_ref_row(
            ~click_shortcut={js|⇧←|js},
            ~click_shortcut2={js|⇧→|js},
            ~badge_cls="qr-when-focused",
            "Resize sample",
            "Drag sample",
          ),
          /* Group 3: Focus */
          quick_ref_divider,
          quick_ref_row(
            ~shortcut=meta ++ {js|↩|js},
            ~badge_cls="qr-focus-probe",
            "Focus probe",
            "Click sample",
          ),
          quick_ref_row(
            ~click_shortcut=meta ++ {js|↩|js},
            ~click_shortcut2="Esc",
            ~badge_cls="qr-when-focused",
            "Focus editor",
            "Click editor",
          ),
        ],
      ),
      div(
        ~attrs=[clss(["qr-icons"])],
        [
          div([text({js|∅ = never evaluated|js})]),
          div([text({js|⍟ = hidden by pin|js})]),
          div([text({js|⊖ = outside focus|js})]),
        ],
      ),
    ],
  );
};

let probearium =
    (~globals: Globals.t, ~explain_this_inject, ~editor: CodeEditable.Model.t) => {
  let z = editor.editor.state.zipper;
  let indicated_id = Indicated.index(z);
  let indicated_has_probe =
    switch (indicated_id) {
    | Some(id) =>
      List.exists(((rid, _)) => rid == id, z.refractors.manuals)
      || Id.Map.mem(id, z.refractors.multis.ephemerals)
    | None => false
    };
  let indicated_has_manual =
    switch (indicated_id) {
    | Some(id) => List.exists(((rid, _)) => rid == id, z.refractors.manuals)
    | None => false
    };
  let indicated_can_probe =
    switch (indicated_id) {
    | Some(id) =>
      switch (Statics.Map.lookup(id, editor.statics.info_map)) {
      | Some(InfoExp(_) | InfoPat(_)) => true
      | _ => false
      }
    | None => false
    };
  [
    div(~attrs=[clss(["header"])], [mode_title(~explain_this_inject)]),
    toggle_controls_view(~globals, ~explain_this_inject),
    quick_ref_view(
      ~indicated_can_probe,
      ~indicated_has_probe,
      ~indicated_has_manual,
    ),
    legend_view(~globals, ~explain_this_inject),
    sketch_view(~globals, ~explain_this_inject),
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
    ~attrs=[Attr.id("probe-sidebar")],
    mode^ == Probes
      ? probearium(~globals, ~explain_this_inject, ~editor)
      : printarium(~explain_this_inject, ~editor),
  );
};
