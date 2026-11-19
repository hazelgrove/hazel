open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* Controls and reference entries shared by the probe sidebar and tutorials. */
type feature =
  /* toggles */
  | AutoProbe
  | SamplesToggle
  /* print console mode switch */
  | Console
  /* quick-ref: actions */
  | AddProbe
  | SeeVars
  | Pin
  | StepInto
  /* quick-ref: navigation */
  | NavSamples
  | NavProbes
  | Resize
  /* quick-ref: focus */
  | ExpandProbe
  | FocusProbe
  | FocusEditor
  /* quick-ref icon legend */
  | IconEmpty
  | IconPinHidden
  | IconOutsideFocus
  /* color legend panel */
  | Legend;

let mem = (flags: list(feature), f: feature) => List.mem(f, flags);

let kbd = (shortcut: string) =>
  span(~attrs=[clss(["kbd-badge"])], [text(shortcut)]);

let arrow_icon =
    (
      direction: [
        | `Left
        | `Right
        | `Up
        | `Down
      ],
    ) => {
  let dir_cls =
    switch (direction) {
    | `Left => "left"
    | `Right => "right"
    | `Up => "up"
    | `Down => "down"
    };
  span(~attrs=[clss(["arrow-icon", dir_cls])], []);
};

let auto_probe_toggle = (~globals: Globals.t, ~is_new: bool) => {
  let mode_now = globals.settings.autoprobe_mode;
  let segment = (label, mode: AutoProbe.t) =>
    div(
      ~attrs=[
        clss(["segment"] @ (mode == mode_now ? ["active"] : [])),
        Attr.on_pointerdown(_ =>
          globals.inject_global(Set(SetAutoprobe(mode)))
        ),
      ],
      [text(label)],
    );
  div(
    ~attrs=[clss(["toggle-group"] @ (is_new ? ["qr-new"] : []))],
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
        /* Caret (follow-the-cursor) mode still exists in the logic but is
         * hidden from the toggle for now. */
        [segment("Off", Off), segment("All", All)],
      ),
      div(
        ~attrs=[clss(["legend-tooltip"])],
        [text("Off, or probe the whole program (All).")],
      ),
    ],
  );
};

let samples_toggle = (~explain_this_inject, ~is_new: bool) => {
  let is_single = ProbeProj.Settings.s^.window == Single;
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
    ~attrs=[clss(["toggle-group"] @ (is_new ? ["qr-new"] : []))],
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
};

let quick_ref_row =
    (
      ~shortcut=?,
      ~click_shortcut=?,
      ~click_shortcut2=?,
      ~badge_cls=?,
      ~row_clss: list(string)=[],
      action: string,
      how: list(Node.t),
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
        [kbd(s)]
        @ (
          switch (click_shortcut2) {
          | Some(s2) => [kbd(s2)]
          | None => []
          }
        ),
      )
    | _ => []
    };
  Node.tr(
    ~attrs=[clss(row_clss)],
    [
      Node.td(~attrs=[clss(["qr-action"])], [text(action)]),
      Node.td(
        ~attrs=[clss(["qr-how"])],
        [span(~attrs=[clss(["qr-how-text"])], how)] @ badge_nodes,
      ),
    ],
  );
};

let quick_ref_divider =
  Node.tr([
    Node.td(
      ~attrs=[Attr.create("colspan", "2"), clss(["qr-divider"])],
      [],
    ),
  ]);

let qr_row = (~meta, ~new_flags: list(feature), f: feature): option(Node.t) => {
  let row_clss = mem(new_flags, f) ? ["qr-new"] : [];
  switch (f) {
  | AddProbe =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~shortcut=meta ++ "E",
        ~badge_cls="qr-cmd-e",
        "Add/remove probe",
        [text("Right-click term")],
      ),
    )
  | SeeVars =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut="/",
        ~badge_cls="qr-when-focused",
        "See env/args",
        [text("Alt-click sample")],
      ),
    )
  | Pin =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut="P",
        ~badge_cls="qr-when-focused",
        "Pin call",
        [text({js|Right-click sample › Pin|js})],
      ),
    )
  | StepInto =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|↩|js},
        ~badge_cls="qr-when-focused",
        "Step into call",
        [text({js|Right-click sample › Step|js})],
      ),
    )
  | NavSamples =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|←|js},
        ~click_shortcut2={js|→|js},
        ~badge_cls="qr-when-focused",
        "Navigate samples",
        [text("Click "), arrow_icon(`Left), arrow_icon(`Right)],
      ),
    )
  | NavProbes =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|↑|js},
        ~click_shortcut2={js|↓|js},
        ~badge_cls="qr-when-focused",
        "Navigate probes",
        [text("Click sample")],
      ),
    )
  | Resize =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|⇧←|js},
        ~click_shortcut2={js|⇧→|js},
        ~badge_cls="qr-when-focused",
        "Resize sample",
        [text("Drag sample")],
      ),
    )
  | ExpandProbe =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut=meta ++ {js|↓|js},
        ~click_shortcut2=meta ++ {js|↑|js},
        ~badge_cls="qr-when-focused",
        "Expand probe",
        [text("Click "), arrow_icon(`Down)],
      ),
    )
  | FocusProbe =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~shortcut=meta ++ {js|↩|js},
        ~badge_cls="qr-focus-probe",
        "Focus probe",
        [text("Click sample")],
      ),
    )
  | FocusEditor =>
    Some(
      quick_ref_row(
        ~row_clss,
        ~click_shortcut=meta ++ {js|↩|js},
        ~click_shortcut2="Esc",
        ~badge_cls="qr-when-focused",
        "Focus editor",
        [text("Click editor")],
      ),
    )
  | _ => None
  };
};

let qr_table_rows =
    (~new_flags: list(feature), flags: list(feature)): list(Node.t) => {
  let meta = Util.Os.is_mac^ ? {js|⌘|js} : "Ctrl+";
  let group = (features: list(feature)) =>
    features
    |> List.filter(f => mem(flags, f))
    |> List.filter_map(qr_row(~meta, ~new_flags));
  let groups =
    [
      group([AddProbe, SeeVars, Pin, StepInto]),
      group([NavSamples, NavProbes, Resize]),
      group([ExpandProbe, FocusProbe, FocusEditor]),
    ]
    |> List.filter(g => g != []);
  switch (groups) {
  | [] => []
  | [first, ...rest] =>
    first @ List.concat_map(g => [quick_ref_divider, ...g], rest)
  };
};

let quick_ref_panel =
    (
      ~context_clss: list(string),
      ~new_flags: list(feature)=[],
      flags: list(feature),
    )
    : list(Node.t) => {
  let rows = qr_table_rows(~new_flags, flags);
  let icon = (f, glyph) =>
    mem(flags, f)
      ? [
        div(
          ~attrs=[clss(mem(new_flags, f) ? ["qr-new"] : [])],
          [text(glyph)],
        ),
      ]
      : [];
  let icons =
    icon(IconEmpty, {js|∅ = never evaluated|js})
    @ icon(IconPinHidden, {js|⍟ = hidden by pin|js})
    @ icon(IconOutsideFocus, {js|⊖ = outside focus|js});
  rows == [] && icons == []
    ? []
    : [
      div(
        ~attrs=[clss(["quick-ref", "panel"] @ context_clss)],
        [
          div(~attrs=[clss(["title"])], [text("Quick Reference")]),
          Node.table(~attrs=[clss(["qr-table"])], rows),
        ]
        @ (icons == [] ? [] : [div(~attrs=[clss(["qr-icons"])], icons)]),
      ),
    ];
};
