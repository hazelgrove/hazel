open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* Progressive probe controls embedded at the top of the tutorial's Task
 * Reference panel. As the tutorial introduces a probe feature, the matching
 * control/quick-ref row switches on for that slide and stays on for every
 * later slide (strictly additive).
 *
 * This is a deliberately thin, throwaway-friendly layer: it reuses the
 * module-level helpers in [ProbeSidebar] (quick_ref_row, kbd, arrow_icon,
 * legend_view, printarium) rather than modifying that file, and the two
 * toggles are copied from ProbeSidebar.toggle_controls_view so they drive the
 * exact same *global* state (so they stay in sync with the real probe
 * sidebar for free). */

/* The set of probe affordances that can appear in the strip. */
type feat =
  /* toggles */
  | AutoProbe
  | SamplesToggle
  /* slide-20 print console mode switch */
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

let mem = (flags: list(feat), f: feat) => List.mem(f, flags);
let console_enabled = (flags: list(feat)) => mem(flags, Console);

/* ------------------------------------------------------------------ */
/* Per-slide flag table (explicit, accumulated; keyed by module_name).
 *
 * Each `sNN` is the full accumulated set as of slide NN; later slides reuse
 * the previous set with `@ [..new..]`, so the monotonic "strictly add"
 * intent is visible at a glance and language-only slides simply inherit.
 * Edit these to retune what the sidebar shows where. */
let s01 = [AddProbe];
let s03 = s01;
let s04 = s03 @ [SeeVars];
let s08 = s04 @ [IconEmpty];
let s10 = s08 @ [SamplesToggle, NavSamples];
let s11 = s10 @ [FocusProbe, IconOutsideFocus];
let s12 = s11 @ [AutoProbe];
let s13 = s12 @ [Resize, ExpandProbe];
let s16 = s13 @ [Pin, IconPinHidden];
let s17 = s16 @ [StepInto];
let s20 = s17 @ [Console];
/* Legend shows only on the colors slide: color schemes are otherwise
 * Simple, so the legend would spend sidebar space explaining colors
 * that are not in use. */
let s36 = s20 @ [Legend];

let flags_of_slide = (module_name: string): list(feat) =>
  switch (module_name) {
  | "TuGen_01ArithmeticAndHoles"
  | "TuGen_02ParserAndBackpack" => s01
  | "TuGen_03Probes" => s03
  | "TuGen_04VariablesAndExploring"
  | "TuGen_05TuplesAndRecords"
  | "TuGen_07IfExpressions" => s04
  | "TuGen_08CaseAndEmpty"
  | "TuGen_09VariantsWithData" => s08
  | "TuGen_10FunctionsAndManySamples" => s10
  | "TuGen_11AligningSamples" => s11
  | "TuGen_12AutoProbe" => s12
  | "TuGen_13BiggerValues"
  | "TuGen_14Map"
  | "TuGen_15Fold" => s13
  | "TuGen_16Pin" => s16
  | "TuGen_17StepInto"
  | "TuGen_19WritingStrings" => s17
  | "TuGen_20Print"
  | "TuGen_21DebuggingWarmup" => s20
  | "TuGen_23GreenhouseArena"
  | "TuGen_24ModelAndUpdate" => s20
  | "TuGen_36SampleColors" => s36
  /* Study task slides (26+): the full kit. */
  | "TuGen_26TaskGroveName"
  | "TuGen_26bTaskDewLedger"
  | "TuGen_27TaskGrowthPlotter"
  | "TuGen_28TaskLogCleaner"
  | "TuGen_29TaskPlantingBug"
  | "TuGen_30TaskRunningSum"
  | "TuGen_31TaskHarvestStreak"
  | "TuGen_32TaskCropPlotter"
  | "TuGen_33TaskGardenSpirit"
  | "TuGen_34TaskWateringTimer"
  | "TuGen_35TaskWateringFormula" => s20
  /* Unlisted slides (the intro, text-only transitions) show no strip:
   * nothing has been introduced there. */
  | _ => []
  };

/* The features INTRODUCED by each slide (the delta the accumulated sNN
 * lists encode): rows/controls in this set render highlighted (qr-new)
 * on that slide, so the strip draws the eye exactly when the prompt says
 * "a toggle just appeared". */
let new_flags_of_slide = (module_name: string): list(feat) =>
  switch (module_name) {
  | "TuGen_01ArithmeticAndHoles" => [AddProbe]
  | "TuGen_04VariablesAndExploring" => [SeeVars]
  | "TuGen_13BiggerValues" => [Resize, ExpandProbe]
  | "TuGen_08CaseAndEmpty" => [IconEmpty]
  | "TuGen_10FunctionsAndManySamples" => [SamplesToggle, NavSamples]
  | "TuGen_11AligningSamples" => [FocusProbe, IconOutsideFocus]
  | "TuGen_12AutoProbe" => [AutoProbe]
  | "TuGen_16Pin" => [Pin, IconPinHidden]
  | "TuGen_17StepInto" => [StepInto]
  | "TuGen_20Print" => [Console]
  | "TuGen_36SampleColors" => [Legend]
  | _ => []
  };

/* ------------------------------------------------------------------ */
/* Toggles, copied from ProbeSidebar.toggle_controls_view so we can show them
 * individually. Both drive global state (autoprobe_mode / ProbeProj window),
 * so they mirror the real probe sidebar's toggles. */

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
          ProbeSidebar.kbd(Util.Os.is_mac^ ? {js|⌘P|js} : "Ctrl+P"),
        ],
      ),
      div(
        ~attrs=[clss(["segmented-control"])],
        [
          segment("Off", Off),
          segment("Caret", Caret),
          segment("All", All),
        ],
      ),
      div(
        ~attrs=[clss(["legend-tooltip"])],
        [
          text(
            "Off, follow the cursor's definition (Caret), or probe the whole program (All).",
          ),
        ],
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

/* ------------------------------------------------------------------ */
/* Quick Reference: render only the enabled rows, grouped as in the probe
 * sidebar (Actions / Navigation / Focus), dropping empty groups and the
 * dividers that would surround them. */

let qr_row = (~meta, ~new_flags: list(feat), f: feat): option(Node.t) => {
  let row_clss = mem(new_flags, f) ? ["qr-new"] : [];
  let row =
    switch (f) {
    | AddProbe =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~shortcut=meta ++ "E",
        ~badge_cls="qr-cmd-e",
        "Add/remove probe",
        [text("Right-click term")],
      )
    | SeeVars =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut="/",
        ~badge_cls="qr-when-focused",
        "See env/args",
        [text("Alt-click sample")],
      )
    | Pin =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut="P",
        ~badge_cls="qr-when-focused",
        "Pin call",
        [text({js|Right-click sample › Pin|js})],
      )
    | StepInto =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|↩|js},
        ~badge_cls="qr-when-focused",
        "Step into call",
        [text({js|Right-click sample › Step|js})],
      )
    | NavSamples =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|←|js},
        ~click_shortcut2={js|→|js},
        ~badge_cls="qr-when-focused",
        "Navigate samples",
        [
          text("Click "),
          ProbeSidebar.arrow_icon(`Left),
          ProbeSidebar.arrow_icon(`Right),
        ],
      )
    | NavProbes =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|↑|js},
        ~click_shortcut2={js|↓|js},
        ~badge_cls="qr-when-focused",
        "Navigate probes",
        [text("Click sample")],
      )
    | Resize =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut={js|⇧←|js},
        ~click_shortcut2={js|⇧→|js},
        ~badge_cls="qr-when-focused",
        "Resize sample",
        [text("Drag sample")],
      )
    | ExpandProbe =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut=meta ++ {js|↓|js},
        ~click_shortcut2=meta ++ {js|↑|js},
        ~badge_cls="qr-when-focused",
        "Expand probe",
        [text("Click "), ProbeSidebar.arrow_icon(`Down)],
      )
    | FocusProbe =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~shortcut=meta ++ {js|↩|js},
        ~badge_cls="qr-focus-probe",
        "Focus probe",
        [text("Click sample")],
      )
    | FocusEditor =>
      ProbeSidebar.quick_ref_row(
        ~row_clss,
        ~click_shortcut=meta ++ {js|↩|js},
        ~click_shortcut2="Esc",
        ~badge_cls="qr-when-focused",
        "Focus editor",
        [text("Click editor")],
      )
    | _ => Node.text("")
    };
  /* Only the quick-ref-row feats produce a row; toggles/icons/legend don't. */
  switch (f) {
  | AddProbe
  | SeeVars
  | Pin
  | StepInto
  | NavSamples
  | NavProbes
  | Resize
  | ExpandProbe
  | FocusProbe
  | FocusEditor => Some(row)
  | _ => None
  };
};

let qr_table_rows =
    (~new_flags: list(feat), flags: list(feat)): list(Node.t) => {
  let meta = Util.Os.is_mac^ ? {js|⌘|js} : "Ctrl+";
  let group = (feats: list(feat)) =>
    feats
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
    first
    @ List.concat_map(g => [ProbeSidebar.quick_ref_divider, ...g], rest)
  };
};

let quick_ref_panel =
    (~new_flags: list(feat), flags: list(feat)): list(Node.t) => {
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
        /* can-probe/has-probe/has-manual keep every row at full opacity:
         * the strip is a reference, not driven by the indicated term. */
        ~attrs=[
          clss([
            "quick-ref",
            "panel",
            "can-probe",
            "has-probe",
            "has-manual",
          ]),
        ],
        [
          div(~attrs=[clss(["title"])], [text("Quick Reference")]),
          Node.table(~attrs=[clss(["qr-table"])], rows),
        ]
        @ (icons == [] ? [] : [div(~attrs=[clss(["qr-icons"])], icons)]),
      ),
    ];
};

/* The strip body: toggles panel + quick reference + (optional) color legend.
 * Does NOT include the console switch/body (handled separately so the panel
 * can swap its whole content into the print console). */
let strip_view =
    (
      ~globals: Globals.t,
      ~explain_this_inject,
      ~flags: list(feat),
      ~new_flags: list(feat)=[],
      (),
    )
    : list(Node.t) => {
  let toggles =
    (
      mem(flags, AutoProbe)
        ? [auto_probe_toggle(~globals, ~is_new=mem(new_flags, AutoProbe))]
        : []
    )
    @ (
      mem(flags, SamplesToggle)
        ? [
          samples_toggle(
            ~explain_this_inject,
            ~is_new=mem(new_flags, SamplesToggle),
          ),
        ]
        : []
    );
  let toggle_panel =
    toggles == []
      ? [] : [div(~attrs=[clss(["toggle-controls", "panel"])], toggles)];
  let legend =
    mem(flags, Legend)
      ? [ProbeSidebar.legend_view(~globals, ~explain_this_inject)] : [];
  toggle_panel @ quick_ref_panel(~new_flags, flags) @ legend;
};

/* ------------------------------------------------------------------ */
/* Slide-20 "Console": a Reference / Console mode switch at the top of the
 * panel. Console mode replaces the whole panel body with the print console.
 * State is a module-level ref (matching ProbeSidebar's own ref-based mode
 * switches); explain_this_inject pokes a re-render. */

let console_mode = ref(false); /* false = Reference, true = Console */

let console_header = (~explain_this_inject): Node.t => {
  let switch_to = (target, _) => {
    console_mode := target;
    explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
  };
  let is_ref = ! console_mode^;
  div(
    ~attrs=[clss(["main-title"])],
    [
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_ref ? ["active"] : ["inactive"]))]
          @ (is_ref ? [] : [Attr.on_pointerdown(switch_to(false))]),
        [text("Reference")],
      ),
      span(~attrs=[clss(["mode-separator"])], [text(" / ")]),
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_ref ? ["inactive"] : ["active"]))]
          @ (is_ref ? [Attr.on_pointerdown(switch_to(true))] : []),
        [text("Console")],
      ),
    ],
  );
};

/* The print console body: reuse ProbeSidebar.printarium but drop its leading
 * Probearium/Printarium header (we supply our own Reference/Console switch). */
let console_body =
    (~explain_this_inject, ~editor: CodeEditable.Model.t): list(Node.t) =>
  switch (ProbeSidebar.printarium(~explain_this_inject, ~editor)) {
  | [_header, ...rest] => rest
  | nodes => nodes
  };
