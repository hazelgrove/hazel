open Haz3lcore;

/* PER-SLIDE INITIAL PROBE SETTINGS
 *
 * Lets a tutorial slide specify an *initial value* for some probe knobs,
 * applied when you navigate into that slide. The user can still change
 * everything afterward via the normal toggles — this only sets a starting
 * point other than the default, where a slide wants one.
 *
 * This is the value-twin of TutorialProbeStrip.flags_of_slide (which says
 * *which controls to show* per slide); here we say *what initial state* to
 * put them in. Both are keyed by the slide's module_name so the per-slide
 * tuning lives in one obvious place.
 *
 * Each field is `option`: None means "leave whatever it currently is"
 * (so unannotated slides are a no-op and inherit the user's state); Some x
 * forces x on entry. The knobs map onto:
 *   - autoprobe : globals.settings.autoprobe_mode  (Settings state — set via
 *                 the ~set_autoprobe callback the caller threads in)
 *   - samples   : ProbeProj.Settings window         (global ref)
 *   - colors    : ProbeProj.Settings sample_base    (global ref)
 *
 * To add another knob: if it's a ProbeProj.Settings ref field, add an
 * option here and a `go(...)` line in `apply`; if it's Settings state, add
 * a callback like ~set_autoprobe. */

type t = {
  autoprobe: option(AutoProbe.t),
  samples: option(Language.Sample.Window.mode),
  colors: option(ProbeProj.Settings.sample_base),
};

let none: t = {
  autoprobe: None,
  samples: None,
  colors: None,
};

/* Per-slide table (keyed by module_name, mirroring flags_of_slide). Edit
 * these to give a slide its starting probe settings on entry.
 *
 * Policy: auto-probe is OFF for the basics (slides 01-12: not introduced
 * until 12, where the user turns it on), then ON (All, whole program) for the
 * slides after, so probes appear without manual placement. Cmd/Ctrl+P toggles
 * All; the Caret follow-the-cursor mode is a performance fallback, set via the
 * toggle and never pre-set here. Overrides:
 *   - 20 Print: everything off, the print console is the focus -> Off
 *   - 36 (colors) also needs the Hybrid color scheme so colors show
 *     -> All + Hybrid
 * Slides not listed inherit the current state (no-op). */
let of_slide = (module_name: string): t =>
  switch (module_name) {
  /* Slides 01-12: auto-probe off (not introduced until 12, where the user
   * turns it on, so it starts off there too). */
  | "TuGen_01ArithmeticAndHoles"
  | "TuGen_02ParserAndBackpack"
  | "TuGen_03Probes"
  | "TuGen_04VariablesAndExploring"
  | "TuGen_05TuplesAndRecords"
  | "TuGen_06BiggerValues"
  | "TuGen_07IfExpressions"
  | "TuGen_08CaseAndEmpty"
  | "TuGen_09VariantsWithData"
  | "TuGen_10FunctionsAndManySamples"
  | "TuGen_11AligningSamples"
  | "TuGen_12AutoProbe" => {
      ...none,
      autoprobe: Some(Off),
    }
  /* Print: everything off so the print console is the focus. */
  | "TuGen_20Print" => {
      ...none,
      autoprobe: Some(Off),
    }
  /* Everything after auto-probe is introduced: All (whole program).
   * Cmd/Ctrl+P toggles All; Caret is a performance fallback set via the
   * toggle, never pre-set here. */
  | "TuGen_14Map"
  | "TuGen_15Fold"
  | "TuGen_16Pin"
  | "TuGen_17StepInto"
  | "TuGen_19WritingStrings"
  | "TuGen_21DebuggingWarmup"
  | "TuGen_23GreenhouseArena"
  | "TuGen_24ModelAndUpdate" => {
      ...none,
      autoprobe: Some(All),
    }
  /* Colors slide: All, plus the Hybrid scheme so the colors actually show. */
  | "TuGen_36SampleColors" => {
      ...none,
      autoprobe: Some(All),
      colors: Some(Hybrid),
    }
  /* Study tasks: writing tasks start with auto-probe All (ambient feedback
   * while building); debugging tasks start Off so the probing strategy is
   * the participant's own choice. */
  | "TuGen_26TaskGroveName"
  | "TuGen_28TaskLogCleaner"
  | "TuGen_30TaskRunningSum"
  | "TuGen_32TaskCropPlotter"
  | "TuGen_35TaskWateringFormula" => {
      ...none,
      autoprobe: Some(All),
    }
  | "TuGen_26bTaskDewLedger"
  | "TuGen_27TaskGrowthPlotter"
  | "TuGen_29TaskPlantingBug"
  | "TuGen_31TaskHarvestStreak"
  | "TuGen_33TaskGardenSpirit"
  | "TuGen_34TaskWateringTimer" => {
      ...none,
      autoprobe: Some(Off),
    }
  | _ => none
  };

/* Apply a slide's initial settings. Samples/colors are global ProbeProj
 * refs (direct side effects); autoprobe is Settings state, so the caller
 * supplies a dispatcher. Autoprobe/samples apply only when specified (Some).
 * Colors ALWAYS apply: a slide that does not pick a scheme knocks the user
 * back to the Simple two-color default, so the full scheme forced on the
 * colors slide (or opted into by the user) does not silently follow them
 * through the rest of the tutorial. */
let apply = (~set_autoprobe: AutoProbe.t => unit, init: t): unit => {
  Option.iter(set_autoprobe, init.autoprobe);
  Option.iter(w => ProbeProj.Settings.go(SetWindow(w)), init.samples);
  let colors = Option.value(init.colors, ~default=ProbeProj.Settings.Simple);
  ProbeProj.Settings.go(SetSampleBase(colors));
};

/* Last slide we applied inits for. Used to fire `apply` exactly once per
 * slide entry: callers pass the current slide's module_name each update;
 * when it differs from what we last applied, we apply and remember it. */
let last_applied: ref(option(string)) = ref(None);

/* Call on each Editors update with the current tutorial slide's
 * module_name (None when not in tutorial mode). Applies the slide's inits
 * the first time we observe a given slide as current — i.e. on entry. */
let maybe_apply_on_change =
    (~set_autoprobe: AutoProbe.t => unit, module_name: option(string)): unit =>
  if (module_name != last_applied^) {
    last_applied := module_name;
    switch (module_name) {
    | Some(name) => apply(~set_autoprobe, of_slide(name))
    | None => ()
    };
  };
