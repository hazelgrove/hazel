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
 * Policy: auto-probe is OFF for the basics (not introduced until the
 * auto-probe slide, where the user turns it on), then ON (All, whole
 * program) for the slides after, so probes appear without manual placement.
 * Cmd/Ctrl+P toggles All; the Caret follow-the-cursor mode is a performance
 * fallback, currently hidden from the toggles and never pre-set here.
 *
 * SAMPLES WINDOW: every slide also starts in a deliberate window mode.
 * `samples` is always applied (default Single, see `apply`), so the mode
 * never silently carries over from a prior slide. Most slides start Single
 * (the calm, one-sample-at-a-time view); a slide opts into Many only when
 * its lesson needs several samples visible at once (a growing accumulator
 * column, "ten samples is a lot" motivating pin, before/after colors, etc.).
 * Task slides always start Single: the participant switches if they want
 * to. Samples-per-call stays Single on purpose -- that is where Many is
 * first introduced via Space, so toggling is the lesson.
 *
 * Overrides:
 *   - print-statements: everything off, the print console is the focus
 *   - bonus-sample-colors also needs the Hybrid color scheme so colors
 *     show -> All + Hybrid + Many
 * Slides not listed inherit auto-probe but start Single. */
let of_slide = (module_name: string): t =>
  switch (module_name) {
  /* Early slides through auto-probe: off (not introduced until the
   * auto-probe slide, where the user turns it on themselves, so it starts
   * off there too); single window. */
  | "TuGen_ArithmeticAndHoles"
  | "TuGen_TheBackpack"
  | "TuGen_AddingAndRemovingProbes"
  | "TuGen_EnvironmentExplorer"
  | "TuGen_TuplesAndRecords"
  | "TuGen_IfExpressions"
  | "TuGen_CaseExpressions"
  | "TuGen_ConstructorsWithData"
  | "TuGen_SamplesPerCall"
  | "TuGen_AligningSamples"
  | "TuGen_AutoProbe" => {
      ...none,
      autoprobe: Some(Off),
    }
  /* Bigger values: one hand-placed probe with three calls, starting in
   * many mode so the squeeze (and the three remedies) is the lesson. */
  | "TuGen_ReadingBiggerValues" => {
      ...none,
      autoprobe: Some(Off),
      samples: Some(Many),
    }
  /* Print: everything off so the print console is the focus. */
  | "TuGen_PrintStatements" => {
      ...none,
      autoprobe: Some(Off),
    }
  /* Auto-probe introduced: All (whole program), single window. */
  | "TuGen_Map" => {
      ...none,
      autoprobe: Some(All),
    }
  /* All + many window: each needs several samples visible at once --
   * fold's growing accumulator column, pin's ten-samples-is-a-lot
   * motivation. */
  | "TuGen_Fold"
  | "TuGen_PinningCalls" => {
      ...none,
      autoprobe: Some(All),
      samples: Some(Many),
    }
  /* Step into: auto-probe OFF so its three effects are visible -- the user
   * places the one probe by hand, and the multi probe that step-into adds
   * is then observable rather than drowned by ambient auto-probes. Many
   * window for reading the per-iteration fold values afterward. */
  | "TuGen_SteppingIntoCalls" => {
      ...none,
      autoprobe: Some(Off),
      samples: Some(Many),
    }
  /* Colors slide: All + Hybrid scheme + many so the before/after color
   * relationships between sibling samples are visible. */
  | "TuGen_BonusSampleColors" => {
      autoprobe: Some(All),
      colors: Some(Hybrid),
      samples: Some(Many),
    }
  /* Study tasks: writing tasks start with auto-probe All (ambient feedback
   * while building); debugging tasks start Off so the probing strategy is
   * the participant's own choice. All tasks start in the single window;
   * the participant switches to many if they want it. */
  | "TuGen_TaskGroveName"
  | "TuGen_TaskLogCleaner"
  | "TuGen_TaskRunningSum"
  | "TuGen_TaskCropPlotter" => {
      ...none,
      autoprobe: Some(All),
    }
  | "TuGen_TaskDewLedger"
  | "TuGen_TaskGrowthPlotter"
  | "TuGen_TaskPlantingBug"
  | "TuGen_TaskHarvestStreak"
  | "TuGen_TaskWateringTimer" => {
      ...none,
      autoprobe: Some(Off),
    }
  | _ => none
  };

/* Apply a slide's initial settings. Samples/colors are global ProbeProj
 * refs (direct side effects); autoprobe is Settings state, so the caller
 * supplies a dispatcher. Autoprobe applies only when specified (Some).
 * Samples and colors ALWAYS apply with a default: a slide that does not
 * pick one is knocked back to the calm baseline (Single window, Simple
 * two-color scheme), so a mode/scheme a slide forced -- or the user picked
 * mid-slide -- does not silently follow them onto the next slide. This is
 * what keeps each slide's starting window mode deterministic. */
let apply = (~set_autoprobe: AutoProbe.t => unit, init: t): unit => {
  Option.iter(set_autoprobe, init.autoprobe);
  let window =
    Option.value(init.samples, ~default=Language.Sample.Window.Single);
  ProbeProj.Settings.go(SetWindow(window));
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
