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
 * these to give a slide a non-default starting point. Slides not listed
 * inherit the current state (no-op).
 *
 * Guiding principle from the slide prompts: do NOT pre-set a knob on the
 * slide that *teaches* its toggle (e.g. slide 10 has you press Space for
 * many mode, slide 12 has you turn on auto-probe) — forcing it would
 * pre-empt the lesson. Only set a value where the prompt assumes a
 * non-default starting state:
 *   - 18 "write one yourself, with auto-probe on"  -> autoprobe Caret
 *   - 22 explains pink/blue/green sample colors, which the global default
 *        Simple scheme can't show                  -> colors Hybrid
 *   - 23 "Turn on auto-probe and wander" (capstone) -> autoprobe Caret
 * Task slides set their own precise values elsewhere. */
let of_slide = (module_name: string): t =>
  switch (module_name) {
  | "TuGen_18WritingRunningSum" => {
      ...none,
      autoprobe: Some(Caret),
    }
  | "TuGen_22SampleColors" => {
      ...none,
      colors: Some(Hybrid),
    }
  | "TuGen_23GreenhouseArena" => {
      ...none,
      autoprobe: Some(Caret),
    }
  | _ => none
  };

/* Apply a slide's initial settings. Samples/colors are global ProbeProj
 * refs (direct side effects); autoprobe is Settings state, so the caller
 * supplies a dispatcher. Each is applied only when specified (Some). */
let apply = (~set_autoprobe: AutoProbe.t => unit, init: t): unit => {
  Option.iter(set_autoprobe, init.autoprobe);
  Option.iter(w => ProbeProj.Settings.go(SetWindow(w)), init.samples);
  Option.iter(c => ProbeProj.Settings.go(SetSampleBase(c)), init.colors);
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
