open Haz3lcore;

/* Apply lesson defaults on entry. Auto-probe is optional; samples and colors
 * always reset, so manual choices do not leak into the next lesson. */
let apply =
    (~set_autoprobe: AutoProbe.t => unit, init: TutorialProbeConfig.initial)
    : unit => {
  Option.iter(set_autoprobe, init.autoprobe);
  ProbeProj.Settings.go(SetWindow(init.samples));
  ProbeProj.Settings.go(SetSampleBase(init.colors));
};

/* The current Probes lesson and the settings saved on entry to its folder.
 * Moving between Probes lessons keeps that snapshot; leaving restores it. */
let last_applied: ref(option(string)) = ref(None);
let previous: ref(option(TutorialProbeConfig.initial)) = ref(None);

let maybe_apply_on_change =
    (
      ~autoprobe: AutoProbe.t,
      ~set_autoprobe: AutoProbe.t => unit,
      lesson: option(Tutorial.p('a)),
    )
    : unit => {
  let module_name =
    switch (lesson) {
    | Some(lesson) when Tutorial.is_probes_lesson(lesson) =>
      Some(lesson.module_name)
    | _ => None
    };
  if (module_name != last_applied^) {
    last_applied := module_name;
    switch (module_name) {
    | Some(name) =>
      if (previous^ == None) {
        previous :=
          Some({
            autoprobe: Some(autoprobe),
            samples: ProbeProj.Settings.s^.window,
            colors: ProbeProj.Settings.s^.sample_base,
          });
      };
      apply(~set_autoprobe, TutorialProbeConfig.of_slide(name).initial);
    | None =>
      Option.iter(apply(~set_autoprobe), previous^);
      previous := None;
    };
  };
};
