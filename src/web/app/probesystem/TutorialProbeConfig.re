open Haz3lcore;
open ProbeControls;

/* One entry per lesson controls what appears, what is highlighted as new,
 * and the initial settings. Later manual choices are handled by the normal
 * shared controls; TutorialSlideInit applies these defaults only on entry. */
type initial = {
  autoprobe: option(AutoProbe.t),
  samples: Language.Sample.Window.mode,
  colors: ProbeProj.Settings.sample_base,
};

type t = {
  flags: list(feature),
  new_flags: list(feature),
  initial,
};

let lesson =
    (
      ~flags,
      ~new_flags=[],
      ~autoprobe=Some(AutoProbe.Off),
      ~samples=Language.Sample.Window.Single,
      ~colors=ProbeProj.Settings.Simple,
      (),
    )
    : t => {
  flags,
  new_flags,
  initial: {
    autoprobe,
    samples,
    colors,
  },
};

/* Each stage keeps the controls from the preceding lessons. The color
 * legend is useful only on the bonus lesson, which uses Hybrid colors. */
let placement = [AddProbe];
let environment = placement @ [SeeVars];
let empty_samples = environment @ [IconEmpty];
let samples = empty_samples @ [SamplesToggle, NavSamples];
let focus = samples @ [FocusProbe, IconOutsideFocus];
let automatic = focus @ [AutoProbe];
let bigger_values = automatic @ [Resize, ExpandProbe];
let pinning = bigger_values @ [Pin, IconPinHidden];
let stepping = pinning @ [StepInto];
let printing = stepping @ [Console];

let of_slide = (module_name: string): t =>
  switch (module_name) {
  | "TuGen_ArithmeticAndHoles" =>
    lesson(~flags=placement, ~new_flags=[AddProbe], ())
  | "TuGen_TheBackpack"
  | "TuGen_AddingAndRemovingProbes" => lesson(~flags=placement, ())
  | "TuGen_EnvironmentExplorer" =>
    lesson(~flags=environment, ~new_flags=[SeeVars], ())
  | "TuGen_TuplesAndRecords"
  | "TuGen_IfExpressions" => lesson(~flags=environment, ())
  | "TuGen_CaseExpressions" =>
    lesson(~flags=empty_samples, ~new_flags=[IconEmpty], ())
  | "TuGen_ConstructorsWithData" => lesson(~flags=empty_samples, ())
  /* Single is deliberate here: switching to Many is the lesson. */
  | "TuGen_SamplesPerCall" =>
    lesson(~flags=samples, ~new_flags=[SamplesToggle, NavSamples], ())
  | "TuGen_AligningSamples" =>
    lesson(~flags=focus, ~new_flags=[FocusProbe, IconOutsideFocus], ())
  /* Auto-probe starts Off so the participant turns it on themselves. */
  | "TuGen_AutoProbe" => lesson(~flags=automatic, ~new_flags=[AutoProbe], ())
  | "TuGen_ReadingBiggerValues" =>
    lesson(
      ~flags=bigger_values,
      ~new_flags=[Resize, ExpandProbe],
      ~samples=Many,
      (),
    )
  | "TuGen_MappingOverAList" =>
    lesson(~flags=bigger_values, ~autoprobe=Some(All), ())
  /* Many exposes the growing accumulator and motivates pinning. */
  | "TuGen_FoldingOverAList" =>
    lesson(~flags=bigger_values, ~autoprobe=Some(All), ~samples=Many, ())
  | "TuGen_PinningCalls" =>
    lesson(
      ~flags=pinning,
      ~new_flags=[Pin, IconPinHidden],
      ~autoprobe=Some(All),
      ~samples=Many,
      (),
    )
  /* Off makes the probes added by stepping into a call stand out. */
  | "TuGen_SteppingIntoCalls" =>
    lesson(~flags=stepping, ~new_flags=[StepInto], ~samples=Many, ())
  | "TuGen_PrintStatements" =>
    lesson(~flags=printing, ~new_flags=[Console], ())
  | "TuGen_BonusSampleColors" =>
    lesson(
      ~flags=printing @ [Legend],
      ~new_flags=[Legend],
      ~autoprobe=Some(All),
      ~samples=Many,
      ~colors=Hybrid,
      (),
    )
  /* Writing tasks use ambient probes; debugging tasks leave placement to
   * the participant. All tasks start Single, with the full reference. */
  | "TuGen_TaskGroveName"
  | "TuGen_TaskLogCleaner"
  | "TuGen_TaskRunningSum"
  | "TuGen_TaskCropPlotter" =>
    lesson(~flags=printing, ~autoprobe=Some(All), ())
  | "TuGen_TaskDewLedger"
  | "TuGen_TaskGrowthPlotter"
  | "TuGen_TaskPlantingBug"
  | "TuGen_TaskHarvestStreak"
  | "TuGen_TaskWateringTimer" => lesson(~flags=printing, ())
  /* Views: each lesson's livelit draws one probe's samples, side by side in
   * Many mode, with the explicit probes alone (auto-probe Off). */
  | "TuGen_ViewsSparkline"
  | "TuGen_ViewsColor"
  | "TuGen_ViewsHorizon"
  | "TuGen_ViewsToggle" => lesson(~flags=bigger_values, ~samples=Many, ())
  /* Intro and text-only transitions inherit auto-probe, reset samples and
   * colors, and show no controls. Caret is never preset or exposed here. */
  | _ => lesson(~flags=[], ~autoprobe=None, ())
  };
