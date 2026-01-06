open Util;
open Language;

/* Unified handler for all probe-related actions.
 * This combines the functionality of DynCursorPerform (cursor control)
 * with probe management from Refractors. */

let update_dyn_cursor = (z: Zipper.t, f: DynCursor.t => DynCursor.t): Zipper.t =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      dyn_cursor: f(refractors.dyn_cursor),
    }
  );

let update_pinned_call =
    (z: Zipper.t, f: option(Probe.call_stack) => option(Probe.call_stack))
    : Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      pinned_stack: f(dyn_cursor.pinned_stack),
    }
  );

let capture = (z: Zipper.t, sample: Sample.t, id: option(Id.t)): Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      time: Some(sample.time),
      iter: sample.iter,
      indicated_call: id,
      stack:
        !ListUtil.is_suffix_of(sample.call_stack, dyn_cursor.stack)
          ? sample.call_stack : dyn_cursor.stack,
      index: List.length(sample.call_stack) - 1,
      step_range: Some((sample.step_start, sample.step_end)),
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack: Probe.call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    Some(call_stack) == pinned_call ? None : Some(call_stack)
  });

let reset = (z: Zipper.t): Zipper.t =>
  update_dyn_cursor(z, _ => DynCursor.init);

let perform =
    (
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      z: Zipper.t,
      a: Action.probe,
    )
    : Zipper.t =>
  switch (a) {
  /* Dynamic cursor actions - only need zipper */
  | Capture(sample, id) => capture(z, sample, id)
  | TogglePinCall(call_stack) => toggle_pin_call(z, call_stack)
  | Reset => reset(z)
  /* Probe management actions - need statics and syntax */
  | ToggleProbeManual => Refractors.toggle_manual_action(~statics, ~syntax, z)
  | ToggleProbeREPL => Refractors.toggle_auto_action(~statics, ~syntax, z)
  | ProbeJump =>
    Refractors.step_into(~syntax, statics.info_map, z)
    |> Option.value(~default=z)
  };


