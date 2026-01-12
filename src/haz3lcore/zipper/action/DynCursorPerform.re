open Util;
open Language;

//TODO(andrew): abstract out non-zipper parts of this to DynCursor.re

let update_dyn_cursor = (z: Zipper.t, f: DynCursor.t => DynCursor.t) =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      dyn_cursor: f(refractors.dyn_cursor),
    }
  );

let update_pinned_call =
    (z: Zipper.t, f: option(Probe.call_stack) => option(Probe.call_stack)) =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      pinned_stack: f(dyn_cursor.pinned_stack),
    }
  );

let capture = (z: Zipper.t, sample: Sample.t, id): Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      time: Some(sample.time),
      iter: sample.iter,
      indicated_call: id /*!= None ? id : z.refractors.dyn_cursor.indicated_call*/,
      stack:
        !ListUtil.is_suffix_of(sample.call_stack, dyn_cursor.stack)
          ? sample.call_stack : dyn_cursor.stack,
      index: List.length(sample.call_stack) - 1,
      step_range: Some((sample.step_start, sample.step_end)),
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    Some(call_stack) == pinned_call ? None : Some(call_stack)
  });

let reset = (z: Zipper.t): Zipper.t =>
  update_dyn_cursor(z, _ => Language.DynCursor.init);

/* Resolve pending focus after step-into by finding and focusing
   the sample that matches the target stack. Called from Refractors
   after it looks up the samples from dynamics. */
let resolve_pending_focus =
    (z: Zipper.t, samples: list(Sample.t), target_stack: Probe.call_stack)
    : Zipper.t => {
  /* Find a sample whose call_stack matches the target */
  let matching_sample =
    List.find_opt((s: Sample.t) => s.call_stack == target_stack, samples);
  switch (matching_sample) {
  | Some(sample) =>
    /* Found matching sample - capture it and clear pending_focus */
    update_dyn_cursor(z, dyn_cursor =>
      {
        ...dyn_cursor,
        time: Some(sample.time),
        iter: sample.iter,
        indicated_call: None,
        stack: sample.call_stack,
        index: List.length(sample.call_stack) - 1,
        step_range: Some((sample.step_start, sample.step_end)),
        pending_focus: None,
      }
    )
  | None =>
    /* No matching sample yet - keep pending_focus for later */
    z
  };
};

let perform = (z: Zipper.t, a: Action.dyn_cursor): Zipper.t =>
  switch (a) {
  | Capture(sample, id) => capture(z, sample, id)
  | TogglePinCall(call_stack) => toggle_pin_call(z, call_stack)
  | Reset => reset(z)
  | ResolvePendingFocus(samples) =>
    switch (z.refractors.dyn_cursor.pending_focus) {
    | None => z
    | Some({target_stack, _}) =>
      resolve_pending_focus(z, samples, target_stack)
    }
  };
