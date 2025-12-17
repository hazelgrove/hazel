open Util;
open OptUtil.Syntax;
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
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    Some(call_stack) == pinned_call ? None : Some(call_stack)
  });

let reset = (z: Zipper.t): Zipper.t =>
  update_dyn_cursor(z, _ => Language.DynCursor.init);

let perform = (z: Zipper.t, a: Action.dyn_cursor): Zipper.t =>
  switch (a) {
  | Capture(sample, id) => capture(z, sample, id)
  | TogglePinCall(call_stack) => toggle_pin_call(z, call_stack)
  | Reset => reset(z)
  };
