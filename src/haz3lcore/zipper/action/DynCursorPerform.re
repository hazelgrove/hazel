open Util;

let update_dyn_cursor =
    (z: Zipper.t, f: Language.Dynamics.Cursor.t => Language.Dynamics.Cursor.t) =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      dyn_cursor: f(refractors.dyn_cursor),
    }
  );

let update_pinned_call =
    (
      z: Zipper.t,
      f:
        option(Language.Probe.call_stack) =>
        option(Language.Probe.call_stack),
    ) =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      pinned_stack: f(dyn_cursor.pinned_stack),
    }
  );

let capture =
    (z: Zipper.t, closure: Language.Dynamics.Probe.Closure.t, id): Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      indicated_call: id /*!= None ? id : z.refractors.dyn_cursor.indicated_call*/,
      stack:
        !ListUtil.is_suffix_of(closure.call_stack, dyn_cursor.stack)
          ? closure.call_stack : dyn_cursor.stack,
      index: List.length(closure.call_stack) - 1,
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    Some(call_stack) == pinned_call ? None : Some(call_stack)
  });

let reset = (z: Zipper.t): Zipper.t =>
  update_dyn_cursor(z, _ => Language.Dynamics.Cursor.init);

let perform = (z: Zipper.t, a: Action.dyn_cursor): Zipper.t =>
  switch (a) {
  | Capture(closure, id) =>
    print_endline(
      "Capture called, id: "
      ++ Id.str8(Option.value(id, ~default=Id.invalid)),
    );
    capture(z, closure, id);
  | TogglePinCall(call_stack) => toggle_pin_call(z, call_stack)
  | Reset => reset(z)
  };
