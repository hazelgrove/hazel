open Util;

let capture =
    (z: Zipper.t, closure: Language.Dynamics.Probe.Closure.t, id): Zipper.t => {
  {
    ...z,
    refractors: {
      ...z.refractors,
      dyn_cursor: {
        ...z.refractors.dyn_cursor,
        indicated_call:
          id != None ? id : z.refractors.dyn_cursor.indicated_call,
        call_cursor:
          if (!
                ListUtil.is_suffix_of(
                  closure.call_stack,
                  z.refractors.dyn_cursor.call_cursor.stack,
                )) {
            {
              stack: closure.call_stack,
              index: List.length(closure.call_stack) - 1,
            };
          } else {
            {
              stack: z.refractors.dyn_cursor.call_cursor.stack,
              index: List.length(closure.call_stack) - 1,
            };
          },
      },
    },
  };
};

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    dyn_cursor: {
      ...z.refractors.dyn_cursor,
      pinned_call:
        Some(call_stack) == z.refractors.dyn_cursor.pinned_call
          ? Some(call_stack) : None,
    },
  },
};

let reset = (z: Zipper.t): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    dyn_cursor: Language.Dynamics.Cursor.init,
  },
};

let perform = (z: Zipper.t, a: Action.dyn_cursor): Zipper.t =>
  switch (a) {
  | Capture(closure, id) => capture(z, closure, id)
  | TogglePinCall(call_stack) => toggle_pin_call(z, call_stack)
  | Reset => reset(z)
  };
