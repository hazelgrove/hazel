open Util;

let update_refractors =
    (z: Zipper.t, f: Zipper.Refractor.t => Zipper.Refractor.t) => {
  ...z,
  refractors: f(z.refractors),
};

let update_dyn_cursor =
    (z: Zipper.t, f: Language.Dynamics.Cursor.t => Language.Dynamics.Cursor.t) =>
  update_refractors(z, refractors =>
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
      pinned_call: f(dyn_cursor.pinned_call),
    }
  );

let capture =
    (z: Zipper.t, closure: Language.Dynamics.Probe.Closure.t, id): Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      indicated_call: id != None ? id : z.refractors.dyn_cursor.indicated_call,
      call_cursor: {
        stack:
          !
            ListUtil.is_suffix_of(
              closure.call_stack,
              dyn_cursor.call_cursor.stack,
            )
            ? closure.call_stack : dyn_cursor.call_cursor.stack,
        index: List.length(closure.call_stack) - 1,
      },
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(
    z,
    pinned_call => {
      print_endline(
        "toggle_pin_call: call_stack = "
        ++ String.concat(" ", List.map(Id.str3, call_stack)),
      );
      print_endline(
        "toggle_pin_call: pinned_call = "
        ++ (
          pinned_call
          |> OptUtil.and_then(ListUtil.hd_opt)
          |> Option.map(Id.str3)
          |> Option.value(~default="None")
        ),
      );
      Some(call_stack) == pinned_call ? None : Some(call_stack);
    },
  );

let reset = (z: Zipper.t): Zipper.t =>
  update_dyn_cursor(z, _ => Language.Dynamics.Cursor.init);

let perform = (z: Zipper.t, a: Action.dyn_cursor): Zipper.t =>
  switch (a) {
  | Capture(closure, id) => capture(z, closure, id)
  | TogglePinCall(call_stack) =>
    print_endline(
      "TogglePinCall: " ++ String.concat(" ", List.map(Id.str3, call_stack)),
    );
    toggle_pin_call(z, call_stack);
  | Reset => reset(z)
  };
