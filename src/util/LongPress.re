open Js_of_ocaml;

/* A finger held still: the touch-screen stand-in for right-click and
 * hover. `arm` it from a touch pointerdown with what the long press does;
 * that runs after `delay_ms` unless the finger lifts, drifts more than
 * `slop_px` (a scroll), or the gesture is cancelled first.
 *
 * One press is armed at a time. Arming replaces the press in progress, so
 * of several elements a press lands in, the innermost to arm wins (a
 * pointerdown reaches the target before its ancestors' bubbling handlers).
 *
 * A press that fires is spent on it. A `pointercancel` sent from the
 * pressed element tells handlers further out, the code editor's own
 * long-press menu among them, to drop the gesture; and the mouse events
 * the lift would make, and the browser's own long-press menu, are
 * swallowed until the next press. Listens on the window, in the capture
 * phase, from the first `arm`. */

let delay_ms = 450.0;
let slop_px = 10.0;

type press = {
  x: float,
  y: float,
  timer: Dom_html.timeout_id_safe,
};

let current: ref(option(press)) = ref(None);
let spent = ref(false);
let listening = ref(false);

let is_touch = (evt: Js.t(Dom_html.pointerEvent)): bool =>
  Js.to_string(Js.Unsafe.get(evt, "pointerType")) == "touch"
  && Js.to_bool(evt##.isPrimary);

let cancel = () =>
  switch (current^) {
  | Some({timer, _}) =>
    Dom_html.clearTimeout(timer);
    current := None;
  | None => ()
  };

let dispatch_pointercancel = (target: Js.t(Dom_html.eventTarget)) => {
  let init =
    Js.Unsafe.obj([|
      ("bubbles", Js.Unsafe.inject(Js._true)),
      ("pointerType", Js.Unsafe.inject(Js.string("touch"))),
    |]);
  let evt =
    Js.Unsafe.new_obj(
      Js.Unsafe.global##.PointerEvent,
      [|
        Js.Unsafe.inject(Js.string("pointercancel")),
        Js.Unsafe.inject(init),
      |],
    );
  ignore(Js.Unsafe.meth_call(target, "dispatchEvent", [|evt|]));
};

let listen = (name: string, handler: Js.t('e) => unit) =>
  ignore(
    Dom_html.addEventListener(
      Dom_html.window,
      Dom.Event.make(name),
      Dom_html.handler(evt => {
        handler(evt);
        Js._true;
      }),
      Js._true,
    ),
  );

let swallow = (evt: Js.t(Dom_html.event)) =>
  if (spent^) {
    Dom.preventDefault(evt);
    Dom_html.stopPropagation(evt);
  };

let start_listening = () =>
  if (! listening^) {
    listening := true;
    listen("pointerdown", _ => {
      cancel();
      spent := false;
    });
    listen("pointermove", (evt: Js.t(Dom_html.pointerEvent)) =>
      switch (current^) {
      | Some({x, y, _})
          when
            Float.abs(float_of_int(evt##.clientX) -. x) > slop_px
            || Float.abs(float_of_int(evt##.clientY) -. y) > slop_px =>
        cancel()
      | _ => ()
      }
    );
    listen("pointerup", _ => cancel());
    listen("pointercancel", _ => cancel());
    List.iter(
      name => listen(name, swallow),
      ["mousedown", "mouseup", "click"],
    );
    listen("contextmenu", (evt: Js.t(Dom_html.event)) =>
      if (spent^ || current^ != None) {
        Dom.preventDefault(evt);
      }
    );
  };

let arm = (evt: Js.t(Dom_html.pointerEvent), on_long_press: unit => unit) => {
  start_listening();
  cancel();
  let fire = () => {
    current := None;
    spent := true;
    Js.Opt.iter(evt##.target, target =>
      dispatch_pointercancel((target :> Js.t(Dom_html.eventTarget)))
    );
    on_long_press();
  };
  current :=
    Some({
      x: float_of_int(evt##.clientX),
      y: float_of_int(evt##.clientY),
      timer: Dom_html.setTimeout(fire, delay_ms),
    });
};
