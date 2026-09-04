open Util_web;
open Js_of_ocaml;

/* This provides various utility data structures and
 * functions for dealing with Mouse and Pointer Events */

module Event = {
  type sys =
    | Mac
    | PC;

  type held =
    | Down
    | Up;

  type button =
    | Left
    | MiddleWheel
    | Right
    | Back
    | Forward
    | Unknown(int);

  /* Data representing a pointer/mouse event. As attributes are
   * needed they can be added here; probably a good idea to keep
   * this data structure restricted to properties common to both
   * MouseEvents and PointerEvents */
  type t = {
    sys,
    loc: Point.t,
    current_target: Js.opt(Js.t(Dom_html.element)),
    button,
    buttons: int,
    shift: held,
    meta: held,
    ctrl: held,
    alt: held,
  };

  let button = (evt): button =>
    switch (Js.Unsafe.coerce(evt)##.button) {
    | 0 => Left
    | 1 => MiddleWheel
    | 2 => Right
    | 3 => Back
    | 4 => Forward
    | b => Unknown(b)
    };

  /* Not incoporate into above to preserve MouseEvent compatibility */
  let id_of = (evt: Js.t(Dom_html.pointerEvent)): int => evt##.pointerId;

  /* Works with both MouseEvents or PointerEvents */
  let mk = (evt): t => {
    sys: Os.is_mac^ ? Mac : PC,
    loc: {
      row: evt##.clientY,
      col: evt##.clientX,
    },
    current_target: evt##.currentTarget,
    button: button(evt),
    buttons: Js.Unsafe.coerce(evt)##.buttons,
    shift: Js.to_bool(evt##.shiftKey) ? Down : Up,
    meta: Js.to_bool(evt##.metaKey) ? Down : Up,
    ctrl: Js.to_bool(evt##.ctrlKey) ? Down : Up,
    alt: Js.to_bool(evt##.altKey) ? Down : Up,
  };
};

module MkState = () => {
  /* This is a simple state machine to track mouse button state, which can
   * be used to manually track the number of clicks when using PointerEvents,
   * which do not set the `detail` field which MouseEvents use.
   *
   * The button can be either down or up, and keeps a count of consecutive
   * clicks, where 'consecutive' means they are made no more than `delay_ms`
   * apart from each-other. `down_transition` and `up_transition` should be
   * called on `pointerdown` and `pointerup` events respectively, with the
   * return value of the latter giving the number of click cycles, and the
   * `is_button_down` method can be used to check button state.
   *
   * We also track the click location. It works okay without this, but there
   * are two subtle issues: (1) when you try to click to break a selection you
   * just made, it won't work if you click before the multi-click detection
   * delay is up, and (2) if you click and move quickly and click again the
   * second click will create a selection instead of moving. It's easy not
   * to notice either of these effects so exercise caution when updating
   * this code. The click location check is currently exact; this is more
   * forgiving than you might expect since it's in text grid as opposed to
   * pixel coordinates, but we might want to relax this to allow a larger
   * radius if we notice multi-clicks failing to register. */

  [@deriving (show({with_path: false}), sexp, yojson)]
  type count = int;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type button =
    | Up
    | Down;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = {
    mutable button,
    mutable count: int,
    mutable loc: Point.t,
    /* Set on the first mousemove since pointerdown that lands at a
     * grid position other than `loc`. Lets drag handlers distinguish
     * "drag returning to the click point" (legitimate target) from
     * "noise mousemove at the click point before any motion" (which
     * would otherwise trigger an unwanted Resize and scroll). */
    mutable has_left_down_loc: bool,
    /* When true, the current down/up gesture does not advance the
     * multi-click counter (used by Shift+click, which extends a
     * selection rather than cycling click depth). */
    mutable skip_count: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type timer = option((state, state));

  let state: state = {
    button: Up,
    count: 0,
    loc: Point.zero,
    has_left_down_loc: false,
    skip_count: false,
  };

  let delay_ms = 310.0;

  let toggle = (old_button): button =>
    switch (old_button) {
    | Up => Down
    | Down => Up
    };

  let count = (): int => state.count;

  let is_button_down = (): bool => state.button == Down;

  let get_down_loc = (): Point.t => state.loc;

  let has_left_down_loc = (): bool => state.has_left_down_loc;

  /* Call from mousemove. Sets `has_left_down_loc` once the cursor
   * has been observed at a position other than the down-loc since
   * the most recent pointerdown. */
  let note_move = (loc: Point.t): unit =>
    if (!state.has_left_down_loc && loc != state.loc) {
      state.has_left_down_loc = true;
    };

  let is_init = (): bool =>
    state.button == Up && state.count == 0 && state.loc == Point.zero;

  /* Clear the per-gesture transient flags. Called from anywhere
   * that starts or ends a gesture, so adding a new gesture-local
   * field only requires extending this one helper. */
  let reset_gesture_locals = (): unit => {
    state.has_left_down_loc = false;
    state.skip_count = false;
  };

  let reset = (): unit => {
    state.button = Up;
    state.count = 0;
    state.loc = Point.zero;
    reset_gesture_locals();
  };

  let count_reset_timer = (old_count): unit =>
    JsUtil.delay(delay_ms, () =>
      if (state.button == Up && state.count == old_count + 1) {
        reset();
      }
    );

  let pointerdown = (loc: Point.t): unit => {
    if (!is_init() && loc != state.loc) {
      state.count = 0;
    };
    state.button = Down;
    state.loc = loc;
    reset_gesture_locals();
  };

  /* Register a button-down for a gesture that should be drag-eligible
   * but not participate in the multi-click counter. Resets the
   * counter so a following plain click starts a fresh streak, and
   * marks the gesture so pointerup won't increment. */
  let pointerdown_no_count = (loc: Point.t): unit => {
    state.button = Down;
    state.loc = loc;
    state.count = 0;
    reset_gesture_locals();
    state.skip_count = true;
  };

  let pointerup = (loc: Point.t): unit => {
    if (state.skip_count) {
      ();
        /* Gesture opted out of click-counting; leave count alone. */
    } else if (loc != state.loc) {
      state.count = 0;
    } else if (!is_init()) {
      count_reset_timer(state.count);
      state.count = state.count + 1;
    };
    state.button = Up;
    state.loc = loc;
    reset_gesture_locals();
  };
};
