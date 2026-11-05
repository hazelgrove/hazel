/* The typing debounce: an edit defers its statics (and the eval request
   they feed) until a pause, and the frame after the pause runs them.
   CodeWithStatics.StaticsDebounce is the page's instance.

   - Fixed: [debounce_ms] after the last edit.
   - Idle: once no input has come for [wait] and none is waiting. The
     wait is the debounce, or longer where frames are slow: a large
     program's frame can take most of the gap between keys, so the fixed
     debounce ends between them and the analysis holds up the next key. */

[@deriving show({with_path: false})]
type policy =
  | Fixed
  | Idle;

let debounce_ms = 225.0;

/* Idle's wait: [frames] recent edit frames (their median), at least the
   debounce, at most [max_wait_ms] */
let frames = 3.0;
let max_wait_ms = 1000.0;
let wait = (~frame_ms: float): float =>
  Float.min(max_wait_ms, Float.max(debounce_ms, frames *. frame_ms));

[@deriving show({with_path: false})]
type decision =
  | Run
  | Wait(float);

/* when the timer fires */
let decide =
    (
      ~policy: policy,
      ~wait: float,
      ~now: float,
      ~last_input: float,
      ~input_pending: bool,
    )
    : decision =>
  switch (policy) {
  | Fixed => Run
  | Idle =>
    let quiet = now -. last_input;
    if (input_pending) {
      Wait(wait);
    } else if (quiet < wait) {
      Wait(wait -. quiet);
    } else {
      Run;
    };
  };

/* what the debounce reads of the page; tests fake it */
module type Env = {
  type timer;
  let now: unit => float;
  /* when an input event (key, pointer) last reached the page */
  let last_input: unit => float;
  let input_pending: unit => bool;
  /* [f] once the frame running now has painted */
  let after_frame: (unit => unit) => unit;
  let policy: unit => policy;
  let set_timeout: (unit => unit, float) => timer;
  let clear_timeout: timer => unit;
};

module Make = (E: Env) => {
  let timer: ref(option(E.timer)) = ref(None);
  /* the next frame runs statics: the timer let them, or a caller asks */
  let force_on_next: ref(bool) = ref(false);

  let pending = () => timer^ != None || force_on_next^;

  /* the last edit frames' durations, from their calculate to their paint */
  let recent: ref(list(float)) = ref([]);
  let frame_ms = () =>
    switch (List.sort(Float.compare, recent^)) {
    | [] => 0.
    | sorted => List.nth(sorted, List.length(sorted) / 2)
    };
  let note_frame = (ms: float) =>
    recent := Util.ListUtil.take(5, [ms, ...recent^]);

  let clear = () =>
    switch (timer^) {
    | Some(t) =>
      E.clear_timeout(t);
      timer := None;
    | None => ()
    };

  /* nothing pending: for tests, which never yield to the timer */
  let reset = () => {
    clear();
    force_on_next := false;
    recent := [];
  };

  let rec arm = (~schedule_refresh: unit => unit, ms: float) =>
    timer :=
      Some(
        E.set_timeout(
          () => {
            timer := None;
            switch (
              decide(
                ~policy=E.policy(),
                ~wait=wait(~frame_ms=frame_ms()),
                ~now=E.now(),
                ~last_input=E.last_input(),
                ~input_pending=E.input_pending(),
              )
            ) {
            | Run =>
              /* Keep sends waiting until the scheduled refresh is consumed. */
              force_on_next := true;
              schedule_refresh();
            | Wait(ms) => arm(~schedule_refresh, ms)
            };
          },
          ms,
        ),
      );

  /* Call from calculate to get the statics_mode for this cycle.
     schedule_refresh should dispatch the mode's RefreshStatics action. */
  let consume = (~is_edited, ~schedule_refresh: unit => unit): StaticsMode.t => {
    let force_now = force_on_next^;
    force_on_next := false;
    /* a projector commit is one discrete edit: run statics now rather
       than after the typing debounce (livelit drag → result: -225ms) */
    let projector_edit = Util.AgentPulse.projector_commit^;
    Util.AgentPulse.projector_commit := false;
    if (force_now || is_edited && projector_edit) {
      clear();
      Force;
    } else if (is_edited) {
      clear();
      let t0 = E.now();
      E.after_frame(() => note_frame(E.now() -. t0));
      arm(
        ~schedule_refresh,
        switch (E.policy()) {
        | Fixed => debounce_ms
        | Idle => wait(~frame_ms=frame_ms())
        },
      );
      Defer;
    } else {
      Normal;
    };
  };
};

/* the page: performance.now, a capture listener for keys and pointers,
   and navigator.scheduling.isInputPending where the browser has it. The
   policy is a flag in the URL: ?statics=fixed or ?statics=idle */
module Browser = {
  open Js_of_ocaml;

  type timer = Dom_html.timeout_id;

  let now = (): float =>
    try(Util.JsUtil.precise_timestamp()) {
    | _ => 0.
    };

  let last: ref(float) = ref(neg_infinity);
  let last_input = () => last^;
  /* none under node (the tests) */
  let () = {
    let note = Js.wrap_callback(_ => last := now());
    List.iter(
      name =>
        try(
          ignore(
            Js.Unsafe.meth_call(
              Dom_html.window,
              "addEventListener",
              [|
                Js.Unsafe.inject(Js.string(name)),
                Js.Unsafe.inject(note),
                Js.Unsafe.inject(Js._true),
              |],
            ),
          )
        ) {
        | _ => ()
        },
      ["keydown", "pointerdown"],
    );
  };

  let input_pending = (): bool =>
    try({
      let scheduling = Js.Unsafe.global##.navigator##.scheduling;
      Js.Optdef.test(scheduling)
      && Js.to_bool(Js.Unsafe.meth_call(scheduling, "isInputPending", [||]));
    }) {
    | _ => false
    };

  let default_policy = Idle;
  let flag: Lazy.t(policy) =
    lazy(
      switch (
        try(Util.JsUtil.QueryParams.get_param("statics")) {
        | _ => None
        }
      ) {
      | Some("fixed") => Fixed
      | Some("idle") => Idle
      | _ => default_policy
      }
    );
  let policy = () => Lazy.force(flag);

  let set_timeout = (f: unit => unit, ms: float): timer =>
    Dom_html.window##setTimeout(Js.wrap_callback(f), ms);
  /* a task queued from the frame runs after its rendering */
  let after_frame = (f: unit => unit) => ignore(set_timeout(f, 0.));
  let clear_timeout = (t: timer) => Dom_html.window##clearTimeout(t);
};
