open Alcotest;
open Web;

/* The typing debounce's policy (StaticsTiming) on a fake page: a clock,
   timers, frames and pending input the tests drive */

module Fake = {
  type timer = int;
  let clock = ref(0.);
  let input_at = ref(neg_infinity);
  let pending = ref(false);
  let policy_ = ref(StaticsTiming.Idle);
  /* (id, due, callback) */
  let timers: ref(list((int, float, unit => unit))) = ref([]);
  let ids = ref(0);
  let painted: ref(list(unit => unit)) = ref([]);

  let now = () => clock^;
  let last_input = () => input_at^;
  let input_pending = () => pending^;
  let after_frame = f => painted := painted^ @ [f];
  let policy = () => policy_^;
  let set_timeout = (f, ms) => {
    incr(ids);
    timers := timers^ @ [(ids^, clock^ +. ms, f)];
    ids^;
  };
  let clear_timeout = id =>
    timers := List.filter(((i, _, _)) => i != id, timers^);

  let reset = (policy: StaticsTiming.policy) => {
    clock := 0.;
    input_at := neg_infinity;
    pending := false;
    policy_ := policy;
    timers := [];
    painted := [];
  };

  /* the frame running now has painted */
  let paint = () => {
    let fs = painted^;
    painted := [];
    List.iter(f => f(), fs);
  };
  let next_due = () =>
    List.fold_left(
      (m, (_, d, _)) => Some(Option.fold(~none=d, ~some=Float.min(d), m)),
      None,
      timers^,
    );
  /* the timers due by now, earliest first; one that waits sets another */
  let rec fire_due = () =>
    switch (List.find_opt(((_, d, _)) => Some(d) == next_due(), timers^)) {
    | Some((i, d, f)) when d <= clock^ =>
      clear_timeout(i);
      f();
      fire_due();
    | _ => ()
    };
};

module D = StaticsTiming.Make(Fake);

let refreshes = ref(0);
let schedule_refresh = () => incr(refreshes);

type action =
  | Key
  | Refresh;

type keys =
  /* pressed at these times (ms), whatever the page is doing */
  | Open(list(float))
  /* this many, each pressed this long after the last one painted */
  | Closed(int, float);

/* when statics ran, typing [keys] on a page whose edit frames take
   [frame_ms] and whose analysis takes [statics_ms]. As in a browser: one
   frame at a time, keys pressed meanwhile wait; once the page is free
   their keydowns run, then due timers (or the timers first, the keys
   still pending input, with [timer_first]), then one frame for the
   actions they queued, in order (a refresh: the RefreshStatics the
   debounce dispatches) */
let simulate =
    (~policy, ~frame_ms, ~statics_ms=150., ~timer_first=false, keys: keys)
    : list(float) => {
  Fake.reset(policy);
  D.reset();
  refreshes := 0;
  let ran = ref([]);
  let (keys, left, gap) =
    switch (keys) {
    | Open(ts) => (ts, 0, 0.)
    | Closed(n, gap) => (n > 0 ? [0.] : [], n - 1, gap)
    };
  let rec loop = (keys, left) => {
    let now = Fake.clock^;
    let (waiting, later) = List.partition(k => k <= now, keys);
    let asked = refreshes^;
    if (timer_first && waiting != []) {
      Fake.pending := true;
      Fake.fire_due();
      Fake.pending := false;
    };
    let first = refreshes^ > asked;
    if (waiting != []) {
      Fake.input_at := now;
    };
    Fake.fire_due();
    let actions =
      (first ? [Refresh] : [])
      @ List.map(_ => Key, waiting)
      @ (refreshes^ > asked && !first ? [Refresh] : []);
    switch (actions) {
    | [] =>
      let next =
        switch (later, Fake.next_due()) {
        | ([k, ..._], Some(d)) => Some(Float.min(k, d))
        | ([k, ..._], None) => Some(k)
        | ([], d) => d
        };
      switch (next) {
      | Some(t) =>
        Fake.clock := t;
        loop(later, left);
      | None => ()
      };
    | _ =>
      let forced =
        List.fold_left(
          (forced, a) =>
            switch (
              switch (a) {
              | Key => D.consume(~is_edited=true, ~schedule_refresh)
              | Refresh =>
                D.force_on_next := true;
                D.consume(~is_edited=false, ~schedule_refresh);
              }
            ) {
            | StaticsMode.Force => true
            | Defer
            | Normal => forced
            },
          false,
          actions,
        );
      if (forced) {
        ran := ran^ @ [now];
      };
      Fake.clock :=
        now +. (waiting != [] ? frame_ms : 0.) +. (forced ? statics_ms : 0.);
      Fake.paint();
      waiting != [] && left > 0
        ? loop(later @ [Fake.clock^ +. gap], left - 1) : loop(later, left);
    };
  };
  loop(keys, left);
  ran^;
};

let ms = list(float(0.5));
let every = (~gap, n) => Open(List.init(n, i => float_of_int(i) *. gap));
let policies = StaticsTiming.[Fixed, Idle];

/* 10 keys 110ms apart on a small program (40ms frames): once, the
   debounce after the last key, either way */
let small_program = () =>
  List.iter(
    policy =>
      check(
        ms,
        StaticsTiming.show_policy(policy),
        [990. +. 225.],
        simulate(~policy, ~frame_ms=40., every(~gap=110., 10)),
      ),
    policies,
  );

/* the same keys on a large program (250ms frames): they queue, the last
   ones (770-990) in the frame at 1000. Fixed analyzes between frames,
   holding up the keys waiting; idle once, three frames after that one */
let large_program = (~timer_first, ()) => {
  let sim = (policy: StaticsTiming.policy) =>
    simulate(~policy, ~frame_ms=250., ~timer_first, every(~gap=110., 10));
  let fixed = sim(StaticsTiming.Fixed);
  check(
    bool,
    Printf.sprintf("fixed: between keys (%d times)", List.length(fixed)),
    true,
    List.length(fixed) > 1,
  );
  check(ms, "idle: once, after", [1000. +. 750.], sim(StaticsTiming.Idle));
};

/* each key 110ms after the last one painted, frames of 125ms (4k): keys
   235ms apart, past the debounce. Idle waits three frames, 375ms */
let closed_loop = () => {
  let sim = (policy: StaticsTiming.policy) =>
    simulate(~policy, ~frame_ms=125., Closed(10, 110.));
  let fixed = sim(StaticsTiming.Fixed);
  check(
    bool,
    Printf.sprintf("fixed: between keys (%d times)", List.length(fixed)),
    true,
    List.length(fixed) > 5,
  );
  check(
    ms,
    "idle: once, after",
    [9. *. 235. +. 375.],
    sim(StaticsTiming.Idle),
  );
};

/* keys 1.1s apart: after each, when the debounce or the key's frame
   ends; idle, on 250ms frames, three frames after */
let paused = () => {
  let sim = (policy, frame_ms) =>
    simulate(~policy, ~frame_ms, every(~gap=1100., 4));
  let at = after => List.init(4, i => float_of_int(i) *. 1100. +. after(i));
  List.iter(
    policy =>
      check(
        ms,
        StaticsTiming.show_policy(policy) ++ ", 40ms frames",
        at(_ => 225.),
        sim(policy, 40.),
      ),
    policies,
  );
  check(
    ms,
    "fixed, 250ms frames",
    at(_ => 250.),
    sim(StaticsTiming.Fixed, 250.),
  );
  check(
    ms,
    "idle, 250ms frames",
    at(_ => 750.),
    sim(StaticsTiming.Idle, 250.),
  );
};

let decision =
  testable(StaticsTiming.pp_decision, (a, b) =>
    switch (a, b) {
    | (StaticsTiming.Run, StaticsTiming.Run) => true
    | (Wait(x), Wait(y)) => Float.abs(x -. y) < 0.5
    | _ => false
    }
  );
let decide = () => {
  let idle = (~input_pending=false, now) =>
    StaticsTiming.decide(
      ~policy=StaticsTiming.Idle,
      ~wait=300.,
      ~now,
      ~last_input=100.,
      ~input_pending,
    );
  check(decision, "the wait since the input", StaticsTiming.Run, idle(400.));
  check(
    decision,
    "input 100ms ago: the rest",
    StaticsTiming.Wait(200.),
    idle(200.),
  );
  check(
    decision,
    "input waiting: the wait again",
    StaticsTiming.Wait(300.),
    idle(~input_pending=true, 900.),
  );
  check(
    decision,
    "fixed: now",
    StaticsTiming.Run,
    StaticsTiming.decide(
      ~policy=StaticsTiming.Fixed,
      ~wait=300.,
      ~now=200.,
      ~last_input=150.,
      ~input_pending=true,
    ),
  );
  let wait = frame_ms => StaticsTiming.wait(~frame_ms);
  check(float(0.5), "small frames: the debounce", 225., wait(40.));
  check(float(0.5), "large: three frames", 450., wait(150.));
  check(float(0.5), "at most a second", 1000., wait(900.));
};

/* a key that doesn't edit (a caret move) puts the statics off but keeps
   them; the refresh it asks for runs them on the next frame */
let caret_key = () => {
  Fake.reset(StaticsTiming.Idle);
  D.reset();
  refreshes := 0;
  let mode = is_edited => D.consume(~is_edited, ~schedule_refresh);
  check(bool, "an edit defers", true, mode(true) == StaticsMode.Defer);
  Fake.clock := 100.;
  Fake.input_at := 100.;
  check(
    bool,
    "a caret move doesn't",
    true,
    mode(false) == StaticsMode.Normal,
  );
  Fake.clock := 225.;
  Fake.fire_due();
  check(int, "not the debounce after the edit", 0, refreshes^);
  check(bool, "still pending", true, D.pending());
  Fake.clock := 325.;
  Fake.fire_due();
  check(int, "the debounce after the caret move", 1, refreshes^);
  check(
    bool,
    "the next frame runs them",
    true,
    mode(false) == StaticsMode.Force,
  );
  check(bool, "none pending after", false, D.pending());
};

let tests = (
  "StaticsTiming",
  [
    test_case("a small program, typing", `Quick, small_program),
    test_case(
      "a large program, typing",
      `Quick,
      large_program(~timer_first=false),
    ),
    test_case(
      "the same, the timer first (input pending)",
      `Quick,
      large_program(~timer_first=true),
    ),
    test_case("keys after each paint", `Quick, closed_loop),
    test_case("typing with pauses", `Quick, paused),
    test_case("the idle decision and wait", `Quick, decide),
    test_case("a caret move after an edit", `Quick, caret_key),
  ],
);
