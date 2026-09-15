open Alcotest;
module T = Web.CanvasTrajectory;

let starting_slide = () => {
  let calls = ref([]);
  let old_new = T.dispatch_new_slide^;
  let old_paste = T.dispatch_paste^;
  Fun.protect(
    ~finally=
      () => {
        T.dispatch_new_slide := old_new;
        T.dispatch_paste := old_paste;
      },
    () => {
      T.dispatch_new_slide := (() => calls := calls^ @ ["new"]);
      T.dispatch_paste := (s => calls := calls^ @ ["paste:" ++ s]);
      let run = text => {
        calls := [];
        let (header, _) = Option.get(T.parse(text));
        T.prepare_replay(header);
        calls^;
      };
      check(
        list(string),
        "null program starts an empty scratchpad",
        ["new"],
        run({|{"version":2,"program":null,"events":[]}|}),
      );
      check(
        list(string),
        "text follows the new slide",
        ["new", "paste:let x = 1 in x"],
        run({|{"version":2,"program":"let x = 1 in x","events":[]}|}),
      );
      check(
        list(string),
        "legacy event-only replay keeps its context",
        [],
        run("[]"),
      );
    },
  );
};

let clock_stops_at_finish = () => {
  let p: T.player = {
    steps: [||],
    idx: 0,
    speed: 2.,
    paused: false,
    t_trace: 250.,
    wall_anchor: T.now() -. 1000.,
    timer: None,
    finished: false,
    total: 2250.,
    label: "clock regression",
    header: None,
  };
  T.finish(p);
  check(
    bool,
    "completion includes elapsed playback",
    true,
    p.t_trace >= 2250.,
  );
  let stopped = p.t_trace;
  p.wall_anchor = p.wall_anchor -. 1000.;
  T.finish(p);
  check(float(0.), "finished clock stays frozen", stopped, T.trace_now(p));
};

let tests = (
  "Canvas trajectory",
  [
    test_case("starting slide isolation", `Quick, starting_slide),
    test_case(
      "completion clock is monotonic and frozen",
      `Quick,
      clock_stops_at_finish,
    ),
  ],
);
