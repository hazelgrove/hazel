open Alcotest;
module S = Web.CanvasScore;

/* The staging language's invariants: one score per beat, the actor
   precedes every effect, times never run backwards, terminals bloom with
   their type, pills follow their arrow, a tidy drift comes last. */

let p = (x, y): S.pos => {
  x,
  y,
};
let node = (~anchor=?, key, x, y): S.new_node => {
  key,
  anchor,
  p: p(x, y),
};

let diff = (~cause="test", ~added=[], ~edges=[], ~removed=[], ~moved=[], ()) => {
  S.d_cause: cause,
  added,
  edges,
  removed,
  moved,
  actor: Some(p(0., 0.)),
};

let pos_of_added = (d: S.diff, k) =>
  List.find_opt((n: S.new_node) => n.key == k, d.added)
  |> Option.map((n: S.new_node) => n.p);

let appear = (s: S.score, k) => List.assoc(k, S.appear_times(s));

let no_violations = (name, s: S.score) =>
  check(list(string), name ++ ": validate", [], S.validate(s));

let three_types = () => {
  /* the bug case of round 100: terminals listed BEFORE their types */
  let d =
    diff(
      ~added=[
        node(~anchor="Score", "Int@Scorec0", 80., 40.),
        node(~anchor="Score", "Int@Scorec1", 80., 90.),
        node("Score", 160., 60.),
        node("Grade", 160., 240.),
        node("Name", 160., 420.),
      ],
      (),
    );
  let s = S.plan(~pos_of=pos_of_added(d), d);
  check(int, "one act per type", 3, List.length(s.acts));
  no_violations("three types", s);
  let a_score = appear(s, "Score")
  and a_t0 = appear(s, "Int@Scorec0")
  and a_t1 = appear(s, "Int@Scorec1")
  and a_grade = appear(s, "Grade")
  and a_name = appear(s, "Name");
  check(
    bool,
    "type before its terminals",
    true,
    a_score < a_t0 && a_t0 < a_t1,
  );
  check(bool, "terminals bloom with their type", true, a_t1 - a_score < 400);
  check(bool, "next type after the first act", true, a_grade > a_t1 + 200);
  check(bool, "reading order", true, a_score < a_grade && a_grade < a_name);
  /* the actor's path never runs backwards in time */
  let path = S.actor_path(~pos_of=pos_of_added(d), s);
  let ts = List.map(((t, _, _)) => t, path);
  check(list(int), "actor path sorted", List.sort(compare, ts), ts);
  check(
    bool,
    "about a second per type",
    true,
    s.total_ms > 4500 && s.total_ms < 9000,
  );
};

let actor_first = () => {
  let d = diff(~added=[node("A", 100., 100.)], ());
  let s = S.plan(~pos_of=pos_of_added(d), d);
  let (t0, a) = List.hd(s.acts);
  check(int, "first act at 0", 0, t0);
  List.iter(
    (e: S.timed_effect) =>
      check(
        bool,
        "effect after arrival+pause",
        true,
        e.at >= a.travel_ms + a.pause_ms,
      ),
    a.effects,
  );
};

let edge_after_types = () => {
  /* Grade already exists; the arrow lands on a NEW terminal of it */
  let d =
    diff(
      ~added=[
        node("Score", 100., 100.),
        node(~anchor="Grade", "Grade@quiz", 440., 100.),
      ],
      ~edges=[
        {
          S.name: "quiz",
          src: "Score",
          dst: "Grade@quiz",
          product: None,
        },
      ],
      (),
    );
  let pos_of = k => k == "Grade" ? Some(p(400., 100.)) : pos_of_added(d, k);
  let s = S.plan(~pos_of, d);
  no_violations("edge after types", s);
  let effs = S.effects_abs(s);
  let t_of = f =>
    List.find_map(
      ((t, _, e: S.timed_effect)) => f(e.effect) ? Some(t) : None,
      effs,
    )
    |> Option.get;
  let t_draw = t_of(e => e == S.Draw("quiz"))
  and t_pill = t_of(e => e == S.Pill("quiz"))
  and t_score = t_of(e => e == S.Appear("Score"))
  and t_term = t_of(e => e == S.Appear("Grade@quiz"));
  check(bool, "types before the arrow", true, t_score < t_draw);
  check(
    bool,
    "arrow's terminal blooms as it lands",
    true,
    t_term > t_draw && t_term <= t_pill,
  );
  check(bool, "pill after the arrow", true, t_pill > t_draw);
  let (_, last) = List.nth(s.acts, List.length(s.acts) - 1);
  check(bool, "edge act is last", true, last.at == S.Edge("quiz"));
};

let batch = () => {
  let d =
    diff(
      ~added=
        List.init(14, i =>
          node("T" ++ string_of_int(i), float_of_int(i) *. 50., 0.)
        ),
      (),
    );
  let s = S.plan(~pos_of=pos_of_added(d), d);
  check(int, "one batch act", 1, List.length(s.acts));
  no_violations("batch", s);
  check(int, "all appear", 14, List.length(S.appear_times(s)));
};

let drift_last = () => {
  let d =
    diff(
      ~added=[node("A", 100., 100.)],
      ~moved=[
        ("B", p(300., 100.), p(400., 140.)),
        ("C", p(300., 200.), p(400., 260.)),
      ],
      (),
    );
  let pos_of = k =>
    switch (k) {
    | "B" => Some(p(400., 140.))
    | "C" => Some(p(400., 260.))
    | k => pos_of_added(d, k)
    };
  let s = S.plan(~pos_of, d);
  no_violations("drift", s);
  /* existing nodes make room before the arrival */
  let (_, first) = List.hd(s.acts);
  check(bool, "tidy act first", true, first.emote == S.Tidy);
  switch (S.drift_at(s)) {
  | Some(t) =>
    check(bool, "drift before the arrival", true, t < appear(s, "A"))
  | None => fail("no drift")
  };
};

let framing = () => {
  let pane = (800., 600.);
  let cur: S.frame = {
    center: p(400., 300.),
    zoom: 1.,
  };
  let all = [p(300., 300.), p(500., 300.), p(400., 200.)];
  /* everything already in view: hold still */
  check(
    bool,
    "hold when all visible",
    true,
    S.frame_for(~pane, ~cur, ~all, ~required=[p(500., 300.)]) == None,
  );
  /* the whole program fits but sits off-center: frame it */
  let far_cur: S.frame = {
    center: p(1400., 300.),
    zoom: 1.,
  };
  switch (S.frame_for(~pane, ~cur=far_cur, ~all, ~required=[p(500., 300.)])) {
  | Some(f) =>
    check(
      bool,
      "frames the whole program",
      true,
      abs_float(f.center.x -. 400.) < 1.,
    );
    check(bool, "never zooms in", true, f.zoom <= 1.);
  | None => fail("expected a frame")
  };
  /* a large program: required point off to the right -> minimal pan, same zoom */
  let big =
    List.init(40, i =>
      p(float_of_int(i) *. 100., float_of_int(i mod 7) *. 150.)
    );
  switch (S.frame_for(~pane, ~cur, ~all=big, ~required=[p(1000., 300.)])) {
  | Some(f) =>
    check(
      bool,
      "pans right just enough",
      true,
      f.center.x > 400. && f.center.x < 1000.,
    );
    check(bool, "keeps zoom", true, abs_float(f.zoom -. 1.) < 0.001);
    check(
      bool,
      "no vertical fidget",
      true,
      abs_float(f.center.y -. 300.) < 1.,
    );
  | None => fail("expected a pan")
  };
};

let tests = [
  test_case(
    "three types, terminals with their type, path monotonic",
    `Quick,
    three_types,
  ),
  test_case("the actor precedes every effect", `Quick, actor_first),
  test_case("edges after types, pill after arrow", `Quick, edge_after_types),
  test_case("more than 12 definitions is one batch act", `Quick, batch),
  test_case("a drift makes room before arrivals", `Quick, drift_last),
  test_case("framing: hold, frame whole, minimal pan", `Quick, framing),
];
