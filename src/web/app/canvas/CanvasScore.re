/* CanvasScore — the staging language. A beat's choreography as a pure,
   validated value built from the graph diff BEFORE anything animates.
   CanvasEnact plays it, Animation takes its arrival times from it, the
   camera its frames. One score per beat; the actor is the only cause;
   effects never precede the actor's arrival. Principles and the shape's
   future (agent-authored emotes/say cues): plans/agent-canvas-principles.md */

type pos = CanvasLayout.pos;

/* where on the board the actor stands */
type site =
  | Node(string)
  | Edge(string)
  | Point(pos)
  | Centroid(list(string))
  | Here;

/* the actor's visible state during an act */
type emote =
  | Think
  | Edit
  | Erase
  | Tidy
  | Look
  | Rest;

/* a change to canvas content */
type effect =
  | Appear(string) /* node key grows in */
  | Form(string, list(string)) /* product dot forms from its parts */
  | Draw(string) /* edge name: the arrow is pulled along its path */
  | Pill(string) /* edge label settles in */
  | Vanish(string) /* node key shrinks out */
  | Erase(string) /* edge name retracts */
  | Change(string) /* cue on a modified element */
  | Drift /* existing nodes glide to their new places */
  | Ripple(site)
  | Say(string);

type timed_effect = {
  effect,
  at: int, /* ms from the act's start */
  dur: int,
};

/* one cause, one actor, one place: travel -> pause -> effects -> settle */
type act = {
  cause: string,
  at: site,
  emote,
  travel_ms: int,
  pause_ms: int,
  effects: list(timed_effect),
  settle_ms: int,
};

type frame = {
  center: pos,
  zoom: float,
};

type score = {
  cause: string,
  acts: list((int, act)), /* absolute start ms, non-decreasing */
  frames: list((int, frame)), /* camera cues, leading the act's travel */
  total_ms: int,
};

type tempo = {
  travel_min: int,
  travel_max: int,
  px_per_ms: float,
  pause: int,
  effect: int,
  settle: int,
  bloom_gap: int, /* a type's terminals bloom this far apart */
  min_gap: int, /* between effects of different acts */
  drift_ms: int,
  batch_over: int, /* more definitions than this = one batch act */
  max_edge_acts: int,
};

/* 2026-09-04: slowed after trial 6 ("boom boom boom") — about 1.9 s per act */
let tempo = {
  travel_min: 450,
  travel_max: 900,
  px_per_ms: 1.0,
  pause: 500,
  effect: 320,
  settle: 450,
  bloom_gap: 120,
  min_gap: 150,
  drift_ms: 700,
  batch_over: 12,
  max_edge_acts: 3,
};

/* ---------------- inputs ---------------- */

type new_node = {
  key: string,
  anchor: option(string), /* a terminal's anchor node, if it is one */
  p: pos,
};

type new_edge = {
  name: string,
  src: string,
  dst: string,
  product: option((string, list(string))) /* new anonymous product source */
};

type diff = {
  d_cause: string,
  added: list(new_node),
  edges: list(new_edge),
  removed: list((string, pos)),
  moved: list((string, pos, pos)), /* key, from, to */
  actor: option(pos),
};

/* ---------------- geometry helpers ---------------- */

let dist = (a: pos, b: pos): float =>
  sqrt((a.x -. b.x) *. (a.x -. b.x) +. (a.y -. b.y) *. (a.y -. b.y));

let travel_for = (~tempo, d: float): int =>
  max(
    tempo.travel_min,
    min(tempo.travel_max, int_of_float(d /. tempo.px_per_ms)),
  );

let centroid = (ps: list(pos)): option(pos) =>
  switch (ps) {
  | [] => None
  | _ =>
    let n = float_of_int(List.length(ps));
    Some({
      x: List.fold_left((s, p: pos) => s +. p.x, 0., ps) /. n,
      y: List.fold_left((s, p: pos) => s +. p.y, 0., ps) /. n,
    });
  };

let site_pos = (~pos_of: string => option(pos), s: site): option(pos) =>
  switch (s) {
  | Node(k) => pos_of(k)
  | Edge(_) => None
  | Point(p) => Some(p)
  | Centroid(ks) => centroid(List.filter_map(pos_of, ks))
  | Here => None
  };

let act_len = (a: act): int => {
  let body =
    List.fold_left(
      (m, e: timed_effect) => max(m, e.at + e.dur),
      a.travel_ms + a.pause_ms,
      a.effects,
    );
  body + a.settle_ms;
};

/* ---------------- planning ---------------- */

/* a definition = a new primary node with the new terminals anchored on it */
type definition = {
  primary: new_node,
  terminals: list(new_node),
};

let definitions = (added: list(new_node)): list(definition) => {
  let is_new = k => List.exists((n: new_node) => n.key == k, added);
  let primaries =
    List.filter(
      (n: new_node) =>
        switch (n.anchor) {
        | None => true
        | Some(a) => !is_new(a) /* an orphan terminal leads its own act */
        },
      added,
    )
    /* reading order: left to right, then top to bottom */
    |> List.sort((a: new_node, b: new_node) =>
         compare((a.p.x, a.p.y), (b.p.x, b.p.y))
       );
  List.map(
    (p: new_node) =>
      {
        primary: p,
        terminals:
          List.filter(
            (n: new_node) => n.anchor == Some(p.key) && n.key != p.key,
            added,
          ),
      },
    primaries,
  );
};

let definition_act = (~tempo, ~cause, ~from: option(pos), d: definition): act => {
  let travel_ms =
    switch (from) {
    | Some(f) => travel_for(~tempo, dist(f, d.primary.p))
    | None => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  {
    cause,
    at: Node(d.primary.key),
    emote: Edit,
    travel_ms,
    pause_ms: tempo.pause,
    effects:
      [
        {
          effect: Appear(d.primary.key),
          at: t0,
          dur: tempo.effect,
        },
        {
          effect: Ripple(Node(d.primary.key)),
          at: t0 + 60,
          dur: 0,
        },
      ]
      @ List.mapi(
          (i, t: new_node) =>
            {
              effect: Appear(t.key),
              at: t0 + tempo.bloom_gap * (i + 1),
              dur: tempo.effect,
            },
          d.terminals,
        ),
    settle_ms: tempo.settle,
  };
};

/* every definition at once: still an actor, still a place, one stagger */
let batch_act =
    (~tempo, ~cause, ~from: option(pos), ds: list(definition)): act => {
  let keys = List.map((d: definition) => d.primary.key, ds);
  let c = centroid(List.map((d: definition) => d.primary.p, ds));
  let travel_ms =
    switch (from, c) {
    | (Some(f), Some(c)) => travel_for(~tempo, dist(f, c))
    | _ => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  let all =
    List.concat_map((d: definition) => [d.primary, ...d.terminals], ds);
  {
    cause,
    at: Centroid(keys),
    emote: Edit,
    travel_ms,
    pause_ms: tempo.pause,
    effects:
      List.mapi(
        (i, n: new_node) =>
          {
            effect: Appear(n.key),
            at: t0 + 80 * i,
            dur: tempo.effect,
          },
        all,
      ),
    settle_ms: tempo.settle,
  };
};

let edge_act =
    (
      ~tempo,
      ~cause,
      ~from: option(pos),
      ~pos_of: string => option(pos),
      ~orphans: list(new_node),
      e: new_edge,
    )
    : act => {
  let sp = pos_of(e.src)
  and dp = pos_of(e.dst);
  let travel_ms =
    switch (from, sp) {
    | (Some(f), Some(s)) => travel_for(~tempo, dist(f, s))
    | _ => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  let (form, t_draw) =
    switch (e.product) {
    | Some((pk, parts)) =>
      let dur = 240 * List.length(parts) + 320;
      (
        [
          {
            effect: Form(pk, parts),
            at: t0,
            dur,
          },
        ],
        t0 + dur + 80,
      );
    | None => ([], t0)
    };
  let ride =
    switch (sp, dp) {
    | (Some(s), Some(d)) => max(700, travel_for(~tempo, dist(s, d) *. 1.6))
    | _ => 900
    };
  /* a terminal this edge lands on (or leaves from) blooms as it arrives */
  let ends =
    List.filter_map(
      (n: new_node) =>
        n.key == e.dst
          ? Some({
              effect: Appear(n.key),
              at: t_draw + ride - 120,
              dur: tempo.effect,
            })
          : n.key == e.src
              ? Some({
                  effect: Appear(n.key),
                  at: t0,
                  dur: tempo.effect,
                })
              : None,
      orphans,
    );
  {
    cause,
    at: Edge(e.name),
    emote: Edit,
    travel_ms,
    pause_ms: tempo.pause,
    effects:
      form
      @ ends
      @ [
        {
          effect: Draw(e.name),
          at: t_draw,
          dur: ride,
        },
        {
          effect: Pill(e.name),
          at: t_draw + ride,
          dur: tempo.effect,
        },
      ],
    settle_ms: tempo.settle,
  };
};

let removal_act =
    (~tempo, ~cause, ~from: option(pos), (k, p): (string, pos)): act => {
  let travel_ms =
    switch (from) {
    | Some(f) => travel_for(~tempo, dist(f, p))
    | None => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  {
    cause,
    at: Point(p),
    emote: Erase,
    travel_ms,
    pause_ms: tempo.pause,
    effects: [
      {
        effect: Vanish(k),
        at: t0,
        dur: tempo.effect,
      },
    ],
    settle_ms: tempo.settle,
  };
};

let drift_act = (~tempo, ~cause, ~from: option(pos), moved): act => {
  let keys = List.map(((k, _, _)) => k, moved);
  let c = centroid(List.map(((_, _, to_)) => to_, moved));
  let travel_ms =
    switch (from, c) {
    | (Some(f), Some(c)) =>
      min(tempo.travel_max, travel_for(~tempo, dist(f, c)))
    | _ => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  {
    cause,
    at: Centroid(keys),
    emote: Tidy,
    travel_ms,
    pause_ms: tempo.pause,
    effects: [
      {
        effect: Ripple(Centroid(keys)),
        at: t0,
        dur: 0,
      },
      {
        effect: Drift,
        at: t0,
        dur: tempo.drift_ms,
      },
    ],
    settle_ms: tempo.settle,
  };
};

/* where the actor ends an act (for the next act's travel) */
let act_end_pos = (~pos_of, a: act): option(pos) =>
  switch (a.at) {
  | Edge(name) =>
    /* the ride ends at the codomain: the last Draw's dst is unknown here,
       so callers pass pos_of that resolves edge names to their dst */
    pos_of("edge-end:" ++ name)
  | s => site_pos(~pos_of, s)
  };

/* lay the acts out in time, each starting when the previous settles */
let sequence = (acts: list(act)): (list((int, act)), int) => {
  let (rev, t) =
    List.fold_left(
      ((acc, t), a) => ([(t, a), ...acc], t + act_len(a)),
      ([], 0),
      acts,
    );
  (List.rev(rev), t);
};

let plan = (~tempo=tempo, ~pos_of: string => option(pos), diff: diff): score => {
  let cause = diff.d_cause;
  let pos_of' = k =>
    switch (pos_of(k)) {
    | Some(p) => Some(p)
    | None =>
      List.find_opt((n: new_node) => n.key == k, diff.added)
      |> Option.map((n: new_node) => n.p)
    };
  /* a new anonymous product that sources a new edge is formed by that
     edge's act (Form: visit the parts, lines draw in, the dot grows) —
     not a definition of its own */
  let formed =
    List.filter_map(
      (e: new_edge) => Option.map(fst, e.product),
      diff.edges,
    );
  let defs =
    definitions(
      List.filter((n: new_node) => !List.mem(n.key, formed), diff.added),
    );
  let edge_keys =
    List.concat_map((e: new_edge) => [e.src, e.dst], diff.edges);
  /* terminals with an old anchor that a new edge touches ride that edge */
  let (edge_orphans, own_defs) =
    List.partition(
      (d: definition) =>
        d.terminals == []
        && d.primary.anchor != None
        && List.mem(d.primary.key, edge_keys),
      defs,
    );
  let orphans = List.map((d: definition) => d.primary, edge_orphans);
  let edges = List.length(diff.edges) > tempo.max_edge_acts ? [] : diff.edges;
  /* threading the actor's position through the acts */
  let from = ref(diff.actor);
  let mk = f => {
    let a = f(from^);
    let end_ =
      switch (a.at) {
      | Edge(name) =>
        List.find_opt((e: new_edge) => e.name == name, diff.edges)
        |> Util.OptUtil.and_then((e: new_edge) => pos_of'(e.dst))
      | s => site_pos(~pos_of=pos_of', s)
      };
    switch (end_) {
    | Some(p) => from := Some(p)
    | None => ()
    };
    a;
  };
  /* removals are not choreographed yet: the node is gone at render, so an
     actor visiting the empty spot would be an orphan gesture (docket:
     "removals — actor first, then the render") */
  let removals = {
    ignore(removal_act);
    ignore(diff.removed);
    [];
  };
  let additions =
    List.length(own_defs) > tempo.batch_over
      ? [mk(from => batch_act(~tempo, ~cause, ~from, own_defs))]
      : List.map(
          d => mk(from => definition_act(~tempo, ~cause, ~from, d)),
          own_defs,
        );
  let edge_acts =
    List.map(
      e =>
        mk(from =>
          edge_act(~tempo, ~cause, ~from, ~pos_of=pos_of', ~orphans, e)
        ),
      edges,
    );
  /* edges we do not enact still need their terminals to appear */
  let leftover_orphans =
    edges == [] && orphans != []
      ? [
        mk(from =>
          batch_act(
            ~tempo,
            ~cause,
            ~from,
            List.map(
              (n: new_node) =>
                {
                  primary: n,
                  terminals: [],
                },
              orphans,
            ),
          )
        ),
      ]
      : [];
  let drift =
    diff.moved == []
      ? [] : [mk(from => drift_act(~tempo, ~cause, ~from, diff.moved))];
  let (acts, total_ms) =
    sequence(removals @ additions @ edge_acts @ leftover_orphans @ drift);
  {
    cause,
    acts,
    frames: [],
    total_ms,
  };
};

/* ---------------- framing (C1–C5) ---------------- */

type bbox = {
  x0: float,
  y0: float,
  x1: float,
  y1: float,
};

let bbox_of = (~pad: float, ps: list(pos)): option(bbox) =>
  switch (ps) {
  | [] => None
  | [p, ...rest] =>
    Some(
      List.fold_left(
        (b, q: pos) =>
          {
            x0: min(b.x0, q.x -. pad),
            y0: min(b.y0, q.y -. pad),
            x1: max(b.x1, q.x +. pad),
            y1: max(b.y1, q.y +. pad),
          },
        {
          x0: p.x -. pad,
          y0: p.y -. pad,
          x1: p.x +. pad,
          y1: p.y +. pad,
        },
        rest,
      ),
    )
  };

let bbox_center = (b: bbox): pos => {
  x: (b.x0 +. b.x1) /. 2.,
  y: (b.y0 +. b.y1) /. 2.,
};

/* the viewport's board-space rect at a frame */
let view_rect = (~pane as (w, h): (float, float), f: frame): bbox => {
  let hw = w /. 2. /. f.zoom
  and hh = h /. 2. /. f.zoom;
  {
    x0: f.center.x -. hw,
    y0: f.center.y -. hh,
    x1: f.center.x +. hw,
    y1: f.center.y +. hh,
  };
};

let contains = (~margin: float, outer: bbox, inner: bbox): bool =>
  inner.x0 >= outer.x0
  +. margin
  && inner.x1 <= outer.x1
  -. margin
  && inner.y0 >= outer.y0
  +. margin
  && inner.y1 <= outer.y1
  -. margin;

let whole_fit_min = 0.75;
let frame_margin = 30.;

/* the camera's decision for one act: the required set (actor's site, the
   act's targets, anything not yet exposed) must be in frame; among frames
   that do it, the whole program if it fits at >= 0.75, else the smallest
   pan (and only then a zoom out). None = hold still. */
let frame_for =
    (
      ~pane: (float, float),
      ~cur: frame,
      ~all: list(pos),
      ~required: list(pos),
    )
    : option(frame) => {
  let (w, h) = pane;
  let pad = 44.;
  switch (bbox_of(~pad, required)) {
  | None => None
  | Some(req) =>
    let whole = bbox_of(~pad, all @ required);
    let fit = (b: bbox) =>
      min(
        (w -. 2. *. frame_margin) /. max(1., b.x1 -. b.x0),
        (h -. 2. *. frame_margin) /. max(1., b.y1 -. b.y0),
      );
    let same = (a: frame, b: frame) =>
      dist(a.center, b.center) < 8. && abs_float(a.zoom -. b.zoom) < 0.02;
    let whole_frame =
      switch (whole) {
      | Some(wb) when fit(wb) >= whole_fit_min =>
        Some({
          center: bbox_center(wb),
          zoom: min(1., fit(wb)),
        })
      | _ => None
      };
    switch (whole_frame) {
    | Some(f) =>
      /* the whole program fits: show it, but do not fidget */
      let vr = view_rect(~pane, cur);
      if (contains(~margin=frame_margin, vr, Option.get(whole))
          && abs_float(cur.zoom -. f.zoom) < 0.1) {
        None;
      } else if (same(cur, f)) {
        None;
      } else {
        Some(f);
      };
    | None =>
      let vr = view_rect(~pane, cur);
      if (contains(~margin=frame_margin, vr, req)) {
        None;
      } else {
        /* zoom out only if the required set cannot fit at this zoom */
        let zoom = min(cur.zoom, max(0.5, fit(req)));
        let f0 = {
          ...cur,
          zoom,
        };
        let vr = view_rect(~pane, f0);
        /* minimal pan: shift just enough to bring req inside the margin */
        let dx =
          req.x0 < vr.x0 +. frame_margin
            ? req.x0 -. (vr.x0 +. frame_margin)
            : req.x1 > vr.x1 -. frame_margin
                ? req.x1 -. (vr.x1 -. frame_margin) : 0.;
        let dy =
          req.y0 < vr.y0 +. frame_margin
            ? req.y0 -. (vr.y0 +. frame_margin)
            : req.y1 > vr.y1 -. frame_margin
                ? req.y1 -. (vr.y1 -. frame_margin) : 0.;
        let f = {
          center: {
            x: cur.center.x +. dx,
            y: cur.center.y +. dy,
          },
          zoom,
        };
        same(cur, f) ? None : Some(f);
      };
    };
  };
};

/* the positions an act needs in frame */
let act_targets = (~pos_of: string => option(pos), a: act): list(pos) => {
  let site = site_pos(~pos_of, a.at) |> Option.to_list;
  let eff =
    List.concat_map(
      (e: timed_effect) =>
        switch (e.effect) {
        | Appear(k)
        | Vanish(k)
        | Change(k) => Option.to_list(pos_of(k))
        | Form(pk, parts) => List.filter_map(pos_of, [pk, ...parts])
        | Draw(name)
        | Erase(name)
        | Pill(name) =>
          List.filter_map(pos_of, ["edge-src:" ++ name, "edge-end:" ++ name])
        | Drift
        | Say(_) => []
        | Ripple(s) => Option.to_list(site_pos(~pos_of, s))
        },
      a.effects,
    );
  site @ eff;
};

/* add camera cues: one per act at most, leading its travel. `exposed` =
   keys the viewer has already seen long enough; the rest are required. */
let with_frames =
    (
      ~pane: (float, float),
      ~cur: frame,
      ~pos_of: string => option(pos),
      ~all_keys: list(string),
      ~exposed: list(string),
      s: score,
    )
    : score => {
  let all = List.filter_map(pos_of, all_keys);
  let unexposed =
    List.filter_map(k => List.mem(k, exposed) ? None : pos_of(k), all_keys);
  let (frames, _) =
    List.fold_left(
      ((acc, cur), (t, a)) => {
        let required = act_targets(~pos_of, a) @ unexposed;
        switch (frame_for(~pane, ~cur, ~all, ~required)) {
        | Some(f) => ([(t, f), ...acc], f)
        | None => (acc, cur)
        };
      },
      ([], cur),
      s.acts,
    );
  {
    ...s,
    frames: List.rev(frames),
  };
};

/* ---------------- queries for the player ---------------- */

let effects_abs = (s: score): list((int, act, timed_effect)) =>
  List.concat_map(
    ((t, a)) =>
      List.map((e: timed_effect) => (t + e.at, a, e), a.effects),
    s.acts,
  )
  |> List.sort(((t1, _, _), (t2, _, _)) => compare(t1, t2));

/* node key -> absolute ms its grow-in starts */
let appear_times = (s: score): list((string, int)) =>
  List.filter_map(
    ((t, _, e: timed_effect)) =>
      switch (e.effect) {
      | Appear(k) => Some((k, t))
      | _ => None
      },
    effects_abs(s),
  );

let drift_at = (s: score): option(int) =>
  List.find_map(
    ((t, _, e: timed_effect)) =>
      switch (e.effect) {
      | Drift => Some(t)
      | _ => None
      },
    effects_abs(s),
  );

/* the actor's own timeline: (t, where, emote). Arrive at t_start+travel,
   hold through the effects, leave at the act's end. Rides (Draw) are
   expanded by the player, which can sample the rendered path. */
let actor_path =
    (~pos_of: string => option(pos), s: score): list((int, pos, emote)) =>
  List.concat_map(
    ((t, a)) => {
      let where =
        switch (a.at) {
        | Edge(name) => pos_of("edge-src:" ++ name)
        | site => site_pos(~pos_of, site)
        };
      switch (where) {
      | None => []
      | Some(p) => [
          (t + a.travel_ms, p, a.emote),
          (t + act_len(a) - a.settle_ms, p, a.emote),
        ]
      };
    },
    s.acts,
  );

/* ---------------- validation (the journal checks, pure) ---------------- */

let validate = (~tempo=tempo, s: score): list(string) => {
  let v = ref([]);
  let bad = msg => v := [msg, ...v^];
  /* act starts never decrease; each act starts after the previous ends */
  ignore(
    List.fold_left(
      (prev_end, (t, a)) => {
        if (t < prev_end) {
          bad(
            Printf.sprintf(
              "OVERLAP: act at %dms starts before %dms",
              t,
              prev_end,
            ),
          );
        };
        t + act_len(a);
      },
      0,
      s.acts,
    ),
  );
  /* the actor precedes every effect */
  List.iter(
    ((_, a)) =>
      List.iter(
        (e: timed_effect) =>
          if (e.at < a.travel_ms + a.pause_ms) {
            bad(
              Printf.sprintf(
                "ORPHAN: effect at %dms before the actor arrives (%dms)",
                e.at,
                a.travel_ms + a.pause_ms,
              ),
            );
          },
        a.effects,
      ),
    s.acts,
  );
  /* structural effects of different acts keep a minimum gap */
  let structural = (e: timed_effect) =>
    switch (e.effect) {
    | Appear(_)
    | Draw(_)
    | Vanish(_)
    | Erase(_)
    | Drift => true
    | _ => false
    };
  ignore(
    List.fold_left(
      (prev, (t, a, e)) =>
        if (structural(e)) {
          switch (prev) {
          | Some((pt, pa)) when pa !== a && t - pt < tempo.min_gap =>
            bad(
              Printf.sprintf(
                "CROWDED: effects %dms apart across acts (min %d)",
                t - pt,
                tempo.min_gap,
              ),
            );
            Some((t, a));
          | _ => Some((t, a))
          };
        } else {
          prev;
        },
      None,
      effects_abs(s),
    ),
  );
  /* frames lead their act and never run backwards */
  ignore(
    List.fold_left(
      (prev, (t, _)) => {
        if (t < prev) {
          bad(
            Printf.sprintf("FRAME: cue at %dms after one at %dms", t, prev),
          );
        };
        t;
      },
      0,
      s.frames,
    ),
  );
  if (s.total_ms > 60000) {
    bad(Printf.sprintf("LONG: %dms", s.total_ms));
  };
  List.rev(v^);
};

/* ---------------- journal ---------------- */

let site_to_string = (s: site): string =>
  switch (s) {
  | Node(k) => k
  | Edge(n) => "ƒ" ++ n
  | Point(_) => "·"
  | Centroid(ks) => Printf.sprintf("%d nodes", List.length(ks))
  | Here => "here"
  };

let emote_to_string = (e: emote): string =>
  switch (e) {
  | Think => "think"
  | Edit => "edit"
  | Erase => "erase"
  | Tidy => "tidy"
  | Look => "look"
  | Rest => "rest"
  };

let act_to_string = ((t, a): (int, act)): string => {
  let n = kind =>
    List.length(
      List.filter((e: timed_effect) => kind(e.effect), a.effects),
    );
  let appear =
    n(e =>
      switch (e) {
      | Appear(_) => true
      | _ => false
      }
    );
  let bits =
    (appear > 0 ? [Printf.sprintf("+%d", appear)] : [])
    @ (
      n(e =>
        switch (e) {
        | Draw(_) => true
        | _ => false
        }
      )
      > 0
        ? ["draw"] : []
    )
    @ (
      n(e =>
        switch (e) {
        | Vanish(_) => true
        | _ => false
        }
      )
      > 0
        ? ["vanish"] : []
    )
    @ (
      n(e =>
        switch (e) {
        | Drift => true
        | _ => false
        }
      )
      > 0
        ? ["drift"] : []
    );
  Printf.sprintf(
    "%.1fs %s@%s %s",
    float_of_int(t) /. 1000.,
    emote_to_string(a.emote),
    site_to_string(a.at),
    String.concat(" ", bits),
  );
};

let to_string = (s: score): string =>
  Printf.sprintf(
    "score[%s]: %d act(s) %.1fs, %d frame(s) — %s",
    s.cause,
    List.length(s.acts),
    float_of_int(s.total_ms) /. 1000.,
    List.length(s.frames),
    String.concat("; ", List.map(act_to_string, s.acts)),
  );
