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
  | Reveal(string) /* edge name: its path draws on, then its pill — no ride
                      (the "and the rest" act when there are more new edges
                      than the actor rides) */
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
  /* A2 without exception: every definition and every edge is its own act
     (2026-09-06, andrew: a batch at a centroid reads as "functions
     appearing with the agent standing still"). A long beat is bounded by
     COMPRESSING the tempo toward [floor], never by merging acts. */
  budget_ms: int, /* compress when the beat would run longer than this */
  floor: float /* the smallest compression factor (trial 6: 1.1 s/act was "boom boom boom") */
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
  budget_ms: 24000,
  floor: 0.6,
};

/* the same tempo, every phase scaled by [f] (the ride's own minimum in
   edge_act scales with travel) */
let compress = (t: tempo, f: float): tempo => {
  let sc = x => max(1, int_of_float(float_of_int(x) *. f));
  {
    ...t,
    travel_min: sc(t.travel_min),
    travel_max: sc(t.travel_max),
    px_per_ms: t.px_per_ms /. f,
    pause: sc(t.pause),
    effect: sc(t.effect),
    settle: sc(t.settle),
    bloom_gap: sc(t.bloom_gap),
    min_gap: sc(t.min_gap),
    drift_ms: sc(t.drift_ms),
  };
};

/* ---------------- inputs ---------------- */

type new_node = {
  key: string,
  anchor: option(string), /* a terminal's anchor node, if it is one */
  p: pos,
  /* the story's order: a named definition's place in the program; an
     unnamed glyph (a builtin, a product) the place of the first function
     that needs it; [max_int] when nothing says. Acts follow this, not the
     screen position (2026-09-06: the score opened on `(Int, Int)` at the
     far left instead of the module the agent wrote). */
  order: int,
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
  removed_edges: list((string, pos)), /* name, a point on the arrow */
  /* elements whose LOOK changed (a hole filled, a constructor added, an
     error fixed, a type changed): key or edge name, and where to touch */
  changed: list((string, pos)),
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
    /* program order first; the screen's reading order breaks ties */
    |> List.sort((a: new_node, b: new_node) =>
         compare((a.order, a.p.x, a.p.y), (b.order, b.p.x, b.p.y))
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

/* more new edges than the actor rides: one act at their centroid reveals
   them all, staggered — every new edge is scheduled by the score, none is
   left to appear with the render (the illegal "labels before edges" state) */
let rest_act =
    (
      ~tempo,
      ~cause,
      ~from: option(pos),
      ~pos_of: string => option(pos),
      es: list(new_edge),
    )
    : act => {
  let dsts = List.filter_map((e: new_edge) => pos_of(e.dst), es);
  let c = centroid(dsts);
  let travel_ms =
    switch (from, c) {
    | (Some(f), Some(c)) => travel_for(~tempo, dist(f, c))
    | _ => tempo.travel_min
    };
  let t0 = travel_ms + tempo.pause;
  {
    cause,
    at: Centroid(List.map((e: new_edge) => e.dst, es)),
    emote: Edit,
    travel_ms,
    pause_ms: tempo.pause,
    effects:
      List.mapi(
        (i, e: new_edge) =>
          {
            effect: Reveal(e.name),
            at: t0 + 150 * i,
            dur: tempo.effect,
          },
        es,
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
      /* 400 ms per part visited, then the lines draw in (form_ms) */
      let dur = 400 * List.length(parts) + 320;
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
    | (Some(s), Some(d)) =>
      max(tempo.travel_min + 250, travel_for(~tempo, dist(s, d) *. 1.6))
    | _ => tempo.travel_max
    };
  /* a terminal this edge lands on (or leaves from) blooms as it arrives */
  let ends =
    List.filter_map(
      (n: new_node) =>
        n.key == e.dst
          ? Some({
              effect: Appear(n.key),
              at: t_draw + ride,
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
      {
        effect: Ripple(Node(k)),
        at: t0 + 60,
        dur: 0,
      },
    ],
    settle_ms: tempo.settle,
  };
};

/* an arrow is erased at its label: the actor travels there, the line
   retracts and the pill fades */
let erase_act =
    (~tempo, ~cause, ~from: option(pos), (name, p): (string, pos)): act => {
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
        effect: Erase(name),
        at: t0,
        dur: tempo.effect + 200,
      },
      {
        effect: Ripple(Point(p)),
        at: t0 + 60,
        dur: 0,
      },
    ],
    settle_ms: tempo.settle,
  };
};

/* a changed element keeps its old look until the actor touches it: the
   swap and a pulse are the effect */
let change_act =
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
    emote: Edit,
    travel_ms,
    pause_ms: tempo.pause,
    effects: [
      {
        effect: Change(k),
        at: t0,
        dur: tempo.effect,
      },
      {
        effect: Ripple(Point(p)),
        at: t0 + 60,
        dur: 0,
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

let rec plan =
        (~tempo=tempo, ~pos_of: string => option(pos), diff: diff): score => {
  let s = plan_at(~tempo, ~pos_of, diff);
  /* over budget: the same acts, compressed — never merged (A2) */
  if (s.total_ms > tempo.budget_ms && tempo.floor < 1.) {
    let f =
      max(
        tempo.floor,
        float_of_int(tempo.budget_ms) /. float_of_int(s.total_ms),
      );
    plan_at(~tempo=compress(tempo, f), ~pos_of, diff);
  } else {
    s;
  };
}
and plan_at = (~tempo, ~pos_of: string => option(pos), diff: diff): score => {
  let cause = diff.d_cause;
  let pos_of' = k =>
    switch (pos_of(k)) {
    | Some(p) => Some(p)
    | None =>
      switch (List.find_opt((n: new_node) => n.key == k, diff.added)) {
      | Some(n) => Some(n.p)
      | None => List.assoc_opt(k, diff.removed)
      }
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
  let edges = diff.edges;
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
  /* removals: the board keeps the removed elements as ghosts until their
     act (CanvasSidebar's leaving set), so the actor erases something that
     is still there — arrows first (a function goes before its type),
     then nodes, each with its own act */
  let removals =
    List.map(
      e => mk(from => erase_act(~tempo, ~cause, ~from, e)),
      diff.removed_edges,
    )
    @ List.map(
        r => mk(from => removal_act(~tempo, ~cause, ~from, r)),
        diff.removed,
      );
  let changes =
    List.map(
      c => mk(from => change_act(~tempo, ~cause, ~from, c)),
      diff.changed,
    );
  let additions =
    List.map(
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
  /* existing nodes make room FIRST when something new arrives, so a new
     node's lines meet nodes that are already where they will be; a beat
     that only moves things is a closing tidy */
  ignore(rest_act);
  let (acts, total_ms) =
    additions @ edge_acts @ leftover_orphans == []
      ? sequence(removals @ changes @ drift)
      : sequence(
          removals @ changes @ drift @ additions @ edge_acts @ leftover_orphans,
        );
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

/* the whole program is shown whenever it fits at this zoom or better —
   a node cut off at the pane's edge is worse than smaller glyphs
   (2026-09-06: at 0.74 the rule fell to a pan that halved `Kind`) */
let whole_fit_min = 0.6;
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
      /* wanted in view when they fit (C2's unexposed nodes); a set the
         view cannot hold at the minimum zoom made the minimal pan chase
         one edge then the other, every act */
      ~soft: list(pos)=[],
      (),
    )
    : option(frame) => {
  let (w, h) = pane;
  let pad = 44.;
  let fit_of = (b: bbox) =>
    min(
      (w -. 2. *. frame_margin) /. max(1., b.x1 -. b.x0),
      (h -. 2. *. frame_margin) /. max(1., b.y1 -. b.y0),
    );
  let with_soft = bbox_of(~pad, required @ soft);
  let required =
    switch (with_soft) {
    | Some(b) when fit_of(b) >= 0.5 => required @ soft
    | _ => required
    };
  switch (bbox_of(~pad, required)) {
  | None => None
  | Some(req) =>
    /* the program's box is its NODES padded by a node's size — the same
       box the camera's own whole-program framing judges by; the act's
       targets (label points, arrow ends) only have to be in view */
    let whole = bbox_of(~pad=32., all);
    let fit = (b: bbox) =>
      min(
        (w -. 2. *. frame_margin) /. max(1., b.x1 -. b.x0),
        (h -. 2. *. frame_margin) /. max(1., b.y1 -. b.y0),
      );
    let same = (a: frame, b: frame) =>
      dist(a.center, b.center) < 8. && abs_float(a.zoom -. b.zoom) < 0.02;
    /* the whole program is framed as tightly as the camera frames it
       (12 px), so the two never disagree about its zoom */
    let fit_tight = (b: bbox) =>
      min(
        (w -. 24.) /. max(1., b.x1 -. b.x0),
        (h -. 24.) /. max(1., b.y1 -. b.y0),
      );
    let whole_frame =
      switch (whole) {
      | Some(wb) when fit_tight(wb) >= whole_fit_min =>
        Some({
          center: bbox_center(wb),
          zoom: min(1., fit_tight(wb)),
        })
      | _ => None
      };
    switch (whole_frame) {
    | Some(f) =>
      /* the whole program fits: hold while it (and the act's targets) is
         in view; otherwise frame it — never zooming IN mid-score (the
         zoom is a slow variable; the camera eases in between bursts) */
      let vr = view_rect(~pane, cur);
      if (contains(~margin=0., vr, Option.get(whole))
          && contains(~margin=0., vr, req)) {
        None;
      } else {
        let f = {
          ...f,
          zoom: min(cur.zoom, f.zoom),
        };
        same(cur, f) ? None : Some(f);
      };
    | None =>
      let vr = view_rect(~pane, cur);
      /* hysteresis: a target that is in view, even inside the margin
         band, holds the camera — panning it to the band's edge on every
         act made a 26-act score rock 20 px left and right per act */
      if (contains(~margin=6., vr, req)) {
        None;
      } else {
        /* zoom out only if the required set cannot fit at this zoom; and
           when everything currently in view plus the required set fits
           at a readable zoom, keep it all rather than pan a node off the
           edge (C1: the most other content in frame) */
        let in_view =
          List.filter(
            (p: pos) =>
              p.x >= vr.x0 && p.x <= vr.x1 && p.y >= vr.y0 && p.y <= vr.y1,
            all,
          );
        let keep = bbox_of(~pad=32., in_view @ required);
        /* only when the viewer had the WHOLE program in view: then losing
           a node off the edge is the greater harm. A large program pans. */
        let had_whole = List.length(in_view) == List.length(all);
        switch (keep) {
        | Some(kb)
            when
              had_whole
              && !contains(~margin=frame_margin, vr, kb)
              && fit(kb) >= 0.5
              && fit(kb) < cur.zoom =>
          let f = {
            center: bbox_center(kb),
            zoom: fit(kb),
          };
          same(cur, f) ? None : Some(f);
        | _ =>
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
        | Pill(name)
        | Reveal(name) =>
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
  /* C3: the beat is framed WHOLE before its first act — everything its
     acts will touch, in one view — and held; per-act frames only when an
     act's targets leave it (a 26-act module insert used to pan 200–500 px
     on every act, following the actor around) */
  let beat_targets =
    List.concat_map(((_, a)) => act_targets(~pos_of, a), s.acts);
  let (frames0, cur) =
    switch (
      frame_for(
        ~pane,
        ~cur,
        ~all,
        ~required=beat_targets,
        ~soft=unexposed,
        (),
      )
    ) {
    | Some(f) => ([(0, f)], f)
    | None => ([], cur)
    };
  let (frames, _) =
    List.fold_left(
      ((acc, cur), (t, a)) => {
        let required = act_targets(~pos_of, a);
        switch (frame_for(~pane, ~cur, ~all, ~required, ())) {
        | Some(f) => ([(t, f), ...acc], f)
        | None => (acc, cur)
        };
      },
      (frames0, cur),
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
/* when each new node becomes visible: Appear at its cue; a product formed
   by an edge act grows at the END of the formation (the player visits
   the parts first, then the dot scales in over its last 320 ms) */
let appear_times = (s: score): list((string, int)) =>
  List.filter_map(
    ((t, _, e: timed_effect)) =>
      switch (e.effect) {
      | Appear(k) => Some((k, t))
      | Form(pk, _) => Some((pk, t + e.dur - 320))
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

/* A2's coverage: every element the diff adds is staged by some effect of
   the score (Appear/Form for nodes, Draw/Reveal for edges); anything else
   would appear at render with no actor — an ORPHAN. Removals are listed
   until removal choreography exists (they vanish at render today). */
let coverage = (diff: diff, s: score): list(string) => {
  let effects =
    List.map(((_, _, e: timed_effect)) => e.effect, effects_abs(s));
  let node_staged = k =>
    List.exists(
      fun
      | Appear(k') => k' == k
      | Form(pk, _) => pk == k
      | _ => false,
      effects,
    );
  let edge_staged = n =>
    List.exists(
      fun
      | Draw(n')
      | Reveal(n') => n' == n
      | _ => false,
      effects,
    );
  List.filter_map(
    (n: new_node) =>
      node_staged(n.key)
        ? None : Some("ORPHAN: node " ++ n.key ++ " unstaged"),
    diff.added,
  )
  @ List.filter_map(
      (e: new_edge) =>
        edge_staged(e.name)
          ? None : Some("ORPHAN: edge " ++ e.name ++ " unstaged"),
      diff.edges,
    )
  @ List.filter_map(
      ((k, _)) =>
        List.exists(
          fun
          | Vanish(k') => k' == k
          | _ => false,
          effects,
        )
          ? None : Some("ORPHAN: removal " ++ k ++ " unstaged"),
      diff.removed,
    )
  @ List.filter_map(
      ((n, _)) =>
        List.exists(
          fun
          | Erase(n') => n' == n
          | _ => false,
          effects,
        )
          ? None : Some("ORPHAN: erased edge " ++ n ++ " unstaged"),
      diff.removed_edges,
    )
  @ List.filter_map(
      ((k, _)) =>
        List.exists(
          fun
          | Change(k') => k' == k
          | _ => false,
          effects,
        )
          ? None : Some("ORPHAN: change of " ++ k ++ " unstaged"),
      diff.changed,
    );
};

/* absolute cue times of the Change effects (the staged old looks swap
   then) */
let change_times = (s: score): list((string, int)) =>
  List.filter_map(
    ((t, _, e: timed_effect)) =>
      switch (e.effect) {
      | Change(k) => Some((k, t))
      | _ => None
      },
    effects_abs(s),
  );

/* ---------------- journal ---------------- */

/* a Point site is named by what the act does there (its first targeted
   effect), so the journal reads "edit@damage" not "edit@·" */
let act_target_name = (a: act): option(string) =>
  List.find_map(
    (e: timed_effect) =>
      switch (e.effect) {
      | Change(k)
      | Vanish(k)
      | Appear(k) => Some(k)
      | Erase(n)
      | Draw(n)
      | Reveal(n) => Some(n)
      | _ => None
      },
    a.effects,
  );

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
    switch (a.at, act_target_name(a)) {
    | (Point(_), Some(n)) => n
    | _ => site_to_string(a.at)
    },
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
