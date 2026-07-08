open Util;
open Js_of_ocaml;
open Haz3lcore;

/* Drag-to-refactor: a pointer front-end to the gesture system
   (plans/refactor-drag.md). Candidates come from
   Refactor.drag_candidates — each is a TRACK from the grabbed
   construct's live position to its position in that candidate's
   layout. Per pointermove we pick the min-gap track (with stickiness)
   and, on reaching its end, dispatch the SAME RefactorGesture action
   the arrow key would — then re-enumerate from the new state so a
   held drag chains rung to rung.

   v0 shows tracks/targets in a body-level overlay (vdom-free) and
   commits on snap or release; the scrubbed whole-buffer preview
   layers on next. */

/* tuning */
let snap_radius = 14.; /* px: reaching a target commits (chain) */
let snap_dwell = 120.; /* ms inside the radius before the commit fires */
let snap_min_t = 0.7; /* progress required before a snap is armed */
/* TEMPORARY (andrew): mid-drag commits confuse while feeling out the
   basics — with chaining off, reaching a target just holds the full
   preview and RELEASE commits; flip back on to ride multiple rungs
   in one drag */
let chaining = false;
let when_far = 56.; /* px: farther than this from every track = no winner */
let stickiness = 10.; /* px bonus for the incumbent track */
let commit_t = 0.55; /* release past this progress commits the winner */
let slop = 4.; /* px before the drag counts as begun */

type vec = {
  x: float,
  y: float,
};

type cand = {
  dir: Action.Gesture.t,
  kind: Action.refactor,
  label: string,
  cur: vec, /* text-box-local px */
  tgt: vec,
  /* whole-buffer scrub: tokens displaced by this candidate, with
     their px deltas (built at compute; animations made lazily) */
  moved: list((CodeFlip.key, Js.t(Dom.node), float, float)),
};

/* Candidate enumeration must wait for the model to settle: on arm,
   until the caret reaches the clicked goal (or the zipper changes —
   clamped clicks land elsewhere); after a chain commit, until the
   zipper changes. Comparing zippers physically is sound: renders
   reuse the record unless the model changed. */
type pending =
  | Idle
  | AwaitGoal(Measured.Point.t, option(Zipper.t))
  | AwaitChange(option(Zipper.t), option(Language.Exp.t));

type session = {
  commit: Action.Gesture.t => unit,
  text_box: Js.t(Dom_html.element),
  mutable cands: list(cand),
  mutable winner: option(int),
  mutable t: float,
  mutable began: bool,
  mutable pending,
  mutable last_z: option(Zipper.t),
  mutable last_term: option(Language.Exp.t),
  mutable down_at: vec,
  mutable listeners: list(Dom.event_listener_id),
  /* paused WAAPI animations per candidate index, scrubbed by track
     progress (the pointer drives currentTime — lerpViews restricted
     to translation) */
  mutable scrub_anims: list((int, list(Js.Unsafe.any))),
  mutable scrub_active: option(int),
  /* dwell: (candidate index, entered-at ms) — a commit needs the
     pointer to LINGER in the snap radius, not just graze it */
  mutable snap_hover: option((int, float)),
};

let session: ref(option(session)) = ref(None);
let active = () => session^ != None;

/* === overlay (owned element under body; never vdom-managed) === */

let overlay_id = "code-drag-overlay";

let overlay_el = (): Js.t(Dom_html.element) =>
  switch (
    Js.Opt.to_option(
      Dom_html.document##getElementById(Js.string(overlay_id)),
    )
  ) {
  | Some(el) => el
  | None =>
    let el = Dom_html.createDiv(Dom_html.document);
    el##.id := Js.string(overlay_id);
    el##.style##.cssText :=
      Js.string("position:fixed;inset:0;pointer-events:none;z-index:999999;");
    Dom.appendChild(Dom_html.document##.body, el);
    el;
  };

let remove_overlay = () =>
  switch (
    Js.Opt.to_option(
      Dom_html.document##getElementById(Js.string(overlay_id)),
    )
  ) {
  | Some(el) => Js.Opt.iter(el##.parentNode, p => Dom.removeChild(p, el))
  | None => ()
  };

let box_origin = (s: session): vec => {
  let r = s.text_box##getBoundingClientRect;
  {
    x: r##.left,
    y: r##.top,
  };
};

let draw = (s: session, pointer: option(vec)) => {
  let o = box_origin(s);
  let seg = (i, c: cand) => {
    let win = s.winner == Some(i);
    let color = win ? "#e33" : "#59f";
    let width = win ? "2.5" : "1.5";
    Printf.sprintf(
      {|<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="%s" stroke-dasharray="%s"/>
        <circle cx="%f" cy="%f" r="5" fill="%s" fill-opacity="0.8"/>
        <text x="%f" y="%f" font-size="10" fill="%s">%s</text>|},
      o.x +. c.cur.x,
      o.y +. c.cur.y,
      o.x +. c.tgt.x,
      o.y +. c.tgt.y,
      color,
      width,
      win ? "" : "4 3",
      o.x +. c.tgt.x,
      o.y +. c.tgt.y,
      color,
      o.x +. c.tgt.x +. 7.,
      o.y +. c.tgt.y -. 4.,
      color,
      c.label,
    );
  };
  let ptr =
    switch (pointer) {
    | Some(p) =>
      Printf.sprintf(
        {|<circle cx="%f" cy="%f" r="3" fill="#333"/>|},
        p.x +. o.x,
        p.y +. o.y,
      )
    | None => ""
    };
  let svg =
    Printf.sprintf(
      {|<svg width="100%%" height="100%%">%s%s</svg>|},
      s.cands |> List.mapi(seg) |> String.concat("\n"),
      ptr,
    );
  overlay_el()##.innerHTML := Js.string(svg);
};

/* === scrub (pointer-driven whole-buffer preview) === */

let scrub_duration = 1000.; /* virtual ms; currentTime = t * this */

let now_ms = (): float => Js.Unsafe.global##.performance##now();

let cancel_anims = (anims: list(Js.Unsafe.any)): unit =>
  anims
  |> List.iter(a =>
       switch (Js.Unsafe.meth_call(a, "cancel", [||])) {
       | exception _ => ()
       | _ => ()
       }
     );

let scrub_clear = (s: session): unit => {
  s.scrub_anims |> List.iter(((_, anims)) => cancel_anims(anims));
  s.scrub_anims = [];
  s.scrub_active = None;
};

let make_anims = (c: cand): list(Js.Unsafe.any) =>
  c.moved
  |> List.filter_map(((_, node, dx, dy)) => {
       let keyframes =
         Js.Unsafe.obj([|
           (
             "transform",
             Js.Unsafe.inject(
               Js.array([|
                 Js.string("translate(0px, 0px)"),
                 Js.string(Printf.sprintf("translate(%fpx, %fpx)", dx, dy)),
               |]),
             ),
           ),
         |]);
       let options =
         Js.Unsafe.obj([|
           (
             "duration",
             Js.Unsafe.inject(Js.number_of_float(scrub_duration)),
           ),
           ("fill", Js.Unsafe.inject(Js.string("both"))),
           ("easing", Js.Unsafe.inject(Js.string("linear"))),
         |]);
       switch (
         Js.Unsafe.meth_call(
           node,
           "animate",
           [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
         )
       ) {
       | exception _ => None
       | anim =>
         switch (Js.Unsafe.meth_call(anim, "pause", [||])) {
         | exception _ => ()
         | _ => ()
         };
         Some(anim);
       };
     });

/* read a paused scrub animation's progress (0..1) */
let anim_t = (anims: list(Js.Unsafe.any)): float =>
  switch (anims) {
  | [] => 0.
  | [a, ..._] =>
    switch (Js.Unsafe.get(a, "currentTime")) {
    | exception _ => 0.
    | ct => Js.float_of_number(Js.Unsafe.coerce(ct)) /. scrub_duration
    }
  };

/* ease displaced tokens back to their natural positions from the
   scrub offset they currently carry — release/cancel/track-switch
   must CONTINUE motion, never restart it */
let relax_from = (c: cand, t: float): unit =>
  if (t > 0.01) {
    c.moved
    |> List.iter(((_, node, dx, dy)) => {
         let keyframes =
           Js.Unsafe.obj([|
             (
               "transform",
               Js.Unsafe.inject(
                 Js.array([|
                   Js.string(
                     Printf.sprintf(
                       "translate(%fpx, %fpx)",
                       dx *. t,
                       dy *. t,
                     ),
                   ),
                   Js.string("translate(0px, 0px)"),
                 |]),
               ),
             ),
           |]);
         let options =
           Js.Unsafe.obj([|
             ("duration", Js.Unsafe.inject(Js.number_of_float(150.))),
             ("easing", Js.Unsafe.inject(Js.string("ease-out"))),
           |]);
         switch (
           Js.Unsafe.meth_call(
             node,
             "animate",
             [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
           )
         ) {
         | exception _ => ()
         | _ => ()
         };
       });
  };

/* drive the winner's scrub to progress t; other tracks rest at 0 */
let scrub_to = (s: session, winner: option(int), t: float): unit => {
  switch (s.scrub_active, winner) {
  | (Some(prev), Some(w)) when prev == w => ()
  | (Some(prev), _) =>
    switch (List.assoc_opt(prev, s.scrub_anims)) {
    | Some(anims) =>
      switch (List.nth_opt(s.cands, prev)) {
      | Some(c) => relax_from(c, anim_t(anims))
      | None => ()
      };
      anims |> List.iter(a => Js.Unsafe.set(a, "currentTime", 0.));
    | None => ()
    }
  | (None, _) => ()
  };
  switch (winner) {
  | None => s.scrub_active = None
  | Some(w) =>
    let anims =
      switch (List.assoc_opt(w, s.scrub_anims)) {
      | Some(anims) => anims
      | None =>
        let anims =
          switch (List.nth_opt(s.cands, w)) {
          | Some(c) => make_anims(c)
          | None => []
          };
        s.scrub_anims = [(w, anims), ...s.scrub_anims];
        anims;
      };
    let time = t *. scrub_duration;
    anims |> List.iter(a => Js.Unsafe.set(a, "currentTime", time));
    s.scrub_active = Some(w);
  };
};

/* === geometry === */

let resolve = (s: session, p: vec): unit => {
  let track = (c: cand) => {
    let ax = c.cur.x
    and ay = c.cur.y;
    let bx = c.tgt.x
    and by = c.tgt.y;
    let dx = bx -. ax
    and dy = by -. ay;
    let len2 = dx *. dx +. dy *. dy;
    let t =
      len2 == 0.
        ? 0.
        : max(0., min(1., ((p.x -. ax) *. dx +. (p.y -. ay) *. dy) /. len2));
    let px = ax +. t *. dx
    and py = ay +. t *. dy;
    let gap = sqrt((p.x -. px) ** 2. +. (p.y -. py) ** 2.);
    (t, gap);
  };
  let scored = s.cands |> List.mapi((i, c) => (i, c, track(c)));
  let best =
    scored
    |> List.fold_left(
         (acc, (i, c, (t, gap))) => {
           let bonus = s.winner == Some(i) ? stickiness : 0.;
           switch (acc) {
           | Some((_, _, _, best_gap)) when gap -. bonus >= best_gap => acc
           | _ => Some((i, c, t, gap -. bonus))
           };
         },
         None,
       );
  switch (best) {
  | Some((i, c, t, gap)) when gap <= when_far =>
    s.winner = Some(i);
    s.t = t;
    scrub_to(s, Some(i), t);
    /* snap: linger at the target (dwell) with real progress — then
       commit and chain */
    let d = sqrt((p.x -. c.tgt.x) ** 2. +. (p.y -. c.tgt.y) ** 2.);
    if (chaining && d <= snap_radius && t >= snap_min_t) {
      switch (s.snap_hover) {
      | Some((j, since)) when j == i =>
        if (now_ms() -. since >= snap_dwell) {
          CodeFlip.set_drag_offsets(
            c.moved |> List.map(((k, _, dx, dy)) => (k, (dx, dy))),
          );
          CodeFlip.adopt(s.scrub_anims |> List.concat_map(snd));
          s.scrub_anims = [];
          s.scrub_active = None;
          s.winner = None;
          s.t = 0.;
          s.cands = [];
          s.snap_hover = None;
          s.pending = AwaitChange(s.last_z, s.last_term);
          s.commit(c.dir);
        }
      | _ => s.snap_hover = Some((i, now_ms()))
      };
    } else {
      s.snap_hover = None;
    };
  | _ =>
    s.winner = None;
    s.t = 0.;
    s.snap_hover = None;
    scrub_to(s, None, 0.);
  };
};

/* === lifecycle === */

let end_session = () => {
  switch (session^) {
  | Some(s) =>
    switch (s.scrub_active) {
    | Some(w) =>
      switch (List.assoc_opt(w, s.scrub_anims), List.nth_opt(s.cands, w)) {
      | (Some(anims), Some(c)) => relax_from(c, anim_t(anims))
      | _ => ()
      }
    | None => ()
    };
    scrub_clear(s);
    s.listeners |> List.iter(Dom.removeEventListener);
  | None => ()
  };
  Dom_html.document##.body##.style##.cursor := Js.string("");
  remove_overlay();
  session := None;
};

let local = (s: session, e: Js.t(Dom_html.event)): vec => {
  let o = box_origin(s);
  let cx: float = Js.Unsafe.coerce(e)##.clientX;
  let cy: float = Js.Unsafe.coerce(e)##.clientY;
  {
    x: cx -. o.x,
    y: cy -. o.y,
  };
};

let on_move = (e: Js.t(Dom_html.event)): unit =>
  switch (session^) {
  | None => ()
  | Some(s) =>
    let p = local(s, e);
    if (!s.began) {
      let d = sqrt((p.x -. s.down_at.x) ** 2. +. (p.y -. s.down_at.y) ** 2.);
      if (d > slop) {
        s.began = true;
        Dom_html.document##.body##.style##.cursor := Js.string("grabbing");
      };
    };
    if (s.began && s.pending == Idle) {
      resolve(s, p);
      draw(s, Some(p));
    };
  };

let on_up = (_e: Js.t(Dom_html.event)): unit =>
  switch (session^) {
  | None => ()
  | Some(s) =>
    switch (s.winner) {
    | Some(i) when s.t >= commit_t =>
      let c = List.nth(s.cands, i);
      /* handoff: the commit's FLIP starts each token from its
         scrubbed offset; the scrub animations are adopted so they
         die exactly when the flights take over */
      CodeFlip.set_drag_offsets(
        c.moved
        |> List.map(((k, _, dx, dy)) => (k, (dx *. s.t, dy *. s.t))),
      );
      CodeFlip.adopt(s.scrub_anims |> List.concat_map(snd));
      s.scrub_anims = [];
      s.scrub_active = None;
      s.commit(c.dir);
    | _ => ()
    };
    end_session();
  };

let listen = (name: string, f: Js.t(Dom_html.event) => unit) =>
  Dom_html.addEventListener(
    Dom_html.document,
    Dom.Event.make(name),
    Dom_html.handler(e => {
      f(e);
      Js._true;
    }),
    Js._true,
  );

let arm =
    (
      ~commit: Action.Gesture.t => unit,
      ~text_box: Js.t(Dom_html.element),
      ~client: (float, float),
      ~goal: Measured.Point.t,
    )
    : unit => {
  end_session();
  let (cx, cy) = client;
  let r = text_box##getBoundingClientRect;
  let s = {
    commit,
    text_box,
    cands: [],
    winner: None,
    t: 0.,
    began: false,
    pending: AwaitGoal(goal, None),
    last_z: None,
    last_term: None,
    scrub_anims: [],
    scrub_active: None,
    snap_hover: None,
    down_at: {
      x: cx -. r##.left,
      y: cy -. r##.top,
    },
    listeners: [],
  };
  s.listeners = [
    listen("pointermove", on_move),
    listen("pointerup", on_up),
    listen("keydown", e => {
      let key: string = Js.to_string(Js.Unsafe.coerce(e)##.key);
      if (key == "Escape") {
        end_session();
      };
    }),
  ];
  session := Some(s);
};

/* Called from the editor view on every render: when a drag session
   needs candidates (just armed, or just committed a chain step),
   enumerate them from the CURRENT model. Runs at most once per
   armed/committed state — not per frame. */
/* The live editor's .code-text children, paired with the segment's
   emission order (CodeFlip's correlation); located via the caret,
   which sits in the dragged editor during a session */
let live_pairs = (segment: Segment.t): list((CodeFlip.key, Js.t(Dom.node))) =>
  switch (JsUtil.get_elem_by_id_opt("caret")) {
  | None => []
  | Some(caret) =>
    switch (
      Js.Opt.to_option(caret##.parentNode)
      |> Option.map(deco => Js.Opt.to_option(deco##.parentNode))
      |> Option.join
    ) {
    | None => []
    | Some(container) =>
      let ct =
        Js.Unsafe.meth_call(
          container,
          "querySelector",
          [|Js.Unsafe.inject(Js.string(":scope > .code > .code-text"))|],
        );
      switch (Js.Opt.to_option(ct)) {
      | None => []
      | Some(ct) =>
        let nodes = Dom.list_of_nodeList(ct##.childNodes);
        switch (CodeFlip.pair(CodeFlip.entries_of_segment(segment), nodes)) {
        | Some(pairs) => pairs
        | None => []
        };
      };
    }
  };

let sync =
    (
      ~info_map: Language.Statics.Map.t,
      ~term: Language.Exp.t,
      ~measured: Measured.t,
      ~segment: Segment.t,
      ~font_metrics: FontMetrics.t,
      z: Zipper.t,
    )
    : unit =>
  switch (session^) {
  | None => ()
  | Some(s) =>
    let compute = () => {
      let px = (p: Measured.Point.t): vec => {
        x: float_of_int(p.col) *. font_metrics.col_width,
        y: (float_of_int(p.row) +. 0.5) *. font_metrics.row_height,
      };
      scrub_clear(s);
      let pairs = live_pairs(segment);
      /* tokens this candidate displaces, in px (single-row tokens
         only, matching CodeFlip's guard) */
      let moved_for = (cand_m: Measured.t) =>
        pairs
        |> List.filter_map(((k, node)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas(cand_m, k),
             ) {
             | (Some(o), Some(n))
                 when
                   o.origin != n.origin
                   && o.origin.row == o.last.row
                   && n.origin.row == n.last.row =>
               let dx =
                 float_of_int(n.origin.col - o.origin.col)
                 *. font_metrics.col_width;
               let dy =
                 float_of_int(n.origin.row - o.origin.row)
                 *. font_metrics.row_height;
               Some((k, node, dx, dy));
             | _ => None
             }
           );
      let cands =
        Refactor.drag_candidates(~info_map, ~term, ~measured, z)
        |> List.map((c: Refactor.DragCandidate.t) =>
             {
               dir: c.dir,
               kind: c.kind,
               label: c.label,
               cur: px(c.current),
               tgt: px(c.target),
               moved: moved_for(c.measured),
             }
           );
      s.cands = cands;
      s.last_z = Some(z);
      s.last_term = Some(term);
      if (cands == []) {
        /* possibly a render where statics lag the zipper — stay
           pending and recompute when the model moves again; release
           or Esc still ends the session */
        s.pending = AwaitChange(Some(z), Some(term));
        remove_overlay();
      } else {
        s.pending = Idle;
        draw(s, None);
      };
    };
    switch (s.pending) {
    | Idle => ()
    | AwaitGoal(goal, stale) =>
      let here = Zipper.Caret.point(measured, z) == goal;
      let changed =
        switch (stale) {
        | Some(z0) => !(z0 === z)
        | None => false
        };
      if (here || changed) {
        compute();
      } else {
        s.pending = AwaitGoal(goal, Some(z));
      };
    | AwaitChange(z0, t0) =>
      let changed =
        switch (z0, t0) {
        | (Some(z0), Some(t0)) => !(z0 === z) || !(t0 === term)
        | (Some(z0), None) => !(z0 === z)
        | (None, _) => true
        };
      if (changed) {
        compute();
      };
    };
  };
