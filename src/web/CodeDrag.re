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
  cur: vec, /* text-box-local px */
  tgt: vec,
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

let dir_name: Action.Gesture.t => string =
  fun
  | Up => "up"
  | Down => "down"
  | Left => "left"
  | Right => "right";

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
      dir_name(c.dir),
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
    /* snap: reached the target — commit and chain */
    let d = sqrt((p.x -. c.tgt.x) ** 2. +. (p.y -. c.tgt.y) ** 2.);
    if (d <= snap_radius) {
      s.winner = None;
      s.t = 0.;
      s.cands = [];
      s.pending = AwaitChange(s.last_z, s.last_term);
      s.commit(c.dir);
    };
  | _ =>
    s.winner = None;
    s.t = 0.;
  };
};

/* === lifecycle === */

let end_session = () => {
  switch (session^) {
  | Some(s) => s.listeners |> List.iter(Dom.removeEventListener)
  | None => ()
  };
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
let sync =
    (
      ~info_map: Language.Statics.Map.t,
      ~term: Language.Exp.t,
      ~measured: Measured.t,
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
      let cands =
        Refactor.drag_candidates(~info_map, ~term, ~measured, z)
        |> List.map((c: Refactor.DragCandidate.t) =>
             {
               dir: c.dir,
               kind: c.kind,
               cur: px(c.current),
               tgt: px(c.target),
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
