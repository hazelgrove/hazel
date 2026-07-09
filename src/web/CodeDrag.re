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
/* Chaining: linger at a target (snap radius + dwell, with real
   progress) to COMMIT mid-drag and re-enumerate from the new state —
   multiple rungs in one hold. Re-enabled once the base settled
   (continuation, frames, exits/enters all in). */
let chaining = true;
let when_far = 56.; /* px: farther than this from every track = no winner */
let stickiness = 6.; /* px bonus for the incumbent track */
let direction_pull = 8.; /* px bonus for tracks aligned with the pull —
   near the shared origin, nearly-parallel tracks (extract vs swap at
   an acute angle) are indistinguishable by gap alone */

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
  scroll_rows: int, /* commit-time scroll bump (pinned extract) */
  /* whole-buffer scrub: tokens displaced by this candidate, with
     their px deltas (built at compute; animations made lazily) */
  moved: list((CodeFlip.key, Js.t(Dom.node), float, float)),
  /* anchored-deco delta by (anchor id, shard). Deco ELEMENTS are
     resolved at ACTIVATION, not compute: compute runs during view
     construction (pre-patch DOM), so elements collected there can be
     re-purposed by the patch (indication hopping tiles on the
     caret-placing click) and would ride stale deltas. */
  deco_delta: ((Id.t, option(int))) => option((float, float)),
  /* tokens absent in the candidate: they dissolve (scale+fade) in
     proportion to the pull — the let/=/in shell, the binder, the
     replaced use (andrew's shrink sketch; lerpViews' unmatched-key
     fade, pointer-driven) */
  exits: list(Js.t(Dom.node)),
  /* tokens absent LIVE: synthetic "before" versions of the REAL
     tokens (dragology's createSyntheticBefore) at their candidate
     positions, running THE grow-in (scale+fade) scrubbed by t; the
     commit's enter animation continues from the same opacity+scale.
     (text, token classes, destination, emerge-origin). With an
     origin (feed's copy emerging from the surviving def), the ghost
     TRAVELS origin->destination while growing — dragology's
     emergeFrom; without, it grows in place. */
  enters: list((CodeFlip.key, string, string, vec, option(vec))),
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
  /* dwell must fire for a STILL pointer too — resolve only runs on
     pointermove, so arming the hover schedules a re-check */
  mutable last_p: vec,
  mutable dwell_timer: option(Dom_html.timeout_id_safe),
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
  let xml_escape = (t: string): string =>
    t
    |> String.to_seq
    |> Seq.map(c =>
         switch (c) {
         | '&' => "&amp;"
         | '<' => "&lt;"
         | '>' => "&gt;"
         | c => String.make(1, c)
         }
       )
    |> List.of_seq
    |> String.concat("");
  /* incoming tokens: REAL-styled token spans (same classes as
     Code.view emits, inside a .code wrapper so the whole stylesheet
     pipeline applies) running the standard grow-in, scrubbed by t */
  let ghosts =
    switch (s.winner) {
    | Some(w) when s.t > 0.02 =>
      switch (List.nth_opt(s.cands, w)) {
      | Some(c) when c.enters != [] =>
        let scale = 0.1 +. 0.9 *. s.t;
        let spans =
          c.enters
          |> List.map(((_, text, cls, dest, origin)) => {
               let pos =
                 switch (origin) {
                 | Some(from) => {
                     x: from.x +. (dest.x -. from.x) *. s.t,
                     y: from.y +. (dest.y -. from.y) *. s.t,
                   }
                 | None => dest
                 };
               Printf.sprintf(
                 {|<span class="%s" style="position:absolute;left:%fpx;top:%fpx;opacity:%f;transform:scale(%f)">%s</span>|},
                 cls,
                 o.x +. pos.x,
                 o.y +. pos.y,
                 s.t,
                 scale,
                 xml_escape(text),
               );
             })
          |> String.concat("");
        Printf.sprintf(
          {|<div class="code" style="position:fixed;inset:0;pointer-events:none">%s</div>|},
          spans,
        );
      | _ => ""
      }
    | _ => ""
    };
  let svg =
    Printf.sprintf(
      {|<svg width="100%%" height="100%%">%s%s</svg>|},
      s.cands |> List.mapi(seg) |> String.concat("\n"),
      ptr,
    );
  overlay_el()##.innerHTML := Js.string(svg ++ ghosts);
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

let paused_anim =
    (node: Js.t(Dom.node), keyframes: Js.Unsafe.any): option(Js.Unsafe.any) => {
  let options =
    Js.Unsafe.obj([|
      ("duration", Js.Unsafe.inject(Js.number_of_float(scrub_duration))),
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
};

/* decos re-queried fresh from the (post-patch) DOM */
let deco_moves = (c: cand): list((Js.t(Dom.node), float, float)) =>
  CodeFlip.anchored_decos()
  |> List.filter_map(((id, shard, node)) =>
       c.deco_delta((id, shard))
       |> Option.map(((dx, dy)) => (node, dx, dy))
     );

let make_anims = (c: cand): list(Js.Unsafe.any) => {
  let moves =
    (c.moved |> List.map(((_, node, dx, dy)) => (node, dx, dy)))
    @ deco_moves(c)
    |> List.filter_map(((node, dx, dy)) =>
         paused_anim(
           node,
           Js.Unsafe.obj([|
             (
               "transform",
               Js.Unsafe.inject(
                 Js.array([|
                   Js.string("translate(0px, 0px)"),
                   Js.string(
                     Printf.sprintf("translate(%fpx, %fpx)", dx, dy),
                   ),
                 |]),
               ),
             ),
           |]),
         )
       );
  let fades =
    c.exits
    |> List.filter_map(node =>
         paused_anim(
           node,
           Js.Unsafe.obj([|
             (
               "opacity",
               Js.Unsafe.inject(
                 Js.array([|Js.string("1"), Js.string("0")|]),
               ),
             ),
             (
               "transform",
               Js.Unsafe.inject(
                 Js.array([|
                   Js.string("scale(1)"),
                   Js.string("scale(0.25)"),
                 |]),
               ),
             ),
           |]),
         )
       );
  moves @ fades;
};

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
let relax_exits = (c: cand, t: float): unit =>
  c.exits
  |> List.iter(node => {
       let keyframes =
         Js.Unsafe.obj([|
           (
             "opacity",
             Js.Unsafe.inject(
               Js.array([|
                 Js.string(Printf.sprintf("%f", 1. -. t)),
                 Js.string("1"),
               |]),
             ),
           ),
           (
             "transform",
             Js.Unsafe.inject(
               Js.array([|
                 Js.string(Printf.sprintf("scale(%f)", 1. -. 0.75 *. t)),
                 Js.string("scale(1)"),
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

let relax_from = (c: cand, t: float): unit =>
  if (t > 0.01) {
    relax_exits(c, t);
    (c.moved |> List.map(((_, node, dx, dy)) => (node, dx, dy)))
    @ deco_moves(c)
    |> List.iter(((node, dx, dy)) => {
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
    /* CANCEL the outgoing track's animations (don't park them at 0:
       a paused fill-both animation at t=0 is an ACTIVE identity
       transform, and later-created animations win compositing — so
       revisiting a track left the shared tokens frozen under the
       other track's identity). Exactly one track's animations exist
       at any time; the relax bridges the visual gap. */
    switch (List.assoc_opt(prev, s.scrub_anims)) {
    | Some(anims) =>
      switch (List.nth_opt(s.cands, prev)) {
      | Some(c) => relax_from(c, anim_t(anims))
      | None => ()
      };
      cancel_anims(anims);
      s.scrub_anims = List.remove_assoc(prev, s.scrub_anims);
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

let rec resolve = (s: session, p: vec): unit => {
  s.last_p = p;
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
    /* pull-direction affinity: cosine between (pointer - start) and
       the track direction */
    let plen = sqrt((p.x -. ax) ** 2. +. (p.y -. ay) ** 2.);
    let tlen = sqrt(len2);
    let cos_sim =
      plen < 6. || tlen == 0.
        ? 0. : ((p.x -. ax) *. dx +. (p.y -. ay) *. dy) /. (plen *. tlen);
    (t, gap -. direction_pull *. max(0., cos_sim));
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
          if (c.enters != []) {
            CodeFlip.set_drag_enter(1.0);
          };
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
      | _ =>
        s.snap_hover = Some((i, now_ms()));
        /* re-check after the dwell even if the pointer holds still */
        switch (s.dwell_timer) {
        | Some(id) => Dom_html.clearTimeout(id)
        | None => ()
        };
        s.dwell_timer =
          Some(
            Dom_html.setTimeout(
              () =>
                switch (session^) {
                | Some(s') when s' === s => resolve(s', s'.last_p)
                | _ => ()
                },
              snap_dwell +. 20.,
            ),
          );
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
    switch (s.dwell_timer) {
    | Some(id) => Dom_html.clearTimeout(id)
    | None => ()
    };
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
  Dom_html.document##.body##.classList##remove(Js.string("code-dragging"));
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
        Dom_html.document##.body##.classList##add(
          Js.string("code-dragging"),
        );
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
    switch (
      s.winner |> Option.map(i => List.nth_opt(s.cands, i)) |> Option.join
    ) {
    | Some(c) when s.t >= commit_t =>
      /* handoff: the commit's FLIP starts each token from its
         scrubbed offset; the scrub animations are adopted so they
         die exactly when the flights take over */
      CodeFlip.set_drag_offsets(
        c.moved
        |> List.map(((k, _, dx, dy)) => (k, (dx *. s.t, dy *. s.t))),
      );
      if (c.scroll_rows > 0) {
        CodeFlip.set_scroll_bump(~rows=c.scroll_rows, ~near=s.text_box);
      };
      if (c.enters != []) {
        CodeFlip.set_drag_enter(s.t);
      };
      /* traveling enters: the real tokens continue POSITIONALLY from
         the ghost's spot — the remaining travel goes through the
         offsets map like every other continuation */
      CodeFlip.set_drag_enter_offsets(
        c.enters
        |> List.filter_map(((k, _, _, dest, origin)) =>
             switch (origin) {
             | Some(from) =>
               Some((
                 k,
                 (
                   (from.x -. dest.x) *. (1. -. s.t),
                   (from.y -. dest.y) *. (1. -. s.t),
                 ),
               ))
             | None => None
             }
           ),
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
    last_p: {
      x: 0.,
      y: 0.,
    },
    dwell_timer: None,
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
      /* track anchors at the CENTER of the anchor's first character
         cell (andrew: reads better than the left-edge midpoint) */
      let px = (p: Measured.Point.t): vec => {
        x: (float_of_int(p.col) +. 0.5) *. font_metrics.col_width,
        y: (float_of_int(p.row) +. 0.5) *. font_metrics.row_height,
      };
      scrub_clear(s);
      let pairs = live_pairs(segment);
      /* tokens this candidate displaces, in px (single-row tokens
         only, matching CodeFlip's guard); candidate positions read
         through the kind's screen frame */
      let moved_for =
          (frame: Refactor.DragCandidate.frame, cand_m: Measured.t) =>
        pairs
        |> List.filter_map(((k, node)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas(cand_m, k),
             ) {
             | (Some(o), Some(n))
                 when o.origin.row == o.last.row && n.origin.row == n.last.row =>
               let n_origin =
                 Refactor.DragCandidate.frame_point(frame, n.origin);
               if (o.origin == n_origin) {
                 None;
               } else {
                 let dx =
                   float_of_int(n_origin.col - o.origin.col)
                   *. font_metrics.col_width;
                 let dy =
                   float_of_int(n_origin.row - o.origin.row)
                   *. font_metrics.row_height;
                 Some((k, node, dx, dy));
               };
             | _ => None
             }
           );
      /* candidate token texts by key (order irrelevant: keyed) */
      let rec token_texts =
              (seg: Segment.t): list((CodeFlip.key, string, string)) =>
        seg
        |> List.concat_map((piece: Piece.t) =>
             switch (piece) {
             | Tile(t) =>
               let plurality = List.length(t.label) == 1 ? "mono" : "poly";
               let sort_cls = Sort.class_of(t.mold.out);
               (
                 t.shards
                 |> List.filter_map(i =>
                      switch (List.nth_opt(t.label, i)) {
                      | Some(txt) =>
                        let cls =
                          ["token", sort_cls, plurality]
                          @ (Token.is_keyword(txt) ? ["keyword"] : [])
                          |> String.concat(" ");
                        Some((CodeFlip.Shard(t.id, i), txt, cls));
                      | None => None
                      }
                    )
               )
               @ List.concat_map(token_texts, t.children);
             | Grout(_)
             | Secondary(_)
             | Projector(_) => []
             }
           );
      /* tokens absent live: ghost text at their (frame-adjusted)
         candidate positions */
      let enters_for =
          (
            frame: Refactor.DragCandidate.frame,
            emerge: list((Id.t, Id.t)),
            cand_seg: Segment.t,
            cand_m: Measured.t,
          ) =>
        token_texts(cand_seg)
        |> List.filter_map(((k, text, cls)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas(cand_m, k),
             ) {
             | (None, Some(n)) =>
               let p = Refactor.DragCandidate.frame_point(frame, n.origin);
               let px_of = (pt: Measured.Point.t) => {
                 x: float_of_int(pt.col) *. font_metrics.col_width,
                 y: float_of_int(pt.row) *. font_metrics.row_height,
               };
               /* emerge origin: the live token this copy grows out
                  of (same shard index on the paired live tile) */
               let origin =
                 switch (k) {
                 | Shard(kid, i) =>
                   switch (List.assoc_opt(kid, emerge)) {
                   | Some(from_id) =>
                     CodeFlip.find_meas(measured, Shard(from_id, i))
                     |> Option.map((m: Measured.measurement) =>
                          px_of(m.origin)
                        )
                   | None => None
                   }
                 | _ => None
                 };
               Some((k, text, cls, px_of(p), origin));
             | _ => None
             }
           );
      /* tokens present live but absent in the candidate */
      let exits_for = (cand_m: Measured.t) =>
        pairs
        |> List.filter_map(((k, node)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas(cand_m, k),
             ) {
             | (Some(_), None) => Some(node)
             | _ => None
             }
           );
      /* anchored-deco delta closure: same deltas as the anchor
         tokens, read from the two measured maps on demand */
      let deco_delta_for =
          (
            frame: Refactor.DragCandidate.frame,
            cand_m: Measured.t,
            (id, shard): (Id.t, option(int)),
          )
          : option((float, float)) =>
        switch (
          CodeFlip.anchor_meas(measured, id, shard),
          CodeFlip.anchor_meas(cand_m, id, shard),
        ) {
        | (Some(o), Some(n)) =>
          let n_origin = Refactor.DragCandidate.frame_point(frame, n.origin);
          if (o.origin == n_origin) {
            None;
          } else {
            let dx =
              float_of_int(n_origin.col - o.origin.col)
              *. font_metrics.col_width;
            let dy =
              float_of_int(n_origin.row - o.origin.row)
              *. font_metrics.row_height;
            Some((dx, dy));
          };
        | _ => None
        };
      let cands =
        Refactor.drag_candidates(~info_map, ~term, ~measured, z)
        |> List.map((c: Refactor.DragCandidate.t) =>
             {
               dir: c.dir,
               kind: c.kind,
               label: c.label,
               cur: px(c.current),
               tgt: px(c.target),
               scroll_rows: c.frame.scroll_rows,
               moved: moved_for(c.frame, c.measured),
               deco_delta: deco_delta_for(c.frame, c.measured),
               exits: exits_for(c.measured),
               enters: enters_for(c.frame, c.emerge, c.segment, c.measured),
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
      /* BOTH must have changed: after a commit the first render has
         the new zipper with the STALE statics term (statics lag one
         render). Computing there yields plausible-but-stale
         candidates — transforms prepared against the old structure
         whose tracks degenerate against the new live measured
         (everything but feed vanished on chain rung 2). Wait for the
         term to catch up. */
      let changed =
        switch (z0, t0) {
        | (Some(z0), Some(t0)) => !(z0 === z) && !(t0 === term)
        | (Some(z0), None) => !(z0 === z)
        | (None, _) => true
        };
      if (changed) {
        compute();
      };
    };
  };
