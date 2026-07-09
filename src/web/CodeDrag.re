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

   Tracks/targets render on an under-text overlay inside the app's
   fixed wrapper; labels + ghosts float above; the whole buffer
   scrubs with the pull, refusals lean, releases commit past
   commit_t. */

/* tuning */
let snap_radius = 14.; /* px: reaching a target commits (chain) */
/* snap_min_t: the radius alone is a disc AROUND the endpoint — other
   tracks' paths can pass through it, and near-parallel targets sit a
   row apart. Requiring travel along the track first makes a snap
   mean "arrived VIA this track", not "happened near its endpoint".
   (Dragology's withSnapRadius is radius-only; their diagrams have
   sparser targets — recorded as incidental drift.) */
let snap_min_t = 0.7;
/* Chaining: reaching a target via its track (radius + min progress)
   COMMITS mid-drag and re-enumerates from the new state — multiple
   rungs in one hold. Instant on arrival, like dragology's
   withSnapRadius(chain:true). OFF (andrew, 2nd time): works
   mechanically but surfaces caret/handoff complexities — parked low
   on the ledger; one commit per drag until then. */
let chaining = false;
/* NO when_far radius (removed): the old 56px perimeter around tracks
   was a DISTANCE cliff — pulling past a track's end eventually
   dropped the winner and popped the whole preview home (andrew:
   sudden, disorienting). The partition is now purely DIRECTIONAL
   (the cos gate below): pull along a track and it stays engaged
   forever, the give absorbing everything past the end; the only
   discontinuities left are genuine direction changes, bridged by
   the relax. This is also closer to D2, whose drop rule is closest-
   state-by-pointer-distance — a Voronoi over states with no dead
   ring (andrew's recollection was right). Consequence: releasing
   far past an end still COMMITS (t=1); abort = swing off-axis or
   Esc. */
let stickiness = 6.; /* px bonus for the incumbent track */
let direction_pull = 8.; /* px bonus for tracks aligned with the pull —
   near the shared origin, nearly-parallel tracks (extract vs swap at
   an acute angle) are indistinguishable by gap alone */

let commit_t = 0.55; /* release past this progress commits the winner */
let slop = 4.; /* px before the drag counts as begun */
/* response curve on the winner's track progress — D2's between(...,
   {sharpness}): weights are raised to this power and renormalized,
   which for a two-state track is t^k / (t^k + (1-t)^k). 1.0 =
   linear (off); >1 hugs the endpoints and snaps through the middle
   (Josh's switch-with-sharpness study demo). Wired for trying, not
   yet a conviction. */
let sharpness = 1.0;
let sharpen = (t: float): float =>
  sharpness == 1.0
    ? t
    : {
      let a = t ** sharpness;
      a /. (a +. (1. -. t) ** sharpness);
    };

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
     positions. (text, token classes, destination, emerge-origin).
     With an origin (feed's copy splitting off the surviving def) the
     ghost TRAVELS origin->destination full-size at full opacity — D2
     emergeMode=clone; without, it grows in place (scale+fade)
     scrubbed by t. The commit continues each from the same state. */
  enters: list((CodeFlip.key, string, string, vec, option(vec))),
  /* LIVE ids of the emerge source (the cloned def subtree) — handed
     to CodeFlip at commit for the positional flight pairing */
  emerge_src: list(Id.t),
  /* the grabbed token's own displacement in this candidate — the
     caret rides it (movement kinds); None = the caret stays (feeds:
     the binder exits, focus stays for the next feed) */
  caret_delta: option((float, float)),
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
  mutable had_cands: bool,
  mutable segment: option(Segment.t), /* for the dead-press shake */
  mutable pending,
  mutable last_z: option(Zipper.t),
  mutable last_term: option(Language.Exp.t),
  mutable down_at: vec,
  /* the grabbed token: its live DOM node, its (tile, shard) for
     resolving companion decorations, the lean animations (one per
     leaning node — token + backings + caret, composite:add over any
     scrub), and the current lean vector (for the release rebound) */
  mutable grab_node: option(Js.t(Dom.node)),
  mutable grab_ids: option((Id.t, option(int))),
  mutable local_give: list(Js.Unsafe.any),
  mutable local_give_g: vec,
  mutable listeners: list(Dom.event_listener_id),
  /* paused WAAPI animations per candidate index, scrubbed by track
     progress (the pointer drives currentTime — lerpViews restricted
     to translation) */
  mutable scrub_anims: list((int, list(Js.Unsafe.any))),
  mutable scrub_active: option(int),
};

let session: ref(option(session)) = ref(None);
let active = () => session^ != None;

/* === overlay (owned element under body; never vdom-managed) === */

let overlay_id = "code-drag-overlay";
let overlay_under_id = "code-drag-overlay-under";

let overlay_named =
    (~parent: Js.t(Dom_html.element), id: string, z: string)
    : Js.t(Dom_html.element) =>
  switch (
    Js.Opt.to_option(Dom_html.document##getElementById(Js.string(id)))
  ) {
  | Some(el) => el
  | None =>
    let el = Dom_html.createDiv(Dom_html.document);
    el##.id := Js.string(id);
    el##.style##.cssText :=
      Js.string(
        Printf.sprintf(
          "position:fixed;inset:0;pointer-events:none;z-index:%s;",
          z,
        ),
      );
    Dom.appendChild(parent, el);
    el;
  };

/* the code layers (backings z 2-4, text z 10) resolve their z inside
   the app's position:fixed wrapper — FIXED CREATES A STACKING
   CONTEXT, so a body-level layer can never slot between them (bit
   us: tracks painted over text everywhere while computed z's looked
   right). The under-layer must mount INSIDE that wrapper; fixed
   positioning still anchors to the viewport, so geometry is
   unchanged. */
let fixed_wrapper = (from: Js.t(Dom_html.element)): Js.t(Dom_html.element) => {
  let rec up = (el: Js.t(Dom_html.element)) => {
    let pos =
      Js.to_string(
        Js.Unsafe.get(
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "getComputedStyle",
            [|Js.Unsafe.inject(el)|],
          ),
          "position",
        ),
      );
    if (pos == "fixed") {
      el;
    } else {
      switch (Js.Opt.to_option(el##.parentNode)) {
      | Some(p) when Js.Opt.test(Dom_html.CoerceTo.element(p)) =>
        switch (Js.Opt.to_option(Dom_html.CoerceTo.element(p))) {
        | Some(pe) => up(pe)
        | None => Dom_html.document##.body
        }
      | _ => Dom_html.document##.body
      };
    };
  };
  switch (up(from)) {
  | el => el
  | exception _ => Dom_html.document##.body
  };
};

/* labels + ghost tokens float over everything; tracks/targets sit
   UNDER the code text but over shard backings (the z table's
   --drag-track-z) — lines through token glyphs read as clutter,
   under them they read as terrain (andrew) */
let overlay_el = (): Js.t(Dom_html.element) =>
  overlay_named(~parent=Dom_html.document##.body, overlay_id, "999999");
let overlay_under_el = (s: session): Js.t(Dom_html.element) =>
  overlay_named(
    ~parent=fixed_wrapper(s.text_box),
    overlay_under_id,
    "var(--drag-track-z)",
  );

let remove_overlay = () =>
  [overlay_id, overlay_under_id]
  |> List.iter(id =>
       switch (
         Js.Opt.to_option(Dom_html.document##getElementById(Js.string(id)))
       ) {
       | Some(el) => Js.Opt.iter(el##.parentNode, p => Dom.removeChild(p, el))
       | None => ()
       }
     );

let box_origin = (s: session): vec => {
  let r = s.text_box##getBoundingClientRect;
  {
    x: r##.left,
    y: r##.top,
  };
};

/* local give: the grabbed token leans toward the hand — a paused
   single-keyframe composite:add animation stacks the give on the
   token's scrub transform (inline styles lose to active animations;
   add-composited animations don't). Clearing a nonzero lean plays a
   springy REBOUND (back-out bezier — kin to D2's elastic-out drop
   settle) instead of snapping: the snap-back read as nothing. */
let give_damp = (dist: float): float =>
  12. *. (1. -. 1. /. (1. +. dist /. 28.));

let rebound = (node: Js.t(Dom.node), from: vec): unit => {
  let keyframes =
    Js.Unsafe.obj([|
      (
        "transform",
        Js.Unsafe.inject(
          Js.array([|
            Js.string(
              Printf.sprintf("translate(%fpx, %fpx)", from.x, from.y),
            ),
            Js.string("translate(0px, 0px)"),
          |]),
        ),
      ),
    |]);
  let options =
    Js.Unsafe.obj([|
      ("duration", Js.Unsafe.inject(Js.number_of_float(180.))),
      ("composite", Js.Unsafe.inject(Js.string("add"))),
      (
        "easing",
        Js.Unsafe.inject(Js.string("cubic-bezier(0.34, 1.56, 0.64, 1)")),
      ),
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
};

/* the lean moves the WHOLE thing in hand: the token's text, its
   anchored decorations (shard backing, error underlays...), and the
   caret sitting on it — text-only lean read as a glitch (andrew).
   Decos resolve fresh per call (activation-time rule: vdom patches
   re-purpose elements). */
let lean_nodes = (s: session): list(Js.t(Dom.node)) => {
  let decos =
    switch (s.grab_ids) {
    | None => []
    | Some((tid, shard)) =>
      CodeFlip.anchored_decos()
      |> List.filter_map(((id, sh, node)) =>
           id == tid && (shard == None || sh == None || sh == shard)
             ? Some(node) : None
         )
    };
  let caret =
    switch (JsUtil.get_elem_by_id_opt("caret")) {
    | Some(el) => [(el :> Js.t(Dom.node))]
    | None => []
    };
  (
    switch (s.grab_node) {
    | Some(n) => [n]
    | None => []
    }
  )
  @ decos
  @ caret;
};

let apply_local_give = (s: session, g: vec): unit => {
  s.local_give
  |> List.iter(anim =>
       switch (Js.Unsafe.meth_call(anim, "cancel", [||])) {
       | exception _ => ()
       | _ => ()
       }
     );
  s.local_give = [];
  if (g.x == 0. && g.y == 0.) {
    let prev = s.local_give_g;
    if (!(prev.x == 0. && prev.y == 0.)) {
      lean_nodes(s) |> List.iter(node => rebound(node, prev));
    };
  };
  s.local_give_g = g;
  if (!(g.x == 0. && g.y == 0.)) {
    lean_nodes(s)
    |> List.iter(node => {
         let keyframes =
           Js.Unsafe.obj([|
             (
               "transform",
               Js.Unsafe.inject(
                 Js.array([|
                   Js.string(
                     Printf.sprintf("translate(%fpx, %fpx)", g.x, g.y),
                   ),
                 |]),
               ),
             ),
           |]);
         let options =
           Js.Unsafe.obj([|
             ("duration", Js.Unsafe.inject(Js.number_of_float(1.))),
             ("fill", Js.Unsafe.inject(Js.string("both"))),
             ("composite", Js.Unsafe.inject(Js.string("add"))),
           |]);
         switch (
           Js.Unsafe.meth_call(
             node,
             "animate",
             [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
           )
         ) {
         | exception _ => ()
         | anim =>
           switch (Js.Unsafe.meth_call(anim, "pause", [||])) {
           | exception _ => ()
           | _ => ()
           };
           /* park at the END of the (1ms) timeline: a single keyframe
              sits at offset 1, so a pause at time 0 renders the
              implicit start = NO offset (the lean was invisible during
              the drag and flashed in only at the release rebound) */
           Js.Unsafe.set(anim, "currentTime", Js.number_of_float(1.));
           s.local_give = [anim, ...s.local_give];
         };
       });
  };
};

/* overlay palette: warm vermillion winner + muted slate idle, both
   cased in the page cream so tracks read over code without shouting */
let ov_accent = "#c2483b";
let ov_idle = "#8a94a2";
let ov_casing = "#fffdf4";

let draw = (s: session) => {
  let o = box_origin(s);
  /* slim pill, text filling most of its height; sits BETWEEN text
     lines rather than lapping the lower one (andrew: they intersected
     more at the bottom than the top) */
  let label_pill = (~win: bool, x: float, y: float, label: string) => {
    let w = float_of_int(String.length(label)) *. 5.8 +. 9.;
    Printf.sprintf(
      {|<rect x="%f" y="%f" width="%f" height="13" rx="6.5" fill="%s" stroke="%s" stroke-width="%s" opacity="%s"/>
        <text x="%f" y="%f" style="font-family:var(--code-font);font-size:9.5px" fill="%s" opacity="%s">%s</text>|},
      x,
      y -. 14.,
      w,
      win ? ov_accent : "rgba(255,253,244,0.92)",
      win ? ov_accent : ov_idle,
      win ? "0" : "0.75",
      win ? "1" : "0.85",
      x +. 4.5,
      y -. 4.2,
      win ? ov_casing : ov_idle,
      win ? "1" : "0.95",
      label,
    );
  };
  let seg = (i, c: cand) => {
    let win = s.winner == Some(i);
    let x1 = o.x +. c.cur.x
    and y1 = o.y +. c.cur.y;
    let x2 = o.x +. c.tgt.x
    and y2 = o.y +. c.tgt.y;
    /* cream casing lifts every track off the code */
    let casing =
      Printf.sprintf(
        {|<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="%s" stroke-linecap="round" opacity="0.85"/>|},
        x1,
        y1,
        x2,
        y2,
        ov_casing,
        win ? "6" : "5",
      );
    if (win) {
      /* traveled reads solid, remaining dotted — the pull's progress
         is on the track itself */
      let mx = x1 +. (x2 -. x1) *. s.t
      and my = y1 +. (y2 -. y1) *. s.t;
      (
        casing
        ++ Printf.sprintf(
             {|<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="2" stroke-dasharray="0.1 6" stroke-linecap="round" opacity="0.9"/>
             <line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="2.5" stroke-linecap="round"/>
             <circle cx="%f" cy="%f" r="8.5" fill="%s" opacity="0.16"/>
             <circle cx="%f" cy="%f" r="4" fill="%s" stroke="%s" stroke-width="1.5"/>|},
             mx,
             my,
             x2,
             y2,
             ov_accent,
             x1,
             y1,
             mx,
             my,
             ov_accent,
             x2,
             y2,
             ov_accent,
             x2,
             y2,
             ov_accent,
             ov_casing,
           ),
        label_pill(~win=true, x2 +. 12., y2 -. 4., c.label),
      );
    } else {
      (
        casing
        ++ Printf.sprintf(
             {|<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="1.5" stroke-dasharray="0.1 5" stroke-linecap="round" opacity="0.65"/>
             <circle cx="%f" cy="%f" r="3.5" fill="%s" stroke="%s" stroke-width="1.5" opacity="0.8"/>|},
             x1,
             y1,
             x2,
             y2,
             ov_idle,
             x2,
             y2,
             ov_casing,
             ov_idle,
           ),
        label_pill(~win=false, x2 +. 12., y2 -. 4., c.label),
      );
    };
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
        let spans =
          c.enters
          |> List.map(((_, text, cls, dest, origin)) => {
               /* with an origin: a CLONE splitting off its source —
                  full-size, full-opacity, position-only (D2
                  emergeMode=clone; the copy already "exists" under
                  the source at t=0). Without: genuinely new material
                  growing in place, scrubbed by t. */
               let (pos, op, scale) =
                 switch (origin) {
                 | Some(from) => (
                     {
                       x: from.x +. (dest.x -. from.x) *. s.t,
                       y: from.y +. (dest.y -. from.y) *. s.t,
                     },
                     1.,
                     1.,
                   )
                 | None => (dest, s.t, 0.1 +. 0.9 *. s.t)
                 };
               Printf.sprintf(
                 {|<span class="%s" style="position:absolute;left:%fpx;top:%fpx;opacity:%f;transform:scale(%f)">%s</span>|},
                 cls,
                 o.x +. pos.x,
                 o.y +. pos.y,
                 op,
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
  let parts = s.cands |> List.mapi(seg);
  let mk_svg = body =>
    Printf.sprintf({|<svg width="100%%" height="100%%">%s</svg>|}, body);
  let tracks = parts |> List.map(fst) |> String.concat("\n");
  let labels = parts |> List.map(snd) |> String.concat("\n");
  overlay_under_el(s)##.innerHTML := Js.string(mk_svg(tracks));
  overlay_el()##.innerHTML := Js.string(mk_svg(labels) ++ ghosts);
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

/* the caret rides the grabbed token's scrub (movement kinds only —
   caret_delta is None when the caret logically stays put) */
let caret_moves = (c: cand): list((Js.t(Dom.node), float, float)) =>
  switch (c.caret_delta, JsUtil.get_elem_by_id_opt("caret")) {
  | (Some((dx, dy)), Some(el)) => [((el :> Js.t(Dom.node)), dx, dy)]
  | _ => []
  };

let make_anims = (c: cand): list(Js.Unsafe.any) => {
  let moves =
    (c.moved |> List.map(((_, node, dx, dy)) => (node, dx, dy)))
    @ deco_moves(c)
    @ caret_moves(c)
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
    @ caret_moves(c)
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

let resolve = (s: session, p: vec): unit => {
  let track = (c: cand) => {
    /* PULL-RELATIVE frame: the track starts where the HAND grabbed,
       not at the token's cell center — anchoring at the center made
       the grab offset count as phantom progress (t != 0 at rest:
       the scrub twitched along the track before any pull, and the
       lean's residual pointed off-hand). Direction and length come
       from the candidate; the origin is the grab point. */
    let ax = s.down_at.x
    and ay = s.down_at.y;
    let dx = c.tgt.x -. c.cur.x
    and dy = c.tgt.y -. c.cur.y;
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
    (t, gap -. direction_pull *. max(0., cos_sim), cos_sim);
  };
  let scored = s.cands |> List.mapi((i, c) => (i, c, track(c)));
  let best =
    scored
    |> List.fold_left(
         (acc, (i, c, (t, gap, cos))) => {
           let bonus = s.winner == Some(i) ? stickiness : 0.;
           /* ALIGNMENT GATE: a track engages only when the pull
              points along it (andrew: pulling away from the sole
              track still scrubbed it — initial jitter projected
              onto short tracks visibly). cos == 0 while the pull is
              too short to have a direction, so nothing engages
              before ~6px — the lean covers that. */
           if (cos <= 0.25) {
             acc;
           } else {
             switch (acc) {
             | Some((_, _, _, best_gap)) when gap -. bonus >= best_gap => acc
             | _ => Some((i, c, t, gap -. bonus))
             };
           };
         },
         None,
       );
  switch (best) {
  | Some((i, c, t, _)) =>
    let t = sharpen(t);
    s.winner = Some(i);
    s.t = t;
    scrub_to(s, Some(i), t);
    {
      /* give = damped UNCONSUMED pull: pointer travel minus what the
         scrub absorbed. Covers every refusal at once — backwards (t
         pinned at 0), past the end (t pinned at 1), perpendicular
         wander, and short tracks — the grabbed unit leans a few px
         toward the hand, clearly wanting to stay. */
      let rx = p.x -. s.down_at.x -. t *. (c.tgt.x -. c.cur.x)
      and ry = p.y -. s.down_at.y -. t *. (c.tgt.y -. c.cur.y);
      let dist = sqrt(rx *. rx +. ry *. ry);
      let mag = give_damp(dist);
      apply_local_give(
        s,
        dist < 1. || mag < 0.5
          ? {
            x: 0.,
            y: 0.,
          }
          : {
            x: rx /. dist *. mag,
            y: ry /. dist *. mag,
          },
      );
    };
    /* snap (dragology's withSnapRadius(chain:true), radius-only and
       INSTANT — no dwell): reaching the target via the track commits
       and re-enumerates */
    let d =
      sqrt(
        (p.x -. s.down_at.x -. (c.tgt.x -. c.cur.x))
        ** 2.
        +. (p.y -. s.down_at.y -. (c.tgt.y -. c.cur.y))
        ** 2.,
      );
    if (chaining && d <= snap_radius && t >= snap_min_t) {
      CodeFlip.set_drag_offsets(
        c.moved |> List.map(((k, _, dx, dy)) => (k, (dx, dy))),
      );
      if (c.enters != []) {
        CodeFlip.set_drag_enter(1.0);
      };
      CodeFlip.set_emerge_src(c.emerge_src);
      CodeFlip.adopt(s.scrub_anims |> List.concat_map(snd));
      s.scrub_anims = [];
      s.scrub_active = None;
      s.winner = None;
      s.t = 0.;
      s.cands = [];
      s.pending = AwaitChange(s.last_z, s.last_term);
      s.commit(c.dir);
    };
  | _ =>
    s.winner = None;
    s.t = 0.;
    scrub_to(s, None, 0.);
    /* dead-direction give: no track wants this pull, but the token
       leans a few damped px toward the pointer — draggable-but-
       refusing (the release shake still lands). Past-a-track's-end
       give lives in the winner arm. */

    let ddx = p.x -. s.down_at.x
    and ddy = p.y -. s.down_at.y;
    let dist = sqrt(ddx *. ddx +. ddy *. ddy);
    let mag = give_damp(dist);
    apply_local_give(
      s,
      dist < 1. || mag < 0.5
        ? {
          x: 0.,
          y: 0.,
        }
        : {
          x: ddx /. dist *. mag,
          y: ddy /. dist *. mag,
        },
    );
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
    apply_local_give(
      s,
      {
        x: 0.,
        y: 0.,
      },
    );
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
      draw(s);
    } else if (s.began) {
      /* nothing draggable here (zero candidates park the session in
         AwaitChange, so resolve never runs) — the token still leans
         toward the hand: refusal you can FEEL before the release
         shake */
      let ddx = p.x -. s.down_at.x
      and ddy = p.y -. s.down_at.y;
      let dist = sqrt(ddx *. ddx +. ddy *. ddy);
      let mag = give_damp(dist);
      apply_local_give(
        s,
        dist < 1. || mag < 0.5
          ? {
            x: 0.,
            y: 0.,
          }
          : {
            x: ddx /. dist *. mag,
            y: ddy /. dist *. mag,
          },
      );
    };
  };

let on_up = (_e: Js.t(Dom_html.event)): unit =>
  switch (session^) {
  | None => ()
  | Some(s) =>
    /* dragged something undraggable: make the refusal visible */
    if (s.began && !s.had_cands) {
      CodeFlip.shake_dead_press(~segment=?s.segment, ());
    };
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
      /* emerge flights: hand over the SOURCE ids, not offsets keyed
         by candidate clone ids — the commit re-runs prepare and
         mints different fresh ids, so an id-keyed map silently
         misses (it did). CodeFlip re-derives the pairing
         positionally against the post-commit segment and continues
         the remaining (1 - t) of each flight. */
      CodeFlip.set_emerge_src(c.emerge_src);
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
    had_cands: false,
    segment: None,
    pending: AwaitGoal(goal, None),
    last_z: None,
    last_term: None,
    scrub_anims: [],
    scrub_active: None,
    down_at: {
      x: cx -. r##.left,
      y: cy -. r##.top,
    },
    grab_node: None,
    grab_ids: None,
    local_give: [],
    local_give_g: {
      x: 0.,
      y: 0.,
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
      ~shape_map: Id.Map.t(ProjectorCore.Shape.t),
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
      s.segment = Some(segment);
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
      let grab_id = Indicated.index(z);
      let grab_shard = Indicated.shard_index(z);
      s.grab_ids = (
        switch (grab_id) {
        | Some(tid) => Some((tid, grab_shard))
        | None => None
        }
      );
      s.grab_node = (
        switch (grab_id) {
        | None => None
        | Some(tid) =>
          let find = exact =>
            pairs
            |> List.find_map(((k, n)) =>
                 switch (k) {
                 | CodeFlip.Shard(id, i)
                     when id == tid && (!exact || Some(i) == grab_shard) =>
                   Some(n)
                 | _ => None
                 }
               );
          switch (find(true)) {
          | Some(n) => Some(n)
          | None => find(false)
          };
        }
      );
      let caret_delta_of =
          (moved: list((CodeFlip.key, Js.t(Dom.node), float, float))) =>
        switch (grab_id) {
        | None => None
        | Some(tid) =>
          let of_key = (want_exact: bool, (k, _, dx, dy)) =>
            switch (k) {
            | CodeFlip.Shard(id, i) when id == tid =>
              !want_exact || Some(i) == grab_shard ? Some((dx, dy)) : None
            | _ => None
            };
          switch (moved |> List.find_map(of_key(true))) {
          | Some(d) => Some(d)
          | None => moved |> List.find_map(of_key(false))
          };
        };
      let cands =
        Refactor.drag_candidates(~info_map, ~term, ~measured, ~shape_map, z)
        |> List.map((c: Refactor.DragCandidate.t) => {
             let moved = moved_for(c.frame, c.measured);
             {
               dir: c.dir,
               kind: c.kind,
               label: c.label,
               cur: px(c.current),
               tgt: px(c.target),
               scroll_rows: c.frame.scroll_rows,
               moved,
               deco_delta: deco_delta_for(c.frame, c.measured),
               exits: exits_for(c.measured),
               enters: enters_for(c.frame, c.emerge, c.segment, c.measured),
               emerge_src: c.emerge |> List.map(snd),
               caret_delta: caret_delta_of(moved),
             };
           });
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
        s.had_cands = true;
        s.pending = Idle;
        draw(s);
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
