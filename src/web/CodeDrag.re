open Util;
open Js_of_ocaml;
open Haz3lcore;

/* Drag-to-refactor (plans/refactor-drag.md). Candidates come from
   Refactor.drag_candidates — every refactor the grabbed element can
   drive (D2's reachable states; the arrow keys play no part). With
   Drag Ladders on, each candidate is a LADDER: that refactor repeated
   (Refactor.next_rung), one state per rung — the way D2's list
   reorder offers every position in one drag. The track runs through
   every rung's target; the preview slides between neighboring rungs;
   release lands on the nearest rung and commits the whole path as one
   undo step. Nothing commits mid-drag. Rungs are computed as the pull
   nears a ladder's end.

   Tracks/targets render on an under-text overlay inside the app's
   fixed wrapper; labels + ghosts float above; the whole buffer
   scrubs with the pull, refusals lean. */

/* tuning */
/* NO when_far radius: a distance cliff popped the preview home when
   a pull overshot (andrew: sudden, disorienting). The partition is
   DIRECTIONAL (the cos gate in resolve): pull along a track and it
   stays engaged, the give absorbing everything past the end — like
   D2's closest-state drop rule, a Voronoi with no dead ring.
   Releasing far past an end still commits; abort = swing off-axis or
   Esc. */
let stickiness = 6.; /* px bonus for the incumbent track */
let direction_pull = 8.; /* px bonus for tracks aligned with the pull —
   near the shared origin, nearly-parallel tracks (extract vs swap at
   an acute angle) are indistinguishable by gap alone */

let slop = 4.; /* px before the drag counts as begun */
let max_rungs = 12;
/* the response within each rung — D2's between(..., {sharpness}):
   t^k / (t^k + (1-t)^k). Above 1 the preview hugs each rung and moves
   briskly between them (detents) while the pointer mapping stays
   continuous */
let sharpness = 1.6;
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

/* one rung: a state the drag can land in, laid out against the LIVE
   layout (px deltas from where things sit now) */
type deco_step =
  | Shift(float, float)
  | Gone;

type rung = {
  kind: Action.refactor,
  label: string,
  /* where this step is invoked, in the state before it */
  at: Id.t,
  tgt: vec, /* text-box-local px: the moving element's place here */
  scroll_rows: int, /* commit-time scroll bump (pinned extract) */
  moved: list((CodeFlip.key, Js.t(Dom.node), float, float)),
  /* anchored-deco delta by (anchor id, shard). Deco ELEMENTS are
     resolved at ACTIVATION, not compute: compute runs during view
     construction (pre-patch DOM), so elements collected there can be
     re-purposed by the patch and would ride stale deltas. A deco
     whose anchor is gone here fades with it. */
  deco_delta: ((Id.t, option(int))) => option(deco_step),
  /* live tokens absent in this state: they dissolve (scale+fade) as
     the pull reaches it — converging by the delta when they merge
     into a survivor or trade their text (a renamed use) */
  exits: list((CodeFlip.key, Js.t(Dom.node), option((float, float)))),
  /* tokens absent LIVE: synthetic "before" versions of the REAL
     tokens (dragology's createSyntheticBefore) at their places here
     (text, token classes, destination, emerge-origin). With an origin
     (feed's copy splitting off the surviving def) the ghost TRAVELS
     origin->destination full-size — D2 emergeMode=clone; without, it
     grows in place. */
  enters: list((CodeFlip.key, string, string, vec, option(vec))),
  /* where a dissolving token lands here: its survivor's token (merge),
     for ghosts that a merge rung drops */
  absorb_into: list((CodeFlip.key, vec)),
  /* LIVE ids of the emerge source — handed to CodeFlip at commit */
  emerge_src: list(Id.t),
  /* the grabbed token's displacement here — the caret rides it */
  caret_delta: option((float, float)),
  /* the state itself, and the element the next rung moves */
  cand: Refactor.DragCandidate.t,
  held: (Id.t, option(int)),
};

/* a ladder: one refactor's rungs, one direction */
type cand = {
  kind: Action.refactor,
  label: string,
  cur: vec, /* text-box-local px: where the moving element sits now */
  mutable rungs: list(rung),
  mutable exhausted: bool,
};

/* Candidate enumeration must wait for the model to settle: on arm,
   until the caret reaches the clicked token (or the zipper changes —
   clamped clicks land elsewhere); with no candidates, until the model
   changes (statics lag the zipper by a render). Comparing zippers
   physically is sound: renders reuse the record unless the model
   changed. */
type pending =
  | Idle
  | AwaitGoal(Measured.Point.t, option(Zipper.t))
  | AwaitChange(option(Zipper.t), option(Language.Exp.t));

type session = {
  /* commit the path to the rung reached: (refactor, invoked at) */
  commit: list((Action.refactor, Id.t)) => unit,
  ladders: bool,
  push: bool,
  show_labels: bool,
  show_tracks: bool,
  text_box: Js.t(Dom_html.element),
  mutable cands: list(cand),
  mutable winner: option(int),
  /* progress along the winner's ladder: rungs reached + the fraction
     of the next (0 = where things are now) */
  mutable u: float,
  mutable began: bool,
  mutable had_cands: bool,
  mutable segment: option(Segment.t), /* for the dead-press shake */
  mutable pending,
  mutable last_z: option(Zipper.t),
  mutable last_term: option(Language.Exp.t),
  mutable down_at: vec,
  /* builds a ladder's next rung (set with the candidates: it closes
     over the live layout and statics) */
  mutable extend: option(cand => option(rung)),
  mutable extending: bool,
  /* the grabbed token: its live DOM node, its (tile, shard) for
     resolving companion decorations, the lean animations (one per
     leaning node — token + backings + caret, composite:add over any
     scrub), and the current lean vector (for the release rebound) */
  mutable grab_node: option(Js.t(Dom.node)),
  mutable grab_ids: option((Id.t, option(int))),
  mutable local_give: list(Js.Unsafe.any),
  mutable local_give_g: vec,
  mutable listeners: list(Dom.event_listener_id),
  /* paused WAAPI animations per ladder index, scrubbed by progress
     (the pointer drives currentTime — lerpViews restricted to
     translation) */
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
      (
        "duration",
        Js.Unsafe.inject(
          Js.number_of_float(float_of_int(CodeFlip.dur(180))),
        ),
      ),
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

/* === ladder geometry === */

let n_rungs = (c: cand): int => List.length(c.rungs);

let lerp = (a: vec, b: vec, t: float): vec => {
  x: a.x +. (b.x -. a.x) *. t,
  y: a.y +. (b.y -. a.y) *. t,
};

/* the point at progress u along a polyline */
let along = (pts: list(vec), u: float): vec => {
  let n = List.length(pts) - 1;
  if (n <= 0) {
    List.hd(pts);
  } else {
    let i = max(0, min(n - 1, int_of_float(floor(u))));
    lerp(
      List.nth(pts, i),
      List.nth(pts, i + 1),
      max(0., min(1., u -. float_of_int(i))),
    );
  };
};

/* the track in screen (text-box-local) space: where the element sits
   now, then its place at each rung */
let screen_pts = (c: cand): list(vec) => [
  c.cur,
  ...List.map((r: rung) => r.tgt, c.rungs),
];

/* the track in pull space: the hand's start plus each rung's
   displacement of the element (pull-relative: the grab offset is not
   progress) */
let pull_pts = (s: session, c: cand): list(vec) =>
  screen_pts(c)
  |> List.map(q =>
       {
         x: s.down_at.x +. q.x -. c.cur.x,
         y: s.down_at.y +. q.y -. c.cur.y,
       }
     );

/* closest point of a polyline: (progress u, gap) */
let project = (pts: list(vec), p: vec): (float, float) => {
  let rec go = (i, pts, best) =>
    switch (pts) {
    | [a, b, ...rest] =>
      let dx = b.x -. a.x
      and dy = b.y -. a.y;
      let len2 = dx *. dx +. dy *. dy;
      let t =
        len2 == 0.
          ? 0.
          : max(
              0.,
              min(1., ((p.x -. a.x) *. dx +. (p.y -. a.y) *. dy) /. len2),
            );
      let gap =
        sqrt((p.x -. a.x -. t *. dx) ** 2. +. (p.y -. a.y -. t *. dy) ** 2.);
      let best =
        switch (best) {
        | Some((_, g)) when g <= gap => best
        | _ => Some((float_of_int(i) +. t, gap))
        };
      go(i + 1, [b, ...rest], best);
    | _ => best
    };
  switch (go(0, pts, None)) {
  | Some(r) => r
  | None => (0., 0.)
  };
};

/* detents: within each rung the preview hugs the ends */
let sharpen_u = (u: float): float => {
  let i = floor(u);
  i +. sharpen(u -. i);
};

let draw = (s: session) => {
  let o = box_origin(s);
  /* slim pill, text filling most of its height; sits BETWEEN text
     lines rather than lapping the lower one */
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
  let points = (pts: list(vec)) =>
    pts
    |> List.map(q => Printf.sprintf("%f,%f", o.x +. q.x, o.y +. q.y))
    |> String.concat(" ");
  let polyline = (~stroke, ~width, ~extra="", pts) =>
    Printf.sprintf(
      {|<polyline points="%s" fill="none" stroke="%s" stroke-width="%s" stroke-linecap="round" stroke-linejoin="round" %s/>|},
      points(pts),
      stroke,
      width,
      extra,
    );
  let dot = (~r, ~fill, ~stroke, ~opacity, q: vec) =>
    Printf.sprintf(
      {|<circle cx="%f" cy="%f" r="%s" fill="%s" stroke="%s" stroke-width="1.5" opacity="%s"/>|},
      o.x +. q.x,
      o.y +. q.y,
      r,
      fill,
      stroke,
      opacity,
    );
  let seg = (i, c: cand) => {
    let win = s.winner == Some(i);
    let pts = screen_pts(c);
    /* the label sits by the FIRST rung: the ladder growing never moves
       it */
    let first = List.nth(pts, 1);
    let rungs = List.tl(pts);
    /* cream casing lifts every track off the code */
    let casing =
      polyline(
        ~stroke=ov_casing,
        ~width=win ? "6" : "5",
        ~extra={|opacity="0.85"|},
        pts,
      );
    if (win) {
      /* traveled reads solid, remaining dotted; the halo marks the rung
         a release lands on */
      let n = List.length(rungs);
      let k = max(0, min(n, int_of_float(Float.round(s.u))));
      let here = along(pts, s.u);
      let i0 = int_of_float(floor(s.u));
      let traveled = (pts |> List.filteri((j, _) => j <= i0)) @ [here];
      let remaining = [here, ...pts |> List.filteri((j, _) => j > i0)];
      (
        casing
        ++ polyline(
             ~stroke=ov_accent,
             ~width="2",
             ~extra={|stroke-dasharray="0.1 6" opacity="0.9"|},
             remaining,
           )
        ++ polyline(~stroke=ov_accent, ~width="2.5", traveled)
        ++ (
          rungs
          |> List.map(
               dot(
                 ~r="2.5",
                 ~fill=ov_accent,
                 ~stroke=ov_casing,
                 ~opacity="0.9",
               ),
             )
          |> String.concat("")
        )
        ++ (
          k >= 1
            ? Printf.sprintf(
                {|<circle cx="%f" cy="%f" r="8.5" fill="%s" opacity="0.16"/>|},
                o.x +. List.nth(pts, k).x,
                o.y +. List.nth(pts, k).y,
                ov_accent,
              )
              ++ dot(
                   ~r="4",
                   ~fill=ov_accent,
                   ~stroke=ov_casing,
                   ~opacity="1",
                   List.nth(pts, k),
                 )
            : ""
        ),
        /* a composite ladder mixes kinds: name the step a release
           lands on */
        label_pill(
          ~win=true,
          o.x +. first.x +. 12.,
          o.y +. first.y -. 4.,
          switch (List.nth_opt(c.rungs, max(0, k - 1))) {
          | Some(r) => r.label
          | None => c.label
          },
        ),
      );
    } else {
      (
        casing
        ++ polyline(
             ~stroke=ov_idle,
             ~width="1.5",
             ~extra={|stroke-dasharray="0.1 5" opacity="0.65"|},
             pts,
           )
        ++ (
          rungs
          |> List.map(
               dot(~r="3", ~fill=ov_casing, ~stroke=ov_idle, ~opacity="0.8"),
             )
          |> String.concat("")
        ),
        label_pill(
          ~win=false,
          o.x +. first.x +. 12.,
          o.y +. first.y -. 4.,
          c.label,
        ),
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
     pipeline applies). A token entering at rung j grows in (or flies
     from its origin) across the step into j, then rides its places in
     later rungs, fading if a later rung drops it. */
  let ghosts =
    switch (s.winner) {
    | Some(w) when s.u > 0.02 =>
      switch (List.nth_opt(s.cands, w)) {
      | Some(c) =>
        let rungs = Array.of_list(c.rungs);
        let n = Array.length(rungs);
        let pos_at = (k, key) =>
          k < 1 || k > n
            ? None
            : rungs[k - 1].enters
              |> List.find_map(((k', _, _, dest, _)) =>
                   k' == key ? Some(dest) : None
                 );
        let seen = Hashtbl.create(16);
        let spans = ref([]);
        let i = int_of_float(floor(s.u))
        and f = s.u -. floor(s.u);
        for (j in 1 to n) {
          rungs[j - 1].enters
          |> List.iter(((key, text, cls, dest, origin)) =>
               if (!Hashtbl.mem(seen, key)) {
                 Hashtbl.add(seen, key, ());
                 let shown =
                   if (j == i + 1) {
                     Some(
                       switch (origin) {
                       | Some(from) => (lerp(from, dest, f), 1., 1.)
                       | None => (dest, f, 0.1 +. 0.9 *. f)
                       },
                     );
                   } else if (j <= i) {
                     switch (pos_at(i, key), pos_at(i + 1, key)) {
                     | (Some(a), Some(b)) => Some((lerp(a, b, f), 1., 1.))
                     | (Some(a), None) when i + 1 > n => Some((a, 1., 1.))
                     | (Some(a), None) =>
                       switch (List.assoc_opt(key, rungs[i].absorb_into)) {
                       | Some(b) => Some((lerp(a, b, f), 1. -. f, 1.))
                       | None => Some((a, 1. -. f, 1.))
                       }
                     | _ => None
                     };
                   } else {
                     None;
                   };
                 /* a ghost renamed by the next rung (a merged use):
                    its old text fades as the new one arrives */
                 let text_at = k =>
                   k < 1 || k > n
                     ? None
                     : rungs[k - 1].enters
                       |> List.find_map(((k', t, _, _, _)) =>
                            k' == key ? Some(t) : None
                          );
                 let shown =
                   switch (shown, text_at(i), text_at(i + 1)) {
                   | (Some((pos, op, scale)), Some(ta), Some(tb))
                       when j <= i && ta != tb =>
                     spans :=
                       [
                         Printf.sprintf(
                           {|<span class="%s" style="position:absolute;left:%fpx;top:%fpx;opacity:%f">%s</span>|},
                           cls,
                           o.x +. pos.x,
                           o.y +. pos.y,
                           op *. f,
                           xml_escape(tb),
                         ),
                         ...spans^,
                       ];
                     Some((pos, op *. (1. -. f), scale));
                   | _ => shown
                   };
                 let text =
                   switch (j <= i ? text_at(i) : None) {
                   | Some(t) => t
                   | None => text
                   };
                 switch (shown) {
                 | Some((pos, op, scale)) when op > 0.01 =>
                   spans :=
                     [
                       Printf.sprintf(
                         {|<span class="%s" style="position:absolute;left:%fpx;top:%fpx;opacity:%f;transform:scale(%f)">%s</span>|},
                         cls,
                         o.x +. pos.x,
                         o.y +. pos.y,
                         op,
                         scale,
                         xml_escape(text),
                       ),
                       ...spans^,
                     ]
                 | _ => ()
                 };
               }
             );
        };
        spans^ == []
          ? ""
          : Printf.sprintf(
              {|<div class="code" style="position:fixed;inset:0;pointer-events:none">%s</div>|},
              String.concat("", spans^),
            );
      | None => ""
      }
    | _ => ""
    };
  let parts = s.cands |> List.mapi(seg);
  let mk_svg = body =>
    Printf.sprintf({|<svg width="100%%" height="100%%">%s</svg>|}, body);
  let tracks =
    s.show_tracks ? parts |> List.map(fst) |> String.concat("\n") : "";
  let labels =
    s.show_labels ? parts |> List.map(snd) |> String.concat("\n") : "";
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

/* each displaced node's (dx, dy, opacity) in every state of a ladder,
   state 0 = live; a token a rung drops holds its place and fades */
let node_tracks =
    (c: cand)
    : list(
        (
          option(CodeFlip.key),
          Js.t(Dom.node),
          array((float, float, float)),
        ),
      ) => {
  let rungs = c.rungs;
  let n = List.length(rungs);
  let keyed =
    rungs
    |> List.concat_map((r: rung) =>
         (r.moved |> List.map(((k, node, _, _)) => (k, node)))
         @ (r.exits |> List.map(((k, node, _)) => (k, node)))
       )
    |> List.fold_left(
         (acc, (k, node)) =>
           List.mem_assoc(k, acc) ? acc : acc @ [(k, node)],
         [],
       );
  let track_of = (state: (int, rung) => option((float, float, float))) => {
    let vals = Array.make(n + 1, (0., 0., 1.));
    rungs
    |> List.iteri((i, r) => {
         let (px, py, _) = vals[i];
         vals[i + 1] = (
           switch (state(i, r)) {
           | Some(v) => v
           | None => (px, py, 0.)
           }
         );
       });
    vals;
  };
  let tokens =
    keyed
    |> List.map(((k, node)) =>
         (
           Some(k),
           node,
           track_of((_, r) =>
             switch (
               r.exits |> List.find_opt(((k', _, _)) => k' == k),
               r.moved |> List.find_opt(((k', _, _, _)) => k' == k),
             ) {
             | (Some((_, _, Some((dx, dy)))), _) => Some((dx, dy, 0.))
             | (Some((_, _, None)), _) => None
             | (None, Some((_, _, dx, dy))) => Some((dx, dy, 1.))
             | (None, None) => Some((0., 0., 1.))
             }
           ),
         )
       );
  let decos =
    CodeFlip.anchored_decos()
    |> List.filter_map(((id, shard, node)) =>
         rungs |> List.exists((r: rung) => r.deco_delta((id, shard)) != None)
           ? Some((
               None,
               node,
               track_of((_, r) =>
                 switch (r.deco_delta((id, shard))) {
                 | Some(Shift(dx, dy)) => Some((dx, dy, 1.))
                 | Some(Gone) => None
                 | None => Some((0., 0., 1.))
                 }
               ),
             ))
           : None
       );
  /* the caret rides the grabbed token (movement kinds; None = the
     caret logically stays put) */
  let caret =
    switch (JsUtil.get_elem_by_id_opt("caret")) {
    | Some(el) when rungs |> List.exists((r: rung) => r.caret_delta != None) => [
        (
          None,
          (el :> Js.t(Dom.node)),
          track_of((_, r) =>
            switch (r.caret_delta) {
            | Some((dx, dy)) => Some((dx, dy, 1.))
            | None => Some((0., 0., 1.))
            }
          ),
        ),
      ]
    | _ => []
    };
  tokens @ decos @ caret;
};

let value_at = (vals: array((float, float, float)), u: float) => {
  let n = Array.length(vals) - 1;
  let i = max(0, min(n, int_of_float(floor(u))));
  let f = max(0., min(1., u -. float_of_int(i)));
  let (x0, y0, o0) = vals[i];
  let (x1, y1, o1) = i < n ? vals[i + 1] : vals[i];
  (x0 +. (x1 -. x0) *. f, y0 +. (y1 -. y0) *. f, o0 +. (o1 -. o0) *. f);
};

let frame_css = ((dx, dy, op)) => (
  Printf.sprintf(
    "translate(%fpx, %fpx) scale(%f)",
    dx,
    dy,
    0.25 +. 0.75 *. op,
  ),
  Printf.sprintf("%f", op),
);

let make_anims = (c: cand): list(Js.Unsafe.any) =>
  node_tracks(c)
  |> List.filter_map(((_, node, vals)) => {
       let frames = Array.to_list(vals) |> List.map(frame_css);
       paused_anim(
         node,
         Js.Unsafe.obj([|
           (
             "transform",
             Js.Unsafe.inject(
               Js.array(
                 frames
                 |> List.map(((t, _)) => Js.string(t))
                 |> Array.of_list,
               ),
             ),
           ),
           (
             "opacity",
             Js.Unsafe.inject(
               Js.array(
                 frames
                 |> List.map(((_, o)) => Js.string(o))
                 |> Array.of_list,
               ),
             ),
           ),
         |]),
       );
     });

/* ease displaced tokens back to rest from where progress u holds
   them — release/cancel/track-switch CONTINUE motion, never restart */
let relax_from = (c: cand, u: float): unit =>
  if (u > 0.01) {
    node_tracks(c)
    |> List.iter(((_, node, vals)) => {
         let (t0, o0) = frame_css(value_at(vals, u));
         let keyframes =
           Js.Unsafe.obj([|
             (
               "transform",
               Js.Unsafe.inject(
                 Js.array([|
                   Js.string(t0),
                   Js.string("translate(0px, 0px) scale(1)"),
                 |]),
               ),
             ),
             (
               "opacity",
               Js.Unsafe.inject(
                 Js.array([|Js.string(o0), Js.string("1")|]),
               ),
             ),
           |]);
         let options =
           Js.Unsafe.obj([|
             (
               "duration",
               Js.Unsafe.inject(
                 Js.number_of_float(float_of_int(CodeFlip.dur(150))),
               ),
             ),
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

/* drive the winner's scrub to progress u; other ladders rest */
let scrub_to = (s: session, winner: option(int), u: float): unit => {
  switch (s.scrub_active, winner) {
  | (Some(prev), Some(w)) when prev == w => ()
  | (Some(prev), _) =>
    /* CANCEL the outgoing ladder's animations (don't park them at 0: a
       paused fill-both animation at 0 is an ACTIVE identity transform,
       and later-created animations win compositing). Exactly one
       ladder's animations exist at any time; the relax bridges. */
    switch (List.assoc_opt(prev, s.scrub_anims)) {
    | Some(anims) =>
      switch (List.nth_opt(s.cands, prev)) {
      | Some(c) => relax_from(c, s.u)
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
    switch (List.nth_opt(s.cands, w)) {
    | Some(c) =>
      let anims =
        switch (List.assoc_opt(w, s.scrub_anims)) {
        | Some(anims) => anims
        | None =>
          let anims = make_anims(c);
          s.scrub_anims = [(w, anims), ...s.scrub_anims];
          anims;
        };
      let time = u /. float_of_int(max(1, n_rungs(c))) *. scrub_duration;
      anims |> List.iter(a => Js.Unsafe.set(a, "currentTime", time));
      s.scrub_active = Some(w);
    | None => s.scrub_active = None
    }
  };
};

/* grow a ladder by one rung (off the pointer event: a rung costs a
   prepare, a print, a layout and statics) */
let request_rung = (s: session, i: int, c: cand): unit =>
  if (!s.extending && !c.exhausted) {
    s.extending = true;
    ignore(
      Dom_html.setTimeout(
        () =>
          switch (session^) {
          | Some(s') when s' === s =>
            switch (Option.map(f => f(c), s.extend)) {
            | Some(Some(r)) =>
              c.rungs = c.rungs @ [r];
              if (n_rungs(c) >= max_rungs) {
                c.exhausted = true;
              };
              /* the keyframes span the rungs: rebuild the active scrub */
              if (s.scrub_active == Some(i)) {
                switch (List.assoc_opt(i, s.scrub_anims)) {
                | Some(anims) => cancel_anims(anims)
                | None => ()
                };
                s.scrub_anims = List.remove_assoc(i, s.scrub_anims);
                s.scrub_active = None;
                scrub_to(s, Some(i), s.u);
              };
              s.extending = false;
              draw(s);
            | _ =>
              c.exhausted = true;
              s.extending = false;
            }
          | _ => ()
          },
        0.,
      ),
    );
  };

/* === resolution === */

let resolve = (s: session, p: vec): unit => {
  let scored =
    s.cands
    |> List.mapi((i, c) => {
         let pts = pull_pts(s, c);
         let (u, gap) = project(pts, p);
         /* pull-direction affinity, against the ladder's first rung */
         let a = List.hd(pts)
         and b = List.nth(pts, 1);
         let dx = b.x -. a.x
         and dy = b.y -. a.y;
         let plen = sqrt((p.x -. a.x) ** 2. +. (p.y -. a.y) ** 2.);
         let tlen = sqrt(dx *. dx +. dy *. dy);
         let cos =
           plen < 6. || tlen == 0.
             ? 0.
             : ((p.x -. a.x) *. dx +. (p.y -. a.y) *. dy) /. (plen *. tlen);
         (i, c, pts, u, gap -. direction_pull *. max(0., cos), cos);
       });
  let best =
    scored
    |> List.fold_left(
         (acc, (i, c, pts, u, gap, cos)) => {
           let bonus = s.winner == Some(i) ? stickiness : 0.;
           /* ALIGNMENT GATE: a track engages only when the pull points
              along it; nothing engages before ~6px — the lean covers
              that */
           if (cos <= 0.25) {
             acc;
           } else {
             switch (acc) {
             | Some((_, _, _, _, best_gap)) when gap -. bonus >= best_gap => acc
             | _ => Some((i, c, pts, u, gap -. bonus))
             };
           };
         },
         None,
       );
  switch (best) {
  | Some((i, c, pts, u, _)) =>
    s.winner = Some(i);
    s.u = sharpen_u(u);
    scrub_to(s, Some(i), s.u);
    {
      /* give = damped UNCONSUMED pull: the hand's offset from where the
         ladder holds the element — backwards, past the end, sideways —
         the grabbed unit leans a few px toward the hand */

      let q = along(pts, u);
      let rx = p.x -. q.x
      and ry = p.y -. q.y;
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
    /* nearing the ladder's end: build the next rung */
    if (s.ladders && u > float_of_int(n_rungs(c)) -. 0.5) {
      request_rung(s, i, c);
    };
  | None =>
    s.winner = None;
    s.u = 0.;
    scrub_to(s, None, 0.);
    /* dead-direction give: no track wants this pull, but the token
       leans a few damped px toward the pointer */
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
      switch (List.nth_opt(s.cands, w)) {
      | Some(c) => relax_from(c, s.u)
      | None => ()
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
    /* D2's drop: the nearest state — the closest rung */
    switch (
      s.winner |> Option.map(i => List.nth_opt(s.cands, i)) |> Option.join
    ) {
    | Some(c) when Float.round(s.u) >= 1. =>
      let k = min(n_rungs(c), int_of_float(Float.round(s.u)));
      let landing = List.nth(c.rungs, k - 1);
      /* handoff: the commit's FLIP starts each token from where the
         scrub holds it; the scrub animations are adopted so they die
         exactly when the flights take over */
      CodeFlip.set_drag_offsets(
        node_tracks(c)
        |> List.filter_map(((key, _, vals)) =>
             switch (key) {
             | Some(key) =>
               let (dx, dy, _) = value_at(vals, s.u);
               Some((key, (dx, dy)));
             | None => None
             }
           ),
      );
      if (landing.scroll_rows > 0) {
        CodeFlip.set_scroll_bump(~rows=landing.scroll_rows, ~near=s.text_box);
      };
      if (landing.enters != []) {
        CodeFlip.set_drag_enter(
          max(0., min(1., s.u -. float_of_int(k - 1))),
        );
      };
      /* emerge flights pair the clone positionally against the
         post-commit segment; several clones (a run of feeds) have no
         unique pairing, so they grow in */
      CodeFlip.set_emerge_src(k == 1 ? landing.emerge_src : []);
      CodeFlip.adopt(s.scrub_anims |> List.concat_map(snd));
      s.scrub_anims = [];
      s.scrub_active = None;
      s.commit(
        c.rungs
        |> List.filteri((j, _) => j < k)
        |> List.map((r: rung) => (r.kind, r.at)),
      );
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
      ~commit: list((Action.refactor, Id.t)) => unit,
      ~ladders: bool,
      ~push: bool,
      ~show_labels: bool,
      ~show_tracks: bool,
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
    ladders,
    push,
    show_labels,
    show_tracks,
    text_box,
    cands: [],
    winner: None,
    u: 0.,
    began: false,
    had_cands: false,
    segment: None,
    pending: AwaitGoal(goal, None),
    last_z: None,
    last_term: None,
    down_at: {
      x: cx -. r##.left,
      y: cy -. r##.top,
    },
    extend: None,
    extending: false,
    grab_node: None,
    grab_ids: None,
    local_give: [],
    local_give_g: {
      x: 0.,
      y: 0.,
    },
    listeners: [],
    scrub_anims: [],
    scrub_active: None,
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
   emission order (CodeFlip's correlation); found in the editor box the
   grab started in (the caret is not drawn until the editor is active,
   so locating by it left a first grab with no token motion) */
let live_pairs =
    (~text_box: Js.t(Dom_html.element), segment: Segment.t)
    : list((CodeFlip.key, Js.t(Dom.node))) => {
  let ct =
    Js.Unsafe.meth_call(
      text_box,
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
};

let sync =
    (
      ~settings: Language.CoreSettings.t,
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
      let pairs = live_pairs(~text_box=s.text_box, segment);
      /* tokens this candidate displaces, in px (single-row tokens
         only, matching CodeFlip's guard); candidate positions read
         through the kind's screen frame */
      let moved_for =
          (frame: Refactor.DragCandidate.frame, cand_m: Measured.t) =>
        pairs
        |> List.filter_map(((k, node)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas_end_aligned(~other=measured, cand_m, k),
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
               let plurality = Tile.arity(t) == 1 ? "mono" : "poly";
               let sort_cls = Sort.class_of(Tile.mold(t).out);
               (
                 t.shards
                 |> List.filter_map(i =>
                      switch (List.nth_opt(Tile.label(t), i)) {
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
      /* a live token whose text differs in the candidate (a use
         renamed by a merge): its old text leaves, its new text enters */
      let live_text = Hashtbl.create(List.length(pairs));
      pairs
      |> List.iter(((k, node)) =>
           Hashtbl.replace(
             live_text,
             k,
             Js.to_string(Js.Unsafe.get(node, "textContent")),
           )
         );
      let renamed = (k: CodeFlip.key, text: string) =>
        switch (Hashtbl.find_opt(live_text, k)) {
        | Some(t) => t != text
        | None => false
        };
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
             | (Some(_), Some(_)) when !renamed(k, text) => None
             | (_, Some(n)) =>
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
      /* tokens present live but absent in the candidate; a merge's
         dissolving copy converges onto its survivor's token, and a
         renamed token's old text fades on its way to its new place */
      let exits_for =
          (
            frame: Refactor.DragCandidate.frame,
            absorb: list((Id.t, Id.t)),
            cand_seg: Segment.t,
            cand_m: Measured.t,
          ) => {
        let texts = Hashtbl.create(256);
        token_texts(cand_seg)
        |> List.iter(((k, text, _)) => Hashtbl.replace(texts, k, text));
        let delta = (o: Measured.measurement, n: Measured.measurement) => {
          let p = Refactor.DragCandidate.frame_point(frame, n.origin);
          (
            float_of_int(p.col - o.origin.col) *. font_metrics.col_width,
            float_of_int(p.row - o.origin.row) *. font_metrics.row_height,
          );
        };
        pairs
        |> List.filter_map(((k, node)) =>
             switch (
               CodeFlip.find_meas(measured, k),
               CodeFlip.find_meas(cand_m, k),
             ) {
             | (Some(o), None) =>
               let into =
                 switch (k) {
                 | CodeFlip.Shard(id, i) =>
                   switch (List.assoc_opt(id, absorb)) {
                   | Some(v) =>
                     CodeFlip.find_meas(cand_m, Shard(v, i))
                     |> Option.map(delta(o))
                   | None => None
                   }
                 | _ => None
                 };
               Some((k, node, into));
             | (Some(o), Some(n)) =>
               switch (Hashtbl.find_opt(texts, k)) {
               | Some(text) when renamed(k, text) =>
                 Some((k, node, Some(delta(o, n))))
               | _ => None
               }
             | _ => None
             }
           );
      };
      /* anchored-deco delta closure: same deltas as the anchor
         tokens, read from the two measured maps on demand */
      let deco_delta_for =
          (
            frame: Refactor.DragCandidate.frame,
            cand_m: Measured.t,
            (id, shard): (Id.t, option(int)),
          )
          : option(deco_step) =>
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
            Some(Shift(dx, dy));
          };
        | (Some(_), None) => Some(Gone)
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
      /* a rung: one state, laid out against the live layout. `at` is
         where its step is invoked; `held` the element the next rung
         moves — the grab while it survives, else the state's focus */
      let mk_rung = (c: Refactor.DragCandidate.t, ~origin: vec, ~held) => {
        let moved = moved_for(c.frame, c.measured);
        let exits = exits_for(c.frame, c.absorb, c.segment, c.measured);
        {
          kind: c.kind,
          label: c.label,
          at: c.at,
          tgt: px(c.target),
          scroll_rows: c.frame.scroll_rows,
          moved,
          deco_delta: deco_delta_for(c.frame, c.measured),
          exits,
          enters: enters_for(c.frame, c.emerge, c.segment, c.measured),
          absorb_into:
            c.absorb
            |> List.concat_map(((d, v)) =>
                 List.init(4, i => i)
                 |> List.filter_map(i =>
                      CodeFlip.find_meas(c.measured, Shard(v, i))
                      |> Option.map((m: Measured.measurement) => {
                           let p =
                             Refactor.DragCandidate.frame_point(
                               c.frame,
                               m.origin,
                             );
                           (
                             CodeFlip.Shard(d, i),
                             {
                               x:
                                 float_of_int(p.col) *. font_metrics.col_width,
                               y:
                                 float_of_int(p.row)
                                 *. font_metrics.row_height,
                             },
                           );
                         })
                    )
               ),
          emerge_src: c.emerge |> List.map(snd),
          /* the caret is the hand: it rides the track from where the
             held piece was to where this state puts it */
          caret_delta: {
            let t = px(c.target);
            Some((t.x -. origin.x, t.y -. origin.y));
          },
          cand: c,
          held:
            Id.Map.mem(fst(held), c.measured.tiles)
              ? held : (c.focus, None),
        };
      };
      s.extend =
        Some(
          (lad: cand) => {
            let last = List.nth(lad.rungs, List.length(lad.rungs) - 1);
            Refactor.next_rung(
              ~settings,
              ~info_map,
              ~root=term,
              ~shape_map,
              ~push=s.push,
              ~family=lad.kind,
              ~held=last.held,
              last.cand,
            )
            |> Option.map(((c: Refactor.DragCandidate.t, used)) => {
                 /* a step that added a line (extract) laid its state out
                    against the screen with a global shift; later steps
                    are laid out against that state, so they inherit
                    the shift */
                 let base = last.cand.frame;
                 let c =
                   c.frame == Refactor.DragCandidate.no_frame
                   && base != Refactor.DragCandidate.no_frame
                   && base.shift_from == 0
                     ? {
                       ...c,
                       frame: base,
                       target:
                         Refactor.DragCandidate.frame_point(base, c.target),
                     }
                     : c;
                 mk_rung(c, ~origin=lad.cur, ~held=used);
               });
          },
        );
      let cands =
        switch (grab_id) {
        | Some(target) =>
          Refactor.drag_candidates(
            ~composites=settings.drag_composites,
            ~info_map,
            ~term,
            ~measured,
            ~shape_map,
            ~push=s.push,
            ~target,
            ~shard=grab_shard,
            (),
          )
          |> List.map((c: Refactor.DragCandidate.t) =>
               {
                 kind: c.kind,
                 label: c.label,
                 cur: px(c.current),
                 rungs: [
                   mk_rung(
                     c,
                     ~origin=px(c.current),
                     ~held=(target, grab_shard),
                   ),
                 ],
                 exhausted: !s.ladders,
               }
             )
        | None => []
        };
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
      /* landed: the caret sits at the clicked point, or on the token
         the click was inside (a mid-token click puts the caret at
         that token's edge — and when the editor wasn't active, the
         first sync already sees it there, so "changed" never fires) */
      let on_clicked_token =
        switch (Indicated.index(z)) {
        | Some(id) =>
          switch (Id.Map.find_opt(id, measured.tiles)) {
          | Some(shards) =>
            shards
            |> List.exists(((_, m: Measured.measurement)) =>
                 m.origin.row == goal.row
                 && m.last.row == goal.row
                 && m.origin.col <= goal.col
                 && goal.col <= m.last.col
               )
          | None => false
          }
        | None => false
        };
      let here = Zipper.Caret.point(measured, z) == goal || on_clicked_token;
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
