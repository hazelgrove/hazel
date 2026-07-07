open Util;
open Js_of_ocaml;
open Haz3lcore;

/* FLIP ghosts for code movement (currently: refactor invocations).
 * The real view renders the new state immediately and is not touched;
 * transient spans (plain DOM, outside the vdom) fly each moved text
 * run from its old grid position to its new one, then remove
 * themselves. Both endpoints come from diffing the before/after
 * Measured maps — pure arithmetic, no layout reads, no per-token DOM
 * ids. Companion to Animation.re (the caret's DOM-box FLIP).
 *
 * request() during the MVU update captures the pre-edit syntax;
 * go() after render (Main.re) diffs against the post-edit syntax. */

/* a leaf token of the display: a tile shard or a secondary */
type leaf = {
  id: Id.t,
  index: int, /* shard index within its tile; 0 for secondaries */
  text: string,
};

let rec leaves_of_segment = (seg: Segment.t): list(leaf) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         Aba.mk(t.shards, t.children)
         |> Aba.join(
              i =>
                [
                  {
                    id: t.id,
                    index: i,
                    text: List.nth(t.label, i),
                  },
                ],
              leaves_of_segment,
            )
         |> List.concat
       | Secondary(s) when Secondary.is_linebreak(s) => []
       | Secondary(s) =>
         let text =
           switch (s.content) {
           | Whitespace(str)
           | Comment(str) => str
           };
         [
           {
             id: s.id,
             index: 0,
             text,
           },
         ];
       | Grout(_)
       | Projector(_) => []
       }
     );

let find_meas =
    (m: Measured.t, ~id: Id.t, ~index: int): option(Measured.measurement) =>
  switch (Id.Map.find_opt(id, m.tiles)) {
  | Some(shards) => List.assoc_opt(index, shards)
  | None => Id.Map.find_opt(id, m.secondary)
  };

/* a run of textually adjacent leaves sharing one movement vector */
type run = {
  text: string,
  origin: Point.t, /* new position (grid) */
  d: Point.t /* movement: new - old */
};

/* perf guards: a transition that moves this much is better skipped
 * than animated */
let max_moved_leaves = 2000;
let max_runs = 64;

let moved_runs =
    (leaves: list(leaf), old_m: Measured.t, new_m: Measured.t): list(run) => {
  let moves =
    leaves
    |> List.filter_map(l =>
         switch (
           find_meas(old_m, ~id=l.id, ~index=l.index),
           find_meas(new_m, ~id=l.id, ~index=l.index),
         ) {
         | (Some(o), Some(n))
             when
               o.origin != n.origin
               && o.origin.row == o.last.row
               && n.origin.row == n.last.row =>
           Some((l, o, n))
         | _ => None
         }
       );
  if (List.length(moves) > max_moved_leaves) {
    [];
  } else {
    /* accumulate runs; cur = (text, old_row, old_end_col, new_origin, d) */
    let (runs, cur) =
      moves
      |> List.fold_left(
           (
             (runs, cur),
             (l: leaf, o: Measured.measurement, n: Measured.measurement),
           ) => {
             let d =
               Point.{
                 row: n.origin.row - o.origin.row,
                 col: n.origin.col - o.origin.col,
               };
             switch (cur) {
             | Some((text, old_row, old_end, origin, d'))
                 when
                   d == d'
                   && o.origin.row == old_row
                   && o.origin.col == old_end => (
                 runs,
                 Some((text ++ l.text, old_row, o.last.col, origin, d)),
               )
             | _ =>
               let runs =
                 switch (cur) {
                 | Some((text, _, _, origin, d)) => [
                     {
                       text,
                       origin,
                       d,
                     },
                     ...runs,
                   ]
                 | None => runs
                 };
               (runs, Some((l.text, o.origin.row, o.last.col, n.origin, d)));
             };
           },
           ([], None),
         );
    let runs =
      switch (cur) {
      | Some((text, _, _, origin, d)) => [
          {
            text,
            origin,
            d,
          },
          ...runs,
        ]
      | None => runs
      };
    List.length(runs) > max_runs ? [] : List.rev(runs);
  };
};

type pending = {
  leaves: list(leaf),
  old_measured: Measured.t,
};

let pending: ref(option(pending)) = ref(None);

/* Call during the MVU update, before the edit applies */
let request = (syntax: CachedSyntax.t): unit =>
  pending :=
    Some({
      leaves: leaves_of_segment(syntax.segment),
      old_measured: syntax.measured,
    });

/* Slowed way down for evaluation; production values more like
 * duration 160 with Animation.easeOutExpo */
let duration = 800;
let easing = "ease-in-out";

/* Each run gets a flying span (text over cell background) plus a
 * static cover over its landing zone: the real view already shows the
 * post-edit text at the destination, and an uncovered destination
 * plus a converging copy reads as a flicker/double-image. The flyer
 * lands exactly on the cover; removing both reveals the real text. */
let spawn = (~font_metrics: FontMetrics.t, parent, r: run): unit => {
  let doc = Dom_html.document;
  let left = float_of_int(r.origin.col) *. font_metrics.col_width;
  let top = float_of_int(r.origin.row) *. font_metrics.row_height;
  let width = float_of_int(String.length(r.text)) *. font_metrics.col_width;
  let cover = Dom_html.createSpan(doc);
  cover##.className := Js.string("flip-ghost flip-ghost-cover");
  cover##.style##.cssText :=
    Js.string(
      Printf.sprintf(
        "position:absolute;left:%fpx;top:%fpx;width:%fpx;height:%fpx;",
        left,
        top,
        width,
        font_metrics.row_height,
      ),
    );
  let sp = Dom_html.createSpan(doc);
  sp##.className := Js.string("flip-ghost");
  sp##.textContent := Js.some(Js.string(r.text));
  sp##.style##.cssText :=
    Js.string(
      Printf.sprintf(
        "position:absolute;left:%fpx;top:%fpx;height:%fpx;",
        left,
        top,
        font_metrics.row_height,
      ),
    );
  Dom.appendChild(parent, cover);
  Dom.appendChild(parent, sp);
  let remove = () => {
    Js.Unsafe.meth_call(sp, "remove", [||]) |> ignore;
    Js.Unsafe.meth_call(cover, "remove", [||]) |> ignore;
  };
  /* FLIP: place at the new position, animate the inverted delta to 0 */
  let keyframes =
    Animation.Js.keyframes_unsafe([
      (
        "transform",
        Printf.sprintf(
          "translate(%fpx, %fpx)",
          -. float_of_int(r.d.col) *. font_metrics.col_width,
          -. float_of_int(r.d.row) *. font_metrics.row_height,
        ),
      ),
      ("transform", "translate(0px, 0px)"),
    ]);
  let options =
    Animation.Js.options_unsafe({
      duration,
      easing,
    });
  switch (
    Js.Unsafe.meth_call(
      sp,
      "animate",
      [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
    )
  ) {
  | exception _ => remove() /* no WAAPI: just show the final state */
  | anim =>
    Js.Unsafe.set(anim, "onfinish", Js.wrap_callback(_ => remove()));
    /* safety net: never leave a stale ghost */
    Dom_html.window##setTimeout(
      Js.wrap_callback(() => remove()),
      float_of_int(duration + 400),
    )
    |> ignore;
  };
};

/* A fresh batch obsoletes any still-flying ghosts (relevant when
 * animating every edit: generations would otherwise stack up) */
let clear_stale = (): unit => {
  let stale = Dom_html.document##querySelectorAll(Js.string(".flip-ghost"));
  List.init(stale##.length, i => stale##item(i))
  |> List.iter(n =>
       Js.Opt.iter(n, n => Js.Unsafe.meth_call(n, "remove", [||]) |> ignore)
     );
};

/* Call after render; ghosts join the caret's coordinate frame */
let go = (~syntax: CachedSyntax.t, ~font_metrics: FontMetrics.t): unit =>
  switch (pending^) {
  | None => ()
  | Some(p) =>
    pending := None;
    switch (
      moved_runs(p.leaves, p.old_measured, syntax.measured),
      JsUtil.get_elem_by_id_opt("caret"),
    ) {
    | ([], _)
    | (_, None) => ()
    | (runs, Some(caret)) =>
      clear_stale();
      Js.Opt.iter(caret##.parentNode, parent =>
        runs |> List.iter(spawn(~font_metrics, parent))
      );
    };
  };
