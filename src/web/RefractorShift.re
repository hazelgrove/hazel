/* Cause-driven compensation for refractor (probe drawer) height changes
 * above the user's locus.
 *
 * Replaces the symptom-driven CaretAnchor. Instead of observing the
 * caret's screen-y every frame and trying to undo unexplained shifts
 * (which conflates drawer-height changes with selection edge motion,
 * natural caret moves, and any other layout-y thing), this module
 * detects the actual cause and computes the exact compensation:
 *
 *   delta_rows = Σ over (id ∈ refractor_shape_map, id is above caret)
 *                  of (new_height − old_height)
 *
 *   if delta_rows ≠ 0:  #main.scrollTop += delta_rows * row_height
 *
 * `refractor_shape_map` is the single source of truth for how many
 * extra rows each refractor reserves; it's recomputed by
 * `CachedSyntax.refresh_shapes` exactly when something could shift
 * drawer heights (statics/dynamics/elaborated/refractor model). On
 * frames where the map's reference is unchanged we do zero work.
 *
 * Limitations / scope:
 *   - Only sums ids present in BOTH old and new map. Add/remove events
 *     (which only happen across full `mk` calls i.e. edits that alter
 *     the segment) aren't compensated; scroll-into-view handles caret
 *     positioning post-edit independently.
 *   - "Above caret" uses caret row. The focus-bar Left/Right case is
 *     still handled by the existing narrow `SampleAnchor` mechanism.
 *   - Skipped while `EdgeScroll`'s timer is active: during edge-scroll
 *     the viewport motion is the user's intent; drawer shifts during
 *     that window are dropped and the baseline resyncs at drag end. */

open Js_of_ocaml;
open Haz3lcore;
module ScrollDebug = Util.ScrollDebug;

let prev_shape_map: ref(option(Id.Map.t(int))) = ref(None);

let scroll_main_by = (dy: float): unit =>
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("main")),
    main => {
      let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      Js.Unsafe.set(main, Js.string("scrollTop"), st +. dy);
    },
  );

/* Sum (new_h - old_h) over ids present in both maps where the id's
 * row in `measured` is strictly above `caret_row`. Ids present only
 * in the new map (additions) and ids that have no `Measured` entry
 * (not yet placed) are skipped. */
let above_locus_delta_rows =
    (
      ~prev: Id.Map.t(int),
      ~curr: Id.Map.t(int),
      ~measured: Measured.t,
      ~caret_row: int,
    )
    : int =>
  Id.Map.fold(
    (id, new_h, acc) =>
      switch (Id.Map.find_opt(id, prev)) {
      | None => acc
      | Some(old_h) when old_h == new_h => acc
      | Some(old_h) =>
        switch (Measured.find_by_id(id, measured)) {
        | Some({origin, _}) when origin.row < caret_row =>
          acc + (new_h - old_h)
        | _ => acc
        }
      },
    curr,
    0,
  );

let update =
    (
      ~font_metrics: FontMetrics.t,
      ~refractor_shape_map: Id.Map.t(int),
      ~measured: Measured.t,
      z: Zipper.t,
    )
    : unit =>
  switch (prev_shape_map^) {
  | None => prev_shape_map := Some(refractor_shape_map)
  | Some(prev) when prev === refractor_shape_map => ()
  | Some(prev) =>
    let caret_row = Zipper.Caret.point(measured, z).row;
    let delta_rows =
      above_locus_delta_rows(
        ~prev,
        ~curr=refractor_shape_map,
        ~measured,
        ~caret_row,
      );
    if (delta_rows != 0) {
      if (EdgeScroll.is_active()) {
        /* Drop the shift on the floor — the user is asking the viewport
         * to move via EdgeScroll; we don't want to fight that. The
         * baseline resyncs to the new map below. */
        ScrollDebug.log(
          "RS",
          Printf.sprintf(
            "skip (EdgeScroll active) delta_rows=%d",
            delta_rows,
          ),
        );
      } else {
        let delta_px = float_of_int(delta_rows) *. font_metrics.row_height;
        scroll_main_by(delta_px);
        ScrollDebug.log(
          "RS",
          Printf.sprintf(
            "compensate delta_rows=%d delta_px=%+.1f caret_row=%d",
            delta_rows,
            delta_px,
            caret_row,
          ),
        );
        ScrollDebug.mark_sT();
      };
    };
    prev_shape_map := Some(refractor_shape_map);
  };
