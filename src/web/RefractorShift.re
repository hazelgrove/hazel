/* Cause-driven compensation for refractor (probe-drawer) height changes above
 * the caret: when a drawer above the locus grows/shrinks, scroll #main by the
 * exact row delta so the caret stays put. Gated on refractor_shape_map ref
 * identity (zero work on unrelated frames). Replaces the old symptom-driven
 * CaretAnchor, which conflated drawer shifts with unrelated caret motion. */

open Js_of_ocaml;
open Haz3lcore;

let prev_shape_map: ref(option(Id.Map.t(int))) = ref(None);

let scroll_main_by = (dy: float): unit =>
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("main")),
    main => {
      let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      Js.Unsafe.set(main, Js.string("scrollTop"), st +. dy);
    },
  );

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
    /* skip while EdgeScroll is driving the viewport, to avoid fighting it */
    if (delta_rows != 0 && !EdgeScroll.is_active()) {
      let delta_px = float_of_int(delta_rows) *. font_metrics.row_height;
      scroll_main_by(delta_px);
    };
    prev_shape_map := Some(refractor_shape_map);
  };
