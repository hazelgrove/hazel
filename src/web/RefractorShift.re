/* Cause-driven compensation for refractor (probe-drawer) height changes above
 * the caret: when a drawer above the locus grows/shrinks, scroll #main by the
 * exact row delta so the caret stays put. Gated on refractor_rows ref
 * identity (zero work on unrelated frames). Replaces the old symptom-driven
 * CaretAnchor, which conflated drawer shifts with unrelated caret motion.
 * Compensates the editor Main hands it (Page.Update.get_editor) — in
 * multi-cell views that is the primary cell only. */

open Js_of_ocaml;
open Haz3lcore;

/* Baseline keyed by editor identity (Editors.Model.editor_key): comparing one
 * editor's map against another's (slide/exercise/mode switch) would read its
 * already-open drawers as freshly opened and scroll #main for nothing. */
let prev: ref(option((string, Id.Map.t(int)))) = ref(None);

let scroll_main_by = (dy: float): unit =>
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("main")),
    main => {
      let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      Js.Unsafe.set(main, Js.string("scrollTop"), st +. dy);
    },
  );

/* refractor_rows holds nonzero entries only, so a drawer opening/closing
 * appears as a key appearing/disappearing: merge over the key union with
 * absent = 0 rows. */
let above_locus_delta_rows =
    (
      ~prev: Id.Map.t(int),
      ~curr: Id.Map.t(int),
      ~measured: Measured.t,
      ~caret_row: int,
    )
    : int =>
  Id.Map.merge(
    (_, old_h, new_h) => {
      let old_h = Option.value(old_h, ~default=0);
      let new_h = Option.value(new_h, ~default=0);
      old_h == new_h ? None : Some(new_h - old_h);
    },
    prev,
    curr,
  )
  |> Id.Map.fold(
       (id, delta, acc) =>
         switch (Measured.find_by_id(id, measured)) {
         | Some({origin, _}) when origin.row < caret_row => acc + delta
         | _ => acc
         },
       _,
       0,
     );

let update =
    (
      ~editor_key: string,
      ~font_metrics: FontMetrics.t,
      ~refractor_rows: Id.Map.t(int),
      ~measured: Measured.t,
      z: Zipper.t,
    )
    : unit =>
  switch (prev^) {
  /* Identity gate assumes CachedSyntax is the sole producer: it reuses
   * the same physical map while refractor shapes are unchanged. Cloning
   * or rebuilding the map elsewhere would silently defeat the gate. */
  | Some((key, prev_rows))
      when
        String.equal(key, editor_key)
        && phys_equal(prev_rows, refractor_rows) =>
    ()
  | Some((key, prev_rows)) when String.equal(key, editor_key) =>
    let caret_row = Zipper.Caret.point(measured, z).row;
    let delta_rows =
      above_locus_delta_rows(
        ~prev=prev_rows,
        ~curr=refractor_rows,
        ~measured,
        ~caret_row,
      );
    /* skip while EdgeScroll is driving the viewport, to avoid fighting it */
    if (delta_rows != 0 && !EdgeScroll.is_active()) {
      let delta_px = float_of_int(delta_rows) *. font_metrics.row_height;
      scroll_main_by(delta_px);
    };
    prev := Some((editor_key, refractor_rows));
  /* first frame, or a different editor: rebaseline, no compensation */
  | _ => prev := Some((editor_key, refractor_rows))
  };
