/* Cause-driven compensation for refractor (probe-drawer) height changes above
 * the locus (the caret, or the probe that owns the keyboard): scroll #main by
 * the exact row delta so the locus stays put. This is also what keeps a probe
 * still when Left/Right sample nav reflows the drawers above it. Gated on
 * refractor_rows ref identity (zero work on unrelated frames). Replaces the
 * old symptom-driven CaretAnchor, which conflated drawer shifts with unrelated
 * caret motion. Compensates the editor Main hands it (Page.Update.get_editor)
 * — in multi-cell views that is the primary cell only. */

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

/* A probe that owns the keyboard is the locus (the caret is hidden then). It
 * displays on its term's last row; focusing it put the caret on the first. */
let locus_row = (~measured: Measured.t, z: Zipper.t): int => {
  let focused_probe = {
    open Util.OptUtil.Syntax;
    let* el = Js.Opt.to_option(Dom_html.document##.activeElement);
    let* probe =
      Js.Opt.to_option(
        el##closest(Js.string(".live-offside[data-probe-id]")),
      );
    let* id =
      Js.Opt.to_option(probe##getAttribute(Js.string("data-probe-id")));
    let* id = Id.of_string(Js.to_string(id));
    Measured.find_by_id(id, measured);
  };
  switch (focused_probe) {
  | Some({last, _}) => last.row
  | None => Zipper.Caret.point(measured, z).row
  };
};

/* refractor_rows holds nonzero entries only, so a drawer opening/closing
 * appears as a key appearing/disappearing: merge over the key union with
 * absent = 0 rows. A drawer's rows follow its term's LAST row (Measured
 * defers them past the tile's last shard), so that is what must be above. */
let above_locus_delta_rows =
    (
      ~prev: Id.Map.t(int),
      ~curr: Id.Map.t(int),
      ~measured: Measured.t,
      ~locus_row: int,
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
         | Some({last, _}) when last.row < locus_row => acc + delta
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
      when key == editor_key && prev_rows === refractor_rows =>
    ()
  | Some((key, prev_rows)) when key == editor_key =>
    let delta_rows =
      above_locus_delta_rows(
        ~prev=prev_rows,
        ~curr=refractor_rows,
        ~measured,
        ~locus_row=locus_row(~measured, z),
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
