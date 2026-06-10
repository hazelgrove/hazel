/* Cause-driven publishing of #main's effective scroll width.
 *
 * The cell's background must stretch across everything the page has
 * been pushed to by absolutely-positioned probe overlays / drawers,
 * which CSS intrinsic sizing can't see — so we measure scrollWidth and
 * publish it as the `--main-scroll-width` CSS variable (see
 * JsUtil.update_main_scroll_width). The measurement is a write-read-
 * write on a :root variable, forcing two whole-document layouts whose
 * cost scales with probe count, so it must NOT run every frame.
 *
 * Like RefractorShift, this runs from Main.after_display and gates on
 * its actual inputs, re-measuring only when one changed:
 *   - measured / refractor_shape_map (by reference): code layout and
 *     drawer heights; both are rebuilt by CachedSyntax exactly when an
 *     edit or a worker result could move things.
 *   - ProbeProj.Settings.version: probe display state (sample lengths,
 *     window mode, dropdowns, sticky/dock) — every writer bumps it.
 *   - sample_focus: changes which samples render (windowing).
 *   - font metrics and viewport width: px scaling of everything. */

open Js_of_ocaml;
open Haz3lcore;

type key = (
  Measured.t,
  Id.Map.t(int),
  int, /* ProbeProj.Settings.version */
  Language.Sample.Focus.t,
  FontMetrics.t,
  int /* viewport width */,
);

let last_key: ref(option(key)) = ref(None);

let update =
    (
      ~measured: Measured.t,
      ~refractor_shape_map: Id.Map.t(int),
      ~sample_focus: Language.Sample.Focus.t,
      ~font_metrics: FontMetrics.t,
    )
    : unit => {
  let viewport_w = Dom_html.document##.documentElement##.clientWidth;
  let version = ProbeProj.Settings.version^;
  let stale =
    switch (last_key^) {
    | Some((m, rsm, v, sf, fm, w)) =>
      m !== measured
      || rsm !== refractor_shape_map
      || v != version
      || sf != sample_focus
      || fm != font_metrics
      || w != viewport_w
    | None => true
    };
  if (stale) {
    Util.JsUtil.update_main_scroll_width();
    last_key :=
      Some((
        measured,
        refractor_shape_map,
        version,
        sample_focus,
        font_metrics,
        viewport_w,
      ));
  };
};
