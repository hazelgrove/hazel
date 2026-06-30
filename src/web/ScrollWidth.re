/* Publishes #main's effective scrollWidth as the `--main-scroll-width` CSS var
 * (see JsUtil.update_main_scroll_width) so the cell background stretches under
 * absolutely-positioned probe overlays/drawers, which CSS intrinsic sizing
 * can't see. The measure forces two whole-document layouts (cost scales with
 * probe count), so we gate on `key` and re-measure only when an input changes. */

open Js_of_ocaml;
open Haz3lcore;

type key = (
  Measured.t,
  Id.Map.t(int),
  int, /* ProbeProj.Settings.version */
  Language.Sample.Focus.t,
  FontMetrics.t,
  int, /* viewport width */
  option(Globals.VisibleRows.t) /* culling range: changes what renders */,
);

let last_key: ref(option(key)) = ref(None);

let update =
    (
      ~measured: Measured.t,
      ~refractor_shape_map: Id.Map.t(int),
      ~sample_focus: Language.Sample.Focus.t,
      ~font_metrics: FontMetrics.t,
      ~visible_rows: option(Globals.VisibleRows.t),
    )
    : unit => {
  let viewport_w = Dom_html.document##.documentElement##.clientWidth;
  let version = ProbeProj.Settings.version^;
  let stale =
    switch (last_key^) {
    | Some((m, rsm, v, sf, fm, w, vr)) =>
      m !== measured
      || rsm !== refractor_shape_map
      || v != version
      || sf != sample_focus
      || fm != font_metrics
      || w != viewport_w
      || vr != visible_rows
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
        visible_rows,
      ));
  };
};
