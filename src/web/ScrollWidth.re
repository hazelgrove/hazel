/* Publishes #main's effective scrollWidth as the `--main-scroll-width` CSS var
 * (see JsUtil.update_main_scroll_width) so the cell background stretches under
 * absolutely-positioned probe overlays/drawers, which CSS intrinsic sizing
 * can't see. The measure forces two whole-document layouts (cost scales with
 * probe count), so we gate on `key` and re-measure only when an input changes;
 * and, since every commit changes `key`, the exact measure waits for things
 * to settle, and an edit only grows the width (grow_main_scroll_width): a
 * read, after ProbeStagger has placed the displays. */

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

/* The exact measure, once things settle: after an edit or a release,
   which re-measures on every commit, it catches the content shrinking
   (growing is caught at once, by grow_main_scroll_width). */
let settle_ms = 400.;
let settle_timer = ref(None);
let schedule_exact = () => {
  switch (settle_timer^) {
  | Some(t) => Dom_html.clearTimeout(t)
  | None => ()
  };
  settle_timer :=
    Some(
      Dom_html.setTimeout(
        () => {
          settle_timer := None;
          Util.JsUtil.update_main_scroll_width();
        },
        settle_ms,
      ),
    );
};

let update =
    (
      ~measured: Measured.t,
      ~refractor_rows: Id.Map.t(int),
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
      || rsm !== refractor_rows
      || v != version
      || sf != sample_focus
      || fm != font_metrics
      || w != viewport_w
      || vr != visible_rows
    | None => true
    };
  if (stale) {
    if (Util.JsUtil.grow_main_scroll_width()) {
      schedule_exact();
    } else {
      Util.JsUtil.update_main_scroll_width();
    };
    last_key :=
      Some((
        measured,
        refractor_rows,
        version,
        sample_focus,
        font_metrics,
        viewport_w,
        visible_rows,
      ));
  };
};
