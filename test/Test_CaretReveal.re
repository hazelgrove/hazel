open Alcotest;
open Util;

/* the arithmetic behind a burst reveal: no DOM, so the band decision and
   the anchor read are checked on numbers */

let height = 500.;
let rh = 20.;
let margin = height *. CaretReveal.margin_ratio;
let delta = (~editor_top=0., ~scroll_top, row) =>
  CaretReveal.band_delta(~editor_top, ~scroll_top, ~height, row, rh);
let close = (name, want, got) => check(float(0.001), name, want, got);

let band = () => {
  /* rows 3 to 21 sit clear of both 50px margins at scroll 0 */
  close("inside the band", 0., delta(~scroll_top=0., 10));
  close("just inside at the bottom", 0., delta(~scroll_top=0., 21));
  /* row 30 spans 600 to 620: bring its bottom to 450 */
  close("below scrolls down", 170., delta(~scroll_top=0., 30));
  close("then sits at the margin", 0., delta(~scroll_top=170., 30));
  /* row 12 spans 240 to 260: at scroll 300 its top is at -60, so up to 50 */
  close("above scrolls up", -110., delta(~scroll_top=300., 12));
  close("then sits at the margin", 0., delta(~scroll_top=190., 12));
  close(
    "the editor's offset counts",
    0.,
    delta(~editor_top=200., ~scroll_top=200., 10),
  );
};

/* typing new lines at the bottom: each line scrolls by one row, and the
   caret's row keeps its place at the bottom margin */
let burst = () => {
  let scroll = ref(delta(~scroll_top=0., 22));
  for (row in 23 to 60) {
    let d = delta(~scroll_top=scroll^, row);
    close("one row per line", rh, d);
    scroll := scroll^ +. d;
  };
  let bottom = 60. *. rh +. rh -. scroll^;
  close("still at the margin", height -. margin, bottom);
};

/* the anchor read off a caret rect gives back the editor's origin, so a
   reveal after a pause starts from where the caret really is */
let anchor = () => {
  let editor_top = 140.;
  let scroll_top = 260.;
  let row = 17;
  let cont_top = 38.;
  let caret_top =
    cont_top +. editor_top +. float_of_int(row) *. rh -. scroll_top;
  close(
    "round trip",
    editor_top,
    CaretReveal.anchor_of(~caret_top, ~cont_top, ~scroll_top, row, rh),
  );
};

let tests = (
  "CaretReveal",
  [
    test_case("the margin band", `Quick, band),
    test_case("a burst of new lines", `Quick, burst),
    test_case("the anchor", `Quick, anchor),
  ],
);
