open Js_of_ocaml;
open Haz3lcore;
open Util;

/* Offside staggering, top-down greedy (see plans/agent-canvas-docket.md):
   an offside display spanning K rows (a card fan, a tall rich view) must
   clear (a) the end of every code line it spans — not just its own — and
   (b) any overlapping display owned by rows above. Rows above have
   priority; displays below re-solve against the result. A single pass
   terminates because displays only ever move right.

   Runs in after_display (precedent: RefractorShift/ScrollWidth): widths
   and heights come from the rendered DOM, the per-row line ends from
   Measured, and positions are patched imperatively before paint. The
   base render position (ProjectorView.offside_base) is reproduced as
   the single-row floor, so nothing moves unless staggering demands it. */

/* keep in sync with ProjectorView.offside_offset / stack_gap */
let offside_offset = 4;
let stack_gap = 2;

type item = {
  row: int,
  origin_col: int,
  el: Js.t(Dom_html.element),
  w_px: float,
  h_px: float,
};

let update = (~measured: Measured.t, ~font_metrics: FontMetrics.t): unit => {
  let nodes =
    Dom_html.document##querySelectorAll(Js.string(".offside-wrapper"));
  let items = ref([]);
  for (i in 0 to nodes##.length - 1) {
    switch (Js.Opt.to_option(nodes##item(i))) {
    | Some(el) =>
      let attr = (a: string): option(int) =>
        Js.Opt.case(
          el##getAttribute(Js.string(a)),
          () => None,
          s => int_of_string_opt(Js.to_string(s)),
        );
      switch (attr("data-row"), attr("data-ocol")) {
      | (Some(row), Some(origin_col)) =>
        let rect = Js.Unsafe.meth_call(el, "getBoundingClientRect", [||]);
        let w_px: float = Js.Unsafe.coerce(rect)##.width;
        let h_px: float = Js.Unsafe.coerce(rect)##.height;
        items :=
          [
            {
              row,
              origin_col,
              el,
              w_px,
              h_px,
            },
            ...items^,
          ];
      | _ => ()
      };
    | None => ()
    };
  };
  let items =
    List.sort(
      (a: item, b: item) =>
        a.row == b.row
          ? compare(a.origin_col, b.origin_col) : compare(a.row, b.row),
      items^,
    );
  let row_end = (r: int): int =>
    switch (IntMap.find_opt(r, measured.rows)) {
    | Some(row) => Measured.Rows.(row.max_col)
    | None => 0
    };
  /* (first row, last row, left col, right col) of placed displays */
  let occupied: ref(list((int, int, int, int))) = ref([]);
  List.iter(
    (it: item) => {
      let rows_spanned =
        max(
          1,
          int_of_float(
            Float.ceil(it.h_px /. font_metrics.row_height -. 0.2),
          ),
        );
      let last_row = it.row + rows_spanned - 1;
      let floor_col = ref(0);
      for (r in it.row to last_row) {
        floor_col := max(floor_col^, row_end(r) + offside_offset);
      };
      let w_cols =
        int_of_float(Float.ceil(it.w_px /. font_metrics.col_width));
      /* first-fit: slide into the leftmost gap at or after the floor —
         a display can sit BEFORE one from a row above when the overhang
         above leaves room (no unnecessary staggering) */
      let blockers =
        occupied^
        |> List.filter(((a, b, _, _)) => !(last_row < a || b < it.row))
        |> List.map(((_, _, l, r)) => (l, r))
        |> List.sort(((l1, _), (l2, _)) => compare(l1, l2));
      let x = ref(floor_col^);
      List.iter(
        ((l, r)) =>
          if (x^ + w_cols + stack_gap <= l) {
            (); /* fits entirely before this blocker */
          } else if (x^ < r + stack_gap) {
            x := r + stack_gap;
          },
        blockers,
      );
      let left_px =
        font_metrics.col_width *. float_of_int(x^ - it.origin_col);
      Js.Unsafe.coerce(it.el)##.style##.left :=
        Js.string(Printf.sprintf("%.1fpx", left_px));
      occupied := [(it.row, last_row, x^, x^ + w_cols), ...occupied^];
    },
    items,
  );
};
