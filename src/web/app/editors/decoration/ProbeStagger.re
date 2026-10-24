open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;

/* Offside staggering, top-down greedy: an offside display spanning K rows
   (a card fan, a tall rich view) must clear (a) the end of every code line
   it spans — not just its own — and (b) any overlapping display owned by
   rows above. Rows above have priority; displays below re-solve against
   the result. A single pass terminates because displays only ever move
   right.

   It runs from after_display without reading layout: widths and heights
   come from the rendered DOM, read once a display first shows and again
   when AfterLayout's observer reports its size changed, and positions are
   patched before paint. The pass is solved PER code container, each
   against its own line ends —
   CodeWithStatics writes them onto the container as `data-row-ends` (via
   `row_ends_attr`) from that editor's Measured, so multi-cell views
   (exercises, derivations, instructor tutorials) never read another
   cell's rows or collide across cells. The base render position
   (ProjectorView.offside_base) is reproduced as the single-row floor, so
   nothing moves unless staggering demands it. */

/* keep in sync with ProjectorView.offside_offset / stack_gap */
let offside_offset = 4;
let stack_gap = 2;

let row_ends_attr_name = "data-row-ends";

/* Dense per-row line ends ("max_col,max_col,..." indexed by row; absent
   rows read as 0), attached to a code container for `update`. */
let row_ends_attr = (measured: Measured.t): Attr.t => {
  let n = Measured.num_rows(measured);
  List.init(n, r =>
    switch (Measured.row_shape(r, measured)) {
    | Some(row) => string_of_int(Measured.Rows.(row.max_col))
    | None => "0"
    }
  )
  |> String.concat(",")
  |> Attr.create(row_ends_attr_name);
};

let parse_row_ends = (s: Js.t(Js.js_string)): array(int) =>
  Js.to_string(s)
  |> String.split_on_char(',')
  |> List.map(s => Option.value(int_of_string_opt(s), ~default=0))
  |> Array.of_list;

/* each container's last attribute and its parse: a frame that didn't
   change its lines reuses it */
let parsed:
  ref(list((Js.t(Dom_html.element), Js.t(Js.js_string), array(int)))) =
  ref([]);

let row_ends_of_container = (el: Js.t(Dom_html.element)): array(int) =>
  Js.Opt.case(
    el##getAttribute(Js.string(row_ends_attr_name)),
    () => [||],
    s =>
      switch (List.find_opt(((c, a, _)) => c === el && a === s, parsed^)) {
      | Some((_, _, ends)) => ends
      | None =>
        let ends = parse_row_ends(s);
        parsed :=
          [
            (el, s, ends),
            ...List.filter(((c, _, _)) => c !== el, parsed^),
          ];
        ends;
      },
  );

type item = {
  row: int,
  origin_col: int,
  el: Js.t(Dom_html.element),
  w_px: float,
  h_px: float,
};

/* each display's rendered border-box size, read once it first shows and
   again when AfterLayout's observer reports it changed */
let sizes: ref(list((Js.t(Dom_html.element), (float, float)))) = ref([]);

let read_size = (el: Js.t(Dom_html.element)): (float, float) => {
  let rect = el##getBoundingClientRect;
  (
    Js.Optdef.get(rect##.width, () => 0.),
    Js.Optdef.get(rect##.height, () => 0.),
  );
};

/* the container's displays, and those whose size isn't known yet */
let items_of_container =
    (container: Js.t(Dom_html.element))
    : (list(item), list(Js.t(Dom_html.element))) => {
  let nodes = Util.JsUtil.by_class(container, "offside-wrapper");
  let items = ref([]);
  let unknown = ref([]);
  for (i in 0 to nodes##.length - 1) {
    switch (Js.Opt.to_option(nodes##item(i))) {
    | Some(el) =>
      let attr = (a: string): option(int) =>
        Js.Opt.case(
          el##getAttribute(Js.string(a)),
          () => None,
          s => int_of_string_opt(Js.to_string(s)),
        );
      switch (
        attr("data-row"),
        attr("data-ocol"),
        List.assq_opt(el, sizes^),
      ) {
      | (Some(row), Some(origin_col), Some((w_px, h_px))) =>
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
          ]
      | (Some(_), Some(_), None) => unknown := [el, ...unknown^]
      | _ => ()
      };
    | None => ()
    };
  };
  (
    List.sort(
      (a: item, b: item) =>
        a.row == b.row
          ? compare(a.origin_col, b.origin_col) : compare(a.row, b.row),
      items^,
    ),
    unknown^,
  );
};

/* each display's place, as a left in px */
let solve =
    (~font_metrics: FontMetrics.t, container, items: list(item))
    : list((Js.t(Dom_html.element), string)) => {
  let placed = ref([]);
  if (items != []) {
    let row_ends = row_ends_of_container(container);
    let row_end = (r: int): int =>
      r >= 0 && r < Array.length(row_ends) ? row_ends[r] : 0;
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
        placed := [(it.el, Printf.sprintf("%.1fpx", left_px)), ...placed^];
        occupied := [(it.row, last_row, x^, x^ + w_cols), ...occupied^];
      },
      items,
    );
  };
  placed^;
};

let place = ((el: Js.t(Dom_html.element), left: string)): unit => {
  let left = Js.string(left);
  if (el##.style##.left !== left) {
    el##.style##.left := left;
  };
};

let metrics = ref(FontMetrics.init);

/* places every container's displays, in document order (an editor nested
   in a drawer comes after, and has the last word on its own); returns the
   displays whose size isn't known yet, whose containers it skips */
let place_all = (): list(Js.t(Dom_html.element)) => {
  let containers =
    Dom_html.document##getElementsByClassName(Js.string("code-container"));
  let unknown = ref([]);
  for (i in 0 to containers##.length - 1) {
    Js.Opt.iter(containers##item(i), c =>
      switch (items_of_container(c)) {
      | (items, []) =>
        List.iter(place, solve(~font_metrics=metrics^, c, items))
      | (_, els) => unknown := els @ unknown^
      }
    );
  };
  unknown^;
};

/* reads sizes, then places again */
let measure = (els: list(Js.t(Dom_html.element))): Util.AfterLayout.write => {
  List.iter(
    el =>
      if (Util.JsUtil.connected(el)) {
        sizes := [(el, read_size(el)), ...List.remove_assq(el, sizes^)];
      },
    els,
  );
  () => ignore(place_all());
};

let watcher = lazy(Util.AfterLayout.watcher(measure));

let update = (~font_metrics: FontMetrics.t): unit => {
  metrics := font_metrics;
  sizes :=
    List.filter(
      ((el, _)) =>
        Util.JsUtil.connected(el)
        || {
          Util.AfterLayout.unwatch(el);
          false;
        },
      sizes^,
    );
  parsed := List.filter(((c, _, _)) => Util.JsUtil.connected(c), parsed^);
  /* new displays: watched for size changes from now, and measured once
     the browser has laid them out */
  switch (place_all()) {
  | [] => ()
  | els =>
    List.iter(Util.AfterLayout.watch(Lazy.force(watcher)), els);
    Util.AfterLayout.schedule(() => measure(els));
  };
};
