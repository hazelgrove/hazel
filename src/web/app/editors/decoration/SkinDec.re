open Js_of_ocaml;
open Virtual_dom.Vdom;
open Haz3lcore;

/* Ornament from the page's optional skin script (www/skin/skin.js). With no
   skin active this draws nothing, so the default look is unchanged. */

let active = (): bool =>
  Js.Unsafe.js_expr(
    "!!(typeof window !== 'undefined' && window.HazelSkin && window.HazelSkin.on)",
  )
  |> Js.to_bool;

/* An indented block: a maximal run of rows indented deeper than the row
   that opens it. Its children are the rows at the block's own indent, so a
   tuple's fields or a sum's variants are siblings, while a single statement
   that wraps has one child. */
type block = {
  first: int,
  last: int,
  col: int,
  depth: int,
  kids: list(int),
};

type open_block = {
  opener: int,
  first_row: int,
  col: ref(int),
  last_row: ref(int),
  children: ref(list(int)),
};

let blocks_of_rows = (rows: Measured.Rows.t): list(block) => {
  /* Rows is a Patricia tree, so its bindings are not in row order */
  let lines =
    Measured.Rows.bindings(rows)
    |> List.filter_map(((r, shape: Measured.Rows.shape)) =>
         shape.content_end > shape.content_start
           ? Some((r, shape.content_start, shape.content_end)) : None
       )
    |> List.sort(((a, _, _), (b, _, _)) => Int.compare(a, b));
  let stack = ref([]);
  let out = ref([]);
  let close = () =>
    switch (stack^) {
    | [b, ...rest] =>
      out :=
        [
          {
            first: b.first_row,
            last: b.last_row^,
            col: b.col^,
            depth: List.length(rest),
            kids: List.rev(b.children^),
          },
          ...out^,
        ];
      stack := rest;
    | [] => ()
    };
  let prev = ref(None);
  List.iter(
    ((r, indent, end_col)) => {
      /* a row no deeper than a block's opener ends that block */
      while (List.exists(b => indent <= b.opener, stack^)) {
        close();
      };
      switch (prev^) {
      | Some(p) when indent > p =>
        stack :=
          [
            {
              opener: p,
              first_row: r,
              col: ref(indent),
              last_row: ref(r),
              children: ref([]),
            },
            ...stack^,
          ]
      | _ => ()
      };
      List.iter(b => b.last_row := r, stack^);
      /* a row of one or two characters ("]," or ")") closes a child
         rather than being one */
      switch (stack^) {
      | [b, ..._] when indent <= b.col^ =>
        b.col := indent;
        if (end_col - indent > 2) {
          b.children := [r, ...b.children^];
        };
      | _ => ()
      };
      prev := Some(indent);
    },
    lines,
  );
  while (stack^ != []) {
    close();
  };
  out^;
};

let svg_node = (svg: Js.t(Js.js_string)): Node.t =>
  Node.create_svg(
    "svg",
    ~attrs=[
      Attr.classes(["orn-brace"]),
      Attr.string_property("innerHTML", Js.to_string(svg)),
    ],
    [],
  );

/* The skin grows each ornament from the block's geometry, in pixels
   relative to the code origin, and caches it by that geometry. */
let brace = (~font_metrics: FontMetrics.t, b: block): Node.t => {
  let row_y = r => Float.of_int(r) *. font_metrics.row_height;
  let kids =
    b.kids
    |> List.map(r => row_y(r) +. font_metrics.row_height /. 2.)
    |> Array.of_list
    |> Js.array;
  svg_node(
    Js.Unsafe.fun_call(
      Js.Unsafe.js_expr("window.HazelSkin.braceSVG"),
      [|
        Js.Unsafe.inject(Float.of_int(b.col) *. font_metrics.col_width),
        Js.Unsafe.inject(row_y(b.first)),
        Js.Unsafe.inject(row_y(b.last + 1)),
        Js.Unsafe.inject(font_metrics.row_height),
        Js.Unsafe.inject(font_metrics.col_width),
        Js.Unsafe.inject(kids),
        Js.Unsafe.inject(b.depth),
      |],
    ),
  );
};

/* One ornament beside every indented block of two or more rows. */
let blocks = (~font_metrics: FontMetrics.t, rows: Measured.Rows.t): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["code-deco", "orn-blocks"])],
    active()
      ? blocks_of_rows(rows)
        |> List.filter(b => b.last > b.first)
        |> List.map(brace(~font_metrics))
      : [],
  );
