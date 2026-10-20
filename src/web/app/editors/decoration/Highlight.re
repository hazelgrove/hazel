open Util;
open Util.WebUtil;
open Haz3lcore;

let shard_svg =
    (
      ~start_shape: ShardDec.tip,
      measurement: Measured.measurement,
      p: Piece.t,
    )
    : (Measured.measurement, (ShardDec.tip, ShardDec.tip)) => (
  measurement,
  switch (p) {
  | Tile(t) => t |> Tile.shapes |> ShardDec.tips_of_shapes
  | Grout(g) => g |> Grout.shapes |> ShardDec.tips_of_shapes
  | Secondary(_) => (
      Option.map(
        (s: Nib.Shape.t) =>
          switch (s) {
          | Concave(_) => Nib.Shape.Convex
          | Convex => Nib.Shape.Concave(0)
          },
        start_shape,
      ),
      None,
    )
  | Projector(p) => p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes
  },
);

let multiline_shard =
    (
      num_lb: int,
      {origin, last}: Measured.measurement,
      tips: (option(Nib.Shape.t), option(Nib.Shape.t)),
    ) =>
  List.init(num_lb + 1, i =>
    [
      Some((
        Measured.{
          origin: {
            row: origin.row + i,
            col: origin.col,
          },
          last: {
            row: origin.row + i,
            col: last.col,
          },
        },
        (i == 0 ? fst(tips) : None, i == num_lb ? snd(tips) : None),
      )),
      None,
    ]
  )
  |> List.concat;

/* Traverse a segment computing per-row measurement and tip data.
 * We divide/partition the shards into linebreak-separated segments,
 * then combine the measurements and shapes of the first and last
 * shard of each segment. Ideally we could just get this info from
 * the row measurements, but we have no current way of figuring out
 * shapes for whitespace without traversing */
/* ~trim_ws: skip leading whitespace on continuation rows (rows after a
 * linebreak), so selection decorations hug code instead of painting
 * indentation. Whitespace only — comments are Secondary too and stay.
 * Whitespace-only rows keep their full extent (else the selection
 * reads as discontinuous), and the first row is never trimmed: its
 * left edge is the selection start, which is deliberate. */
let rows_of_segment =
    (
      ~trim_ws: bool=false,
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~shape_init: ShardDec.tip,
      segment: Segment.t,
    )
    : list((Measured.measurement, (ShardDec.tip, ShardDec.tip))) => {
  let find_g = Measured.find_g(~msg="Highlight.of_piece", _, measured);
  let find_w = Measured.find_w(~msg="Highlight.of_piece", _, measured);
  let rec of_piece =
          (start_shape: ShardDec.tip, p: Piece.t)
          : (
              ShardDec.tip,
              list(
                option(
                  (
                    (Measured.measurement, (ShardDec.tip, ShardDec.tip)),
                    bool,
                  ),
                ),
              ),
            ) => {
    let tag = is_ws => Option.map(x => (x, is_ws));
    let shard_data =
      switch (p) {
      | Tile(t) => of_tile(~start_shape, t)
      | Projector(p) => of_projector(~start_shape, p)
      | Grout(g) => [
          Some(shard_svg(~start_shape, find_g(g), p)) |> tag(false),
        ]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [
          Some((
            find_w(w),
            (start_shape |> Option.map(Nib.Shape.flip), start_shape),
          ))
          |> tag(
               switch (w.content) {
               | Whitespace(_) => true
               | _ => false
               },
             ),
        ]
      };
    let next_start_shape =
      switch (Piece.nibs(p)) {
      | None => start_shape
      | Some((_, {shape, _})) => Some(shape)
      };
    (next_start_shape, shard_data);
  }
  and of_tile = (~start_shape, t: Tile.t): list(option(_)) => {
    let shards = Measured.find_shards(~msg="sel_of_tile", t, measured);
    let tile_shards =
      shards
      |> List.filter_map(((i, m)) =>
           List.mem(i, t.shards) ? Some((i, m)) : None
         )
      |> List.map(((index, m)) => {
           let token = Tile.token(t, index);
           let shard = Tile.shard_of(t, index);
           switch (StringUtil.num_linebreaks(token)) {
           | 0 => [
               Some(shard_svg(~start_shape, m, Tile(shard)))
               |> Option.map(x => (x, false)),
             ]
           | num_lb =>
             multiline_shard(num_lb, m, (Some(Convex), Some(Convex)))
             |> List.map(Option.map(x => (x, false)))
           };
         });
    let shape_at = index =>
      Some(snd(Mold.nibs(~index, Tile.mold(t))).shape);
    let children_shards =
      t.children |> List.mapi(index => of_segment(shape_at(index)));
    if (List.length(tile_shards) != List.length(children_shards) + 1) {
      failwith(
        "Highlight.of_tile: shard mismatch:"
        ++ "tile: "
        ++ Tile.show(t)
        ++ "tile_Shards:"
        ++ string_of_int(List.length(tile_shards))
        ++ ", children_Shards:"
        ++ string_of_int(List.length(children_shards)),
      );
    };
    ListUtil.interleave(tile_shards, children_shards) |> List.flatten;
  }
  and of_projector = (~start_shape, p: Base.projector): list(option(_)) =>
    switch (Measured.find_pr_opt(p, measured)) {
    | None => failwith("Highlight.of_projector: missing measurement")
    | Some(_m) =>
      let shape = ProjectorCore.Shape.Map.lookup(p.id, shape_map);
      /* Handling this internal to ProjectorsView at the moment because the
       * commented-out strategy doesn't work well, since the inserted str8-
       * edged lines vertical edge placement doesn't account for whether
       * the initial/final rows begin/end as concave/convex, and hence are
       * of slightly different lengths than is desirable */
      let num_lb =
        switch (shape.vertical) {
        | Inline => 0
        | Tab(num_lbs) => num_lbs
        | Block(num_lbs) => num_lbs
        };
      if (num_lb == 0) {
        [
          Some((
            shard_svg(
              ~start_shape,
              Measured.find_pr(p, measured),
              Projector(p),
            ),
            false,
          )),
        ];
      } else {
        List.init(num_lb + 1, _ => None);
      };
    }
  and of_segment =
      (start_shape: ShardDec.tip, seg: Segment.t): list(option(_)) =>
    seg |> List.fold_left_map(of_piece, start_shape) |> snd |> List.flatten;
  let trim_row = row => {
    let rec drop = xs =>
      switch (xs) {
      | [(_, true), ...rest] => drop(rest)
      | _ => xs
      };
    switch (drop(row)) {
    | [] => row
    | trimmed => trimmed
    };
  };
  of_segment(shape_init, segment)
  |> ListUtil.split_at_nones
  |> List.mapi((i, row) => trim_ws && i > 0 ? trim_row(row) : row)
  |> List.map(List.map(fst))
  |> ListUtil.first_and_last
  |> List.map((((m1, (l1, _)), (m2, (_, r2)))) =>
       (
         Measured.{
           origin: m1.origin,
           last: m2.last,
         },
         (l1, r2),
       )
     );
};

/* --- Unified outline path construction ---
 *
 * One path around the exterior of a connected group of rows, so a stroke
 * has no interior edges, keeping the chevron nib shapes on every row's left
 * and right edges. Built in pixels: each row's edges come from the grid, so
 * a row's extent includes any gadgets inside it, and the rows of a group
 * meet halfway through the leading between them. */

type row_data = {
  row_num: int,
  left_col: int,
  right_col: int,
  left_tip: option(Direction.t),
  right_tip: option(Direction.t),
};

let row_data_of =
    (measurement: Measured.measurement, tips: (ShardDec.tip, ShardDec.tip))
    : row_data => {
  let (l, r) = tips;
  {
    row_num: measurement.origin.row,
    left_col: measurement.origin.col,
    right_col: measurement.last.col,
    left_tip: Option.map(Nib.Shape.direction_of(Left), l),
    right_tip: Option.map(Nib.Shape.direction_of(Right), r),
  };
};

/* A row's pixel box: its edges with the tips' sub-cell adjustment, its top
   and bottom meeting the group's neighboring rows halfway. */
type row_box = {
  row: row_data,
  left: float,
  right: float,
  top: float,
  bottom: float,
};

let boxes_of = (~grid: Grid.t, rows: list(row_data)): list(row_box) => {
  let {col_width, _}: FontMetrics.t = Grid.font_metrics(grid);
  let mid = (above: row_data, below: row_data) =>
    (Grid.y_bottom(grid, above.row_num) +. Grid.y_top(grid, below.row_num))
    /. 2.;
  let rec go = (above: option(row_data), rows: list(row_data)) =>
    switch (rows) {
    | [] => []
    | [row, ...rest] =>
      let r = row.row_num;
      let box = {
        row,
        left:
          Grid.x_start(grid, ~row=r, row.left_col)
          -. ShardDec.shape_adjust(Left, row.left_tip)
          *. col_width,
        right:
          Grid.x_end(grid, ~row=r, row.right_col)
          -. ShardDec.shape_adjust(Right, row.right_tip)
          *. col_width,
        top:
          switch (above) {
          | None => Grid.y_top(grid, r)
          | Some(a) => mid(a, row)
          },
        bottom:
          switch (ListUtil.hd_opt(rest)) {
          | None => Grid.y_bottom(grid, r)
          | Some(b) => mid(row, b)
          },
      };
      [box, ...go(Some(row), rest)];
    };
  go(None, rows);
};

/* Group rows into connected components (consecutive row_num values) */
let group_consecutive = (rows: list(row_data)): list(list(row_data)) => {
  let rec go =
          (acc_group: list(row_data), acc_groups, remaining)
          : list(list(row_data)) =>
    switch (remaining) {
    | [] =>
      switch (acc_group) {
      | [] => List.rev(acc_groups)
      | _ => List.rev([List.rev(acc_group), ...acc_groups])
      }
    | [row, ...rest] =>
      switch (acc_group) {
      | [] => go([row], acc_groups, rest)
      | [prev, ..._] =>
        if (row.row_num == prev.row_num + 1) {
          go([row, ...acc_group], acc_groups, rest);
        } else {
          go([row], [List.rev(acc_group), ...acc_groups], rest);
        }
      }
    };
  go([], [], rows);
};

/* One side of the outline: each box's edge, then the step to the next. */
let rec side =
        (edge: row_box => list(SvgUtil.Path.cmd), x: row_box => float, boxes)
        : list(SvgUtil.Path.cmd) =>
  switch (boxes) {
  | [] => []
  | [box] => edge(box)
  | [box, next, ...rest] =>
    edge(box)
    @ (x(box) == x(next) ? [] : [SvgUtil.Path.H({x: x(next)})])
    @ side(edge, x, [next, ...rest])
  };

/* A single closed path around a connected group of rows. Traces clockwise:
 * top edge, right side down (chevrons + steps), bottom edge, left side up
 * (chevrons + steps). */
let outline_path =
    (~grid: Grid.t, boxes: list(row_box)): list(SvgUtil.Path.cmd) =>
  switch (boxes) {
  | [] => []
  | [first, ..._] =>
    let {col_width, _}: FontMetrics.t = Grid.font_metrics(grid);
    let last = ListUtil.last(boxes);
    let chevron = (box: row_box, tip, from) =>
      ShardDec.chevron(tip, from)
      |> SvgUtil.Path.scale_x(col_width)
      |> SvgUtil.Path.scale_y(box.bottom -. box.top);
    SvgUtil.Path.[
      M({
        x: first.left,
        y: first.top,
      }),
      H({x: first.right}),
    ]
    @ side(
        box => chevron(box, box.row.right_tip, Right),
        b => b.right,
        boxes,
      )
    @ [SvgUtil.Path.H({x: last.left})]
    @ side(
        box => chevron(box, box.row.left_tip, Left),
        b => b.left,
        List.rev(boxes),
      )
    @ [SvgUtil.Path.Z];
  };

/* Fraction of a group's width covered by the active-eval sweep bar. */
let sweep_width_ratio = 0.45;

let svg_of_group =
    (
      ~grid: Grid.t,
      ~clss: list(string),
      ~sweep: bool=false,
      rows: list(row_data),
    )
    : option(Node.t) =>
  switch (rows) {
  | [] => None
  | _ =>
    let path = outline_path(~grid, boxes_of(~grid, rows));
    let active_sweep = (box: SvgUtil.Rect.t) =>
      if (sweep) {
        /* Clip-path ids must be document-unique; derive one from the
         * group's box, to tenths of a pixel. */
        let clip_id =
          Printf.sprintf(
            "incremental-active-%d-%d-%d-%d",
            int_of_float(box.min.x *. 10.0),
            int_of_float(box.min.y *. 10.0),
            int_of_float(box.width *. 10.0),
            int_of_float(box.height *. 10.0),
          );
        let sweep_width =
          max(
            Grid.font_metrics(grid).col_width,
            box.width *. sweep_width_ratio,
          );
        [
          Node.create_svg(
            "defs",
            [
              Node.create_svg(
                "clipPath",
                ~attrs=[Attr.create("id", clip_id)],
                [SvgUtil.Path.view(~attrs=[], path)],
              ),
            ],
          ),
          Node.create_svg(
            "g",
            ~attrs=[Attr.create("clip-path", "url(#" ++ clip_id ++ ")")],
            [
              Node.create_svg(
                "rect",
                ~attrs=[
                  Attr.classes(["incremental-sweep"]),
                  Attr.create("x", Printf.sprintf("%f", box.min.x)),
                  Attr.create("y", Printf.sprintf("%f", box.min.y)),
                  Attr.create("width", Printf.sprintf("%f", sweep_width)),
                  Attr.create("height", Printf.sprintf("%f", box.height)),
                ],
                [],
              ),
            ],
          ),
        ];
      } else {
        [];
      };
    Some(
      DecUtil.svg_px(~base_cls=["shard"] @ clss, ~extra=active_sweep, path),
    );
  };

/* Clip partial-token boundaries for char-level selections.
 * Adjusts the first/last row's left/right columns when the
 * selection boundary falls mid-token (Inner caret). */
let clip_char_selection =
    (~measured: Measured.t, z: Zipper.t, rows: list(row_data))
    : list(row_data) => {
  let content = z.selection.content;
  switch (content, rows) {
  | ([], _)
  | (_, []) => rows
  | _ =>
    /* Determine left/right inner offsets based on focus direction.
     * Content is always left-to-right spatially.
     * focus=Right: anchor at left, focus at right.
     * focus=Left: focus at left, anchor at right.
     *
     * When smart_rounded is set, the anchor end displays at the outer
     * boundary of its piece (even if anchor_caret is Inner) — the
     * selection has been rounded up beyond the starting token. */
    let anchor_inner: option(int) =
      z.selection.smart_rounded
        ? None
        : (
          switch (z.selection.anchor_caret) {
          | CaretBase.Inner(n) => Some(n)
          | CaretBase.Outer => None
          }
        );
    let focus_inner: option(int) =
      switch (z.caret) {
      | Inner(n) => Some(n)
      | Outer => None
      };
    let (left_inner, right_inner) =
      switch (z.selection.focus) {
      | Right => (anchor_inner, focus_inner)
      | Left => (focus_inner, anchor_inner)
      };

    /* Clip left boundary of first row */
    let rows =
      switch (left_inner) {
      | None => rows
      | Some(n) =>
        let left_piece = List.hd(content);
        let shard = List.hd(Piece.disassemble(left_piece));
        switch (Piece.token_of(shard)) {
        | Some(tok) =>
          let offset = Zipper.Caret.inner_offset_for_token(n, tok);
          switch (rows) {
          | [] => []
          | [first, ...rest] => [
              {
                ...first,
                left_col: first.left_col + offset,
                left_tip: None,
              },
              ...rest,
            ]
          };
        | None => rows
        };
      };

    /* Clip right boundary of last row */
    let rows =
      switch (right_inner) {
      | None => rows
      | Some(n) =>
        let right_piece = ListUtil.last(content);
        let last_shard = ListUtil.last(Piece.disassemble(right_piece));
        switch (Piece.token_of(last_shard)) {
        | Some(tok) =>
          let offset = Zipper.Caret.inner_offset_for_token(n, tok);
          let m =
            Measured.find_p(~msg="clip_char_sel_right", last_shard, measured);
          let new_right_col = m.origin.col + offset;
          switch (ListUtil.split_last_opt(rows)) {
          | None => []
          | Some((init, last_row)) =>
            init
            @ [
              {
                ...last_row,
                right_col: new_right_col,
                right_tip: None,
              },
            ]
          };
        | None => rows
        };
      };

    rows;
  };
};

/* --- Public API --- */

let of_segment =
    (
      ~trim_ws: bool=false,
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~grid: Grid.t,
      ~shape_init: ShardDec.tip,
      ~clss: list(string),
      ~sweep: bool=false,
      segment: Segment.t,
    )
    : list(Node.t) => {
  let rows =
    rows_of_segment(~trim_ws, ~measured, ~shape_map, ~shape_init, segment)
    |> List.map(((m, tips)) => row_data_of(m, tips));
  let groups = group_consecutive(rows);
  List.filter_map(svg_of_group(~grid, ~clss, ~sweep), groups);
};

let selection =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~grid: Grid.t,
      z: Zipper.t,
    ) => {
  let rows =
    rows_of_segment(
      ~trim_ws=true,
      ~measured,
      ~shape_map,
      ~shape_init=Some(fst(Siblings.shapes(z.relatives.siblings))),
      z.selection.content,
    )
    |> List.map(((m, tips)) => row_data_of(m, tips));
  /* Clip partial-token boundaries for char-level selections */
  let rows = clip_char_selection(~measured, z, rows);
  let clss = ["selected", Selection.buffer_cls(z.selection)];
  let groups = group_consecutive(rows);
  div_c("selects", List.filter_map(svg_of_group(~grid, ~clss), groups));
};

// Expands selection to make it a subtree of the exp
let selection_expanded =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~grid: Grid.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    ) =>
  div_c(
    "selects",
    switch (
      TermData.get_root_id_using_ranges(
        z.selection.content,
        term_data,
        measured,
      )
    ) {
    | None => []
    | Some(id) =>
      let seg = TermData.segment(id, term_data);
      switch (seg) {
      | None => []
      | Some(seg) =>
        of_segment(
          ~measured,
          ~shape_map,
          ~grid,
          ~shape_init=Some(fst(Siblings.shapes(z.relatives.siblings))),
          ~clss=["selected-expanded", Selection.buffer_cls(z.selection)],
          seg,
        )
        @ of_segment(
            ~trim_ws=true,
            ~measured,
            ~shape_map,
            ~grid,
            ~shape_init=Some(fst(Siblings.shapes(z.relatives.siblings))),
            ~clss=["selected", Selection.buffer_cls(z.selection)],
            z.selection.content,
          )
      };
    },
  );

let indicated_refractor =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~grid: Grid.t,
      ~kind: ProjectorCore.Kind.t,
      seg: Segment.t,
    ) => {
  let kind_cls = ProjectorCore.Kind.name(kind);
  div_c(
    "refractor-backing",
    of_segment(
      ~measured,
      ~shape_map,
      ~grid,
      ~shape_init=Some(Convex),
      ~clss=["refractor-indicated", kind_cls],
      seg,
    ),
  );
};

let color =
    (
      ~syntax: CachedSyntax.t,
      ~grid: Grid.t,
      ~sweep: bool=false,
      clss: list(string),
      id: Id.t,
    ) =>
  switch (TermData.segment(id, syntax.term_data)) {
  | Some(segment) =>
    of_segment(
      ~measured=syntax.measured,
      ~shape_map=syntax.shape_map,
      ~grid,
      ~shape_init=Some(Convex),
      ~clss,
      ~sweep,
      segment,
    )
  | None => []
  };

let colors =
    (
      ~grid: Grid.t,
      ~syntax: CachedSyntax.t,
      color_highlights: option(ColorSteps.colorMap),
    ) =>
  div_c(
    "color-highlights",
    List.concat_map(
      ((id, c)) => color(~syntax, ~grid, ["highlight-code-" ++ c], id),
      switch (color_highlights) {
      | Some(colorMap) => ColorSteps.to_list(colorMap)
      | _ => []
      },
    ),
  );

/* `predicted_reuse` is the ReusePass plan (not the accumulating cache). */
let incr_eval =
    (
      ~grid: Grid.t,
      ~syntax: CachedSyntax.t,
      ~pending_eval_ids: list(Id.t)=[],
      ~show_active_eval: bool=false,
      ~show_frozen: bool=true,
      predicted_reuse: Language.EvaluatorState.incr_eval,
    ) => {
  let range_eq = ((o1, l1), (o2, l2)) =>
    Point.equals(o1, o2) && Point.equals(l1, l2);
  let range_contains = ((o1, l1), (o2, l2)) =>
    Point.compare(o1, o2) <= 0 && Point.compare(l2, l1) <= 0;
  let ranged_ids_of = ids =>
    ids
    |> List.sort_uniq(Id.compare)
    |> List.filter_map(id =>
         switch (
           TermData.extreme_measures(id, syntax.term_data, syntax.measured)
         ) {
         | Some(range) => Some((id, range))
         | None => None
         }
       );
  let range_compare = ((_, (o1, l1)), (_, (o2, l2))) =>
    switch (Point.compare(o1, o2)) {
    | 0 => Point.compare(l1, l2)
    | cmp => cmp
    };
  let outermost = ranged_ids =>
    List.fold_left(
      (acc, (id, r)) =>
        if (List.exists(
              ((_, r2)) => range_contains(r2, r) && !range_eq(r2, r),
              ranged_ids,
            )
            || List.exists(((_, r2)) => range_eq(r2, r), acc)) {
          acc;
        } else {
          [(id, r), ...acc];
        },
      [],
      ranged_ids,
    );
  let frozen_ids =
    show_frozen ? Language.IncrEval.frozen_ids(~incr=predicted_reuse) : [];
  let pending_eval_ranges =
    pending_eval_ids |> ranged_ids_of |> List.sort(range_compare);
  let active_ids =
    if (show_active_eval) {
      pending_eval_ranges |> ListUtil.hd_opt |> Option.to_list;
    } else {
      [];
    };
  let pending_inactive_ranges =
    pending_eval_ranges
    |> List.filter(((_, range)) =>
         !
           List.exists(
             ((_, active_range)) => range_eq(active_range, range),
             active_ids,
           )
       );
  let frozen_outermost = frozen_ids |> ranged_ids_of |> outermost;
  div_c(
    "incremental-highlights",
    List.concat_map(
      ((id, _)) => color(~syntax, ~grid, ["incremental-frozen"], id),
      frozen_outermost,
    )
    @ List.concat_map(
        ((id, _)) => color(~syntax, ~grid, ["incremental-pending"], id),
        pending_inactive_ranges,
      )
    @ List.concat_map(
        ((id, _)) =>
          color(
            ~syntax,
            ~grid,
            ~sweep=true,
            ["incremental-pending", "incremental-active"],
            id,
          ),
        active_ids,
      ),
  );
};

/* Blocks (settings: Blocks): a box per block per row, one shape per run of
   rows (SvgUtil.Rows). */
let blocks =
    (blocks: list((Language.Blocks.paint, list(SvgUtil.Rows.box)))): Node.t =>
  div_c(
    "blocks",
    List.concat_map(
      ((paint, boxes)) => {
        let base_cls = ["shard", "block", Language.Blocks.paint_name(paint)];
        boxes
        |> SvgUtil.Rows.split
        |> List.map(group =>
             DecUtil.svg_px(~base_cls, SvgUtil.Rows.outline(group))
           );
      },
      blocks,
    ),
  );

/* Inset layout: nesting shown by insetting each block a little more than
   its parent, up to `inset_levels` deep (deeper blocks share edges). A row
   narrower than twice its inset collapses to its midline rather than
   crossing itself. */
let inset_per_level = 1.5; /* px */
let inset_levels = 3;

let inset_boxes =
    (~grid: Grid.t, ~syntax: CachedSyntax.t, blocks: list(Language.Blocks.t))
    : list((Language.Blocks.paint, list(SvgUtil.Rows.box))) =>
  List.filter_map(
    ({id, level, paint}: Language.Blocks.t) => {
      let inset = inset_per_level *. float_of_int(min(level, inset_levels));
      TermData.row_extents(id, syntax.term_data, syntax.measured)
      |> Option.map(
           List.map(({row, start_col, end_col}: TermData.row_extent) => {
             let left = Grid.x_start(grid, ~row, start_col);
             let right = Grid.x_end(grid, ~row, end_col);
             let mid = (left +. right) /. 2.;
             SvgUtil.Rows.{
               row,
               top: Grid.y_top(grid, row) +. inset,
               bottom: Grid.y_bottom(grid, row) -. inset,
               left: min(left +. inset, mid),
               right: max(right -. inset, mid),
             };
           }),
         )
      |> Option.map(boxes => (paint, boxes));
    },
    blocks,
  );

/* Padded layout: each box stands as many pads out from its text as block
   edges stack there (Padding). */
let padded_boxes =
    (~grid: Grid.t, padding: Padding.t)
    : list((Language.Blocks.paint, list(SvgUtil.Rows.box))) => {
  let out = (rank: int) => float_of_int(rank) *. Grid.pad(grid);
  List.map(
    ({block: {paint, _}, rows}: Padding.block) =>
      (
        paint,
        List.map(
          ({row, left, right, rank}: Padding.block_row) =>
            SvgUtil.Rows.{
              row,
              top: Grid.text_top(grid, row) -. out(rank),
              bottom: Grid.text_bottom(grid, row) +. out(rank),
              left: Grid.x_start(grid, ~row, left.col) -. out(left.rank),
              right: Grid.x_end(grid, ~row, right.col) +. out(right.rank),
            },
          rows,
        ),
      ),
    padding.blocks,
  );
};
