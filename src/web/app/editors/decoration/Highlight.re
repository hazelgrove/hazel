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
let rows_of_segment =
    (
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
                  (Measured.measurement, (ShardDec.tip, ShardDec.tip)),
                ),
              ),
            ) => {
    let shard_data =
      switch (p) {
      | Tile(t) => of_tile(~start_shape, t)
      | Projector(p) => of_projector(~start_shape, p)
      | Grout(g) => [Some(shard_svg(~start_shape, find_g(g), p))]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [
          Some((
            find_w(w),
            (start_shape |> Option.map(Nib.Shape.flip), start_shape),
          )),
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
           let token = List.nth(t.label, index);
           let shard = Tile.shard_of(t, index);
           switch (StringUtil.num_linebreaks(token)) {
           | 0 => [Some(shard_svg(~start_shape, m, Tile(shard)))]
           | num_lb =>
             multiline_shard(num_lb, m, (Some(Convex), Some(Convex)))
           };
         });
    let shape_at = index => Some(snd(Mold.nibs(~index, t.mold)).shape);
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
          Some(
            shard_svg(
              ~start_shape,
              Measured.find_pr(p, measured),
              Projector(p),
            ),
          ),
        ];
      } else {
        List.init(num_lb + 1, _ => None);
      };
    }
  and of_segment =
      (start_shape: ShardDec.tip, seg: Segment.t): list(option(_)) =>
    seg |> List.fold_left_map(of_piece, start_shape) |> snd |> List.flatten;
  of_segment(shape_init, segment)
  |> ListUtil.split_at_nones
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
 * Instead of drawing one SVG per row (which gives per-row outlines when
 * stroked), we trace a single path around the exterior of all connected
 * rows, eliminating shared interior edges. The path preserves chevron
 * nib shapes on every row's left and right edges. */

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

let left_x = (row: row_data): float =>
  float_of_int(row.left_col) -. ShardDec.shape_adjust(Left, row.left_tip);

let right_x = (row: row_data): float =>
  float_of_int(row.right_col) -. ShardDec.shape_adjust(Right, row.right_tip);

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

/* Build a single closed path around a connected group of rows.
 * Traces clockwise: top edge, right side down (chevrons + steps),
 * bottom edge, left side up (chevrons + steps). */
let outline_path =
    (~origin_col: float, ~origin_row: int, rows: list(row_data))
    : list(SvgUtil.Path.cmd) => {
  switch (rows) {
  | [] => []
  | [first, ..._] =>
    let last = ListUtil.last(rows);
    let n = List.length(rows);

    let lx = (row: row_data) => left_x(row) -. origin_col;
    let rx = (row: row_data) => right_x(row) -. origin_col;
    let ty = (row: row_data) => float_of_int(row.row_num - origin_row);

    let top =
      SvgUtil.Path.[
        M({
          x: lx(first),
          y: ty(first),
        }),
        H({x: rx(first)}),
      ];

    let right_side =
      rows
      |> List.mapi((i, row) => {
           let chevron = ShardDec.chevron(row.right_tip, Right);
           let step =
             if (i < n - 1) {
               let next = List.nth(rows, i + 1);
               let rx_cur = rx(row);
               let rx_next = rx(next);
               if (rx_cur == rx_next) {
                 [];
               } else {
                 [SvgUtil.Path.H({x: rx_next})];
               };
             } else {
               [];
             };
           chevron @ step;
         })
      |> List.flatten;

    let bottom = [SvgUtil.Path.H({x: lx(last)})];

    let rows_rev = List.rev(rows);
    let left_side =
      rows_rev
      |> List.mapi((i, row) => {
           let chevron = ShardDec.chevron(row.left_tip, Left);
           let step =
             if (i < n - 1) {
               let next_up = List.nth(rows_rev, i + 1);
               let lx_cur = lx(row);
               let lx_next = lx(next_up);
               if (lx_cur == lx_next) {
                 [];
               } else {
                 [SvgUtil.Path.H({x: lx_next})];
               };
             } else {
               [];
             };
           chevron @ step;
         })
      |> List.flatten;

    top @ right_side @ bottom @ left_side @ [SvgUtil.Path.Z];
  };
};

type bbox = {
  min_col: float,
  max_col: float,
  min_row: int,
  max_row: int,
};

let bbox_of = (rows: list(row_data)): option(bbox) =>
  switch (rows) {
  | [] => None
  | [first, ...rest] =>
    let row_bounds = (row: row_data) => {
      let l_tip_x =
        left_x(row) -. abs_float(ShardDec.caret_run(row.left_tip));
      let r_tip_x =
        right_x(row) +. abs_float(ShardDec.caret_run(row.right_tip));
      (min(left_x(row), l_tip_x), max(right_x(row), r_tip_x));
    };
    let (l0, r0) = row_bounds(first);
    Some(
      List.fold_left(
        (bb, row) => {
          let (l, r) = row_bounds(row);
          {
            min_col: min(bb.min_col, l),
            max_col: max(bb.max_col, r),
            min_row: min(bb.min_row, row.row_num),
            max_row: max(bb.max_row, row.row_num),
          };
        },
        {
          min_col: l0,
          max_col: r0,
          min_row: first.row_num,
          max_row: first.row_num,
        },
        rest,
      ),
    );
  };

/* Fraction of a group's width covered by the active-eval sweep bar. */
let sweep_width_ratio = 0.45;

let svg_of_bbox =
    (
      ~font_metrics: FontMetrics.t,
      ~clss: list(string),
      ~sweep: bool=false,
      ~path_cmds: list(SvgUtil.Path.cmd),
      bb: bbox,
    )
    : Node.t => {
  let width_f = bb.max_col -. bb.min_col;
  let height = bb.max_row - bb.min_row + 1;
  let height_f = float_of_int(height);

  /* Clip-path ids must be document-unique; derive one from the group's
   * bounding box (cols are fractional, so scale to tenths of a column). */
  let clip_id =
    Printf.sprintf(
      "incremental-active-%d-%d-%d-%d",
      int_of_float(bb.min_col *. 10.0),
      bb.min_row,
      int_of_float(bb.max_col *. 10.0),
      bb.max_row,
    );
  let active_sweep =
    if (sweep) {
      let sweep_width = max(1.0, width_f *. sweep_width_ratio);
      [
        Node.create_svg(
          "defs",
          [
            Node.create_svg(
              "clipPath",
              ~attrs=[Attr.create("id", clip_id)],
              [SvgUtil.Path.view(~attrs=[], path_cmds)],
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
                Attr.create("x", "0"),
                Attr.create("y", "0"),
                Attr.create("width", Printf.sprintf("%f", sweep_width)),
                Attr.create("height", Printf.sprintf("%f", height_f)),
              ],
              [],
            ),
          ],
        ),
      ];
    } else {
      [];
    };

  Node.create_svg(
    "svg",
    ~attrs=[
      Attr.classes(["shard"] @ clss),
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
          bb.min_col *. font_metrics.col_width,
          float_of_int(bb.min_row) *. font_metrics.row_height,
          width_f *. font_metrics.col_width,
          height_f *. font_metrics.row_height,
        ),
      ),
      Attr.create(
        "viewBox",
        Printf.sprintf("%f 0 %f %d", 0.0, width_f, height),
      ),
      Attr.create("preserveAspectRatio", "none"),
    ],
    [SvgUtil.Path.view(~attrs=[], path_cmds)] @ active_sweep,
  );
};

let svg_of_group =
    (
      ~font_metrics: FontMetrics.t,
      ~clss: list(string),
      ~sweep: bool=false,
      rows: list(row_data),
    )
    : option(Node.t) =>
  switch (bbox_of(rows)) {
  | None => None
  | Some(bb) =>
    let path_cmds =
      outline_path(~origin_col=bb.min_col, ~origin_row=bb.min_row, rows);
    Some(svg_of_bbox(~font_metrics, ~clss, ~sweep, ~path_cmds, bb));
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
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
      ~shape_init: ShardDec.tip,
      ~clss: list(string),
      ~sweep: bool=false,
      segment: Segment.t,
    )
    : list(Node.t) => {
  let rows =
    rows_of_segment(~measured, ~shape_map, ~shape_init, segment)
    |> List.map(((m, tips)) => row_data_of(m, tips));
  let groups = group_consecutive(rows);
  List.filter_map(svg_of_group(~font_metrics, ~clss, ~sweep), groups);
};

let selection =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
      z: Zipper.t,
    ) => {
  let rows =
    rows_of_segment(
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
  div_c(
    "selects",
    List.filter_map(svg_of_group(~font_metrics, ~clss), groups),
  );
};

// Expands selection to make it a subtree of the exp
let selection_expanded =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
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
          ~font_metrics,
          ~shape_init=Some(fst(Siblings.shapes(z.relatives.siblings))),
          ~clss=["selected-expanded", Selection.buffer_cls(z.selection)],
          seg,
        )
        @ of_segment(
            ~measured,
            ~shape_map,
            ~font_metrics,
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
      ~font_metrics: FontMetrics.t,
      ~kind: ProjectorCore.Kind.t,
      seg: Segment.t,
    ) => {
  let kind_cls = ProjectorCore.Kind.name(kind);
  div_c(
    "refractor-backing",
    of_segment(
      ~measured,
      ~shape_map,
      ~font_metrics,
      ~shape_init=Some(Convex),
      ~clss=["refractor-indicated", kind_cls],
      seg,
    ),
  );
};

let color =
    (
      ~syntax: CachedSyntax.t,
      ~font_metrics: FontMetrics.t,
      ~sweep: bool=false,
      clss: list(string),
      id: Id.t,
    ) =>
  switch (TermData.segment(id, syntax.term_data)) {
  | Some(segment) =>
    of_segment(
      ~measured=syntax.measured,
      ~shape_map=syntax.shape_map,
      ~font_metrics,
      ~shape_init=Some(Convex),
      ~clss,
      ~sweep,
      segment,
    )
  | None => []
  };

/* per-id RANGE memo for the pending-eval highlight: incr_eval runs on
   every streamed chunk and recomputed extreme_measures for every
   pending leaf id (thousands of measured lookups per frame). Ranges
   only change with the measured/term_data generation, so one table
   serves a whole evaluation. Cleared on generation change. */
module RangeCache = {
  let key: ref(Obj.t) = ref(Obj.repr(0));
  let tbl: Hashtbl.t(Id.t, option((Measured.Point.t, Measured.Point.t))) =
    Hashtbl.create(1024);
  let for_gen =
      (~measured: Measured.t, ~term_data: TermData.t)
      : (Id.t => option((Measured.Point.t, Measured.Point.t))) => {
    /* measured is the stricter generation proxy: term_data only
       changes in mk, where measured is rebuilt too */
    if (!(key^ === Obj.repr(measured))) {
      Hashtbl.reset(tbl);
      key := Obj.repr(measured);
    };
    id =>
      switch (Hashtbl.find_opt(tbl, id)) {
      | Some(r) => r
      | None =>
        let r = TermData.extreme_measures(id, term_data, measured);
        Hashtbl.replace(tbl, id, r);
        r;
      };
  };
};

/* per-id node cache for the pending-eval highlight (see incr_eval).
   Eviction: tick sweep, view-side cache discipline. */
module IncrEvalCache = {
  type entry = {
    mutable e_meas: Obj.t,
    mutable e_range: (Measured.Point.t, Measured.Point.t),
    mutable e_fm: Obj.t,
    mutable e_sweep: bool,
    mutable e_nodes: list(Node.t),
    mutable e_tick: int,
  };
  let cache: Hashtbl.t(Id.t, entry) = Hashtbl.create(64);
  let tick = ref(0);
  let bump = () => {
    incr(tick);
    if (tick^ mod 64 == 0) {
      let dead =
        Hashtbl.fold(
          (id, e, acc) => e.e_tick < tick^ - 16 ? [id, ...acc] : acc,
          cache,
          [],
        );
      List.iter(Hashtbl.remove(cache), dead);
    };
  };
  let get =
      (~id, ~measured, ~range, ~font_metrics, ~sweep: bool, ~mk)
      : list(Node.t) =>
    switch (Hashtbl.find_opt(cache, id)) {
    | Some(e)
        when
          e.e_meas === Obj.repr(measured)
          && e.e_range == range
          && e.e_fm === Obj.repr(font_metrics)
          && e.e_sweep == sweep =>
      e.e_tick = tick^;
      e.e_nodes;
    | _ =>
      let nodes = mk();
      Hashtbl.replace(
        cache,
        id,
        {
          e_meas: Obj.repr(measured),
          e_range: range,
          e_fm: Obj.repr(font_metrics),
          e_sweep: sweep,
          e_nodes: nodes,
          e_tick: tick^,
        },
      );
      nodes;
    };
};

let bbox_of_range =
    (~measured: Measured.t, (origin: Point.t, final: Point.t)): option(bbox) =>
  if (final.row < origin.row) {
    None;
  } else {
    List.init(final.row - origin.row + 1, i => origin.row + i)
    |> List.fold_left(
         (acc, row) =>
           switch (acc, Measured.row_shape(row, measured)) {
           | (None, _)
           | (_, None) => None
           | (Some(bb), Some(shape: Measured.Rows.shape)) =>
             let left =
               float_of_int(row == origin.row ? origin.col : shape.indent);
             let right =
               float_of_int(row == final.row ? final.col : shape.max_col);
             Some({
               ...bb,
               min_col: min(bb.min_col, left),
               max_col: max(bb.max_col, right),
             });
           },
         Some({
           min_col: float_of_int(origin.col),
           max_col: float_of_int(origin.col),
           min_row: origin.row,
           max_row: final.row,
         }),
       );
  };

let color_range =
    (
      ~font_metrics: FontMetrics.t,
      ~measured: Measured.t,
      ~sweep: bool=false,
      clss: list(string),
      range: (Point.t, Point.t),
    )
    : list(Node.t) =>
  switch (bbox_of_range(~measured, range)) {
  | None => []
  | Some(bb) =>
    let width = bb.max_col -. bb.min_col;
    let height = float_of_int(bb.max_row - bb.min_row + 1);
    let path_cmds =
      SvgUtil.Path.[
        M({
          x: 0.0,
          y: 0.0,
        }),
        H({x: width}),
        V({y: height}),
        H({x: 0.0}),
        Z,
      ];
    [svg_of_bbox(~font_metrics, ~clss, ~sweep, ~path_cmds, bb)];
  };

/* One text-hugging svg per contiguous row run of a range: per-row
   extents from the measured row shapes, contoured by outline_path -
   shard-shaped visuals at bounding-box node cost. (Rendering the
   range's SEGMENT through the shard machinery emitted hundreds of
   nodes per item-sized range - 23k+ overlays on a mega program.)
   Blank rows split the contour, so the highlight skips empty lines. */
let contour_of_range =
    (
      ~font_metrics: FontMetrics.t,
      ~measured: Measured.t,
      ~sweep: bool=false,
      clss: list(string),
      (origin: Point.t, final: Point.t),
    )
    : list(Node.t) =>
  if (final.row < origin.row) {
    [];
  } else {
    let rows =
      List.init(final.row - origin.row + 1, i => origin.row + i)
      |> List.filter_map(row =>
           switch (Measured.row_shape(row, measured)) {
           | None => None
           | Some(shape) =>
             /* max/min: window-clamped regions may start/end mid-range
                with cols from a different row */
             let left =
               row == origin.row
                 ? max(origin.col, shape.indent) : shape.indent;
             let right =
               row == final.row
                 ? min(final.col, shape.max_col) : shape.max_col;
             right <= left
               ? None
               : Some({
                   row_num: row,
                   left_col: left,
                   right_col: right,
                   left_tip: None,
                   right_tip: None,
                 });
           }
         );
    group_consecutive(rows)
    |> List.filter_map(svg_of_group(~font_metrics, ~clss, ~sweep));
  };

let colors =
    (
      ~font_metrics: FontMetrics.t,
      ~syntax: CachedSyntax.t,
      color_highlights: option(ColorSteps.colorMap),
    ) =>
  div_c(
    "color-highlights",
    List.concat_map(
      ((id, c)) =>
        color(~syntax, ~font_metrics, ["highlight-code-" ++ c], id),
      switch (color_highlights) {
      | Some(colorMap) => ColorSteps.to_list(colorMap)
      | _ => []
      },
    ),
  );

let incr_eval =
    (
      ~font_metrics: FontMetrics.t,
      ~syntax: CachedSyntax.t,
      ~visible: option(Globals.VisibleRows.t)=?,
      ~pending_eval_ids: list(Id.t)=[],
      ~show_active_eval: bool=false,
      (),
    ) => {
  IncrEvalCache.bump();
  let range_eq = ((o1, l1), (o2, l2)) =>
    Point.equals(o1, o2) && Point.equals(l1, l2);
  let range_of =
    RangeCache.for_gen(
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
    );
  let ranged_ids_of = ids =>
    ids
    |> List.sort_uniq(Id.compare)
    |> List.filter_map(id =>
         switch (range_of(id)) {
         | Some(range) => Some((id, range))
         | None => None
         }
       );
  let range_compare = ((_, (o1, l1)), (_, (o2, l2))) =>
    switch (Point.compare(o1, o2)) {
    | 0 => Point.compare(l1, l2)
    | cmp => cmp
    };
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
  /* None = the scroll handler has not fired yet (or the mode has no
     culling), i.e. the view is at its initial scroll position: cull
     to a generous top window rather than building every range - a
     full mega-program pending set is tens of thousands of SVGs */
  let visible_bounds =
    switch (visible) {
    | None => (0, 300)
    | Some({first, last}) => (first, last)
    };
  let visible_ranges = (ranges: list((Id.t, (Point.t, Point.t)))) => {
    let (first, last) = visible_bounds;
    List.filter(
      ((_, (origin: Point.t, final: Point.t))) =>
        origin.row <= last && final.row >= first,
      ranges,
    );
  };
  /* The pending set shrinks on EVERY streamed chunk, and each of those
     frames used to rebuild every remaining range's SVG (~70 ranges x
     all their rows) - the highlight itself cost multiples of the
     actual statics work per edit. Cache nodes per id: within one
     evaluation the measured/range/metrics are stable, so all but the
     popped head are reference-equal and the vdom diff skips them. */
  /* Shard-hugging contours, not bounding rectangles - but at REGION
     granularity: the pending set is leaf-granular (thousands of ids on
     a mega program), and per-leaf SVGs cost ~1s of path serialization
     per edit even viewport-culled. Adjacent pending rows merge into a
     few contiguous regions (the worklist completes roughly in program
     order, so the remainder stays near-contiguous), clamped to the
     visible window so path sizes stay bounded. */
  let merge_regions = (ranges: list((Id.t, (Point.t, Point.t)))) =>
    List.fold_left(
      (acc, (id, (o: Point.t, f: Point.t))) =>
        switch (acc) {
        | [(rid, (ro: Point.t, rf: Point.t)), ...rest]
            when o.row <= rf.row + 1 =>
          let rf' = rf.row > f.row ? rf : f;
          [(rid, (ro, rf')), ...rest];
        | _ => [(id, (o, f)), ...acc]
        },
      [],
      ranges,
    )
    |> List.rev;
  let clamp_region = ((id, (o: Point.t, f: Point.t))) => {
    let (first, last) = visible_bounds;
    let o =
      o.row >= first
        ? o
        : Point.{
            row: first,
            col: 0,
          };
    let f =
      f.row <= last
        ? f
        : Point.{
            row: last,
            col: Int.max_int,
          };
    (id, (o, f));
  };
  let regions =
    visible_ranges(pending_inactive_ranges)
    |> merge_regions
    |> List.map(clamp_region);
  let inactive_nodes =
    regions
    |> List.concat_map(((id, range)) =>
         IncrEvalCache.get(
           ~id,
           ~measured=syntax.measured,
           ~range,
           ~font_metrics,
           ~sweep=false,
           ~mk=() =>
           contour_of_range(
             ~font_metrics,
             ~measured=syntax.measured,
             ["incremental-pending"],
             range,
           )
         )
       );
  /* The active range can span thousands of rows (head-by-origin of the
     pending set) - clamp it to the visible window like the inactive
     regions, and cache its node: the sweep animates via CSS, so an
     identical vdom node across renders keeps animating. Uncached +
     unclamped this was ~50ms of path serialization on EVERY render
     for the whole duration of an evaluation. */
  let active_nodes =
    visible_ranges(active_ids)
    |> List.map(clamp_region)
    |> List.concat_map(((id, range)) =>
         IncrEvalCache.get(
           ~id,
           ~measured=syntax.measured,
           ~range,
           ~font_metrics,
           ~sweep=true,
           ~mk=() =>
           contour_of_range(
             ~font_metrics,
             ~measured=syntax.measured,
             ~sweep=true,
             ["incremental-pending", "incremental-active"],
             range,
           )
         )
       );
  div_c("incremental-highlights", inactive_nodes @ active_nodes);
};
