open Util;
open Util.WebUtil;
open Haz3lcore;

type shard_data = (Measured.measurement, Nibs.shapes);

let sel_shard_svg =
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

module HighlightSegment =
       (
         M: {
           let measured: Measured.t;
           let shape_map: ProjectorCore.Shape.Map.t;
           let font_metrics: FontMetrics.t;
         },
       ) => {
  let find_g = Measured.find_g(~msg="Highlight.of_piece", _, M.measured);
  let find_w = Measured.find_w(~msg="Highlight.of_piece", _, M.measured);
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
      | Grout(g) => [Some(sel_shard_svg(~start_shape, find_g(g), p))]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [
          Some((
            find_w(w),
            (start_shape |> Option.map(Nib.Shape.flip), start_shape),
          )),
        ]
      };
    let start_shape =
      switch (Piece.nibs(p)) {
      | None => start_shape
      | Some((_, {shape, _})) => Some(Nib.Shape.flip(shape))
      };
    (start_shape, shard_data);
  }
  and of_tile = (~start_shape, t: Tile.t): list(option(_)) => {
    let tile_shards =
      Measured.find_shards(~msg="sel_of_tile", t, M.measured)
      |> List.filter(((i, _)) => List.mem(i, t.shards))
      |> List.map(((index, m)) => {
           let token = List.nth(t.label, index);
           switch (StringUtil.num_linebreaks(token)) {
           | 0 => [Some(sel_shard_svg(~start_shape, m, Tile(t)))]
           | num_lb =>
             multiline_shard(num_lb, m, (Some(Convex), Some(Convex)))
           };
         });
    let shape_at = index => Some(snd(Mold.nibs(~index, t.mold)).shape);
    let children_shards =
      t.children |> List.mapi(index => of_segment(shape_at(index)));
    if (List.length(tile_shards) != List.length(children_shards) + 1) {
      failwith(
        "Deco.of_tile: shard mismatch:"
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
    switch (Measured.find_pr_opt(p, M.measured)) {
    | None => failwith("Deco.of_projector: missing measurement")
    | Some(_m) =>
      let shape = ProjectorCore.Shape.Map.lookup(p.id, M.shape_map);
      /* Handling this internal to ProjectorsView at the moment because the
       * commented-out strategy doesn't work well, since the inserted str8-
       * edged lines vertical edge placement doesn't account for whether
       * the initial/final rows begin/end as concave/convex, and hence are
       * of slightly different lengths than is desirable */
      // multiline_shard(
      //   StringUtil.num_linebreaks(token),
      //   m,
      //   (Some(Convex), Some(Convex)),
      // );
      let num_lb =
        switch (shape.vertical) {
        | Inline => 0
        | Tab(num_lbs) => num_lbs
        | Block(num_lbs) => num_lbs
        };
      if (num_lb == 0) {
        [
          Some(
            sel_shard_svg(
              ~start_shape,
              Measured.find_pr(p, M.measured),
              Projector(p),
            ),
          ),
        ];
      } else {
        List.init(num_lb + 1, _ => None);
      };
    }
  and of_segment =
      (start_shape: ShardDec.tip, seg: Segment.t): list(option(_)) => {
    seg
    |> ListUtil.fold_left_map(of_piece, start_shape)
    |> snd
    |> List.flatten;
  }
  and go =
      (segment: Segment.t, shape_init: ShardDec.tip, classes): list(Node.t) =>
    /* We draw a single deco per row by dividing partionining the shards
     * into linebreak-seperated segments, then combining the measurements
     * and shapes of the first and last shard of each segment. Ideally we
     * could just get this info from the row measurements, but we have no
     * current way of figuring out shapes for whitespace without traversing */
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
       )
    |> List.map(((measurement, tips)) =>
         ShardDec.simple(
           {
             font_metrics: M.font_metrics,
             measurement,
             tips,
           },
           classes,
         )
       );
};

let quick_select_deco = (segment: Segment.t): Node.t => {
  let shape_map = ProjectorCore.Shape.Map.empty; // assume no projectors
  module Highlight =
    HighlightSegment({
      let measured = Measured.of_segment(segment, shape_map);
      let shape_map = shape_map;
      let font_metrics =
        FontMetrics.{
          row_height: 25.125,
          col_width: 10.390625,
        };
    });
  switch (Highlight.go(segment, Some(Convex), [])) {
  | exception _exn => Node.div([])
  | ya => div_c("quick-select-deco", ya)
  };
};

module Deco =
       (
         M: {
           let globals: Globals.t;
           let editor: Editor.t;
           let statics: CachedStatics.t;
         },
       ) => {
  let font_metrics = M.globals.font_metrics;
  let map = M.editor.syntax.measured;
  let terms = M.editor.syntax.terms;
  let term_ranges = M.editor.syntax.term_ranges;
  let tiles = M.editor.syntax.tiles;
  let measured = M.editor.syntax.measured;
  let rows = measured.rows;
  let projectors = M.editor.syntax.projectors;
  let error_ids = M.statics.error_ids;
  let color_highlights = M.globals.color_highlights;
  let segment = M.editor.syntax.segment;

  let tile = id => Id.Map.find(id, tiles);

  let caret = (z: Zipper.t): Node.t => {
    let origin = Zipper.caret_point(map, z);
    let shape = Zipper.caret_direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | _
          when
            !Selection.is_empty(z.selection)
            && !Selection.is_buffer(z.selection) =>
        z.selection.focus
      | Some((_, side, _)) => Direction.toggle(side)
      | _ => Right
      };
    CaretDec.view(
      ~font_metrics,
      ~profile={
        side,
        origin,
        shape,
      },
    );
  };
  module Highlight =
    HighlightSegment({
      let measured = M.editor.syntax.measured;
      let shape_map = M.editor.syntax.shape_map;
      let font_metrics = font_metrics;
    });

  let segment_selected = (z: Zipper.t) =>
    Highlight.go(
      z.selection.content,
      Some(fst(Siblings.shapes(z.relatives.siblings))),
      ["selected", Selection.buffer_cls(z.selection)],
    );

  let term_range = (p): option((Point.t, Point.t)) => {
    let id = Language.Any.rep_id(Id.Map.find(Piece.id(p), terms));
    switch (TermRanges.find_opt(id, term_ranges)) {
    | None => None
    | Some((p_l, p_r)) =>
      let l = Measured.find_p(~msg="Dec.range", p_l, measured).origin;
      let r = Measured.find_p(~msg="Dec.range", p_r, measured).last;
      Some((l, r));
    };
  };

  let all_tiles = (p: Piece.t): list((Uuidm.t, Mold.t, Measured.Shards.t)) =>
    Id.Map.find(Piece.id(p), terms)
    |> Language.Any.ids
    |> List.map(id => {
         let t = tile(id);
         let shards = Measured.find_shards(~msg="all_tiles", t, measured);
         (id, t.mold, shards);
       });

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (Indicated.piece(z)) {
    | _ when z.selection.content != [] => []
    | None => []
    | Some((Grout(_), _, _)) => []
    | Some((Projector(p), _, _)) =>
      switch (Measured.find_pr_opt(p, M.editor.syntax.measured)) {
      | Some(measurement) => [
          ShardDec.simple(
            {
              font_metrics,
              measurement,
              tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
            },
            [
              p.syntax |> Piece.sort |> fst |> Sort.to_string,
              "caret",
              "indicated",
            ],
          ),
        ]
      | None => []
      }
    | Some((p, side, _)) =>
      // root_profile calculation assumes p is tile
      // TODO encode in types
      let _nib_shape =
        switch (Zipper.caret_direction(z)) {
        | None => Nib.Shape.Convex
        | Some(nib) => Nib.Shape.relative(nib, side)
        };
      let range = term_range(p);
      let index =
        switch (Indicated.shard_index(z)) {
        | None => (-1)
        | Some(i) => i
        };
      switch (range) {
      | None => []
      | _ when Piece.is_infix_delimiter_op_prefix(p) => []
      | Some(range) =>
        let tiles = all_tiles(p);
        IndicationDec.term(
          ~line_clss=[],
          ~font_metrics,
          ~rows,
          ~caret=(Piece.id(p), index),
          ~tiles,
          range,
        );
      };
    };
  };

  let backpack = (z: Zipper.t): Node.t => {
    /* If there is a selection, any tiles bisected by the selection
     * will show as incomplete. While a more intelligent approach is
     * possible here, I've opted for the simpler option of supressing
     * backpack display during selection */
    let contents =
      Selection.is_empty(z.selection) || Selection.is_buffer(z.selection)
        ? Zipper.local_backpack(z)
          @ Segment.global_missing_shards(segment)
          |> ListUtil.dedup
          |> List.map(Tile.effective_label)
          |> List.map(List.hd)
        : [];
    Backpack.view(
      ~font_metrics,
      ~can_put_down=Zipper.can_put_down(z),
      ~caret_d=Zipper.caret_direction(z),
      ~ind_d=
        switch (Indicated.piece(z)) {
        | Some((_, d, _)) => Some(d)
        | None => None
        },
      ~origin=Zipper.caret_point(measured, z),
      contents,
    );
  };

  let term_decoration =
      (~id: Id.t, deco: ((Point.t, Point.t, SvgUtil.Path.t)) => Node.t) => {
    let (p_l, p_r) = TermRanges.find(id, term_ranges);
    let l = Measured.find_p(~msg="Deco.term", p_l, measured).origin;
    let r = Measured.find_p(~msg="Deco.term", p_r, measured).last;
    open SvgUtil.Path;
    let r_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.concat_map(i => {
           let row = Measured.Rows.find(i, measured.rows);
           [h(~x=i == r.row ? r.col : row.max_col), v_(~dy=1)];
         });
    let l_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.rev_map(i => {
           let row = Measured.Rows.find(i, measured.rows);
           [h(~x=i == l.row ? l.col : row.indent), v_(~dy=-1)];
         })
      |> List.concat;
    let path =
      [m(~x=l.col, ~y=l.row), ...r_edge]
      @ l_edge
      @ [Z]
      |> translate({
           dx: Float.of_int(- l.col),
           dy: Float.of_int(- l.row),
         });
    (l, r, path) |> deco;
  };

  let term_highlight = (~clss: list(string), id: Id.t) =>
    try(
      term_decoration(~id, ((origin, last, path)) =>
        DecUtil.code_svg_sized(
          ~font_metrics,
          ~measurement={
            origin,
            last,
          },
          ~base_cls=clss,
          path,
        )
      )
    ) {
    | Not_found =>
      /* This is caused by the statics overloading for exercise mode. The overriding
       * Exercise mode statics maps are calculated based on splicing together multiple
       * editors, but error_ids are extracted generically from the statics map, so
       * there may be error holes that don't occur in the editor being rendered.
       * Additionally, when showing color highlights when the backpack is non-empty,
       * the prospective completion may have different ids than the displayed code. */
      Node.div([])
    };

  let color_highlights = () =>
    div_c(
      "color-highlights",
      List.map(
        ((id, color)) =>
          term_highlight(~clss=["highlight-code-" ++ color], id),
        switch (color_highlights) {
        | Some(colorMap) => ColorSteps.to_list(colorMap)
        | _ => []
        },
      ),
    );

  let error_view = (id: Id.t) =>
    try(
      switch (Id.Map.find_opt(id, projectors)) {
      | Some(p) =>
        /* Special case for projectors as they are not in tile map */
        let shapes = ProjectorCore.shapes(p);
        let measurement = Id.Map.find(id, measured.projectors);
        div_c(
          "errors-piece",
          [
            ShardDec.simple(
              {
                font_metrics,
                tips: ShardDec.tips_of_shapes(shapes),
                measurement,
              },
              ["error"],
            ),
          ],
        );
      | None =>
        let p = Piece.Tile(tile(id));
        switch (term_range(p)) {
        | Some(range) =>
          let tiles = all_tiles(p);
          div_c(
            "errors-piece",
            IndicationDec.error_term(~font_metrics, ~rows, range, tiles),
          );
        | None => div_c("errors-piece", [])
        };
      }
    ) {
    | Not_found =>
      /* This is caused by the statics overloading for exercise mode. The overriding
       * Exercise mode statics maps are calculated based on splicing together multiple
       * editors, but error_ids are extracted generically from the statics map, so
       * there may be error holes that don't occur in the editor being rendered */
      Node.div([])
    };

  let errors = () => div_c("errors", List.map(error_view, error_ids));

  let indication = (z: Zipper.t) =>
    div_c("indication", indicated_piece_deco(z));

  let selection = (z: Zipper.t) => div_c("selects", segment_selected(z));

  let always = () => [errors()];

  let next_steps = (next_steps, ~inject) => {
    let tiles = List.filter_map(TileMap.find_opt(_, tiles), next_steps);
    List.mapi(
      (i, t: Tile.t) => {
        let id = Tile.id(t);
        let mold = t.mold;
        let shards = Measured.find_shards(t, map);
        let range: option((Measured.Point.t, Measured.Point.t)) = {
          // if (Piece.has_ends(p)) {
          let id = Id.Map.find(id, terms) |> Language.Any.rep_id;
          switch (TermRanges.find_opt(id, term_ranges)) {
          | None => None
          | Some((p_l, p_r)) =>
            let l = Measured.find_p(p_l, map).origin;
            let r = Measured.find_p(p_r, map).last;
            Some((l, r));
          };
        };
        Option.map(
          x => {
            IndicationDec.term(
              ~base_clss="tile-next-step",
              ~attr=[Virtual_dom.Vdom.Attr.on_mousedown(_ => {inject(i)})],
              ~line_clss=["next-step-line"],
              ~font_metrics,
              ~caret=(Id.invalid, 0),
              ~rows=measured.rows,
              ~tiles=[(id, mold, shards)],
              x,
            )
            @ IndicationDec.term(
                ~base_clss="tile-next-step-top",
                ~attr=[Virtual_dom.Vdom.Attr.on_mousedown(_ => {inject(i)})],
                ~line_clss=["next-step-line"],
                ~font_metrics,
                ~caret=(Id.invalid, 0),
                ~rows=measured.rows,
                ~tiles=[(id, mold, shards)],
                x,
              )
          },
          range,
        );
      },
      tiles,
    )
    |> List.filter_map(x => x)
    |> List.flatten;
  };

  let taken_steps = taken_steps => {
    let tiles = List.filter_map(TileMap.find_opt(_, tiles), taken_steps);
    List.mapi(
      (_, t: Tile.t) => {
        let id = Tile.id(t);
        let mold = t.mold;
        let shards = Measured.find_shards(t, map);
        let range: option((Measured.Point.t, Measured.Point.t)) = {
          // if (Piece.has_ends(p)) {
          let id = Id.Map.find(id, terms) |> Language.Any.rep_id;
          switch (TermRanges.find_opt(id, term_ranges)) {
          | None => None
          | Some((p_l, p_r)) =>
            let l = Measured.find_p(p_l, map).origin;
            let r = Measured.find_p(p_r, map).last;
            Some((l, r));
          };
        };
        IndicationDec.term(
          ~base_clss="tile-taken-step",
          ~line_clss=["taken-step-line"],
          ~font_metrics,
          ~caret=(Id.invalid, 0),
          ~rows=measured.rows,
          ~tiles=[(id, mold, shards)],
        )
        |> Option.map(_, range);
      },
      tiles,
    )
    |> List.filter_map(x => x)
    |> List.flatten;
  };

  let statics = () => [errors()];

  let editor = (z, selected: bool) =>
    selected
      ? [
        caret(z),
        indication(z),
        selection(z),
        backpack(z),
        color_highlights(),
      ]
      : [];
};
