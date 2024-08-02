open Virtual_dom.Vdom;
open Util;
open Util.Web;
open Haz3lcore;

type shard_data = (Measured.measurement, Nibs.shapes);

let sel_shard_svg =
    (
      ~index=?,
      ~start_shape: PieceDec.tip,
      measurement: Measured.measurement,
      p: Piece.t,
    )
    : (Measured.measurement, (PieceDec.tip, PieceDec.tip)) => (
  measurement,
  switch (p) {
  | Tile(t) => Mold.nib_shapes(~index?, t.mold) |> PieceDec.tips_of_shapes
  | Grout(g) =>
    Mold.nib_shapes(Mold.of_grout(g, Any)) |> PieceDec.tips_of_shapes
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
  | Projector(p) =>
    ProjectorBase.mold_of(p, Any)
    |> Mold.nib_shapes
    |> PieceDec.tips_of_shapes
  },
);
module HighlightSegment =
       (
         M: {
           let measured: Measured.t;
           let info_map: Statics.Map.t;
           let font_metrics: FontMetrics.t;
         },
       ) => {
  let find_g = Measured.find_g(~msg="Highlight.of_piece", _, M.measured);
  let find_w = Measured.find_w(~msg="Highlight.of_piece", _, M.measured);
  let rec of_piece =
          (start_shape: PieceDec.tip, p: Piece.t)
          : (
              PieceDec.tip,
              list(
                option(
                  (Measured.measurement, (PieceDec.tip, PieceDec.tip)),
                ),
              ),
            ) => {
    let shard_data =
      switch (p) {
      | Tile(t) => of_tile(~start_shape, t)
      | Projector(p) => of_projector(~start_shape, p)
      | Grout(g) => [Some(sel_shard_svg(~start_shape, find_g(g), p))]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [Some((find_w(w), (None, None)))]
      };
    let start_shape =
      switch (Piece.nibs(p)) {
      | None => None
      | Some((_, {shape, _})) => Some(shape)
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
           | 0 => [Some(sel_shard_svg(~start_shape, ~index, m, Tile(t)))]
           //TODO(andrew): clarify
           /* Deco for multi-line tokens e.g. projector placeholders
            * handled in ProjectorsView but need to leave some blank lines */
           | num_lb => List.init(num_lb, _ => None)
           };
         });
    //TODO(andrew): Some below
    let shape_at = index => Some(snd(Mold.nibs(~index, t.mold)).shape);
    let children_shards =
      t.children |> List.mapi(index => of_segment(shape_at(index)));
    ListUtil.interleave(tile_shards, children_shards) |> List.flatten;
  }
  and of_projector = (~start_shape, p: Base.projector): list(option(_)) => {
    let m =
      switch (Measured.find_pr_opt(p, M.measured)) {
      | None =>
        failwith("TODO(andrew): Deco.sel_of_projector: missing measurement")
      | Some(m) => m
      };
    let ci = Id.Map.find_opt(p.id, M.info_map);
    let token = Projector.placeholder(p, ci);
    switch (StringUtil.num_linebreaks(token)) {
    | 0 => [Some(sel_shard_svg(~start_shape, ~index=0, m, Projector(p)))]
    //TODO(andrew): clarify
    /* Deco for multi-line tokens e.g. projector placeholders
     * handled in ProjectorsView but need to leave some blank lines */
    | num_lb => List.init(num_lb, _ => None)
    };
  }
  and of_segment =
      (start_shape: PieceDec.tip, seg: Segment.t): list(option(_)) => {
    seg
    |> ListUtil.fold_left_map(of_piece, start_shape)
    |> snd
    |> List.flatten;
  }
  and go =
      (segment: Segment.t, shape_init: PieceDec.tip, classes): list(Node.t) =>
    /* We draw a single deco per row by dividing partionining the shards
     * into linebreak-seperated segments, then combining the measurements
     * and shapes of the first and last shard of each segment. Ideally we
     * could just get this info from the row measurements, but we have no
     * current way of figuring out shapes for whitespace without traversing */
    of_segment(shape_init, segment)
    |> ListUtil.split_at_nones
    |> ListUtil.first_and_last
    |> List.map((((m1, (l1, _)), (m2, (_, r2)))) =>
         (Measured.{origin: m1.origin, last: m2.last}, (l1, r2))
       )
    |> List.map(((measurement, tips)) =>
         PieceDec.simple_shard(
           {font_metrics: M.font_metrics, measurement, tips},
           classes,
         )
       );
};

module Deco =
       (
         M: {
           let ui_state: Model.ui_state;
           let meta: Editor.Meta.t;
           let highlights: option(ColorSteps.colorMap);
         },
       ) => {
  module Highlight =
    HighlightSegment({
      let measured = M.meta.syntax.measured;
      let info_map = M.meta.statics.info_map;
      let font_metrics = M.ui_state.font_metrics;
    });
  let font_metrics = M.ui_state.font_metrics;

  let tile = id => Id.Map.find(id, M.meta.syntax.tiles);

  let caret = (z: Zipper.t): Node.t => {
    let origin = Zipper.caret_point(M.meta.syntax.measured, z);
    let shape = Zipper.caret_direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | Some((_, side, _)) => Direction.toggle(side)
      | _ => Right
      };
    CaretDec.view(~font_metrics, ~profile={side, origin, shape});
  };

  let segment_selected = (z: Zipper.t) =>
    Highlight.go(
      z.selection.content,
      Some(fst(Siblings.shapes(z.relatives.siblings))),
      ["selected"] @ (Selection.is_buffer(z.selection) ? ["buffer"] : []),
    );

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (Indicated.piece(z)) {
    | _ when z.selection.content != [] => []
    | None => []
    | Some((Grout(_), _, _)) => []
    | Some((p, side, _)) =>
      // root_profile calculation assumes p is tile
      // TODO encode in types
      let _nib_shape =
        switch (Zipper.caret_direction(z)) {
        | None => Nib.Shape.Convex
        | Some(nib) => Nib.Shape.relative(nib, side)
        };
      let range: option((Point.t, Point.t)) = {
        // if (Piece.has_ends(p)) {
        let id = Id.Map.find(Piece.id(p), M.meta.syntax.terms) |> Any.rep_id;
        switch (TermRanges.find_opt(id, M.meta.syntax.term_ranges)) {
        | None => None
        | Some((p_l, p_r)) =>
          let l =
            Measured.find_p(
              ~msg="Deco.indicated",
              p_l,
              M.meta.syntax.measured,
            ).
              origin;
          let r =
            Measured.find_p(
              ~msg="Deco.indicated",
              p_r,
              M.meta.syntax.measured,
            ).
              last;
          Some((l, r));
        };
      };
      let index =
        switch (Indicated.shard_index(z)) {
        | None => (-1)
        | Some(i) => i
        };
      switch (range) {
      | None => []
      | Some(range) =>
        let tiles =
          Id.Map.find(Piece.id(p), M.meta.syntax.terms)
          |> Any.ids
          |> List.map(id => {
               let t = tile(id);
               (
                 id,
                 t.mold,
                 Measured.find_shards(
                   ~msg="Deco.indicated",
                   t,
                   M.meta.syntax.measured,
                 ),
               );
             });
        PieceDec.indicated(
          ~font_metrics,
          ~rows=M.meta.syntax.measured.rows,
          ~caret=(Piece.id(p), index),
          ~tiles,
          range,
        );
      };
    };
  };

  let rec targets = (~container_shards=?, bp: Backpack.t, seg: Segment.t) => {
    let with_container_shards = ((pre, suf) as sibs) =>
      switch (container_shards) {
      | None => sibs
      | Some((l, r)) => ([l, ...pre], suf @ [r])
      };
    let root_targets =
      ListUtil.splits(seg)
      |> List.concat_map(((l, r)) => {
           let sibs =
             Segment.(incomplete_tiles(l), incomplete_tiles(r))
             |> with_container_shards;
           switch (Backpack.pop(sibs, bp)) {
           | None
           | Some((true, _, _)) => []
           | Some(_) =>
             let measurement =
               switch (Siblings.neighbors((l, r))) {
               | (None, None) => failwith("impossible")
               | (_, Some(p)) =>
                 let m =
                   Measured.find_p(
                     ~msg="Deco.targets",
                     p,
                     M.meta.syntax.measured,
                   );
                 Measured.{origin: m.origin, last: m.origin};
               | (Some(p), _) =>
                 let m =
                   Measured.find_p(
                     ~msg="Deco.targets",
                     p,
                     M.meta.syntax.measured,
                   );
                 Measured.{origin: m.last, last: m.last};
               };
             let profile = CaretPosDec.Profile.{style: `Sibling, measurement};
             [CaretPosDec.view(~font_metrics, profile)];
           };
         });
    switch (root_targets) {
    | [_, ..._] => root_targets
    | [] =>
      seg
      |> List.filter_map(
           fun
           | Piece.Tile(t) => Some(t)
           | _ => None,
         )
      |> List.concat_map((t: Tile.t) => {
           // TODO(d): unify with Relatives.local_incomplete_tiles
           Tile.contained_children(t)
           |> List.concat_map(((l, seg, r)) =>
                targets(~container_shards=(l, r), bp, seg)
              )
         })
    };
  };

  let backpack = (z: Zipper.t): Node.t =>
    BackpackView.view(
      ~font_metrics,
      ~origin=Zipper.caret_point(M.meta.syntax.measured, z),
      z,
    );

  let backpack_targets = (backpack, seg) =>
    div_c(
      "backpack-targets",
      M.ui_state.show_backpack_targets && Backpack.restricted(backpack)
        ? targets(backpack, seg) : [],
    );

  let term_decoration =
      (~id: Id.t, deco: ((Point.t, Point.t, SvgUtil.Path.t)) => Node.t) => {
    let (p_l, p_r) = TermRanges.find(id, M.meta.syntax.term_ranges);
    let l =
      Measured.find_p(~msg="Deco.term", p_l, M.meta.syntax.measured).origin;
    let r =
      Measured.find_p(~msg="Deco.term", p_r, M.meta.syntax.measured).last;
    open SvgUtil.Path;
    let r_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.concat_map(i => {
           let row = Measured.Rows.find(i, M.meta.syntax.measured.rows);
           [h(~x=i == r.row ? r.col : row.max_col), v_(~dy=1)];
         });
    let l_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.rev_map(i => {
           let row = Measured.Rows.find(i, M.meta.syntax.measured.rows);
           [h(~x=i == l.row ? l.col : row.indent), v_(~dy=-1)];
         })
      |> List.concat;
    let path =
      [m(~x=l.col, ~y=l.row), ...r_edge]
      @ l_edge
      @ [Z]
      |> translate({dx: Float.of_int(- l.col), dy: Float.of_int(- l.row)});
    (l, r, path) |> deco;
  };

  let term_highlight = (~clss: list(string), id: Id.t) =>
    try(
      term_decoration(~id, ((origin, last, path)) =>
        DecUtil.code_svg_sized(
          ~font_metrics,
          ~measurement={origin, last},
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
        switch (M.highlights) {
        | Some(colorMap) => ColorSteps.to_list(colorMap)
        | _ => []
        },
      ),
    );

  let error_view = (id: Id.t) =>
    try(
      switch (Id.Map.find_opt(id, M.meta.syntax.projectors)) {
      | Some(p) =>
        /* Special case for projectors as they are not in tile map */
        let shapes = ProjectorBase.shapes(p);
        let measurement = Id.Map.find(id, M.meta.syntax.measured.projectors);
        div_c(
          "errors-piece",
          [
            PieceDec.simple_shard_error({
              font_metrics,
              tips: PieceDec.tips_of_shapes(shapes),
              measurement,
            }),
          ],
        );
      | None =>
        let tiles =
          Id.Map.find(id, M.meta.syntax.terms)
          |> Term.Any.ids
          |> List.map(id => {
               let t = tile(id);
               let shards =
                 Measured.find_shards(
                   ~msg="Deco.errors_of_tile",
                   t,
                   M.meta.syntax.measured,
                 );
               PieceDec.simple_shards_errors(~font_metrics, t.mold, shards);
             });
        div_c("errors-piece", List.flatten(tiles));
      }
    ) {
    | Not_found =>
      /* This is caused by the statics overloading for exercise mode. The overriding
       * Exercise mode statics maps are calculated based on splicing together multiple
       * editors, but error_ids are extracted generically from the statics map, so
       * there may be error holes that don't occur in the editor being rendered */
      Node.div([])
    };

  let errors = () =>
    div_c("errors", List.map(error_view, M.meta.statics.error_ids));

  let indication = (z: Zipper.t) =>
    switch (Projector.indicated(z)) {
    | Some(_) => Node.div([]) /* projector indication handled internally */
    | None => div_c("indication", indicated_piece_deco(z))
    };

  let selection = (z: Zipper.t) => div_c("selects", segment_selected(z));

  let always = () => [errors()];

  let all = z => [
    caret(z),
    indication(z),
    selection(z),
    backpack(z),
    backpack_targets(z.backpack, M.meta.syntax.segment),
    errors(),
    color_highlights(),
  ];
};
