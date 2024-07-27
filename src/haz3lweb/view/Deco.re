open Virtual_dom.Vdom;
open Util;
open Util.Web;
open Haz3lcore;

module Deco = (M: {
                 let ui_state: Model.ui_state;
                 let meta: Editor.Meta.t;
               }) => {
  let font_metrics = M.ui_state.font_metrics;

  let tile = id => Id.Map.find(id, M.meta.syntax.tiles);

  let caret = (z: Zipper.t): Node.t => {
    let origin = Zipper.caret_point(M.meta.syntax.measured, z);
    let shape = Zipper.caret_direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | Some((_, side, _)) => side
      | _ => Right
      };
    CaretDec.view(~font_metrics, ~profile={side, origin, shape});
  };

  type shard_data = (Measured.measurement, Nibs.shapes);

  let sel_shard_svg =
      (~index=?, ~start_shape, measurement: Measured.measurement, p)
      : (Measured.measurement, Nibs.shapes) => (
    measurement,
    Mold.nib_shapes(~index?, Piece.mold_of(~shape=start_shape, p)),
  );

  let rec sel_of_piece =
          (start_shape: Nib.Shape.t, p: Piece.t)
          : (Nib.Shape.t, list(option(shard_data))) => {
    let shard_data =
      switch (p) {
      | Tile(t) => sel_of_tile(~start_shape, t)
      | Projector(p) => sel_of_projector(~start_shape, p)
      | Grout(g) => [
          Some(
            sel_shard_svg(
              ~start_shape,
              Measured.find_g(
                ~msg="Deco.sel_of_piece",
                g,
                M.meta.syntax.measured,
              ),
              p,
            ),
          ),
        ]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [
          Some(
            sel_shard_svg(
              ~start_shape,
              Measured.find_w(
                ~msg="Deco.sel_of_piece",
                w,
                M.meta.syntax.measured,
              ),
              p,
            ),
          ),
        ]
      };
    let start_shape =
      switch (Piece.nibs(p)) {
      | None => start_shape
      | Some((_, {shape, _})) => shape
      };
    (start_shape, shard_data);
  }
  and sel_of_tile = (~start_shape, t: Tile.t): list(option(shard_data)) => {
    let tile_shards =
      Measured.find_shards(~msg="sel_of_tile", t, M.meta.syntax.measured)
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
    let shape_at = index => snd(Mold.nibs(~index, t.mold)).shape;
    let children_shards =
      t.children |> List.mapi(index => sel_of_segment(shape_at(index)));
    ListUtil.interleave(tile_shards, children_shards) |> List.flatten;
  }
  and sel_of_projector =
      (~start_shape, p: Base.projector): list(option(shard_data)) => {
    let m =
      switch (Measured.find_pr_opt(p, M.meta.syntax.measured)) {
      | None =>
        failwith("TODO(andrew): Deco.sel_of_projector: missing measurement")
      | Some(m) => m
      };
    let ci = Id.Map.find_opt(p.id, M.meta.statics.info_map);
    let token = Projector.placeholder(p, ci);
    switch (StringUtil.num_linebreaks(token)) {
    | 0 => [Some(sel_shard_svg(~start_shape, ~index=0, m, Projector(p)))]
    //TODO(andrew): clarify
    /* Deco for multi-line tokens e.g. projector placeholders
     * handled in ProjectorsView but need to leave some blank lines */
    | num_lb => List.init(num_lb, _ => None)
    };
  }
  and sel_of_segment =
      (start_shape: Nib.Shape.t, seg: Segment.t): list(option(shard_data)) => {
    seg
    |> ListUtil.fold_left_map(sel_of_piece, start_shape)
    |> snd
    |> List.flatten;
  }
  and selected_pieces = (z: Zipper.t): list(Node.t) =>
    /* We draw a single deco per row by dividing partionining the shards
     * into linebreak-seperated segments, then combining the measurements
     * and shapes of the first and last shard of each segment. Ideally we
     * could just get this info from the row measurements, but we have no
     * current way of figuring out shapes for whitespace without traversing */
    sel_of_segment(
      fst(Siblings.shapes(z.relatives.siblings)),
      z.selection.content,
    )
    |> ListUtil.split_at_nones
    |> ListUtil.first_and_last
    |> List.map((((m1, (l1, _)): shard_data, (m2, (_, r2)): shard_data)) =>
         (({origin: m1.origin, last: m2.last}, (l1, r2)): shard_data)
       )
    |> List.map(((measurement, shapes)) =>
         PieceDec.simple_shard_selected(
           ~buffer=Selection.is_buffer(z.selection),
           ~font_metrics,
           ~measurement,
           ~shapes,
         )
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
        let id =
          Id.Map.find(Piece.id(p), M.meta.syntax.terms) |> Term.rep_id;
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
          |> Term.ids
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
             let profile =
               CaretPosDec.Profile.{style: `Sibling, measurement, sort: Exp};
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

  let color_highlights = (colorings: list((Id.t, string))) => {
    List.map(
      ((id, color)) =>
        term_highlight(~clss=["highlight-code-" ++ color], id),
      colorings,
    );
  };

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
            PieceDec.simple_shard_error(~font_metrics, ~shapes, ~measurement),
          ],
        );
      | None =>
        let tiles =
          Id.Map.find(id, M.meta.syntax.terms)
          |> Term.ids
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

  let selection = (z: Zipper.t) => div_c("selects", selected_pieces(z));

  let always = () => [errors()];

  let all = z => [
    caret(z),
    indication(z),
    selection(z),
    backpack(z),
    backpack_targets(z.backpack, M.meta.syntax.segment),
    errors(),
  ];
};
