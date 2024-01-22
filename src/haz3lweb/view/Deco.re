open Virtual_dom.Vdom;
open Util;
open Haz3lcore;

module Deco =
       (
         M: {
           let font_metrics: FontMetrics.t;
           let map: Measured.t;
           let show_backpack_targets: bool;
           let terms: TermMap.t;
           let term_ranges: TermRanges.t;

           let global_inference_info: InferenceResult.global_inference_info;
           let info_map: Statics.Map.t;
           let tiles: TileMap.t;
         },
       ) => {
  let font_metrics = M.font_metrics;

  let tile = id => Id.Map.find(id, M.tiles);

  let caret = (z: Zipper.t): list(Node.t) => {
    let origin = Zipper.caret_point(M.map, z);
    let shape = Zipper.caret_direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | Some((_, side, _)) => side
      | _ => Right
      };
    [CaretDec.view(~font_metrics, ~profile={side, origin, shape})];
  };

  let selected_piece_profile =
      (~buffer, p: Piece.t, nib_shape: Nib.Shape.t): PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Secondary(_) => Mold.of_secondary({sort: Any, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Any)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Secondary(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) =>
        Measured.find_shards(t, M.map)
        |> List.filter(((i, _)) => List.mem(i, t.shards))
      };
    let id = Piece.id(p);
    let tiles = [(id, mold, shards)];
    let l = fst(List.hd(shards));
    let r = fst(ListUtil.last(shards));
    // TODO this is ignored in view, clean this up
    let caret = (id, (-1));
    let style: PieceDec.Profile.style =
      buffer
        ? SelectedBuffer((id, l), (id, r)) : Selected((id, l), (id, r));
    PieceDec.Profile.{tiles, caret, style};
  };

  let root_piece_profile =
      (index: int, p: Piece.t, (l, r)): PieceDec.Profile.t => {
    let tiles =
      Id.Map.find(Piece.id(p), M.terms)
      |> Term.ids
      /* NOTE(andrew): dark_ids were originally filtered here.
       * Leaving this comment in place in case issues in the
       * future are traced back to here.
       * |> List.filter(id => id >= 0)*/
      |> List.map(id => {
           let t = tile(id);
           (id, t.mold, Measured.find_shards(t, M.map));
         });
    PieceDec.Profile.{
      tiles,
      caret: (Piece.id(p), index),
      style: Root(l, r),
    };
  };

  let selected_pieces = (z: Zipper.t): list(Node.t) =>
    // TODO(d) mold/nibs/selemdec clean up pass
    z.selection.content
    |> List.filter(
         fun
         | Piece.Secondary(w) when Secondary.is_linebreak(w) => false
         | _ => true,
       )
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let profile =
             selected_piece_profile(
               ~buffer=Selection.is_buffer(z.selection),
               p,
               l,
             );
           let shape =
             switch (Piece.nibs(p)) {
             | None => l
             | Some((_, {shape, _})) => shape
             };
           // TODO(andrew): do something different for the caret
           // adjacent piece so it lines up nice
           (shape, PieceDec.view(~font_metrics, ~rows=M.map.rows, profile));
         },
         fst(Siblings.shapes(z.relatives.siblings)),
       )
    |> snd
    |> List.flatten;

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
      let range: option((Measured.Point.t, Measured.Point.t)) = {
        // if (Piece.has_ends(p)) {
        switch (TermRanges.find_opt(Piece.id(p), M.term_ranges)) {
        | None => None
        | Some((p_l, p_r)) =>
          let l = Measured.find_p(p_l, M.map).origin;
          let r = Measured.find_p(p_r, M.map).last;
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
        PieceDec.view(
          ~font_metrics,
          ~rows=M.map.rows,
          ~segs=[],
          root_piece_profile(index, p, range),
        )
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
                 let m = Measured.find_p(p, M.map);
                 Measured.{origin: m.origin, last: m.origin};
               | (Some(p), _) =>
                 let m = Measured.find_p(p, M.map);
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

  let backback =
      (
        ~global_inference_info: InferenceResult.global_inference_info,
        z: Zipper.t,
      )
      : list(Node.t) => [
    BackpackView.view(
      ~global_inference_info,
      ~font_metrics,
      ~origin=Zipper.caret_point(M.map, z),
      z,
    ),
  ];

  let targets' = (backpack, seg) => {
    M.show_backpack_targets && Backpack.restricted(backpack)
      ? targets(backpack, seg) : [];
  };

  let term_decoration =
      (
        ~id: Id.t,
        deco:
          ((Measured.Point.t, Measured.Point.t, SvgUtil.Path.t)) => Node.t,
      ) => {
    let (p_l, p_r) = TermRanges.find(id, M.term_ranges);
    let l = Measured.find_p(p_l, M.map).origin;
    let r = Measured.find_p(p_r, M.map).last;
    open SvgUtil.Path;
    let r_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.concat_map(i => {
           let row = Measured.Rows.find(i, M.map.rows);
           [h(~x=i == r.row ? r.col : row.max_col), v_(~dy=1)];
         });
    let l_edge =
      ListUtil.range(~lo=l.row, r.row + 1)
      |> List.rev_map(i => {
           let row = Measured.Rows.find(i, M.map.rows);
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

  let term_highlight = (~clss: list(string), id: Id.t) => {
    term_decoration(~id, ((origin, last, path)) =>
      DecUtil.code_svg_sized(
        ~font_metrics,
        ~measurement={origin, last},
        ~base_cls=clss,
        path,
      )
    );
  };

  let color_highlights = (colorings: list((Id.t, string))) => {
    List.map(
      ((id, color)) => {
        term_highlight(~clss=["highlight-code-" ++ color], id)
      },
      colorings,
    );
  };

  // recurses through skel structure to enable experimentation
  // with hiding nested err holes
  let _err_holes = (z: Zipper.t) => {
    let seg = Zipper.unselect_and_zip(z);
    let is_err = (id: Id.t) =>
      switch (Id.Map.find_opt(id, M.info_map)) {
      | None => false
      | Some(info) => Info.is_error(info)
      };
    let is_rep = (id: Id.t) =>
      switch (Id.Map.find_opt(id, M.terms)) {
      | None => false
      | Some(term) => id == Term.rep_id(term)
      };
    let rec go_seg = (seg: Segment.t): list(Id.t) => {
      let rec go_skel = (skel: Skel.t): list(Id.t) => {
        let root = Skel.root(skel);
        let root_ids =
          Aba.get_as(root)
          |> List.map(List.nth(seg))
          |> List.map(Piece.id)
          |> List.filter(is_rep)
          |> List.filter(is_err);
        let between_ids = Aba.get_bs(root) |> List.concat_map(go_skel);
        let uni_ids =
          switch (skel) {
          | Op(_) => []
          | Pre(_, r) => go_skel(r)
          | Post(l, _) => go_skel(l)
          | Bin(l, _, r) => go_skel(l) @ go_skel(r)
          };
        root_ids @ between_ids @ uni_ids;
      };
      let bi_ids =
        seg
        |> List.concat_map(p => List.concat_map(go_seg, Piece.children(p)));

      go_skel(Segment.skel(seg)) @ bi_ids;
    };
    go_seg(seg) |> List.map(term_highlight(~clss=["err-hole"]));
  };

  // faster infomap traversal
  let err_holes = (_z: Zipper.t) => {
    Id.Map.fold(
      (id, info, acc) =>
        /* Because of artefacts in Maketerm ID handling,
         * there are be situations where ids appear in the
         * info_map which do not occur in term_ranges. These
         * ids should be purely duplicative, so skipping them
         * when iterating over the info_map should have no
         * effect, beyond supressing the resulting Not_found exs */
        switch (Id.Map.find_opt(id, M.term_ranges)) {
        | Some(_) when Info.is_error(info) => [
            term_highlight(~clss=["err-hole"], id),
            ...acc,
          ]
        | _ => acc
        },
      M.info_map,
      [],
    );
  };

  let all = (zipper, sel_seg) =>
    List.concat([
      caret(zipper),
      indicated_piece_deco(zipper),
      selected_pieces(zipper),
      backback(~global_inference_info=M.global_inference_info, zipper),
      targets'(zipper.backpack, sel_seg),
      err_holes(zipper),
    ]);
};
