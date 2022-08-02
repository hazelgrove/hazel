open Virtual_dom.Vdom;
open Util;
open Core;

module Deco =
       (
         M: {
           let font_metrics: FontMetrics.t;
           let map: Measured.t;
           let show_backpack_targets: bool;
         },
       ) => {
  let font_metrics = M.font_metrics;

  let caret = (z: Zipper.t): list(Node.t) => {
    let origin = Caret.point(M.map, z);
    let shape = Caret.direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | Some((_, side, _)) => side
      | _ => Right
      };
    [CaretDec.view(~font_metrics, ~profile={side, origin, shape})];
  };

  /*
   let children = (p: Piece.t): list(Measured.measurement_lin) =>
     switch (p) {
     | Whitespace(_)
     | Grout(_) => []
     | Tile(t) =>
       let m = Measured.find_t(t, M.map);
       let token = List.nth(t.label);
       Aba.mk(t.shards, t.children)
       |> Aba.fold_left(
            shard => (m.origin.col + Unicode.length(token(shard)), []),
            (
              (origin, children: list(Measured.measurement_lin)),
              child,
              shard,
            ) => {
              let length = Measured.length(child, M.map);
              (
                origin + length + Unicode.length(token(shard)),
                children @ [{origin, length}],
              );
            },
          )
       |> snd;
     };
     */

  let selected_piece_profile =
      (p: Piece.t, nib_shape: Nib.Shape.t): PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Whitespace(_) => Mold.of_whitespace({sort: Any, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Any)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) =>
        Measured.find_shards(t, M.map)
        |> List.filter(((i, _)) => List.mem(i, t.shards))
      };
    let l = fst(List.hd(shards));
    let r = fst(ListUtil.last(shards));
    PieceDec.Profile.{shards, mold, style: Selected(l, r), index: 0};
  };

  let root_piece_profile =
      (index: int, p: Piece.t, nib_shape: Nib.Shape.t, (l, r))
      : PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Whitespace(_) => Mold.of_whitespace({sort: Any, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Any)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) => Measured.find_shards(t, M.map)
      };
    PieceDec.Profile.{shards, mold, style: Root(l, r), index};
  };

  let selected_pieces = (z: Zipper.t): list(Node.t) =>
    // TODO(d) mold/nibs/selemdec clean up pass
    z.selection.content
    |> List.filter(
         fun
         | Piece.Whitespace(w) when w.content == Whitespace.linebreak => false
         | _ => true,
       )
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let profile = selected_piece_profile(p, l);
           // TODO(andrew): do something different for the caret
           // adjacent piece so it lines up nice
           (
             snd(Mold.nibs(profile.mold)).shape,
             PieceDec.view(~font_metrics, ~rows=M.map.rows, profile),
           );
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
      let nib_shape =
        switch (Caret.direction(z)) {
        | None => Nib.Shape.Convex
        | Some(nib) => Nib.Shape.relative(nib, side)
        };
      let range: option((Measured.point, Measured.point)) =
        if (Piece.has_ends(p)) {
          let ranges = TermRanges.mk(Zipper.zip(z));
          switch (TermRanges.find_opt(Piece.id(p), ranges)) {
          | None => None
          | Some((p_l, p_r)) =>
            let l = Measured.find_p(p_l, M.map).origin;
            let r = Measured.find_p(p_r, M.map).last;
            Some((l, r));
          };
        } else {
          // using range of piece itself hides unidelimited child borders
          let m = Measured.find_p(p, M.map);
          Some((m.origin, m.last));
        };
      let index =
        switch (Indicated.shard_index(z)) {
        | None => (-1)
        | Some(i) => i
        };
      //TODO(andrew): get this working
      let _segs =
        switch (p) {
        | Tile({children, mold, _}) =>
          children
          |> List.flatten
          |> List.filter(
               fun
               | Piece.Whitespace(w) when w.content == Whitespace.linebreak =>
                 false
               | _ => true,
             )
          |> List.map(p => (mold, Measured.find_p(p, M.map)))
        | _ => []
        };
      switch (range) {
      | None => []
      | Some(range) =>
        PieceDec.view(
          ~font_metrics,
          ~rows=M.map.rows,
          ~segs=[],
          root_piece_profile(index, p, nib_shape, range),
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
      |> List.map(((l, r)) => {
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
         })
      |> List.concat;
    switch (root_targets) {
    | [_, ..._] => root_targets
    | [] =>
      seg
      |> List.filter_map(
           fun
           | Piece.Tile(t) => Some(t)
           | _ => None,
         )
      |> List.map((t: Tile.t) => {
           // TODO(d): unify with Relatives.local_incomplete_tiles
           Tile.contained_children(t)
           |> List.map(((l, seg, r)) =>
                targets(~container_shards=(l, r), bp, seg)
              )
           |> List.concat
         })
      |> List.concat
    };
  };

  let backback = (z: Zipper.t): list(Node.t) => [
    BackpackView.view(~font_metrics, ~origin=Caret.point(M.map, z), z),
  ];

  let targets' = (backpack, seg) => {
    M.show_backpack_targets && Backpack.restricted(backpack)
      ? targets(backpack, seg) : [];
  };

  let error_holes = zipper => {
    //TODO(andrew): how do i fold over this
    let _ci =
      switch (zipper |> Indicated.index) {
      | Some(index) =>
        let (_, _, info_map) =
          zipper |> Term.of_zipper |> Statics.uexp_to_info_map;
        Id.Map.find_opt(index, info_map);
      | None => None
      };
    ();
  };

  let all = (zipper, sel_seg) =>
    List.concat([
      caret(zipper),
      indicated_piece_deco(zipper),
      selected_pieces(zipper),
      backback(zipper),
      targets'(zipper.backpack, sel_seg),
    ]);
};
