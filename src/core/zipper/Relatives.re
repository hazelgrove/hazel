open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let empty = {siblings: Siblings.empty, ancestors: Ancestors.empty};

let push = (d: Direction.t, p: Piece.t, rs: t): t => {
  ...rs,
  siblings: Siblings.push(d, p, rs.siblings),
};

let prepend = (d: Direction.t, seg: Segment.t, rs: t): t => {
  let siblings = Siblings.prepend(d, seg, rs.siblings);
  {...rs, siblings};
};

let pop = (d: Direction.t, rs: t): option((Piece.t, t)) =>
  switch (Siblings.pop(d, rs.siblings)) {
  | Some((p, siblings)) => Some((p, {...rs, siblings}))
  | None =>
    switch (rs.ancestors) {
    | [] => None
    | [(ancestor, siblings), ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (p, siblings) =
        Siblings.(pop(d, concat([rs.siblings, siblings', siblings])));
      (p, {siblings, ancestors});
    }
  };

let zip = (~sel=Segment.empty, {siblings, ancestors}: t) =>
  Ancestors.zip(Siblings.zip(~sel, siblings), ancestors);

let local_incomplete_tiles = ({siblings: (pre, suf), ancestors}: t) => {
  let sibs =
    switch (ancestors) {
    | [] => (pre, suf)
    | [(a, _), ..._] =>
      let (l, r) = Ancestor.container_shards(a);
      ([l, ...pre], suf @ [r]);
    };
  Siblings.incomplete_tiles(sibs);
};

let parent =
    (~sel=Segment.empty, {siblings: (l_sibs, r_sibs), ancestors}: t)
    : option(Piece.t) =>
  ancestors
  |> Ancestors.parent
  |> Option.map(p => Base.Tile(Ancestor.zip(l_sibs @ sel @ r_sibs, p)));

let disassemble = ({siblings, ancestors}: t): Siblings.t =>
  Siblings.concat([siblings, Ancestors.disassemble(ancestors)]);

let remold = ({siblings, ancestors}: t): list(t) => {
  open ListUtil.Syntax;
  // let+ ancestors = Ancestors.remold(ancestors)
  let s = Ancestors.sort(ancestors);
  let+ siblings = Siblings.remold(siblings, s);
  {ancestors, siblings};
};

let sort_rank = ({siblings, ancestors}: t) => {
  let s = Ancestors.sort(ancestors);
  // let (s_l, s_r) = Siblings.sorts(siblings, s);
  Ancestors.sort_rank(ancestors)
  // + Siblings.sort_rank(siblings, s)
  + Segment.sort_rank(Siblings.zip(siblings), s);
  // + Bool.to_int(!Sort.consistent(s_l, s_r));
};

let shape_rank = ({siblings, ancestors}: t) => {
  let (l, r) = Siblings.shapes(siblings);
  Ancestors.shape_rank(ancestors)
  + Siblings.shape_rank(siblings)
  + Bool.to_int(!Nib.Shape.fits(l, r));
};

let regrout = (d: Direction.t, {siblings, ancestors}: t): IdGen.t(t) => {
  open IdGen.Syntax; /* Direction is side of grout caret will end up on */

  let* ancestors = Ancestors.regrout(ancestors);
  let+ siblings = {
    let* ((pre, s_l, trim_l), (trim_r, s_r, suf)) =
      Siblings.regrout(siblings);
    let+ (trim_l, trim_r) = {
      open Segment.Trim;
      let ((_, gs_l), (_, gs_r)) = (trim_l, trim_r);
      let (seg_l, seg_r) = (to_seg(trim_l), to_seg(trim_r));
      switch (ListUtil.split_last_opt(gs_l), gs_r) {
      | (Some((_, g_l)), [g_r, ..._]) =>
        IdGen.return(
          Grout.fits(g_l, g_r)
            // note: assumes single grout invariant in un-caret-interrupted trim
            ? (ws(trim_l), ws(trim_r))  //(ws(trim_l), seg_r)
            : (
              switch (d) {
              | Left => (ws(trim_l), seg_r)
              | Right => (seg_l, ws(trim_r))
              }
            ),
        )
      | (Some((_, g)), []) =>
        IdGen.return(
          Grout.fits_shape(g, s_r) ? (seg_l, seg_r) : (ws(trim_l), seg_r),
        )
      | (None, [g, ..._]) =>
        IdGen.return(
          Grout.fits_shape(g, s_l) ? (seg_l, seg_r) : (seg_l, ws(trim_r)),
        )
      | (None, []) =>
        Nib.Shape.fits(s_l, s_r)
          ? IdGen.return((seg_l, seg_r))
          // can modulate with directional arg
          : (
            switch (d) {
            | Left =>
              let+ trim = add_grout(s_r, trim_r);
              (seg_l, to_seg(trim));
            | Right =>
              let+ trim = add_grout(s_l, trim_l);
              (to_seg(trim), seg_r);
            }
          )
      };
    };
    (pre @ trim_l, trim_r @ suf);
  };
  {siblings, ancestors};
};

let prepend_generation = ((a, sibs): Ancestors.generation, rs: t): t => {
  siblings: Siblings.empty,
  ancestors: [(a, Siblings.concat([sibs, rs.siblings])), ...rs.ancestors],
};
let prepend_siblings = (sibs: Siblings.t, rs: t): t => {
  ...rs,
  siblings: Siblings.concat([sibs, rs.siblings]),
};

let concat = (rss: list(t)): t =>
  List.fold_right(
    (rs: t, cat: t) =>
      List.fold_right(prepend_generation, rs.ancestors, cat)
      |> prepend_siblings(rs.siblings),
    rss,
    empty,
  );

let reassemble_parent = (rs: t): t =>
  switch (rs.ancestors) {
  | [] => rs
  | [(a, sibs), ...ancs] =>
    let (l, r) =
      rs.siblings
      |> Siblings.split_by_matching(a.id)
      |> TupleUtil.map2(Aba.trim);
    let flatten_match =
      Aba.fold_right(
        (t: Tile.t, kid, (shards, kids)) =>
          Aba.mk(t.shards @ shards, t.children @ [kid, ...kids]),
        (t: Tile.t) => Aba.mk(t.shards, t.children),
      );
    let (a, l) =
      switch (l) {
      | None => (a, fst(rs.siblings))
      | Some((outer_l, match_l, inner_l)) =>
        let (shards_l, kids_l) = flatten_match(match_l);
        let a = {
          ...a,
          shards: a.shards |> PairUtil.map_fst(ss => ss @ shards_l),
          children:
            a.children
            |> PairUtil.map_fst(kids => kids @ [outer_l, ...kids_l]),
        };
        (a, inner_l);
      };
    let (a, r) =
      switch (r) {
      | None => (a, snd(rs.siblings))
      | Some((inner_r, match_r, outer_r)) =>
        let (shards_r, kids_r) = flatten_match(match_r);
        let a = {
          ...a,
          shards: a.shards |> PairUtil.map_snd(ss => shards_r @ ss),
          children:
            a.children
            |> PairUtil.map_snd(kids => [outer_r, ...kids_r] @ kids),
        };
        (a, inner_r);
      };
    {siblings: (l, r), ancestors: [(a, sibs), ...ancs]};
  };

let reassemble_siblings = (rs: t) => {
  ...rs,
  siblings: Siblings.reassemble(rs.siblings),
};

let reassemble = (rs: t): t => {
  let rec go = (rs: t): t =>
    switch (Segment.incomplete_tiles(snd(rs.siblings))) {
    | [] => rs
    | [t, ..._] =>
      switch (
        rs.siblings
        |> Siblings.split_by_matching(t.id)
        |> TupleUtil.map2(Aba.trim)
      ) {
      | (_, None) => failwith("impossible")
      | (None, Some((inner_r, match_r, outer_r))) =>
        let {siblings: (pre, suf), ancestors} =
          go({...rs, siblings: (fst(rs.siblings), outer_r)});
        let t = Tile.reassemble(match_r);
        let suf = Segment.concat([inner_r, [Tile.to_piece(t), ...suf]]);
        {siblings: (pre, suf), ancestors};
      | (
          Some((outer_l, match_l, inner_l)),
          Some((inner_r, match_r, outer_r)),
        ) =>
        let rs = go({...rs, siblings: (outer_l, outer_r)});
        let ancestors = [
          (Ancestor.reassemble(match_l, match_r), rs.siblings),
          ...rs.ancestors,
        ];
        let siblings = (inner_l, inner_r);
        {ancestors, siblings};
      }
    };
  rs |> reassemble_siblings |> reassemble_parent |> go;
};

// let rec reassemble = (rs: t): t => {
//   let siblings = Siblings.reassemble(rs.siblings);
//   switch (Siblings.incomplete_tiles(siblings)) {
//   | ([], _)
//   | (_, []) => {...rs, siblings}
//   | ([_, ..._], [t, ..._]) =>
//     switch (
//       siblings
//       |> Siblings.split_by_matching(t.id)
//       |> TupleUtil.map2(Aba.trim)
//     ) {
//     | (None, None) => {...rs, siblings}
//     | (None, Some((inner_r, match_r, outer_r))) =>
//       let {siblings: (l, r), ancestors} =
//         reassemble({...rs, siblings: (fst(siblings), outer_r)});
//       {
//         siblings: (
//           l,
//           Segment.concat([inner_r, Ancestor.Match.Suffix.join(match_r), r]),
//         ),
//         ancestors,
//       };
//     | (Some((inner_l, match_l, outer_l)), None) =>
//       let {siblings: (l, r), ancestors} =
//         reassemble({...rs, siblings: (outer_l, snd(rs.siblings))});
//       {
//         siblings: (
//           Segment.concat([inner_l, Ancestor.Match.Suffix.join(match_l), l]),
//           r,
//         ),
//         ancestors,
//       };
//     | (Some((inner_l, match_l, outer_l)), Some((inner_r, match_r, outer_r))) =>
//       let match = (match_l, match_r);
//       let rs_inner =
//         switch (Ancestor.Match.complete(match)) {
//         | None => {
//             siblings:
//               Siblings.concat([
//                 (inner_l, inner_r),
//                 Ancestor.Match.join(match),
//               ]),
//             ancestors: Ancestors.empty,
//           }
//         | Some(a) => {
//             siblings: (inner_l, inner_r),
//             ancestors: [(a, Siblings.empty)],
//           }
//         };
//       let rs_outer = reassemble({...rs, siblings: (outer_l, outer_r)});
//       concat([rs_inner, rs_outer]);
//     }
//   };
// };
