open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let empty = {siblings: Siblings.empty, ancestors: Ancestors.empty};

let nibs = ({siblings: (pre, suf), ancestors}: t) => {
  let (l, r) = Ancestors.inner_nibs(ancestors);
  let l =
    switch (Segment.hard_nib(Right, pre)) {
    | None => l
    | Some(l) => l
    };
  let r =
    switch (Segment.hard_nib(Left, suf)) {
    | None => r
    | Some(r) => r
    };
  (l, r);
};

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

let pop_trim = (d: Direction.t, rs: t): (Segment.Trim.t, t) => {
  open Segment.Trim;
  let rec go = rs =>
    switch (pop(d, rs)) {
    | Some((Whitespace(w), rs)) =>
      let (trim, rs) = go(rs);
      (cons_w(w, trim), rs);
    | Some((Grout(g), rs)) =>
      let (trim, rs) = go(rs);
      (cons_g(g, trim), rs);
    | Some((Tile(_), _)) => (empty, rs)
    | None => (empty, rs)
    };
  let (trim, rs) = go(rs);
  d == Left ? (rev(trim), rs) : (trim, rs);
};

let push_trim = (d: Direction.t, trim, rs: t): t =>
  prepend(d, Segment.Trim.to_seg(trim), rs);

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

let remold = ({siblings, ancestors}: t): t => {
  let s = Ancestors.sort(ancestors);
  let siblings = Siblings.remold(siblings, s);
  {ancestors, siblings};
};

let regrout = (_d: Direction.t, {siblings, ancestors}: t): IdGen.t(t) => {
  open IdGen.Syntax; /* Direction is side of grout caret will end up on */

  // let* ancestors = Ancestors.regrout(ancestors);
  let s = Ancestors.sort(ancestors);
  let nibs = Ancestors.inner_nibs(ancestors);
  let+ siblings = {
    let* ((pre, s_l, trim_l), (trim_r, s_r, suf)) =
      Siblings.regrout(siblings, nibs, s);
    let caret = Segment.Trim.length(trim_l);
    let trim = Segment.Trim.append(trim_l, trim_r);
    let+ (caret, trim) =
      Segment.Trim.regrout(~lint=false, ~caret, (s_l, s_r), trim, s);
    let trim_seg = Segment.Trim.to_seg(trim);
    let (trim_l, trim_r) =
      switch (trim_seg |> ListUtil.split_n_opt(caret)) {
      | None => ([], trim_seg)
      | Some(p) => p
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

module Backpack = {
  module Stack = {
    type t = list(Token.t);
    let empty = [];
  };

  type t = {
    // obligations from the left
    l: Stack.t,
    // obligations from the right
    r: Stack.t,
    // obligations from both left and right
    b: Stack.t,
  };

  let empty = Stack.{l: empty, r: empty, b: empty};

  let push_from_sib = (d: Direction.t, t: Tile.t, bp: t): t => {
    let toks = Tile.complete(Direction.toggle(d), t);
    switch (d) {
    | Left => {...bp, l: toks @ bp.l}
    | Right => {...bp, r: toks @ bp.r}
    };
  };

  let push_from_pre = (ts: list(Tile.t), bp: t) =>
    List.fold_left(Fun.flip(push_from_sib(Left)), bp, ts);
  let push_from_suf = (ts: list(Tile.t), bp: t) =>
    List.fold_right(push_from_sib(Right), ts, bp);

  let push_from_anc = (a: Ancestor.t, bp: t): t => {
    ...bp,
    b: Ancestor.complete(a),
  };
};

// assumes relatives are maximally assembled
let mk_backpack = (rs: t): Backpack.t => {
  open Backpack;
  let (l, r) = rs.siblings;
  let push_from_anc =
    switch (rs.ancestors) {
    | [] => Fun.id
    | [(a, _), ..._] => push_from_anc(a)
    };
  push_from_anc(empty)
  |> push_from_pre(Segment.incomplete_tiles(l))
  |> push_from_suf(Segment.incomplete_tiles(r));
};

let _reassemble = (rs: t): t => {
  let rec go_l = (split_l, split_r, rs: t): t =>
    switch (Aba.uncons(split_l)) {
    | None =>
      let l = Segment.Split.flatten(split_l);
      let siblings = Siblings.concat([(l, []), rs.siblings]);
      {...rs, siblings};
    | Some((seg_l, t_l, split_l)) =>
      let rec go_r = (split, rs): t =>
        switch (Aba.unsnoc(split)) {
        | None =>
          let inner = (seg_l @ [Piece.Tile(t_l)], []);
          let siblings = Siblings.concat([inner, rs.siblings]);
          go_l(split_l, split_r, {...rs, siblings});
        | Some((split_r, t_r, seg_r)) =>
          // TODO review matches
          if (Segment.matches(t_l, t_r)) {
            let a =
              Ancestor.{
                id: t_l.id,
                label: t_l.label,
                mold: t_l.mold,
                shards: (t_l.shards, t_r.shards),
                children: (t_l.children, t_r.children),
              };
            let sibs = Siblings.concat([(seg_l, seg_r), rs.siblings]);
            let rs = {
              siblings: ([], []),
              ancestors: [(a, sibs), ...rs.ancestors],
            };
            go_l(split_l, split_r, rs);
          } else {
            let siblings =
              Siblings.concat([([], [Tile(t_r), ...seg_r]), rs.siblings]);
            go_r(split_r, {...rs, siblings});
          }
        };
      go_r(split_r, rs);
    };
  let (l, r) = rs.siblings;
  let (split_l, split_r) = Segment.Split.(reassemble(l), reassemble(r));
  go_l(split_l, split_r, {...rs, siblings: ([], [])});
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
