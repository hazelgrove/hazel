open Util;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Op(root)
  | Pre(root, t)
  | Post(t, root)
  | Bin(t, root, t)
and root = Aba.t(int, t);

// let rec size =
//   fun
//   | Op(_) => 1
//   | Pre(_, r) => 1 + size(r)
//   | Post(l, _) => size(l) + 1
//   | Bin(l, _, r) => size(l) + 1 + size(r);

// TODO(d): rename to reflect aba
let root =
  fun
  | Op(r)
  | Pre(r, _)
  | Post(_, r)
  | Bin(_, r, _) => r;

// let children =
//   fun
//   | Op(_) => []
//   | Pre(_, skel) => [(Direction.Right, skel)]
//   | Post(skel, _) => [(Left, skel)]
//   | Bin(l, _, r) => [(Left, l), (Right, r)];

// returns inclusive lower bound, exclusive upper bound
// let rec range =
//   fun
//   | Op(n) => (n, n + 1)
//   | Pre(n, r) => (n, snd(range(r)))
//   | Post(l, n) => (fst(range(l)), n + 1)
//   | Bin(l, _, r) => (fst(range(l)), snd(range(r)));

// let rec skel_at = (n, skel) =>
//   switch (skel) {
//   | Op(m) => n == m ? skel : raise(Invalid_argument("Skel.skel_at"))
//   | Pre(m, r) => n == m ? skel : skel_at(n, r)
//   | Post(l, m) => n == m ? skel : skel_at(n, l)
//   | Bin(l, m, r) =>
//     if (n < m) {
//       skel_at(n, l);
//     } else if (n > m) {
//       skel_at(n, r);
//     } else {
//       skel;
//     }
//   };

exception Input_contains_whitespace;
exception Nonconvex_segment;

[@deriving show({with_path: false})]
type ip = (int, Piece.t);

let rel = (p1: Piece.t, p2: Piece.t): option(Ord.t) =>
  switch (p1, p2) {
  | (Whitespace(_), _)
  | (_, Whitespace(_)) => None
  | (
      Grout({shape: Concave, sort: s1, _}),
      Grout({shape: Concave, sort: s2, _}),
    ) =>
    Sort.ord(s1, s2) |> Option.map(Ord.flip)
  | (Grout({shape: Concave, sort, _}), Tile(t)) when !Tile.has_end(Left, t) =>
    assert(Nib.Shape.is_concave(fst(Tile.shapes(t))));
    Sort.ord(sort, t.mold.out) |> Option.map(Ord.flip);
  | (Tile(t), Grout({shape: Concave, sort, _})) when !Tile.has_end(Right, t) =>
    assert(Nib.Shape.is_concave(snd(Tile.shapes(t))));
    Sort.ord(t.mold.out, sort) |> Option.map(Ord.flip);
  | (Grout({shape, sort, _}), _) =>
    let sort' = Option.get(Piece.sort(p2));
    switch (Sort.ord(sort, sort') |> Option.map(Ord.flip)) {
    | None
    | Some(Eq) =>
      switch (shape) {
      | Convex => Some(Gt)
      | Concave => Some(Lt)
      }
    | r => r
    };
  | (_, Grout({shape, sort, _})) =>
    let sort' = Option.get(Piece.sort(p1));
    switch (Sort.ord(sort', sort) |> Option.map(Ord.flip)) {
    | None
    | Some(Eq) =>
      switch (shape) {
      | Convex => Some(Lt)
      | Concave => Some(Gt)
      }
    | r => r
    };
  | (Tile(t1), Tile(t2)) when Sort.ord(t1.mold.out, t2.mold.out) == Some(Lt) =>
    Some(Gt)
  | (Tile(t1), Tile(t2)) when Sort.ord(t1.mold.out, t2.mold.out) == Some(Gt) =>
    Some(Lt)
  | (Tile(t1), Tile(t2)) =>
    open Labels;
    let lbl1 = (==)(t1.label);
    let lbl2 = (==)(t2.label);
    let eq =
      [
        lbl1(case) && lbl2(rule),
        lbl1(rule) && lbl2(rule),
        lbl1(comma) && lbl2(comma),
      ]
      |> List.fold_left((||), false);
    if (eq) {
      Some(Eq);
    } else {
      let (_, r1) = Tile.shapes(t1);
      let (l2, _) = Tile.shapes(t2);
      switch (r1, l2) {
      | (Convex, Convex) => None
      | (Concave(_), Convex) => Some(Lt)
      | (Convex, Concave(_)) => Some(Gt)
      | (Concave({prec: p, _}), Concave({prec: p', _})) =>
        let c = Precedence.compare(p, p');
        if (c < 0) {
          Some(Lt);
        } else if (c > 0) {
          Some(Gt);
        } else {
          switch (Precedence.associativity(p)) {
          | Some(Left) => Some(Gt)
          | Some(Right) => Some(Lt)
          | None =>
            // may want to make this Some(Eq)
            // for things like comma but don't
            // want to bother with mold concerns here
            None
          };
        };
      };
    };
  };

module Stacks = {
  [@deriving show({with_path: false})]
  type skel = t;
  [@deriving show({with_path: false})]
  type t = {
    output: list(skel),
    shunted: list(ip),
  };

  let empty = {output: [], shunted: []};

  let rec pop_chain =
          (~popped=[], shunted: list(ip)): (list(ip), list(ip)) =>
    switch (shunted) {
    | [] => (popped, shunted)
    | [hd, ...tl] =>
      switch (popped) {
      | [] => pop_chain(~popped=[hd], tl)
      | [p, ..._] =>
        switch (rel(snd(hd), snd(p))) {
        | Some(Eq) => pop_chain(~popped=[hd, ...popped], tl)
        | _ => (popped, shunted)
        }
      }
    };

  let shapes = p =>
    Piece.shapes(p) |> OptUtil.get_or_raise(Input_contains_whitespace);

  let ends_of_chain = (chain: list(ip)): option((ip, ip)) =>
    switch (chain, ListUtil.split_last_opt(chain)) {
    | ([first, ..._], Some((_, last))) => Some((first, last))
    | _ => None
    };

  let rec push_output = (~hd: option(ip)=?, stacks: t): t => {
    let (chain, shunted) = pop_chain(stacks.shunted);
    switch (hd, ends_of_chain(chain)) {
    | (_, None) => stacks
    | (Some((_, hd)), Some((_, (_, r)))) when rel(r, hd) != Some(Gt) => stacks
    | (_, Some(((_, l), (_, r)))) =>
      let is = List.map(fst, chain);
      let split_kids = n =>
        ListUtil.split_n(n, stacks.output) |> PairUtil.map_fst(List.rev);
      let (l, _) = shapes(l);
      let (_, r) = shapes(r);
      let output =
        switch (l, r) {
        | (Convex, Convex) =>
          let (kids, output) = split_kids(List.length(chain) - 1);
          [Op(Aba.mk(is, kids)), ...output];
        | (Convex, Concave(_)) =>
          let (kids, output) = split_kids(List.length(chain));
          let (kids, r) = ListUtil.split_last(kids);
          [Pre(Aba.mk(is, kids), r), ...output];
        | (Concave(_), Convex) =>
          let (kids, output) = split_kids(List.length(chain));
          let (l, kids) = ListUtil.split_first(kids);
          [Post(l, Aba.mk(is, kids)), ...output];
        | (Concave(_), Concave(_)) =>
          let (kids, output) = split_kids(List.length(chain) + 1);
          let (l, kids) = ListUtil.split_first(kids);
          let (kids, r) = ListUtil.split_last(kids);
          [Bin(l, Aba.mk(is, kids), r), ...output];
        };
      push_output(~hd?, {shunted, output});
    };
  };

  let push_shunted = ((_, p) as ip: ip, stacks: t): t => {
    let (l, _) = shapes(p);
    let stacks =
      switch (l) {
      | Convex => stacks
      | Concave(_) => push_output(~hd=ip, stacks)
      };
    {...stacks, shunted: [ip, ...stacks.shunted]};
  };

  let finish = stacks => push_output(stacks);
};

let mk = (seg: list(ip)): t => {
  let stacks =
    seg
    |> List.fold_left(Fun.flip(Stacks.push_shunted), Stacks.empty)
    |> Stacks.finish;
  ListUtil.hd_opt(stacks.output) |> OptUtil.get_or_raise(Nonconvex_segment);
};
