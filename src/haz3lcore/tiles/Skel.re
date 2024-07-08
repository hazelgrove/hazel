open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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

exception Input_contains_secondary;
exception Nonconvex_segment;

[@deriving show({with_path: false})]
type ip = (int, Piece.t);

type rel =
  | Lt
  | Eq
  | Gt;

let rel = (p1: Piece.t, p2: Piece.t): option(rel) =>
  switch (p1, p2) {
  | (Secondary(_), _)
  | (_, Secondary(_)) => None
  | (Grout({shape, _}), _) =>
    switch (shape) {
    | Convex => Some(Gt)
    | Concave => Some(Lt)
    }
  | (_, Grout({shape, _})) =>
    switch (shape) {
    | Convex => Some(Lt)
    | Concave => Some(Gt)
    }
  | (Tile(t1), Tile(t2)) =>
    open Labels;
    let lbl1 = (==)(t1.label);
    let lbl2 = (==)(t2.label);
    let eq =
      [
        lbl1(case) && lbl2(rule),
        lbl1(rule) && lbl2(rule),
        lbl1(comma) && lbl2(comma) && t1.mold == t2.mold,
        lbl1(["+"]) && lbl2(["+"]) && t1.mold == t2.mold,
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
      | (Concave(p), Concave(p')) =>
        if (p < p') {
          Some(Lt);
        } else if (p > p') {
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
        }
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
    Piece.shapes(p) |> OptUtil.get_or_raise(Input_contains_secondary);

  let shapes_of_chain =
      (chain: list(ip)): option((Nib.Shape.t, Nib.Shape.t)) =>
    switch (chain, ListUtil.split_last_opt(chain)) {
    | ([(_, first), ..._], Some((_, (_, last)))) =>
      let (l, _) = shapes(first);
      let (_, r) = shapes(last);
      Some((l, r));
    | _ => None
    };

  let rec push_output = (~prec: option(Precedence.t)=?, stacks: t): t => {
    let (chain, shunted) = pop_chain(stacks.shunted);
    switch (prec, shapes_of_chain(chain)) {
    | (Some(prec), Some((_, Concave(prec'))))
        when
          Precedence.compare(prec', prec) < 0
          || Precedence.compare(prec', prec) == 0
          && Precedence.associativity(prec') != Some(Left) => stacks
    | (_, None) => stacks
    | (_, Some((l, r))) =>
      let is = List.map(fst, chain);
      let split_kids = n =>
        try(ListUtil.split_n(n, stacks.output) |> PairUtil.map_fst(List.rev)) {
        | _ =>
          print_endline(show(stacks));
          failwith("Skel.push_output: split_kids: index out of bounds");
        };
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
      push_output(~prec?, {shunted, output});
    };
  };

  let push_shunted = ((_, p) as ip: ip, stacks: t): t => {
    let (l, _) = shapes(p);
    let stacks =
      switch (l) {
      | Convex => stacks
      | Concave(prec) => push_output(~prec, stacks)
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
