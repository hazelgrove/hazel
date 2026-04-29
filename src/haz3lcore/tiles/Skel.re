open Util;

/* A piece_ref identifies a position in the skeleton: either a real
   piece in the segment (by index) or a structural hole (by shape).
   Hole(Convex) = missing operand, Hole(Concave(prec)) = missing operator. */
[@deriving (show({with_path: false}), sexp, yojson)]
type piece_ref =
  | Piece(int)
  | Hole(Nib.Shape.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Op(root)
  | Pre(root, t)
  | Post(t, root)
  | Bin(t, root, t)
and root = Aba.t(piece_ref, t);

let root =
  fun
  | Op(r)
  | Pre(r, _)
  | Post(_, r)
  | Bin(_, r, _) => r;

exception Input_contains_secondary;
exception Nonconvex_segment;

/* Internal shunting-yard item: either a real piece or a hole marker */
type ip =
  | PieceIP(int, Piece.t)
  | HoleIP(Nib.Shape.t);

let ip_ref =
  fun
  | PieceIP(idx, _) => Piece(idx)
  | HoleIP(shape) => Hole(shape);

// Chainable label constants (TODO: unhardcode)
let comma_label = [","];
let case_label = ["case", "end"];
let rule_label = ["|", "=>"];
let plus_label = ["+"];
let semi_label = [";"];

// Determines if two items can be chained together in the skeleton.
// Only returns true for operators that should form a single chain node.
let is_chainable = (ip1: ip, ip2: ip): bool =>
  switch (ip1, ip2) {
  | (HoleIP(Concave(_)), HoleIP(Concave(_))) =>
    /* Concave holes chain with each other */
    true
  | (PieceIP(_, Tile(t1)), PieceIP(_, Tile(t2))) =>
    let lbl1 = (==)(t1.label);
    let lbl2 = (==)(t2.label);
    lbl1(case_label)
    && lbl2(rule_label)
    || lbl1(rule_label)
    && lbl2(rule_label)
    || lbl1(comma_label)
    && lbl2(comma_label)
    && Mold.is_infix_op(t1.mold)
    && Mold.is_infix_op(t2.mold)
    || lbl1(plus_label)
    && lbl2(plus_label)
    && Mold.is_infix_op(t1.mold)
    && Mold.is_infix_op(t2.mold)
    || lbl1(semi_label)
    && lbl2(semi_label)
    && Mold.is_infix_op(t1.mold)
    && Mold.is_infix_op(t2.mold)
    && t1.mold.out == Sort.Mod
    && t2.mold.out == Sort.Mod
    || lbl1(semi_label)
    && lbl2(semi_label)
    && Mold.is_infix_op(t1.mold)
    && Mold.is_infix_op(t2.mold)
    && t1.mold.out == Sort.Sig
    && t2.mold.out == Sort.Sig;
  | _ => false
  };

module Stacks = {
  [@deriving show({with_path: false})]
  type skel = t;
  type t = {
    output: list(skel),
    shunted: list(ip),
  };

  let empty = {
    output: [],
    shunted: [],
  };

  let rec pop_chain =
          (~popped=[], shunted: list(ip)): (list(ip), list(ip)) =>
    switch (shunted) {
    | [] => (popped, shunted)
    | [hd, ...tl] =>
      switch (popped) {
      | [] => pop_chain(~popped=[hd], tl)
      | [p, ..._] =>
        if (is_chainable(hd, p)) {
          pop_chain(~popped=[hd, ...popped], tl);
        } else {
          (popped, shunted);
        }
      }
    };

  let shapes = (~sort: Sort.t, ip: ip): (Nib.Shape.t, Nib.Shape.t) =>
    switch (ip) {
    | HoleIP(Convex) => (Convex, Convex)
    | HoleIP(Concave(_)) =>
      let prec =
        switch (sort) {
        | Mod
        | Sig => Precedence.mod_seq
        | _ => Precedence.concave_hole
        };
      (Concave(prec), Concave(prec));
    | PieceIP(_, p) =>
      Piece.shapes(p) |> OptUtil.get_or_raise(Input_contains_secondary)
    };

  let shapes_of_chain =
      (~sort: Sort.t, chain: list(ip)): option((Nib.Shape.t, Nib.Shape.t)) =>
    switch (chain, ListUtil.split_last_opt(chain)) {
    | ([first, ..._], Some((_, last))) =>
      let (l, _) = shapes(~sort, first);
      let (_, r) = shapes(~sort, last);
      Some((l, r));
    | _ => None
    };

  let rec push_output =
          (~sort: Sort.t, ~prec: option(Precedence.t)=?, stacks: t): t => {
    let (chain, shunted) = pop_chain(stacks.shunted);
    switch (prec, shapes_of_chain(~sort, chain)) {
    | (Some(prec), Some((_, Concave(prec'))))
        when
          Precedence.compare(prec', prec) < 0
          || Precedence.compare(prec', prec) == 0
          && Precedence.associativity(prec') != Some(Left) => stacks
    | (_, None) => stacks
    | (_, Some((l, r))) =>
      let refs = List.map(ip_ref, chain);
      let chain_len = List.length(chain);
      let split_kids = (n: int): (list(skel), list(skel)) =>
        try(ListUtil.split_n(n, stacks.output) |> PairUtil.map_fst(List.rev)) {
        | _ => failwith("Skel.push_output: split_kids: index out of bounds")
        };
      let output =
        switch (l, r) {
        | (Convex, Convex) =>
          let (kids, output) = split_kids(chain_len - 1);
          [Op(Aba.mk(refs, kids)), ...output];
        | (Convex, Concave(_)) =>
          let (kids, output) = split_kids(chain_len);
          let (kids, r) = ListUtil.split_last(kids);
          [Pre(Aba.mk(refs, kids), r), ...output];
        | (Concave(_), Convex) =>
          let (kids, output) = split_kids(chain_len);
          let (l, kids) = ListUtil.split_first(kids);
          [Post(l, Aba.mk(refs, kids)), ...output];
        | (Concave(_), Concave(_)) =>
          let (kids, output) = split_kids(chain_len + 1);
          let (l, kids) = ListUtil.split_first(kids);
          let (kids, r) = ListUtil.split_last(kids);
          [Bin(l, Aba.mk(refs, kids), r), ...output];
        };
      push_output(
        ~sort,
        ~prec?,
        {
          shunted,
          output,
        },
      );
    };
  };

  let push_shunted = (~sort: Sort.t, ip: ip, stacks: t): t => {
    let (l, _) = shapes(~sort, ip);
    let stacks =
      switch (l) {
      | Convex => stacks
      | Concave(prec) => push_output(~sort, ~prec, stacks)
      };
    {
      ...stacks,
      shunted: [ip, ...stacks.shunted],
    };
  };

  let finish = (~sort: Sort.t, stacks) => push_output(~sort, stacks);
};

let mk = (~sort=Sort.Exp, seg: list(ip)): t => {
  let stacks =
    seg
    |> List.fold_left(Fun.flip(Stacks.push_shunted(~sort)), Stacks.empty)
    |> Stacks.finish(~sort);
  ListUtil.hd_opt(stacks.output) |> OptUtil.get_or_raise(Nonconvex_segment);
};

/* Get the index range (leftmost, rightmost) of real pieces in a skel.
   Returns None if the skel contains only holes (no real pieces). */
let rec range = (skel: t): option((int, int)) => {
  let merge = (a, b) =>
    switch (a, b) {
    | (None, x)
    | (x, None) => x
    | (Some((l1, r1)), Some((l2, r2))) =>
      Some((min(l1, l2), max(r1, r2)))
    };
  let root_range = root => {
    let refs = Aba.get_as(root);
    let idx_range =
      List.filter_map(
        fun
        | Piece(i) => Some(i)
        | Hole(_) => None,
        refs,
      )
      |> (
        fun
        | [] => None
        | [first, ..._] as idxs => Some((first, ListUtil.last(idxs)))
      );
    List.fold_left(
      (acc, kid) => merge(acc, range(kid)),
      idx_range,
      Aba.get_bs(root),
    );
  };
  switch (skel) {
  | Op(root) => root_range(root)
  | Pre(root, child) => merge(root_range(root), range(child))
  | Post(child, root) => merge(range(child), root_range(root))
  | Bin(l, root, r) => merge(merge(range(l), root_range(root)), range(r))
  };
};
