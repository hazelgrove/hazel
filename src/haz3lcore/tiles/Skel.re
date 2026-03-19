open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Op(root)
  | Pre(root, t)
  | Post(t, root)
  | Bin(t, root, t)
and root = Aba.t(int, t);

let root =
  fun
  | Op(r)
  | Pre(r, _)
  | Post(_, r)
  | Bin(_, r, _) => r;

exception Input_contains_secondary;
exception Nonconvex_segment;

[@deriving show({with_path: false})]
type ip = (int, Piece.t);

// Chainable label constants (TODO: unhardcode)
let comma_label = [","];
let case_label = ["case", "end"];
let rule_label = ["|", "=>"];
let plus_label = ["+"];
let semi_label = [";"];

// Determines if two pieces can be chained together in the skeleton.
// Only returns true for operators that should form a single chain node.
let is_chainable = (p1: Piece.t, p2: Piece.t): bool =>
  switch (p1, p2) {
  | (Grout({shape: Concave, _}), Grout({shape: Concave, _})) => true
  | (Tile(t1), Tile(t2)) =>
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
  [@deriving show({with_path: false})]
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
        if (is_chainable(snd(hd), snd(p))) {
          pop_chain(~popped=[hd, ...popped], tl);
        } else {
          (popped, shunted);
        }
      }
    };

  let shapes = (~sort: Sort.t, p: Piece.t): (Nib.Shape.t, Nib.Shape.t) =>
    switch (p) {
    | Grout({shape: Concave, _}) =>
      /* In Mod/Sig sort context, grout acts like a module/sig semicolon
         (very loose) so that incomplete syntax between module items
         doesn't absorb surrounding items into one broken body. */
      let prec =
        switch (sort) {
        | Mod
        | Sig => Precedence.mod_seq
        | _ => Precedence.concave_grout
        };
      (Concave(prec), Concave(prec));
    | _ => Piece.shapes(p) |> OptUtil.get_or_raise(Input_contains_secondary)
    };

  let shapes_of_chain =
      (~sort: Sort.t, chain: list(ip)): option((Nib.Shape.t, Nib.Shape.t)) =>
    switch (chain, ListUtil.split_last_opt(chain)) {
    | ([(_, first), ..._], Some((_, (_, last)))) =>
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
      let is = List.map(fst, chain);
      let chain_len = List.length(chain);
      let split_kids = (n: int): (list(skel), list(skel)) =>
        try(ListUtil.split_n(n, stacks.output) |> PairUtil.map_fst(List.rev)) {
        | _ => failwith("Skel.push_output: split_kids: index out of bounds")
        };
      let output =
        switch (l, r) {
        | (Convex, Convex) =>
          let (kids, output) = split_kids(chain_len - 1);
          [Op(Aba.mk(is, kids)), ...output];
        | (Convex, Concave(_)) =>
          let (kids, output) = split_kids(chain_len);
          let (kids, r) = ListUtil.split_last(kids);
          [Pre(Aba.mk(is, kids), r), ...output];
        | (Concave(_), Convex) =>
          let (kids, output) = split_kids(chain_len);
          let (l, kids) = ListUtil.split_first(kids);
          [Post(l, Aba.mk(is, kids)), ...output];
        | (Concave(_), Concave(_)) =>
          let (kids, output) = split_kids(chain_len + 1);
          let (l, kids) = ListUtil.split_first(kids);
          let (kids, r) = ListUtil.split_last(kids);
          [Bin(l, Aba.mk(is, kids), r), ...output];
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

  let push_shunted = (~sort: Sort.t, (_, p) as ip: ip, stacks: t): t => {
    let (l, _) = shapes(~sort, p);
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

let rec range = (skel: t): (int, int) =>
  switch (skel) {
  | Op(root) => (Aba.first_a(root), Aba.last_a(root))
  | Pre(root, skel) => (Aba.first_a(root), snd(range(skel)))
  | Post(skel, root) => (fst(range(skel)), Aba.last_a(root))
  | Bin(l, _, r) => (fst(range(l)), snd(range(r)))
  };
