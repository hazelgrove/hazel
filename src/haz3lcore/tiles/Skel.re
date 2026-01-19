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
    && Mold.is_infix_op(t2.mold);
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

  let shapes = (p: Piece.t): (Nib.Shape.t, Nib.Shape.t) =>
    switch (p) {
    | Grout({shape: Concave, _}) => (
        Concave(Precedence.concave_grout),
        Concave(Precedence.concave_grout),
      )
    | _ => Piece.shapes(p) |> OptUtil.get_or_raise(Input_contains_secondary)
    };

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
        ~prec?,
        {
          shunted,
          output,
        },
      );
    };
  };

  let push_shunted = ((_, p) as ip: ip, stacks: t): t => {
    let (l, _) = shapes(p);
    let stacks =
      switch (l) {
      | Convex => stacks
      | Concave(prec) => push_output(~prec, stacks)
      };
    {
      ...stacks,
      shunted: [ip, ...stacks.shunted],
    };
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

let rec range = (skel: t): (int, int) =>
  switch (skel) {
  | Op(root) => (Aba.first_a(root), Aba.last_a(root))
  | Pre(root, skel) => (Aba.first_a(root), snd(range(skel)))
  | Post(skel, root) => (fst(range(skel)), Aba.last_a(root))
  | Bin(l, _, r) => (fst(range(l)), snd(range(r)))
  };
