open Sexplib.Std;
open Util;

// todo: rename as Meld
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(option(kid), Piece.t)
[@deriving (show({with_path: false}), sexp, yojson)]
and kid =
  | K(t);

// we expect a kid to be constructed only when there is
// a concrete parent piece inducing kidhood, hence we should
// never encounter a chain consisting solely of Some(kid).
exception Orphaned_kid;

// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;

exception Missing_root;

let empty = Aba.singleton(None);
let is_empty: t => bool = (==)(empty);

let of_piece = (~l=?, ~r=?, p: Piece.t) => Aba.mk([l, r], [p]);
let of_grout = (~l=?, ~r=?, g: Grout.t) =>
  of_piece(~l?, ~r?, Piece.mk(G(g)));
let of_tile = (~l=?, ~r=?, t: Tile.t) =>
  of_piece(~l?, ~r?, Piece.mk(T(t)));

let root: t => list(Piece.t) = Aba.get_bs;
let kids: t => list(option(kid)) = Aba.get_as;

let rec to_lexemes = c =>
  c |> Aba.join(kid_to_lexemes, Piece.to_lexemes) |> List.concat
and kid_to_lexemes =
  fun
  | None => []
  | Some(K(c)) => to_lexemes(c);

[@warning "-27"]
let mold = (c: t, ~kid=?, t: Token.t) => failwith("todo Chain.mold");
// precond: root(c) != []
let sort = _ => failwith("todo sort");
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let expected_sort = (_side: Dir.t, _) =>
  failwith("todo Chain.expected_sort");

let match_ = (_, ~kid as _=?, _) => failwith("todo match_");

module Padded = {
  type c = t;
  // chain with padding (ie single-chain segment)
  type t = (c, (Space.t, Space.t));
  let mk = (~l=Space.empty, ~r=Space.empty, c: c): t => (c, (l, r));
  let empty = (~l=Space.empty, ~r=Space.empty, ()) => mk(~l, ~r, empty);
  let is_empty = ((c, (l, r))) => is_empty(c) ? None : Some(l @ r);
  let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
    c,
    (l @ l', r' @ r),
  );
};

let cmp = (_: t, _: t): Cmp.t => failwith("todo cmp");

// precond: root(par) != []
// precond: kid convexified
// let push_kid_l = (kid: Padded.t, par: Padded.t): Padded.t => {
//   let (c_kid, (l_kid, r_kid)) = kid;
//   let (c_par, (l_par, r_par)) = par;
//   let (k, p, c) = Aba.uncons(c_par) |> OptUtil.get_or_raise(Missing_root);
//   // todo: consider relaxing this and merging with input kid
//   assert(k == None);
//   let p = Piece.pad(~l=l_par @ r_kid, p);
//   let c = Aba.cons(Some(K(c_kid)), p, c);
//   Padded.mk(~l=l_kid, ~r=r_par, c);
// };
// // precond: root(par) != []
// // precond: kid convexified
// let push_kid_r = (par: Padded.t, kid: Padded.t): Padded.t => {
//   let (c_par, (l_par, r_par)) = par;
//   let (c_kid, (l_kid, r_kid)) = kid;
//   let (c, p, k) = Aba.unsnoc(c_par) |> OptUtil.get_or_raise(Missing_root);
//   // todo: consider relaxing this and merging with input kid
//   assert(k == None);
//   let p = Piece.pad(~r=r_par @ l_kid, p);
//   let c = Aba.snoc(c, p, Some(K(c_kid)));
//   Padded.mk(~l=l_par, ~r=r_kid, c);
// };

let convexify_l = (~expected, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected));
  } else {
    let (kid, p, tl) = Aba.uncons(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.expected_sort(L, p)) {
    | None =>
      assert(kid == None);
      c;
    | Some(s) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Aba.cons(Some(K(kid)), p, tl);
      }
    };
  };
let convexify_r = (~expected, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected));
  } else {
    let (tl, p, kid) = Aba.unsnoc(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.expected_sort(R, p)) {
    | None =>
      assert(kid == None);
      c;
    | Some(s) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Aba.snoc(tl, p, Some(K(kid)));
      }
    };
  };
let convexify = (~expected: Sort.t, c) =>
  c |> convexify_r(~expected) |> convexify_l(~expected);

let rec is_porous = c =>
  c
  |> Aba.join(kid_is_porous, Piece.is_porous)
  |> OptUtil.sequence
  |> Option.map(List.concat)
and kid_is_porous =
  fun
  | None => Some(Space.empty)
  | Some(K(kid)) => is_porous(kid);

// precond: l and r are nonempty
let rec fills = (l: Padded.t, r: Padded.t): option(Padded.t) => {
  let (c_l, (s_ll, s_lr)) = l;
  let (c_r, (s_rl, s_rr)) = r;
  let (c_l', p_l, kid_l) =
    Aba.unsnoc(c_l) |> OptUtil.get_or_raise(Orphaned_kid);
  let (kid_r, p_r, c_r') =
    Aba.uncons(c_r) |> OptUtil.get_or_raise(Orphaned_kid);
  open OptUtil.Syntax;
  let* fp = Piece.fills_or_passes(p_l, p_r);
  switch (fp) {
  | Fill(d) =>
    let+ s_l = kid_is_porous(kid_l)
    and+ s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_l, s_lr, s_rl, s_r]);
    let p =
      switch (d) {
      | L => Piece.pad(~l=Piece.space(p_l) @ s_mid, p_r)
      | R => Piece.pad(~r=s_mid @ Piece.space(p_r), p_l)
      };
    Padded.mk(~l=s_ll, ~r=s_rr, Aba.append(c_l', p, c_r'));
  | Pass(L) =>
    let* s_l = kid_is_porous(kid_l);
    let s_mid = List.concat([Piece.space(p_l), s_l, s_lr]);
    let l = Padded.mk(~l=s_ll, ~r=s_mid, c_l');
    fills(l, r);
  | Pass(R) =>
    let* s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_rl, s_r, Piece.space(p_l)]);
    let r = Padded.mk(~l=s_mid, ~r=s_rr, c_r');
    fills(l, r);
  };
};

let rec merge = (l: Padded.t, r: Padded.t): Padded.t =>
  switch (Padded.is_empty(l), Padded.is_empty(r)) {
  | (Some(l), Some(r)) => Padded.empty(~l, ~r, ())
  | (Some(l), None) => Padded.pad(~l, r)
  | (None, Some(r)) => Padded.pad(~r, l)
  | (None, None) => merge_nonempty(l, r)
  }
and merge_nonempty = (l, r) =>
  switch (fills(l, r)) {
  | Some(pc) => pc
  | None =>
    let (c_l, (s_ll, s_lr)) = l;
    let (c_r, (s_rl, s_rr)) = r;
    let (tl_l, p_l, kid_l) =
      Aba.unsnoc(c_l) |> OptUtil.get_or_raise(Orphaned_kid);
    let (kid_r, p_r, tl_r) =
      Aba.uncons(c_r) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.cmp(p_l, p_r)) {
    | In () =>
      assert(kid_l == None && kid_r == None);
      Piece.(mk(G(Grout.mk_concave(mold(p_l), mold(p_r)))))
      |> Piece.pad(~l=s_lr, ~r=s_rl)
      |> of_piece(~l=?kid_l, ~r=?kid_r)
      |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Lt(expected) =>
      assert(kid_l == None);
      let p_l = Piece.pad(~r=s_lr @ s_rl, p_l);
      let r = mk_kid(~expected, c_r);
      Aba.snoc(tl_l, p_l, Some(r)) |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Eq(expected) =>
      let (kid, (l, r)) = merge_kids(~expected, kid_l, s_lr, s_rl, kid_r);
      let p_l = Piece.pad(~r=l, p_l);
      let p_r = Piece.pad(~l=r, p_r);
      Aba.append(tl_l, p_l, Aba.cons(Some(K(kid)), p_r, tl_r))
      |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Gt(expected) =>
      assert(kid_l == None);
      let p_r = Piece.pad(~l=s_lr @ s_rl, p_r);
      let l = mk_kid(~expected, c_l);
      Aba.cons(Some(l), p_r, tl_r) |> Padded.mk(~l=s_ll, ~r=s_rr);
    };
  }
and merge_kids =
    (
      ~expected: Sort.t,
      l: option(kid),
      s_l: Space.t,
      s_r: Space.t,
      r: option(kid),
    )
    : Padded.t =>
  switch (l, r) {
  | (None, None) =>
    of_grout(Grout.mk_convex(expected)) |> Padded.mk(~l=s_l, ~r=s_r)
  | (None, Some(K(r))) => Padded.mk(~l=s_l @ s_r, r)
  | (Some(K(l)), None) => Padded.mk(~r=s_l @ s_r, l)
  | (Some(K(l)), Some(K(r))) =>
    let l = Padded.mk(~r=s_l, l);
    let r = Padded.mk(~l=s_r, r);
    merge(l, r);
  }
and mk_kid = (~expected: Sort.t, c: t): kid => K(convexify(~expected, c));

let merge_all = pcs => List.fold_right(merge, pcs, Padded.empty());

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");

let cmp_merge = (l: t, ~kid=Padded.empty(), r: t): Cmp.Result.s(Padded.t) =>
  switch (cmp(l, r)) {
  | In =>
    let (c, (s_l, s_r)) = kid;
    assert(is_empty(c));
    In(merge(Padded.mk(~r=s_l, l), Padded.mk(~l=s_r, r)));
  | Lt => Lt(merge(kid, Padded.mk(r)))
  | Eq => Eq(merge_all([Padded.mk(l), kid, Padded.mk(r)]))
  | Gt => Gt(merge(Padded.mk(l), kid))
  };

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, _) => failwith("todo pop_lexeme");

// todo: probably want to replace with lexeme
[@warning "-27"]
let pop_token = (~from: Dir.t, _) => failwith("todo pop_token");

// let tip_l = (c: t) => {
//   let (kid, p) =
//     Aba.first_ab(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(L, p)
//   };
// };
// let tip_l = (c: t) => {
//   let (p, kid) = Aba.lab_ba(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(R, p)
//   };
// };
// let tip = (d: Dir.t): (t => Tip.t) =>
//   switch (d) {
//   | L => tip_l
//   | R => tip_r
//   };

// let merge = (l: t, r: t): t =>
// switch (Chain.tip(R, c), Chain.tip(L, hd)) {
// | (Convex, Convex) => raise(Nonmonotonic)
// | (Convex, Concave(_)) =>
// }
