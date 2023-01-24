open Sexplib.Std;
open Util;

// todo: rename as Meld
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(option(kid), Piece.t)
[@deriving (show({with_path: false}), sexp, yojson)]
and kid =
  | K(t);

// we expect a kid to be constructed only when there is
// a concrete parent piece inducing kidhood, hence we should
// never encounter a meld consisting solely of Some(kid).
exception Orphaned_kid;

// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;

exception Missing_root;

let empty = Chain.singleton(None);
let is_empty: t => bool = (==)(empty);

let of_piece = (~l=?, ~r=?, p: Piece.t) => Chain.mk([l, r], [p]);
let of_grout = (~l=?, ~r=?, g: Grout.t) =>
  of_piece(~l?, ~r?, Piece.mk(G(g)));
let of_tile = (~l=?, ~r=?, t: Tile.t) =>
  of_piece(~l?, ~r?, Piece.mk(T(t)));

let root: t => list(Piece.t) = Chain.links;
let kids: t => list(option(kid)) = Chain.loops;

let rec to_lexemes = c =>
  c |> Chain.to_list(kid_to_lexemes, Lexeme.s_of_piece) |> List.concat
and kid_to_lexemes =
  fun
  | None => []
  | Some(K(c)) => to_lexemes(c);

[@warning "-27"]
let mold = (c: t, ~kid=?, t: Token.t) => failwith("todo Meld.mold");
// precond: root(c) != []
let sort = _ => failwith("todo sort");
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let expected_sort = (_side: Dir.t, _) => failwith("todo Meld.expected_sort");

let match_ = (_, ~kid as _=?, _) => failwith("todo match_");

module Padded = {
  type c = t;
  // meld with padding (ie single-meld segment)
  type t = (c, (Space.s, Space.s));
  let mk = (~l=Space.empty, ~r=Space.empty, c: c): t => (c, (l, r));
  let empty = (~l=Space.empty, ~r=Space.empty, ()) => mk(~l, ~r, empty);
  let is_empty = ((c, (l, r))) => is_empty(c) ? None : Some(l @ r);
  let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
    c,
    (l @ l', r' @ r),
  );
};

// todo: consider generalizing to return expected sort
let cmp = (l: t, r: t): Cmp.t =>
  switch (Chain.unknil(l), Chain.unlink(r)) {
  | (None, None) => Eq()
  | (None, Some(_)) => Gt()
  | (Some(_), None) => Lt()
  | (Some((_, p_l, _)), Some((_, p_r, _))) =>
    Cmp.t_of_r(Piece.cmp(p_l, p_r))
  };

let convexify_l = (~expected: Sort.Ana.t, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (kid, p, tl) =
      Chain.unlink(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(L, p)) {
    | Convex =>
      assert(kid == None);
      c;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Chain.link(Some(K(kid)), p, tl);
      }
    };
  };
let convexify_r = (~expected: Sort.Ana.t, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (tl, p, kid) =
      Chain.unknil(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(R, p)) {
    | Convex =>
      assert(kid == None);
      c;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Chain.knil(tl, p, Some(K(kid)));
      }
    };
  };
let convexify = (~expected: Sort.Ana.t, c) =>
  c |> convexify_r(~expected) |> convexify_l(~expected);

let rec is_porous = c =>
  c
  |> Chain.to_list(kid_is_porous, Piece.is_porous)
  |> OptUtil.sequence
  |> Option.map(List.concat)
and kid_is_porous =
  fun
  | None => Some(Space.empty)
  | Some(K(kid)) => is_porous(kid);

// precond: l and r are nonempty
let rec degrout = (l: Padded.t, r: Padded.t): option(Padded.t) => {
  let (c_l, (s_ll, s_lr)) = l;
  let (c_r, (s_rl, s_rr)) = r;
  let (tl_l, p_l, kid_l) =
    Chain.unknil(c_l) |> OptUtil.get_or_raise(Orphaned_kid);
  let (kid_r, p_r, tl_r) =
    Chain.unlink(c_r) |> OptUtil.get_or_raise(Orphaned_kid);
  open OptUtil.Syntax;
  let* dg = Piece.degrout(p_l, p_r);
  switch (dg) {
  | Degrout =>
    let* s_l = kid_is_porous(kid_l);
    let* s_r = kid_is_porous(kid_r);
    let l = Padded.mk(~l=s_ll, ~r=s_lr @ s_l, tl_l);
    let r = Padded.mk(~l=s_r @ s_rl, ~r=s_rr, tl_r);
    degrout(l, r);
  | Fill(d) =>
    let+ s_l = kid_is_porous(kid_l)
    and+ s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_l, s_lr, s_rl, s_r]);
    let p =
      switch (d) {
      | L => Piece.pad(~l=Piece.space(p_l) @ s_mid, p_r)
      | R => Piece.pad(~r=s_mid @ Piece.space(p_r), p_l)
      };
    Padded.mk(~l=s_ll, ~r=s_rr, Chain.append(tl_l, p, tl_r));
  | Pass(L) =>
    let* s_l = kid_is_porous(kid_l);
    let s_mid = List.concat([Piece.space(p_l), s_l, s_lr]);
    let l = Padded.mk(~l=s_ll, ~r=s_mid, tl_l);
    degrout(l, r);
  | Pass(R) =>
    let* s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_rl, s_r, Piece.space(p_l)]);
    let r = Padded.mk(~l=s_mid, ~r=s_rr, tl_r);
    degrout(l, r);
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
  switch (degrout(l, r)) {
  | Some(c) => c
  | None =>
    let (c_l, (s_ll, s_lr)) = l;
    let (c_r, (s_rl, s_rr)) = r;
    let (tl_l, p_l, kid_l) =
      Chain.unknil(c_l) |> OptUtil.get_or_raise(Orphaned_kid);
    let (kid_r, p_r, tl_r) =
      Chain.unlink(c_r) |> OptUtil.get_or_raise(Orphaned_kid);
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
      Chain.knil(tl_l, p_l, Some(r)) |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Eq(expected) =>
      let (kid, (l, r)) = merge_kids(~expected, kid_l, s_lr, s_rl, kid_r);
      let p_l = Piece.pad(~r=l, p_l);
      let p_r = Piece.pad(~l=r, p_r);
      Chain.append(tl_l, p_l, Chain.link(Some(K(kid)), p_r, tl_r))
      |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Gt(expected) =>
      assert(kid_l == None);
      let p_r = Piece.pad(~l=s_lr @ s_rl, p_r);
      let l = mk_kid(~expected, c_l);
      Chain.link(Some(l), p_r, tl_r) |> Padded.mk(~l=s_ll, ~r=s_rr);
    };
  }
and merge_kids =
    (
      ~expected: Sort.Ana.t,
      l: option(kid),
      s_l: Space.s,
      s_r: Space.s,
      r: option(kid),
    )
    : Padded.t =>
  switch (l, r) {
  | (None, None) =>
    of_grout(Grout.mk_convex(expected.sort)) |> Padded.mk(~l=s_l, ~r=s_r)
  | (None, Some(K(r))) => Padded.mk(~l=s_l @ s_r, r)
  | (Some(K(l)), None) => Padded.mk(~r=s_l @ s_r, l)
  | (Some(K(l)), Some(K(r))) =>
    let l = Padded.mk(~r=s_l, l);
    let r = Padded.mk(~l=s_r, r);
    merge(l, r);
  }
and mk_kid = (~expected: Sort.Ana.t, c: t): kid =>
  K(convexify(~expected, c));

let merge_all = pcs => List.fold_right(merge, pcs, Padded.empty());

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");

let cmp_merge = (l: t, ~kid=Padded.empty(), r: t): Cmp.s(Padded.t) =>
  switch (cmp(l, r)) {
  | In () =>
    let (c, (s_l, s_r)) = kid;
    assert(is_empty(c));
    In(merge(Padded.mk(~r=s_l, l), Padded.mk(~l=s_r, r)));
  | Lt () => Lt(merge(kid, Padded.mk(r)))
  | Eq () => Eq(merge_all([Padded.mk(l), kid, Padded.mk(r)]))
  | Gt () => Gt(merge(Padded.mk(l), kid))
  };

// precond: c is left-closed
// postcond: returned segment is left-closed
// let uncons_char = (c: t): option((Lexeme.t, Base.Segment.t)) => {
//   open OptUtil.Syntax;
//   let* (kid, p, c_tl) = Aba.uncons(c);
//   if (kid != None) {
//     raise(Invalid_argument("uncons_char expects a left-closed chain"));
//   };
//   let+ (l, p_tl) = Piece.uncons_char(p);
//   l;
// };

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, _) => failwith("todo pop_lexeme");

// todo: probably want to replace with lexeme
[@warning "-27"]
let pop_token = (~from: Dir.t, _) => failwith("todo pop_token");
