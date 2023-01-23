open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Chain.t, Chain.t);

exception Convex_inner_tips;

// let root = failwith("todo parent root");

[@warning "-27"]
let mold = (~match, ~kid=?, t, par) => failwith("todo mold");

let zip =
    ((c, (l, r)) as kid: Chain.Padded.t, (par_l, par_r): t): Chain.Padded.t =>
  switch (Aba.unsnoc(par_l), Aba.uncons(par_r)) {
  | (None, None) => kid
  | (None, Some((kid_r, p_r, tl_r))) =>
    assert(kid_r == None);
    let p_r = Piece.pad(~l=r, p_r);
    Chain.(Padded.mk(~l, Aba.cons(Some(K(c)), p_r, tl_r)));
  | (Some((tl_l, p_l, kid_l)), None) =>
    assert(kid_l == None);
    let p_l = Piece.pad(~r=l, p_l);
    Chain.Padded.mk(~r, Aba.snoc(tl_l, p_l, Some(K(c))));
  | (Some((tl_l, p_l, kid_l)), Some((kid_r, p_r, tl_r))) =>
    assert(kid_l == None && kid_r == None);
    let p_l = Piece.pad(~r=l, p_l);
    let p_r = Piece.pad(~l=r, p_r);
    Chain.(
      Padded.mk(Aba.(append(tl_l, p_l, cons(Some(K(c)), p_r, tl_r))))
    );
  };
