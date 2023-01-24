open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Meld.t, Meld.t);

exception Convex_inner_tips;

// let root = failwith("todo parent root");

[@warning "-27"]
let mold = (~match, ~kid=?, t, par) => failwith("todo mold");

let zip =
    ((c, (l, r)) as kid: Meld.Padded.t, (par_l, par_r): t): Meld.Padded.t =>
  switch (Chain.unknil(par_l), Chain.unlink(par_r)) {
  | (None, None) => kid
  | (None, Some((kid_r, p_r, tl_r))) =>
    assert(kid_r == None);
    let p_r = Piece.pad(~l=r, p_r);
    Meld.(Padded.mk(~l, Chain.link(Some(K(c)), p_r, tl_r)));
  | (Some((tl_l, p_l, kid_l)), None) =>
    assert(kid_l == None);
    let p_l = Piece.pad(~r=l, p_l);
    Meld.Padded.mk(~r, Chain.knil(tl_l, p_l, Some(K(c))));
  | (Some((tl_l, p_l, kid_l)), Some((kid_r, p_r, tl_r))) =>
    assert(kid_l == None && kid_r == None);
    let p_l = Piece.pad(~r=l, p_l);
    let p_r = Piece.pad(~l=r, p_r);
    Meld.(
      Padded.mk(Chain.(append(tl_l, p_l, link(Some(K(c)), p_r, tl_r))))
    );
  };
