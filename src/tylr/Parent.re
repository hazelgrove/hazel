open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Meld.t, Meld.t);
// <let< x >=< _kid_ >in< 1
// -----------       ------

exception Convex_inner_tips;

// let root = failwith("todo parent root");

let uncons = (~from_l, ~from_r, ~from: Dir.t, (l, r): t) =>
  switch (from) {
  | L =>
    let (l, a) = from_l(l);
    (a, (l, Segment.of_meld(r)));
  | R =>
    let (a, r) = from_r(r);
    (a, (Segment.of_meld(l), r));
  };
let uncons_char =
  uncons(
    ~from_l=Segment.Meld_.unsnoc_char,
    ~from_r=Segment.Meld_.uncons_char,
  );
let uncons_lexeme =
  uncons(
    ~from_l=Segment.Meld_.unsnoc_lexeme,
    ~from_r=Segment.Meld_.uncons_lexeme,
  );

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

module Step = {
  type par = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let of_ = ((l, _): par): t => List.length(Meld.root(l));
};

let unzip = (step: Step.t, mel: Meld.t): (Meld.Padded.t, t) =>
  try({
    let (ks, ps) = mel;
    let (ks_l, k, ks_r) = ListUtil.split_nth(step, ks);
    let (ps_l, ps_r) = ListUtil.split_n(step, ps);
    let (ps_l, s_l) = {
      let (ps_l, p_l) = ListUtil.split_last(ps_l);
      let (p_l, s_l) = Piece.pop_space_r(p_l);
      (ps_l @ [p_l], s_l);
    };
    let (s_r, ps_r) = {
      let (p_r, ps_r) = ListUtil.split_first(ps_r);
      let (s_r, p_r) = Piece.pop_space_l(p_r);
      (s_r, [p_r, ...ps_r]);
    };
    let mel_l = Chain.mk(ks_l @ [None], ps_l);
    let mel_r = Chain.mk([None, ...ks_r], ps_r);
    let K(kid) = Option.get(k);
    (Meld.Padded.mk(~l=s_l, ~r=s_r, kid), (mel_l, mel_r));
  }) {
  | _ =>
    raise(
      Invalid_argument(
        Printf.sprintf("Parent.unzip(%d, %s)", step, Meld.show(mel)),
      ),
    )
  };
