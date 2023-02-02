open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Meld.t, Meld.t);
// <let< x >=< _kid_ >in< 1
// -----------       ------

exception Convex_inner_tips;

let empty: t = Meld.(empty, empty);
let is_empty = (==)(empty);

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

let step = ((l, _): t) => Meld.length(l);
let zip =
    ((par_l, par_r) as par: t, (kid, path): Meld.Zipped.t): Meld.Zipped.t => {
  let (mel, (l, r)) = kid;
  let zipped =
    switch (Chain.unknil(par_l), Chain.unlink(par_r)) {
    | (None, None) => kid
    | (None, Some((kid_r, p_r, tl_r))) =>
      assert(kid_r == None);
      let p_r = Piece.pad(~l=r, p_r);
      Meld.(Padded.mk(~l, Chain.link(Some(K(mel)), p_r, tl_r)));
    | (Some((tl_l, p_l, kid_l)), None) =>
      assert(kid_l == None);
      let p_l = Piece.pad(~r=l, p_l);
      Meld.Padded.mk(~r, Chain.knil(tl_l, p_l, Some(K(mel))));
    | (Some((tl_l, p_l, kid_l)), Some((kid_r, p_r, tl_r))) =>
      assert(kid_l == None && kid_r == None);
      let p_l = Piece.pad(~r=l, p_l);
      let p_r = Piece.pad(~l=r, p_r);
      Meld.(
        Padded.mk(
          Chain.(append(tl_l, p_l, link(Some(K(mel)), p_r, tl_r))),
        )
      );
    };
  let path = Meld.Path.cons(step(par), path);
  (zipped, path);
};

let unzip = (step, mel) => {
  let (l, kid, r) = Meld.split_nth_kid(step, mel);
  let (l, s_l) = Meld.uncons_space_r(l);
  let (s_r, r) = Meld.uncons_space_l(r);
  let kid =
    switch (kid) {
    | None => Meld.empty
    | Some(K(mel)) => mel
    };
  (Meld.Padded.mk(~l=s_l, ~r=s_r, kid), (l, r));
};
