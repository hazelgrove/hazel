open Util;

// constituent terraces are the bridge's "base"
[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Terrace.t, Terrace.t);
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

let zip = (par: t, mel: Meld.t): Meld.t => {
  let ((tl_l, hd_l), r) = par;
  Meld.(append(tl_l, hd_l, Closed.open_l(mel, r)));
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
