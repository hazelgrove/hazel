open Util;

// terraces connected by bridge
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = (Terrace.R.t('a), Terrace.L.t('a));
[@deriving (show({with_path: false}), sexp, yojson)]
type p = t(Piece.t);
// <let< x >=< _kid_ >in< 1
// -----------       ------

exception Convex_inner_tips;

let uncons_lexeme = (~char=false, ~from: Dir.t, (l, r): t) =>
  switch (from) {
  | L =>
    let (l, s, lx) = Terrace.R.unsnoc_lexeme(~char, l);
    (lx, Slope.(Dn.mk(~s, l), Up.of_terr(r)));
  | R =>
    let (lx, s, r) = Terrace.L.uncons_lexeme(~char, r);
    (lx, Slope.(Dn.of_terr(l), Up.mk(~s, r)));
  };

let mold = (~slot=?, t, (l, _)) => Terrace.R.mold(l, ~slot?, t);

let zip = ((l, r): t, kid: Meld.t) => Option.get(Terrace.eq(l, ~slot, r));

let unzip = (n, mel: Meld.t) => {
  open OptUtil.Syntax;
  let* (mel_l, wal, mel_r) = Wald.mk(mel);
  let+ (wal_l, kid, wal_r) =
    switch (Chain.split_nth_link(n, wal)) {
    | r => Some(r)
    | exception _ => None
    };
  let l = Terrace.{mel: mel_l, wal: wal_l};
  let r = Terrace.{wal: wal_r, mel: mel_r};
  (kid, (l, r));
};
