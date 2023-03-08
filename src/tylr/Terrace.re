open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  wal: Wald.t,
  mel: Meld.t,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type terr = t;

let of_wald = wal => {wal, mel: Meld.empty()};
let of_piece = p => of_wald(Wald.of_piece(p));

let map_wal = (f, terr) => {...terr, wal: f(terr.wal)};

let split_face = ({wal, mel}: t): (Piece.t, Meld.t) =>
  switch (Chain.unlink(wal)) {
  | None => (Chain.fst(wal), mel)
  | Some((face, kid, rest)) => (face, Wald.unmk(~l=kid, rest, ~r=mel))
  };
let face = terr => fst(split_face(terr));
let sort = (terr: t) => Piece.sort(face(terr));
let prec = (terr: t) => Piece.prec(face(terr));

module L = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = terr; // left-to-right: wal mel

  let mk = (mel: Meld.t): option((Meld.t, t)) =>
    Wald.mk(mel) |> Option.map(((kid, wal, mel)) => (kid, {wal, mel}));
  let unmk = (kid, {wal, mel}: t) => Wald.unmk(~l=kid, wal, ~r=mel);
  let append = (_: Meld.t, _: t) => failwith("todo append");

  let pad = (t: t, s: Space.t) => {...t, mel: Meld.pad(t.mel, ~r=s)};

  let rec mk_s = (mel: Meld.t): (Space.t, list(t)) =>
    switch (mk(mel)) {
    | None =>
      let ((l, r), _empty) = Meld.unpad(mel);
      (Space.cat(l, r), []);
    | Some((kid, l)) =>
      let (s, ls) = mk_s(kid);
      (s, [l, ...ls]);
    };

  let uncons_lexeme = (~char=false, l: t): (Lexeme.t, Space.t, list(t)) => {
    let (face, rest) = split_face(l);
    switch (Piece.unzip(1, face)) {
    | R((c, rest_face)) when char =>
      let (_empty, l) = Option.get(mk(Meld.link(rest_face, rest)));
      (Lexeme.of_piece(c), Space.empty, [l]);
    | _ =>
      let (s, ls) = mk_s(rest);
      (Lexeme.of_piece(face), s, ls);
    };
  };

  let tip = (terr: t) => Piece.tip(L, face(terr));
};

module R = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = terr; // left-to-right: mel wal

  let mk = (mel: Meld.t): option((t, Meld.t)) =>
    Wald.mk(mel) |> Option.map(((mel, wal, kid)) => ({mel, wal}, kid));
  let unmk = ({mel, wal}: t, kid) => Wald.unmk(~l=mel, wal, ~r=kid);

  let pad = (s: Space.t, t: t) => {...t, mel: Meld.pad(~l=s, t.mel)};

  let rec mk_s = (mel: Meld.t): (list(t), Space.t) =>
    switch (mk(mel)) {
    | None =>
      let ((l, r), _empty) = Meld.unpad(mel);
      ([], Space.cat(l, r));
    | Some((r, kid)) =>
      let (rs, s) = mk_s(kid);
      ([r, ...rs], s);
    };

  let prepend = (_: t, _: Meld.t) => failwith("todo prepend");

  let unsnoc_lexeme = (~char=false, r: t): (list(t), Space.t, Lexeme.t) => {
    print_endline("Terrace.unsnoc_lexeme");
    let (face, rest) = split_face(r);
    // left-to-right: rest face
    switch (Piece.unzip(Piece.length(face) - 1, face)) {
    | R((rest_face, c)) when char =>
      let (r, _empty) = Option.get(mk(Meld.knil(rest, rest_face)));
      ([r], Space.empty, Lexeme.of_piece(c));
    | _ =>
      let (rs, s) = mk_s(rest);
      (rs, s, Lexeme.of_piece(face));
    };
  };

  let tip = (terr: t) => Piece.tip(R, face(terr));
  // todo: remove option from return type, added just to get things typechecking
  // todo: review whether match flag is needed here
  let mold = (terr: t, ~kid: option(Sort.o)=?, t: Token.t) =>
    switch (tip(terr)) {
    | Convex => Error(Some(sort(terr)))
    | Concave(s, _) =>
      LangUtil.mold_of_token(kid, s, t)
      |> Result.of_option(~error=Some(sort(terr)))
    };

  let complement = (terr: t) =>
    Piece.complement_beyond(~side=R, face(terr));
};

let lt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) =>
  Wald.lt(l.wal, ~kid, r.wal)
  |> Option.map(((kid, wal)) => Wald.unmk(~l=kid, wal, ~r=r.mel));

let gt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) =>
  Wald.gt(l.wal, ~kid, r.wal)
  |> Option.map(((wal, kid)) => Wald.unmk(~l=l.mel, wal, ~r=kid));

let eq = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) =>
  Wald.eq(l.wal, ~kid, r.wal)
  |> Option.map(((s_l, wal, s_r)) => {
       let l = Meld.pad(l.mel, ~r=s_l);
       let r = Meld.pad(~l=s_r, r.mel);
       Wald.unmk(~l, wal, ~r);
     });

let in_ = (_, ~s as _: Space.t, _) => failwith("todo Terrace.in_");

type cmp = {
  lt: option(Meld.t),
  eq: option(Meld.t),
  gt: option(Meld.t),
};
let cmp = (l: R.t, ~kid=Meld.empty(), r: L.t) => {
  lt: lt(l, ~kid, r),
  eq: eq(l, ~kid, r),
  gt: gt(l, ~kid, r),
};
