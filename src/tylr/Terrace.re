open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  retainer: Retainer.t,
  backfill: Meld.t,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type terr = t;

let of_piece = p => {retainer: Retainer.of_piece(p), backfill: Meld.empty()};

let map_retainer = (f, terr) => {...terr, retainer: f(terr.retainer)};

let split_face = ({retainer, backfill}: t): (Piece.t, Meld.t) =>
  switch (Chain.unlink(retainer)) {
  | None => (Chain.fst(retainer), backfill)
  | Some((face, kid, rest)) => (
      face,
      Retainer.to_meld(~l=kid, rest, ~r=backfill),
    )
  };
let face = terr => fst(split_face(terr));

module L = {
  type t = terr; // left-to-right: retainer backfill

  let mk = (mel: Meld.t): option((Meld.t, t)) =>
    Retainer.of_meld(mel)
    |> Option.map(((kid, retainer, backfill)) =>
         (kid, {retainer, backfill})
       );
  let unmk = (kid, {retainer, backfill}: t) =>
    Retainer.to_meld(~l=kid, retainer, ~r=backfill);
  let append = (_: Meld.t, _: t) => failwith("todo append");

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
    | Some((c, rest_face)) when char =>
      let (_empty, l) = Option.get(mk(Meld.link(rest_face, rest)));
      (Lexeme.of_piece(c), Space.empty, [l]);
    | _ =>
      let (s, ls) = mk_s(rest);
      (Lexeme.of_piece(face), s, ls);
    };
  };
};

module R = {
  type t = terr; // left-to-right: backfill retainer

  let mk = (mel: Meld.t): option((t, Meld.t)) =>
    Retainer.of_meld(mel)
    |> Option.map(((backfill, retainer, kid)) =>
         ({backfill, retainer}, kid)
       );
  let unmk = ({backfill, retainer}: t, kid) =>
    Retainer.to_meld(~l=backfill, retainer, ~r=kid);

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
    let (face, rest) = split_face(r);
    // left-to-right: rest face
    switch (Piece.unzip(Piece.length(face) - 1, face)) {
    | Some((rest_face, c)) when char =>
      let (r, _empty) = Option.get(mk(Meld.knil(rest, rest_face)));
      ([r], Space.empty, Lexeme.of_piece(c));
    | _ =>
      let (rs, s) = mk_s(rest);
      (rs, s, Lexeme.of_piece(face));
    };
  };
};

// todo: consider requiring kid already be completed
let lt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let (p_l, p_r) = (face(l), face(r));
  let+ _ = Piece.lt(p_l, p_r);
  let kid =
    switch (Piece.tip(L, p_r)) {
    | Convex =>
      assert(Option.is_some(Meld.is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  L.unmk(kid, r);
};

let gt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let (p_l, p_r) = (face(l), face(r));
  let+ _ = Piece.gt(p_l, p_r);
  let kid =
    switch (Piece.tip(R, p_l)) {
    | Convex =>
      assert(Option.is_some(Meld.is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  R.unmk(l, kid);
};

let rec eq = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let ((p_l, tl_l), (p_r, tl_r)) = (split_face(l), split_face(r));
  // left-to-right: tl_l p_l p_r tl_r
  switch (
    // todo: relax to porous
    Meld.is_empty(kid),
    Piece.replaces(p_l, p_r),
    // let x   //     in
    Piece.passes(p_l, p_r),
  ) {
  | (Some(s), Some(L), _) => return(L.append(Meld.pad(tl_l, ~r=s), r))
  | (Some(s), Some(R), _) => return(R.prepend(l, Meld.pad(~l=s, tl_r)))
  | (Some(s), _, Some(L)) =>
    let* (l, kid) = R.mk(tl_l);
    eq(l, ~kid=Meld.pad(kid, ~r=s), r);
  | (Some(s), _, Some(R)) =>
    let* (kid, r) = L.mk(r.backfill);
    eq(l, ~kid=Meld.pad(~l=s, kid), r);
  | _ =>
    let+ compl = Piece.eq(p_l, p_r);
    // todo: abstract into some join-complement fn
    let r =
      List.fold_right(
        ((sugg, mold), r) =>
          switch (Mold.tip(R, mold)) {
          | Convex => raise(Gram.Ill_typed)
          | Concave(s, _) =>
            let kid = Meld.of_grout(Grout.mk_convex(s));
            let g = Piece.of_grout(Grout.mk(~sugg, mold));
            map_retainer(Chain.link(g, kid), r);
          },
        compl,
        r,
      );
    R.prepend(l, L.unmk(kid, r));
  };
};

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
