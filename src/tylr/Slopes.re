open Util;
open Slope;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Dn.t, Up.t);
// type t = (Segment.t, Segment.t);

let mk = (~l=Dn.empty, ~r=Up.empty, ()) => (l, r);
let empty = mk();

let cons = (~onto_l, ~onto_r, ~onto: Dir.t, a, (l, r): t) =>
  switch (onto) {
  | L => (onto_l(l, a), r)
  | R => (l, onto_r(a, r))
  };
let cons_space =
  cons(
    ~onto_l=(seg, s) => Segment.knil(seg, ~s, ()),
    ~onto_r=(s, seg) => Segment.link(~s, seg),
  );

let uncons = (~from_l, ~from_r, ~from: Dir.t, (l, r): t) =>
  switch (from) {
  | L => from_l(l) |> Option.map(((l, a)) => (a, (l, r)))
  | R => from_r(r) |> Option.map(((a, r)) => (a, (l, r)))
  };
let uncons_lexeme =
  uncons(~from_l=Segment.unsnoc_lexeme, ~from_r=Segment.uncons_lexeme);
let uncons_char =
  uncons(~from_l=Segment.unsnoc_char, ~from_r=Segment.uncons_char);
let uncons_opt_lexeme = (~from: Dir.t, sib) =>
  switch (uncons_lexeme(~from, sib)) {
  | None => (None, sib)
  | Some((lx, sib)) => (Some(lx), sib)
  };
let uncons_opt_lexemes = (sib: t) => {
  let (l, sib) = uncons_opt_lexeme(~from=L, sib);
  let (r, sib) = uncons_opt_lexeme(~from=R, sib);
  ((l, r), sib);
};
let peek_lexemes = sib => fst(uncons_opt_lexemes(sib));

let cat = ((l_inner, r_inner), (l_outer, r_outer)) =>
  Segment.(cat(l_outer, l_inner), cat(r_inner, r_outer));
// let concat = _ => failwith("todo concat");

let zip_piece = (sib: t): (Segment.t, t) => {
  let ((l, r), sib') = uncons_opt_lexemes(sib);
  switch (Option.bind(l, Lexeme.to_piece), Option.bind(r, Lexeme.to_piece)) {
  | (Some(p_l), Some(p_r)) when Option.is_some(Piece.zip(p_l, p_r)) =>
    let p = Option.get(Piece.zip(p_l, p_r));
    (Segment.of_meld(Meld.of_piece(p)), sib');
  | _ => (Segment.empty, sib)
  };
};
let zip_piece_l = (sel, sib) => {
  let (lx, sib') = uncons_opt_lexeme(~from=L, sib);
  switch (Option.bind(lx, Lexeme.to_piece), Chain.unlink(sel)) {
  | (Some(p_l), Some(([], mel, tl)))
      when Option.is_some(Meld.zip_piece_l(p_l, mel)) =>
    let mel = Option.get(Meld.zip_piece_l(p_l, mel));
    (Chain.link([], mel, tl), sib');
  | _ => (sel, sib)
  };
};
let zip_piece_r = (sel, sib) => {
  let (lx, sib') = uncons_opt_lexeme(~from=R, sib);
  switch (Chain.unknil(sel), Option.bind(lx, Lexeme.to_piece)) {
  | (Some((tl, mel, [])), Some(p_r))
      when Option.is_some(Meld.zip_piece_r(mel, p_r)) =>
    let mel = Option.get(Meld.zip_piece_r(mel, p_r));
    (Chain.knil(tl, mel, []), sib');
  | _ => (sel, sib)
  };
};
let zip_pieces = (sel: Segment.t, sib: t): (Segment.t, t) => {
  let (sel, sib) = zip_piece_r(sel, sib);
  let (sel, sib) = zip_piece_l(sel, sib);
  Segment.is_empty(sel) ? zip_piece(sib) : (sel, sib);
};

let bounds = ((pre, suf): t): (Segment.Bound.t as 'b, 'b) => {
  let l = Chain.unknil(pre) |> Option.map(((_, mel, _)) => mel);
  let r = Chain.unlink(suf) |> Option.map(((_, mel, _)) => mel);
  (l, r);
};

let peek_space = ((l, r): t) => (Chain.lst(l), Chain.fst(r));

let bound = (~l=None, ~r=None, (pre, suf): t) =>
  Segment.Bounded.(bound_l(l, pre), bound_r(suf, r));

let offset = sib =>
  switch (peek_lexemes(sib)) {
  | (Some(l), Some(r)) when Lexeme.(id(l) == id(r)) => - Lexeme.length(r)
  | _ => List.length(fst(peek_space(sib)))
  };
let steps = ((l, _): t) => List.map(Meld.length, Segment.melds(l));
let path = (sib: t) =>
  Meld.Path.mk(~offset=offset(sib), ~steps=steps(sib), ());

let rec zip = ((dn, up): t, kid: Meld.t) =>
  switch (Dn.uncons(dn), Up.uncons(up)) {
  | (Error(s), _) => Up.zip(Meld.pad(~l=s, kid), up)
  | (_, Error(s)) => Dn.zip(dn, Meld.pad(kid, ~r=s))
  | (Ok((dn', l, s_l)), Ok((s_r, r, up'))) =>
    let kid = Meld.pad(~l=s_l, kid, ~r=s_r);
    switch (Meld.cmp(l, ~kid, r)) {
    | {lt: None, eq: None, gt: None} => raise(Meld.Invalid_prec)
    | {lt: Some(kid_r), _} => zip((dn, up'), kid_r)
    | {gt: Some(l_kid), _} => zip((dn', up), l_kid)
    | {eq: Some(l_kid_r), _} => zip((dn', up'), l_kid_r)
    };
  };
let zip_init = ((dn, up): t) =>
  switch (Dn.uncons(dn), Up.uncons(up)) {
  | (Ok((dn, (tl_l, hd_l), s_l)), Ok((s_r, (hd_r, tl_r), up)))
      when
        Space.(is_empty(s_l) && is_empty(s_r))
        && Option.is_some(Piece.zip(hd_l, hd_r)) =>
    let p = Option.get(Piece.zip(hd_l, hd_r));
    let kid = Meld.append(tl_l, p, tl_r);
    zip((dn, up), kid);
  | _ => zip((dn, up), Meld.empty(~paths=[Path.mk()], ()))
  };
