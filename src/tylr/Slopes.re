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
let cons_space = cons(~onto_l=Dn.snoc_space, ~onto_r=Up.cons_space);

let uncons = (~from_l, ~from_r, ~from: Dir.t, (l, r): t) =>
  switch (from) {
  | L => from_l(l) |> Option.map(((l, a)) => (a, (l, r)))
  | R => from_r(r) |> Option.map(((a, r)) => (a, (l, r)))
  };
let uncons_lexeme =
  uncons(
    ~from_l=Dn.unsnoc_lexeme(~char=false),
    ~from_r=Up.uncons_lexeme(~char=false),
  );
let uncons_char =
  uncons(
    ~from_l=Dn.unsnoc_lexeme(~char=true),
    ~from_r=Up.uncons_lexeme(~char=true),
  );
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

let cat = ((l_inner, r_inner), (l_outer, r_outer)) => (
  Dn.cat(l_outer, l_inner),
  Up.cat(r_inner, r_outer),
);
// let concat = _ => failwith("todo concat");

// let zip_piece = (sib: t): (Segment.t, t) => {
//   let ((l, r), sib') = uncons_opt_lexemes(sib);
//   switch (Option.bind(l, Lexeme.to_piece), Option.bind(r, Lexeme.to_piece)) {
//   | (Some(p_l), Some(p_r)) when Option.is_some(Piece.zip(p_l, p_r)) =>
//     let p = Option.get(Piece.zip(p_l, p_r));
//     (Segment.of_meld(Meld.of_piece(p)), sib');
//   | _ => (Segment.empty, sib)
//   };
// };
// let zip_piece_l = (sel, sib) => {
//   let (lx, sib') = uncons_opt_lexeme(~from=L, sib);
//   switch (Option.bind(lx, Lexeme.to_piece), Chain.unlink(sel)) {
//   | (Some(p_l), Some(([], mel, tl)))
//       when Option.is_some(Meld.zip_piece_l(p_l, mel)) =>
//     let mel = Option.get(Meld.zip_piece_l(p_l, mel));
//     (Chain.link([], mel, tl), sib');
//   | _ => (sel, sib)
//   };
// };
// let zip_piece_r = (sel, sib) => {
//   let (lx, sib') = uncons_opt_lexeme(~from=R, sib);
//   switch (Chain.unknil(sel), Option.bind(lx, Lexeme.to_piece)) {
//   | (Some((tl, mel, [])), Some(p_r))
//       when Option.is_some(Meld.zip_piece_r(mel, p_r)) =>
//     let mel = Option.get(Meld.zip_piece_r(mel, p_r));
//     (Chain.knil(tl, mel, []), sib');
//   | _ => (sel, sib)
//   };
// };
// let zip_pieces = (sel: Segment.t, sib: t): (Segment.t, t) => {
//   let (sel, sib) = zip_piece_r(sel, sib);
//   let (sel, sib) = zip_piece_l(sel, sib);
//   Segment.is_empty(sel) ? zip_piece(sib) : (sel, sib);
// };

let bounds = ((l, r): t) => ListUtil.(hd_opt(l.terrs), hd_opt(r.terrs));

// let peek_space = ((l, r): t) => (Chain.lst(l), Chain.fst(r));

// let bound = (~l=None, ~r=None, (pre, suf): t) =>
//   Segment.Bounded.(bound_l(l, pre), bound_r(suf, r));

// let offset = sib =>
//   switch (peek_lexemes(sib)) {
//   | (Some(l), Some(r)) when Lexeme.(id(l) == id(r)) => - Lexeme.length(r)
//   | _ => List.length(fst(peek_space(sib)))
//   };
// let steps = ((l, _): t) => List.map(Meld.length, Segment.melds(l));
// let path = (sib: t) =>
//   Meld.Path.mk(~offset=offset(sib), ~steps=steps(sib), ());

let rec zip = ((dn, up): t, kid: Meld.t) => {
  let kid = Meld.pad(~l=dn.space, kid, ~r=up.space);
  switch (dn.terrs, up.terrs) {
  | ([], _) => Up.zip(kid, up)
  | (_, []) => Dn.zip(dn, kid)
  | ([l, ...tl_l], [r, ...tl_r]) =>
    switch (Terrace.cmp(l, ~kid, r)) {
    | {lt: None, eq: None, gt: None} => raise(Meld.Invalid_prec)
    | {lt: Some(kid_r), _} => zip((dn, Up.mk(tl_r)), kid_r)
    | {gt: Some(l_kid), _} => zip((Dn.mk(tl_l), up), l_kid)
    | {eq: Some(l_kid_r), _} => zip((Dn.mk(tl_l), Up.mk(tl_r)), l_kid_r)
    }
  };
};
let zip_init = ((dn, up): t) =>
  switch (dn.terrs, up.terrs) {
  // when caret is in the middle of a piece
  | ([l, ...terrs_l], [r, ...terrs_r])
      when
        Space.(is_empty(dn.space) && is_empty(up.space))
        && Option.is_some(Piece.zip(Terrace.face(l), Terrace.face(r))) =>
    let (hd_l, tl_l) = Terrace.split_face(l);
    let (hd_r, tl_r) = Terrace.split_face(r);
    let p = Option.get(Piece.zip(hd_l, hd_r));
    let kid = Meld.append(tl_l, p, tl_r);
    zip((Dn.mk(terrs_l), Up.mk(terrs_r)), kid);
  | _ => zip((dn, up), Meld.empty())
  };
