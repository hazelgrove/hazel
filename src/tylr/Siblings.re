open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Segment.t, Segment.t);

let mk = (~l=Segment.empty, ~r=Segment.empty, ()) => (l, r);
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
let cons_meld =
  cons(
    ~onto_l=(seg, mel) => Segment.Bounded.snoc_meld(None, seg, mel),
    ~onto_r=(mel, seg) => Segment.Bounded.cons_meld(mel, seg, None),
  );
let cons_lexeme =
  cons(
    ~onto_l=(seg, lx) => Segment.Bounded.snoc_lexeme(None, seg, lx),
    ~onto_r=(lx, seg) => Segment.Bounded.cons_lexeme(lx, seg, None),
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

let zip = (~l=?, ~r=?, ~sel=Segment.empty, sib: t): Meld.Padded.t => {
  let (sel, (pre, suf)) = zip_pieces(sel, sib);
  Segment.Bounded.concat_r([pre, sel, suf], r)
  |> Segment.Bounded.bound_l(l)
  |> Segment.to_padded
  |> OptUtil.get_or_raise(Meld.Invalid_prec);
};

let piece_bounds = ((pre, suf): t): (option(Piece.t), option(Piece.t)) => {
  let l =
    Chain.unknil(pre)
    |> OptUtil.and_then(((_, mel, _)) => ListUtil.last_opt(Meld.root(mel)));
  let r =
    Chain.unlink(suf)
    |> OptUtil.and_then(((_, mel, _)) => ListUtil.hd_opt(Meld.root(mel)));
  (l, r);
};

let bound = (~l=None, ~r=None, (pre, suf): t) =>
  Segment.Bounded.(bound_l(l, pre), bound_r(suf, r));

module Step = {
  type sib = t;
  // counts of melds in prefix
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let of_ = ((l, _): sib): t => List.length(Segment.melds(l));
};

let unzip = (step: Step.t, seg: Segment.t): (Meld.t, t) => {
  let (pre, mel, suf) =
    try(Chain.split_nth_link(step, seg)) {
    | Invalid_argument(_) =>
      print_endline("step = " ++ string_of_int(step));
      print_endline("seg = " ++ Segment.show(seg));
      raise(Invalid_argument("Siblings.unzip"));
    };
  (mel, bound((pre, suf)));
};
