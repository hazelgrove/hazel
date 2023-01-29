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
let cons_space = cons(~onto_l=Segment.snoc_space, ~onto_r=Segment.cons_space);
let cons_meld = cons(~onto_l=Segment.snoc_meld, ~onto_r=Segment.cons_meld);
let cons_lexeme =
  cons(~onto_l=Segment.snoc_lexeme, ~onto_r=Segment.cons_lexeme);

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
  Segment.concat([pre, sel, suf])
  |> Segment.assemble(~l?, ~r?)
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

let assemble = ((pre, suf): t) => {
  Segment.(assemble_l(pre), assemble_r(suf));
};

let choose_matching = (c: Meld.t, t: Token.t) =>
  LangUtil.molds_of_token(t)
  |> List.filter(m =>
       Mold.matching(L, m) && Meld.cmp_mold(c, m) == Some(Eq())
     )
  |> ListUtil.hd_opt;

// todo: reimplement in terms of precedence bounds
let choose = (in_l: option(Sort.o), out: Sort.o, t: Token.t) => {
  let out_consistent =
    LangUtil.molds_of_token(t)
    |> List.filter((m: Mold.t) => Sort.compare_o(m.sort, out) <= 0);
  switch (out_consistent) {
  | [] => None
  | [m] => Some(m)
  | [_, _, ..._] =>
    let in_l_consistent =
      out_consistent
      |> List.filter(m =>
           switch (in_l, Mold.expected_sort(L, m)) {
           | (None, Some(_))
           | (Some(_), None) => false
           | (None, None) => true
           | (Some(actual), Some(expected)) =>
             Sort.compare_o(actual, expected) <= 0
           }
         );
    switch (in_l_consistent) {
    | [] => None
    | [m, ..._] => Some(m) // unspecified choice
    };
  };
};

let rec mold_matching = (t: Token.t, (pre, suf): t): option(Mold.t) =>
  Chain.unknil(pre)
  |> OptUtil.and_then(((pre, c, _)) =>
       switch (choose_matching(c, t)) {
       | None => mold_matching(t, (pre, suf))
       | Some(m) => Some(m)
       }
     );

let mold = (t: Token.t, (pre, _): t): option(Mold.t) => {
  let rec go = (~in_l: option(Sort.o)=?, pre: Segment.t) =>
    Chain.unknil(pre)
    |> OptUtil.and_then(((pre, mel, _)) => {
         let go_next = () => go(~in_l=Meld.sort(mel), pre);
         switch (Meld.tip(R, mel)) {
         | None
         | Some(Convex) => go_next()
         | Some(Concave(out, _)) =>
           switch (choose(in_l, out, t)) {
           | None => go_next()
           | Some(m) => Some(m)
           }
         };
       });
  go(pre);
};

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
  (mel, assemble((pre, suf)));
};
