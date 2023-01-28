open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Segment.t, Segment.t);

let empty = Segment.(empty, empty);

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

let cat = ((l_inner, r_inner), (l_outer, r_outer)) =>
  Segment.(cat(l_outer, l_inner), cat(r_inner, r_outer));
// let concat = _ => failwith("todo concat");

let uncons_opt_lexemes = (sib: t) => {
  let (l, sib) =
    switch (uncons_lexeme(~from=L, sib)) {
    | None => (None, sib)
    | Some((l, sib)) => (Some(l), sib)
    };
  let (r, sib) =
    switch (uncons_lexeme(~from=R, sib)) {
    | None => (None, sib)
    | Some((r, sib)) => (Some(r), sib)
    };
  ((l, r), sib);
};

let within_piece = (sib: t) =>
  switch (uncons_opt_lexemes(sib)) {
  | ((Some(G(l)), Some(G(r))), sib) when l.id == r.id =>
    Some((Piece.mk(G({...l, prefix: l.prefix ++ r.prefix})), sib))
  | ((Some(T(l)), Some(T(r))), sib) when l.id == r.id =>
    Some((Piece.mk(T({...l, token: l.token ++ r.token})), sib))
  | _ => None
  };

let zip = (~l=?, ~r=?, ~sel=Segment.empty, (pre, suf): t): Meld.Padded.t => {
  let suf = Segment.cat(sel, suf);
  let (pre, suf) =
    switch (within_piece((pre, suf))) {
    | Some((p, (pre, suf))) => (
        pre,
        Segment.cons_meld(Meld.of_piece(p), suf),
      )
    | None => (pre, suf)
    };
  Segment.cat(pre, suf)
  |> Segment.assemble(~l?, ~r?)
  |> Segment.to_padded
  |> OptUtil.get_or_raise(Meld.Invalid_prec);
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
