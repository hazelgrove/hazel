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
  uncons(~from_l=Segment.unsnoc_lexeme, ~from_r=Segment.uncons_lexeme);

let cat = ((l_inner, r_inner), (l_outer, r_outer)) =>
  Segment.(cat(l_outer, l_inner), cat(r_inner, r_outer));
// let concat = _ => failwith("todo concat");

let zip = (~l=?, ~r=?, ~sel=Segment.empty, (pre, suf): t): Meld.Padded.t =>
  Segment.concat([pre, sel, suf])
  |> Segment.assemble(~l?, ~r?)
  |> Segment.to_padded
  |> OptUtil.get_or_raise(Meld.Invalid_prec);

let assemble = ((pre, suf): t) => {
  Segment.(assemble_l(pre), assemble_r(suf));
};

let choose_matching = (c: Meld.t, t: Token.t) =>
  LangUtil.molds(t)
  |> List.filter(m =>
       Mold.matching(L, m) && Meld.cmp_mold(c, m) == Some(Eq())
     )
  |> ListUtil.hd_opt;

// todo: reimplement in terms of precedence bounds
let choose = (in_l: option(Sort.o), out: Sort.o, t: Token.t) => {
  let out_consistent =
    LangUtil.molds(t)
    |> List.filter((m: Mold.t) => Sort.compare(m.sort, out) <= 0);
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
             Sort.compare(actual, expected) <= 0
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
    |> OptUtil.and_then(((pre, c, _)) => {
         let go_next = () => go(~in_l=Meld.sort(c), pre);
         switch (Meld.expected_sort(R, c)) {
         | None => go_next()
         | Some(out) =>
           switch (choose(in_l, out, t)) {
           | None => go_next()
           | Some(m) => Some(m)
           }
         };
       });
  go(pre);
};
