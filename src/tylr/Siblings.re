open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Segment.t, Segment.t);

let empty = Segment.(empty, empty);

let cons_space = (~onto: Dir.t, s, (l, r): t) => {
  let s = Segment.of_space(s);
  switch (onto) {
  | L => (Segment.concat([l, s]), r)
  | R => (l, Segment.concat([s, r]))
  };
};
let cons_chain = (~onto: Dir.t, c, (l, r)) =>
  switch (onto) {
  | L => (Segment.snoc_chain(l, c), r)
  | R => (l, Segment.cons_chain(c, r))
  };

let zip = (~l=?, ~r=?, ~sel=Segment.empty, (pre, suf): t): Chain.Padded.t =>
  Segment.concat([pre, sel, suf])
  |> Segment.assemble(~l?, ~r?)
  |> Segment.to_padded
  |> OptUtil.get_or_raise(Chain.Invalid_prec);

let assemble = ((pre, suf): t) => {
  let l = Aba.uncons(suf) |> Option.map(((_, c, _)) => c);
  let r = Aba.unsnoc(pre) |> Option.map(((_, c, _)) => c);
  Segment.(assemble_r(~r?, pre), assemble_l(~l?, suf));
};

[@warning "-27"]
let pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) =>
  failwith("todo");

let choose_matching = (c: Chain.t, t: Token.t) =>
  LangUtil.molds(t)
  |> List.filter(m =>
       Mold.matching(L, m) && Chain.cmp_mold(c, m) == Some(Eq())
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
  Aba.unsnoc(pre)
  |> OptUtil.and_then(((pre, c, _)) =>
       switch (choose_matching(c, t)) {
       | None => mold_matching(t, (pre, suf))
       | Some(m) => Some(m)
       }
     );

let mold = (t: Token.t, (pre, _): t): option(Mold.t) => {
  let rec go = (~in_l: option(Sort.o)=?, pre: Segment.t) =>
    Aba.unsnoc(pre)
    |> OptUtil.and_then(((pre, c, _)) => {
         let go_next = () => go(~in_l=Chain.sort(c), pre);
         switch (Chain.expected_sort(R, c)) {
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

let concat = _ => failwith("todo concat");
