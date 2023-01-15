open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Segment.t, Segment.t);

let empty = Segment.(empty, empty);

[@warning "-27"]
let pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) =>
  failwith("todo");

let choose_matching = (c: Chain.t, t: Token.t) =>
  LangUtil.molds(t)
  |> List.filter(m =>
       Mold.matching(L, m) && Chain.cmp_mold(c, m) == Some(Eq)
     )
  |> ListUtil.hd_opt;

// todo: reimplement in terms of precedence bounds
let choose = (in_l: option(Sort.t), out: Sort.t, t: Token.t) => {
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
  let rec go = (~in_l: option(Sort.t)=?, pre: Segment.t) =>
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
