open Util;

type lang = list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));

let sorts = List.map(fst, Lang.t);

let molds_of_token = t => Prototiles.of_token(shape_of_token(t));
// todo: reimplement in terms of precedence bounds
// let mold_of_token = (in_l: option(Sort.o), out: Sort.o, t: Token.t) => {
//   let out_consistent =
//     molds_of_token(t)
//     |> List.filter((m: Mold.t) => Sort.compare_o(m.sort, out) <= 0);
//   switch (out_consistent) {
//   | [] => None
//   | [m] => Some(m)
//   | [_, _, ..._] =>
//     let in_l_consistent =
//       out_consistent
//       |> List.filter(m =>
//            switch (in_l, Mold.tip(L, m)) {
//            | (None, Concave(_))
//            | (Some(_), Convex) => false
//            | (None, Convex) => true
//            | (Some(actual), Concave(expected, _)) =>
//              Sort.compare_o(actual, expected) <= 0
//            }
//          );
//     switch (in_l_consistent) {
//     | [] => None
//     | [m, ..._] => Some(m) // unspecified choice
//     };
//   };
// };

let assoc = (s, p) => {
  open OptUtil.Syntax;
  let* s = s;
  let* (_, a) = p < 0 ? None : List.nth_opt(List.assoc(s, Lang.t), p);
  a;
};
