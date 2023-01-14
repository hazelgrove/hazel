open Util;

type t = Aba.t(Space.t, Chain.t);

// when input chain structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

let empty = ([Space.empty], []);
let is_empty: t => bool = (==)(empty);

let concat = _ => failwith("todo concat");

let of_space = (s: Space.t): t => Aba.singleton(s);
let of_chain = (c: Chain.t): t => Aba.mk(Space.[empty, empty], [c]);

let cons_lexeme = (l: Lexeme.t, seg: t): t =>
  switch (l) {
  | S(s) => Aba.map_first((@)(s), seg)
  | T(t) => Aba.cons(Space.empty, Chain.of_tile(t), seg)
  | G(g) => Aba.cons(Space.empty, Chain.of_grout(g), seg)
  };

let of_lexemes = (ls: list(Lexeme.t)): t =>
  List.fold_right(cons_lexeme, ls, empty);

let join = (segs: Aba.t(Space.t, t)): t =>
  segs
  |> Aba.fold_right(
       (s, seg, acc) => concat([of_space(s), seg, acc]),
       s => of_space(s),
     );

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, seg: t) => failwith("todo pop_lexeme");

let rec mold =
        (~match: bool, pre: t, ~kid: option(Sort.t)=?, t: Token.t)
        : Mold.Result.t =>
  switch (Aba.unsnoc(pre)) {
  | None => Error(kid)
  | Some((pre, c, _)) =>
    open Result.Syntax;
    let/ kid = Chain.mold(c, ~kid?, t);
    mold(~match, pre, ~kid?, t);
  };

[@warning "-27"]
let push_chain = (seg: t, ~kid=?, c: Chain.t): Cmp.Result.t(t, t, t) =>
  failwith("todo push_chain");

let to_prefix = _ => failwith("todo to_prefix");
let to_suffix = _ => failwith("todo to_suffix");

[@warning "-27"]
let finish_l = (~kid=empty, c: Chain.t): t => failwith("todo finish_l");

let finish = (~expected: Sort.t, seg: t): (Space.t, Chain.t, Space.t) =>
  switch (seg) {
  | ([s], []) => (s, Chain.(finish(~expected, empty)), Space.empty)
  | ([s_l, s_r], [c]) => (s_l, Chain.finish(~expected, c), s_r)
  | _ => failwith("unexpected call to finish with more than one chain")
  };

// assume push onto head of chains in left-to-right order
// let push = (c: Chain.t, cs: t): t => {
//   let rec go = (c0: Chain.t, ~mid=?, cs: t): t =>
//     switch (cs) {
//     | [] => [c0, ...Option.to_list(mid)]
//     | [c1, ...tl] =>
//       switch (Chain.comp(c0, c1)) {
//       | Some(Eq) =>
//         let c =
//           switch (mid) {
//           | None => Chain.merge(c0, c1)
//           | Some(c_mid) => Chain.(merge(c0, merge(c_mid, c1)))
//           };
//         [c, ...tl];
//       | Some(Lt) =>
//         let mid =
//           switch (mid) {
//           | None => c1
//           | Some(c_mid) => Chain.merge(c_mid, c1)
//           };
//         go(c0, ~mid, tl);
//       | Some(Gt) =>
//         switch (mid) {
//         | None =>
//           // hull_r on c0?
//           [c0, ...cs]
//         | Some(c_mid) => [Chain.merge(c0, c_mid), ...tl]
//         }
//       | None =>
//         assert(mid == None);

//         // ignore matching molds atm

//         let g = failwith("todo grout");
//         [c0, ...go(g, tl)];
//       }
//     };
//   go(c, cs);
// };
