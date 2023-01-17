open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(Space.t, Chain.t);

// when input chain structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

let empty = ([Space.empty], []);
let is_empty: t => bool = (==)(empty);

let cons_space = (s, seg) => Aba.map_first((@)(s), seg);
let cons_lexeme = (l: Lexeme.t, seg: t): t =>
  switch (l) {
  | S(s) => cons_space(s, seg)
  | T(t) => Aba.cons(Space.empty, Chain.of_tile(t), seg)
  | G(g) => Aba.cons(Space.empty, Chain.of_grout(g), seg)
  };

let concat = (segs: list(t)): t =>
  List.fold_right(
    (seg, acc) => seg |> Aba.fold_right(Aba.cons, s => cons_space(s, acc)),
    segs,
    empty,
  );

let of_space = (s: Space.t): t => Aba.singleton(s);
let of_chain = (c: Chain.t): t => Aba.mk(Space.[empty, empty], [c]);
let of_padded = ((c, (l, r)): Chain.Padded.t): t => Aba.mk([l, r], [c]);

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

let push_remolded_chain =
    (seg: t, ~kid=Chain.Padded.empty, c: Chain.t)
    : (Cmp.Result.t(t, t, t, Chain.Padded.t) as 'r) =>
  seg
  |> Aba.fold_right(
       (s, c', r: 'r) =>
         switch (r) {
         | In(seg) => In(Aba.cons(s, c', seg))
         | Lt(seg) => Lt(Aba.cons(s, c', seg))
         | Eq(seg) => Eq(Aba.cons(s, c', seg))
         | Gt(kid) =>
           switch (Chain.cmp_merge(c', ~kid, c)) {
           | In(c) => In(of_padded(Chain.Padded.mk(~l=s, c)))
           | Lt(kid) => Lt(Aba.cons(s, c', of_padded(kid)))
           | Eq(c) => Eq(of_padded(Chain.Padded.mk(~l=s, c)))
           | Gt(kid) => Gt(kid)
           }
         },
       s => Cmp.Result.Gt(Chain.Padded.pad(~l=s, kid)),
     );

let push_chain = (onto, ~kid=Chain.Padded.empty, c) =>
  switch (push_remolded_chain(onto, ~kid, c)) {
  | In(seg)
  | Lt(seg)
  | Eq(seg) => seg
  | Gt(kid) => of_padded(Chain.finish_l(~kid, c))
  };
let push_seg = (onto: t, seg: t) =>
  seg
  |> Aba.fold_left(
       s => concat([onto, of_space(s)]),
       (onto, c, s) => concat([push_chain(onto, c), of_space(s)]),
     );

// precond: in prefix form
// todo: rework using aba interface or possibly reformulate
// overall using parent merging
let finish_prefix = (pre: t): Chain.Padded.t =>
  pre
  |> Aba.fold_right(
       (s, c, kid) => Chain.finish_r(c, ~kid, ()) |> Chain.Padded.pad(~l=s),
       s => (Chain.empty, (s, [])),
     );

let to_prefix = (seg: t) =>
  seg
  |> Aba.fold_right(
       (s, c, pre) => concat([of_space(s), Chain.to_prefix(c), pre]),
       of_space,
     );
let to_suffix = _ => failwith("todo to_suffix");

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
