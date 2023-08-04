// module Mold = {
//   include Gram.Zipper;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = Gram.Zipper.t(unit);
// };
// open Util;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type t = {
//   proto: Proto.t,
//   filled: Token.t,
// };
// exception Invalid;
// let mk = (~filled="", mold) => {filled, proto: Proto.Grout.mk(mold)};
// let mk_operand = (sort: Sort.o) => mk(Mold.mk(sort, Prec.max));
// let mk_prefix = (~r=?, sort, prec) => mk(Mold.mk_prefix(sort, prec, ~r?));
// let mk_postfix = (~l=?, sort, prec) => mk(Mold.mk_postfix(~l?, sort, prec));
// let mk_infix = (sort, prec) => mk(Mold.mk_infix(sort, prec));
// // todo: incorporate filled
// let length = _ => 1;
// let is_empty = g => g.filled == "";
// let tip = (d, g) =>
//   Mold.tips(d, g.proto.mold)
//   |> ListUtil.hd_opt
//   |> OptUtil.get_or_raise(Invalid);
// let of_tips = (~filled, l: Tip.t, s: Sort.o, r: Tip.t) =>
//   mk(~filled, Mold.of_tips(l, s, r));
// let merge = (l, r) =>
//   is_empty(l) || is_empty(r)
//     // saving l sort is arbitrary, not sure if this will cause issues
//     ? Some(
//         of_tips(
//           ~filled=l.filled ++ r.filled,
//           tip(L, l),
//           l.proto.mold.sort,
//           tip(R, r),
//         ),
//       )
//     : None;
// // let has_sugg = g => g.sugg != "";
// // let is_hole = g => !has_sugg(g);
// let uncons_char = (g: t): option((t, t)) => {
//   open OptUtil.Syntax;
//   let* (hd, tl) = StringUtil.uncons(g.filled);
//   tl == "" ? None : Some(({...g, filled: hd}, {...g, filled: tl}));
// };
// let unsnoc_char = (g: t): option((t, t)) => {
//   open OptUtil.Syntax;
//   let* (tl, hd) = StringUtil.unsnoc(g.filled);
//   tl == "" ? None : Some(({...g, filled: tl}, {...g, filled: hd}));
// };
// let zip = (l, r) => {...l, filled: l.filled ++ r.filled};
// let unzip = (n: int, g: t): Either.t(Dir.t, (t, t)) =>
//   Token.unzip(n, g.filled)
//   |> Either.map_r(((l, r)) => ({...g, filled: l}, {...g, filled: r}));
// // let unzip = (n, g): Either.t(Dir.t, (t, t)) =>
// //   switch (n) {
// //   | 0 => L(L)
// //   // todo: unicode length
// //   | _ when n >= String.length(g.filled) => L(R)
// //   | _ =>
// //     let (l, r) = Token.split(n, g.filled);
// //     R(({...g, filled: l}, {...g, filled: r}));
// //   };
