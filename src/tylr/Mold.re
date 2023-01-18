open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sort: Sort.t,
  prec: Prec.t,
  frames: Gram.Frame.s(Sort.t),
  // range: (int, int),
};

// derived
// let plus: t = {
//   sort: Exp,
//   prec: 0,
//   frames: [Seq_([Kid(Exp)], [Kid(Exp)])],
// };

let sort_ = m => m.sort;
let prec_ = m => m.prec;

let mk = (~frames=Gram.Frame.empty, sort, prec) => {sort, prec, frames};

let mk_infix = (~l=?, ~r=?, sort, prec) => {
  let l = Option.value(l, ~default=sort);
  let r = Option.value(r, ~default=sort);
  mk(~frames=[Seq_([Atom(Kid(l))], [Atom(Kid(r))])], sort, prec);
};

let init = (sort, prec) => mk(sort, prec);

let push = (f, m) => {...m, frames: [f, ...m.frames]};

// let tips = (d: Dir.t, m: t): list(Tip.t) =>
//   Grex.Frame.interior(d, m.frames)
//   |> List.filter_map(
//        fun
//        | None => Some(Convex)
//        | Some(Kid(s)) => Some(Concave(s))
//        | Some(Tok(_)) => raise(Grex.No_consecutive_tokens),
//      );

let must_match = (d: Dir.t, m: t): bool =>
  Gram.Frame.must_match(d, m.frames);

// todo: see if use of this is just must_match
let matching = (_: Dir.t, _) => failwith("todo matching");

let expected_sort = (side: Dir.t, m: t) =>
  Gram.Frame.interior(side, m.frames)
  |> List.filter_map(
       fun
       | Some(Gram.Atom.Kid(s)) => Some(s)
       | _ => None,
     )
  |> ListUtil.hd_opt;

module Result = {
  type m = t;
  type kid = option(Sort.t);
  type t = Result.t(m, kid);
};
