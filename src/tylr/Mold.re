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

let init = (sort, prec) => {sort, prec, frames: Gram.Frame.empty};

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

let expected_sort = (_: Dir.t, _) => failwith("todo expected_sort");

module Result = {
  type m = t;
  type kid = option(Sort.t);
  type t = Result.t(m, kid);
};
