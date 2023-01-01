type t('sort) = {
  sort: 'sort,
  prec: Prec.t,
  frames: Grex.Frame.s('sort),
};

// derived
// let plus: t = {
//   sort: Exp,
//   prec: 0,
//   frames: [Seq_([Kid(Exp)], [Kid(Exp)])],
// };

let init = (sort, prec) => {sort, prec, frames: []};

let push = (f, m) => {...m, frames: [f, ...m.frames]};

let tips = (d: Dir.t, m: t(_)): list(Tip.t(_)) =>
  Grex.Frame.interior(d, m.frames)
  |> List.filter_map(
       fun
       | None => Some(Convex)
       | Some(Kid(s)) => Some(Concave(s))
       | Some(Tok(_)) => raise(Grex.No_consecutive_tokens),
     );

let must_match = (d: Dir.t, m: t(_)): bool =>
  Grex.Frame.must_match(d, m.frames);
