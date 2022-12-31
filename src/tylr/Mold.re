type t = {
  sort: Sort.t,
  prec: Prec.t,
  frames: Gram.Frame.s,
};

// derived
let plus: t = {
  sort: Exp,
  prec: 0,
  frames: [Seq_([Kid(Exp)], [Kid(Exp)])],
};

let init = (sort, prec) => {sort, prec, frames: []};

let push = (f, m) => {...m, frames: [f, ...m.frames]};

let tips = (d: Dir.t, m: t): list(Tip.t) =>
  Gram.Frame.interior(d, m.frames)
  |> List.filter_map(
       fun
       | None => Some(Convex)
       | Some(Kid(s)) => Some(Concave(s))
       | Some(Tok(_)) => raise(Gram.No_consecutive_tokens),
     );

let must_match = (d: Dir.t, m: t): bool =>
  Gram.Frame.must_match(d, m.frames);
