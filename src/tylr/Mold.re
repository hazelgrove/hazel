open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  sort: Sort.o,
  prec: Prec.t,
  [@opaque]
  frames: Gram.Frame.s(Sort.o),
  // range: (int, int),
};

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

let sort_ = m => m.sort;
let prec_ = m => m.prec;

let mk = (~frames=Gram.Frame.empty, sort, prec) => {sort, prec, frames};

let mk_operand = sort => mk(sort, Prec.max);
let mk_infix = (~l: option(Sort.o)=?, ~r: option(Sort.o)=?, sort, prec) => {
  let l = Option.value(l, ~default=sort);
  let r = Option.value(r, ~default=sort);
  mk(~frames=[Seq_([Atom(Kid(l))], [Atom(Kid(r))])], sort, prec);
};

let default_operand = mk_operand(None);
let default_infix = mk_infix(None, Prec.max_op);

let init = (sort, prec) => mk(sort, prec);

let push = (f, m) => {...m, frames: [f, ...m.frames]};

// tips across alternatives
let tips = (d: Dir.t, m: t): list(Tip.t) =>
  Gram.Frame.interior(d, m.frames)
  |> List.map(
       fun
       | None => Tip.Convex
       | Some(Gram.Atom.Tok(_)) => raise(Gram.Ill_typed)
       | Some(Kid(s)) => {
           // todo: this should be no_tokens
           let p = Gram.Frame.nullable(d, m.frames) ? m.prec : Prec.min;
           Concave(s, p);
         },
     );
let tip = (d: Dir.t, m: t): Tip.t =>
  ListUtil.hd_opt(tips(d, m))
  |> OptUtil.get_or_fail("expecting at least one tip");

let must_match = (d: Dir.t, m: t): bool =>
  Gram.Frame.must_match(d, m.frames);

let concave_tips = (side, m) =>
  m
  |> tips(side)
  |> List.filter_map(
       fun
       | Tip.Concave(s, p) => Some((s, p))
       | Convex => None,
     );

// todo: need to review this and completion
let expected = (~side as _: Dir.t, _) => failwith("todo Mold.expected");
