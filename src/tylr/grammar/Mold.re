// open Util;

include Gram.Zipper;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Gram.Zipper.t(Label.t);

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

let sort_ = m => m.sort;
let prec_ = m => m.prec;

let label = m => fst(m.zipper);

let mk = (~ctx=Regex.Ctx.empty, sort, prec, lbl) => {
  sort,
  prec,
  zipper: (lbl, ctx),
};

let mk_operand = sort => mk(sort, Prec.max);
let mk_prefix = (~r=?, sort, prec) => {
  let r = Option.value(r, ~default=sort);
  mk(~ctx=[Seq_([], [Atom(Kid(r))])], sort, prec);
};
let mk_postfix = (~l=?, sort, prec) => {
  let l = Option.value(l, ~default=sort);
  mk(~ctx=[Seq_([Atom(Kid(l))], [])], sort, prec);
};
let mk_infix = (~l=?, ~r=?, sort, prec) => {
  let l = Option.value(l, ~default=sort);
  let r = Option.value(r, ~default=sort);
  mk(~ctx=[Seq_([Atom(Kid(l))], [Atom(Kid(r))])], sort, prec);
};

// let of_tips = (l: Tip.t, s: Sort.o, r: Tip.t) =>
//   switch (l, r) {
//   | (Convex, Convex) => mk_operand(s)
//   | (Convex, Concave(r, p)) => mk_prefix(s, p, ~r)
//   | (Concave(l, p), Convex) => mk_postfix(~l, s, p)
//   | (Concave(l, p_l), Concave(r, p_r)) =>
//     let p_l = l == s ? p_l : Prec.max;
//     let p_r = r == s ? p_r : Prec.max;
//     mk_infix(~l, s, min(p_l, p_r), ~r);
//   };

// let default_operand = mk_operand(None);
// let default_infix = mk_infix(None, Prec.max_op);

// let init = (sort, prec) => mk(sort, prec);

// let push = (f, m) => {...m, ctx: [f, ...m.ctx]};

// tips across alternatives
// let tips = (d: Dir.t, m: t): list(Tip.t) =>
//   Gram.Frame.interior(d, m.ctx)
//   |> List.map(
//        fun
//        | None => Tip.Convex
//        | Some(Gram.Atom.Tok(_)) => raise(Gram.Ill_typed)
//        | Some(Kid(s)) => {
//            // todo: this should be no_tokens
//            let p = Gram.Frame.nullable(d, m.ctx) ? m.prec : Prec.min;
//            Concave(s, p);
//          },
//      );
// let tip = (d: Dir.t, m: t): Tip.t =>
//   ListUtil.hd_opt(tips(d, m))
//   |> OptUtil.get_or_fail("expecting at least one tip");

// let must_match = (d: Dir.t, m: t): bool =>
//   Gram.Frame.must_match(d, m.ctx);

// let convexable = (side, m) => List.mem(Tip.Convex, tips(side, m));

// let concavable = (side, m) =>
//   m
//   |> tips(side)
//   |> List.filter_map(
//        fun
//        | Tip.Concave(s, p) => Some((s, p))
//        | Convex => None,
//      )
//   // assuming there will only be one such concave tip, if any,
//   // given well-molded assumption
//   |> ListUtil.hd_opt;

// todo: need to review this and completion
// let expected = (~side as _: Dir.t, _) => failwith("todo Mold.expected");

// let eq = (l: t, r: t): option(Sort.Ana.t) => {
//   switch (Mold.tip(R, mold(l))) {
//   | Convex => None
//   | Concave(sort, _) =>
//     let (z_l, z_r) = (zipper(l), zipper(r));
//     let (moved_l, moved_r) =
//       Gram.Zipper.(move_to_tok(R, z_l), move_to_tok(L, z_r));
//     let strict = is_strict(l) || is_strict(r);
//     List.mem(z_l, moved_r) && List.mem(z_r, moved_l)
//       ? Some(Sort.Ana.mk(~strict, ~sort, ())) : None;
//   };
// };
// let eq_transitive = (l: t, r: t): bool => {
//   let rec go = (z_l, z_r) => {
//     let moved_r = Gram.Zipper.move_to_tok(L, z_r);
//     List.mem(z_l, moved_r) ? true : List.exists(go(z_l), moved_r);
//   };
//   go(zipper(l), zipper(r));
// };
let eq = (_, _) => failwith("todo Mold.eq");
