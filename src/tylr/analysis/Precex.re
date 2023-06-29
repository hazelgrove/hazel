open Util;

module Bounding_lbl = {
  type t = (Label.t, Prec.Bound.s);
  let mk_bound = (p: Prec.t, ctx: Regex.Ctx.t(_), side: Dir.t) =>
    // might want to review whether nullability can safely
    // guarantee max prec tip
    switch () {
    | () when nullable(side, ctx) => Prec.Bound.Max
    | () when nullable_tok(side, ctx) => Mid(p)
    | () => Min
    };
  let mk = (p: Prec.t, ctx: Regex.Ctx.t(_), lbl: Label.t) => {
    let bound = mk_bound(p, ctx);
    let bounds = (bound(L), bound(R));
    (lbl, bounds);
  };
};

type t = Regex.t(Bounding_lbl.t);

// module Ctx = {
//   type t = Regex.Ctx.t(Atom.t);

//   let bounds = (p: Prec.t, ctx: t) =>
//     (bound(L, p, ctx), bound(R, p, ctx));
// };

let of_table = (tbl: Table.t) =>
  tbl
  |> List.mapi((p, (r, _)) =>
    r
    |> Regex.fold_zipper(
      ~atom=ctx => Regex.Atom.map_tok(Bounding_lbl.mk(p, ctx)),
      ~star=(_ => Regex.star),
      ~seq=(_ => Regex.seq),
      ~alt=(_ => Regex.alt),
    )
  )
  |> Regex.alt;12

let get = (s: Sort.t) =>
  of_table(Sort.Map.find(s, Grammar.v));

module Ctx = {
  type t = Regex.Ctx.t(Bounding_lbl.t);
  let bounds = ((l, r): Prec.Bound.s, ctx: t): Prec.Bound.s => {
    open OptUtil.Syntax;
    switch (Regex.Ctx.nearest_toks(ctx)) {
    | ([])
    }


    let l =
      Regex.Ctx.nearest_toks(ctx)
      |> List.map(())



    let+ (_, (_, ))
  }
}

// type t = Prec.Table.t(Regex.t(Sort.t));
// precex + prec.ctx ==> regex
// prec.ctx is what determines which prec levels may be entered
// eg prec.ctx (mult, min) would not allow entry from left into plus form
// Lt(plus) '+' Gt(plus)

// module Framed = {
//   type t = Regex.t(Prec.Framed.t(Sort.t));
// };
// module Bounded = {
//   type t = Regex.t(Prec.Bounded.t(Sort.t));
// };

// let frame = (s: Sort.t, p: t): Framed.t => {
//   Prec.Table.bindings(p)
//   |> List.map(((p, (r, a))) => )
// };

// let bound = ((_l, _r): Prec.Bounds.t, _p: Framed.t): Bounded.t =>
//   failwith("todo bound");

// module Ctx = {
//   type t = Regex.Unzipped.s(Prec.Framed.t(Sort.t));
// };
