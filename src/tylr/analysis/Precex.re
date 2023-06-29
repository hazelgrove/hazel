open Util;

module Table = {
  type t = Prec.Table.t(Regex.t(Atom.t(Label.t)));
};

module Bounding_label = {
  type t = (Label.t, Prec.Bounds.t);
};
module Atom = {
  type t = Atom.t(Bounding_label.t);
};
type t = Regex.t(Atom.t);

module Ctx = {
  type t = Regex.Ctx.t(Atom.t);

  let bounds = (p: Prec.t, ctx: t) =>
    (bound(L, p, ctx), bound(R, p, ctx));
};

let bound = (p: Prec.t, side: Dir.t, ctx: Regex.Ctx.t(_)) =>
  // might want to review whether nullability can safely
  // guarantee max prec tip
  if (nullable(side, ctx)) {
    Max
  } else if (nullable_tok(side, ctx)) {
    Mid(p)
  } else {
    Min
  };

let of_table = (tbl: Table.t) =>
  tbl
  |> List.mapi((p, (r, _)) =>
    r
    |> Regex.fold_zipper(
      ~atom=(ctx, atom: Atom.t) =>
        switch (atom) {
        | Kid(_) => Regex.Atom(atom)
        | Tok(lbl) =>
          let bounds = (bound(p, L, ctx), bound(p, R, ctx));
          Tok((lbl, bounds));
        },
      ~star=(_ => Regex.star),
      ~seq=(_ => Regex.seq),
      ~alt=(_ => Regex.alt),
    )
  )
  |> Regex.alt;

let get = (s: Sort.t) =>
  of_table(Sort.Map.find(s, Grammar.v));

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
