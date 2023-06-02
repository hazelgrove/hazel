type t = Prec.Table.t(Regex.t(Sort.t));
// precex + prec.ctx ==> regex
// prec.ctx is what determines which prec levels may be entered
// eg prec.ctx (mult, min) would not allow entry from left into plus form
// Lt(plus) '+' Gt(plus)

module Framed = {
  type t = Regex.t(Prec.Framed.t(Sort.t));
};
module Bounded = {
  type t = Regex.t(Prec.Bounded.t(Sort.t));
};


  switch (r) {
  | Atom(Tok(_)) => r
  | Atom(Kid(s)) =>
    let null_l = Regex.Unzipped.nullable(L, ctx);
    let null_r = Regex.Unzipped.nullable(R, ctx);

  }


let frame = (s: Sort.t, p: t): Framed.t => {
  Prec.Table.bindings(p)
  |> List.map(((p, (r, a))) => )
};

let bound = ((_l, _r): Prec.Bounds.t, _p: Framed.t): Bounded.t =>
  failwith("todo bound");

module Ctx = {
  type t = Regex.Unzipped.s(Prec.Framed.t(Sort.t));
};