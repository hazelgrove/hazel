open Util;

/* What the guards of a branch say about variables compared with integer
 * literals: `if n <= 0 then a else b` has `n >= 1` in `b`. Only a
 * comparison of a variable with a literal, either way round, counts,
 * through `!`, `&&` where the guard holds and `||` where it doesn't. */

type bound = {
  lo: option(Bigint.t),
  hi: option(Bigint.t),
  excluded: list(Bigint.t),
};

type t = list((Var.t, bound));

let empty: t = [];

type comparison =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge;

let negate =
  fun
  | Eq => Ne
  | Ne => Eq
  | Lt => Ge
  | Le => Gt
  | Gt => Le
  | Ge => Lt;

let rec literal = (e: Exp.t): option(Bigint.t) =>
  switch (Exp.term_of(e)) {
  | Parens(e) => literal(e)
  | Atom(Int(n) | Nat(n)) => Some(n)
  | UnOp(Int(Minus), e) => Option.map(~f=Bigint.neg, literal(e))
  | _ => None
  };

let rec variable = (e: Exp.t): option(Var.t) =>
  switch (Exp.term_of(e)) {
  | Parens(e) => variable(e)
  | Var(x) => Some(x)
  | _ => None
  };

/* `e` as `x op k`, with the variable on the left. */
let comparison = (e: Exp.t): option((Var.t, comparison, Bigint.t)) => {
  let op: option(comparison) =
    switch (Exp.term_of(e)) {
    | BinOp(Poly(Equals), _, _) => Some(Eq)
    | BinOp(Poly(NotEquals), _, _) => Some(Ne)
    | BinOp(Int(LessThan) | Nat(LessThan), _, _) => Some(Lt)
    | BinOp(Int(LessThanOrEqual) | Nat(LessThanOrEqual), _, _) => Some(Le)
    | BinOp(Int(GreaterThan) | Nat(GreaterThan), _, _) => Some(Gt)
    | BinOp(Int(GreaterThanOrEqual) | Nat(GreaterThanOrEqual), _, _) =>
      Some(Ge)
    | _ => None
    };
  switch (op, Exp.term_of(e)) {
  | (Some(op), BinOp(_, a, b)) =>
    switch (variable(a), literal(b), literal(a), variable(b)) {
    | (Some(x), Some(k), _, _) => Some((x, op, k))
    | (_, _, Some(k), Some(x)) =>
      let flipped =
        switch (op) {
        | Lt => Gt
        | Le => Ge
        | Gt => Lt
        | Ge => Le
        | (Eq | Ne) as op => op
        };
      Some((x, flipped, k));
    | _ => None
    }
  | _ => None
  };
};

let bound_of = (t: t, x: Var.t): bound =>
  List.Assoc.find(t, x, ~equal=String.equal)
  |> Option.value(
       ~default={
         lo: None,
         hi: None,
         excluded: [],
       },
     );

let add = (t: t, (x, op, k): (Var.t, comparison, Bigint.t)): t => {
  let b = bound_of(t, x);
  let raise_lo = l =>
    Some(Option.value_map(b.lo, ~default=l, ~f=lo => Bigint.max(lo, l)));
  let lower_hi = h =>
    Some(Option.value_map(b.hi, ~default=h, ~f=hi => Bigint.min(hi, h)));
  let b =
    switch (op) {
    | Eq => {
        ...b,
        lo: raise_lo(k),
        hi: lower_hi(k),
      }
    | Ne => {
        ...b,
        excluded: [k, ...b.excluded],
      }
    | Lt => {
        ...b,
        hi: lower_hi(Bigint.(k - one)),
      }
    | Le => {
        ...b,
        hi: lower_hi(k),
      }
    | Gt => {
        ...b,
        lo: raise_lo(Bigint.(k + one)),
      }
    | Ge => {
        ...b,
        lo: raise_lo(k),
      }
    };
  [(x, b), ...List.Assoc.remove(t, x, ~equal=String.equal)];
};

/* `t` where `guard` evaluates to `holds`. */
let rec assume = (t: t, guard: Exp.t, holds: bool): t =>
  switch (Exp.term_of(guard)) {
  | Parens(g) => assume(t, g, holds)
  | UnOp(Bool(Not), g) => assume(t, g, !holds)
  | BinOp(Bool(And), a, b) when holds => assume(assume(t, a, true), b, true)
  | BinOp(Bool(Or), a, b) when !holds =>
    assume(assume(t, a, false), b, false)
  | _ =>
    switch (comparison(guard)) {
    | Some((x, op, k)) => add(t, (x, holds ? op : negate(op), k))
    | None => t
    }
  };

/* `t` with nothing known of `names`, for a scope that rebinds them. */
let forget = (t: t, names: list(Var.t)): t =>
  List.filter(~f=((x, _)) => !List.mem(names, x, ~equal=String.equal), t);

/* `t` with `alias` known to be what `x` is. */
let alias = (t: t, alias: Var.t, x: Var.t): t =>
  switch (List.Assoc.find(t, x, ~equal=String.equal)) {
  | Some(b) => [(alias, b), ...forget(t, [alias])]
  | None => forget(t, [alias])
  };

/* The least value `x` can have, a natural number's floor of 0 included
 * when `nat`, past the values excluded. */
let lower = (~nat=false, t: t, x: Var.t): option(Bigint.t) => {
  let b = bound_of(t, x);
  let lo =
    switch (b.lo, nat) {
    | (Some(lo), true) => Some(Bigint.max(lo, Bigint.zero))
    | (None, true) => Some(Bigint.zero)
    | (lo, false) => lo
    };
  let rec up = l =>
    List.mem(b.excluded, l, ~equal=Bigint.equal) ? up(Bigint.(l + one)) : l;
  Option.map(~f=up, lo);
};

let upper = (t: t, x: Var.t): option(Bigint.t) => {
  let b = bound_of(t, x);
  let rec down = h =>
    List.mem(b.excluded, h, ~equal=Bigint.equal)
      ? down(Bigint.(h - one)) : h;
  Option.map(~f=down, b.hi);
};

/* Whether `t` makes the comparison `condition` true. `nat` says which
 * variables are natural numbers. */
let implies = (~nat: Var.t => bool, t: t, condition: Exp.t): bool =>
  switch (comparison(condition)) {
  | None => false
  | Some((x, op, k)) =>
    let lo = lower(~nat=nat(x), t, x);
    let hi = upper(t, x);
    let at_least = l => Option.exists(lo, ~f=lo => Bigint.(lo >= l));
    let at_most = h => Option.exists(hi, ~f=hi => Bigint.(hi <= h));
    switch (op) {
    | Ge => at_least(k)
    | Gt => at_least(Bigint.(k + one))
    | Le => at_most(k)
    | Lt => at_most(Bigint.(k - one))
    | Eq => at_least(k) && at_most(k)
    | Ne =>
      List.mem(bound_of(t, x).excluded, k, ~equal=Bigint.equal)
      || at_least(Bigint.(k + one))
      || at_most(Bigint.(k - one))
    };
  };
