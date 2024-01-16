open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Truth
  | Falsity
  | Hole
  | Int(int)
  | NotInt(int)
  | Float(float)
  | NotFloat(float)
  | String(string)
  | NotString(string)
  | And(t, t)
  | Or(t, t)
  | InjL(t)
  | InjR(t)
  | Pair(t, t);

let rec dual = (c: t): t =>
  switch (c) {
  | Truth => Falsity
  | Falsity => Truth
  | Hole => Hole
  | Int(n) => NotInt(n)
  | NotInt(n) => Int(n)
  | Float(n) => NotFloat(n)
  | NotFloat(n) => Float(n)
  | String(s) => NotString(s)
  | NotString(s) => String(s)
  | And(c1, c2) => Or(dual(c1), dual(c2))
  | Or(c1, c2) => And(dual(c1), dual(c2))
  | InjL(c1) => Or(InjL(dual(c1)), InjR(Truth))
  | InjR(c2) => Or(InjR(dual(c2)), InjL(Truth))
  | Pair(c1, c2) =>
    Or(
      Pair(c1, dual(c2)),
      Or(Pair(dual(c1), c2), Pair(dual(c1), dual(c2))),
    )
  };

/** substitute Truth for Hole */
let rec truify = (c: t): t =>
  switch (c) {
  | Hole => Truth
  | Truth
  | Falsity
  | Int(_)
  | NotInt(_)
  | Float(_)
  | NotFloat(_)
  | String(_)
  | NotString(_) => c
  | And(c1, c2) => And(truify(c1), truify(c2))
  | Or(c1, c2) => Or(truify(c1), truify(c2))
  | InjL(c) => InjL(truify(c))
  | InjR(c) => InjR(truify(c))
  | Pair(c1, c2) => Pair(truify(c1), truify(c2))
  };

/** substitute Falsity for Hole */
let rec falsify = (c: t): t =>
  switch (c) {
  | Hole => Falsity
  | Truth
  | Falsity
  | Int(_)
  | NotInt(_)
  | Float(_)
  | NotFloat(_)
  | String(_)
  | NotString(_) => c
  | And(c1, c2) => And(falsify(c1), falsify(c2))
  | Or(c1, c2) => Or(falsify(c1), falsify(c2))
  | InjL(c) => InjL(falsify(c))
  | InjR(c) => InjR(falsify(c))
  | Pair(c1, c2) => Pair(falsify(c1), falsify(c2))
  };

let is_injL =
  fun
  | InjL(_) => true
  | _ => false;

let is_injR =
  fun
  | InjR(_) => true
  | _ => false;

let unwrapL =
  fun
  | InjL(c) => c
  | _ => failwith("input can only be InjL(_)");

let unwrapR =
  fun
  | InjR(c) => c
  | _ => failwith("input can only be InjR(_)");

let unwrap_pair =
  fun
  | Pair(c1, c2) => (c1, c2)
  | _ => failwith("input can only be pair(_, _)");

let rec or_constraints = (lst: list(t)): t =>
  switch (lst) {
  | [] => Falsity
  | [xi] => xi
  | [xi, ...xis] => Or(xi, or_constraints(xis))
  };

// // Because `Statics.uexp_to_info_map` generates a nested AND structure as part
// // of its output, we need a function to revert it into lists.
// let rec unwrap_and = (x: t): list(t) =>
//   switch (x) {
//   | And(lhs, rhs) => [lhs, ...unwrap_and(rhs)]
//   | Truth => []
//   | _ => [x]
//   };

// Temporary name
let rec ctr_of_nth_variant = (num_variants, nth): (t => t) =>
  if (num_variants == 1) {
    Fun.id;
  } else if (nth == 0) {
    xi => InjL(xi);
  } else {
    xi => InjR(xi |> ctr_of_nth_variant(num_variants - 1, nth - 1));
  };

let of_ap = (ctx, ctr: option(Constructor.t), arg: t): t =>
  switch (ctr) {
  | Some(name) =>
    switch (Ctx.lookup_ctr(ctx, name)) {
    | None => Falsity // TODO: review
    | Some({num_variants, nth, _}) =>
      arg |> ctr_of_nth_variant(num_variants, nth)
    }
  | None => Falsity // TODO: review
  };

let of_ctr = (ctx, name) => of_ap(ctx, Some(name), Truth);

let rec to_upat_term = (xi, ctx, ty): Term.UPat.term => {
  let to_upat = (term): Term.UPat.t => {ids: [], term};
  let rec unwrap_sum = (xi, num_variants, acc) =>
    if (acc == num_variants - 1) {
      (xi, acc);
    } else {
      switch (xi) {
      | InjL(xi) => (xi, acc)
      | InjR(xi) => unwrap_sum(xi, num_variants, acc + 1)
      | _ => assert(false) // impossible
      };
    };
  switch (xi, Typ.weak_head_normalize(ctx, ty)) {
  | (
      Falsity | Hole | And(_) | Or(_) | NotInt(_) | NotFloat(_) | NotString(_),
      _,
    ) =>
    assert(false) // impossible
  | (Truth, _) => Wild
  | (Int(n), _) => Int(n)
  | (Float(n), _) => Float(n)
  | (String(s), _) => String(s)
  | (InjL(_), Bool) => Bool(true)
  | (InjR(_), Bool) => Bool(false)
  | (InjL(_), List(_)) => ListLit([])
  | (InjR(Truth), List(_)) => Cons(Wild |> to_upat, Wild |> to_upat)
  | (InjR(Pair(xi_first, xi_rest)), List(ty)) =>
    let upat_first = to_upat_term(xi_first, ctx, ty) |> to_upat;
    switch (to_upat_term(xi_rest, ctx, List(ty))) {
    | ListLit(upat_rest) => ListLit([upat_first, ...upat_rest])
    | upat_rest => Cons(upat_first, upat_rest |> to_upat)
    };
  | (Pair(xi1, xi2), Prod([ty1, ty2])) =>
    Tuple([
      to_upat_term(xi1, ctx, ty1) |> to_upat,
      to_upat_term(xi2, ctx, ty2) |> to_upat,
    ])
  | (Pair(xi_first, xi_rest), Prod([ty_first, ...ty_rest])) =>
    switch (to_upat_term(xi_rest, ctx, Prod(ty_rest))) {
    | Tuple(upat_rest) =>
      Tuple([to_upat_term(xi_first, ctx, ty_first) |> to_upat, ...upat_rest])
    | _ => assert(false) // impossible
    }
  | (Pair(_), _) => assert(false) // impossible
  | (_, Sum(sum_map)) =>
    let (xi, nth) = unwrap_sum(xi, ConstructorMap.cardinal(sum_map), 0);
    switch (List.nth(sum_map, nth)) {
    | (ctr, None) => Constructor(ctr)
    | (ctr, Some(ty)) =>
      Ap(Constructor(ctr) |> to_upat, to_upat_term(xi, ctx, ty) |> to_upat)
    }; // TODO: + functionality in ConstructorMap.re
  | (InjL(_) | InjR(_), _) => assert(false) // TBD
  };
};
