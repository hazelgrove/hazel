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
  | Bool(bool) // Wrong
  | NotBool(bool) // Wrong
  | String(string)
  | NotString(string)
  | And(t, t)
  | Or(t, t)
  | InjL(t)
  | InjR(t)
  | Pair(t, t);
// | Tuple(list(t));

// Unused
let rec constrains = (c: t, ty: Typ.t): bool =>
  switch (c, Typ.weak_head_normalize(Builtins.ctx_init, ty)) {
  | (Truth, _)
  | (Falsity, _)
  | (Hole, _) => true
  | (Int(_) | NotInt(_), Int) => true
  | (Int(_) | NotInt(_), _) => false
  | (Float(_) | NotFloat(_), Float) => true
  | (Float(_) | NotFloat(_), _) => false
  | (Bool(_) | NotBool(_), Bool) => true
  | (Bool(_) | NotBool(_), _) => false
  | (String(_) | NotString(_), String) => true
  | (String(_) | NotString(_), _) => false
  | (And(c1, c2), _) => constrains(c1, ty) && constrains(c2, ty)
  | (Or(c1, c2), _) => constrains(c1, ty) && constrains(c2, ty)
  // Treates sum as if it is left associative
  | (InjL(c1), Sum(map)) =>
    switch (List.hd(map)) {
    | (_, Some(ty1)) => constrains(c1, ty1)
    | _ => false
    }
  | (InjL(_), _) => false
  | (InjR(c2), Sum(map)) =>
    switch (List.tl(map)) {
    | [] => false
    | [(_, Some(ty2))] => constrains(c2, ty2)
    | map' => constrains(c2, Sum(map'))
    }
  | (InjR(_), _) => false
  | (Pair(c1, c2), Prod([ty_hd, ...ty_tl])) =>
    constrains(c1, ty_hd) && constrains(c2, Prod(ty_tl))
  | (Pair(_), _) => false
  // | (Tuple(cs), Prod(tys)) when List.length(cs) == List.length(tys) =>
  //   List.for_all2(constrains, cs, tys)
  // | (Tuple(_), _) => false
  };

let rec dual = (c: t): t =>
  switch (c) {
  | Truth => Falsity
  | Falsity => Truth
  | Hole => Hole
  | Int(n) => NotInt(n)
  | NotInt(n) => Int(n)
  | Float(n) => NotFloat(n)
  | NotFloat(n) => Float(n)
  | Bool(b) => NotBool(b)
  | NotBool(b) => Bool(b)
  | String(n) => NotString(n)
  | NotString(n) => String(n)
  | And(c1, c2) => Or(dual(c1), dual(c2))
  | Or(c1, c2) => And(dual(c1), dual(c2))
  | InjL(c1) => Or(InjL(dual(c1)), InjR(Truth))
  | InjR(c2) => Or(InjR(dual(c2)), InjL(Truth))
  | Pair(c1, c2) =>
    Or(
      Pair(c1, dual(c2)),
      Or(Pair(dual(c1), c2), Pair(dual(c1), dual(c2))),
    )
  // | Tuple(cs) => // TODO
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
  | Bool(_)
  | NotBool(_)
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
  | Bool(_)
  | NotBool(_)
  | String(_)
  | NotString(_) => c
  | And(c1, c2) => And(falsify(c1), falsify(c2))
  | Or(c1, c2) => Or(falsify(c1), falsify(c2))
  | InjL(c) => InjL(falsify(c))
  | InjR(c) => InjR(falsify(c))
  | Pair(c1, c2) => Pair(falsify(c1), falsify(c2))
  };

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
    | None => Hole // TODO: review
    | Some({num_variants, nth, _}) =>
      arg |> ctr_of_nth_variant(num_variants, nth)
    }
  | None => Hole // TODO: review
  };

let of_ctr = (ctx, name) => of_ap(ctx, Some(name), Truth);
