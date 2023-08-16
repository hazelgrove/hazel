open Sexplib.Std;

[@deriving sexp]
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
  | List(list(t));

// How to replace this function?
let rec constrains = (c: t, ty: Typ.t): bool =>
  switch (
    c,
    Typ.weak_head_normalize(Builtins.ctx(Builtins.Pervasives.builtins), ty),
  ) {
  | (Truth, _)
  | (Falsity, _)
  | (Hole, _) => true
  | (Int(_) | NotInt(_), Int) => true
  | (Int(_) | NotInt(_), _) => false
  | (Float(_) | NotFloat(_), Float) => true
  | (Float(_) | NotFloat(_), _) => false
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
  | (List(c), ty) =>
    List.fold_left((last, x) => last && constrains(x, ty), true, c)
  };

let rec or_constraints = (lst: list(t)): t =>
  switch (lst) {
  | [] => failwith("should have at least one constraint")
  | [xi] => xi
  | [xi, ...xis] => Or(xi, or_constraints(xis))
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
  | String(n) => NotString(n)
  | NotString(n) => String(n)
  | And(c1, c2) => Or(dual(c1), dual(c2))
  | Or(c1, c2) => And(dual(c1), dual(c2))
  | InjL(c1) => Or(InjL(dual(c1)), InjR(Truth))
  | InjR(c2) => Or(InjR(dual(c2)), InjL(Truth))
  // Thought: generate all combinations of ways to dual the list (2^n - 1), and connect them with or
  | List(l) =>
    let permutation = List.map(l' => List(l'), List.tl(permutate(l)));
    // ``Or'' is associative, so I try to use this way to simplify it
    or_constraints(permutation);
  }
and permutate = (l: list(t)): list(list(t)) =>
  switch (l) {
  | [] => [[]]
  | [hd, ...tl] =>
    let result_tl = permutate(tl);
    List.map(l' => [hd, ...l'], result_tl)
    @ List.map(l' => [dual(hd), ...l'], result_tl);
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
  | List(l) => List(List.map(c => truify(c), l))
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
  | List(l) => List(List.map(c => falsify(c), l))
  };

let unwrapL =
  fun
  | InjL(c) => c
  | _ => failwith("input can only be InjL(_)");

let unwrapR =
  fun
  | InjR(c) => c
  | _ => failwith("input can only be InjR(_)");

let unwrap_list =
  fun
  | List(l) => l
  | _ => failwith("input can only be List([_, ..._])");
