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

let rec ctr_of_nth_variant = (num_variants, nth): (t => t) =>
  if (num_variants == 1) {
    Fun.id;
  } else if (nth == 0) {
    xi => InjL(xi);
  } else {
    xi => InjR(xi |> ctr_of_nth_variant(num_variants - 1, nth - 1));
  };

let of_ap = (ctx, mode, ctr: option(Constructor.t), arg: t, syn_ty): t =>
  switch (ctr) {
  | Some(name) =>
    let ty =
      switch (mode) {
      | Mode.Ana(ty) => Some(ty)
      | Syn => syn_ty
      | _ => None
      };
    switch (ty) {
    | Some(ty) =>
      switch (Typ.weak_head_normalize(ctx, ty)) {
      | Rec(_, Sum(map))
      | Sum(map) =>
        let num_variants = ConstructorMap.cardinal(map);
        switch (ConstructorMap.nth(map, name)) {
        | Some(nth) => arg |> ctr_of_nth_variant(num_variants, nth)
        | None => Falsity
        };
      | _ => Falsity
      }
    | None => Falsity
    };
  | None => Falsity
  };

let of_ctr = (ctx, mode, name, self) => {
  let syn_ty =
    switch (self) {
    | Self.IsConstructor({syn_ty, _}) => syn_ty
    | _ => assert(false) // impossible
    };
  of_ap(ctx, mode, Some(name), Truth, syn_ty);
};
