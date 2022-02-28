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
  | And(t, t)
  | Or(t, t)
  | Inj(HTyp.sum_body, UHTag.t, option(t))
  | Pair(t, t);

// Lists
let nil_tag = UHTag.Tag(NotInTagHole, "Nil");
let cons_tag = UHTag.Tag(NotInTagHole, "Cons");
let list_tagmap: TagMap.t(option(HTyp.t)) =
  TagMap.of_list([(nil_tag, None), (cons_tag, Some(HTyp.Hole))]);
let nil_constraint = Inj(Finite(list_tagmap), nil_tag, None);
let cons_constraint = (c_opt: option(t)) =>
  Inj(Finite(list_tagmap), cons_tag, c_opt);

// Booleans
let false_tag = UHTag.Tag(NotInTagHole, "False");
let true_tag = UHTag.Tag(NotInTagHole, "True");
let bool_tagmap: TagMap.t(option(HTyp.t)) =
  TagMap.of_list([(false_tag, None), (true_tag, None)]);
let false_constraint = Inj(Finite(bool_tagmap), false_tag, None);
let true_constraint = Inj(Finite(bool_tagmap), true_tag, None);

let rec constrains = (c: t, ty: HTyp.t): bool =>
  switch (c, ty) {
  | (Truth, _)
  | (Falsity, _)
  | (Hole, _) => true
  | (Int(_) | NotInt(_), Int) => true
  | (Int(_) | NotInt(_), _) => false
  | (Float(_) | NotFloat(_), Float) => true
  | (Float(_) | NotFloat(_), _) => false
  | (And(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
  | (Or(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
  | (Inj(_, tag, c_arg_opt), Sum(Finite(tymap))) =>
    switch (TagMap.find_opt(tag, tymap), c_arg_opt) {
    | (None, _) => false
    | (Some(None), None | Some(Truth)) => true
    | (Some(None), _) => false
    | (Some(Some(ty_arg)), Some(c_arg)) => constrains(c_arg, ty_arg)
    | (Some(_), _) => false
    }
  | (Inj(_, tag, c_arg_opt), Sum(Elided(tag', ty_arg_opt))) =>
    UHTag.equal(tag, tag')
    && (
      switch (c_arg_opt, ty_arg_opt) {
      | (None, None)
      | (Some(Truth), None) => true
      | (None, Some(_))
      | (Some(_), None) => false
      | (Some(c_arg), Some(ty_arg)) => constrains(c_arg, ty_arg)
      }
    )
  | (Inj(_), _) => false
  | (Pair(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
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
  | And(c1, c2) => Or(dual(c1), dual(c2))
  | Or(c1, c2) => And(dual(c1), dual(c2))
  | Inj(Finite(tagmap) as sum_body, tag, c_arg_opt) =>
    let c0 =
      switch (c_arg_opt) {
      | None => Inj(sum_body, tag, None)
      | Some(c_arg) => Inj(sum_body, tag, Some(dual(c_arg)))
      };
    TagMap.bindings(tagmap)
    |> List.filter_map(((tag', _)) =>
         UHTag.equal(tag, tag')
           ? None
           : Some(
               Inj(sum_body, tag, c_arg_opt == None ? None : Some(Truth)),
             )
       )
    |> List.fold_left((cs, c) => Or(c, cs), c0);
  | Inj(Elided(tag, _) as sum_body, tag', c_arg_opt) =>
    if (UHTag.equal(tag, tag')) {
      switch (c_arg_opt) {
      | None => Inj(sum_body, tag, None)
      | Some(c_arg) => Inj(sum_body, tag, Some(dual(c_arg)))
      };
    } else {
      Inj(sum_body, tag, c_arg_opt == None ? None : Some(Truth));
    }
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
  | NotFloat(_) => c
  | And(c1, c2) => And(truify(c1), truify(c2))
  | Or(c1, c2) => Or(truify(c1), truify(c2))
  | Inj(tagmap, tag, Some(c1)) => Inj(tagmap, tag, Some(truify(c1)))
  | Inj(_, _, None) => c
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
  | NotFloat(_) => c
  | And(c1, c2) => And(falsify(c1), falsify(c2))
  | Or(c1, c2) => Or(falsify(c1), falsify(c2))
  | Inj(tagmap, tag, Some(c1)) => Inj(tagmap, tag, Some(falsify(c1)))
  | Inj(tagmap, tag, None) => Inj(tagmap, tag, Some(Falsity))
  | Pair(c1, c2) => Pair(falsify(c1), falsify(c2))
  };

let unwrap_pair =
  fun
  | Pair(c1, c2) => (c1, c2)
  | _ => failwith("input can only be pair(_, _)");

let rec or_constraints = (lst: list(t)): t =>
  switch (lst) {
  | [] => failwith("should have at least one constraint")
  | [xi] => xi
  | [xi, ...xis] => Or(xi, or_constraints(xis))
  };
