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
  | ConstInj(HTyp.sum_body, UHTag.t)
  | ArgInj(HTyp.sum_body, UHTag.t, t)
  | Pair(t, t);

// Lists
let nil_tag = UHTag.Tag(NotInTagHole, "Nil");
let cons_tag = UHTag.Tag(NotInTagHole, "Cons");
let list_tagmap: TagMap.t(option(HTyp.t)) =
  TagMap.of_list([(nil_tag, None), (cons_tag, Some(HTyp.Hole))]);
let nil_constraint = ConstInj(Finite(list_tagmap), nil_tag);
let cons_constraint = (c: t) => ArgInj(Finite(list_tagmap), cons_tag, c);

// Booleans
let false_tag = UHTag.Tag(NotInTagHole, "False");
let true_tag = UHTag.Tag(NotInTagHole, "True");
let bool_tagmap: TagMap.t(option(HTyp.t)) =
  TagMap.of_list([(false_tag, None), (true_tag, None)]);
let false_constraint = ConstInj(Finite(bool_tagmap), false_tag);
let true_constraint = ConstInj(Finite(bool_tagmap), true_tag);

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
  | (ConstInj(sum_body, tag), Sum(_)) =>
    {
      open OptUtil.Syntax;
      let* tymap0 = HTyp.matched_finite_sum(Sum(sum_body));
      let+ ty_arg_opt = TagMap.find_opt(tag, tymap0);
      Option.is_none(ty_arg_opt);
    }
    |> Option.value(~default=false)
  | (ArgInj(sum_body, tag, c_arg), Sum(_)) =>
    {
      open OptUtil.Syntax;
      let* tymap0 = HTyp.matched_finite_sum(Sum(sum_body));
      let* ty_arg_opt = TagMap.find_opt(tag, tymap0);
      let+ ty_arg = ty_arg_opt;
      constrains(c_arg, ty_arg);
    }
    |> Option.value(~default=false)
  | (ConstInj(_) | ArgInj(_), _) => false
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
  | ConstInj(sum_body, tag) => dual_injs_constraint(sum_body, tag)
  | ArgInj(sum_body, tag, c_arg) =>
    Or(
      ArgInj(sum_body, tag, dual(c_arg)),
      dual_injs_constraint(sum_body, tag),
    )
  | Pair(c1, c2) =>
    Or(
      Pair(c1, dual(c2)),
      Or(Pair(dual(c1), c2), Pair(dual(c1), dual(c2))),
    )
  }
and dual_injs_constraint = (sum_body: HTyp.sum_body, tag0: UHTag.t): t => {
  let binding_to_constraint = ((tag, ty_arg_opt): TagMap.binding(_)): t => {
    switch (ty_arg_opt) {
    | None => ConstInj(sum_body, tag)
    | Some(_) => ArgInj(sum_body, tag, Truth)
    };
  };
  switch (HTyp.matched_finite_sum(Sum(sum_body))) {
  | None => failwith(__LOC__ ++ ": impossible matched_finite_sum failure")
  | Some(tymap) =>
    let other_injs =
      switch (tag0) {
      | Tag(InTagHole(_), _)
      | EmptyTagHole(_) =>
        TagMap.bindings(tymap)
        |> List.filter(((tag, _)) =>
             switch (tag) {
             | UHTag.Tag(NotInTagHole, _) => true
             | Tag(InTagHole(_), _)
             | EmptyTagHole(_) => false
             }
           )
        |> List.map(binding_to_constraint)
      // List.map(binding_to_constraint, TagMap.bindings(tymap));
      | Tag(NotInTagHole, _) =>
        TagMap.bindings(tymap)
        |> List.filter(((tag, _)) => !UHTag.equal(tag, tag0))
        |> List.map(binding_to_constraint)
      };
    let result =
      switch (other_injs) {
      | [] => Falsity
      | [c1, ...cs] => List.fold_right((ci, join) => Or(ci, join), cs, c1)
      };
    print_endline("CONSTRAINTS dual_inj_constraint");
    print_endline(Sexplib.Sexp.to_string_hum(UHTag.sexp_of_t(tag0)));
    print_endline(
      Sexplib.Sexp.to_string_hum(HTyp.sexp_of_sum_body(sum_body)),
    );
    print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(result)));
    result;
  };
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
  | ConstInj(_) => c
  | ArgInj(tagmap, tag, c_arg) => ArgInj(tagmap, tag, truify(c_arg))
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
  | ConstInj(_) => c
  | ArgInj(sum_body, tag, c_arg) => ArgInj(sum_body, tag, falsify(c_arg))
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
