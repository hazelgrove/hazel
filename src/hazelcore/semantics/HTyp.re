open Sexplib.Std;

[@deriving sexp]
type primitive =
  | Hole
  | Unit
  | Num
  | Bool;

/* types with holes */
[@deriving (sexp, map)]
type composite('a) =
  | Primitive('a)
  | Arrow(composite('a), composite('a))
  | Sum(composite('a), composite('a))
  | Prod(tuple('a))
  | List(composite('a))
and tuple('a) = (composite('a), composite('a), list(composite('a)));

[@deriving sexp]
type t = composite(primitive);

module Tuple = {
  type t('a) = tuple('a);

  let list_of_tuple = ((ty1, ty2, tys): t('a)): list(composite('a)) => [
    ty1,
    ty2,
    ...tys,
  ];

  let tuple_of_list: list(composite('a)) => composite('a) =
    fun
    | [ty1, ty2, ...tys] => Prod((ty1, ty2, tys))
    | _ => failwith("tuple_of_list: expected at least 2 elements");
};

let rec fold = (combine: ('a, 'a) => 'a, ty: composite('a)): 'a => {
  let _fold = fold(combine);
  switch (ty) {
  | Primitive(p) => p
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => combine(_fold(ty1), _fold(ty2))
  | Prod((ty1, ty2, tys)) =>
    [ty2, ...tys] |> List.map(_fold) |> List.fold_left(combine, _fold(ty1))
  | List(ty) => _fold(ty)
  };
};

let rec combine_with =
        (f: ('a, 'b) => 'c, ty1: composite('a), ty2: composite('b))
        : option(composite('c)) => {
  let _combine_with = combine_with(f);
  switch (ty1, ty2) {
  | (Primitive(p1), Primitive(p2)) => Some(Primitive(f(p1, p2)))
  | (Arrow(ty1a, ty2a), Arrow(ty1b, ty2b)) =>
    OptUtil.map2(
      (combi_arrow1, combi_arrow2) => Arrow(combi_arrow1, combi_arrow2),
      _combine_with(ty1a, ty2a),
      _combine_with(ty1b, ty2b),
    )
  | (Sum(ty1a, ty2a), Sum(ty1b, ty2b)) =>
    OptUtil.map2(
      (combi_sum1, combi_sum2) => Sum(combi_sum1, combi_sum2),
      _combine_with(ty1a, ty2a),
      _combine_with(ty1b, ty2b),
    )
  | (Prod((ty1a, ty2a, tysa)), Prod((ty1b, ty2b, tysb))) =>
    ListUtil.combine_with(_combine_with, tysa, tysb)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> OptUtil.map3(
         (combi_ty1, combi_ty2, combi_list) =>
           Prod((combi_ty1, combi_ty2, combi_list)),
         _combine_with(ty1a, ty1b),
         _combine_with(ty2a, ty2b),
       )
  | (List(ty1), List(ty2)) =>
    Option.map(combi_type => List(combi_type), _combine_with(ty1, ty2))
  | (_, _) => None
  };
};

/* type consistency */
let consistent = (x, y) =>
  combine_with((p1, p2) => p1 == Hole || p2 == Hole || p1 == p2, x, y)
  |> Option.map(combi_type => combi_type |> fold((&&)))
  |> Option.value(~default=false);

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

/* matched arrow types */
let matched_arrow =
  fun
  | Primitive(Hole) => Some((Primitive(Hole), Primitive(Hole)))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let has_matched_arrow =
  fun
  | Primitive(Hole) => true
  | Arrow(_) => true
  | _ => false;

/* matched sum types */
let matched_sum =
  fun
  | Primitive(Hole) => Some((Primitive(Hole), Primitive(Hole)))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

let has_matched_sum =
  fun
  | Primitive(Hole) => true
  | Sum(_) => true
  | _ => false;

/* matched list types */
let matched_list =
  fun
  | Primitive(Hole) => Some(Primitive(Hole))
  | List(ty) => Some(ty)
  | _ => None;

let has_matched_list =
  fun
  | Primitive(Hole) => true
  | List(_) => true
  | _ => false;
