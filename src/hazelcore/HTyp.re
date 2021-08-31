open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(TagMap.t(option(t)))
  | Prod(list(t))
  | List(t);

[@deriving sexp]
type join =
  | GLB
  | LUB;

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Hole
  | Prod([])
  | Sum(_)
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */

let rec eq = (ty, ty') =>
  switch (ty, ty') {
  | (Hole, Hole) => true
  | (Hole, _) => false
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) => eq(ty1, ty1') && eq(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(tys), Sum(tys')) =>
    TagMap.equal(
      (map1, map2) =>
        switch (map1, map2) {
        | (None, None) => true
        | (None, _) => false
        | (Some(ty), Some(ty')) => eq(ty, ty')
        | (Some(_), _) => false
        },
      tys,
      tys',
    )
  | (Sum(_), _) => false
  | (Prod(tys1), Prod(tys2)) => ListUtil.equal(eq, tys1, tys2)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => eq(ty, ty')
  | (List(_), _) => false
  };

/* type consistency */
let rec consistent = (x, y) =>
  switch (x, y) {
  | (Hole, _)
  | (_, Hole) => true
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(tys), Sum(tys')) =>
    TagMap.equal(
      (ty_opt, ty_opt') =>
        switch (ty_opt, ty_opt') {
        | (None, None) => true
        | (Some(ty), Some(ty')) => consistent(ty, ty')
        | _ => false
        },
      tys,
      tys',
    )
  | (Sum(_), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.for_all2_opt(consistent, tys1, tys2)
    |> Option.value(~default=false)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

let rec consistent_all = (types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(hd), tl)) {
      false;
    } else {
      consistent_all(tl);
    }
  };

/* matched arrow types */
let matched_arrow =
  fun
  | Hole => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched list types */
let matched_list =
  fun
  | Hole => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None;

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole => false
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2) => complete(ty1) && complete(ty2)
  | Sum(tys) =>
    List.for_all(
      ((tag: UHTag.t, ty_opt)) =>
        (
          switch (tag) {
          | Tag(_) => true
          | EmptyTagHole(_) => false
          }
        )
        && (
          switch (ty_opt) {
          | Some(ty) => complete(ty)
          | None => true
          }
        ),
      tys,
    )
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec join = (j, ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Hole) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty1)
    }
  | (Hole, _) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty2)
    }
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(tys), Sum(tys')) =>
    /* if tys != tys', then sort them first */
    let (tys, tys') =
      TagMap.equal_tags(tys, tys')
        ? (tys, tys') : (TagMap.sort(tys), TagMap.sort(tys'));
    Option.map(
      joined_tys => Sum(joined_tys),
      switch (
        List.fold_left2(
          (acc_opt, (tag, ty_opt), (tag', ty_opt')) =>
            Option.bind(acc_opt, acc =>
              switch (UHTag.eq(tag, tag'), ty_opt, ty_opt') {
              | (false, _, _)
              | (true, None, None) => Some([(tag, None), ...acc])
              | (true, Some(ty), Some(ty')) =>
                Option.bind(join(j, ty, ty'), joined_ty =>
                  Some([(tag, Some(joined_ty)), ...acc])
                )
              | (_, Some(_), None)
              | (_, None, Some(_)) => None
              }
            ),
          Some([]),
          tys,
          tys',
        )
      ) {
      | opt => opt
      | exception (Invalid_argument(_)) => None
      },
    );
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt(join(j), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(j, ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all = (j: join, types: list(t)): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};
