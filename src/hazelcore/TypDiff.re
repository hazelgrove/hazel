/**
 * Gives the list of steps to which parts of two types is the source of
 * a type inconsistency. If there is no type inconsistency, the list
 * of steps will be empty.
 * For example, for Int -> (Bool -> Int) and Int -> Float this function would
 * give the paths for (Bool -> Int) and Float.
 */
let rec mk =
        (ty1: HTyp.t, ty2: HTyp.t)
        : (list(CursorPath.steps), list(CursorPath.steps)) => {
  let diff_subtypes = (subtype_step, (ty1, ty2)) =>
    TupleUtil.map2(List.map(List.cons(subtype_step)), mk(ty1, ty2));
  switch (HTyp.to_syntax(ty1), HTyp.to_syntax(ty2)) {
  | (Hole, _)
  | (_, Hole)
  | (TyVarHole(_), _)
  | (_, TyVarHole(_))
  | (Int, Int)
  | (Float, Float)
  | (Bool, Bool) => ([], [])
  // TODO: add ctx to TyVar
  | (TyVar(i, _), TyVar(j, _)) => i == j ? ([], []) : ([[]], [[]])
  | (TyVar(_), _)
  | (_, TyVar(_)) => ([], [])
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    let (steps1, steps1') =
      diff_subtypes(0, (HTyp.of_syntax(ty1), HTyp.of_syntax(ty1')));
    let (steps2, steps2') =
      diff_subtypes(1, (HTyp.of_syntax(ty2), HTyp.of_syntax(ty2')));
    (steps1 @ steps2, steps1' @ steps2');
  | (Prod(tys), Prod(tys')) =>
    if (List.length(tys) != List.length(tys')) {
      ([[]], [[]]);
    } else {
      List.combine(tys, tys')
      |> List.mapi((i, (ty, ty')) =>
           diff_subtypes(i, (HTyp.of_syntax(ty), HTyp.of_syntax(ty')))
         )
      |> List.split
      |> TupleUtil.map2(List.flatten);
    }
  | (List(ty), List(ty')) =>
    diff_subtypes(0, (HTyp.of_syntax(ty), HTyp.of_syntax(ty')))
  | (Int, _)
  | (Float, _)
  | (Bool, _)
  | (Arrow(_), _)
  | (Sum(_), _)
  | (Prod(_), _)
  | (List(_), _) => ([[]], [[]])
  };
};
