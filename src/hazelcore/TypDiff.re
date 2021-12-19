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
  // let diff_tagged_subtypes = (step, ((_, ty1), (_, ty2))) =>
  //   TupleUtil.map2(List.map(List.cons(step)), mk(ty1, ty2));
  switch (ty1, ty2) {
  | (Hole, _)
  | (_, Hole)
  | (Int, Int)
  | (Float, Float)
  | (Bool, Bool) => ([], [])
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let (steps1, steps1') = diff_subtypes(0, (ty1, ty1'));
    let (steps2, steps2') = diff_subtypes(1, (ty2, ty2'));
    (steps1 @ steps2, steps1' @ steps2');
  | (Sum(Finite(tymap)), Sum(Finite(tymap'))) =>
    let ty_opts = TagMap.bindings(tymap);
    let ty_opts' = TagMap.bindings(tymap');
    if (List.length(ty_opts) != List.length(ty_opts')
        || List.map(fst, ty_opts) != List.map(fst, ty_opts')) {
      ([[]], [[]]);
    } else {
      List.combine(List.map(snd, ty_opts), List.map(snd, ty_opts'))
      |> List.mapi((i, (ty_opt, ty_opt')) =>
           switch (ty_opt, ty_opt') {
           | (Some(ty), Some(ty')) => diff_subtypes(i, (ty, ty'))
           | (None, None) => ([], [])
           | (_, _) => ([[]], [[]])
           }
         )
      |> List.split
      |> TupleUtil.map2(List.flatten);
    };
  | (Sum(Elided(tag, ty_opt)), Sum(Elided(tag', ty_opt'))) =>
    if (UHTag.equal(tag, tag')) {
      switch (ty_opt, ty_opt') {
      | (None, None) => ([], [])
      | (Some(ty), Some(ty')) => diff_subtypes(0, (ty, ty'))
      | (None, Some(_))
      | (Some(_), None) => ([[]], [[]])
      };
    } else {
      ([[]], [[]]);
    }
  | (Prod(tys), Prod(tys')) =>
    if (List.length(tys) != List.length(tys')) {
      ([[]], [[]]);
    } else {
      List.combine(tys, tys')
      |> List.mapi((i, (ty, ty')) => diff_subtypes(i, (ty, ty')))
      |> List.split
      |> TupleUtil.map2(List.flatten);
    }
  | (List(ty), List(ty')) => diff_subtypes(0, (ty, ty'))
  | (Int, _)
  | (Float, _)
  | (Bool, _)
  | (Arrow(_), _)
  | (Sum(_), _)
  | (Prod(_), _)
  | (List(_), _) => ([[]], [[]])
  };
};
