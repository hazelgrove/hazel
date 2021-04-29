let rec mk =
        (ty1: HTyp.t, ty2: HTyp.t)
        : (list(CursorPath.steps), list(CursorPath.steps)) =>
  switch (ty1, ty2) {
  | (Hole, _)
  | (_, Hole)
  | (Int, Int)
  | (Float, Float)
  | (Bool, Bool) => ([], [])
  | (Arrow(ty_in, ty_out), Arrow(ty_in', ty_out')) =>
    let (steps_in, steps_in') =
      TupleUtil.map2(List.map(List.cons(0)), mk(ty_in, ty_in'));
    let (steps_out, steps_out') =
      TupleUtil.map2(List.map(List.cons(1)), mk(ty_out, ty_out'));
    (steps_in @ steps_out, steps_in' @ steps_out');
  | (Sum(ty_l, ty_r), Sum(ty_l', ty_r')) =>
    let (steps_in, steps_in') =
      TupleUtil.map2(List.map(List.cons(0)), mk(ty_l, ty_l'));
    let (steps_out, steps_out') =
      TupleUtil.map2(List.map(List.cons(1)), mk(ty_r, ty_r'));
    (steps_in @ steps_out, steps_in' @ steps_out');
  | (Prod(tys), Prod(tys')) =>
    if (List.length(tys) != List.length(tys')) {
      ([[]], [[]]);
    } else {
      List.combine(tys, tys')
      |> List.mapi((i, (ty, ty')) =>
           TupleUtil.map2(List.map(List.cons(i)), mk(ty, ty'))
         )
      |> List.split
      |> TupleUtil.map2(List.flatten);
    }
  | (List(ty), List(ty')) =>
    TupleUtil.map2(List.map(List.cons(0)), mk(ty, ty'))
  | (Int, _)
  | (Float, _)
  | (Bool, _)
  | (Arrow(_), _)
  | (Sum(_), _)
  | (Prod(_), _)
  | (List(_), _) => ([[]], [[]])
  };
