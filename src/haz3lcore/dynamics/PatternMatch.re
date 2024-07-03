open Util;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let const_unknown: 'a => Typ.t = _ => Unknown(Internal);

let cast_sum_maps =
    (sm1: Typ.sum_map, sm2: Typ.sum_map)
    : option(ConstructorMap.t((Typ.t, Typ.t))) => {
  let (ctrs1, tys1) = sm1 |> ConstructorMap.bindings |> List.split;
  let (ctrs2, tys2) = sm2 |> ConstructorMap.bindings |> List.split;
  if (ctrs1 == ctrs2) {
    let tys1 = tys1 |> List.filter(Option.is_some) |> List.map(Option.get);
    let tys2 = tys2 |> List.filter(Option.is_some) |> List.map(Option.get);
    if (List.length(tys1) == List.length(tys2)) {
      Some(
        List.(combine(tys1, tys2) |> combine(ctrs1))
        |> ConstructorMap.of_list,
      );
    } else {
      None;
    };
  } else {
    None;
  };
};

let rec matches = (dp: DHPat.t, d: DHExp.t): match_result => {
  switch (dp, d) {
  // TODO: Fix TupLabel matching
  | (_, BoundVar(_)) => DoesNotMatch
  | (EmptyHole(_), _)
  | (NonEmptyHole(_), _) => IndetMatch
  | (Wild, _) => Matches(Environment.empty)
  | (InvalidText(_), _) => IndetMatch
  | (BadConstructor(_), _) => IndetMatch
  /* Labels are a special case */
  | (TupLabel(s1, dp), TupLabel(s2, d)) =>
    if (LabeledTuple.compare(s1, s2) == 0) {
      matches(dp, d);
    } else {
      DoesNotMatch;
    }
  | (Var(x), TupLabel(s, d)) when LabeledTuple.compare(x, s) == 0 =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  // TODO: Label casting need fixing?
  | (TupLabel(_), Cast(d, Label(_, _), Unknown(_))) =>
    switch (d) {
    | TupLabel(_, d) => matches(dp, d)
    | _ => matches(dp, d)
    }
  | (TupLabel(_), Cast(d, Unknown(_), Label(s2, _))) =>
    switch (d) {
    | TupLabel(_, d) => matches(dp, TupLabel(s2, d))
    | _ => matches(dp, TupLabel(s2, d))
    }
  | (_, Cast(d, Label(_, ty1), ty2)) =>
    switch (d) {
    | TupLabel(_, d) => matches(dp, Cast(d, ty1, ty2))
    | _ => matches(dp, Cast(d, ty1, ty2))
    }
  | (_, Cast(d, ty1, Label(_, ty2))) =>
    switch (d) {
    | TupLabel(_, d) => matches(dp, Cast(d, ty1, ty2)) // shouldn't happen?
    | _ => matches(dp, Cast(d, ty1, ty2))
    }
  | (TupLabel(_, dp), _) => matches(dp, d)
  | (_, TupLabel(_, d)) => matches(dp, d)
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_)) => IndetMatch
  | (_, NonEmptyHole(_)) => IndetMatch
  | (_, FailedCast(_)) => IndetMatch
  | (_, InvalidOperation(_)) => IndetMatch
  | (_, FreeVar(_)) => IndetMatch
  | (_, InvalidText(_)) => IndetMatch
  | (_, Let(_)) => IndetMatch
  | (_, FixF(_)) => DoesNotMatch
  | (_, Fun(_)) => DoesNotMatch
  | (_, BinBoolOp(_)) => IndetMatch
  | (_, BinIntOp(_)) => IndetMatch
  | (_, BinFloatOp(_)) => IndetMatch
  | (_, Dot(_)) => IndetMatch
  | (_, ConsistentCase(Case(_))) => IndetMatch

  /* Closure should match like underlying expression. */
  | (_, Closure(_, d'))
  | (_, Filter(_, d')) => matches(dp, d')

  | (BoolLit(b1), BoolLit(b2)) =>
    if (b1 == b2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (BoolLit(_), Cast(d, Bool, Unknown(_))) => matches(dp, d)
  | (BoolLit(_), Cast(d, Unknown(_), Bool)) => matches(dp, d)
  | (BoolLit(_), _) => DoesNotMatch
  | (IntLit(n1), IntLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (IntLit(_), Cast(d, Int, Unknown(_))) => matches(dp, d)
  | (IntLit(_), Cast(d, Unknown(_), Int)) => matches(dp, d)
  | (IntLit(_), _) => DoesNotMatch
  | (FloatLit(n1), FloatLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (FloatLit(_), Cast(d, Float, Unknown(_))) => matches(dp, d)
  | (FloatLit(_), Cast(d, Unknown(_), Float)) => matches(dp, d)
  | (FloatLit(_), _) => DoesNotMatch
  | (StringLit(s1), StringLit(s2)) =>
    if (s1 == s2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (StringLit(_), Cast(d, String, Unknown(_))) => matches(dp, d)
  | (StringLit(_), Cast(d, Unknown(_), String)) => matches(dp, d)
  | (StringLit(_), _) => DoesNotMatch

  | (Ap(dp1, dp2), Ap(d1, d2)) =>
    switch (matches(dp1, d1)) {
    | DoesNotMatch => DoesNotMatch
    | IndetMatch =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch
      | Matches(_) => IndetMatch
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | (
      Ap(Constructor(ctr), dp_opt),
      Cast(d, Sum(sm1) | Rec(_, Sum(sm1)), Sum(sm2) | Rec(_, Sum(sm2))),
    ) =>
    switch (cast_sum_maps(sm1, sm2)) {
    | Some(castmap) => matches_cast_Sum(ctr, Some(dp_opt), d, [castmap])
    | None => DoesNotMatch
    }

  | (Ap(_, _), Cast(d, Sum(_) | Rec(_, Sum(_)), Unknown(_)))
  | (Ap(_, _), Cast(d, Unknown(_), Sum(_) | Rec(_, Sum(_)))) =>
    matches(dp, d)
  | (Ap(_, _), _) => DoesNotMatch

  | (Constructor(ctr), Constructor(ctr')) =>
    ctr == ctr' ? Matches(Environment.empty) : DoesNotMatch
  | (
      Constructor(ctr),
      Cast(d, Sum(sm1) | Rec(_, Sum(sm1)), Sum(sm2) | Rec(_, Sum(sm2))),
    ) =>
    switch (cast_sum_maps(sm1, sm2)) {
    | Some(castmap) => matches_cast_Sum(ctr, None, d, [castmap])
    | None => DoesNotMatch
    }
  | (Constructor(_), Cast(d, Sum(_) | Rec(_, Sum(_)), Unknown(_))) =>
    matches(dp, d)
  | (Constructor(_), Cast(d, Unknown(_), Sum(_) | Rec(_, Sum(_)))) =>
    matches(dp, d)
  | (Constructor(_), _) => DoesNotMatch

  | (Tuple(dps), Tuple(ds)) =>
    let f = (result, dp, d) => {
      switch (result) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env) =>
        switch (matches(dp, d)) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env') => Matches(Environment.union(env, env'))
        }
      };
    };
    // TODO (Anthony): optimize to work both way intead of run-twice hack.
    let f_reverse = (result, d, dp) => {
      switch (result) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env) =>
        switch (matches(dp, d)) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env') => Matches(Environment.union(env, env'))
        }
      };
    };
    let fin =
      LabeledTuple.ana_tuple(
        DHPat.get_label,
        DHExp.get_label,
        f,
        Matches(Environment.empty),
        DoesNotMatch,
        dps,
        ds,
      );
    switch (fin) {
    | DoesNotMatch =>
      LabeledTuple.ana_tuple(
        DHExp.get_label,
        DHPat.get_label,
        f_reverse,
        Matches(Environment.empty),
        DoesNotMatch,
        ds,
        dps,
      )
    | _ => fin
    };
  | (Tuple(dps), Cast(d, Prod(tys), Prod(tys'))) =>
    assert(List.length(tys) == List.length(tys'));
    switch (d) {
    | TupLabel(_, Tuple(ds))
    | Tuple(ds) =>
      let ds =
        LabeledTuple.rearrange(
          Typ.get_label, DHExp.get_label, tys, ds, (s, e) =>
          TupLabel(s, e)
        );
      let d_typ_pairs = List.combine(ds, tys);
      let get_label_pair:
        ((DHExp.t, Typ.t)) => option((LabeledTuple.t, (DHExp.t, Typ.t))) = (
        ((d, t)) =>
          switch (t) {
          | Label(s, t') => Some((s, (d, t')))
          | _ => None
          }
      );
      let d_typ_pairs =
        LabeledTuple.rearrange(
          Typ.get_label, get_label_pair, tys', d_typ_pairs, (s, (d, t)) =>
          (TupLabel(s, d), Label(s, t))
        );
      let (ds, tys) = List.split(d_typ_pairs);
      matches_cast_Tuple(
        dps,
        DHExp.Tuple(ds),
        List.map(p => [p], List.combine(tys, tys')),
      );
    | _ =>
      matches_cast_Tuple(
        dps,
        d,
        List.map(p => [p], List.combine(tys, tys')),
      )
    };
  | (Tuple(dps), Cast(d, Prod(tys), Unknown(_))) =>
    matches_cast_Tuple(
      dps,
      d,
      List.map(
        p => [p],
        List.combine(tys, List.init(List.length(tys), const_unknown)),
      ),
    )
  | (Tuple(dps), Cast(d, Unknown(_), Prod(tys'))) =>
    matches_cast_Tuple(
      dps,
      d,
      List.map(
        p => [p],
        List.combine(List.init(List.length(tys'), const_unknown), tys'),
      ),
    )
  | (Tuple(_), Cast(_)) => DoesNotMatch
  | (Tuple(_), _) => DoesNotMatch
  | (Cons(_) | ListLit(_), Cast(d, List(ty1), List(ty2))) =>
    matches_cast_Cons(dp, d, [(ty1, ty2)])
  | (Cons(_) | ListLit(_), Cast(d, Unknown(_), List(ty2))) =>
    matches_cast_Cons(dp, d, [(Unknown(Internal), ty2)])
  | (Cons(_) | ListLit(_), Cast(d, List(ty1), Unknown(_))) =>
    matches_cast_Cons(dp, d, [(ty1, Unknown(Internal))])
  | (Cons(_, _), Cons(_, _))
  | (ListLit(_, _), Cons(_, _))
  | (Cons(_, _), ListLit(_))
  | (ListLit(_), ListLit(_)) => matches_cast_Cons(dp, d, [])
  | (Cons(_) | ListLit(_), _) => DoesNotMatch
  };
}
and matches_cast_Sum =
    (
      ctr: string,
      dp: option(DHPat.t),
      d: DHExp.t,
      castmaps: list(ConstructorMap.t((Typ.t, Typ.t))),
    )
    : match_result =>
  switch (d) {
  | Constructor(ctr') =>
    switch (
      dp,
      castmaps |> List.map(ConstructorMap.find_opt(ctr')) |> OptUtil.sequence,
    ) {
    | (None, Some(_)) =>
      ctr == ctr' ? Matches(Environment.empty) : DoesNotMatch
    | _ => DoesNotMatch
    }
  | Ap(Constructor(ctr'), d') =>
    switch (
      dp,
      castmaps |> List.map(ConstructorMap.find_opt(ctr')) |> OptUtil.sequence,
    ) {
    | (Some(dp), Some(side_casts)) =>
      matches(dp, DHExp.apply_casts(d', side_casts))
    | _ => DoesNotMatch
    }
  | Cast(d', Sum(sm1) | Rec(_, Sum(sm1)), Sum(sm2) | Rec(_, Sum(sm2))) =>
    switch (cast_sum_maps(sm1, sm2)) {
    | Some(castmap) => matches_cast_Sum(ctr, dp, d', [castmap, ...castmaps])
    | None => DoesNotMatch
    }
  | Cast(d', Sum(_) | Rec(_, Sum(_)), Unknown(_))
  | Cast(d', Unknown(_), Sum(_) | Rec(_, Sum(_))) =>
    matches_cast_Sum(ctr, dp, d', castmaps)
  | TupLabel(_, d') => matches_cast_Sum(ctr, dp, d', castmaps)
  | FreeVar(_)
  | InvalidText(_)
  | Let(_)
  | TypAp(_)
  | Ap(_)
  | ApBuiltin(_)
  | BinBoolOp(_)
  | BinIntOp(_)
  | BinFloatOp(_)
  | BinStringOp(_)
  | InconsistentBranches(_)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | FailedCast(_, _, _)
  | Test(_)
  | InvalidOperation(_)
  | ConsistentCase(_)
  | Prj(_)
  | IfThenElse(_)
  | BuiltinFun(_) => IndetMatch
  | Cast(_)
  | BoundVar(_)
  | FixF(_)
  | TypFun(_)
  | Fun(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | ListLit(_)
  | Tuple(_)
  | Dot(_, _)
  | Sequence(_, _)
  | Closure(_)
  | Filter(_)
  | Cons(_)
  | ListConcat(_) => DoesNotMatch
  }
and matches_cast_Tuple =
    (
      dps: list(DHPat.t),
      d: DHExp.t,
      elt_casts: list(list((Typ.t, Typ.t))),
    )
    : match_result =>
  switch (d) {
  | Tuple(ds) =>
    if (List.length(dps) != List.length(ds)) {
      DoesNotMatch;
    } else {
      let ds_elts = List.combine(ds, elt_casts);
      let filt2: ((DHExp.t, 'a)) => option((LabeledTuple.t, (DHExp.t, 'a))) = (
        ((d, c)) =>
          switch (d) {
          | TupLabel(s, d') => Some((s, (d', c)))
          | _ => None
          }
      );
      assert(List.length(List.combine(dps, ds)) == List.length(elt_casts));
      let f = (result, dp, (d, casts)) => {
        switch (result) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env) =>
          switch (matches(dp, DHExp.apply_casts(d, casts))) {
          | DoesNotMatch => DoesNotMatch
          | IndetMatch => IndetMatch
          | Matches(env') => Matches(Environment.union(env, env'))
          }
        };
      };
      // TODO (Anthony): optimize to work both way intead of run-twice hack.
      let f_reverse = (result, (d, casts), dp) => {
        switch (result) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env) =>
          switch (matches(dp, DHExp.apply_casts(d, casts))) {
          | DoesNotMatch => DoesNotMatch
          | IndetMatch => IndetMatch
          | Matches(env') => Matches(Environment.union(env, env'))
          }
        };
      };
      let fin =
        LabeledTuple.ana_tuple(
          DHPat.get_label,
          filt2,
          f,
          Matches(Environment.empty),
          DoesNotMatch,
          dps,
          ds_elts,
        );
      switch (fin) {
      | DoesNotMatch =>
        LabeledTuple.ana_tuple(
          filt2,
          DHPat.get_label,
          f_reverse,
          Matches(Environment.empty),
          DoesNotMatch,
          ds_elts,
          dps,
        )
      | _ => fin
      };
    }
  | Cast(d', Prod(tys), Prod(tys')) =>
    if (List.length(dps) != List.length(tys)) {
      DoesNotMatch;
    } else {
      assert(List.length(tys) == List.length(tys'));
      switch (d') {
      | TupLabel(_, Tuple(ds))
      | Tuple(ds) =>
        let ds =
          LabeledTuple.rearrange(
            Typ.get_label, DHExp.get_label, tys, ds, (s, e) =>
            TupLabel(s, e)
          );
        let d_typ_pairs = List.combine(ds, tys);
        let get_label_pair:
          ((DHExp.t, Typ.t)) => option((LabeledTuple.t, (DHExp.t, Typ.t))) = (
          ((d, t)) =>
            switch (t) {
            | Label(s, t') => Some((s, (d, t')))
            | _ => None
            }
        );
        let d_typ_pairs =
          LabeledTuple.rearrange(
            Typ.get_label, get_label_pair, tys', d_typ_pairs, (s, (d, t)) =>
            (TupLabel(s, d), Label(s, t))
          );
        let (ds, tys) = List.split(d_typ_pairs);
        matches_cast_Tuple(
          dps,
          DHExp.Tuple(ds),
          List.map2(List.cons, List.combine(tys, tys'), elt_casts),
        );
      | _ =>
        matches_cast_Tuple(
          dps,
          d',
          List.map2(List.cons, List.combine(tys, tys'), elt_casts),
        )
      };
    }
  | Cast(d', Prod(tys), Unknown(_)) =>
    let tys' = List.init(List.length(tys), const_unknown);
    matches_cast_Tuple(
      dps,
      d',
      List.map2(List.cons, List.combine(tys, tys'), elt_casts),
    );
  | Cast(d', Unknown(_), Prod(tys')) =>
    let tys = List.init(List.length(tys'), const_unknown);
    matches_cast_Tuple(
      dps,
      d',
      List.map2(List.cons, List.combine(tys, tys'), elt_casts),
    );
  | TupLabel(_, d') => matches_cast_Tuple(dps, d', elt_casts)
  | Dot(_, _)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_) => IndetMatch
  | InvalidText(_) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | TypFun(_, _, _) => DoesNotMatch
  | Fun(_, _, _, _) => DoesNotMatch
  | Closure(_, Fun(_)) => DoesNotMatch
  | Closure(_, _) => IndetMatch
  | TypAp(_, _) => IndetMatch
  | Filter(_, _) => IndetMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BinStringOp(_)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | BuiltinFun(_)
  | Test(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | StringLit(_) => DoesNotMatch
  | ListLit(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | ListConcat(_) => DoesNotMatch
  | Prj(_) => IndetMatch
  | Constructor(_) => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_) => IndetMatch
  | NonEmptyHole(_) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  | IfThenElse(_) => IndetMatch
  }
and matches_cast_Cons =
    (dp: DHPat.t, d: DHExp.t, elt_casts: list((Typ.t, Typ.t))): match_result =>
  switch (d) {
  | ListLit(_, _, _, []) =>
    switch (dp) {
    | ListLit(_, []) => Matches(Environment.empty)
    | _ => DoesNotMatch
    }
  | ListLit(u, i, ty, [dhd, ...dtl] as ds) =>
    switch (dp) {
    | Cons(dp1, dp2) =>
      switch (matches(dp1, DHExp.apply_casts(dhd, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
            },
            elt_casts,
          );
        let d2 = DHExp.ListLit(u, i, ty, dtl);
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | ListLit(_, dps) =>
      switch (ListUtil.opt_zip(dps, ds)) {
      | None => DoesNotMatch
      | Some(lst) =>
        lst
        |> List.map(((dp, d)) =>
             matches(dp, DHExp.apply_casts(d, elt_casts))
           )
        |> List.fold_left(
             (match1, match2) =>
               switch (match1, match2) {
               | (DoesNotMatch, _)
               | (_, DoesNotMatch) => DoesNotMatch
               | (IndetMatch, _)
               | (_, IndetMatch) => IndetMatch
               | (Matches(env1), Matches(env2)) =>
                 Matches(Environment.union(env1, env2))
               },
             Matches(Environment.empty),
           )
      }
    | _ => failwith("called matches_cast_Cons with non-list pattern")
    }
  | Cons(d1, d2) =>
    switch (dp) {
    | Cons(dp1, dp2) =>
      switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
            },
            elt_casts,
          );
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | ListLit(_, []) => DoesNotMatch
    | ListLit(ty, [dphd, ...dptl]) =>
      switch (matches(dphd, DHExp.apply_casts(d1, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
            },
            elt_casts,
          );
        let dp2 = DHPat.ListLit(ty, dptl);
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | _ => failwith("called matches_cast_Cons with non-list pattern")
    }
  | Cast(d', List(ty1), List(ty2)) =>
    matches_cast_Cons(dp, d', [(ty1, ty2), ...elt_casts])
  | Cast(d', List(ty1), Unknown(_)) =>
    matches_cast_Cons(dp, d', [(ty1, Unknown(Internal)), ...elt_casts])
  | Cast(d', Unknown(_), List(ty2)) =>
    matches_cast_Cons(dp, d', [(Unknown(Internal), ty2), ...elt_casts])
  | TupLabel(_, d') => matches_cast_Cons(dp, d', elt_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_) => IndetMatch
  | InvalidText(_) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | TypFun(_, _, _) => DoesNotMatch
  | Fun(_, _, _, _) => DoesNotMatch
  | Closure(_, d') => matches_cast_Cons(dp, d', elt_casts)
  | TypAp(_, _) => IndetMatch
  | Filter(_, d') => matches_cast_Cons(dp, d', elt_casts)
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BinStringOp(_)
  | ListConcat(_)
  | BuiltinFun(_) => DoesNotMatch
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | Test(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | StringLit(_) => DoesNotMatch
  | Tuple(_) => DoesNotMatch
  | Dot(_, _) => DoesNotMatch
  | Prj(_) => IndetMatch
  | Constructor(_) => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_) => IndetMatch
  | NonEmptyHole(_) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  | IfThenElse(_) => IndetMatch
  };
