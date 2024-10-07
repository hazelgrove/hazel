type match_result = Unboxing.unboxed(Environment.t);
let ( let* ) = Unboxing.( let* );

let combine_result = (r1: match_result, r2: match_result): match_result =>
  switch (r1, r2) {
  | (DoesNotMatch, _)
  | (_, DoesNotMatch) => DoesNotMatch
  | (IndetMatch, _)
  | (_, IndetMatch) => IndetMatch
  | (Matches(env1), Matches(env2)) =>
    Matches(Environment.union(env1, env2))
  };

let rec matches = (dp: Pat.t, d: DHExp.t): match_result =>
  switch (DHPat.term_of(dp)) {
  | InvalidPat(_)
  | EmptyHolePat
  | MultiHolePat(_)
  | Wild => Matches(Environment.empty)
  | IntPat(n) =>
    let* n' = Unboxing.unbox(Int, d);
    n == n' ? Matches(Environment.empty) : DoesNotMatch;
  | FloatPat(n) =>
    let* n' = Unboxing.unbox(Float, d);
    n == n' ? Matches(Environment.empty) : DoesNotMatch;
  | BoolPat(b) =>
    let* b' = Unboxing.unbox(Bool, d);
    b == b' ? Matches(Environment.empty) : DoesNotMatch;
  | StringPat(s) =>
    let* s' = Unboxing.unbox(String, d);
    s == s' ? Matches(Environment.empty) : DoesNotMatch;
  | ListLitPat(xs) =>
    let* s' = Unboxing.unbox(List, d);
    if (List.length(xs) == List.length(s')) {
      List.map2(matches, xs, s')
      |> List.fold_left(combine_result, Matches(Environment.empty));
    } else {
      DoesNotMatch;
    };
  | ConsPat(x, xs) =>
    let* (x', xs') = Unboxing.unbox(Cons, d);
    let* m_x = matches(x, x');
    let* m_xs = matches(xs, xs');
    Matches(Environment.union(m_x, m_xs));
  | ConstructorPat(ctr, _) =>
    let* () = Unboxing.unbox(SumNoArg(ctr), d);
    Matches(Environment.empty);
  | ApPat({term: ConstructorPat(ctr, _), _}, p2) =>
    let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
    matches(p2, d2);
  | ApPat(_, _) => IndetMatch // TODO: should this fail?
  | VarPat(x) => Matches(Environment.singleton((x, d)))
  | TuplePat(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
    List.map2(matches, ps, ds)
    |> List.fold_left(combine_result, Matches(Environment.empty));
  | ParensPat(p) => matches(p, d)
  | CastPat(p, t1, t2) =>
    matches(p, Cast(d, t2, t1) |> DHExp.fresh |> Casts.transition_multiple)
  };
