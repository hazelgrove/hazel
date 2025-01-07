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

type closure_closures =
  list((Probe.stack, Probe.stack) => (Id.t, Dynamics.Probe.Closure.t));
let closure_closures: ref(closure_closures) = ref([]);

let capture_closure = (pr, id: Id.t, d, inner_match: match_result): unit =>
  switch (inner_match) {
  | DoesNotMatch => ()
  | IndetMatch => ()
  | Matches(env) =>
    closure_closures :=
      List.cons(
        (stack, dyn_stack) =>
          (
            id,
            Dynamics.Probe.Closure.mk(d, {env, stack, dyn_stack, id}, pr),
          ),
        closure_closures^,
      )
  };

let rec matches = (dp: Pat.t, d: DHExp.t): match_result =>
  switch (DHPat.term_of(dp)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild => Matches(Environment.empty)
  | Int(n) =>
    let* n' = Unboxing.unbox(Int, d);
    n == n' ? Matches(Environment.empty) : DoesNotMatch;
  | Float(n) =>
    let* n' = Unboxing.unbox(Float, d);
    n == n' ? Matches(Environment.empty) : DoesNotMatch;
  | Bool(b) =>
    let* b' = Unboxing.unbox(Bool, d);
    b == b' ? Matches(Environment.empty) : DoesNotMatch;
  | String(s) =>
    let* s' = Unboxing.unbox(String, d);
    s == s' ? Matches(Environment.empty) : DoesNotMatch;
  | ListLit(xs) =>
    let* s' = Unboxing.unbox(List, d);
    if (List.length(xs) == List.length(s')) {
      List.map2(matches, xs, s')
      |> List.fold_left(combine_result, Matches(Environment.empty));
    } else {
      DoesNotMatch;
    };
  | Cons(x, xs) =>
    let* (x', xs') = Unboxing.unbox(Cons, d);
    let* m_x = matches(x, x');
    let* m_xs = matches(xs, xs');
    Matches(Environment.union(m_x, m_xs));
  | Constructor(ctr, _) =>
    let* () = Unboxing.unbox(SumNoArg(ctr), d);
    Matches(Environment.empty);
  | Ap({term: Constructor(ctr, _), _}, p2) =>
    let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
    matches(p2, d2);
  | Ap(_, _) => IndetMatch // TODO: should this fail?
  | Var(x) => Matches(Environment.singleton((x, d)))
  | Tuple(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
    List.map2(matches, ps, ds)
    |> List.fold_left(combine_result, Matches(Environment.empty));
  | Parens(p, Paren) => matches(p, d)
  | Parens(p, Probe(pr)) =>
    let inner_match = matches(p, d);
    capture_closure(pr, Term.Pat.rep_id(dp), d, inner_match);
    inner_match;
  | Cast(p, t1, t2) =>
    matches(p, Cast(d, t2, t1) |> DHExp.fresh |> Casts.transition_multiple)
  };

type matches_and_closures = {
  matches: match_result,
  closures: closure_closures,
};

// wrap matches but do stateful thing (closure capture)
let matches = (dp: Pat.t, d: DHExp.t): matches_and_closures => {
  closure_closures := [];
  let res = matches(dp, d);
  {matches: res, closures: closure_closures^};
};
