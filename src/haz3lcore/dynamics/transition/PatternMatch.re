[@deriving (show({with_path: false}), sexp, yojson)]
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

let rec matches = (capture, dp: Pat.t, d: DHExp.t): match_result => {
  let matches = matches(capture);

  let d = Casts.transition_multiple(d);
  switch (DHPat.term_of(dp)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild => Matches(Environment.empty)
  | Atom(c) =>
    let V(value, kind) = Atom.unpack(c);
    let* d' = Unboxing.unbox(Atom(kind), d);
    value == d' ? Matches(Environment.empty) : DoesNotMatch;
  | ListLit(xs) =>
    let* s' = Unboxing.unbox(ListLitn(List.length(xs)), d);
    List.map2(matches, xs, s')
    |> List.fold_left(combine_result, Matches(Environment.empty));
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
  /* Labels are a special case */
  | Label(name) =>
    let* name' = Unboxing.unbox(Label, d);
    LabeledTuple.match_labels(name, name')
      ? Matches(Environment.empty) : DoesNotMatch;
  | TupLabel(_, x) =>
    let* x' = Unboxing.unbox(TupLabel(dp), d);
    matches(x, x');
  | Tuple(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);

    List.map2(matches, ps, ds)
    |> List.fold_left(combine_result, Matches(Environment.empty));
  | Parens(p) => matches(p, d)
  | Probe(p, pr) =>
    let inner_match = matches(p, d);
    capture(pr, dp, d, inner_match);
    inner_match;
  | Cast(p, t1, t2) =>
    matches(p, Casts.transition_multiple(Cast(d, t2, t1) |> DHExp.fresh))
  };
};

type closure_closures = list(Probe.call_stack => Dynamics.Probe.Closure.t);

type matches_and_closures = {
  matches: match_result,
  closures: closure_closures,
};

let matches = (dp: Pat.t, d: DHExp.t): matches_and_closures => {
  /* Closure capture for Probe instrumentation */
  let closure_closures: ref(closure_closures) = ref([]);
  let capture =
      (pr: Probe.t, dp: Term.Pat.t, d: DHExp.t, inner_match: match_result)
      : unit =>
    switch (inner_match) {
    | DoesNotMatch => ()
    | IndetMatch => ()
    | Matches(env) =>
      closure_closures :=
        List.cons(
          Dynamics.Probe.Closure.mk(Term.Pat.rep_id(dp), d, env, _, pr),
          closure_closures^,
        )
    };
  let res = matches(capture, dp, d);
  {
    matches: res,
    closures: closure_closures^,
  };
};
