open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type match_result = Unboxing.unboxed(list(Environment.binding(Exp.t)));

let ( let* ) = Unboxing.( let* );

let combine_result = (r1: match_result, r2: match_result): match_result =>
  switch (r1, r2) {
  | (DoesNotMatch, _)
  | (_, DoesNotMatch) => DoesNotMatch
  | (IndetMatch, _)
  | (_, IndetMatch) => IndetMatch
  | (Matches(env1), Matches(env2)) => Matches(env1 @ env2)
  };

let rec matches = (capture, dp: Pat.t, d: DHExp.t): match_result => {
  let matches = matches(capture);
  let d = Ascriptions.transition_multiple(d);
  switch (DHPat.term_of(dp)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild => Matches([])
  | ExplicitNonlabel =>
    raise(
      Failure(
        "PatternMatch ExplicitNonlabel should not show up since these are removed during elaboration",
      ),
    )
  | Atom(c) =>
    let V(value, kind) = Atom.unpack(c);
    let* d' = Unboxing.unbox(Atom(kind), d);
    value == d' ? Matches([]) : DoesNotMatch;
  | ListLit(xs) =>
    let* s' = Unboxing.unbox(ListLitn(List.length(xs)), d);
    List.map2(matches, xs, s')
    |> List.fold_left(combine_result, Matches([]));
  | Cons(x, xs) =>
    let* (x', xs') = Unboxing.unbox(Cons, d);
    let* m_x = matches(x, x');
    let* m_xs = matches(xs, xs');
    Matches(m_x @ m_xs);
  | Constructor(ctr, _) =>
    let* () = Unboxing.unbox(SumNoArg(ctr), d);
    Matches([]);
  | Ap({term: Constructor(ctr, _), _}, p2) =>
    let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
    matches(p2, d2);
  | Ap(_, _) => IndetMatch // TODO: should this fail?
  | Var(x) => Matches([(x, d)])
  /* Labels are a special case */
  | Label(name) =>
    let* name' = Unboxing.unbox(Label, d);
    LabeledTuple.match_labels(name, name') ? Matches([]) : DoesNotMatch;
  | TupLabel(_, x) =>
    let* x' = Unboxing.unbox(TupLabel(dp), d);
    matches(x, x');
  | Tuple(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
    List.map2(matches, ps, ds)
    |> List.fold_left(combine_result, Matches([]));
  | Parens(p) => matches(p, d)
  | Probe(p, pr) =>
    let inner_match = matches(p, d);
    capture(pr, dp, d, inner_match);
    inner_match;
  | Asc(p, t1) =>
    matches(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))
  };
};

type sample_closures = list(Probe.call_stack => Dynamics.Sample.t);

type matches_and_samples = {
  matches: match_result,
  samples: sample_closures,
};

let matches = (dp: Pat.t, d: DHExp.t): matches_and_samples => {
  /* Closure capture for Probe instrumentation */
  let sample_closures: ref(sample_closures) = ref([]);
  let capture =
      (pr: Probe.t, dp: Pat.t, d: DHExp.t, inner_match: match_result): unit =>
    switch (inner_match) {
    | DoesNotMatch => ()
    | IndetMatch => ()
    | Matches(env) =>
      sample_closures :=
        List.cons(
          Dynamics.Sample.mk(
            Pat.rep_id(dp),
            d,
            Environment.of_bindings(env),
            _,
            pr,
          ),
          sample_closures^,
        )
    };
  let res = matches(capture, dp, d);
  {
    matches: res,
    samples: sample_closures^,
  };
};
