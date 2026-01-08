open Util;
open Unboxing;

[@deriving (show({with_path: false}), sexp, yojson)]
type match_result = unboxed(list(Environment.binding(Exp.t)));

let ( let* ) = Unboxing.( let* );

let combine_result = (r1: match_result, r2: match_result): match_result =>
  switch (r1, r2) {
  | (DoesNotMatch, _)
  | (_, DoesNotMatch) => DoesNotMatch
  | (IndetMatch, _)
  | (_, IndetMatch) => IndetMatch
  | (Matches(env1), Matches(env2)) => Matches(env1 @ env2)
  };

/* Sample closures take call_stack, step_start, and step_end.
 * Collected during pattern matching when patterns are in probe_map. */
type sample_closures = list((Probe.call_stack, int, int) => Sample.t);

let rec matches_inner =
        (
          probe_map: Id.Map.t(Probe.t),
          sample_closures: ref(sample_closures),
          dp: Pat.t,
          d: DHExp.t,
        )
        : match_result => {
  let matches_inner = matches_inner(probe_map, sample_closures);
  let d = Ascriptions.transition_multiple(d);

  /* Check if this pattern should be probed */
  let pat_id = Pat.rep_id(dp);
  let maybe_probe = Id.Map.find_opt(pat_id, probe_map);

  let result =
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
      List.map2(matches_inner, xs, s')
      |> List.fold_left(combine_result, Matches([]));
    | Cons(x, xs) =>
      let* (x', xs') = Unboxing.unbox(Cons, d);
      let* m_x = matches_inner(x, x');
      let* m_xs = matches_inner(xs, xs');
      Matches(m_x @ m_xs);
    | Constructor(ctr, _) =>
      let* () = Unboxing.unbox(SumNoArg(ctr), d);
      Matches([]);
    | Ap(c, p2) =>
      switch (Pat.ctr_name(c)) {
      | Some(ctr) =>
        let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
        matches_inner(p2, d2);
      | None => IndetMatch
      }
    | Var(x) => Matches([(x, d)])
    /* Labels are a special case */
    | Label(name) =>
      let* name' = Unboxing.unbox(Label, d);
      LabeledTuple.match_labels(name, name') ? Matches([]) : DoesNotMatch;
    | TupLabel(_, x) =>
      let* x' = Unboxing.unbox(TupLabel(dp), d);
      matches_inner(x, x');
    | Tuple(ps) =>
      let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
      List.map2(matches_inner, ps, ds)
      |> List.fold_left(combine_result, Matches([]));
    | Parens(p) => matches_inner(p, d)
    | Probe(p, _) =>
      /* Probe AST nodes are no longer used for probe functionality.
       * The new system uses probe_map passed to the evaluator.
       * Just pass through like Parens. */
      matches_inner(p, d)
    | Asc(p, t1) =>
      matches_inner(
        p,
        Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh),
      )
    };

  /* If this pattern is in probe_map and matched successfully, record a sample */
  switch (maybe_probe, result) {
  | (Some(pr), Matches(env)) =>
    sample_closures :=
      List.cons(
        (call_stack: Probe.call_stack, step_start: int, step_end: int) =>
          Sample.mk(
            ~step_start,
            ~step_end,
            pat_id,
            d,
            Environment.of_bindings(env),
            call_stack,
            pr,
          ),
        sample_closures^,
      )
  | _ => ()
  };

  result;
};

type matches_and_samples = {
  matches: match_result,
  samples: sample_closures,
};

let matches =
    (probe_map: Id.Map.t(Probe.t), dp: Pat.t, d: DHExp.t)
    : matches_and_samples => {
  let sample_closures = ref([]);
  let result = matches_inner(probe_map, sample_closures, dp, d);
  {
    matches: result,
    samples: sample_closures^,
  };
};
