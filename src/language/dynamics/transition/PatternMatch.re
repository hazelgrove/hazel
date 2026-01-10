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

/* Core pattern matching logic - just a switch on pattern structure */
let match_pattern =
    (recur: (Pat.t, DHExp.t) => match_result, dp: Pat.t, d: DHExp.t)
    : match_result =>
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
    List.map2(recur, xs, s') |> List.fold_left(combine_result, Matches([]));
  | Cons(x, xs) =>
    let* (x', xs') = Unboxing.unbox(Cons, d);
    let* m_x = recur(x, x');
    let* m_xs = recur(xs, xs');
    Matches(m_x @ m_xs);
  | Constructor(ctr, _) =>
    let* () = Unboxing.unbox(SumNoArg(ctr), d);
    Matches([]);
  | Ap(c, p2) =>
    switch (Pat.ctr_name(c)) {
    | Some(ctr) =>
      let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
      recur(p2, d2);
    | None => IndetMatch
    }
  | Var(x) => Matches([(x, d)])
  | Label(name) =>
    let* name' = Unboxing.unbox(Label, d);
    LabeledTuple.match_labels(name, name') ? Matches([]) : DoesNotMatch;
  | TupLabel(_, x) =>
    let* x' = Unboxing.unbox(TupLabel(dp), d);
    recur(x, x');
  | Tuple(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
    List.map2(recur, ps, ds) |> List.fold_left(combine_result, Matches([]));
  | Parens(p) => recur(p, d)
  | Probe(p, _) => recur(p, d)
  | Asc(p, t1) =>
    recur(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))
  };

/* Record a sample closure if this pattern is probed and matched */
let record_probe_sample =
    (
      sample_closures: ref(sample_closures),
      pat_id: Id.t,
      maybe_probe: option(Probe.t),
      d: DHExp.t,
      result: match_result,
    )
    : unit =>
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

/* Main matching function - coordinates pattern matching and probe recording */
let rec matches_inner =
        (
          probe_map: Id.Map.t(Probe.t),
          sample_closures: ref(sample_closures),
          dp: Pat.t,
          d: DHExp.t,
        )
        : match_result => {
  let d = Ascriptions.transition_multiple(d);
  let pat_id = Pat.rep_id(dp);
  let maybe_probe = Id.Map.find_opt(pat_id, probe_map);
  let recur = matches_inner(probe_map, sample_closures);

  let result = match_pattern(recur, dp, d);
  record_probe_sample(sample_closures, pat_id, maybe_probe, d, result);
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
