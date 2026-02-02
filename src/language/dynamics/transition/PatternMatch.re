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
 * Collected during pattern matching when patterns are targeted. */
type sample_closures = list((Sample.call_stack, int, int) => Sample.t);

/* Core pattern matching logic - just a switch on pattern structure */
let match_pattern =
    (
      ~targets: Sample.targets,
      recur: (Pat.t, DHExp.t) => match_result,
      dp: Pat.t,
      d: DHExp.t,
    )
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
  | Asc(p, t1) =>
    // TODO Capture closures
    let (_closures, exp) =
      Ascriptions.transition_multiple(~targets, Asc(d, t1) |> DHExp.fresh);
    recur(p, exp);
  };

/* Record a sample closure if this pattern is targeted and matched */
let record_sample =
    (
      sample_closures: ref(sample_closures),
      pat_id: Id.t,
      maybe_spec: option(Sample.capture_spec),
      d: DHExp.t,
      result: match_result,
    )
    : unit =>
  switch (maybe_spec, result) {
  | (Some(spec), Matches(env)) =>
    sample_closures :=
      List.cons(
        (call_stack: Sample.call_stack, step_start: int, step_end: int) =>
          Sample.mk(
            ~step_start,
            ~step_end,
            pat_id,
            d,
            Environment.of_bindings(env),
            call_stack,
            spec,
          ),
        sample_closures^,
      )
  | _ => ()
  };

/* Main matching function - coordinates pattern matching and sample recording */
let rec matches_inner =
        (
          targets: Sample.targets,
          sample_closures: ref(sample_closures),
          dp: Pat.t,
          d: DHExp.t,
        )
        : match_result => {
  // TODO Record closures
  let (_closures, d) = Ascriptions.transition_multiple(~targets, d);
  let pat_id = Pat.rep_id(dp);
  let maybe_spec = Id.Map.find_opt(pat_id, targets);
  let recur = matches_inner(targets, sample_closures);

  let result = match_pattern(~targets, recur, dp, d);
  record_sample(sample_closures, pat_id, maybe_spec, d, result);
  result;
};

type matches_and_samples = {
  matches: match_result,
  samples: sample_closures,
};

let matches =
    (targets: Sample.targets, dp: Pat.t, d: DHExp.t): matches_and_samples => {
  let sample_closures = ref([]);
  let result = matches_inner(targets, sample_closures, dp, d);
  {
    matches: result,
    samples: sample_closures^,
  };
};
