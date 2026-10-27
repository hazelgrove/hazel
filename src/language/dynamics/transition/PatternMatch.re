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
type sample_closures = list((CallStack.t, int, int) => Sample.t);

/* ===== Guard evaluation for `p where g` (Grammar.re Pat.Where) =====
 *
 * `where` and OCaml's `when` are the same construct: `p where g` matches
 * iff `p` matches AND `g` holds under the bindings `p` produced. That is
 * what makes the proof-side reading sound -- you only reach the body (a
 * case arm, a function body) when `g` holds, so `g` is a citable fact
 * there (ProofCheck.add_where_facts).
 *
 * Deciding a match therefore requires RUNNING `g`, i.e. calling the
 * evaluator from inside the transition machinery the evaluator is built
 * on: Evaluator -> Transition -> PatternMatch is a module cycle. So the
 * evaluator installs itself here at init (see the `guard_hook :=` at the
 * bottom of Evaluator.re) rather than being called directly.
 *
 * If no evaluator is installed (a caller that links PatternMatch without
 * Evaluator), a guard is IndetMatch -- never silently ignored. */
[@deriving (show({with_path: false}), sexp, yojson)]
type guard_result =
  | GuardTrue
  | GuardFalse
  | GuardIndet;

let guard_hook: ref(option((Environment.t(Exp.t), Exp.t) => guard_result)) =
  ref(None);

let eval_guard = (env: Environment.t(Exp.t), g: Exp.t): guard_result =>
  switch (guard_hook^) {
  | Some(f) => f(env, g)
  | None => GuardIndet
  };

/* Core pattern matching logic - just a switch on pattern structure */
let match_pattern =
    (
      ~env: Environment.t(Exp.t),
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
  | Parens(p)
  | Projector(_, p) => recur(p, d)
  | Asc(p, t1) =>
    recur(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))
  /* D78 `p !! h`: matches as `p` (the fact name binds no value). */
  | StdNamed(p, _) => recur(p, d)
  | Where(p, g) =>
    /* Structural match first, then the guard under those bindings. */
    switch (recur(p, d)) {
    | DoesNotMatch => DoesNotMatch
    | IndetMatch => IndetMatch
    | Matches(bindings) as m =>
      switch (eval_guard(Environment.add_bindings(env, bindings), g)) {
      | GuardTrue => m
      | GuardFalse => DoesNotMatch
      | GuardIndet => IndetMatch
      }
    }
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
        (call_stack: CallStack.t, step_start: int, step_end: int) =>
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
          ~env: Environment.t(Exp.t),
          targets: Sample.targets,
          sample_closures: ref(sample_closures),
          dp: Pat.t,
          d: DHExp.t,
        )
        : match_result => {
  let d = Ascriptions.transition_multiple(d);
  let pat_id = Pat.rep_id(dp);
  let maybe_spec = Id.Map.find_opt(pat_id, targets);
  let recur = matches_inner(~env, targets, sample_closures);

  let result = match_pattern(~env, recur, dp, d);
  record_sample(sample_closures, pat_id, maybe_spec, d, result);
  result;
};

type matches_and_samples = {
  matches: match_result,
  samples: sample_closures,
};

/* `env` is the environment the match happens in; it is only consulted to
   evaluate `where` guards (empty in substitution mode, where the tree is
   already substituted). */
let matches =
    (
      ~env: Environment.t(Exp.t)=Environment.empty,
      targets: Sample.targets,
      dp: Pat.t,
      d: DHExp.t,
    )
    : matches_and_samples => {
  let sample_closures = ref([]);
  let result = matches_inner(~env, targets, sample_closures, dp, d);
  {
    matches: result,
    samples: sample_closures^,
  };
};
