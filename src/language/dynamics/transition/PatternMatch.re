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

/* `d` as `e + k` for a number literal `k`, on either side: the form an
   induction case gives an open number, as in `m + 1`. */
let rec open_sum = (d: DHExp.t): option((Operators.op_bin, DHExp.t, Atom.t)) =>
  switch (DHExp.term_of(d)) {
  | BinOp(
      (Int(Plus) | Nat(Plus) | SInt(Plus)) as op,
      e,
      {term: Atom(k), _},
    )
  | BinOp(
      (Int(Plus) | Nat(Plus) | SInt(Plus)) as op,
      {term: Atom(k), _},
      e,
    ) =>
    Some((op, e, k))
  | Parens(d) => open_sum(d)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Constructor(_)
  | Fun(_)
  | TypFun(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
  | Forall(_)
  | FixF(_)
  | TyAlias(_)
  | Use(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | HintedTest(_)
  | Filter(_)
  | Closure(_)
  | Projector(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Asc(_)
  | Module(_)
  | ModuleExp(_) => None
  };

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
    switch (Unboxing.unbox(Atom(kind), d), open_sum(d)) {
    | (IndetMatch, Some((_, e, k))) =>
      switch (c, k, Pat.Addition.minus(c, k)) {
      | (_, _, Some(rest)) => recur(Atom(rest) |> Pat.fresh, e)
      /* `e + k` is at least `k`, which is more than `c`. */
      | (Nat(_), Nat(_), None) => DoesNotMatch
      | _ => IndetMatch
      }
    | (unboxed, _) =>
      let* d' = unboxed;
      Poly.equal(value, d') ? Matches([]) : DoesNotMatch;
    };
  | Add(p1, p2) =>
    switch (Pat.Addition.split(p1, p2)) {
    | None => IndetMatch
    | Some((p, c)) =>
      let V(_, kind) = Atom.unpack(c);
      switch (Unboxing.unbox(Atom(kind), d), open_sum(d)) {
      | (Matches(n), _) =>
        switch (Pat.Addition.minus(Atom.repack(kind, n), c)) {
        | Some(rest) => recur(p, Atom(rest) |> DHExp.fresh)
        | None => DoesNotMatch
        }
      | (IndetMatch, Some((_, e, k))) when Atom.equal(k, c) => recur(p, e)
      | (IndetMatch, Some((op, e, k))) =>
        switch (Pat.Addition.minus(k, c), Pat.Addition.minus(c, k)) {
        | (Some(rest), _) =>
          recur(p, BinOp(op, e, Atom(rest) |> DHExp.fresh) |> DHExp.fresh)
        /* A Nat `p + 2` against `e + 1` is `p + 1` against `e`. */
        | (None, Some(rest)) =>
          recur(Add(p, Atom(rest) |> Pat.fresh) |> Pat.fresh, e)
        | (None, None) => IndetMatch
        }
      | (IndetMatch, None) => IndetMatch
      | (DoesNotMatch, _) => DoesNotMatch
      };
    }
  | ListLit(xs) =>
    let* s' = Unboxing.unbox(ListLitn(List.length(xs)), d);
    List.map2_exn(xs, s', ~f=recur)
    |> List.fold_left(~f=combine_result, ~init=Matches([]));
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
    List.map2_exn(ps, ds, ~f=recur)
    |> List.fold_left(~f=combine_result, ~init=Matches([]));
  | Parens(p)
  | Projector(_, p) => recur(p, d)
  | Asc(p, t1) =>
    recur(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))
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
          targets: Sample.targets,
          sample_closures: ref(sample_closures),
          dp: Pat.t,
          d: DHExp.t,
        )
        : match_result => {
  let d = Ascriptions.transition_multiple(d);
  let pat_id = Pat.rep_id(dp);
  let maybe_spec = Id.Map.find_opt(pat_id, targets);
  let recur = matches_inner(targets, sample_closures);

  let result = match_pattern(recur, dp, d);
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
