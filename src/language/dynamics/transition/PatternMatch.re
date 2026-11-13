open Util;
open Unboxing;

/* Matching is structural and runs left to right over a worklist of what
 * is still to match. When it reaches a `where` guard it stops, returning
 * the guard, the bindings so far (which are what the guard sees) and the
 * rest of the worklist. The transition rules turn that into an `if` whose
 * then-branch matches the rest (Transition.re), so a guard is evaluated as
 * part of the program: after everything to its left has matched, before
 * anything to its right is looked at. */

[@deriving (show({with_path: false}), sexp, yojson)]
type pending =
  | Match(Pat.t, DHExp.t)
  | Check(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type matched =
  | Complete(list(Environment.binding(Exp.t)))
  | Guarded({
      bindings: list(Environment.binding(Exp.t)),
      guard: Exp.t,
      rest: list(pending),
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type match_result = unboxed(matched);

/* Sample closures take call_stack, step_start, and step_end.
 * Collected during pattern matching when patterns are targeted. */
type sample_closures = list((CallStack.t, int, int) => Sample.t);

type matches_and_samples = {
  matches: match_result,
  samples: sample_closures,
};

/* What matching `dp` against `d` binds and what it leaves to match. */
let expand =
    (dp: Pat.t, d: DHExp.t)
    : unboxed((list(Environment.binding(Exp.t)), list(pending))) => {
  let ( let* ) = Unboxing.( let* );
  let sub = items => Matches(([], items));
  let all = (ps, ds) => sub(List.map2((p, d) => Match(p, d), ps, ds));
  switch (DHPat.term_of(dp)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild => sub([])
  | ExplicitNonlabel =>
    raise(
      Failure(
        "PatternMatch ExplicitNonlabel should not show up since these are removed during elaboration",
      ),
    )
  | Atom(c) =>
    let V(value, kind) = Atom.unpack(c);
    let* d' = Unboxing.unbox(Atom(kind), d);
    value == d' ? sub([]) : DoesNotMatch;
  | ListLit(xs) =>
    let* ds = Unboxing.unbox(ListLitn(List.length(xs)), d);
    all(xs, ds);
  | Cons(x, xs) =>
    let* (d1, d2) = Unboxing.unbox(Cons, d);
    all([x, xs], [d1, d2]);
  | Constructor(ctr, _) =>
    let* () = Unboxing.unbox(SumNoArg(ctr), d);
    sub([]);
  | Ap(c, p2) =>
    switch (Pat.ctr_name(c)) {
    | Some(ctr) =>
      let* d2 = Unboxing.unbox(SumWithArg(ctr), d);
      sub([Match(p2, d2)]);
    | None => IndetMatch
    }
  | Var(x) => Matches(([(x, d)], []))
  | Label(name) =>
    let* name' = Unboxing.unbox(Label, d);
    LabeledTuple.match_labels(name, name') ? sub([]) : DoesNotMatch;
  | TupLabel(_, x) =>
    let* x' = Unboxing.unbox(TupLabel(dp), d);
    sub([Match(x, x')]);
  | Tuple(ps) =>
    let* ds = Unboxing.unbox(Tuple(List.length(ps)), d);
    all(ps, ds);
  | Parens(p)
  | Projector(_, p) => sub([Match(p, d)])
  | Asc(p, t1) =>
    sub([
      Match(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh)),
    ])
  | Where(p, g) => sub([Match(p, d), Check(g)])
  };
};

/* A targeted sub-pattern, whose sample is recorded once everything inside
 * it has matched. `depth` is how many bindings there were when matching it
 * started. */
type to_record = {
  pat_id: Id.t,
  spec: Sample.capture_spec,
  value: DHExp.t,
  depth: int,
};

type work =
  | Pending(pending)
  | Record(to_record);

let matches =
    (targets: Sample.targets, dp: Pat.t, d: DHExp.t): matches_and_samples => {
  let samples = ref([]);
  /* `bound` is newest first. */
  let record = (bound, {pat_id, spec, value, depth}) => {
    let env =
      List.filteri((i, _) => i < List.length(bound) - depth, bound)
      |> List.rev
      |> Environment.of_bindings;
    samples :=
      [
        (call_stack, step_start, step_end) =>
          Sample.mk(
            ~step_start,
            ~step_end,
            pat_id,
            value,
            env,
            call_stack,
            spec,
          ),
        ...samples^,
      ];
  };
  /* An indeterminate sub-match doesn't stop the scan: a later one that
     doesn't match still makes the whole thing not match. */
  let rec go = (~indet, bound, work: list(work)): match_result =>
    switch (work) {
    | [] => indet ? IndetMatch : Matches(Complete(List.rev(bound)))
    | [Record(r), ...work] =>
      if (!indet) {
        record(bound, r);
      };
      go(~indet, bound, work);
    | [Pending(Check(_)), ..._] when indet => IndetMatch
    | [Pending(Check(guard)), ...work] =>
      /* Sub-patterns around the guard haven't finished matching; they
         record what they have bound so far. */
      List.iter(
        fun
        | Record(r) => record(bound, r)
        | Pending(_) => (),
        work,
      );
      let rest =
        List.filter_map(
          fun
          | Pending(p) => Some(p)
          | Record(_) => None,
          work,
        );
      Matches(
        Guarded({
          bindings: List.rev(bound),
          guard,
          rest,
        }),
      );
    | [Pending(Match(p, d)), ...work] =>
      let d = Ascriptions.transition_multiple(d);
      let work =
        switch (Id.Map.find_opt(Pat.rep_id(p), targets)) {
        | Some(spec) => [
            Record({
              pat_id: Pat.rep_id(p),
              spec,
              value: d,
              depth: List.length(bound),
            }),
            ...work,
          ]
        | None => work
        };
      switch (expand(p, d)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => go(~indet=true, bound, work)
      | Matches((bindings, items)) =>
        go(
          ~indet,
          List.rev(bindings) @ bound,
          List.map(item => Pending(item), items) @ work,
        )
      };
    };
  let matches = go(~indet=false, [], [Pending(Match(dp, d))]);
  {
    matches,
    samples: samples^,
  };
};

/* The rest of a guarded match as one pattern and the value it matches:
 * the pending sub-patterns as a tuple, with each pending guard attached to
 * the components before it, so it still sees their bindings. */
let rest_as_pattern = (rest: list(pending)): (Pat.t, DHExp.t) => {
  let tuple = (parts: list((Pat.t, DHExp.t))) =>
    switch (parts) {
    | [part] => part
    | _ => (
        Pat.fresh(Tuple(List.map(fst, parts))),
        DHExp.fresh(Tuple(List.map(snd, parts))),
      )
    };
  rest
  |> List.fold_left(
       (parts, item) =>
         switch (item) {
         | Match(p, d) => parts @ [(p, d)]
         | Check(g) =>
           let (p, d) = tuple(parts);
           [(Pat.fresh(Where(p, g)), d)];
         },
       [],
     )
  |> tuple;
};
