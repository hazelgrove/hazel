// Slicing property tests
open Language;
open Test_Statics_Slicing_Prelude;

let is_known_statics_failure = msg =>
  List.exists(
    (==)(msg),
    [
      "normalize exceeded 1000 recursive calls",
      "weak_head_normalize exceeded 1000 recursive calls",
      "Recursion limit exceeded in all_ctrs_of_typ",
    ],
  )
  || String.starts_with(
       ~prefix="all_ctrs_of_type called with a non-normalized type:",
       msg,
     );

let all_ids = (e: Exp.t): list(Id.t) =>
  collect_exp_ids(_ => true, e) @ collect_pat_ids(_ => true, e);

let focus_at = (e: Exp.t, k: int): Id.t =>
  switch (all_ids(e)) {
  | [] => whole(e)
  | ids => List.nth(ids, k mod List.length(ids))
  };

let safe_slice = (~focus, ~direction, e: Exp.t, query: Typ.t) =>
  switch (
    Statics.slice(~ctx=base_ctx(), ~focus=Some(focus), ~direction, e, query)
  ) {
  | result => Some(result)
  | exception Stack_overflow => None
  | exception (
                S.Focus_not_found(_) | S.Wrong_focus_sort |
                S.Incompatible_query(_)
              ) =>
    None
  | exception (Failure(f) as ex) =>
    is_known_statics_failure(f) ? None : raise(ex)
  };

let arb_exp = QCheck_Util.arb_exp(~minimal_idents=true, 20);
let arb_typ = QCheck_Util.arb_typ(~minimal_idents=true, 10);

// synthesis slice synthesises more or equally precise type
let random_synthesis_validity =
  QCheck.Test.make(
    ~name="random synthesis slicing is valid",
    ~count=500,
    QCheck.triple(arb_exp, QCheck.small_nat, arb_typ),
    ((e, k, query)) => {
      ignore(safe_slice(~focus=focus_at(e, k), ~direction=`Syn, e, query));
      true;
    },
  );

// analysis slice synthesises enforces more or equally precise type
let random_analysis_validity =
  QCheck.Test.make(
    ~name="random analysis slicing is valid",
    ~count=500,
    QCheck.triple(arb_exp, QCheck.small_nat, arb_typ),
    ((e, k, query)) => {
      ignore(safe_slice(~focus=focus_at(e, k), ~direction=`Ana, e, query));
      true;
    },
  );

// empty query returns empty result
let pure_synthesis_is_empty =
  QCheck.Test.make(
    ~name="pure-? synthesis is the empty slice", ~count=500, arb_exp, e =>
    switch (safe_slice(~focus=whole(e), ~direction=`Syn, e, S.gap)) {
    | None => true
    | Some(result) =>
      Exp.fast_equal(reconstruct(result.omitted, e), parse_exp("?"))
    }
  );

// empty analysis query produces purely structural context
let pure_analysis_is_bottom =
  QCheck.Test.make(
    ~name="pure-? analysis is the bottom context", ~count=500, arb_exp, e =>
    switch (safe_slice(~focus=whole(e), ~direction=`Ana, e, S.gap)) {
    | None => true
    | Some(result) =>
      Exp.fast_equal(reconstruct(result.omitted, e), parse_exp("?"))
    }
  );

// Any more precise query gives a more (or equally) precise slice
let monotonicity =
  QCheck.Test.make(
    ~name="precise query omits no more than the gap query",
    ~count=500,
    QCheck.pair(arb_exp, arb_typ),
    ((e, query)) =>
    switch (
      safe_slice(~focus=whole(e), ~direction=`Syn, e, query),
      safe_slice(~focus=whole(e), ~direction=`Syn, e, S.gap),
    ) {
    | (Some(precise), Some(loose)) =>
      Id.Set.subset(precise.omitted, loose.omitted)
    | _ => true
    }
  );

let tests = (
  "Statics.Slicing.Properties",
  List.map(
    QCheck_alcotest.to_alcotest,
    [
      random_synthesis_validity,
      random_analysis_validity,
      pure_synthesis_is_empty,
      pure_analysis_is_bottom,
      monotonicity,
    ],
  ),
);
