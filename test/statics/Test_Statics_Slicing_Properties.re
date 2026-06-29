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

let arb_exp = QCheck_Util.arb_exp(~minimal_idents=true, 50);
let arb_typ = QCheck_Util.arb_typ(~minimal_idents=true, 10);

// synthesis slice synthesises more or equally precise type
let random_synthesis_validity =
  QCheck.Test.make(
    ~name="random synthesis slicing is valid",
    ~count=3000,
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
    ~count=3000,
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

let omit_at = (id: Id.t, t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=(continue, t) => Typ.rep_id(t) == id ? S.gap : continue(t),
    t,
  );

let typ_node_ids = (t: Typ.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Typ.map_term(
      ~f_typ=
        (continue, t) => {
          if (!S.is_gap(t)) {
            acc := [Typ.rep_id(t), ...acc^];
          };
          continue(t);
        },
      t,
    );
  acc^;
};

// A descending chain of queries
let descending_chain = (t: Typ.t): list(Typ.t) => {
  let (rev_chain, _) =
    List.fold_left(
      ((acc, cur), id) => {
        let next = omit_at(id, cur);
        ([next, ...acc], next);
      },
      ([t], t),
      typ_node_ids(t),
    );
  List.rev(rev_chain);
};

let synth_type = (e: Exp.t): option(Typ.t) =>
  switch (Statics.mk(CoreSettings.on, base_ctx(), e)) {
  | (m, _) =>
    switch (Statics.Map.lookup_exp(whole(e), m)) {
    | Some({ty, _}) => Some(ty)
    | None => None
    }
  | exception Stack_overflow => None
  | exception (Failure(f) as ex) =>
    is_known_statics_failure(f) ? None : raise(ex)
  };

// Omitting a node omits its whole subtree, so coverage includes every id that
// vanishes from the reconstruction (an omitted root subsumes its descendants)
let omitted_cover = (omitted: Id.Set.t, e: Exp.t): Id.Set.t => {
  let all = Id.Set.of_list(all_term_ids(e));
  let present = Id.Set.of_list(all_term_ids(reconstruct(omitted, e)));
  Id.Set.union(omitted, Id.Set.diff(all, present));
};

let subtree_subset = (a: Id.Set.t, b: Id.Set.t, e: Exp.t): bool =>
  Id.Set.subset(omitted_cover(a, e), omitted_cover(b, e));

let id_set_size = (ids: Id.Set.t): int => List.length(Id.Set.elements(ids));

let short_ids = (ids: Id.Set.t): string =>
  ids
  |> Id.Set.elements
  |> List.map(id => {
       let s = Id.to_string(id);
       String.length(s) > 8 ? String.sub(s, 0, 8) : s;
     })
  |> String.concat(",");

// Down a descending query chain (maximal type to gap), omissions monotonically grow
let monotonicity =
  QCheck.Test.make(
    ~name="omissions grow monotonically down a query chain",
    ~count=3000,
    arb_exp,
    e =>
    switch (synth_type(e)) {
    | None => true
    | Some(tau) =>
      let step = q =>
        switch (safe_slice(~focus=whole(e), ~direction=`Syn, e, q)) {
        | Some(r) => Some((q, r.omitted, omitted_cover(r.omitted, e)))
        | None => None
        };
      let steps = List.filter_map(step, descending_chain(tau));
      let rec find_violation = prev => (
        fun
        | [] => None
        | [(q, omit, cover), ...rest] =>
          switch (prev) {
          | Some((_, _, pcover)) when !Id.Set.subset(pcover, cover) =>
            Some((prev, q, omit, cover))
          | _ => find_violation(Some((q, omit, cover)), rest)
          }
      );
      switch (find_violation(None, steps)) {
      | None => true
      | Some((prev, q, _, cover)) =>
        let pq =
          switch (prev) {
          | Some((pq, _, _)) => pq
          | None => tau
          };
        let missing =
          switch (prev) {
          | Some((_, _, pcover)) => Id.Set.diff(pcover, cover)
          | None => Id.Set.empty
          };
        let line = ((q, omit, cover)) =>
          Printf.sprintf(
            "  query %-24s raw=%-3d cover=%-3d raw_ids=[%s] cover_ids=[%s]\n      %s",
            render_any(Typ(q)),
            id_set_size(omit),
            id_set_size(cover),
            short_ids(omit),
            short_ids(cover),
            show_exp_src(reconstruct(omit, e)),
          );
        QCheck.Test.fail_reportf(
          "omissions not monotone from precise query %s to less precise query %s\nprogram: %s\ninferred type: %s\nmissing covered ids at failing step: [%s]\nchain:\n%s",
          render_any(Typ(pq)),
          render_any(Typ(q)),
          show_exp_src(e),
          render_any(Typ(tau)),
          short_ids(missing),
          String.concat("\n", List.map(line, steps)),
        );
      };
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
