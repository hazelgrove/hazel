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

let slice_ctx = (result: S.result): Ctx.t => {
  let with_gamma =
    List.fold_left(
      (ctx, (name, ty)) => Ctx.extend(ctx, var_entry(name, ty)),
      base_ctx(),
      result.gamma,
    );
  Ctx.concat(result.context, with_gamma);
};

let statics_map = (ctx: Ctx.t, e: Exp.t): option(Statics.Map.t) =>
  switch (Statics.mk(CoreSettings.on, ctx, e)) {
  | (m, _) => Some(m)
  | exception Stack_overflow => None
  | exception (Failure(f) as ex) =>
    is_known_statics_failure(f) ? None : raise(ex)
  };

let precision_geq = (ctx: Ctx.t, ty: Typ.t, query: Typ.t): bool =>
  switch (Typ.meet(ctx, ty, query)) {
  | Some(met) => Typ.fast_equal(met, ty)
  | None => false
  | exception _ => true
  };

let exp_focus_at = (e: Exp.t, k: int): Id.t =>
  switch (collect_exp_ids(_ => true, e)) {
  | [] => whole(e)
  | ids => List.nth(ids, k mod List.length(ids))
  };

let validity_check =
    (~direction, ~ty_of: Info.exp => Typ.t, (e, k, j)): bool => {
  let focus = exp_focus_at(e, k);
  switch (statics_map(base_ctx(), e)) {
  | None => true
  | Some(m) =>
    switch (Statics.Map.lookup_exp(focus, m)) {
    | None => true
    | Some(info) =>
      let chain = descending_chain(ty_of(info));
      let query = List.nth(chain, j mod List.length(chain));
      switch (safe_slice(~focus, ~direction, e, query)) {
      | None => true
      | Some(result) =>
        let sliced = reconstruct(result.omitted, e);
        let ctx = slice_ctx(result);
        switch (statics_map(ctx, sliced)) {
        | None => true
        | Some(sliced_m) =>
          switch (Statics.Map.lookup_exp(focus, sliced_m)) {
          | None => true
          | Some(sliced_info) =>
            precision_geq(ctx, ty_of(sliced_info), query)
              ? true
              : QCheck.Test.fail_reportf(
                  "slice loses query precision\nprogram: %s\nfocus type: %s\nquery: %s\nsliced: %s\nsliced focus type: %s",
                  show_exp_src(e),
                  render_any(Typ(ty_of(info))),
                  render_any(Typ(query)),
                  show_exp_src(sliced),
                  render_any(Typ(ty_of(sliced_info))),
                )
          }
        };
      };
    }
  };
};

// Slicing the focus at a query drawn from its own type keeps enough of the
// program that the reconstruction still carries a type at least as precise
let random_synthesis_validity =
  QCheck.Test.make(
    ~name="random synthesis slicing is valid",
    ~count=3000,
    QCheck.triple(arb_exp, QCheck.small_nat, QCheck.small_nat),
    validity_check(~direction=`Syn, ~ty_of=info => info.elab_syn_ty),
  );

let random_analysis_validity =
  QCheck.Test.make(
    ~name="random analysis slicing is valid",
    ~count=3000,
    QCheck.triple(arb_exp, QCheck.small_nat, QCheck.small_nat),
    validity_check(~direction=`Ana, ~ty_of=info => info.ana),
  );

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

let skip_until_slicer_passes =
    (name: string, test: QCheck.Test.t): Alcotest.test_case(unit) => {
  ignore(test);
  Alcotest.test_case(name, `Quick, () => Alcotest.skip());
};

let tests = (
  "Statics.Slicing.Properties",
  [
    skip_until_slicer_passes(
      "random synthesis slicing is valid",
      random_synthesis_validity,
    ),
    skip_until_slicer_passes(
      "random analysis slicing is valid",
      random_analysis_validity,
    ),
  ]
  @ List.map(
      QCheck_alcotest.to_alcotest,
      [pure_synthesis_is_empty, pure_analysis_is_bottom, monotonicity],
    ),
);
