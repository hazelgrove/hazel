/* Which term ids a probe gesture at a piece targets, the probe status of
 * those targets, and the memoized per-row multi-probe expansion. Pure
 * queries over statics/syntax; ProbePerform applies them. */
open Language;

let rec target_subterm_ids =
        (~drill_let=true, id: Id.t, info_map: Statics.Map.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term: {term: Fun(pat, body, _, _), _}, _})) => [
      IdTagged.rep_id(body),
      IdTagged.rep_id(pat),
    ]
  | Some(InfoExp({user_term: {term: Let(pat, def, _), _} as let_term, _}))
      when drill_let =>
    /* Probe the def (drill_let=false: a nested let chain anchors whole); a test-body let probes itself to show the test result. */
    let is_test_body =
      switch (
        Statics.Map.parent_term_of(info_map, IdTagged.rep_id(let_term))
      ) {
      | Some(Exp({term: Test(_) | HintedTest(_, _), _})) => true
      | _ => false
      };
    if (is_test_body) {
      [IdTagged.rep_id(let_term)];
    } else {
      let def_targets =
        target_subterm_ids(~drill_let=false, IdTagged.rep_id(def), info_map);
      /* Function-sugar keeps params in the binder, so anchor the args pattern too. */
      switch (FunctionSugar.detect(pat)) {
      | Some((_f_name, args, _ret_ty)) => def_targets @ [Pat.rep_id(args)]
      | None => def_targets
      };
    };
  | Some(InfoExp({user_term: {term: ModuleExp(_, def, _), _}, _})) =>
    target_subterm_ids(IdTagged.rep_id(def), info_map)

  | Some(InfoExp({user_term: {term: Var(_), _} as v, _})) =>
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap))
        when Poly.equal(f_expr, v) => [
        IdTagged.rep_id(ap),
      ]
    | Some(Exp({term: DeferredAp(f_expr, _), _} as dap))
        when Poly.equal(f_expr, v) =>
      switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(dap))) {
      | Some(Exp({term: Ap(_, f_expr, _), _} as ap))
          when Poly.equal(f_expr, dap) => [
          IdTagged.rep_id(ap),
        ]
      | _ => [id]
      }
    | _ => [id]
    }
  | Some(InfoExp({user_term: {term: DeferredAp(_), _} as v, _})) =>
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap))
        when Poly.equal(f_expr, v) => [
        IdTagged.rep_id(ap),
      ]
    | _ => [id]
    }
  | info when !Info.is_typable_term(info) => []
  /* rep_id: multi-tile forms (tuples, lists, case) must match between probe_map and the evaluator. */
  | Some(InfoExp({user_term, _})) => [IdTagged.rep_id(user_term)]
  | Some(InfoPat({user_term, _})) => [Pat.rep_id(user_term)]
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t))
  | Statics(list(Id.t))
  | Multi
  | Ephemeral(list(Id.t))
  | Suppressed(list(Id.t))
  | Non;

let probe_status =
    (id: Id.t, info_map: Statics.Map.t, refractors: Zipper.Refractor.t)
    : probe_status => {
  let target_ids = target_subterm_ids(id, info_map);
  /* ANY (not ALL) target id: else a cleaned-up sibling target (remove_colliding_probes dropping a single-line fun's pat probe) makes the toggle re-add forever instead of removing. */
  let manual_entries =
    List.filter_map(
      ~f=id => List.Assoc.find(refractors.manuals, id, ~equal=Id.equal),
      target_ids,
    );
  if (!List.is_empty(manual_entries)) {
    let all_statics =
      List.for_all(
        ~f=
          (entry: Refractors.entry) =>
            ProjectorCore.Kind.equal(entry.kind, Statics),
        manual_entries,
      );
    all_statics ? Statics(target_ids) : Manual(target_ids);
  } else if (List.exists(
               ~f=id => Id.Map.mem(id, refractors.multis.ids),
               target_ids,
             )) {
    Multi;
  } else {
    let ephemeral_ids =
      List.filter(
        ~f=id => Id.Map.mem(id, refractors.multis.ephemerals),
        target_ids,
      );
    if (!List.is_empty(ephemeral_ids)) {
      Ephemeral(ephemeral_ids);
    } else {
      let suppressed_ids =
        List.filter(
          ~f=id => Id.Map.mem(id, refractors.multis.suppressed),
          target_ids,
        );
      if (!List.is_empty(suppressed_ids)) {
        Suppressed(suppressed_ids);
      } else {
        Non;
      };
    };
  };
};

/* Memoize the O(program) per-row multi-probe expansion. It's a pure function
 * of the immutable syntax/statics snapshots + anchor id, so we key on physical
 * identity of those refs (O(1)) and drop the table when any ref changes — a
 * stable-syntax run (pure caret moves) serves every anchor from cache. Single
 * global entry, so multiple editors invalidate each other (correct). */
let expansion_inputs:
  ref(option((TermData.t, TermMap.t, Measured.t, Statics.Map.t))) =
  ref(None);
let expansion_results: ref(list((Id.t, list(Id.t)))) = ref([]);

let ids_from_term =
    (~syntax: CachedSyntax.t, ~info_map, id: Id.t): list(Id.t) => {
  let inputs_stable =
    switch (expansion_inputs^) {
    | Some((term_data, terms, measured, prev_info_map)) =>
      phys_equal(term_data, syntax.term_data)
      && phys_equal(terms, syntax.terms)
      && phys_equal(measured, syntax.measured)
      && phys_equal(prev_info_map, info_map)
    | None => false
    };
  if (!inputs_stable) {
    expansion_inputs :=
      Some((syntax.term_data, syntax.terms, syntax.measured, info_map));
    expansion_results := [];
  };
  switch (List.Assoc.find(expansion_results^, id, ~equal=Id.equal)) {
  | Some(result) => result
  | None =>
    let result =
      MultiProbe.ids_to_multiprobe(
        id,
        syntax.term_data,
        syntax.terms,
        syntax.measured,
        info_map,
      )
      |> Option.to_list
      |> List.concat
      |> List.filter_map(~f=Fun.id);
    expansion_results := [(id, result), ...expansion_results^];
    result;
  };
};

let sort_ids_lexically =
    (~syntax: CachedSyntax.t, ids: list(Id.t)): list(Id.t) => {
  let with_positions =
    List.filter_map(
      ~f=
        id =>
          switch (
            TermData.extreme_measures(id, syntax.term_data, syntax.measured)
          ) {
          | Some((start_pt, _)) => Some((id, start_pt.row, start_pt.col))
          | None => None
          },
      ids,
    );
  let sorted =
    List.sort(
      ~compare=
        ((_, r1, c1), (_, r2, c2)) =>
          switch (Int.compare(r1, r2)) {
          | 0 => Int.compare(c1, c2)
          | n => n
          },
      with_positions,
    );
  List.map(~f=((id, _, _)) => id, sorted);
};
