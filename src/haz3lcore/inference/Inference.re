let rec unify = (eq_graph: EqGraph.t, constraints: ITyp.constraints): unit => {
  List.iter(unify_one(eq_graph), constraints);
}
and unify_one = (eq_graph: EqGraph.t, typs: (ITyp.t, ITyp.t)): unit => {
  switch (typs) {
  | (Arrow(ty1_lhs, ty1_rhs), Arrow(ty2_lhs, ty2_rhs))
  | (Prod(ty1_lhs, ty1_rhs), Prod(ty2_lhs, ty2_rhs)) =>
    unify(eq_graph, [(ty1_lhs, ty2_lhs), (ty1_rhs, ty2_rhs)])
  | (Unknown(_) as hole, t)
  | (t, Unknown(_) as hole) =>
    EqGraph.add_typ_as_node(eq_graph, hole);

    if (ITyp.contains_hole(t)) {
      EqGraph.add_typ_as_node(eq_graph, t);

      EqGraph.make_occurs_check(eq_graph, t, hole);
      EqGraph.equate_typs(eq_graph, t, hole);
    };
  | _ => ()
  };
};

let unify_and_report_status =
    (constraints: Typ.constraints): list(InferenceResult.t) => {
  let inference_eq_graph = EqGraph.create();
  let constraints = ITyp.to_ityp_constraints(constraints);

  unify(inference_eq_graph, constraints);

  let acc_results =
      (
        key: ITyp.t,
        mut_eq_class: MutableEqClass.t,
        acc: list(InferenceResult.t),
      )
      : list(InferenceResult.t) => {
    [(key, InferenceResult.condense(mut_eq_class)), ...acc];
  };

  let unsorted_results = Hashtbl.fold(acc_results, inference_eq_graph, []);

  List.fast_sort(InferenceResult.comp_results, unsorted_results);
};
