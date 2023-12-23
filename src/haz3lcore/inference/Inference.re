/**
 * NOTE:
 * Current formulation does not unify constraints comparing inconsistent constructors.
 * Unifying these would cause PotentialTypeSets to be potentially considered invalid without any
 * inconsistencies within them, which is a confusing result to represent to a user and may
 * pollute other equivalence classes with unhelpful error statuses that static inference can
 * already give better results on.
 * We decide here that we will only draw inference results on holes and the things these holes
 * are compared to through their neighborhood of implied consistencies as governed by attempted
 * consistency checks in synthesis and analysis.
 */
// A unification algorithm based on Huet's unification, adjusted so it does not fail
let rec unify = (pts_graph: PTSGraph.t, constraints: ITyp.constraints): unit => {
  List.iter(unify_one(pts_graph), constraints);
}
and unify_one = (pts_graph: PTSGraph.t, typs: (ITyp.t, ITyp.t)): unit => {
  switch (typs) {
  | (List(ty1), List(ty2)) => unify_one(pts_graph, (ty1, ty2))
  | (Arrow(ty1_lhs, ty1_rhs), Arrow(ty2_lhs, ty2_rhs))
  | (Prod(ty1_lhs, ty1_rhs), Prod(ty2_lhs, ty2_rhs))
  | (Sum(ty1_lhs, ty1_rhs), Sum(ty2_lhs, ty2_rhs)) =>
    unify(pts_graph, [(ty1_lhs, ty2_lhs), (ty1_rhs, ty2_rhs)])
  | (Unknown(_) as hole, t)
  | (t, Unknown(_) as hole) =>
    PTSGraph.add_typ_as_node(pts_graph, hole);

    if (ITyp.contains_hole(t)) {
      // if the type it is being constrained to is a potential node, add it then connect the two nodes
      PTSGraph.add_typ_as_node(pts_graph, t);
      PTSGraph.make_occurs_check(pts_graph, t, hole);
      PTSGraph.create_traversable_edge(pts_graph, t, hole);
    } else {
      // otherwise, simply add t to hole's PotentialTypeSet without making a new node
      PTSGraph.create_solution_edge(
        pts_graph,
        hole,
        t,
      );
    };
  | _ => ()
  };
};

let unify_and_report_status =
    (constraints: Typ.constraints): list(InferenceResult.t) => {
  let inference_pts_graph = PTSGraph.create();
  let constraints = ITyp.to_ityp_constraints(constraints);

  unify(inference_pts_graph, constraints);

  let acc_results =
      (
        key: ITyp.t,
        mut_potential_typ_set: MutablePotentialTypeSet.t,
        acc: list(InferenceResult.t),
      )
      : list(InferenceResult.t) => {
    [(key, InferenceResult.condense(mut_potential_typ_set, key)), ...acc];
  };

  let unsorted_results = Hashtbl.fold(acc_results, inference_pts_graph, []);

  List.fast_sort(InferenceResult.comp_results, unsorted_results);
};
