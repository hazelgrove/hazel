// let string_of_constramnots = (cs: list(Typ.equivalence)): string => {
//   "{" ++ String.concat("\n", List.map(Typ.show_equivalence, cs)) ++ "}";
// };

let rec solution_typ_replace_typ =
        (
          prov: StringProv.t,
          typ: Typ.t,
          sol_typ: Typ.t,
          prov_map: PossibleProvTypesMap.t,
        )
        : Typ.t => {
  let (unwrapped_typ, rewrap_typ) = Typ.unwrap(typ);
  switch (unwrapped_typ) {
  | Unknown(q) when prov == StringProv.of_prov(q) => sol_typ
  // | Hole(q) => Hole(q)
  | Unknown(_) as u => u |> rewrap_typ
  | Atom(_) as atom => atom |> rewrap_typ
  | List(t) =>
    List(solution_typ_replace_typ(prov, t, sol_typ, prov_map)) |> rewrap_typ
  | Poly(pat, body) =>
    Poly(pat, solution_typ_replace_typ(prov, body, sol_typ, prov_map))
    |> rewrap_typ
  | Sum(_) as sum => sum |> rewrap_typ
  | Var(_) as var => var |> rewrap_typ
  | Prod(args) =>
    Prod(
      List.map(
        arg => solution_typ_replace_typ(prov, arg, sol_typ, prov_map),
        args,
      ),
    )
    |> rewrap_typ
  | Label(_) as label => label |> rewrap_typ
  | TupLabel(label, ty) =>
    TupLabel(
      solution_typ_replace_typ(prov, label, sol_typ, prov_map),
      solution_typ_replace_typ(prov, ty, sol_typ, prov_map),
    )
    |> rewrap_typ
  | Parens(term) => solution_typ_replace_typ(prov, term, sol_typ, prov_map)
  | Rec(pat, body) =>
    Rec(pat, solution_typ_replace_typ(prov, body, sol_typ, prov_map))
    |> rewrap_typ
  | Arrow(t1, t2) =>
    Arrow(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  | ProofOf(_) as st => st |> rewrap_typ
  | ExplicitNonlabel as st => st |> rewrap_typ
  | ProdExtension(t1, t2) =>
    ProdExtension(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  | ProdProjection(t1, t2) =>
    ProdProjection(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  };
};

let solution_typ_replace_con =
    (
      prov_to_replace: StringProv.t,
      Con(cons_t1, cons_t2): Typ.equivalence,
      sol_typ: Typ.t,
      prov_map: PossibleProvTypesMap.t,
    )
    : Typ.equivalence => {
  Con(
    solution_typ_replace_typ(prov_to_replace, cons_t1, sol_typ, prov_map),
    solution_typ_replace_typ(prov_to_replace, cons_t2, sol_typ, prov_map),
  );
};

let solution_typ_replace_cons =
    (
      prov_to_replace: StringProv.t,
      constraints: list(Typ.equivalence),
      sol_typ: Typ.t,
      prov_map: PossibleProvTypesMap.t,
    )
    : list(Typ.equivalence) =>
  List.map(
    c => solution_typ_replace_con(prov_to_replace, c, sol_typ, prov_map),
    constraints,
  );

let extend_sol_map =
    (
      constraints: list(Typ.equivalence),
      sol_map: SolutionMap.t,
      cyclic_provs: list(StringProv.t),
    )
    : option((list(Typ.equivalence), SolutionMap.t, list(StringProv.t))) => {
  // print_endline("Constraints:");
  // print_endline(string_of_constramnots(constraints));
  let canonical_cs = CanonicalConstramnot.unfold_constramnots(constraints); // make constraints canonical
  // String.concat(
  //   "\n",
  //   List.map(s => show_canonical_constramnot(s), canonical_cs),
  // )
  // |> print_endline;
  let prov_map = PossibleProvTypesMap.of_constramnots(canonical_cs, sol_map); // compute provenance map
  // print_endline("Provenance Map:");
  // print_endline(string_of_prov_map(m));
  switch (PossibleProvTypesMap.find_dominant_provs(prov_map)) {
  // if you find a dominant provenance...
  | ([], _) => None
  | ([prov_to_solve, ..._], is_solution_cyclic) =>
    Some(
      {
        // print_endline(
        //   "Solving: " ++ (StringProv.of_prov(p) |> StringProv.show),
        // );
        let sol = PossibleProvTypesMap.solve_prov(prov_to_solve, prov_map); // solve it
        // print_endline("Solution: " ++ show_solution(s));

        // identify all provenances that are merged with the provenance
        // that was just solved
        let equiv_provs: list(StringProv.t) =
          List.filter_map(
            ((other_prov, _)) => {
              let are_provs_equivalent =
                UnionFind.eq(
                  PossibleProvTypesMap.lookup_prov(prov_to_solve, prov_map),
                  PossibleProvTypesMap.find(other_prov, prov_map),
                );
              if (are_provs_equivalent) {
                Some(other_prov);
              } else {
                None;
              };
            },
            PossibleProvTypesMap.bindings(prov_map),
          );
        // print_endline(
        //   "Equivalent provs: "
        //   ++ String.concat(",", List.map(StringProv.show, equiv_provs)),
        // );

        let cyclic_provs' =
          if (is_solution_cyclic) {
            List.append(cyclic_provs, equiv_provs);
          } else {
            cyclic_provs;
          };

        let solution_type = Solution.solution_typ(sol); // turn it into a type

        // replace the unsolved provenances in the constraints
        // with the solution type we just derived
        // e.g. ?1 ~ Int; { ?1 } -> { ?2 } ==> { Int } -> { ?2 }
        let constraints' =
          List.fold_left(
            (cs_acc, pss) => {
              solution_typ_replace_cons(pss, cs_acc, solution_type, prov_map)
            },
            constraints,
            equiv_provs,
          );

        // extend the solution map with the provenances we just solved
        // once solved, a provenance will never be re-added to the map
        let sol_map' =
          List.fold_left(
            (sm_acc, pss) => SolutionMap.add(pss, sol, sm_acc),
            sol_map,
            equiv_provs,
          );

        // identify all the provenances in the solution, so we can
        // later check if the solution contains a provenance that
        // we just solved
        let all_provs_in_sol =
          List.map(StringProv.of_prov, Solution.all_provs_in_sol(sol));

        // replace the solutions of all existing provenances with
        // we just generated
        let sol_map'' =
          List.fold_left(
            (sol_map_acc, curr_prov) => {
              // a prov is defined to be cyclic if we solved for it, but it
              // appears inside the solution
              let is_prov_cyclic = List.mem(curr_prov, all_provs_in_sol);
              SolutionMap.map(
                sol_to_update => {
                  let (updated_sol, replaced_any) =
                    Solution.replace_solution(curr_prov, sol_to_update, sol);

                  if (replaced_any && is_prov_cyclic) {
                    // if the solution is cyclic, then the original solution should
                    // be identical to sol, except with possibly a few more substitutions
                    // of the cyclic solution.
                    // to make the substitution count consistent, replace the old solution
                    // TODO: i think this might have edge cases, when a cyclic solution is nested
                    sol;
                  } else {
                    updated_sol;
                  };
                },
                sol_map_acc,
              );
            },
            sol_map',
            equiv_provs,
          );

        (constraints', sol_map'', cyclic_provs');
      },
    )
  };
};

let rec solve_rec =
        (
          constraints: list(Typ.equivalence),
          sol_map: SolutionMap.t,
          cyclic_provs: list(StringProv.t),
        )
        : SolutionMap.t => {
  switch (extend_sol_map(constraints, sol_map, cyclic_provs)) {
  | None =>
    // print_endline("No dominant provenances");
    // print_endline(string_of_constramnots(cs));
    SolutionMap.replace_cycles(sol_map, cyclic_provs)
  | Some((cs', sm', cyclic_provs')) => solve_rec(cs', sm', cyclic_provs')
  };
};

let solve = (cs: list(Typ.equivalence)): SolutionMap.t => {
  // print_endline("SOLVING");
  solve_rec(cs, SolutionMap.empty, []);
};

let go = (cs: list(Typ.equivalence)): SolutionMap.t => {
  solve(
    cs,
    // let cs = unfold_constramnots(cs);
    // let m = prov_map_of_constramnots(cs);
    // print_endline("go2");
    // print_endline(string_of_int(List.length(StringMap.to_list(m))));
    // print_endline(string_of_prov_map(m));
  );
};
