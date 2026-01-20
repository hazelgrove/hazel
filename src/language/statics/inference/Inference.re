// let string_of_constramnots = (cs: list(Typ.equivalence)): string => {
//   "{" ++ String.concat("\n", List.map(Typ.show_equivalence, cs)) ++ "}";
// };

module TypProvMap =
  PossibleProvTypesMap.Make(
    Solution.TypSolution,
    PossibleProvTypesMap.TypInfo,
  );
module TPatProvMap =
  PossibleProvTypesMap.Make(
    Solution.TPatSolution,
    PossibleProvTypesMap.TPatInfo,
  );

module TypSolutionMap = SolutionMap.Make(Solution.TypSolution);
module TPatSolutionMap = SolutionMap.Make(Solution.TPatSolution);

type solution_maps = (TypSolutionMap.t, TPatSolutionMap.t);

let rec solution_typ_replace_tpat =
        (prov: StringProv.t, typ: Typ.t, sol_tpat: TPat.t): Typ.t => {
  let (unwrapped_typ, rewrap_typ) = Typ.unwrap(typ);
  switch (unwrapped_typ) {
  | Unknown(_) as u => u |> rewrap_typ
  | Atom(_) as atom => atom |> rewrap_typ
  | List(t) =>
    List(solution_typ_replace_tpat(prov, t, sol_tpat)) |> rewrap_typ
  | Poly(tpat, body) =>
    let (unwrapped_tpat, rewrap_tpat) = IdTagged.unwrap(tpat);
    let tpat' =
      switch (unwrapped_tpat) {
      | Unknown(p) when prov == StringProv.of_prov(p) => sol_tpat
      | Unknown(_) as u => u |> rewrap_tpat
      | Var(_) as v => v |> rewrap_tpat
      };

    Poly(tpat', solution_typ_replace_tpat(prov, body, sol_tpat))
    |> rewrap_typ;
  | Sum(_) as sum => sum |> rewrap_typ
  | Var(_) as var => var |> rewrap_typ
  | Prod(args) =>
    Prod(
      List.map(arg => solution_typ_replace_tpat(prov, arg, sol_tpat), args),
    )
    |> rewrap_typ
  | Label(_) as label => label |> rewrap_typ
  | TupLabel(label, ty) =>
    TupLabel(
      solution_typ_replace_tpat(prov, label, sol_tpat),
      solution_typ_replace_tpat(prov, ty, sol_tpat),
    )
    |> rewrap_typ
  | Parens(term) => solution_typ_replace_tpat(prov, term, sol_tpat)
  | Rec(pat, body) =>
    Rec(pat, solution_typ_replace_tpat(prov, body, sol_tpat)) |> rewrap_typ
  | Arrow(t1, t2) =>
    Arrow(
      solution_typ_replace_tpat(prov, t1, sol_tpat),
      solution_typ_replace_tpat(prov, t2, sol_tpat),
    )
    |> rewrap_typ
  | ProofOf(_) as st => st |> rewrap_typ
  | ExplicitNonlabel as st => st |> rewrap_typ
  | ProdExtension(t1, t2) =>
    ProdExtension(
      solution_typ_replace_tpat(prov, t1, sol_tpat),
      solution_typ_replace_tpat(prov, t2, sol_tpat),
    )
    |> rewrap_typ
  | ProdProjection(t1, t2) =>
    ProdProjection(
      solution_typ_replace_tpat(prov, t1, sol_tpat),
      solution_typ_replace_tpat(prov, t2, sol_tpat),
    )
    |> rewrap_typ
  };
};

let rec solution_typ_replace_typ =
        (prov: StringProv.t, typ: Typ.t, sol_typ: Typ.t): Typ.t => {
  let (unwrapped_typ, rewrap_typ) = Typ.unwrap(typ);
  switch (unwrapped_typ) {
  | Unknown(q) when prov == StringProv.of_prov(q) => sol_typ
  | Unknown(_) as u => u |> rewrap_typ
  | Atom(_) as atom => atom |> rewrap_typ
  | List(t) => List(solution_typ_replace_typ(prov, t, sol_typ)) |> rewrap_typ
  | Poly(pat, body) =>
    Poly(pat, solution_typ_replace_typ(prov, body, sol_typ)) |> rewrap_typ
  | Sum(_) as sum => sum |> rewrap_typ
  | Var(_) as var => var |> rewrap_typ
  | Prod(args) =>
    Prod(
      List.map(arg => solution_typ_replace_typ(prov, arg, sol_typ), args),
    )
    |> rewrap_typ
  | Label(_) as label => label |> rewrap_typ
  | TupLabel(label, ty) =>
    TupLabel(
      solution_typ_replace_typ(prov, label, sol_typ),
      solution_typ_replace_typ(prov, ty, sol_typ),
    )
    |> rewrap_typ
  | Parens(term) => solution_typ_replace_typ(prov, term, sol_typ)
  | Rec(pat, body) =>
    Rec(pat, solution_typ_replace_typ(prov, body, sol_typ)) |> rewrap_typ
  | Arrow(t1, t2) =>
    Arrow(
      solution_typ_replace_typ(prov, t1, sol_typ),
      solution_typ_replace_typ(prov, t2, sol_typ),
    )
    |> rewrap_typ
  | ProofOf(_) as st => st |> rewrap_typ
  | ExplicitNonlabel as st => st |> rewrap_typ
  | ProdExtension(t1, t2) =>
    ProdExtension(
      solution_typ_replace_typ(prov, t1, sol_typ),
      solution_typ_replace_typ(prov, t2, sol_typ),
    )
    |> rewrap_typ
  | ProdProjection(t1, t2) =>
    ProdProjection(
      solution_typ_replace_typ(prov, t1, sol_typ),
      solution_typ_replace_typ(prov, t2, sol_typ),
    )
    |> rewrap_typ
  };
};

let solution_typ_replace_con =
    (
      ~replace_typ_with,
      prov_to_replace: StringProv.t,
      Con(cons_t1, cons_t2): Typ.equivalence,
      sol_typ,
    )
    : Typ.equivalence => {
  Con(
    replace_typ_with(prov_to_replace, cons_t1, sol_typ),
    replace_typ_with(prov_to_replace, cons_t2, sol_typ),
  );
};

let solution_typ_replace_cons =
    (
      ~replace_typ_with,
      prov_to_replace: StringProv.t,
      constraints: list(Typ.equivalence),
      sol_typ,
    )
    : list(Typ.equivalence) =>
  List.map(
    c =>
      solution_typ_replace_con(
        prov_to_replace,
        c,
        sol_typ,
        ~replace_typ_with,
      ),
    constraints,
  );

let solution_typ_replace_typ_cons =
  solution_typ_replace_cons(~replace_typ_with=solution_typ_replace_typ);
let solution_typ_replace_tpat_cons =
  solution_typ_replace_cons(~replace_typ_with=solution_typ_replace_tpat);

let rec solution_substitute_var =
        (
          statics_map: StaticsBase.Map.t,
          tvars: list(TPat.t),
          sol: Solution.TypSolution.t,
          sub_with: Solution.TypSolution.t,
        ) => {
  print_endline(sol |> Solution.TypSolution.show);
  List.iter(tyvar => print_endline(tyvar |> TPat.show), tvars);
  let solution_substitute_var = solution_substitute_var(statics_map);

  let (unwrapped_sol, rewrap_sol) = IdTagged.unwrap(sol);
  switch (unwrapped_sol) {
  | Unknown(_) as u => u |> rewrap_sol
  | Atom(_) as atom => atom |> rewrap_sol
  | List(t) =>
    List(solution_substitute_var(tvars, t, sub_with)) |> rewrap_sol
  | Rec(pat, body) =>
    Rec(pat, solution_substitute_var(tvars, body, sub_with)) |> rewrap_sol
  | Poly(tpat, body) =>
    let tvars' =
      switch (tpat |> IdTagged.term_of) {
      | Var(_) =>
        List.filter(
          tvar => {IdTagged.rep_id(tvar) != IdTagged.rep_id(tpat)},
          tvars,
        )
      | Multi(_) => tvars
      | Unknown(_) => tvars
      };
    Poly(tpat, solution_substitute_var(tvars', body, sub_with)) |> rewrap_sol;
  | Sum(_) as sum => sum |> rewrap_sol
  | Var(_) as v =>
    let var_id = IdTagged.rep_id(sol);
    Uuidm.to_string(var_id) |> print_endline;
    switch (StaticsBase.Map.lookup(var_id, statics_map)) {
    | Some(info) =>
      switch (info) {
      | InfoTyp(info) =>
        let substitute =
          List.exists(
            (tpat: TPat.t) => {
              switch (tpat |> IdTagged.term_of) {
              | Var(var_name) =>
                switch (Ctx.lookup_tvar_id(info.ctx, var_name)) {
                | Some(id) => IdTagged.rep_id(tpat) == id
                | None => false
                }
              | Unknown(_) => false
              }
            },
            tvars,
          );

        print_endline(substitute |> string_of_bool);
        substitute ? sol : v |> rewrap_sol;
      | _ =>
        print_endline("cooked two");
        v |> rewrap_sol;
      }
    | None =>
      print_endline("cooked one");
      v |> rewrap_sol;
    };
  | Prod(args) =>
    Prod(
      List.map(arg => solution_substitute_var(tvars, arg, sub_with), args),
    )
    |> rewrap_sol
  | Label(_) as label => label |> rewrap_sol
  | TupLabel(label, ty) =>
    TupLabel(
      solution_substitute_var(tvars, label, sub_with),
      solution_substitute_var(tvars, ty, sub_with),
    )
    |> rewrap_sol
  | Arrow(t1, t2) =>
    Arrow(
      solution_substitute_var(tvars, t1, sub_with),
      solution_substitute_var(tvars, t2, sub_with),
    )
    |> rewrap_sol
  | ProofOf(_) as st => st |> rewrap_sol
  | ExplicitNonlabel as st => st |> rewrap_sol
  | ProdExtension(t1, t2) =>
    ProdExtension(
      solution_substitute_var(tvars, t1, sub_with),
      solution_substitute_var(tvars, t2, sub_with),
    )
    |> rewrap_sol
  | ProdProjection(t1, t2) =>
    ProdProjection(
      solution_substitute_var(tvars, t1, sub_with),
      solution_substitute_var(tvars, t2, sub_with),
    )
    |> rewrap_sol
  | Multi(sols) =>
    Multi(
      List.map(sol' => solution_substitute_var(tvars, sol', sub_with), sols),
    )
    |> rewrap_sol
  };
};

let extend_sol_map =
    (
      constraints: list(Typ.equivalence),
      (typ_sol_map, tpat_sol_map): solution_maps,
      cyclic_provs: list(StringProv.t),
    )
    : option((list(Typ.equivalence), solution_maps, list(StringProv.t))) => {
  // print_endline("Constraints:");
  // print_endline(string_of_constramnots(constraints));
  let canonical_cs = CanonicalConstramnot.unfold_constramnots(constraints); // make constraints canonical
  // String.concat(
  //   "\n",
  //   List.map(s => show_canonical_constramnot(s), canonical_cs),
  // )
  // |> print_endline;

  let (typ_cons, tpat_cons) =
    List.partition_map(
      v => {
        switch (v) {
        | CanonicalConstramnot.Typ(equiv) => Either.Left(equiv)
        | CanonicalConstramnot.TPat(equiv) => Either.Right(equiv)
        }
      },
      canonical_cs,
    );

  let typ_prov_map = TypProvMap.of_constramnots(typ_cons, typ_sol_map); // compute provenance map
  let tpat_prov_map = TPatProvMap.of_constramnots(tpat_cons, tpat_sol_map);
  // print_endline("Provenance Map:");
  switch (TypProvMap.find_dominant_provs(typ_prov_map)) {
  // if you find a dominant provenance...
  | ([], _) =>
    switch (TPatProvMap.find_dominant_provs(tpat_prov_map)) {
    | ([], _) => None
    | ([prov_to_solve, ..._], is_solution_cyclic) =>
      Some(
        {
          let sol = TPatProvMap.solve_prov(prov_to_solve, tpat_prov_map); // solve it

          let equiv_provs: list(StringProv.t) =
            List.filter_map(
              ((other_prov, _)) => {
                let are_provs_equivalent =
                  UnionFind.eq(
                    TPatProvMap.lookup_prov(prov_to_solve, tpat_prov_map),
                    TPatProvMap.find(other_prov, tpat_prov_map),
                  );

                are_provs_equivalent ? Some(other_prov) : None;
              },
              TPatProvMap.bindings(tpat_prov_map),
            );

          let cyclic_provs' =
            if (is_solution_cyclic) {
              List.append(cyclic_provs, equiv_provs);
            } else {
              cyclic_provs;
            };

          let solution_tpat = Solution.TPatSolution.solution_term(sol); // turn it into a type

          let constraints' =
            List.fold_left(
              (cs_acc, pss) => {
                solution_typ_replace_tpat_cons(pss, cs_acc, solution_tpat)
              },
              constraints,
              equiv_provs,
            );

          let tpat_sol_map' =
            List.fold_left(
              (sm_acc, pss) => TPatSolutionMap.add(pss, sol, sm_acc),
              tpat_sol_map,
              equiv_provs,
            );

          let all_provs_in_sol =
            List.map(
              StringProv.of_prov,
              Solution.TPatSolution.all_provs_of(sol),
            );

          let tpat_sol_map'' =
            List.fold_left(
              (sol_map_acc, curr_prov) => {
                let is_prov_cyclic = List.mem(curr_prov, all_provs_in_sol);
                TPatSolutionMap.map(
                  sol_to_update => {
                    let (updated_sol, replaced_any) =
                      Solution.TPatSolution.replace_solution(
                        curr_prov,
                        sol_to_update,
                        sol,
                      );

                    replaced_any && is_prov_cyclic ? sol : updated_sol;
                  },
                  sol_map_acc,
                );
              },
              tpat_sol_map',
              equiv_provs,
            );

          let typ_sol_map' =
            List.fold_left(
              (sol_map_acc, curr_prov) => {
                TypSolutionMap.map(
                  sol_to_update => {
                    let (updated_sol, _) =
                      Solution.TypSolution.replace_tpat_solution(
                        curr_prov,
                        sol_to_update,
                        sol,
                      );

                    updated_sol;
                  },
                  sol_map_acc,
                )
              },
              typ_sol_map,
              equiv_provs,
            );

          (constraints', (typ_sol_map', tpat_sol_map''), cyclic_provs');
        },
      )
    }
  | ([prov_to_solve, ..._], is_solution_cyclic) =>
    Some(
      {
        // print_endline(
        //   "Solving: " ++ (StringProv.of_prov(p) |> StringProv.show),
        // );
        let sol = TypProvMap.solve_prov(prov_to_solve, typ_prov_map); // solve it
        // print_endline("Solution: " ++ show_solution(s));

        // identify all provenances that are merged with the provenance
        // that was just solved
        let equiv_provs: list(StringProv.t) =
          List.filter_map(
            ((other_prov, _)) => {
              let are_provs_equivalent =
                UnionFind.eq(
                  TypProvMap.lookup_prov(prov_to_solve, typ_prov_map),
                  TypProvMap.find(other_prov, typ_prov_map),
                );
              if (are_provs_equivalent) {
                Some(other_prov);
              } else {
                None;
              };
            },
            TypProvMap.bindings(typ_prov_map),
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

        let solution_type = Solution.TypSolution.solution_term(sol); // turn it into a type

        // replace the unsolved provenances in the constraints
        // with the solution type we just derived
        // e.g. ?1 ~ Int; { ?1 } -> { ?2 } ==> { Int } -> { ?2 }
        let constraints' =
          List.fold_left(
            (cs_acc, pss) => {
              solution_typ_replace_typ_cons(pss, cs_acc, solution_type)
            },
            constraints,
            equiv_provs,
          );

        // extend the solution map with the provenances we just solved
        // once solved, a provenance will never be re-added to the map
        let typ_sol_map' =
          List.fold_left(
            (sm_acc, pss) => TypSolutionMap.add(pss, sol, sm_acc),
            typ_sol_map,
            equiv_provs,
          );

        // identify all the provenances in the solution, so we can
        // later check if the solution contains a provenance that
        // we just solved
        let all_provs_in_sol =
          List.map(
            StringProv.of_prov,
            Solution.TypSolution.all_provs_of(sol),
          );

        // replace the solutions of all existing provenances with
        // we just generated
        let typ_sol_map'' =
          List.fold_left(
            (sol_map_acc, curr_prov) => {
              // a prov is defined to be cyclic if we solved for it, but it
              // appears inside the solution
              let is_prov_cyclic = List.mem(curr_prov, all_provs_in_sol);
              TypSolutionMap.map(
                sol_to_update => {
                  let (updated_sol, replaced_any) =
                    Solution.TypSolution.replace_solution(
                      curr_prov,
                      sol_to_update,
                      sol,
                    );

                  // if the solution is cyclic, then the original solution should
                  // be identical to sol, except with possibly a few more substitutions
                  // of the cyclic solution.
                  // to make the substitution count consistent, replace the old solution
                  // TODO: i think this might have edge cases, when a cyclic solution is nested
                  replaced_any && is_prov_cyclic ? sol : updated_sol;
                },
                sol_map_acc,
              );
            },
            typ_sol_map',
            equiv_provs,
          );

        (constraints', (typ_sol_map'', tpat_sol_map), cyclic_provs');
      },
    )
  };
};

let rec solve_rec =
        (
          constraints: list(Typ.equivalence),
          sol_maps: solution_maps,
          cyclic_provs: list(StringProv.t),
          statics_map: StaticsBase.Map.t,
        )
        : TypSolutionMap.t => {
  switch (extend_sol_map(constraints, sol_maps, cyclic_provs)) {
  | None =>
    // print_endline("No dominant provenances");
    // print_endline(string_of_constramnots(cs));
    // TODO: add cycle replacement
    let (typ_sol_map, tpat_sol_map) = sol_maps;
    let tvars_to_subsitute =
      List.filter_map(
        ((str_prov, tpats)) => {
          let prov = StringProv.to_prov(str_prov);
          switch (prov |> Prov.term_of) {
          | TypeSubstitution(typ) => Some((tpats, typ))
          | _ => None
          };
        },
        TPatSolutionMap.bindings(tpat_sol_map),
      );
    let typ_sol_map' =
      List.fold_left(
        (sol_map_acc, (tvars, typ)) => {
          TypSolutionMap.map(
            sol =>
              solution_substitute_var(
                statics_map,
                Solution.TPatSolution.expand_solution(tvars),
                sol,
                Solution.TypSolution.of_sol_term(typ),
              ),
            sol_map_acc,
          )
        },
        typ_sol_map,
        tvars_to_subsitute,
      );
    TypSolutionMap.replace_cycles(typ_sol_map', cyclic_provs);
  | Some((cs', sm', cyclic_provs')) =>
    solve_rec(cs', sm', cyclic_provs', statics_map)
  };
};

let solve =
    (cs: list(Typ.equivalence), staics_map: StaticsBase.Map.t)
    : TypSolutionMap.t => {
  // print_endline("SOLVING");
  solve_rec(
    cs,
    (TypSolutionMap.empty, TPatSolutionMap.empty),
    [],
    staics_map,
  );
};

let go =
    (cs: list(Typ.equivalence), statics_map: StaticsBase.Map.t)
    : TypSolutionMap.t => {
  solve(
    cs,
    statics_map,
    // let cs = unfold_constramnots(cs);
    // let m = prov_map_of_constramnots(cs);
    // print_endline("go2");
    // print_endline(string_of_int(List.length(StringMap.to_list(m))));
    // print_endline(string_of_prov_map(m));
  );
};
