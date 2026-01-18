module Make = (SolValue: Solution.SolutionBase) => {
  include ProvMap;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = ProvMap.t(SolValue.t);

  let lookup_prov = (p: Prov.t, m: t): option('a) =>
    ProvMap.find_opt(StringProv.of_prov(p), m);

  // relax solution to have no cycles (i.e. replace any un-substituted
  // provnances with cycles)
  let replace_cycles =
    List.fold_left((sol_map, prov) => {
      map(
        sol => {
          let (sol, _) =
            SolValue.replace_solution(prov, sol, SolValue.temp_cyclic);
          sol;
        },
        sol_map,
      )
    });
};
