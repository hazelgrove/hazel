include ProvMap;
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = ProvMap.t('a);

let lookup_prov = (p: Prov.t, m: t('a)): option('a) =>
  ProvMap.find_opt(StringProv.of_prov(p), m);

// relax solution to have no cycles (i.e. replace any un-substituted
// provnances with cycles)
// let replace_cycles =
//   List.fold_left((sol_map, prov) => {
//     map(
//       sol => {
//         let (sol, _) =
//           Solution.replace_solution(prov, sol, Solution.temp_cyclic);
//         sol;
//       },
//       sol_map,
//     )
//   });
