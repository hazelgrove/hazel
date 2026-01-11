include (module type of ProvMap);
type data = (Prov.t, list(Prov.t), PossibleTypeSet.t);
type data_elem = UnionFind.elem(data);
type t = ProvMap.t(data_elem);

let of_constramnots: (list(CanonicalConstramnot.t), SolutionMap.t) => t;
let find_dominant_provs: t => (list(Prov.t), bool);
let lookup: (StringProv.t, t) => data_elem;
let lookup_prov: (Prov.t, t) => data_elem;

let solve_prov: (Prov.t, t) => Solution.t;
