[@deriving sexp]
type map = DHExp.map;

[@deriving sexp]
type t = DHExp.env;

let id_of = ((ei, _)) => ei;
let map_of = ((_, map)) => map;

let to_list = ((_, map)) => map |> VarBstMap.to_list;
let to_environment = ((_, map)) =>
  map
  |> VarBstMap.map(((_, r)) =>
       switch (r) {
       | DHExp.BoxedValue(d)
       | DHExp.Indet(d) => d
       }
     )
  |> VarBstMap.to_list;

/* Equals only needs to check environment ID's (faster than structural equality
 * checking.) */
let equal = (env1, env2) => id_of(env1) == id_of(env2);

let empty = eig => {
  let (ei, eig) = EnvironmentIdGen.next(eig);
  ((ei, VarBstMap.empty), eig);
};

let is_empty = env => env |> map_of |> VarBstMap.is_empty;

let length = env => VarBstMap.length(map_of(env));

let lookup = (env, x) => env |> map_of |> (map => VarBstMap.lookup(map, x));

let contains = (env, x) =>
  env |> map_of |> (map => VarBstMap.contains(map, x));

let extend = (env, xr, eig) => {
  let (ei, eig) = EnvironmentIdGen.next(eig);
  ((ei, VarBstMap.extend(map_of(env), xr)), eig);
};

let union = (env1, env2, eig) => {
  let (ei, eig) = EnvironmentIdGen.next(eig);
  ((ei, VarBstMap.union(map_of(env1), map_of(env2))), eig);
};

let map = (f, env, eig) => {
  let (ei, eig) = EnvironmentIdGen.next(eig);
  ((ei, env |> map_of |> VarBstMap.map(f)), eig);
};

let map_keep_id = (f, env) => (id_of(env), VarBstMap.map(f, map_of(env)));

let filter = (f, env, eig) => {
  let (ei, eig) = EnvironmentIdGen.next(eig);
  ((ei, env |> map_of |> VarBstMap.filter(f)), eig);
};

let placeholder = (EnvironmentId.invalid, VarBstMap.empty);
