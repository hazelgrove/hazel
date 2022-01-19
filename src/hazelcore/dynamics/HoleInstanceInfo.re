open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(list((Environment.t, InstancePath.t)));

let empty: t = (MetaVarMap.empty: t);

let next =
    (hii: t, u: MetaVar.t, sigma: Environment.t, path: InstancePath.t)
    : (int, t) => {
  let v = (sigma, path);
  let envs =
    hii
    |> MetaVarMap.find_opt(u)
    |> Option.fold(~none=[v], ~some=envs => [v, ...envs]);
  let hii = MetaVarMap.add(u, envs, hii);
  (List.length(envs) - 1, hii);
};

let update_environment = (hii: t, inst: HoleInstance.t, sigma: EvalEnv.t): t => {
  let (u, i) = inst;
  let hii =
    hii
    |> MetaVarMap.update(
         u,
         Option.map(instances => {
           let length = List.length(instances);
           ListUtil.update_nth(
             length - i - 1,
             instances,
             (inst_info: (Environment.t, InstancePath.t)) => {
               let (_, path) = inst_info;
               (EvalEnv.environment_of_evalenv(sigma), path);
             },
           );
         }),
       );
  hii;
};

let num_instances = (hii: t, u: MetaVar.t): int =>
  switch (MetaVarMap.find_opt(u, hii)) {
  | Some(envs) => List.length(envs)
  | None => 0
  };

let lookup =
    (hii: t, inst: HoleInstance.t): option((Environment.t, InstancePath.t)) => {
  let (u, i) = inst;
  switch (MetaVarMap.find_opt(u, hii)) {
  | Some(envs) =>
    let length = List.length(envs);
    List.nth_opt(envs, length - i - 1);
  | None => None
  };
};
