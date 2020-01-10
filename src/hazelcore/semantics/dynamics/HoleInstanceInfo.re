open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(list((Environment.t, InstancePath.t)));

let empty: t = (MetaVarMap.empty: t);

let next =
    (hii: t, u: MetaVar.t, sigma: Environment.t, path: InstancePath.t)
    : (int, t) => {
  let (envs, hii) =
    MetaVarMap.insert_or_map(
      hii,
      u,
      _ => [(sigma, path)],
      envs => [(sigma, path), ...envs],
    );
  (List.length(envs) - 1, hii);
};

let update_environment =
    (hii: t, inst: HoleInstance.t, sigma: Environment.t): t => {
  let (u, i) = inst;
  let (_, hii) =
    MetaVarMap.update_with(
      instances => {
        let length = List.length(instances);
        ListUtil.update_nth(
          length - i - 1,
          instances,
          (inst_info: (Environment.t, InstancePath.t)) => {
            let (_, path) = inst_info;
            (sigma, path);
          },
        );
      },
      u,
      hii,
      [],
    );
  hii;
};

let num_instances = (hii: t, u: MetaVar.t): int =>
  switch (MetaVarMap.lookup(hii, u)) {
  | Some(envs) => List.length(envs)
  | None => 0
  };

let default_instance = (hii: t, u: MetaVar.t): option((MetaVar.t, int)) =>
  switch (MetaVarMap.lookup(hii, u)) {
  | Some(envs) =>
    switch (envs) {
    | [] => None
    | [_, ..._] => Some((u, 0))
    }
  | None => None
  };

let lookup =
    (hii: t, inst: HoleInstance.t): option((Environment.t, InstancePath.t)) => {
  let (u, i) = inst;
  switch (MetaVarMap.lookup(hii, u)) {
  | Some(envs) =>
    let length = List.length(envs);
    List.nth_opt(envs, length - i - 1);
  | None => None
  };
};
