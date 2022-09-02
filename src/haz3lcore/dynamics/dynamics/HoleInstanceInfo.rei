[@deriving sexp]
type t = MetaVarMap.t(list((Environment.t, InstancePath.t)));

let empty: t;

let next: (t, MetaVar.t, Environment.t, InstancePath.t) => (int, t);

let update_environment: (t, HoleInstance.t, Environment.t) => t;

let num_instances: (t, MetaVar.t) => int;

let lookup: (t, HoleInstance.t) => option((Environment.t, InstancePath.t));
