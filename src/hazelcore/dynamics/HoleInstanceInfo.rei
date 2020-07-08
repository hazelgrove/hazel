[@deriving sexp]
type t = NodeInstanceInfo.t(unit);

let next: (t, MetaVar.t, Environment.t, InstancePath.t) => (int, t);

let update_environment: (t, NodeInstance.t, Environment.t) => t;

let lookup: (t, NodeInstance.t) => option((Environment.t, InstancePath.t));

let empty: NodeInstanceInfo.t(unit);
