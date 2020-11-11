open Sexplib.Std;

[@deriving sexp]
type t = NodeInstanceInfo.t(unit);
let next = (hii: t, u: MetaVar.t, sigma: Environment.t, path: InstancePath.t) =>
  NodeInstanceInfo.next(hii, u, sigma, path, ());
let update_environment =
    (hii: t, inst: NodeInstance.t, sigma: Environment.t): t =>
  NodeInstanceInfo.update_environment(hii, inst, sigma, ());
let lookup =
    (hii: t, inst: NodeInstance.t): option((Environment.t, InstancePath.t)) =>
  NodeInstanceInfo.lookup(hii, inst)
  |> Option.map(((env, path, _)) => (env, path));

let empty = NodeInstanceInfo.empty;
