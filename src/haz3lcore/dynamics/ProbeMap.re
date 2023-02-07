open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type instance = {
  env: ClosureEnvironment.t, //what is ClosureEnvironment vs Environment?
  res: EvaluatorResult.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = list(instance);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(entry);
let empty: t = Id.Map.empty;

let get: (Id.t, t) => option(entry) = Id.Map.find_opt;

let to_list: t => list((Id.t, entry)) = Id.Map.bindings;

let extend = (id: Id.t, instance: instance, t: t): t => {
  let instances =
    switch (get(id, t)) {
    | Some(instances) => instances
    | None => []
    };
  Id.Map.add(id, [instance, ...instances], t);
};

let abbreviate_envs = (probemap: t): list((Id.t, entry)) => {
  let bindings = Id.Map.bindings(probemap);
  // first, assert that all instances have equal length:
  assert(
    Util.ListUtil.all_eq(
      List.map(((_, is)) => List.length(is), bindings),
    ),
  );
  switch (bindings) {
  | [] => []
  | _
      when
        List.map(((_, e)) => List.hd(e), bindings) |> Util.ListUtil.all_eq =>
    List.map(((id, entry)) => (id, List.tl(entry)), bindings)
  | _ => bindings
  };
};
