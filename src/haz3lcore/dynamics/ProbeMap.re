open Sexplib.Std;
open Util.OptUtil.Syntax;

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

[@deriving (show({with_path: false}), sexp, yojson)]
type init_env_entry = (string, DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type final_env_entry = {
  v: DHExp.t,
  binding_id: Id.t, // id of the binding
  measurement: Measured.measurement // location of binding
};

[@deriving (show({with_path: false}), sexp, yojson)]
type dhexp_env = list((string, final_env_entry));

[@deriving (show({with_path: false}), sexp, yojson)]
type processed_instance = {
  res: DHExp.t,
  env: dhexp_env,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type processed_instances = list(processed_instance);

[@deriving (show({with_path: false}), sexp, yojson)]
type processed_map = Id.Map.t(processed_instances);

let process_d: DHExp.t => DHExp.t =
  fun
  | Closure(_) => BoundVar("fun")
  | DHExp.FixF(_) => BoundVar("fix")
  | d => DHExp.strip_casts(d);

let add_derived =
    (indicated_info: Info.t, measured: Measured.t, (name, d): init_env_entry)
    : option(final_env_entry) => {
  let ctx = Info.ctx_of(indicated_info);
  let* {id, typ: _, _} = Ctx.lookup_var(ctx, name);
  let+ measurement = Measured.find_by_id(id, measured);
  {v: process_d(d), binding_id: id, measurement};
};

let process_entry = (index_info, measured, (name, d)) =>
  switch (d) {
  /* Note builtins are implictly filtered
     because their id lookup fails */
  //| _ when name == "pi" => None
  | DHExp.Closure(_) => None
  | DHExp.FixF(_) => None
  | _ =>
    let+ b = add_derived(index_info, measured, (name, d));
    (name, b);
  };

let process_res = (res: EvaluatorResult.t) =>
  switch (res) {
  | BoxedValue(res) => process_d(res)
  | Indet(res) => process_d(res)
  //| Indet(_res) => BoundVar("indet")
  };

let fuckin_n_truckin =
    (index_info, measured, {env, res}: instance): processed_instance => {
  env:
    env
    |> ClosureEnvironment.to_list
    |> List.filter_map(process_entry(index_info, measured)),
  res: process_res(res),
};

let process = (index_info, measured, probemap: t): processed_map =>
  Id.Map.map(
    instances => List.map(fuckin_n_truckin(index_info, measured), instances),
    probemap,
  );
