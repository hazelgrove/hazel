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

[@deriving (show({with_path: false}), sexp, yojson)]
type abbr = list((Id.t, entry));

let abbreviate_envs = (probemap: t): abbr => {
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

let g: DHExp.t => DHExp.t =
  fun
  | Closure(_) => BoundVar("fun")
  | d => DHExp.strip_casts(d);

[@deriving (show({with_path: false}), sexp, yojson)]
type nu_map = list((string, DHExp.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type nu_instance = {
  res: DHExp.t,
  env: nu_map,
};

let fuckin_n_truckin: instance => nu_instance =
  ({env, res}) => {
    env:
      env
      |> ClosureEnvironment.to_list
      |> List.filter_map(((v, d)) =>
           switch (d) {
           | _ when v == "pi" => None
           | DHExp.Closure(_) => None
           | _ => Some((v, g(d)))
           }
         ),
    res:
      switch (res) {
      | BoxedValue(res) => g(res)
      | Indet(res) => g(res)
      },
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type nuer_map = Id.Map.t(list(nu_instance));

let filtershit = (probemap: t): nuer_map =>
  Id.Map.map(instances => List.map(fuckin_n_truckin, instances), probemap);

/*
 TODO:
 for probe id, get measurements
 and get get ctx
 foreach var in env,
   look up var in ctx; get binding id
   get measurements


  */

//let map = Measured.of_segment(unselected)

type final_env_entry = {
  v: DHExp.t,
  id: Id.t, // id of the binding
  measure: Measured.measurement //Measured.find_by_id(id, map):option(measurement)
};
