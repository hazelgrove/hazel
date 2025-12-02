open Util;

/* This file exists to define data structures that statics uses during the dynamic to static feedback phase.
 * Importantly Statics cannot depend on Dynamics due to recursive dependencies, so we define these types here
 * in a way that both Statics can depend on and can be constructed from Dynamics.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type sample = {exp: Exp.t};

/* A type instantiation records when a type variable is instantiated
 * with a concrete type during type application evaluation */
[@deriving (show({with_path: false}), sexp, yojson)]
type type_instantiation = {
  tpat_id: Id.t,
  type_var: string,
  instantiated_type: Typ.t,
};

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = list(sample);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_inst_entry = list(type_instantiation);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    exp_probes: Id.Map.t(entry),
    type_inst_probes: Id.Map.t(type_inst_entry),
  };

  let empty = {
    exp_probes: Id.Map.empty,
    type_inst_probes: Id.Map.empty,
  };

  let mk = (exp_probes, type_inst_probes): t => {
    exp_probes,
    type_inst_probes,
  };

  let lookup = (id, map) => Id.Map.find_opt(id, map.exp_probes);
  let lookup_type_inst = (id, map) =>
    Id.Map.find_opt(id, map.type_inst_probes);
};

/* Helper to extend Ctx with type instantiations from probes */
let extend_ctx_with_instantiations =
    (ctx: Ctx.t, name, tpat_id: Id.t, insts: list(type_instantiation)): Ctx.t => {
  let new_ty =
    Typ.join_all(ctx, List.map(inst => inst.instantiated_type, insts))
    |> Option.value(~default=Unknown(Internal) |> Typ.temp);
  Ctx.extend_tvar(
    ctx,
    {
      name,
      id: tpat_id,
      kind: Singleton(new_ty),
    },
  );
};
