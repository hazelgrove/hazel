open Util;

/*
     This module is a combination of a Ctx.t and a ClosureEnvironment.t
     It is used in places where we need to associate both types and values
     with variables, such as in the stepper.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ctx: Ctx.t,
  env: ClosureEnvironment.t,
};

let free_name = (t: t, base: Var.t): Var.t =>
  Var.free_name(
    base,
    List.map((e: Ctx.var_entry) => e.name, Ctx.get_var_entries(t.ctx)),
  );

let of_ctx_and_env: (Ctx.t, ClosureEnvironment.t) => t =
  (ctx, env) => {
    ctx,
    env,
  };

let add_entry: (t, Id.t, Var.t, Typ.t, option(Exp.t)) => t =
  ({ctx, env}, id, name, typ, value) => {
    let ctx =
      Ctx.extend(
        ctx,
        Ctx.VarEntry({
          name,
          id,
          typ,
          custom_statics: None,
        }),
      );
    let value = OptUtil.get(() => Exp.fresh(Var(name)), value);
    let env =
      env
      |> ClosureEnvironment.update_env(Environment.extend(_, (name, value)));
    {
      ctx,
      env,
    };
  };

let add_entry_free_name =
    (t: t, name: Var.t, typ: Typ.t, value: option(Exp.t)): (t, Binding.t) => {
  let name = free_name(t, name);
  let id = Id.mk();
  let t = add_entry(t, id, name, typ, value);
  (
    t,
    {
      id,
      name,
    },
  );
};

let add_from_pattern = ({ctx, env}: t, pattern: Pat.t, pat_typ: Typ.t) => {
  let ctx =
    ProofHacks.dhpat_extend_ctx(pattern, pat_typ, ctx)
    |> Option.value(~default=ctx);
  let env =
    ClosureEnvironment.update_env(
      List.fold_left(
        Environment.extend,
        _,
        List.map(
          v => (v, Exp.fresh(Var(v))),
          pattern |> Pat.bindings |> Binding.variable_names,
        ),
      ),
      env,
    );
  {
    ctx,
    env,
  };
};

let add_hypothesis = (t: t, name: Var.t, hyp: Exp.t): (t, Binding.t) => {
  add_entry_free_name(
    t,
    name,
    Typ.fresh(ProofOf(hyp)),
    Some(Exp.fresh(ProofObject(hyp))),
  );
};

let get_ctx = (t: t): Ctx.t => t.ctx;
let get_env = (t: t): ClosureEnvironment.t => t.env;
