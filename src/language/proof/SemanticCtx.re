open Util;

/*
     This module is a combination of a Ctx.t and a ClosureEnvironment.t
     It is used in places where we need to associate both types and values
     with variables, such as in the stepper.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ctx: Ctx.t,
  env: Environment.t(Exp.t),
};

let free_name = (t: t, base: Var.t): Var.t =>
  Var.free_name(
    base,
    List.map((e: Ctx.var_entry) => e.name, Ctx.get_var_entries(t.ctx)),
  );

let of_ctx_and_env: (Ctx.t, Environment.t(Exp.t)) => t =
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
    let env = env |> Environment.extend(_, (name, value));
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
    List.fold_left(
      Environment.extend,
      env,
      List.map(
        v => (v, Exp.fresh(Var(v))),
        pattern |> Pat.bindings |> Binding.variable_names,
      ),
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
let get_env = (t: t): Environment.t(Exp.t) => t.env;

let rec collect_mod_binding_names = (items: list(Mod.t)): list(Var.t) =>
  List.concat_map(
    (item: Mod.t) =>
      switch (item.term) {
      | ModLet(p, e) => Pat.bound_vars(p) @ collect_binding_names(e)
      | ModuleMod(mp, e) =>
        (
          switch (mp.term) {
          | Var(name) => [name]
          | Asc(inner, _) =>
            switch (inner.term) {
            | Var(name) => [name]
            | _ => []
            }
          | _ => []
          }
        )
        @ collect_binding_names(e)
      | ModExp(e) => collect_binding_names(e)
      | ModType(_, _) => []
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      },
    items,
  )
and collect_binding_names = (exp: Exp.t): list(Var.t) => {
  let names = ref([]);
  let add_pat = (p: Pat.t) => names := Pat.bound_vars(p) @ names^;
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, exp) => {
          switch (exp.term) {
          | Let(p, _, _) => add_pat(p)
          | Theorem(p, _, _) => add_pat(p)
          | Forall(p, _) => add_pat(p)
          | FixF(p, _, _) => add_pat(p)
          | Fun(p, _, _, _) => add_pat(p)
          | Match(_, rules) => List.iter(((p, _)) => add_pat(p), rules)
          | Module(items) =>
            names := collect_mod_binding_names(items) @ names^
          | ModuleExp(mp, def, body) =>
            switch (mp.term) {
            | Var(name) => names := [name, ...names^]
            | Asc(inner, _) =>
              switch (inner.term) {
              | Var(name) => names := [name, ...names^]
              | _ => ()
              }
            | _ => ()
            };
            names :=
              collect_binding_names(def)
              @ collect_binding_names(body)
              @ names^;
          | _ => ()
          };
          cont(exp);
        },
      exp,
    );
  names^ |> ListUtil.dedup;
};

let collect_named_fn_names = (exp: Exp.t): list(Var.t) => {
  let names = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (Exp.get_fn_name(e)) {
          | Some(name) =>
            let name =
              if (String.ends_with(~suffix="+", name)) {
                String.sub(name, 0, String.length(name) - 1);
              } else {
                name;
              };
            names := [name, ...names^];
          | None => ()
          };
          cont(e);
        },
      exp,
    );
  names^ |> ListUtil.dedup;
};

let names_from_exp = (exp: Exp.t): list(Var.t) =>
  ListUtil.dedup(collect_binding_names(exp) @ collect_named_fn_names(exp));

let add_binding_names = (ctx: Ctx.t, names: list(Var.t)): Ctx.t =>
  List.fold_left(
    (ctx, name) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | Some(_) => ctx
      | None =>
        Ctx.extend(
          ctx,
          Ctx.VarEntry({
            name,
            id: Id.mk(),
            typ: Typ.temp(Unknown(Internal)),
            custom_statics: None,
          }),
        )
      },
    ctx,
    names,
  );

let add_binding_names_from_exp = (ctx: Ctx.t, exp: Exp.t): Ctx.t =>
  add_binding_names(ctx, names_from_exp(exp));

let extend_with_exp_names = (sctx: t, exp: Exp.t): t => {
  ctx: add_binding_names_from_exp(sctx.ctx, exp),
  env: sctx.env,
};
