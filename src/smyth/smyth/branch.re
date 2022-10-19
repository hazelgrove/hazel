open Lang;

module Ctor_map = {
  include Map.Make({
    type t = string;
    let compare = String.compare;
  });

  let from_assoc = (xs: list((string, 'a))): t('a) =>
    List.fold_left(
      (dict, (ctor_name, x)) => add(ctor_name, x, dict),
      empty,
      xs,
    );

  let from_assoc_many = (xs: list((string, 'a))): t(list('a)) =>
    List.fold_left(
      (dict, (ctor_name, x)) =>
        union((_, k, v) => Some(k @ v), singleton(ctor_name, [x]), dict),
      empty,
      xs,
    );
};

let filter = (ws: worlds): worlds =>
  List.filter(((_env, ex)) => ex != ExTop, ws);

let distribute =
    (
      delta: hole_ctx,
      sigma: datatype_ctx,
      hf: hole_filling,
      ctor_names: list(string),
      arg_name: string,
      scrutinee: exp,
      (env, ex): world,
    )
    : Nondet.t(((string, world), constraints)) =>
  Nondet.Syntax.
    /* Type soundness ensures that scrutinee evaluates either to a constructor
     * or to an indeterminate result.
     */
    (
      switch (Eval.eval(env, scrutinee)) {
      | Ok((RCtor(ctor_name, arg), [])) =>
        Nondet.pure((
          (ctor_name, (Env.add_res((arg_name, arg), env), ex)),
          Constraints.empty,
        ))

      | Ok((r, [])) =>
        let* ctor_name = Nondet.from_list(ctor_names);
        let+ ks =
          Uneval.uneval(delta, sigma, hf, r, ExCtor(ctor_name, ExTop));
        (
          (
            ctor_name,
            (Env.add_res((arg_name, RCtorInverse(ctor_name, r)), env), ex),
          ),
          ks,
        );

      | _ =>
        Log.warn(
          "Scrutinee did not evaluate to a constructor or indeterminate "
          ++ "result",
        );
        Nondet.none;
      }
    );

let branch =
    (
      max_scrutinee_size,
      delta,
      sigma,
      hf,
      ((gamma, goal_type, goal_dec), worlds),
    ) => {
  open Nondet.Syntax;
  let* _ = Nondet.guard(Option.is_none(goal_dec));
  /* Informativeness Restriction (A) */
  let filtered_worlds = filter(worlds);

  let arg_name = Term_gen.fresh_ident(gamma, Term_gen.match_char);

  let* (data_name, (datatype_params, data_ctors)) = Nondet.from_list(sigma);
  let ctor_names = List.map(fst, data_ctors);

  let ctor_info: Ctor_map.t((string, typ)) = (
    data_ctors
    |> List.map(Pair2.map_snd @@ (typ => (arg_name, typ)))
    |> Ctor_map.from_assoc:
      Ctor_map.t((string, typ))
  );

  let* scrutinee =
    Term_gen.up_to_e(
      sigma,
      max_scrutinee_size,
      (
        gamma,
        TData(data_name, List.map(_ => Type.wildcard, datatype_params)),
        None,
      ),
    );
  let* datatype_args =
    Type.infer(sigma, gamma, scrutinee)
    |> Result2.to_option
    |> Option.map(fst)
    |> Option2.and_then(
         fun
         | TData(_, datatype_args) => Some(datatype_args)

         | _ => {
             Log.warn(
               "Non-datatype scrutinee from term generation: "
               ++ Pretty.exp(scrutinee),
             );
             None;
           },
       )
    |> Nondet.lift_option;
  let top_worlds =
    data_ctors
    |> List.map(Pair2.map_snd @@ (_ => (Env.empty, ExTop)))
    |> Ctor_map.from_assoc_many;

  let* (distributed_worldss, ks) =
    filtered_worlds
    |> List.map(
         distribute(delta, sigma, hf, ctor_names, arg_name, scrutinee),
       )
    |> Nondet.one_of_each
    |> Nondet.map(List.split)
    |> Nondet.map(Pair2.map_fst(Ctor_map.from_assoc_many))
    |> Nondet.filter(((ctor_map, _)) =>
         List.length(data_ctors) == 1 || Ctor_map.cardinal(ctor_map) >= 2
       )
    |> Nondet.map(
         Pair2.map_fst @@ Ctor_map.union((_, _, w) => Some(w), top_worlds),
       )
    |> Nondet.and_then(((dw, kss)) =>
         kss
         |> Constraints.merge
         |> Option2.map(ks => (dw, ks))
         |> Nondet.lift_option
       );
  let+ branches_goals =
    Nondet.lift_option @@
    Ctor_map.fold(
      (ctor_name, distributed_worlds, acc_opt) => {
        open! Option2.Syntax;
        let* acc = acc_opt;
        let+ (arg_name, raw_arg_type) =
          Ctor_map.find_opt(ctor_name, ctor_info);
        let arg_type =
          Type.substitute_many(
            ~bindings=List.combine(datatype_params, datatype_args),
            raw_arg_type,
          );

        let arg_bind_spec =
          scrutinee |> Type.bind_spec(gamma) |> Type.sub_bind_spec;

        let hole_name = Fresh.gen_hole();

        let goal = (
          hole_name,
          (
            (
              Type_ctx.add_type(
                (arg_name, (arg_type, arg_bind_spec)),
                gamma,
              ),
              goal_type,
              None,
            ),
            distributed_worlds,
          ),
        );

        let branch = (ctor_name, (PVar(arg_name), EHole(hole_name)));

        [(branch, goal), ...acc];
      },
      distributed_worldss,
      Some([]),
    );
  (
    branches_goals
    |> List.split
    |> Pair2.map_fst(branches => ECase(scrutinee, branches)),
    ks,
  );
};
