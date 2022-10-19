open Lang;

let filter = (ws: worlds): worlds =>
  List.filter(((_env, ex)) => ex != ExTop, ws);

let refine = (_delta, sigma, ((gamma, goal_type, goal_dec), worlds)) => {
  open Option2.Syntax;
  let* _ = Option2.guard(Option.is_none(goal_dec));
  /* Refine-Fix */
  /* Refine-Tuple */
  /* Refine-Ctor */
  /* Refine-TAbs */
  /* Not really necessary, for now */
  /* Cannot refine a type variable */
  let filtered_worlds = filter(worlds);

  switch (goal_type) {
  | TArr(tau1, tau2) =>
    let hole_name = Fresh.gen_hole();

    let f_name = Term_gen.fresh_ident(gamma, Term_gen.function_char);

    let x_name = Term_gen.fresh_ident(gamma, Term_gen.variable_char);

    let+ refined_worlds =
      filtered_worlds
      |> List.map(((env, io_ex)) =>
           switch (io_ex) {
           | ExInputOutput(v, ex) =>
             Some((
               Env.concat_res(
                 [
                   (x_name, Res.from_value(v)),
                   (
                     f_name,
                     RFix(
                       env,
                       Some(f_name),
                       PatParam(PVar(x_name)),
                       EHole(hole_name),
                     ),
                   ),
                 ],
                 env,
               ),
               ex,
             ))

           | _ => None
           }
         )
      |> Option2.sequence;
    let new_goal = (
      hole_name,
      (
        (
          Type_ctx.concat_type(
            [
              (f_name, (TArr(tau1, tau2), Rec(f_name))),
              (x_name, (tau1, Arg(f_name))),
            ],
            gamma,
          ),
          tau2,
          None,
        ),
        refined_worlds,
      ),
    );

    let exp =
      EFix(Some(f_name), PatParam(PVar(x_name)), EHole(hole_name));

    (exp, [new_goal]);

  | TTuple(taus) =>
    let* refined_worldss =
      filtered_worlds
      |> List.map(((env, tuple_ex)) =>
           switch (tuple_ex) {
           | ExTuple(exs) => Some(List.map(ex => (env, ex), exs))

           | _ => None
           }
         )
      |> Option2.sequence
      |> Option2.map(List2.transpose);
    if (List.length(refined_worldss) != List.length(taus)) {
      None;
    } else {
      let new_goals =
        List.map2(
          (tau, refined_worlds) =>
            (Fresh.gen_hole(), ((gamma, tau, None), refined_worlds)),
          taus,
          refined_worldss,
        );

      let exp =
        ETuple(List.map(((hole_name, _)) => EHole(hole_name), new_goals));

      Some((exp, new_goals));
    };

  | TData(datatype_name, datatype_args) =>
    let* (datatype_params, datatype_ctors) =
      List.assoc_opt(datatype_name, sigma);
    let* (ctor_name, refined_worlds) =
      filtered_worlds
      |> List.map(((env, ctor_ex)) =>
           switch (ctor_ex) {
           | ExCtor(ctor_name, arg_ex) => Some((ctor_name, (env, arg_ex)))

           | _ => None
           }
         )
      |> Option2.sequence
      |> Option2.and_then(
           List.split
           >> Pair2.map_fst(List2.collapse_equal)
           >> Option2.sequence_fst,
         );
    let+ arg_type =
      List.assoc_opt(ctor_name, datatype_ctors)
      |> Option.map(
           Type.substitute_many(
             ~bindings=List.combine(datatype_params, datatype_args),
           ),
         );
    let hole_name = Fresh.gen_hole();

    let new_goal = (hole_name, ((gamma, arg_type, None), refined_worlds));

    let exp = ECtor(ctor_name, datatype_args, EHole(hole_name));

    (exp, [new_goal]);

  | TForall(_, _) => None

  | TVar(_) => None
  };
};
