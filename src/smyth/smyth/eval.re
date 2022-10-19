open Lang;

type eval_result = result((res, resumption_assertions), string);

type eval_env_result = result((env, resumption_assertions), string);

/* Note: fuel gets applied at every application. */
module FuelLimited = {
  let rec eval = (fuel, env, exp) => {
    open Result2.Syntax;
    let* _ =
      Result2.guard("Evaluation time exceeded") @@
      Timer.Single.check(Timer.Single.Eval); /* 0 < i <= n = |comps| */
    /* Ctor types are erased at runtime */
    let* _ = Result2.guard("Ran out of fuel", fuel > 0); /* 0 < i <= n = |comps| */
    switch (exp) {
    | EFix(f, x, body) => Ok((RFix(env, f, x, body), []))

    | EVar(x) =>
      switch (List.assoc_opt(x, Env.all_res(env))) {
      | None => Error("Variable not found: " ++ x)

      | Some(r) => Ok((r, []))
      }

    | EHole(name) => Ok((RHole(env, name), []))

    | ETuple(comps) =>
      comps
      |> List.map(eval(fuel, env))
      |> Result2.sequence
      |> Result2.map(evals =>
           evals
           |> List.split
           |> Pair2.map_fst(rs => RTuple(rs))
           |> Pair2.map_snd(List.concat)
         )

    | ECtor(name, _, arg) =>
      let+ (r, ks) = eval(fuel, env, arg);
      (RCtor(name, r), ks);

    | EApp(_, e1, EAExp(e2)) =>
      let* (r1, ks1) = eval(fuel, env, e1);
      let* (r2, ks2) = eval(fuel, env, e2);
      switch (r1) {
      | RFix(f_env, f, PatParam(x), body) =>
        let f_env_extension = Pat.bind_rec_name_res(f, r1);

        let* x_env_extension =
          Pat.bind_res(x, r2)
          |> Option.to_result(~none="Pattern match failed");
        let new_env = Env.concat([x_env_extension, f_env_extension, f_env]);

        Result2.map(
          Pair2.map_snd @@ (ks3 => ks1 @ ks2 @ ks3),
          eval(fuel - 1, new_env, body),
        );

      | _ => Ok((RApp(r1, RARes(r2)), ks1 @ ks2))
      };

    | EApp(_, head, EAType(type_arg)) =>
      let* (r_head, ks1) = eval(fuel, env, head);
      switch (r_head) {
      | RFix(f_env, f, TypeParam(type_param), body) =>
        let f_env_extension = Pat.bind_rec_name_res(f, r_head);

        let param_env_extension =
          Env.add_type((type_param, type_arg), Env.empty);

        let new_env =
          Env.concat([param_env_extension, f_env_extension, f_env]);

        Result2.map(
          Pair2.map_snd @@ (ks2 => ks1 @ ks2),
          eval(fuel, new_env, body),
        );

      | _ => Ok((RApp(r_head, RAType(type_arg)), ks1))
      };

    | EProj(n, i, arg) =>
      if (i <= 0) {
        Error("Non-positive projection index");
      } else if (i > n) {
        Error("Projection index greater than projection length");
      } else {
        let* (r_arg, ks_arg) = eval(fuel, env, arg); /* 0 < i <= n = |comps| */
        switch (r_arg) {
        | RTuple(comps) =>
          if (n != List.length(comps)) {
            Error("Projection length does not match tuple size");
          } else {
            /* 0 < i <= n = |comps| */
            Ok((List.nth(comps, i - 1), ks_arg));
          }

        | _ => Ok((RProj(n, i, r_arg), ks_arg))
        };
      }

    | ECase(scrutinee, branches) =>
      let* (r0, ks0) = eval(fuel, env, scrutinee);
      switch (r0) {
      | RCtor(ctor_name, r_arg) =>
        switch (List.assoc_opt(ctor_name, branches)) {
        | Some((arg_pattern, body)) =>
          let* arg_env_extension =
            Pat.bind_res(arg_pattern, r_arg)
            |> Option.to_result(~none="Pattern match failed");
          Result2.map(
            Pair2.map_snd @@ (ks_body => ks0 @ ks_body),
            eval(fuel, Env.concat([arg_env_extension, env]), body),
          );

        | None =>
          Error(
            "Non-exhaustive pattern match, "
            ++ "could not find constructor '"
            ++ ctor_name
            ++ "'",
          )
        }

      | _ => Ok((RCase(env, r0, branches), ks0))
      };

    | EAssert(e1, e2) =>
      let* (r1, ks1) = eval(fuel, env, e1);
      let* (r2, ks2) = eval(fuel, env, e2);
      switch (Res.consistent(r1, r2)) {
      | Some(ks3) => Ok((RTuple([]), ks1 @ ks2 @ ks3))

      | None => Error("Result consistency failure")
      };

    | ETypeAnnotation(e, _) => eval(fuel, env, e)
    };
  };

  let rec resume = (fuel, hf, res) =>
    Result2.Syntax.(
      switch (res) {
      | RHole(env, name) =>
        switch (Hole_map.find_opt(name, hf)) {
        | Some(binding) =>
          if (binding == EHole(name)) {
            Ok((res, []));
          } else {
            let* (r, ks) = eval(fuel, env, binding);
            let+ (r', ks') = resume(fuel, hf, r);
            (r', ks @ ks');
          }

        | None =>
          let+ (env', ks) = resume_env(fuel, hf, env);
          (RHole(env', name), ks);
        }

      | RFix(env, f, x, body) =>
        let+ (env', ks) = resume_env(fuel, hf, env);
        (RFix(env', f, x, body), ks);

      | RTuple(comps) =>
        comps
        |> List.map(resume(fuel, hf))
        |> Result2.sequence
        |> Result2.map(rs =>
             rs
             |> List.split
             |> Pair2.map_fst(rs => RTuple(rs))
             |> Pair2.map_snd(List.concat)
           )

      | RCtor(name, arg) =>
        let+ (arg', ks) = resume(fuel, hf, arg);
        (RCtor(name, arg'), ks);

      | RApp(r1, RARes(r2)) =>
        let* (r1', ks1) = resume(fuel, hf, r1);
        let* (r2', ks2) = resume(fuel, hf, r2);
        switch (r1') {
        | RFix(f_env, f, PatParam(x), body) =>
          let f_env_extension = Pat.bind_rec_name_res(f, r1');

          let* x_env_extension =
            Pat.bind_res(x, r2')
            |> Option.to_result(~none="Pattern match failed");
          let new_env = Env.concat([x_env_extension, f_env_extension, f_env]);

          let* (r, ks) = eval(fuel - 1, new_env, body);
          let+ (r', ks') = resume(fuel - 1, hf, r);
          (r', ks1 @ ks2 @ ks @ ks');

        | _ => Ok((RApp(r1', RARes(r2')), ks1 @ ks2))
        };

      | RApp(r_head, RAType(type_arg)) =>
        let* (r_head', ks_head) = resume(fuel, hf, r_head);
        switch (r_head') {
        | RFix(f_env, f, TypeParam(type_param), body) =>
          let f_env_extension = Pat.bind_rec_name_res(f, r_head');

          let param_env_extension =
            Env.add_type((type_param, type_arg), Env.empty);

          let new_env =
            Env.concat([param_env_extension, f_env_extension, f_env]);

          let* (r, ks) = eval(fuel - 1, new_env, body);
          let+ (r', ks') = resume(fuel - 1, hf, r);
          (r', ks_head @ ks @ ks');

        | _ => Ok((RApp(r_head', RAType(type_arg)), ks_head))
        };

      | RProj(n, i, arg) =>
        if (i <= 0) {
          Error("Non-positive projection index");
        } else if (i > n) {
          Error("Projection index greater than projection length");
        } else {
          let* (arg', ks_arg) = resume(fuel, hf, arg); /* 0 < i <= n = |comps| */
          switch (arg') {
          | RTuple(comps) =>
            if (n != List.length(comps)) {
              Error("Projection length does not match tuple size");
            } else {
              /* 0 < i <= n = |comps| */
              Ok((List.nth(comps, i - 1), ks_arg));
            }

          | _ => Ok((RProj(n, i, arg'), ks_arg))
          };
        }

      | RCase(env, scrutinee, branches) =>
        let* (r0, ks0) = resume(fuel, hf, scrutinee);
        switch (r0) {
        | RCtor(ctor_name, r_arg) =>
          switch (List.assoc_opt(ctor_name, branches)) {
          | Some((arg_pattern, body)) =>
            Result2.map(
              Pair2.map_snd @@ (ks_body => ks0 @ ks_body),
              resume(fuel, hf) @@
              RApp(
                RFix(env, None, PatParam(arg_pattern), body),
                RARes(r_arg),
              ),
            )

          | None =>
            Error(
              "Non-exhaustive pattern match, "
              ++ "could not find constructor '"
              ++ ctor_name
              ++ "'",
            )
          }

        | _ =>
          let+ (env', ks_env) = resume_env(fuel, hf, env);
          (RCase(env', r0, branches), ks0 @ ks_env);
        };

      | RCtorInverse(name, arg) =>
        let+ (arg', ks) = resume(fuel, hf, arg);
        (RCtorInverse(name, arg'), ks);
      }
    )

  and resume_env = (fuel, hf, env): eval_env_result => {
    open Result2.Syntax;
    let+ (new_res_env, ks) =
      env
      |> Env.all_res
      |> List.map(binding =>
           binding
           |> Pair2.map_snd(resume(fuel, hf))
           |> Pair2.lift_snd_result
         )
      |> Result2.sequence
      |> Result2.map(binding =>
           binding
           |> List.map(((x, (r, ks))) => ((x, r), ks))
           |> List.split
           |> Pair2.map_snd(List.concat)
         );
    ((new_res_env, Env.all_type(env)), ks);
  };
};

let eval = (env, exp) => {
  Timer.Single.start(Timer.Single.Eval);
  FuelLimited.eval(Params.initial_fuel^, env, exp);
};

let resume = (hf, res) => {
  Timer.Single.start(Timer.Single.Eval);
  FuelLimited.resume(Params.initial_fuel^, hf, res);
};
