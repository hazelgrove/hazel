open Lang;
open Nondet.Syntax;

let minimal_uneval = ref(true);

/* Note: fuel gets applied at case expressions in non-minimal mode. */
module FuelLimited = {
  let rec check = (fuel, delta, sigma, hf, exp, worlds) => {
    let check_one = ((env, ex)) =>
      switch (Eval.eval(env, exp)) {
      | Ok((r, [])) =>
        switch (Eval.resume(hf, r)) {
        | Ok((r', [])) => uneval(fuel, delta, sigma, hf, r', ex)

        | _ => Nondet.none
        }

      | _ => Nondet.none
      };

    worlds
    |> List.map(check_one)
    |> Nondet.one_of_each
    |> Nondet.curb_overflow(Params.uneval_limiter^)
    |> Nondet.map(Constraints.merge)
    |> Nondet.collapse_option;
  }

  and uneval = (fuel, delta, sigma, hf, res, ex) => {
    let rec blocking_hole = (r: res): option(hole_name) =>
      switch (r) {
      /* Determinate results */

      | RFix(_, _, _, _)
      | RTuple(_)
      | RCtor(_, _) => None

      /* Indeterminate results */

      | RHole(_, hole_name) => Some(hole_name)

      | RApp(head, _) => blocking_hole(head)

      | RProj(_, _, arg) => blocking_hole(arg)

      | RCase(_, scrutinee, _) => blocking_hole(scrutinee)

      | RCtorInverse(_, arg) => blocking_hole(arg)
      };

    let guesses =
        (delta: hole_ctx, sigma: datatype_ctx, res: res)
        : Nondet.t(hole_filling) => {
      let* hole_name = Nondet.lift_option @@ blocking_hole(res);
      let* (gamma, tau, dec, _) =
        Nondet.lift_option @@ List.assoc_opt(hole_name, delta);
      Nondet.map(
        Hole_map.singleton(hole_name),
        Term_gen.up_to_e(sigma, 1, (gamma, tau, dec)),
      );
    };

    let* _ = Nondet.guard(fuel > 0);
    switch (res, ex) {
    | (_, ExTop) => Nondet.pure(Constraints.empty)

    | (RTuple(comps1), ExTuple(comps2)) =>
      if (List.length(comps1) == List.length(comps2)) {
        List.map2(uneval(fuel, delta, sigma, hf), comps1, comps2)
        |> Nondet.one_of_each
        |> Nondet.map(Constraints.merge)
        |> Nondet.collapse_option;
      } else {
        Nondet.none;
      }

    | (RCtor(name1, arg1), ExCtor(name2, arg2)) =>
      if (String.equal(name1, name2)) {
        uneval(fuel, delta, sigma, hf, arg1, arg2);
      } else {
        Nondet.none;
      }

    | (RHole(env, hole_name), _) =>
      Nondet.pure @@ Constraints.unsolved_singleton(hole_name, [(env, ex)])

    | (RFix(env, f, PatParam(x), body), ExInputOutput(input, output)) =>
      let fix_env_extension = Pat.bind_rec_name_res(f, res);

      let* x_env_extension =
        Pat.bind_res(x, Res.from_value(input)) |> Nondet.lift_option;
      check(
        fuel,
        delta,
        sigma,
        hf,
        body,
        [(Env.concat([x_env_extension, fix_env_extension, env]), output)],
      );

    | (RApp(r1, RARes(r2)), _) =>
      switch (Res.to_value(r2)) {
      | Some(v2) =>
        uneval(fuel, delta, sigma, hf, r1) @@ ExInputOutput(v2, ex)

      | None => Nondet.none
      }

    | (RProj(n, i, arg), _) =>
      uneval(fuel, delta, sigma, hf, arg) @@
      ExTuple(
        List2.repeat(i - 1, ExTop) @ [ex] @ List2.repeat(n - i, ExTop),
      )

    | (RCase(env, scrutinee, branches), _) =>
      if (minimal_uneval^) {
        let* hf_guesses = guesses(delta, sigma, scrutinee);
        let* hf' =
          Nondet.lift_option @@ Constraints.merge_solved([hf, hf_guesses]);
        let ks_guesses = (hf_guesses, Hole_map.empty);

        let* (r_scrutinee, rcs_scrutinee) =
          Nondet.lift_result @@ Eval.resume(hf', scrutinee);
        let* ks_scrutinee =
          simplify_assertions(fuel, delta, sigma, rcs_scrutinee);
        let* ks_branch =
          switch (r_scrutinee) {
          | RCtor(ctor_name, r_arg) =>
            switch (List.assoc_opt(ctor_name, branches)) {
            | Some((arg_pattern, body)) =>
              let* arg_env_extension =
                Pat.bind_res(arg_pattern, r_arg) |> Nondet.lift_option;
              check(fuel, delta, sigma, hf', body) @@
              [(Env.concat([arg_env_extension, env]), ex)];

            | None => Nondet.none
            }

          | _ => Nondet.none
          };
        Nondet.lift_option @@
        Constraints.merge([ks_guesses, ks_scrutinee, ks_branch]);
      } else {
        let try_branch = ((ctor_name, (arg_pattern, body))) => {
          let* k1 =
            uneval(
              fuel - 1,
              delta,
              sigma,
              hf,
              scrutinee,
              ExCtor(ctor_name, ExTop),
            );
          let* arg_env_extension =
            Pat.bind_res(arg_pattern, RCtorInverse(ctor_name, scrutinee))
            |> Nondet.lift_option;
          let* k2 =
            check(fuel - 1, delta, sigma, hf, body) @@
            [(Env.concat([arg_env_extension, env]), ex)];
          Nondet.lift_option @@ Constraints.merge([k1, k2]);
        };

        branches |> Nondet.from_list |> Nondet.and_then(try_branch);
      }

    | (RCtorInverse(name, arg), _) =>
      uneval(fuel, delta, sigma, hf, arg, ExCtor(name, ex))

    | _ =>
      Log.warn("Mistyped uneval");
      Nondet.none;
    };
  }

  and simplify_assertions = (fuel, delta, sigma, rcs) => {
    let simplify_one = ((res, value)) =>
      if (Res.final(res)) {
        uneval(
          fuel,
          delta,
          sigma,
          Hole_map.empty,
          res,
          Example.from_value(value),
        );
      } else {
        Nondet.none;
      };

    rcs
    |> List.map(simplify_one)
    |> Nondet.one_of_each
    |> Nondet.map(Constraints.merge)
    |> Nondet.collapse_option;
  };
};

let check = (delta, sigma, hf, exp, worlds) =>
  FuelLimited.check(
    Params.uneval_case_budget^,
    delta,
    sigma,
    hf,
    exp,
    worlds,
  );

let uneval = (delta, sigma, hf, res, ex) =>
  FuelLimited.uneval(Params.uneval_case_budget^, delta, sigma, hf, res, ex);

let simplify_assertions = (delta, sigma, rcs) =>
  FuelLimited.simplify_assertions(
    Params.uneval_case_budget^,
    delta,
    sigma,
    rcs,
  );
