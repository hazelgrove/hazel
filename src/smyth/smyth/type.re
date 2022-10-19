open Lang;

/** Helpers */;

let rec equal = (tau1, tau2) =>
  switch (tau1, tau2) {
  | (TArr(tau11, tau12), TArr(tau21, tau22)) =>
    equal(tau11, tau21) && equal(tau12, tau22)

  | (TTuple(taus1), TTuple(taus2)) =>
    List.length(taus1) == List.length(taus2)
    && List.for_all2(equal, taus1, taus2)

  | (TData(d1, params1), TData(d2, params2)) =>
    String.equal(d1, d2)
    && List.length(params1) == List.length(params2)
    && List.for_all2(equal, params1, params2)

  | (TForall(a1, bound_type1), TForall(a2, bound_type2)) =>
    String.equal(a1, a2) && equal(bound_type1, bound_type2)

  | (TVar(x1), TVar(x2)) => String.equal(x1, x2)

  | _ => false
  };

let wildcard = TVar("*");

let rec matches = (tau1, tau2) =>
  if (equal(tau1, wildcard)) {
    true;
  } else if (equal(tau2, wildcard)) {
    true;
  } else {
    switch (tau1, tau2) {
    | (TArr(tau11, tau12), TArr(tau21, tau22)) =>
      matches(tau11, tau21) && matches(tau12, tau22)

    | (TTuple(taus1), TTuple(taus2)) =>
      List.length(taus1) == List.length(taus2)
      && List.for_all2(matches, taus1, taus2)

    | (TData(d1, params1), TData(d2, params2)) =>
      String.equal(d1, d2)
      && List.length(params1) == List.length(params2)
      && List.for_all2(matches, params1, params2)

    | (TForall(a1, bound_type1), TForall(a2, bound_type2)) =>
      String.equal(a1, a2) && matches(bound_type1, bound_type2)

    | (TVar(x1), TVar(x2)) => String.equal(x1, x2)

    | _ => false
    };
  };

let is_base = tau =>
  switch (tau) {
  | TArr(_) => false

  | TTuple(_) => false

  | TData(_) => true

  | TForall(_, _) => false

  | TVar(_) => true
  };

let rec domain_of_codomain = (~codomain, tau) =>
  switch (tau) {
  | TArr(tau1, tau2) =>
    if (equal(codomain, tau2)) {
      Some(tau1);
    } else {
      domain_of_codomain(~codomain, tau2);
    }

  | TForall(_, bound_type) => domain_of_codomain(~codomain, bound_type)

  | _ => None
  };

let sub_bind_spec = bind_spec =>
  switch (bind_spec) {
  | NoSpec
  | Rec(_) => NoSpec

  | Arg(name)
  | Dec(name) => Dec(name)
  };

let rec bind_spec = (gamma, exp) =>
  switch (exp) {
  | EProj(_, _, arg) => sub_bind_spec(bind_spec(gamma, arg))

  | EVar(x) =>
    List.assoc_opt(x, Type_ctx.all_type(gamma))
    |> Option2.map(snd)
    |> Option2.with_default(NoSpec)

  | EApp(_, head, EAType(_)) => bind_spec(gamma, head)

  | _ => NoSpec
  };

let structurally_decreasing_bind_spec = (~head_spec, ~arg_spec) =>
  switch (head_spec, arg_spec) {
  | (Rec(rec_name), Dec(dec_name)) => String.equal(rec_name, dec_name)

  | (Rec(_), _) => false

  | _ => true
  };

let structurally_decreasing = (gamma, ~head, ~arg) =>
  structurally_decreasing_bind_spec(
    ~head_spec=bind_spec(gamma, head),
    ~arg_spec=bind_spec(gamma, arg),
  );

let matches_dec = (annot, bind_spec) =>
  switch (annot) {
  | Some(f) =>
    structurally_decreasing_bind_spec(~head_spec=Rec(f), ~arg_spec=bind_spec)

  | None => true
  };

let peel_forall: typ => (list(string), typ) = (
  {
    let rec helper = (acc, tau) =>
      switch (tau) {
      | TForall(a, bound_type) => helper([a, ...acc], bound_type)

      | _ => (List.rev(acc), tau)
      };

    helper([]);
  }:
    typ => (list(string), typ)
);

/* Substitution */

let rec substitute: (~before: string, ~after: typ, typ) => typ = (
  (~before, ~after, tau) =>
    switch (tau) {
    | TArr(arg, ret) =>
      TArr(
        substitute(~before, ~after, arg),
        substitute(~before, ~after, ret),
      )

    | TTuple(components) =>
      TTuple(List.map(substitute(~before, ~after), components))

    | TData(name, args) =>
      TData(name, List.map(substitute(~before, ~after), args))

    | TForall(a, bound_type) =>
      if (String.equal(before, a)) {
        tau;
      } else {
        TForall(a, substitute(~before, ~after, bound_type));
      }

    | TVar(x) =>
      if (String.equal(before, x)) {
        after;
      } else {
        tau;
      }
    }:
    (~before: string, ~after: typ, typ) => typ
);

let substitute_many: (~bindings: list((string, typ)), typ) => typ = (
  (~bindings, tau) =>
    List.fold_left(
      (acc, (before, after)) => substitute(~before, ~after, acc),
      tau,
      bindings,
    ):
    (~bindings: list((string, typ)), typ) => typ
);

/** Type checking */;

type error =
  | VarNotFound(string)
  | CtorNotFound(string)
  | PatternMatchFailure(typ, pat)
  | WrongNumberOfTypeArguments(int, int)
  | GotFunctionButExpected(typ)
  | GotTupleButExpected(typ)
  | GotTypeAbstractionButExpected(typ)
  | GotButExpected(typ, typ)
  | BranchMismatch(string, string)
  | CannotInferFunctionType
  | CannotInferCaseType
  | CannotInferHoleType
  | ExpectedArrowButGot(typ)
  | ExpectedTupleButGot(typ)
  | ExpectedForallButGot(typ)
  | ExpectedDatatypeButGot(typ)
  | TupleLengthMismatch(typ)
  | ProjectionLengthMismatch(typ)
  | ProjectionOutOfBounds(int, int)
  | TypeAbstractionParameterNameMismatch(string, string)
  | AssertionTypeMismatch(typ, typ);

/* returns: type params * arg type * datatype name */
let ctor_info:
  (exp, datatype_ctx, string) =>
  result((list(string), typ, string), (exp, error)) = (
  (exp_context, sigma, ctor_name) =>
    Option.to_result(
      ~none=(exp_context, CtorNotFound(ctor_name)),
      List2.find_map(
        ((type_name, (type_params, ctors))) =>
          List2.find_map(
            ((ctor_name', arg_type)) =>
              if (String.equal(ctor_name, ctor_name')) {
                Some((type_params, arg_type, type_name));
              } else {
                None;
              },
            ctors,
          ),
        sigma,
      ),
    ):
    (exp, datatype_ctx, string) =>
    result((list(string), typ, string), (exp, error))
);

type state = {
  function_decrease_requirement: option(string), /* TODO: currently unused */
  match_depth: int,
  arg_of: option(string),
};

let rec check':
  (state, datatype_ctx, type_ctx, exp, typ) => result(hole_ctx, (exp, error)) = (
  (state, sigma, gamma, exp, tau) => {
    open Result2.Syntax;
    let state =
      switch (exp) {
      | EFix(_, _, _) => state

      | _ => {...state, arg_of: None}
      };

    switch (exp) {
    | EFix(func_name_opt, PatParam(param_pat), body) =>
      switch (tau) {
      | TArr(arg_type, return_type) =>
        let func_name_gamma = Pat.bind_rec_name_typ(func_name_opt, tau);

        let arg_bind_spec =
          func_name_opt
          |> Option.map(name => Arg(name))
          |> Option2.with_default(
               switch (state.arg_of) {
               | Some(name) => Arg(name)

               | None => NoSpec
               },
             );

        let* param_gamma =
          Pat.bind_typ(arg_bind_spec, param_pat, arg_type)
          |> Option.to_result(
               ~none=(exp, PatternMatchFailure(arg_type, param_pat)),
             );
        check'(
          {...state, arg_of: None},
          sigma,
          Type_ctx.concat([param_gamma, func_name_gamma, gamma]),
          body,
          return_type,
        );

      | _ => Error((exp, GotFunctionButExpected(tau)))
      }

    | EFix(func_name_opt, TypeParam(a), body) =>
      switch (tau) {
      | TForall(a', bound_type) =>
        if (!String.equal(a, a')) {
          Error((exp, TypeAbstractionParameterNameMismatch(a, a')));
        } else {
          let func_name_gamma = Pat.bind_rec_name_typ(func_name_opt, tau);

          let param_gamma = Type_ctx.add_poly(a, Type_ctx.empty);

          check'(
            {
              ...state,
              arg_of:
                switch (state.arg_of) {
                | Some(name) => Some(name)

                | None => func_name_opt
                },
            },
            sigma,
            Type_ctx.concat([param_gamma, func_name_gamma, gamma]),
            body,
            bound_type,
          );
        }

      | _ => Error((exp, GotTypeAbstractionButExpected(tau)))
      }

    | ETuple(exps) =>
      switch (tau) {
      | TTuple(taus) =>
        if (Int.equal(List.length(exps), List.length(taus))) {
          List.map2(check'(state, sigma, gamma), exps, taus)
          |> Result2.sequence
          |> Result.map(List.concat);
        } else {
          Error((exp, TupleLengthMismatch(tau)));
        }

      | _ => Error((exp, GotTupleButExpected(tau)))
      }

    | ECase(scrutinee, branches) =>
      let* (scrutinee_type, scrutinee_delta) =
        infer'(state, sigma, gamma, scrutinee);
      switch (scrutinee_type) {
      | TData(scrutinee_data_name, scrutinee_data_args) =>
        let scrutinee_data_args_len = List.length(scrutinee_data_args);

        let dec_bind_spec = scrutinee |> bind_spec(gamma) |> sub_bind_spec;

        let+ branch_deltas =
          branches
          |> List.map(((ctor_name, (param_pat, body))) => {
               let* (type_params, arg_type, data_name) =
                 ctor_info(exp, sigma, ctor_name);
               if (String.equal(scrutinee_data_name, data_name)) {
                 let type_params_len = List.length(type_params);

                 if (Int.equal(type_params_len, scrutinee_data_args_len)) {
                   let substituted_arg_type =
                     substitute_many(
                       ~bindings=
                         List.combine(type_params, scrutinee_data_args),
                       arg_type,
                     );

                   let* param_gamma =
                     substituted_arg_type
                     |> Pat.bind_typ(dec_bind_spec, param_pat)
                     |> Option.to_result(
                          ~none=(
                            exp,
                            PatternMatchFailure(arg_type, param_pat),
                          ),
                        );
                   check'(
                     {...state, match_depth: state.match_depth + 1},
                     sigma,
                     Type_ctx.concat([param_gamma, gamma]),
                     body,
                     tau,
                   );
                 } else {
                   Error((
                     exp,
                     WrongNumberOfTypeArguments(
                       type_params_len,
                       scrutinee_data_args_len,
                     ),
                   ));
                 };
               } else {
                 Error((
                   exp,
                   BranchMismatch(ctor_name, scrutinee_data_name),
                 ));
               };
             })
          |> Result2.sequence
          |> Result.map(List.concat);
        scrutinee_delta @ branch_deltas;

      | _ => Error((exp, ExpectedDatatypeButGot(scrutinee_type)))
      };

    | EHole(name) =>
      Ok([
        (
          name,
          (
            gamma,
            tau,
            state.function_decrease_requirement,
            state.match_depth,
          ),
        ),
      ])

    | EApp(_, head, EAExp(ETypeAnnotation(arg, arg_type))) =>
      let* arg_delta = check'(state, sigma, gamma, arg, arg_type);
      let+ head_delta =
        check'(state, sigma, gamma, head, TArr(arg_type, tau));
      head_delta @ arg_delta;

    | EApp(_, _, _)
    | EVar(_)
    | EProj(_, _, _)
    | ECtor(_, _, _)
    | EAssert(_, _)
    | ETypeAnnotation(_, _) =>
      let* (tau', delta) = infer'(state, sigma, gamma, exp);
      if (equal(tau, tau')) {
        Ok(delta);
      } else {
        Error((exp, GotButExpected(tau', tau)));
      };
    };
  }:
    (state, datatype_ctx, type_ctx, exp, typ) =>
    result(hole_ctx, (exp, error))
  /* Nonstandard, but useful for let-bindings */
)

and infer':
  (state, datatype_ctx, type_ctx, exp) =>
  result((typ, hole_ctx), (exp, error)) = (
  (state, sigma, gamma, exp) =>
    Result2.Syntax.(
      switch (exp) {
      | EFix(_, _, _) => Error((exp, CannotInferFunctionType))

      | EApp(_, head, EAExp(arg)) =>
        let* (head_type, head_delta) = infer'(state, sigma, gamma, head);
        switch (head_type) {
        | TArr(arg_type, return_type) =>
          let+ arg_delta = check'(state, sigma, gamma, arg, arg_type);
          (return_type, head_delta @ arg_delta);

        | _ => Error((exp, ExpectedArrowButGot(head_type)))
        };

      | EApp(_, head, EAType(type_arg)) =>
        let* (head_type, head_delta) = infer'(state, sigma, gamma, head);
        switch (head_type) {
        | TForall(a, bound_type) =>
          Ok((
            substitute(~before=a, ~after=type_arg, bound_type),
            head_delta,
          ))

        | _ => Error((exp, ExpectedForallButGot(head_type)))
        };

      | EVar(name) =>
        switch (List.assoc_opt(name, Type_ctx.all_type(gamma))) {
        | Some((tau', _)) => Ok((tau', []))

        | None => Error((exp, VarNotFound(name)))
        }

      | ETuple(exps) =>
        exps
        |> List.map(infer'(state, sigma, gamma))
        |> Result2.sequence
        |> Result.map(List.split)
        |> Result.map(((taus, deltas)) =>
             (TTuple(taus), List.concat(deltas))
           )

      | EProj(n, i, arg) =>
        let* (arg_type, arg_delta) = infer'(state, sigma, gamma, arg);
        switch (arg_type) {
        | TTuple(components) =>
          if (Int.equal(n, List.length(components))) {
            if (i <= n) {
              Ok((List.nth(components, i - 1), arg_delta));
            } else {
              Error((exp, ProjectionOutOfBounds(n, i)));
            };
          } else {
            Error((exp, ProjectionLengthMismatch(arg_type)));
          }

        | _ => Error((exp, ExpectedTupleButGot(arg_type)))
        };

      | ECtor(ctor_name, type_args, arg) =>
        let* (type_params, arg_type, data_name) =
          ctor_info(exp, sigma, ctor_name);
        let args_len = List.length(type_args);

        let params_len = List.length(type_params);

        if (Int.equal(args_len, params_len)) {
          let+ arg_delta =
            check'(
              state,
              sigma,
              gamma,
              arg,
              substitute_many(
                ~bindings=List.combine(type_params, type_args),
                arg_type,
              ),
            );
          (TData(data_name, type_args), arg_delta);
        } else {
          Error((exp, WrongNumberOfTypeArguments(args_len, params_len)));
        };

      | ECase(_, _) => Error((exp, CannotInferCaseType))

      | EHole(_) => Error((exp, CannotInferHoleType))

      | EAssert(left, right) =>
        let* (left_type, left_delta) = infer'(state, sigma, gamma, left);
        let* (right_type, right_delta) = infer'(state, sigma, gamma, right);
        if (equal(left_type, right_type)) {
          Ok((TTuple([]), left_delta @ right_delta));
        } else {
          Error((exp, AssertionTypeMismatch(left_type, right_type)));
        };

      | ETypeAnnotation(exp', tau') =>
        let+ delta = check'(state, sigma, gamma, exp', tau');
        (tau', delta);
      }
    ):
    (state, datatype_ctx, type_ctx, exp) =>
    result((typ, hole_ctx), (exp, error))
);

let check =
  check'({function_decrease_requirement: None, match_depth: 0, arg_of: None});

let infer =
  infer'({function_decrease_requirement: None, match_depth: 0, arg_of: None});
