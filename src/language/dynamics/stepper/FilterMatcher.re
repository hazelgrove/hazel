let matches_exp = (~denv, d, ~fenv, f) =>
  Equality.equality(
    ~type_alpha=true,
    ~exp_alpha=true,
    ~ignore_wrappers=true,
    ~ignore_casts=true,
    ~ignore_constructor_types=true,
    ~ignore_unknown_provenance=true,
    ~ignore_function_names=true,
    ~ignore_filters=true,
    ~closures_by_id=true,
    ~ignore_fixpoints=true,
    ~use_expr_wildcards=
      (env, exp) => ValueChecker.check_value((), env, exp) != Expr,
    ~env1=fenv,
    ~env2=denv,
    (),
  ).
    exp(
    f,
    d,
  );

let matches =
    (~env: ClosureEnvironment.t, ~exp: DHExp.t, ~flt: TermBase.filter)
    : option(FilterAction.t) =>
  if (matches_exp(~denv=env, exp, ~fenv=env, flt.pat)) {
    Some(flt.act);
  } else {
    None;
  };

let matches =
    (~env: ClosureEnvironment.t, ~exp: DHExp.t, ~act: FilterAction.t, flt_env)
    : (FilterAction.t, int) => {
  let len = List.length(flt_env);
  let rec matches' = (~env, ~exp, ~act, flt_env, idx) => {
    switch (flt_env) {
    | [] => (act, idx)
    | [hd, ...tl] =>
      switch (matches(~env, ~exp, ~flt=hd)) {
      | Some(act) => (act, idx)
      | None => matches'(~env, ~exp, ~act, tl, idx + 1)
      }
    };
  };
  let (act, idx) = matches'(~env, ~exp, ~act, flt_env, 0);
  (act, len - idx);
};
