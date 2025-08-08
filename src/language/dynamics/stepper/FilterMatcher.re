let matches_exp = (~denv, d, ~fenv, f) =>
  Equality.(
    equality({
      ...semantic_settings,
      ignore_fixpoints: true,
      use_expr_wildcards:
        Some((env, exp) => ValueChecker.check_value((), env, exp) != Expr),
      env1: Some(fenv),
      env2: Some(denv),
    })
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
