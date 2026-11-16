/* A data-constructor application is a value, unlike a function call.
   Filter/closure/presentation wrappers are transparent to matching. */
let rec is_constructor_head = (exp: Exp.t): bool =>
  switch (exp.term) {
  | Constructor(name, _) => !String.starts_with(name, ~prefix="$")
  | Filter(_, body)
  | Closure(_, body)
  | Parens(body)
  | Projector(_, body)
  | Asc(body, _) => is_constructor_head(body)
  | _ => false
  };

/* Variables remain non-values even when an environment binds them.
   Check compound values structurally, without evaluating their components. */
let rec is_filter_value = (env: Environment.t(Exp.t), exp: Exp.t): bool =>
  switch (exp.term) {
  | Filter(_, body)
  | Parens(body)
  | Projector(_, body)
  | Asc(body, _)
  | TupLabel(_, body) => is_filter_value(env, body)
  | Closure(closure_env, body) => is_filter_value(closure_env, body)
  | Atom(_)
  | Fun(_)
  | TypFun(_)
  | ProofObject(_)
  | FilterAction(_)
  | FilterSelector(_)
  | LivelitName(_)
  | Label(_)
  | ExplicitNonlabel
  | Constructor(_)
  | BuiltinFun(_) => true
  | Tuple(elements)
  | ListLit(elements) => List.for_all(is_filter_value(env), elements)
  | Ap(_, head, argument) =>
    is_constructor_head(head) && is_filter_value(env, argument)
  | DeferredAp(head, arguments) =>
    is_filter_value(env, head)
    && List.for_all(
         (argument: Exp.t) =>
           switch (argument.term) {
           | Deferral(_) => true
           | _ => is_filter_value(env, argument)
           },
         arguments,
       )
  | DrvQuote(_) =>
    /* Quoted derivations retain Hazel's administrative normalization check. */
    ValueChecker.check_value(env, exp) == Value
  | _ => false
  };

let matches_exp = (~denv, d, ~fenv, f) => {
  Equality.(
    equality({
      ...semantic_settings,
      ignore_fixpoints: true,
      ignore_ascriptions: true,
      is_filter_selector_value: Some(is_filter_value),
      env1: Some(fenv),
      env2: Some(denv),
    })
  ).
    exp(
    f,
    d,
  );
};
