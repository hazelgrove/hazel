open Nondeterminism;

module Make = (S: Search) => {
  type tag =
    | BoxedValue
    | Indet
    | Expr;
  type expert_tag = OneStepEvaluator.TryStep.t;

  type t('a) = (DHExp.t, tag) => S.t('a);
  type expert_t('a) = (DHExp.t, expert_tag) => (S.t('a), S.t(DHExp.t));

  let boxed_values: t('a) =
    S.(
      d =>
        fun
        | BoxedValue => return(d)
        | _ => fail
    );
  let indet_values: t('a) =
    S.(
      d =>
        fun
        | Indet => return(d)
        | _ => fail
    );

  let values: t('a) =
    S.(
      d =>
        fun
        | Indet => return(d)
        | BoxedValue => return(d)
        | _ => fail
    );
  let expressions: t('a) =
    S.(
      d =>
        fun
        | Expr => return(d)
        | _ => fail
    );

  let cast_error: t('a) =
    S.(
      Syntax.(
        d =>
          fun
          | Indet => {
              let* () = guard(CastErrorChecker.contains_error(d));
              return(d);
            }
          | _ => fail
      )
    );

  let no_instantiation: t('a) => expert_t('a) =
    S.(
      (logic, d) =>
        fun
        | BoxedValue => (logic(d, BoxedValue), fail)
        | Indet => (logic(d, Indet), fail)
        | Step(d') => (logic(d, Expr), return(d'))
    );

  let deterministic: expert_t('a) =
    S.(
      d =>
        fun
        | BoxedValue
        | Indet => (fail, fail)
        | Step(d') => (fail, return(d'))
    );
};
