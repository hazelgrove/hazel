open Nondeterminism;

module Make = (S: Search) => {
  type tag =
    | BoxedValue
    | Indet
    | Expr;
  type expert_tag = OneStepEvaluator.TryStep.t(DHExp.t);

  type t('a) = (DHExp.t, tag) => S.t('a);
  type expert_t('a, 'state) =
    ('state, DHExp.t, expert_tag) =>
    (S.t(('state, 'a)), S.t(('state, DHExp.t)));

  let all: t('a) = S.((d, _) => return(d));

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

  let no_instantiation: t('a) => expert_t('a, 'state) =
    S.(
      Infix.(
        (logic, state, d) =>
          fun
          | BoxedValue => (logic(d, BoxedValue) >>| (r => (state, r)), fail)
          | Indet => (logic(d, Indet) >>| (r => (state, r)), fail)
          | Step(d') => (
              logic(d, Expr) >>| (r => (state, r)),
              return((IndetEvaluatorState.incr_trace(1, state), d')),
            )
      )
    );

  let deterministic: expert_t('a, 'state) =
    S.(
      (state, d) =>
        fun
        | BoxedValue
        | Indet => (return((state, d)), fail)
        | Step(d') => (
            fail,
            return((IndetEvaluatorState.incr_trace(1, state), d')),
          )
    );

  let custom_instantiation:
    (('state, TermBase.exp_t) => S.t(('state, TermBase.exp_t)), t('a)) =>
    expert_t('a, 'state) = {
    S.(
      Infix.(
        (instantiator, logic, state, d) =>
          fun
          | BoxedValue => (logic(d, BoxedValue) >>| (r => (state, r)), fail)
          | Indet => (
              logic(d, Indet) >>| (r => (state, r)),
              instantiator(state, d),
            )
          | Step(d') => (
              logic(d, Expr) >>| (r => (state, r)),
              return((IndetEvaluatorState.incr_trace(1, state), d')),
            )
      )
    );
  };
};
