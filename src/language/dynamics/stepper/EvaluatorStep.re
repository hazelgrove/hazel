open Util;
open Transition;

module Decompose = {
  module Result = {
    type t =
      | Indet
      | BoxedValue
      | Step(list(EvalObj.t));

    let unbox = (r: t): list(EvalObj.t) => {
      switch (r) {
      | Indet
      | BoxedValue => []
      | Step(objs) => objs
      };
    };
  };

  module DecomposeEVMode: {
    include
      EV_MODE with
        type result = Result.t and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
    type requirement('a) = (Result.t, 'a);
    type requirements('a, 'b) = ('b, Result.t, ClosureEnvironment.t, 'a);
    type result = Result.t;

    let (&&): (Result.t, Result.t) => Result.t =
      (u, v) =>
        switch (u, v) {
        | (Step(ss1), Step(ss2)) => Step(ss1 @ ss2)
        | (Step(ss), _)
        | (_, Step(ss)) => Step(ss)
        | (Indet, BoxedValue)
        | (BoxedValue, Indet)
        | (Indet, Indet) => Indet
        | (BoxedValue, BoxedValue) => BoxedValue
        };

    let req_final = (cont, wr, d) => {
      (
        switch (cont(d)) {
        | Result.Indet => Result.Indet
        | Result.BoxedValue => Result.BoxedValue
        | Result.Step(objs) =>
          Result.Step(List.map(EvalObj.wrap(wr), objs))
        },
        d,
      );
    };

    let rec req_all_final' = (cont, wr, ds') =>
      fun
      | [] => (Result.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_final(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_final'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };

    let req_all_final = (cont, wr, ds) => {
      req_all_final'(cont, wr, [], ds);
    };

    let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
      (rq, rl) => {
        switch (rq) {
        | (_, Result.Step(_) as r, _, _) => r
        | (undo, r, env, v) =>
          switch (rl(v)) {
          | Constructor => r
          | Value => Result.BoxedValue
          | Indet => Result.Indet
          | Step(s) => Result.Step([EvalObj.mk(Mark, env, undo, s.kind)])
          // TODO: Actually show these exceptions to the user!
          | exception (EvaluatorError.Exception(_)) => Result.Indet
          }
        };

    let (and.):
      (requirements('a, 'c => 'b), requirement('c)) =>
      requirements(('a, 'c), 'b) =
      ((u, r1, env, v1), (r2, v2)) => (u(v2), r1 && r2, env, (v1, v2));

    let otherwise = (env, o) => (o, Result.BoxedValue, env, ());
    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);
    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := EvaluatorState.add_closure(state^, closure);
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (~in_closure=?, state, env, exp) => {
    switch (exp) {
    | _ =>
      Decomp.transition(
        decompose,
        ~mode=`Substitution,
        ~in_closure?,
        state,
        env,
        exp,
      )
    };
  };
};

module TakeStep = {
  module TakeStepEVMode: {
    include
      EV_MODE with
        type result = option(DHExp.t) and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
    type requirement('a) = 'a;
    type requirements('a, 'b) = 'a;
    type result = option(DHExp.t);

    // Assume that everything is either value or final as required.
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let (let.) = (rq: requirements('a, DHExp.t), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({expr, state_update, _}) =>
        state_update();
        Some(expr);
      | Constructor
      | Value
      | Indet => None
      };

    let (and.) = (x1, x2) => (x1, x2);

    let otherwise = (_, _) => ();

    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);

    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := EvaluatorState.add_closure(state^, closure);
  };

  module TakeStepEV = Transition(TakeStepEVMode);

  let take_step = (~in_closure=?, state, env, d) =>
    TakeStepEV.transition(
      (~in_closure as _=?, _, _, _) => None,
      ~mode=`Substitution,
      ~in_closure?,
      state,
      env,
      d,
    )
    |> Option.map(DHExp.replace_all_ids);
};

let take_step = TakeStep.take_step;

let decompose = (d: DHExp.t, es: EvaluatorState.t) => {
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
};

/* ========== PUBLIC METHODS ========== */
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = EvalObj.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | AutoStep(step)
  | AvailableSteps(list(step));

let get_status = (~settings: CoreSettings.t, exp, state) => {
  let eos =
    decompose(exp, state)
    |> List.map(EvalObj.should_hide_eval_obj(~settings=settings.evaluation)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  switch (List.find_opt(((x, _)) => x == FilterAction.Eval, eos)) {
  | Some((_, x)) => AutoStep(x)
  | None => AvailableSteps(List.map(((_, x)) => x, eos))
  };
};

let get_step_id = (step: step): Id.t => step.d_loc |> DHExp.rep_id;

let get_step_kind = (step: step): step_kind => step.knd;

let take_step = (step: EvalObj.t) => {
  let state = ref(EvaluatorState.init); // HACK: state isn't actually carried through the stepper...
  let+ next_expr = take_step(state, step.env, step.d_loc);
  let next_expr = {
    ...next_expr,
    annotation: IdTagged.IdTag.{ids: step.d_loc |> IdTagged.ids},
  };
  let next_state = state^;
  let next_expr = EvalCtx.compose(step.ctx, next_expr) |> Exp.replace_all_ids;
  //|> DHExp.substitute_closures(Builtins.env_init);
  (next_expr, next_state);
};

let refresh_step =
    (
      ~settings: CoreSettings.t,
      exp: Exp.t,
      state: EvaluatorState.t,
      step: step,
    ) => {
  let eos =
    decompose(exp, state)
    |> List.map(should_hide_eval_obj(~settings=settings.evaluation)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  let* (h, x) =
    List.find_opt(
      ((_, step': step)) =>
        IdTagged.ids(step'.d_loc) == IdTagged.ids(step.d_loc),
      eos,
    );
  Some((h, x));
};
