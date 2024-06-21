open Haz3lcore;
open Sexplib.Std;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    step: EvaluatorStep.EvalObj.persistent,
    // Calculated
    eval_obj: option(EvaluatorStep.EvalObj.t), // If None then step is invalid
    hidden: bool,
    next_expr: Exp.t,
    next_state: EvaluatorState.t,
  };

  let mk = (step): t => {
    {
      step,
      eval_obj: None,
      hidden: false,
      next_expr: Exp.fresh(EmptyHole),
      next_state: EvaluatorState.init,
    };
  };

  let get_next_expr = (model: t) => model.next_expr;
  let get_next_state = (model: t) => model.next_state;

  type persistent = EvaluatorStep.EvalObj.persistent;
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    |;

  let update = (~settings as _, _, _): Updated.t(Model.t) =>
    failwith("EvalStep: no updates available");

  let calculate =
      (
        ~settings as _: CoreSettings.t,
        ~prev_expr: Exp.t,
        ~options: list((FilterAction.action, EvaluatorStep.EvalObj.t)),
        ~state,
        model: Model.t,
      )
      : Model.t => {
    let eval_obj =
      List.find_opt(
        ((_, eo: EvaluatorStep.EvalObj.t)) =>
          eo.d_loc |> Exp.rep_id == model.step.old_id,
        options,
      );
    switch (eval_obj) {
    | Some((action, eval_obj)) =>
      let state = ref(state);
      let next_expr =
        EvaluatorStep.take_step(
          state,
          ClosureEnvironment.of_environment(Builtins.env_init),
          eval_obj.d_loc,
        )
        |> Option.get;
      let next_state = state^;
      let next_expr = {...next_expr, ids: [model.step.new_id]};
      let next_expr = EvalCtx.compose(eval_obj.ctx, next_expr);
      Model.{
        step: model.step,
        hidden:
          switch (action) {
          | Step => false
          | Eval => true
          },
        eval_obj: Some(eval_obj),
        next_expr,
        next_state,
      };
    | None => {
        step: model.step,
        hidden: true,
        eval_obj: None,
        next_expr: prev_expr,
        next_state: state,
      }
    };
  };
};

module Selection = {
  type t;
};

module View = {
  open Virtual_dom.Vdom;

  let is_hidden = (model: Model.t) => model.hidden;

  let stepped_id = (model: Model.t) => Some(model.step.old_id);

  let view = (_: Model.t) => [];

  let get_text = (model: Model.t) =>
    (
      switch (model.eval_obj) {
      | Some(eval_obj) => eval_obj.knd |> Transition.stepper_justification
      | None => "Broken Step"
      }
    )
    |> Node.text;

  let get_selected_id = (model: Model.t) =>
    model.eval_obj
    |> Option.map((eo: EvaluatorStep.EvalObj.t) => eo.d_loc |> Exp.rep_id);
};
