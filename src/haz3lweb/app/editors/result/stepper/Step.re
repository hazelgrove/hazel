module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | EvalStep(EvalStep.Model.t);
};

module Update = {
  let calculate =
      (~settings, ~prev_expr, ~options, ~state): (Model.t => Model.t) =>
    fun
    | EvalStep(m) =>
      EvalStep(
        EvalStep.Update.calculate(~settings, m, ~prev_expr, ~options, ~state),
      );
};

module View = {
  let is_hidden: Model.t => bool =
    fun
    | EvalStep(m) => EvalStep.View.is_hidden(m);

  let stepped_id: Model.t => option(Haz3lcore.Id.t) =
    fun
    | EvalStep(m) => EvalStep.View.stepped_id(m);

  let view: Model.t => 'a =
    fun
    | EvalStep(m) => EvalStep.View.view(m);

  let get_text: Model.t => 'a =
    fun
    | EvalStep(m) => EvalStep.View.get_text(m);
};
