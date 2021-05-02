open EvaluatorStep;
let mk:
  (
    ~settings: Settings.Evaluation.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    ~eva_obj: option(EvalObj.t)=?,
    DHExp.t
  ) =>
  DHDoc.t;
