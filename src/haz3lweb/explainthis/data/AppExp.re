/*open Haz3lcore;
  open ExplainThisForm;
  open Example;

  let funapp_exp_group = "funapp_exp_group";
  let conapp_exp_group = "conapp_exp_group";
  let funapp_exp_ex = {
    sub_id: "funapp_exp_ex",
    term: mk_example("(fun x -> x)(1)"),
    message: "The identity function is applied to 1. The argument x is bound to 1 in the function body and the body evaluates to 1.",
    feedback: Unselected,
  };
  // TODO Has a red box around it in the result
  let conapp_exp_ex = {
    sub_id: "conapp_exp_ex",
    term: mk_example("Some(1)"),
    message: "The constructor Some is applied to 1, which evaluates to Some(1).",
    feedback: Unselected,
  };
  let _exp_fun = exp("e_fun");
  let _exp_arg = exp("e_arg");
  let funapp_exp_coloring_ids =
      (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_exp_fun), x_id),
    (Piece.id(_exp_arg), arg_id),
  ];
  let funapp_exp: form = {
    let explanation = {
      message: "Function application. Apply the [*function*](%i) to the [*argument*](%i).",
      feedback: Unselected,
    };
    {
      id: "funapp_exp",
      syntactic_form: [_exp_fun, mk_ap_exp([[_exp_arg]])],
      expandable_id: None,
      explanation,
      examples: [funapp_exp_ex],
    };
  };
  let _exp_con = exp("e_con");
  let _exp_arg = exp("e_arg");
  let conapp_exp_coloring_ids =
      (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_exp_con), x_id),
    (Piece.id(_exp_arg), arg_id),
  ];
  let conapp_exp: form = {
    let explanation = {
      message: "Constructor application. Apply the [*`%s` constructor*](%i) to the [*argument*](%i).",
      feedback: Unselected,
    };
    {
      id: "conapp_exp",
      syntactic_form: [_exp_con, mk_ap_exp([[_exp_arg]])],
      expandable_id: None,
      explanation,
      examples: [conapp_exp_ex],
    };
  };*/
