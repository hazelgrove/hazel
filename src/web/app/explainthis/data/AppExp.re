open Haz3lcore;
open ExplainThisForm;
open Example;

let funapp_exp_ex = {
  sub_id: FunAp,
  term: mk_example("(fun x -> x)(1)"),
  message: "The identity function is applied to 1. The argument x is bound to 1 in the function body and the body evaluates to 1.",
};
// TODO Has a red box around it in the result
let conapp_exp_ex = {
  sub_id: ConAp,
  term: mk_example("type T = None + Some(Int)\nin Some(1)"),
  message: "The constructor Some is applied to 1, which evaluates to Some(1).",
};
let deferred_funapp_exp_ex = {
  sub_id: DeferredAp,
  term:
    mk_example(
      "let plus = fun (x, y) -> x + y in\nlet incr = plus(_, 1) in\nincr(5)",
    ),
  message: "The plus function is partially applied. The argument y is bound to 1 in the function body. The deferred argument x is not applied until in the full function application, incr(5), where it's bound to 5. The partial application evaluates to a new function, (fun x -> x + 1).",
};
let exp_fun = exp("e_fun");
let exp_arg = exp("e_arg");
let funapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_fun), x_id),
  (Piece.id(exp_arg), arg_id),
];
let funapp_exp_form = [exp_fun, mk_ap_exp([[exp_arg]])];
let funapp_exp = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: FunApExp,
  syntactic_form: funapp_exp_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Applies the [*function*](%s) to the [*argument*](%s).",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [funapp_exp_ex],
};
let exp_con = exp("e_con");
let exp_arg = exp("e_arg");
let conapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_con), x_id),
  (Piece.id(exp_arg), arg_id),
];
let conapp_exp_form = [exp_con, mk_ap_exp([[exp_arg]])];
let conapp_exp = (~name: string, ~x_id: Id.t, ~arg_id: Id.t): form => {
  id: ConApExp,
  syntactic_form: conapp_exp_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Applies the [*`%s` constructor*](%s) to the [*argument*](%s).",
      name,
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [conapp_exp_ex],
};
let exp_fun = exp("e_fun");
let exp_deferral = deferral();
let deferred_funapp_exp_coloring_ids =
    (~x_id: Id.t, ~deferred_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_fun), x_id),
  (Piece.id(exp_deferral), deferred_id),
];
let deferred_funapp_exp_comma = comma_exp();
let deferred_funapp_exp_form = [
  exp_fun,
  mk_ap_exp([
    [
      exp("..."),
      deferred_funapp_exp_comma,
      space(),
      exp_deferral,
      deferred_funapp_exp_comma,
      space(),
      exp("..."),
    ],
  ]),
];
let deferred_funapp_exp =
    (~x_id: Id.t, ~supplied_id: Id.t, ~deferred_id: Id.t): form => {
  id: DeferredApExp,
  syntactic_form: deferred_funapp_exp_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Applies the [*function*](%s) to the [*supplied arguments*](%s). The [*deferred arguments*](%s) can be applied in future applications.",
      Id.to_string(x_id),
      Id.to_string(supplied_id),
      Id.to_string(deferred_id),
    ),
  examples: [deferred_funapp_exp_ex],
};

let funaps = (~x_id: Id.t, ~arg_id: Id.t): group => {
  id: FunApExp,
  forms: [funapp_exp(~x_id, ~arg_id)],
};

let conaps = (~name: string, ~x_id: Id.t, ~arg_id: Id.t): group => {
  id: ConApExp,
  forms: [conapp_exp(~name, ~x_id, ~arg_id)],
};

let deferredaps = (~x_id: Id.t, ~supplied_id: Id.t, ~deferred_id: Id.t): group => {
  id: DeferredApExp,
  forms: [deferred_funapp_exp(~x_id, ~supplied_id, ~deferred_id)],
};

let livelitapp_exp_ex = {
  sub_id: LivelitAp,
  term: mk_example("^slider(50)"),
  message: "The slider livelit is expanded to its value, which is 50 in this case. The livelit presents a GUI widget that allows setting the value.",
};

let exp_livelit = exp("^livelit_name");
let exp_arg = exp("model");
let livelitapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_livelit), x_id),
  (Piece.id(exp_arg), arg_id),
];

let livelitapp_exp = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: LivelitApExp,
  syntactic_form: [exp_livelit, mk_ap_exp([[exp_arg]])],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Expands the [*livelit*](%s) to some value based on its [*model*](%s). When projected, creates a GUI widget.",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [livelitapp_exp_ex],
};

let livelitaps = (~x_id: Id.t, ~arg_id: Id.t): group => {
  id: LivelitApExp,
  forms: [livelitapp_exp(~x_id, ~arg_id)],
};
