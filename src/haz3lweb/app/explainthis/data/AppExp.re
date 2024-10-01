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
let _exp_fun = exp("e_fun");
let _exp_arg = exp("e_arg");
let funapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_fun), x_id),
  (Piece.id(_exp_arg), arg_id),
];
let funapp_exp: form = {
  let explanation = "Applies the [*function*](%s) to the [*argument*](%s).";
  {
    id: FunApExp,
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
  let explanation = "Applies the [*`%s` constructor*](%s) to the [*argument*](%s).";
  {
    id: ConApExp,
    syntactic_form: [_exp_con, mk_ap_exp([[_exp_arg]])],
    expandable_id: None,
    explanation,
    examples: [conapp_exp_ex],
  };
};
let _exp_fun = exp("e_fun");
let _exp_deferral = deferral();
let deferred_funapp_exp_coloring_ids =
    (~x_id: Id.t, ~deferred_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_fun), x_id),
  (Piece.id(_exp_deferral), deferred_id),
];
let deferred_funapp_exp: form = {
  let explanation = "Applies the [*function*](%s) to the [*supplied arguments*](%s). The [*deferred arguments*](%s) can be applied in future applications.";
  let comma = comma_exp();
  {
    id: DeferredApExp,
    syntactic_form: [
      _exp_fun,
      mk_ap_exp([
        [
          exp("..."),
          comma,
          space(),
          _exp_deferral,
          comma,
          space(),
          exp("..."),
        ],
      ]),
    ],
    expandable_id: None,
    explanation,
    examples: [deferred_funapp_exp_ex],
  };
};

let funaps: group = {id: FunApExp, forms: [funapp_exp]};

let conaps: group = {id: ConApExp, forms: [conapp_exp]};

let deferredaps: group = {id: DeferredApExp, forms: [deferred_funapp_exp]};
