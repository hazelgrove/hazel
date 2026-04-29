open Haz3lcore;
open ExplainThisForm;
open Example;

let filter_hide_example = {
  sub_id: Filter(Hide),
  term:
    mk_example(
      {|# skip-over eval of all expressions #
debug eval($e) in
let fib : Int -> Int = fun n ->
  case n
    | 0 => 1
    | 1 => 2
    | n => fib(n - 1) + fib(n - 2)
  end
in
# stop at application of fib function to a value #
debug stop(fib($v)) in
 # skip-over the evaluation of fib(2) #
debug hide(fib(2)) in
fib(3)|},
    ),
  message: "Here `hide` means we want to hide the evaluation of such expression, but not skip them entirely. This action is especially useful if you want to un-stop certain expressions.",
};

let filter_eval_example = {
  sub_id: Filter(Eval),
  term: mk_example("debug eval($e) in\n1 + 2 + 3 + 4"),
  message: "Here `eval` means we want to skip the evaluation of all expression.",
};

let filter_stop_example = {
  sub_id: Filter(Stop),
  term:
    mk_example(
      {|# skip-over eval of all expressions #
debug eval($e) in
let fib : Int -> Int = fun n ->
  case n
    | 0 => 1
    | 1 => 2
    | n => fib(n - 1) + fib(n - 2)
  end
in
 # stop at application of fib function to a value #
debug stop(fib($v)) in
fib(3)|},
    ),
  message: "Here `stop` means we want to stop at the evaluation of such expression, and resume immediately.",
};

let filter_step_example = {
  sub_id: Filter(Step),
  term:
    mk_example(
      {|# skip-over eval of all expressions #
debug eval($e) in
let fib : Int -> Int = fun n ->
  case n
    | 0 => 1
    | 1 => 2
    | n => fib(n - 1) + fib(n - 2)
  end
in
# stop at application of fib function, and resume after evaluation of current expression. #
debug step(fib(2)) in
fib(3)|},
    ),
  message: "Here `step` means we want to step through the evaluation of such expression. Once such expression finish evaluating, the stepper will resume to the stepping behavior it used to have.",
};

let _pat = exp("pat");

let _act = exp("act");

let _body = exp("e_body");

let filter_hide_exp: form = {
  let _hide = exp("hide");
  let explanation = "The stepper will [*skip-over/hide*](%s) the first step of evaluation of any expression that matches the [*pattern*](%s) inside [*body*](%s).";
  let form = [
    mk_filter([[space(), _hide, mk_ap_exp([[_pat]]), space()]]),
    linebreak(),
    _body,
  ];
  {
    id: FilterExp((Eval, One)),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_hide), [_hide])),
    explanation,
    examples: [filter_hide_example],
  };
};

let filter_action_exp = (act: Language.FilterAction.t): form => {
  id: FilterAction,
  syntactic_form: [Language.FilterAction.string_of_t(act) |> exp],
  expandable_id: None,
  explanation: "Filter action, can be one of `eval`, `hide`, `step`, or `stop`.",
  examples: [],
};

let filter_action_exps = (act: Language.FilterAction.t): group => {
  {
    id: FilterAction,
    forms: [filter_action_exp(act)],
  };
};

let filter_eval_exp: form = {
  let _eval = exp("eval");
  let explanation = "The stepper will [*skip-over/eval*](%s) the evaluation of any expression that matches the [*pattern*](%s) inside [*body*](%s).";
  let form = [
    mk_filter([[space(), _eval, mk_ap_exp([[_pat]]), space()]]),
    linebreak(),
    _body,
  ];
  {
    id: FilterExp((Eval, All)),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_eval), [_eval])),
    explanation,
    examples: [filter_eval_example],
  };
};

let filter_stop_exp: form = {
  let _stop = exp("stop");
  let explanation = "The stepper will [*stop*](%s) at any expression that matches the [*pattern*](%s) inside [*body*](%s), and will resume immediately.";
  let form = [
    mk_filter([[space(), _stop, mk_ap_exp([[_pat]]), space()]]),
    linebreak(),
    _body,
  ];
  {
    id: FilterExp((Step, One)),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_stop), [_stop])),
    explanation,
    examples: [filter_stop_example],
  };
};

let filter_step_exp: form = {
  let _step = exp("step");
  let explanation = "The stepper will [*step-through*](%s) at any expression that matches the [*pattern*](%s) inside [*body*](%s), and will resume stepping after evaluating that expression.";
  let form = [
    mk_filter([[space(), _step, mk_ap_exp([[_pat]]), space()]]),
    linebreak(),
    _body,
  ];
  {
    id: FilterExp((Step, All)),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_step), [_step])),
    explanation,
    examples: [filter_step_example],
  };
};

let _filter_exp_coloring_ids =
    (
      sf_act_id: Id.t,
      sf_pat_id: Id.t,
      sf_body_id: Id.t,
      ~act_id: Id.t,
      ~pat_id: Id.t,
      ~body_id: Id.t,
    )
    : list((Id.t, Id.t)) => {
  [(sf_act_id, act_id), (sf_pat_id, pat_id), (sf_body_id, body_id)];
};

let filter_exp_coloring_ids =
  _filter_exp_coloring_ids(
    Piece.id(_act),
    Piece.id(_pat),
    Piece.id(_body),
  );

let filter_exp: group = {
  {
    id: FilterExp,
    forms: [
      filter_eval_exp,
      filter_hide_exp,
      filter_step_exp,
      filter_stop_exp,
    ],
  };
};

let filter_selector_exp_example = {
  sub_id: FilterSelector(Exp),
  term: mk_example("debug eval($e) in\n1 + 2 + 3 + 4"),
  message: "Here `$e` matches any expression when applying `eval` filter to an expression, which is `1 + 2` in this case, as it is the immediately next expression to be evaluated.",
};

let filter_selector_exp_exp: form = {
  let _e = exp("$e");
  {
    id: FilterSelector(Exp),
    syntactic_form: [_e],
    expandable_id: Some((Piece.id(_e), [_e])),
    explanation: "Matches expression, i.e. anything when apply filters to an expression.",
    examples: [filter_selector_exp_example],
  };
};

let filter_selector_val_example = {
  sub_id: FilterSelector(Val),
  term:
    mk_example(
      "debug eval($e) in\ndebug step($v + $v) in\n(1 + 2) * (3 + 4)",
    ),
  message: {|Here `$v` matches any value when applying `step` filter to an expression.

For example, `$v` matches `3`, but does not match `1 + 2` because it is an expression, not a value;

`$v * $v` matches `3 * 4`, but does not match `(1 + 2) * (3 + 4)` because it both left hand side and right hand side of the multiplication are expressions, not values.

In this case, the two `$v`s match the two values which are `3` (first `$v`) and `7` (second `$v`), thus the stepper stops when it evaluates to `3 * 7`.|},
};

let filter_selector_val_exp: form = {
  let _v = exp("$v");
  {
    id: FilterSelector(Val),
    syntactic_form: [_v],
    expandable_id: Some((Piece.id(_v), [_v])),
    explanation: "Matches value, i.e. fully evaluated expressions when apply filters to an expression.",
    examples: [filter_selector_val_example],
  };
};

let filter_selector_exps = (sel: Language.FilterSelector.t): group => {
  switch (sel) {
  | Exp => {
      id: FilterSelector,
      forms: [filter_selector_exp_exp, filter_selector_val_exp],
    }
  | Val => {
      id: FilterSelector,
      forms: [filter_selector_val_exp, filter_selector_exp_exp],
    }
  };
};
