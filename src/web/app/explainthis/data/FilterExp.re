open Haz3lcore;
open ExplainThisForm;
open Example;

let filter_pause = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: FilterPause,
  form_id: FilterPause,
  abstract:
    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
      [mk_pause([[space(), p', space()]]), linebreak(), e_body']
    ),
  explanation:
    Printf.sprintf(
      "Pause filter for stepper. The evaluation of all subexpressions within [*body*](%s) that match the [*pattern*](%s) will be paused during evaluation",
      body_id |> Id.to_string,
      p_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: FilterStep,
      term: mk_example("eval $e + $e in\n(1 + 2) * (3 + 4)"),
      message: "The expression (1 * 2) + (3 * 4) is guarded by a pause filter expression pause $v + $v, which instruct the evaluator to pause the evaluation when it sees a value is added to another value. After evaluating subterms (1 * 2) and (3 * 4), the expression turns into 2 + 12. 2 matches the first $v pattern, and 12 matches the second $v pattern. Therefore, the evaluator stops when the expression steps to 2 + 12",
    },
  ],
};

let filter_eval = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: FilterEval,
  form_id: FilterEval,
  abstract:
    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
      [mk_eval([[space(), p', space()]]), linebreak(), e_body']
    ),
  explanation:
    Printf.sprintf(
      "Full evaluation filter for stepper. All subexpressions within [*body*](%s) that match the [*pattern*](%s) will get evaluated in one go",
      body_id |> Id.to_string,
      p_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: FilterEval,
      term:
        mk_example(
          "pause $e in\nhide let = in in\nlet x = 1 in\nlet y = 2 in\nx + y",
        ),
      message: "pause $e in instruct the evaluator to act like a single-stepper, e.g. stop at every step. The hide filter expression instructs the evaluator to skip over all evaluator steps that destructs perform substitution on a let-expression. Here, the substitution of variable x and y is skipped over and we directly got 1 + 2 in the result area.",
    },
  ],
};

let filter_hide = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: FilterHide,
  form_id: FilterHide,
  abstract:
    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
      [mk_hide([[space(), p', space()]]), linebreak(), e_body']
    ),
  explanation:
    Printf.sprintf(
      "Step hiding filter for stepper. The elimination of all language constructs (like binary operator + or let .. = .. in) within [*body*](%s) that match the [*pattern*](%s) will get skipped.",
      body_id |> Id.to_string,
      p_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: FilterHide,
      term:
        mk_example(
          "pause $e in\nhide let = in in\nlet x = 1 in\nlet y = 2 in\nx + y",
        ),
      message: "pause $e in instruct the evaluator to act like a single-stepper, e.g. stop at every step. The hide filter expression instructs the evaluator to skip over all evaluator steps that destructs perform substitution on a let-expression. Here, the substitution of variable x and y is skipped over and we directly got 1 + 2 in the result area.",
    },
  ],
};

let filter_debug = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: FilterDebug,
  form_id: FilterDebug,
  abstract:
    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
      [mk_debug([[space(), p', space()]]), linebreak(), e_body']
    ),
  explanation:
    Printf.sprintf(
      "Debug filter for stepper. All matched sub-expression within [*body*](%s) that match the [*pattern*](%s) will be stepped through.",
      body_id |> Id.to_string,
      p_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: FilterDebug,
      term: mk_example("eval $e in\ndebug $v + $v + $v in\n1 + 2 + 3"),
      message: "The debug filter pattern $v + $v + $v matches 1 + 2 + 3, therefore, the evaluator will step into the evaluation of the matched sub-expression 1 + 2 + 3.",
    },
  ],
};

let unquote = (~sel_id: Id.t): Simple.t => {
  group_id: FilterSelector,
  form_id: FilterSelector,
  abstract: Simple.mk_1(("sel", sel_id), sel' => [mk_unquote([]), sel']),
  explanation:
    Printf.sprintf(
      "Selector expression for a stepper filter pattern. When [*sel*](%s) is 'e', it matches any expression, when [*sel*](%s) is 'v' it only matches values.",
      sel_id |> Id.to_string,
      sel_id |> Id.to_string,
    ),
  examples: [],
};
