open Alcotest;

open Language;
open IdTagged.FreshGrammar;
open Util;

let settings = CoreSettings.on;
let env = Environment.empty;

let print_for_algebrite = exp =>
  Web.RewriteChecker.print_exp_for_algebrite(~name_other=_ => "unknown", exp);

/* Legacy macro fixtures remain test-local so production proof search cannot
   manufacture a collapsed authorization trace. */
let collapsed_macro_summary_for_purpose = (~purpose, request) => {
  let profile =
    Axioms.effective_profile_for_rewrite(
      ~requested_level=request.Web.ProofSearchBackend.level,
      request.source,
      request.target,
    );
  let plan =
    Web.ProofSearchBackend.rocq_plan_for_profile_and_purpose(
      profile,
      purpose,
    );
  let rule_id = profile.rocq_macro_rule_id;
  let step =
    Web.ProofTrace.prover_step(
      ~origin=Normalization,
      ~rule_id,
      ~before_full_exp=request.source,
      ~after_full_exp=request.target,
      ~before_exp=request.source,
      ~after_exp=request.target,
      ~detail="legacy macro export fixture: " ++ plan.id,
    );
  Web.ProofTrace.{
    justification: "Rocq tactic search",
    group_name:
      List.rev(profile.groups)
      |> ListUtil.hd_opt
      |> Option.map((group: Axioms.rewrite_group) => group.name),
    from_normal_exp: request.target,
    to_normal_exp: request.target,
    from_rule_ids: [rule_id],
    to_rule_ids: [],
    rule_ids: [rule_id],
    prover_steps: [step],
    exportable: true,
  };
};

let collapsed_macro_summary = request =>
  collapsed_macro_summary_for_purpose(~purpose=Axioms.CheckResult, request);

let local_profile_trace = (~profile, ~settings, ~env, request) =>
  Web.ProofSearchBackend.local_profile_plan(
    ~profile,
    ~settings,
    ~env,
    request,
  )
  |> Option.map((plan: Web.ProfileProofPlan.authorized_plan) => plan.summary);

let plus = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Plus), left, right);

let minus = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Minus), left, right);

let times = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Times), left, right);

let power = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Power), left, right);

let divide = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Divide), left, right);

let negate = exp => Exp.un_op(Operators.Int(Operators.Minus), exp);

let app = (name, arg) =>
  Language.Exp.fresh(Ap(Operators.Forward, Exp.var(name), arg));

let builtin_app = (name, arg) =>
  Language.Exp.fresh(
    Ap(Operators.Forward, Language.Exp.fresh(BuiltinFun(name)), arg),
  );

let diff = (expression, variable) =>
  app("diff", Exp.tuple([expression, variable]));
let function_diff = expression => app("diff", expression);
let expression_derivative = (expression, variable) =>
  Language.DerivativeOperator.expression(~body=expression, ~variable);
let function_derivative = Language.DerivativeOperator.function_;

let sin = arg => app("sin", arg);
let cos = arg => app("cos", arg);
let tan = arg => app("tan", arg);
let builtin_sin = arg => builtin_app("sin", arg);
let builtin_cos = arg => builtin_app("cos", arg);

let float = value => Exp.float(value);

let float_plus = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Plus), left, right);

let float_minus = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Minus), left, right);

let float_divide = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Divide), left, right);

let float_times = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Times), left, right);

let float_power = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Power), left, right);

let real = value => Exp.real(Real.of_bigint(Bigint.of_int(value)));

let real_plus = (left, right) =>
  Exp.bin_op(Operators.Real(Operators.Plus), left, right);

let real_minus = (left, right) =>
  Exp.bin_op(Operators.Real(Operators.Minus), left, right);

let real_times = (left, right) =>
  Exp.bin_op(Operators.Real(Operators.Times), left, right);

let real_power = (left, right) =>
  Exp.bin_op(Operators.Real(Operators.Power), left, right);

type exact_numeric_syntax = {
  label: string,
  number: int => Exp.t,
  plus: (Exp.t, Exp.t) => Exp.t,
  minus: (Exp.t, Exp.t) => Exp.t,
  times: (Exp.t, Exp.t) => Exp.t,
  power: (Exp.t, Exp.t) => Exp.t,
};

let exact_numeric_syntaxes = [
  {
    label: "Int",
    number: Exp.int,
    plus,
    minus,
    times,
    power,
  },
  {
    label: "Real",
    number: real,
    plus: real_plus,
    minus: real_minus,
    times: real_times,
    power: real_power,
  },
];

let check_written = (name, left, right, expected) =>
  check(
    option(string),
    name,
    expected,
    Web.RewriteChecker.check_written_step(~settings, ~env, left, right),
  );

let check_written_at_level = (name, level, left, right, expected) =>
  check(
    option(string),
    name,
    expected,
    Web.RewriteChecker.check_written_step_at_level(
      ~level,
      ~settings,
      ~env,
      left,
      right,
    ),
  );

let require_written_result = (left, right) =>
  switch (
    Web.RewriteChecker.check_written_step_result(~settings, ~env, left, right)
  ) {
  | Some(result) => result
  | None => fail("expected written step to be accepted")
  };

let require_written_result_at_level = (level, left, right) =>
  switch (
    Web.RewriteChecker.check_written_step_result_at_level(
      ~level,
      ~settings,
      ~env,
      left,
      right,
    )
  ) {
  | Some(result) => result
  | None => fail("expected written step at level to be accepted")
  };

let require_written_trace = (left, right) =>
  switch (
    Web.RewriteChecker.check_written_step_trace(~settings, ~env, left, right)
  ) {
  | Some(result) => result
  | None => fail("expected written step trace to be accepted")
  };

let require_single_eval_result = (left, right) =>
  switch (
    Web.RewriteChecker.check_single_eval_step_result(
      ~settings,
      ~env,
      left,
      right,
    )
  ) {
  | Some(result) => result
  | None => fail("expected single eval step to be accepted")
  };

let require_single_step_result_at_level = (level, left, right) =>
  switch (
    Web.RewriteChecker.check_single_step_result_at_level(
      ~level,
      ~settings,
      ~env,
      left,
      right,
    )
  ) {
  | Some(result) => result
  | None => fail("expected single step to be accepted")
  };

let rewrite_group_name = (group: Axioms.rewrite_group) => group.name;

let rewrite_group_level = (group: Axioms.rewrite_group) => group.level;

let rewrite_rule_id = (rule: Axioms.rewrite_rule) => rule.id;

let rocq_tactic_step_id = (step: Axioms.rocq_tactic_step) => step.id;

let rocq_tactic_step_mode = (step: Axioms.rocq_tactic_step) =>
  Axioms.rocq_tactic_mode_label(step.mode);

let rocq_tactic_plan_purpose_label = ((purpose, _plan)) =>
  Axioms.rocq_tactic_plan_purpose_label(purpose);

let cleanup_capability_label = capability =>
  Axioms.cleanup_capability_label(capability);

let prover_hint_prover = (hint: Axioms.prover_hint) => hint.prover;

let prover_hint_tactic = (hint: Axioms.prover_hint) => hint.tactic;

let has_lean_hint = (rule: Axioms.rewrite_rule) =>
  rule.prover_hints |> List.exists(hint => prover_hint_prover(hint) == "lean");

let has_trace_rule = (rule_id, result: Web.RewriteChecker.check_result) =>
  result.trace |> List.exists(rule => rewrite_rule_id(rule) == rule_id);

let has_rule_id = (rule_id, rule_ids) => rule_ids |> List.mem(rule_id);

let prover_step_rule_id = (step: Web.ProofTrace.prover_step) => step.rule_id;

let prover_step_origin = (step: Web.ProofTrace.prover_step) =>
  switch (step.origin) {
  | ManualRewrite => "manual"
  | Normalization => "normalization"
  | AutoEvaluation => "auto"
  };

let profile_board_result_id = (result: Web.ProfileBoard.example_result) =>
  result.example.id;

let profile_board_result_status = (result: Web.ProfileBoard.example_result) =>
  result.accepted;

let visible_rule_summary_id = (summary: Web.ProfileBoard.visible_rule_summary) =>
  summary.rule_id;

let visible_rule_summary_name =
    (summary: Web.ProfileBoard.visible_rule_summary) =>
  summary.name;

let visible_rule_summary_example =
    (summary: Web.ProfileBoard.visible_rule_summary) =>
  summary.example;

let visible_rule_summary_cleanup =
    (summary: Web.ProfileBoard.visible_rule_summary) =>
  summary.cleanup_labels;

let string_contains = (needle, haystack) => {
  let needle_len = String.length(needle);
  let haystack_len = String.length(haystack);
  let rec loop = offset =>
    offset
    + needle_len <= haystack_len
    && (
      String.sub(haystack, offset, needle_len) == needle || loop(offset + 1)
    );
  needle_len == 0 || loop(0);
};

let check_exp_equal = (name, expected, actual) =>
  check(
    testable(
      Fmt.using(Language.Exp.show, Fmt.string),
      Language.Exp.fast_equal,
    ),
    name,
    expected,
    actual,
  );

let check_prover_step =
    (name, rule_id, before_exp, after_exp, step: Web.ProofTrace.prover_step) => {
  check(string, name ++ " rule", rule_id, step.rule_id);
  check_exp_equal(name ++ " local before", before_exp, step.before_exp);
  check_exp_equal(name ++ " local after", after_exp, step.after_exp);
  check_exp_equal(name ++ " whole before", before_exp, step.before_full_exp);
  check_exp_equal(name ++ " whole after", after_exp, step.after_full_exp);
};

let check_written_result_at_level = (name, level, left, right, expected) =>
  check(
    option(string),
    name,
    expected,
    Web.RewriteChecker.check_written_step_result_at_level(
      ~level,
      ~settings,
      ~env,
      left,
      right,
    )
    |> Option.map((result: Web.RewriteChecker.check_result) =>
         result.justification
       ),
  );

let check_simplifies = (name, input, expected) =>
  switch (Web.RewriteChecker.simplify_arithmetic(~settings, ~env, input)) {
  | Some(actual) => check_exp_equal(name, expected, actual)
  | None => fail(name ++ " did not simplify")
  };

let check_simplifies_at_level = (name, level, input, expected) =>
  switch (
    Web.RewriteChecker.simplify_at_level(~level, ~settings, ~env, input)
  ) {
  | Some(actual) => check_exp_equal(name, expected, actual)
  | None => fail(name ++ " did not simplify")
  };

let write_text_file = (path, contents) => {
  let write_file_sync =
    Js_of_ocaml.Js.Unsafe.js_expr("require('fs').writeFileSync");
  Js_of_ocaml.Js.Unsafe.fun_call(
    write_file_sync,
    [|
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(path)),
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(contents)),
    |],
  )
  |> ignore;
};

let saved = value => Calc.Calculated(value);

let step_model = (~expr, ~step_kind, ~next_step) => {
  Web.StepperBase.expr: saved(expr),
  editor: Calc.Pending,
  step_kind,
  next_step,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
  export_warning: None,
  coq_check_status: Web.StepperBase.CoqCheckIdle,
};

let parens = exp => Language.Exp.fresh(Parens(exp));

let sample_export_chain = () => {
  let x = Exp.var("x");
  let source =
    plus(
      plus(
        plus(
          plus(plus(plus(x, Exp.int(1)), Exp.int(2)), Exp.int(3)),
          Exp.int(45),
        ),
        Exp.var("x"),
      ),
      Exp.var("x"),
    );
  let reparenthesized = parens(source);
  let target = plus(times(Exp.int(3), Exp.var("x")), Exp.int(51));
  let trace =
    switch (
      Web.RewriteChecker.check_written_step_trace_for_profile(
        ~profile=Axioms.math_profile(Arithmetic),
        ~settings,
        ~env,
        reparenthesized,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected sample export chain arithmetic step")
    };
  let final_step =
    step_model(
      ~expr=target,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let written_step =
    step_model(
      ~expr=reparenthesized,
      ~step_kind=
        WrittenStep({
          at_idx: 0,
          at_exp: reparenthesized,
          with_exp: target,
          justification: Web.ProofTrace.trace_summary_label(trace),
          trace_summary: Some(trace),
          next_exp: saved(target),
        }),
      ~next_step=Some(final_step),
    );
  step_model(
    ~expr=source,
    ~step_kind=
      ReparenthesizeStep({
        original_exp: source,
        reparenthesized_exp: reparenthesized,
        selected_id: Some(Language.Exp.rep_id(source)),
        evaluate_after_parenthesize: true,
        next_exp: saved(reparenthesized),
      }),
    ~next_step=Some(written_step),
  );
};

let sample_written_step_export_chain = (~source, ~target, ~trace) => {
  let final_step =
    step_model(
      ~expr=target,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  step_model(
    ~expr=source,
    ~step_kind=
      WrittenStep({
        at_idx: 0,
        at_exp: source,
        with_exp: target,
        justification: Web.ProofTrace.trace_summary_label(trace),
        trace_summary: Some(trace),
        next_exp: saved(target),
      }),
    ~next_step=Some(final_step),
  );
};

let sample_written_steps_export_chain = transitions => {
  let (_, final_exp, _) = ListUtil.last(transitions);
  let terminal =
    step_model(
      ~expr=final_exp,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  transitions
  |> List.rev
  |> List.fold_left(
       (next_step, (source, target, trace)) =>
         step_model(
           ~expr=source,
           ~step_kind=
             WrittenStep({
               at_idx: 0,
               at_exp: source,
               with_exp: target,
               justification: Web.ProofTrace.trace_summary_label(trace),
               trace_summary: Some(trace),
               next_exp: saved(target),
             }),
           ~next_step=Some(next_step),
         ),
       terminal,
     );
};

let sample_foil_cleanup_export_chain = () => {
  let x = Exp.var("x");
  let two_x = times(Exp.int(2), x);
  let x_plus_four = plus(x, Exp.int(4));
  let source = times(minus(two_x, Exp.int(3)), x_plus_four);
  let distributed_once =
    minus(times(two_x, x_plus_four), times(Exp.int(3), x_plus_four));
  let distributed_twice =
    minus(
      plus(
        times(Exp.int(2), power(x, Exp.int(2))),
        times(two_x, Exp.int(4)),
      ),
      times(Exp.int(3), x_plus_four),
    );
  let fully_distributed =
    minus(
      plus(
        times(Exp.int(2), power(x, Exp.int(2))),
        times(two_x, Exp.int(4)),
      ),
      plus(times(Exp.int(3), x), times(Exp.int(3), Exp.int(4))),
    );
  let first_product_folded =
    minus(
      plus(
        times(Exp.int(2), power(x, Exp.int(2))),
        times(Exp.int(8), x),
      ),
      plus(times(Exp.int(3), x), times(Exp.int(3), Exp.int(4))),
    );
  let products_folded =
    minus(
      plus(
        times(Exp.int(2), power(x, Exp.int(2))),
        times(Exp.int(8), x),
      ),
      plus(times(Exp.int(3), x), Exp.int(12)),
    );
  let target =
    minus(
      plus(
        times(Exp.int(2), power(x, Exp.int(2))),
        times(Exp.int(5), x),
      ),
      Exp.int(12),
    );
  let trace = (index, source, target, local_source, local_target) =>
    switch (
      Web.RewriteChecker.check_single_step_trace_at_level(
        ~level=Axioms.Algebra,
        ~settings,
        ~env,
        local_source,
        local_target,
      )
    ) {
    | Some(trace) => {
        ...trace,
        from_normal_exp: target,
        to_normal_exp: target,
        prover_steps:
          trace.prover_steps
          |> List.map((step: Web.ProofTrace.prover_step) =>
               {
                 ...step,
                 before_full_exp: source,
                 after_full_exp: target,
               }
             ),
      }
    | None =>
      fail("expected FOIL cleanup transition " ++ string_of_int(index))
    };
  let second_distribution_local = times(two_x, x_plus_four);
  let second_distribution_target =
    plus(
      times(Exp.int(2), power(x, Exp.int(2))),
      times(two_x, Exp.int(4)),
    );
  let third_distribution_local = times(Exp.int(3), x_plus_four);
  let third_distribution_target =
    plus(times(Exp.int(3), x), times(Exp.int(3), Exp.int(4)));
  let first_product_local = times(two_x, Exp.int(4));
  let first_product_target = times(Exp.int(8), x);
  let second_product_local = times(Exp.int(3), Exp.int(4));
  let second_product_target = Exp.int(12);
  let transitions = [
    (
      source,
      distributed_once,
      trace(1, source, distributed_once, source, distributed_once),
    ),
    (
      distributed_once,
      distributed_twice,
      trace(
        2,
        distributed_once,
        distributed_twice,
        second_distribution_local,
        second_distribution_target,
      ),
    ),
    (
      distributed_twice,
      fully_distributed,
      trace(
        3,
        distributed_twice,
        fully_distributed,
        third_distribution_local,
        third_distribution_target,
      ),
    ),
    (
      fully_distributed,
      first_product_folded,
      trace(
        4,
        fully_distributed,
        first_product_folded,
        first_product_local,
        first_product_target,
      ),
    ),
    (
      first_product_folded,
      products_folded,
      trace(
        5,
        first_product_folded,
        products_folded,
        second_product_local,
        second_product_target,
      ),
    ),
    (
      products_folded,
      target,
      trace(6, products_folded, target, products_folded, target),
    ),
  ];
  sample_written_steps_export_chain(transitions);
};

let sample_reparenthesized_written_step_export_chain =
    (~source, ~reparenthesized, ~target, ~trace) => {
  let written_step =
    sample_written_step_export_chain(
      ~source=reparenthesized,
      ~target,
      ~trace,
    );
  step_model(
    ~expr=source,
    ~step_kind=
      ReparenthesizeStep({
        original_exp: source,
        reparenthesized_exp: reparenthesized,
        selected_id: Some(Language.Exp.rep_id(source)),
        evaluate_after_parenthesize: true,
        next_exp: saved(reparenthesized),
      }),
    ~next_step=Some(written_step),
  );
};

let sample_reparenthesized_trig_export_chain = () => {
  let source = builtin_sin(Exp.var("x"));
  let reparenthesized = parens(source);
  let final_step =
    step_model(
      ~expr=reparenthesized,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  step_model(
    ~expr=source,
    ~step_kind=
      ReparenthesizeStep({
        original_exp: source,
        reparenthesized_exp: reparenthesized,
        selected_id: Some(Language.Exp.rep_id(source)),
        evaluate_after_parenthesize: true,
        next_exp: saved(reparenthesized),
      }),
    ~next_step=Some(final_step),
  );
};

let sample_rocq_algebra_with_var_trig_export_chain = () => {
  let x = Exp.var("x");
  let source =
    plus(
      minus(divide(Exp.int(1), Exp.int(2)), cos(times(Exp.int(2), x))),
      times(
        divide(Exp.int(1), Exp.int(4)),
        plus(Exp.int(1), cos(times(Exp.int(4), x))),
      ),
    );
  let target =
    plus(
      minus(divide(Exp.int(3), Exp.int(4)), cos(times(Exp.int(2), x))),
      times(divide(Exp.int(1), Exp.int(4)), cos(times(Exp.int(4), x))),
    );
  let trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Trigonometry,
        max_depth: 4,
        max_states: 80,
        source,
        target,
      },
    );
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_calculus_export_chain = (~source, ~target) => {
  let trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Calculus,
        max_depth: 4,
        max_states: 80,
        source,
        target,
      },
    );
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let simple_let_program = (bindings, body) =>
  List.fold_right(
    ((name, rhs), body) => Exp.let_(Pat.var(name), rhs, body),
    bindings,
    body,
  );

let written_program_step =
    (
      ~program,
      ~next_program,
      ~local_source,
      ~local_target,
      ~trace,
      ~next_step,
    ) =>
  step_model(
    ~expr=program,
    ~step_kind=
      WrittenStep({
        at_idx: 0,
        at_exp: local_source,
        with_exp: local_target,
        justification: Web.ProofTrace.trace_summary_label(trace),
        trace_summary: Some(trace),
        next_exp: saved(next_program),
      }),
    ~next_step=Some(next_step),
  );

let required_profile_trace = (~profile, source, target) =>
  switch (
    Web.RewriteChecker.check_written_step_trace_for_profile(
      ~profile,
      ~settings,
      ~env,
      source,
      target,
    )
  ) {
  | Some(trace) => trace
  | None => fail("expected a profile-authorized fixture trace")
  };

let sample_linear_let_export_chain = () => {
  let a_source = plus(Exp.int(1), Exp.int(2));
  let a_target = Exp.int(3);
  let b_source = plus(Exp.var("a"), plus(Exp.int(2), Exp.int(3)));
  let b_target = plus(Exp.var("a"), Exp.int(5));
  let body_source = plus(Exp.var("b"), plus(Exp.int(4), Exp.int(5)));
  let body_target = plus(Exp.var("b"), Exp.int(9));
  let initial =
    simple_let_program([("a", a_source), ("b", b_source)], body_source);
  let after_a =
    simple_let_program([("a", a_target), ("b", b_source)], body_source);
  let after_b =
    simple_let_program([("a", a_target), ("b", b_target)], body_source);
  let after_body =
    simple_let_program([("a", a_target), ("b", b_target)], body_target);
  let terminal =
    step_model(
      ~expr=after_body,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let arithmetic = Axioms.math_profile(Arithmetic);
  let body_step =
    written_program_step(
      ~program=after_b,
      ~next_program=after_body,
      ~local_source=body_source,
      ~local_target=body_target,
      ~trace=
        required_profile_trace(~profile=arithmetic, body_source, body_target),
      ~next_step=terminal,
    );
  let b_step =
    written_program_step(
      ~program=after_a,
      ~next_program=after_b,
      ~local_source=b_source,
      ~local_target=b_target,
      ~trace=required_profile_trace(~profile=arithmetic, b_source, b_target),
      ~next_step=body_step,
    );
  written_program_step(
    ~program=initial,
    ~next_program=after_a,
    ~local_source=a_source,
    ~local_target=a_target,
    ~trace=required_profile_trace(~profile=arithmetic, a_source, a_target),
    ~next_step=b_step,
  );
};

let sample_derivative_let_export_chain =
    (
      ~drop_power_rule=false,
      ~atomic_f2_finish=false,
      ~drop_atomic_scalar_rule=false,
      (),
    ) => {
  let x = Exp.var("x");
  let f =
    Exp.fn(
      Pat.var("x"),
      plus(power(x, Exp.int(3)), times(Exp.int(2), x)),
      None,
      None,
    );
  let f1 =
    Exp.fn(
      Pat.var("x"),
      plus(times(Exp.int(3), power(x, Exp.int(2))), Exp.int(2)),
      None,
      None,
    );
  let f2 = Exp.fn(Pat.var("x"), times(Exp.int(6), x), None, None);
  let f1_source = function_derivative(Exp.var("f"));
  let f2_source = function_derivative(Exp.var("f1"));
  let body = Language.Exp.fresh(Ap(Forward, Exp.var("f2"), Exp.int(2)));
  let initial =
    simple_let_program(
      [("f", f), ("f1", f1_source), ("f2", f2_source)],
      body,
    );
  let after_f1 =
    simple_let_program([("f", f), ("f1", f1), ("f2", f2_source)], body);
  let after_f2 =
    simple_let_program([("f", f), ("f1", f1), ("f2", f2)], body);
  let calculus = Axioms.math_profile(Calculus);
  let atomic_trace_rule_allowed = rule_id =>
    rule_id != "arith.affine_normalize"
    && (
      switch (Axioms.cleanup_capability_for_id(rule_id)) {
      | Some(Axioms.AddAssoc | AddComm | ConstFold | CollectLikeTerms) =>
        false
      | _ => true
      }
    );
  let f1_trace =
    Web.RewriteChecker.calculus_check_result_trace_for_profile(
      ~profile=calculus,
      function_derivative(f),
      f1,
    )
    |> Option.get;
  let f1_trace =
    drop_power_rule
      ? {
        ...f1_trace,
        rule_ids:
          f1_trace.rule_ids
          |> List.filter(rule_id => rule_id != "calc.diff_power"),
        prover_steps:
          f1_trace.prover_steps
          |> List.filter((step: Web.ProofTrace.prover_step) =>
               step.rule_id != "calc.diff_power"
             ),
      }
      : f1_trace;
  let f1_trace =
    atomic_f2_finish
      ? {
        ...f1_trace,
        rule_ids: f1_trace.rule_ids |> List.filter(atomic_trace_rule_allowed),
        prover_steps:
          f1_trace.prover_steps
          |> List.filter((step: Web.ProofTrace.prover_step) =>
               atomic_trace_rule_allowed(step.rule_id)
             ),
      }
      : f1_trace;
  let f2_trace =
    Web.RewriteChecker.calculus_check_result_trace_for_profile(
      ~profile=calculus,
      function_derivative(f1),
      f2,
    )
    |> Option.get;
  let f2_trace =
    if (atomic_f2_finish) {
      let scalar_source =
        plus(times(Exp.int(3), times(Exp.int(2), x)), Exp.int(0));
      let scalar_target = plus(times(Exp.int(6), x), Exp.int(0));
      let final_target = times(Exp.int(6), x);
      let atomic_steps = [
        Web.ProofTrace.prover_step_at(
          ~origin=Web.ProofTrace.Normalization,
          ~rule_id="arith.simplify_scalar_products",
          ~before_full_exp=scalar_source,
          ~after_full_exp=scalar_target,
          ~before_exp=scalar_source,
          ~after_exp=scalar_target,
          ~occurrence=1,
          ~detail="scalar/sign normalization",
        ),
        Web.ProofTrace.prover_step_at(
          ~origin=Web.ProofTrace.Normalization,
          ~rule_id="add.identity",
          ~before_full_exp=scalar_target,
          ~after_full_exp=final_target,
          ~before_exp=scalar_target,
          ~after_exp=final_target,
          ~occurrence=1,
          ~detail="bounded axiom search",
        ),
      ];
      let recorded_atomic_steps =
        drop_atomic_scalar_rule
          ? atomic_steps
            |> List.filter((step: Web.ProofTrace.prover_step) =>
                 step.rule_id != "arith.simplify_scalar_products"
               )
          : atomic_steps;
      let recorded_atomic_rule_ids =
        recorded_atomic_steps
        |> List.map((step: Web.ProofTrace.prover_step) => step.rule_id);
      {
        ...f2_trace,
        rule_ids:
          f2_trace.rule_ids
          |> List.filter(atomic_trace_rule_allowed)
          |> List.filter(rule_id =>
               !drop_atomic_scalar_rule
               || rule_id != "arith.simplify_scalar_products"
             )
          |> List.append(recorded_atomic_rule_ids)
          |> Web.RewriteChecker.dedup,
        prover_steps:
          f2_trace.prover_steps
          |> List.filter((step: Web.ProofTrace.prover_step) =>
               atomic_trace_rule_allowed(step.rule_id)
             )
          |> List.append(recorded_atomic_steps),
      };
    } else {
      f2_trace;
    };
  let terminal =
    step_model(
      ~expr=after_f2,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let f2_step =
    written_program_step(
      ~program=after_f1,
      ~next_program=after_f2,
      ~local_source=f2_source,
      ~local_target=f2,
      ~trace=f2_trace,
      ~next_step=terminal,
    );
  written_program_step(
    ~program=initial,
    ~next_program=after_f1,
    ~local_source=f1_source,
    ~local_target=f1,
    ~trace=f1_trace,
    ~next_step=f2_step,
  );
};

let sample_evaluated_derivative_let_export_chain = () => {
  let x = Exp.var("x");
  let f =
    Exp.fn(
      Pat.var("x"),
      plus(power(x, Exp.int(3)), times(Exp.int(2), x)),
      None,
      None,
    );
  let f1 =
    Exp.fn(
      Pat.var("x"),
      plus(times(Exp.int(3), power(x, Exp.int(2))), Exp.int(2)),
      None,
      None,
    );
  let f2 = Exp.fn(Pat.var("x"), times(Exp.int(6), x), None, None);
  let f1_source = function_derivative(Exp.var("f"));
  let f2_source = function_derivative(Exp.var("f1"));
  let body = Language.Exp.fresh(Ap(Forward, Exp.var("f2"), Exp.int(2)));
  let initial =
    simple_let_program(
      [("f", f), ("f1", f1_source), ("f2", f2_source)],
      body,
    );
  let after_f_substitution =
    simple_let_program(
      [("f1", function_derivative(f)), ("f2", f2_source)],
      body,
    );
  let after_f1 = simple_let_program([("f1", f1), ("f2", f2_source)], body);
  let after_f1_substitution =
    simple_let_program([("f2", function_derivative(f1))], body);
  let after_f2 = simple_let_program([("f2", f2)], body);
  let substituted_body = Language.Exp.fresh(Ap(Forward, f2, Exp.int(2)));
  let applied_body = times(Exp.int(6), Exp.int(2));
  let final_body = Exp.int(12);
  let terminal =
    step_model(
      ~expr=final_body,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let arithmetic_step =
    written_program_step(
      ~program=applied_body,
      ~next_program=final_body,
      ~local_source=applied_body,
      ~local_target=final_body,
      ~trace=
        required_profile_trace(
          ~profile=Axioms.math_profile(Arithmetic),
          applied_body,
          final_body,
        ),
      ~next_step=terminal,
    );
  let application_step =
    step_model(
      ~expr=substituted_body,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=Some(arithmetic_step),
    );
  let f2_substitution_step =
    step_model(
      ~expr=after_f2,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=Some(application_step),
    );
  let calculus = Axioms.math_profile(Calculus);
  let f2_step =
    written_program_step(
      ~program=after_f1_substitution,
      ~next_program=after_f2,
      ~local_source=function_derivative(f1),
      ~local_target=f2,
      ~trace=
        Web.RewriteChecker.calculus_check_result_trace_for_profile(
          ~profile=calculus,
          function_derivative(f1),
          f2,
        )
        |> Option.get,
      ~next_step=f2_substitution_step,
    );
  let f1_substitution_step =
    step_model(
      ~expr=after_f1,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=Some(f2_step),
    );
  let f1_step =
    written_program_step(
      ~program=after_f_substitution,
      ~next_program=after_f1,
      ~local_source=function_derivative(f),
      ~local_target=f1,
      ~trace=
        Web.RewriteChecker.calculus_check_result_trace_for_profile(
          ~profile=calculus,
          function_derivative(f),
          f1,
        )
        |> Option.get,
      ~next_step=f1_substitution_step,
    );
  step_model(
    ~expr=initial,
    ~step_kind=MissingStep(Web.MissingStep.Model.init),
    ~next_step=Some(f1_step),
  );
};

let sample_anonymous_derivative_let_export_chain =
    (~drop_power_rule=false, ()) => {
  let x = Exp.var("x");
  let f =
    Exp.fn(
      Pat.var("x"),
      plus(
        plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
        Exp.int(2),
      ),
      None,
      None,
    );
  let f1 =
    Exp.fn(
      Pat.var("x"),
      plus(times(Exp.int(2), x), Exp.int(3)),
      None,
      None,
    );
  let f1_source = function_derivative(f);
  let applied_f = Language.Exp.fresh(Ap(Forward, f, Exp.int(0)));
  let body_source = plus(applied_f, Exp.int(1));
  let body_applied =
    plus(
      plus(
        plus(
          power(Exp.int(0), Exp.int(2)),
          times(Exp.int(3), Exp.int(0)),
        ),
        Exp.int(2),
      ),
      Exp.int(1),
    );
  let body_target = Exp.int(3);
  let initial = simple_let_program([("f1", f1_source)], body_source);
  let after_f1 = simple_let_program([("f1", f1)], body_source);
  let after_application = simple_let_program([("f1", f1)], body_applied);
  let after_body = simple_let_program([("f1", f1)], body_target);
  let terminal =
    step_model(
      ~expr=after_body,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let arithmetic_trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Arithmetic,
        max_depth: 4,
        max_states: 80,
        source: body_applied,
        target: body_target,
      },
    );
  let body_step =
    written_program_step(
      ~program=after_application,
      ~next_program=after_body,
      ~local_source=body_applied,
      ~local_target=body_target,
      ~trace=arithmetic_trace,
      ~next_step=terminal,
    );
  let application_step =
    step_model(
      ~expr=after_f1,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=Some(body_step),
    );
  let f1_trace =
    Web.RewriteChecker.calculus_check_result_trace_for_profile(
      ~profile=Axioms.math_profile(Calculus),
      f1_source,
      f1,
    )
    |> Option.get;
  let f1_trace =
    drop_power_rule
      ? {
        ...f1_trace,
        rule_ids:
          f1_trace.rule_ids
          |> List.filter(rule_id => rule_id != "calc.diff_power"),
        prover_steps:
          f1_trace.prover_steps
          |> List.filter((step: Web.ProofTrace.prover_step) =>
               step.rule_id != "calc.diff_power"
             ),
      }
      : f1_trace;
  written_program_step(
    ~program=initial,
    ~next_program=after_f1,
    ~local_source=f1_source,
    ~local_target=f1,
    ~trace=f1_trace,
    ~next_step=application_step,
  );
};

let sample_trig_taylor_derivative_let_export_chain = () => {
  let t = Exp.var("t");
  let fn = body => Exp.fn(Pat.var("t"), body, None, None);
  let f =
    fn(
      float_plus(
        float_minus(
          float_divide(float(7.0), float(4.0)),
          builtin_cos(float_times(float(2.0), t)),
        ),
        float_times(
          float_divide(float(1.0), float(4.0)),
          builtin_cos(float_times(float(4.0), t)),
        ),
      ),
    );
  let f1 =
    fn(
      float_minus(
        float_times(float(2.0), builtin_sin(float_times(float(2.0), t))),
        builtin_sin(float_times(float(4.0), t)),
      ),
    );
  let f2 =
    fn(
      float_minus(
        float_times(float(4.0), builtin_cos(float_times(float(2.0), t))),
        float_times(float(4.0), builtin_cos(float_times(float(4.0), t))),
      ),
    );
  let f3 =
    fn(
      float_plus(
        float_times(
          float_minus(float(0.0), float(8.0)),
          builtin_sin(float_times(float(2.0), t)),
        ),
        float_times(float(16.0), builtin_sin(float_times(float(4.0), t))),
      ),
    );
  let f1_source = function_derivative(f);
  let f2_source = function_derivative(Exp.var("f1"));
  let f3_source = function_derivative(Exp.var("f2"));
  let body = Language.Exp.fresh(Ap(Forward, Exp.var("f3"), float(0.3)));
  let initial =
    simple_let_program(
      [("f1", f1_source), ("f2", f2_source), ("f3", f3_source)],
      body,
    );
  let after_f1 =
    simple_let_program(
      [("f1", f1), ("f2", f2_source), ("f3", f3_source)],
      body,
    );
  let after_f2 =
    simple_let_program([("f1", f1), ("f2", f2), ("f3", f3_source)], body);
  let after_f3 =
    simple_let_program([("f1", f1), ("f2", f2), ("f3", f3)], body);
  let terminal =
    step_model(
      ~expr=after_f3,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let calculus = Axioms.math_profile(Calculus);
  let trace = (source, target) =>
    Web.RewriteChecker.calculus_check_result_trace_for_profile(
      ~profile=calculus,
      source,
      target,
    )
    |> Option.get;
  let f3_step =
    written_program_step(
      ~program=after_f2,
      ~next_program=after_f3,
      ~local_source=f3_source,
      ~local_target=f3,
      ~trace=trace(function_derivative(f2), f3),
      ~next_step=terminal,
    );
  let f2_step =
    written_program_step(
      ~program=after_f1,
      ~next_program=after_f2,
      ~local_source=f2_source,
      ~local_target=f2,
      ~trace=trace(function_derivative(f1), f2),
      ~next_step=f3_step,
    );
  written_program_step(
    ~program=initial,
    ~next_program=after_f1,
    ~local_source=f1_source,
    ~local_target=f1,
    ~trace=trace(f1_source, f1),
    ~next_step=f2_step,
  );
};

let sample_local_algebra_under_trig_export_chain = () => {
  let x = Exp.var("x");
  let local_source = times(Exp.int(2), times(Exp.int(2), x));
  let local_target = times(Exp.int(4), x);
  let trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Algebra,
        max_depth: 4,
        max_states: 80,
        source: local_source,
        target: local_target,
      },
    );
  let source = plus(cos(local_source), cos(times(Exp.int(2), x)));
  let target = plus(cos(local_target), cos(times(Exp.int(2), x)));
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_local_algebra_under_mul_context_export_chain = () => {
  let x = Exp.var("x");
  let trig_tail =
    plus(
      minus(divide(Exp.int(1), Exp.int(2)), cos(times(Exp.int(2), x))),
      times(
        divide(Exp.int(1), Exp.int(2)),
        power(cos(times(Exp.int(2), x)), Exp.int(2)),
      ),
    );
  let local_source =
    times(times(Exp.int(2), divide(Exp.int(1), Exp.int(2))), trig_tail);
  let local_target = trig_tail;
  let trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Algebra,
        max_depth: 4,
        max_states: 80,
        source: local_source,
        target: local_target,
      },
    );
  let source = plus(Exp.int(1), local_source);
  let target = plus(Exp.int(1), local_target);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_integer_trinomial_square_export_chain = () => {
  let a = Exp.var("a");
  let b = Exp.var("b");
  let c = Exp.var("c");
  let source = power(plus(plus(a, b), c), Exp.int(2));
  let two_times = exp => times(Exp.int(2), exp);
  let target =
    plus(
      plus(
        plus(
          plus(
            plus(times(a, a), two_times(times(a, b))),
            two_times(times(a, c)),
          ),
          times(b, b),
        ),
        two_times(times(b, c)),
      ),
      times(c, c),
    );
  let trace =
    collapsed_macro_summary(
      Web.ProofSearchBackend.{
        backend: JSCoqTacticSearch,
        level: Algebra,
        max_depth: 4,
        max_states: 80,
        source,
        target,
      },
    );
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_one_step_constant_export_chain = () => {
  let source =
    plus(plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)), Exp.int(4));
  let target = plus(plus(Exp.int(1), Exp.int(5)), Exp.int(4));
  let trace =
    switch (
      Web.RewriteChecker.check_single_step_trace_at_level(
        ~level=Axioms.Arithmetic,
        ~settings,
        ~env,
        source,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected tiny one-step arithmetic export trace")
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_check_result_constant_export_chain = () => {
  let source =
    plus(plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)), Exp.int(4));
  let target = Exp.int(10);
  let trace =
    switch (
      Web.RewriteChecker.check_written_step_trace_for_profile(
        ~profile=Axioms.math_profile(Arithmetic),
        ~settings,
        ~env,
        source,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected tiny check-result arithmetic export trace")
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_reverse_constant_export_chain = () => {
  let source =
    plus(plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)), Exp.int(4));
  let target =
    plus(plus(plus(Exp.int(4), Exp.int(3)), Exp.int(2)), Exp.int(1));
  let trace =
    switch (
      Web.RewriteChecker.check_written_step_trace_for_profile(
        ~profile=Axioms.math_profile(Arithmetic),
        ~settings,
        ~env,
        source,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected reverse constant arithmetic export trace")
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_reparenthesized_reverse_constant_export_chain = () => {
  let source =
    plus(plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)), Exp.int(4));
  let reparenthesized = parens(source);
  let target =
    plus(plus(plus(Exp.int(4), Exp.int(3)), Exp.int(2)), Exp.int(1));
  let trace =
    switch (
      Web.RewriteChecker.check_written_step_trace_for_profile(
        ~profile=Axioms.math_profile(Arithmetic),
        ~settings,
        ~env,
        reparenthesized,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected reparenthesized reverse arithmetic export trace")
    };
  sample_reparenthesized_written_step_export_chain(
    ~source,
    ~reparenthesized,
    ~target,
    ~trace,
  );
};

let sample_reparenthesized_affine_variable_export_chain = () => {
  let source =
    plus(
      plus(
        plus(
          plus(plus(Exp.int(1), Exp.int(2)), Exp.var("x")),
          Exp.int(3),
        ),
        Exp.var("x"),
      ),
      Exp.int(4),
    );
  let reparenthesized = parens(source);
  let target = plus(times(Exp.int(2), Exp.var("x")), Exp.int(10));
  let trace =
    switch (
      Web.RewriteChecker.check_written_step_trace_for_profile(
        ~profile=Axioms.math_profile(Arithmetic),
        ~settings,
        ~env,
        reparenthesized,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected reparenthesized affine variable export trace")
    };
  sample_reparenthesized_written_step_export_chain(
    ~source,
    ~reparenthesized,
    ~target,
    ~trace,
  );
};

let sample_algebra_distribution_export_chain = () => {
  let local_source =
    times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
  let local_target =
    plus(
      times(Exp.var("x"), Exp.var("y")),
      times(Exp.var("x"), Exp.var("z")),
    );
  let source = plus(Exp.int(1), local_source);
  let target = plus(Exp.int(1), local_target);
  let trace =
    switch (
      Web.RewriteChecker.check_single_step_trace_at_level(
        ~level=Axioms.Algebra,
        ~settings,
        ~env,
        source,
        target,
      )
    ) {
    | Some(trace) => trace
    | None => fail("expected algebra distribution export trace")
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_axiom_search_distribution_export_chain = () => {
  let source = times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
  let target =
    plus(
      times(Exp.var("x"), Exp.var("y")),
      times(Exp.var("x"), Exp.var("z")),
    );
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Algebra,
        ~max_depth=1,
        ~allowed_rule_ids=["alg.distribute_mul_add"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected axiom search distribution proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_axiom_search_add_reorder_export_chain = () => {
  let source =
    plus(plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)), Exp.int(4));
  let target =
    plus(plus(plus(Exp.int(4), Exp.int(3)), Exp.int(2)), Exp.int(1));
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Arithmetic,
        ~max_depth=4,
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected axiom search addition reorder proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_trig_sin_sum_export_chain = () => {
  let x = Exp.var("x");
  let y = Exp.var("y");
  let source = builtin_sin(plus(x, y));
  let target =
    plus(
      times(builtin_sin(x), builtin_cos(y)),
      times(builtin_cos(x), builtin_sin(y)),
    );
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=1,
        ~allowed_rule_ids=["trig.sin_sum"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected trig sin-sum export proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_trig_sin_double_export_chain = () => {
  let x = Exp.var("x");
  let source = builtin_sin(times(Exp.int(2), x));
  let target = times(times(Exp.int(2), builtin_cos(x)), builtin_sin(x));
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=2,
        ~allowed_rule_ids=["trig.sin_double", "arith.reorder_mul_factors"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected trig sine double-angle export proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_trig_power_split_export_chain = () => {
  let x = Exp.var("x");
  let sin_x = builtin_sin(x);
  let sin_x2 = power(sin_x, Exp.int(2));
  let source =
    plus(Exp.int(1), times(Exp.int(2), power(sin_x, Exp.int(4))));
  let target = plus(Exp.int(1), times(times(Exp.int(2), sin_x2), sin_x2));
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=2,
        ~allowed_rule_ids=["alg.power_add", "arith.mul_assoc"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected trig power split export proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_trig_power_nested_export_chain = () => {
  let x = Exp.var("x");
  let sin_x = builtin_sin(x);
  let sin_x2 = power(sin_x, Exp.int(2));
  let source =
    plus(Exp.int(1), times(Exp.int(2), power(sin_x, Exp.int(4))));
  let target =
    plus(Exp.int(1), times(Exp.int(2), power(sin_x2, Exp.int(2))));
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=1,
        ~allowed_rule_ids=["alg.power_mul"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected trig nested power export proof")
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_trig_rule_export_chain = (~rule_id, ~source, ~target) => {
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=1,
        ~allowed_rule_ids=[rule_id],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected trig export proof for " ++ rule_id)
    };
  let trace = Web.AxiomSearch.trace_summary(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_scalar_product_simplification_export_chain = () => {
  let x = Exp.var("x");
  let source =
    minus(
      times(Exp.int(2), Exp.int(1)),
      times(Exp.int(2), builtin_cos(times(Exp.int(2), x))),
    );
  let target =
    minus(
      Exp.int(2),
      times(Exp.int(2), builtin_cos(times(Exp.int(2), x))),
    );
  let trace =
    Web.ProofTrace.{
      justification: "simplify scalar products",
      group_name: Some("arithmetic"),
      from_normal_exp: source,
      to_normal_exp: target,
      from_rule_ids: ["arith.simplify_scalar_products"],
      to_rule_ids: [],
      rule_ids: ["arith.simplify_scalar_products"],
      prover_steps: [
        prover_step_at(
          ~origin=ManualRewrite,
          ~rule_id="arith.simplify_scalar_products",
          ~before_full_exp=source,
          ~after_full_exp=target,
          ~before_exp=source,
          ~after_exp=target,
          ~occurrence=1,
          ~detail="simplify scalar products",
        ),
      ],
      exportable: true,
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_function_argument_scalar_export_chain = () => {
  let x = Exp.var("x");
  let source =
    plus(
      divide(Exp.int(1), Exp.int(2)),
      app("f", times(Exp.int(3), times(Exp.int(2), x))),
    );
  let target =
    plus(
      divide(Exp.int(1), Exp.int(2)),
      app("f", times(Exp.int(6), x)),
    );
  let result =
    switch (
      Web.AxiomSearch.search(
        ~level=Axioms.Trigonometry,
        ~max_depth=1,
        ~allowed_rule_ids=["arith.simplify_scalar_products"],
        ~log=false,
        source,
        target,
      )
    ) {
    | Some(result) => result
    | None => fail("expected opaque-function argument normalization proof")
    };
  sample_written_step_export_chain(
    ~source,
    ~target,
    ~trace=Web.AxiomSearch.trace_summary(result),
  );
};

let sample_single_algebra_export_chain = (~source, ~target) => {
  let result = require_single_step_result_at_level(Algebra, source, target);
  let trace = Web.RewriteChecker.trace_summary_of_result(result);
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_real_distribution_export_chain = () => {
  let x = Exp.var("x");
  let source = times(Exp.int(2), plus(builtin_cos(x), Exp.int(1)));
  let target =
    plus(
      times(Exp.int(2), builtin_cos(x)),
      times(Exp.int(2), Exp.int(1)),
    );
  let trace =
    Web.ProofTrace.{
      justification: "algebra one step",
      group_name: Some("algebra"),
      from_normal_exp: source,
      to_normal_exp: target,
      from_rule_ids: ["alg.distribute_mul_add"],
      to_rule_ids: [],
      rule_ids: ["alg.distribute_mul_add"],
      prover_steps: [
        prover_step_at(
          ~origin=ManualRewrite,
          ~rule_id="alg.distribute_mul_add",
          ~before_full_exp=source,
          ~after_full_exp=target,
          ~before_exp=source,
          ~after_exp=target,
          ~occurrence=1,
          ~detail="real-domain distribution",
        ),
      ],
      exportable: true,
    };
  sample_written_step_export_chain(~source, ~target, ~trace);
};

let sample_real_distribution_with_cleanup_export_chain = () => {
  let x = Exp.var("x");
  let source = times(Exp.int(2), plus(builtin_cos(x), Exp.int(1)));
  let distributed =
    plus(
      times(Exp.int(2), builtin_cos(x)),
      times(Exp.int(2), Exp.int(1)),
    );
  let target = plus(times(Exp.int(2), builtin_cos(x)), Exp.int(2));
  let trace =
    Web.ProofTrace.{
      justification: "algebra one step",
      group_name: Some("algebra"),
      from_normal_exp: source,
      to_normal_exp: distributed,
      from_rule_ids: ["alg.distribute_mul_add"],
      to_rule_ids: [],
      rule_ids: ["alg.distribute_mul_add"],
      prover_steps: [
        prover_step_at(
          ~origin=ManualRewrite,
          ~rule_id="alg.distribute_mul_add",
          ~before_full_exp=source,
          ~after_full_exp=distributed,
          ~before_exp=source,
          ~after_exp=distributed,
          ~occurrence=1,
          ~detail="real-domain distribution",
        ),
      ],
      exportable: true,
    };
  let final_step =
    step_model(
      ~expr=target,
      ~step_kind=MissingStep(Web.MissingStep.Model.init),
      ~next_step=None,
    );
  let cleanup_step =
    step_model(
      ~expr=distributed,
      ~step_kind=
        AutoSimplifyStep({
          original_exp: distributed,
          simplified_exp: target,
          next_exp: saved(target),
        }),
      ~next_step=Some(final_step),
    );
  step_model(
    ~expr=source,
    ~step_kind=
      WrittenStep({
        at_idx: 0,
        at_exp: source,
        with_exp: distributed,
        justification: Web.ProofTrace.trace_summary_label(trace),
        trace_summary: Some(trace),
        next_exp: saved(distributed),
      }),
    ~next_step=Some(cleanup_step),
  );
};

let tests = (
  "RewriteChecker",
  [
    test_case(
      "rewrite levels inherit through a branching DAG",
      `Quick,
      () => {
        check(
          int,
          "arithmetic rank",
          0,
          Axioms.rewrite_level_rank(Arithmetic),
        );
        check(
          list(string),
          "arithmetic groups",
          ["arithmetic"],
          Axioms.allowed_groups(Arithmetic) |> List.map(rewrite_group_name),
        );
        check(
          list(string),
          "algebra includes arithmetic",
          ["arithmetic", "algebra"],
          Axioms.allowed_groups(Algebra) |> List.map(rewrite_group_name),
        );
        check(
          list(string),
          "calculus inherits the trig branch",
          ["arithmetic", "algebra", "trigonometry", "calculus"],
          Axioms.allowed_groups(Calculus) |> List.map(rewrite_group_name),
        );
        check(
          list(string),
          "functions branch inherits algebra without trig",
          ["arithmetic", "algebra"],
          Axioms.allowed_groups(FunctionsAndLists)
          |> List.map(rewrite_group_name),
        );
        check(
          list(string),
          "calculus ancestor closure",
          ["Arithmetic", "Algebra", "Trigonometry", "Calculus"],
          Axioms.inherited_rewrite_levels(Calculus)
          |> List.map(Axioms.rewrite_level_label),
        );
        check(
          list(string),
          "functions ancestor closure excludes sibling branches",
          ["Arithmetic", "Algebra", "Functions/lists"],
          Axioms.inherited_rewrite_levels(FunctionsAndLists)
          |> List.map(Axioms.rewrite_level_label),
        );
        check(
          bool,
          "calculus inherits trigonometry",
          true,
          Axioms.rewrite_level_inherits(
            ~current_level=Calculus,
            Trigonometry,
          ),
        );
        check(
          bool,
          "functions branch does not inherit trigonometry",
          false,
          Axioms.rewrite_level_inherits(
            ~current_level=FunctionsAndLists,
            Trigonometry,
          ),
        );
        check(
          bool,
          "arithmetic enabled",
          true,
          Axioms.rewrite_level_enabled(Arithmetic),
        );
        check(
          bool,
          "algebra enabled",
          true,
          Axioms.rewrite_level_enabled(Algebra),
        );
        check(
          bool,
          "trigonometry enabled",
          true,
          Axioms.rewrite_level_enabled(Trigonometry),
        );
        check(
          bool,
          "calculus enabled",
          true,
          Axioms.rewrite_level_enabled(Calculus),
        );
        check(
          list(string),
          "disabled internal levels are absent from the selector",
          ["Arithmetic", "Algebra", "Trigonometry", "Calculus"],
          Axioms.selectable_rewrite_levels
          |> List.map(Axioms.rewrite_level_label),
        );
      },
    ),
    test_case(
      "calculus one step applies the power rule",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(power(x, Exp.int(3)), x);
        let target =
          times(times(Exp.int(3), power(x, Exp.int(2))), diff(x, x));
        check_written_at_level(
          "power rule",
          Calculus,
          source,
          target,
          Some("calculus one step"),
        );
      },
    ),
    test_case(
      "calculus one step keeps linearity visible and cleans power output",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(plus(power(x, Exp.int(2)), Exp.int(2)), x);
        let target =
          plus(diff(power(x, Exp.int(2)), x), diff(Exp.int(2), x));
        check_written_at_level(
          "sum rule preserves both derivative branches",
          Calculus,
          source,
          target,
          Some("calculus one step"),
        );
        check_written_at_level(
          "power rule drops exponent one and variable derivative",
          Calculus,
          diff(power(x, Exp.int(2)), x),
          times(Exp.int(2), x),
          Some("calculus one step"),
        );
      },
    ),
    test_case(
      "calculus one step distributes linearity across complete operator chains",
      `Quick,
      () => {
        let x = Exp.var("x");
        let squared = power(x, Exp.int(2));
        let three_x = times(Exp.int(3), x);
        let source =
          diff(
            plus(plus(plus(squared, three_x), Exp.int(5)), three_x),
            x,
          );
        let target =
          plus(
            plus(
              plus(diff(squared, x), diff(three_x, x)),
              diff(Exp.int(5), x),
            ),
            diff(three_x, x),
          );
        check_written_at_level(
          "sum rule expands every term",
          Calculus,
          source,
          target,
          Some("calculus one step"),
        );
        let right_associated_source =
          diff(plus(squared, plus(three_x, Exp.int(5))), x);
        let right_associated_target =
          plus(
            diff(squared, x),
            plus(diff(three_x, x), diff(Exp.int(5), x)),
          );
        check_written_at_level(
          "sum rule handles right-associated terms",
          Calculus,
          right_associated_source,
          right_associated_target,
          Some("calculus one step"),
        );
        let difference_source =
          diff(minus(minus(squared, three_x), Exp.int(5)), x);
        let difference_target =
          minus(
            minus(diff(squared, x), diff(three_x, x)),
            diff(Exp.int(5), x),
          );
        check_written_at_level(
          "difference rule expands every term",
          Calculus,
          difference_source,
          difference_target,
          Some("calculus one step"),
        );
      },
    ),
    test_case(
      "calculus one-step cleanup follows profile toggles",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(plus(power(x, Exp.int(2)), Exp.int(2)), x);
        let raw_target =
          plus(diff(power(x, Exp.int(2)), x), diff(Exp.int(2), x));
        let cleaned_target = diff(power(x, Exp.int(2)), x);
        let profile =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=[Axioms.AddIdentity, Axioms.MulIdentity],
            Axioms.math_profile(Calculus),
          );
        check(
          bool,
          "raw calculus step remains available",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            raw_target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "disabled derivative cleanup does not disappear",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            cleaned_target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "calculus profile can disable the power rule",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(power(x, Exp.int(3)), x);
        let target =
          times(times(Exp.int(3), power(x, Exp.int(2))), diff(x, x));
        let profile = Axioms.math_profile(Calculus);
        let profile = {
          ...profile,
          step_policy: {
            ...profile.step_policy,
            visible_rules:
              profile.step_policy.visible_rules
              |> List.filter((rule: Axioms.visible_rule_policy) =>
                   rule.rule_id != "calc.diff_power"
                 ),
          },
        };
        check(
          bool,
          "disabled power rule is rejected",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_none,
        );
        check(
          bool,
          "automatic differentiation preserves disabled power",
          true,
          switch (
            Web.RewriteChecker.simplify_for_profile(
              ~profile,
              ~settings,
              ~env,
              source,
            )
          ) {
          | None => true
          | Some(result) => Web.DifferentiationRewrite.contains_diff(result)
          },
        );
      },
    ),
    test_case(
      "calculus normalization composes product power and sine chain rules",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(times(power(x, Exp.int(3)), builtin_sin(x)), x);
        let normalized =
          Web.DifferentiationRewrite.normalize(
            ~rule_enabled=_ => true,
            source,
          );
        let rule_ids =
          normalized.steps
          |> List.map((step: Web.TrigRewrite.rewrite) => step.rule_id);
        check(bool, "normalization completes", true, normalized.complete);
        check(
          bool,
          "normalization removes diff",
          false,
          Web.DifferentiationRewrite.contains_diff(normalized.exp),
        );
        [
          "calc.diff_product",
          "calc.diff_power",
          "calc.diff_chain_sin",
          "calc.diff_variable",
        ]
        |> List.iter(rule_id =>
             check(
               bool,
               "normalization records " ++ rule_id,
               true,
               List.mem(rule_id, rule_ids),
             )
           );
      },
    ),
    test_case(
      "calculus accepts named function differentiation syntax",
      `Quick,
      () => {
        let x = Exp.var("x");
        let body = power(x, Exp.int(2));
        let named_function = Exp.fn(Pat.var("x"), body, None, Some("f"));
        check_written_at_level(
          "named function body",
          Calculus,
          diff(named_function, x),
          diff(body, x),
          Some("calculus one step"),
        );
      },
    ),
    test_case(
      "calculus accepts parsed named function differentiation syntax",
      `Quick,
      () => {
        let source =
          switch (
            Haz3lcore.Parser.to_term("diff(fun f(x) -> x**2, x)", ~root=Exp)
          ) {
          | Some(source) => source
          | None => fail("expected named derivative syntax to parse")
          };
        let normalized =
          Web.DifferentiationRewrite.normalize(
            ~rule_enabled=_ => true,
            source,
          );
        check(bool, "normalization completes", true, normalized.complete);
        check(
          bool,
          "normalization removes diff",
          false,
          Web.DifferentiationRewrite.contains_diff(normalized.exp),
        );
      },
    ),
    test_case(
      "calculus differentiates a function value without freeing its binder",
      `Quick,
      () => {
        let x = Exp.var("x");
        let body = power(x, Exp.int(2));
        let function_ = Exp.fn(Pat.var("x"), body, None, Some("f"));
        let source = function_diff(function_);
        let lifted = Exp.fn(Pat.var("x"), diff(body, x), None, Some("f"));
        check_written_at_level(
          "binder-preserving function derivative",
          Calculus,
          source,
          lifted,
          Some("calculus one step"),
        );
        let normalized =
          Web.DifferentiationRewrite.normalize(
            ~rule_enabled=_ => true,
            source,
          );
        check(bool, "normalization completes", true, normalized.complete);
        check(
          bool,
          "normalization removes diff inside function",
          false,
          Web.DifferentiationRewrite.contains_diff(normalized.exp),
        );
        let expected =
          Exp.fn(
            Pat.var("x"),
            times(times(Exp.int(2), power(x, Exp.int(1))), Exp.int(1)),
            None,
            Some("f"),
          );
        check_exp_equal(
          "the derivative remains a function",
          expected,
          normalized.exp,
        );
        let applied_source =
          Language.Exp.fresh(Ap(Operators.Forward, source, Exp.int(5)));
        switch (
          Web.DifferentiationRewrite.rewrite_first(
            ~rule_enabled=_ => true,
            applied_source,
          )
        ) {
        | Some((result, step)) =>
          check(
            string,
            "embedded application uses the function derivative rule",
            "calc.diff_function_value",
            step.rule_id,
          );
          check_exp_equal(
            "the derivative function remains callable",
            Language.Exp.fresh(Ap(Operators.Forward, lifted, Exp.int(5))),
            result,
          );
        | None => fail("expected differentiation inside function application")
        };
      },
    ),
    test_case(
      "function-valued differentiation respects calculus profile rules",
      `Quick,
      () => {
        let x = Exp.var("x");
        let body = power(x, Exp.int(2));
        let source =
          function_diff(Exp.fn(Pat.var("x"), body, None, Some("f")));
        let target =
          Exp.fn(Pat.var("x"), times(Exp.int(2), x), None, Some("f"));
        let profile = Axioms.math_profile(Calculus);
        let trace =
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            source,
            target,
          );
        check(
          bool,
          "enabled function and power rules certify the result",
          true,
          Option.is_some(trace),
        );
        let disabled_profile = {
          ...profile,
          step_policy: {
            ...profile.step_policy,
            visible_rules:
              profile.step_policy.visible_rules
              |> List.filter((rule: Axioms.visible_rule_policy) =>
                   rule.rule_id != "calc.diff_power"
                 ),
          },
        };
        check(
          bool,
          "disabled power rule rejects the finished derivative function",
          true,
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile=disabled_profile,
            source,
            target,
          )
          |> Option.is_none,
        );
        let without_cleanup = capability =>
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(candidate => candidate != capability),
            profile,
          );
        check(
          bool,
          "disabled automatic derivative cleanup preserves visible rules",
          true,
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile=without_cleanup(Axioms.DerivativeBasics),
            source,
            target,
          )
          |> Option.is_some,
        );
        let without_variable =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_variable",
            without_cleanup(Axioms.DerivativeBasics),
          );
        check(
          bool,
          "disabled visible variable rule rejects the finished derivative",
          true,
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile=without_variable,
            source,
            target,
          )
          |> Option.is_none,
        );
        check(
          bool,
          "disabled identity-power cleanup rejects x**1 removal",
          true,
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile=without_cleanup(Axioms.PowerIdentity),
            source,
            target,
          )
          |> Option.is_none,
        );
        switch (
          Web.RewriteChecker.simplify_for_profile(
            ~profile=disabled_profile,
            ~settings,
            ~env,
            source,
          )
        ) {
        | Some(result) =>
          check(
            bool,
            "auto simplify retains the unresolved derivative",
            true,
            Web.DifferentiationRewrite.contains_diff(result),
          )
        | None => ()
        };
      },
    ),
    test_case(
      "function-valued Check Result covers product trig and quotient rules",
      `Quick,
      () => {
        let x = Exp.var("x");
        let function_ = body => Exp.fn(Pat.var("x"), body, None, Some("f"));
        let profile = Axioms.math_profile(Calculus);
        let accepts = (profile, body, derivative) =>
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            function_diff(function_(body)),
            function_(derivative),
          )
          |> Option.is_some;
        let product_body =
          times(plus(x, Exp.int(1)), minus(x, Exp.int(2)));
        let product_derivative = minus(times(Exp.int(2), x), Exp.int(1));
        check(
          bool,
          "product function is accepted",
          true,
          accepts(profile, product_body, product_derivative),
        );
        let without_product =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_product",
            profile,
          );
        check(
          bool,
          "disabled product rule rejects the same result",
          false,
          accepts(without_product, product_body, product_derivative),
        );
        let trig_body = power(builtin_sin(x), Exp.int(2));
        let trig_derivative =
          times(times(Exp.int(2), builtin_sin(x)), cos(x));
        check(
          bool,
          "trig function is accepted",
          true,
          accepts(profile, trig_body, trig_derivative),
        );
        let without_sine_chain =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_chain_sin",
            profile,
          );
        check(
          bool,
          "disabled sine chain rule rejects the same result",
          false,
          accepts(without_sine_chain, trig_body, trig_derivative),
        );
        let denominator = plus(x, Exp.int(1));
        let quotient_body = divide(Exp.int(1), denominator);
        let quotient_derivative =
          divide(negate(Exp.int(1)), power(denominator, Exp.int(2)));
        check(
          bool,
          "quotient function is accepted",
          true,
          accepts(profile, quotient_body, quotient_derivative),
        );
        let without_quotient =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_quotient",
            profile,
          );
        check(
          bool,
          "disabled quotient rule rejects the same result",
          false,
          accepts(without_quotient, quotient_body, quotient_derivative),
        );
        let quotient_export =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile,
            function_diff(function_(quotient_body)),
            function_(quotient_derivative),
          )
          |> Option.get;
        write_text_file(
          "/tmp/hazel_function_derivative_quotient_export.v",
          quotient_export,
        );
        check(
          bool,
          "quotient export records the nonzero denominator hypothesis",
          true,
          string_contains("(x + 1) <> 0 ->", quotient_export),
        );
      },
    ),
    test_case(
      "function-valued differentiation preserves closure variables and shadowing",
      `Quick,
      () => {
        let x = Exp.var("x");
        let a = Exp.var("a");
        let source =
          function_diff(
            Exp.fn(Pat.var("x"), times(a, x), None, Some("scaled")),
          );
        let normalized =
          Web.DifferentiationRewrite.normalize(
            ~rule_enabled=_ => true,
            source,
          );
        let expected =
          Exp.fn(
            Pat.var("x"),
            plus(times(Exp.int(0), x), times(a, Exp.int(1))),
            None,
            Some("scaled"),
          );
        check_exp_equal(
          "free coefficient remains free under the original binder",
          expected,
          normalized.exp,
        );
        let nested_identity = Exp.fn(Pat.var("x"), x, None, Some("inner"));
        let nested_body =
          Language.Exp.fresh(Ap(Operators.Forward, nested_identity, x));
        let nested_source =
          function_diff(
            Exp.fn(Pat.var("x"), nested_body, None, Some("outer")),
          );
        let lifted =
          Web.DifferentiationRewrite.rewrite_first(
            ~rule_enabled=_ => true,
            nested_source,
          );
        switch (lifted) {
        | Some((result, _)) =>
          let expected =
            Exp.fn(
              Pat.var("x"),
              diff(nested_body, x),
              None,
              Some("outer"),
            );
          check_exp_equal(
            "nested shadowing is untouched by function lifting",
            expected,
            result,
          );
        | None => fail("expected function-valued differentiation step")
        };
      },
    ),
    test_case(
      "Rocq export certifies a function-valued derivative pointwise",
      `Quick,
      () => {
        let x = Exp.var("x");
        let body =
          plus(
            plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
            Exp.int(5),
          );
        let source =
          function_diff(Exp.fn(Pat.var("x"), body, None, Some("f")));
        let target =
          Exp.fn(
            Pat.var("x"),
            plus(times(Exp.int(2), x), Exp.int(3)),
            None,
            Some("f"),
          );
        let export =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            source,
            target,
          )
          |> Option.get;
        write_text_file("/tmp/hazel_function_derivative_export.v", export);
        check(
          bool,
          "exports a pointwise real derivative theorem",
          true,
          string_contains("forall x : R", export)
          && string_contains("derivable_pt_lim (fun x : R =>", export),
        );
        check(
          bool,
          "exports the simplified derivative body",
          true,
          string_contains("((2 * x) + 3)", export),
        );
        check(
          bool,
          "does not serialize a Hazel function equality",
          false,
          string_contains("unsupported Coq real export term", export),
        );

        let cubic_body = plus(power(x, Exp.int(3)), times(Exp.int(2), x));
        let cubic_source = diff(cubic_body, x);
        let cubic_target =
          plus(times(Exp.int(3), power(x, Exp.int(2))), Exp.int(2));
        let cubic_export =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            cubic_source,
            cubic_target,
          )
          |> Option.get;
        write_text_file(
          "/tmp/hazel_function_derivative_cubic_export.v",
          cubic_export,
        );
        check(
          bool,
          "higher powers reconcile Rocq's INR/pred derivative form",
          true,
          string_contains("derivable_pt_lim_pow", cubic_export)
          && string_contains(
               "replace ((3 * Rsqr (x))) with (INR 3",
               cubic_export,
             ),
        );

        let shifted = plus(x, Exp.int(1));
        let shifted_quartic_source = diff(power(shifted, Exp.int(4)), x);
        let shifted_quartic_target =
          times(Exp.int(4), power(shifted, Exp.int(3)));
        let shifted_quartic_export =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            shifted_quartic_source,
            shifted_quartic_target,
          )
          |> Option.get;
        write_text_file(
          "/tmp/hazel_function_derivative_shifted_quartic_export.v",
          shifted_quartic_export,
        );
        check(
          bool,
          "the same certificate handles a shifted quartic",
          true,
          string_contains("derivable_pt_lim_pow", shifted_quartic_export)
          && string_contains("INR 4", shifted_quartic_export),
        );

        let profile_without_power =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_power",
            Axioms.math_profile(Calculus),
          );
        check(
          bool,
          "a disabled power rule still blocks the cubic certificate",
          true,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=profile_without_power,
            cubic_source,
            cubic_target,
          )
          |> Option.is_none,
        );
        check(
          bool,
          "an incorrect cubic derivative is still rejected",
          true,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            cubic_source,
            plus(times(Exp.int(3), power(x, Exp.int(2))), Exp.int(3)),
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "calculus auto simplify uses explicit profile cleanup",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(power(x, Exp.int(2)), x);
        let target = times(Exp.int(2), x);
        switch (
          Web.RewriteChecker.simplify_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            ~settings,
            ~env,
            source,
          )
        ) {
        | Some(result) => check_exp_equal("clean derivative", target, result)
        | None => fail("expected a derivative result")
        };
      },
    ),
    test_case(
      "calculus cleanup simplifies basic derivatives and identity powers",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let profile = Axioms.math_profile(Calculus);
        let simplify = source =>
          Web.RewriteChecker.simplify_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
          );
        [
          ("same variable derivative", diff(x, x), Exp.int(1)),
          ("numeric constant derivative", diff(Exp.int(7), x), Exp.int(0)),
          ("independent variable derivative", diff(y, x), Exp.int(0)),
          ("power one", power(x, Exp.int(1)), x),
          (
            "power zero",
            power(plus(x, Exp.int(2)), Exp.int(0)),
            Exp.int(1),
          ),
        ]
        |> List.iter(((label, source, expected)) =>
             switch (simplify(source)) {
             | Some(result) => check_exp_equal(label, expected, result)
             | None => fail("expected cleanup for " ++ label)
             }
           );
      },
    ),
    test_case(
      "calculus cleanup leaves non-basic forms alone",
      `Quick,
      () => {
        let x = Exp.var("x");
        let cleanup_enabled = capability =>
          capability == Axioms.DerivativeBasics
          || capability == Axioms.PowerIdentity;
        let dependent_derivative = diff(plus(x, Exp.int(1)), x);
        let nonidentity_power = power(x, Exp.int(2));
        check_exp_equal(
          "dependent derivative",
          dependent_derivative,
          Web.DifferentiationRewrite.cleanup(
            ~cleanup_enabled,
            dependent_derivative,
          ),
        );
        check_exp_equal(
          "nonidentity power",
          nonidentity_power,
          Web.DifferentiationRewrite.cleanup(
            ~cleanup_enabled,
            nonidentity_power,
          ),
        );
      },
    ),
    test_case(
      "calculus profile can disable basic cleanup capabilities",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let profile = {
          ...profile,
          step_policy: {
            ...profile.step_policy,
            default_cleanup:
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.DerivativeBasics
                   && capability != Axioms.PowerIdentity
                 ),
          },
        };
        let cleanup_enabled = capability =>
          List.mem(capability, profile.step_policy.default_cleanup);
        let variable_derivative = diff(x, x);
        let identity_power = power(x, Exp.int(1));
        check_exp_equal(
          "disabled derivative cleanup",
          variable_derivative,
          Web.DifferentiationRewrite.cleanup(
            ~cleanup_enabled,
            variable_derivative,
          ),
        );
        check_exp_equal(
          "disabled power cleanup",
          identity_power,
          Web.DifferentiationRewrite.cleanup(
            ~cleanup_enabled,
            identity_power,
          ),
        );
      },
    ),
    test_case(
      "calculus check result emits a derivative proposition certificate",
      `Quick,
      () => {
        let x = Exp.var("x");
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source: diff(power(x, Exp.int(2)), x),
            target: times(Exp.int(2), x),
          };
        let coq = Web.ProofSearchBackend.rocq_search_program(request);
        write_text_file("/tmp/hazel_stepper_rocq_derivative_power.v", coq);
        check(
          bool,
          "certificate states differentiability",
          true,
          string_contains("derivable_pt_lim (fun x : R =>", coq),
        );
        check(
          bool,
          "certificate uses the standard square lemma",
          true,
          string_contains("apply derivable_pt_lim_Rsqr", coq),
        );
        check(
          bool,
          "certificate does not use broad ring automation",
          false,
          string_contains("ring", coq),
        );
      },
    ),
    test_case(
      "calculus auto simplify reuses profile derivative certificates",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let auto_program = (source, target) =>
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile,
            ~purpose=AutoSimplify,
            request(source, target),
          );
        let polynomial_source =
          diff(
            plus(
              plus(
                times(Exp.int(2), power(x, Exp.int(2))),
                times(Exp.int(4), x),
              ),
              Exp.int(-2),
            ),
            x,
          );
        let polynomial_target =
          Web.RewriteChecker.simplify_for_profile(
            ~profile,
            ~settings,
            ~env,
            polynomial_source,
          )
          |> Option.value(~default=Exp.int(0));
        let polynomial_coq =
          auto_program(polynomial_source, polynomial_target);
        check(
          bool,
          "polynomial auto result gets a derivative certificate",
          true,
          string_contains(
            "Hazel profile-directed derivative certificate",
            polynomial_coq,
          ),
        );
        let product_source = diff(times(x, builtin_sin(x)), x);
        let product_target =
          Web.RewriteChecker.simplify_for_profile(
            ~profile,
            ~settings,
            ~env,
            product_source,
          )
          |> Option.value(~default=Exp.int(0));
        let product_coq = auto_program(product_source, product_target);
        check(
          bool,
          "structurally different product auto result is certified",
          true,
          string_contains(
            "Hazel profile-directed derivative certificate",
            product_coq,
          ),
        );
        let without_power =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_power",
            profile,
          );
        check_raises(
          "disabled derivative rule remains rejected",
          Failure(
            "the active calculus profile cannot certify this derivative candidate",
          ),
          () =>
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=without_power,
            ~purpose=AutoSimplify,
            request(diff(power(x, Exp.int(2)), x), times(Exp.int(2), x)),
          )
          |> ignore
        );
      },
    ),
    test_case(
      "calculus derivative cleanup does not rewrite constants in the function",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(times(Exp.int(6), x), x);
        let target = Exp.int(6);
        let coq =
          Web.ProofSearchBackend.calculus_export_program(source, target)
          |> Option.value(~default="");
        write_text_file(
          "/tmp/hazel_stepper_rocq_derivative_constant_product.v",
          coq,
        );
        check(
          bool,
          "cleanup uses scoped equality transport",
          true,
          string_contains("@eq_ind R", coq)
          && string_contains("H_hazel_cleanup", coq)
          && !string_contains("replace (6) with", coq),
        );
      },
    ),
    test_case(
      "calculus certifies one derivative inside an arithmetic context",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let certificate = (source, target) =>
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile,
            source,
            target,
          );
        let right_source =
          plus(times(Exp.int(2), x), diff(times(Exp.int(6), x), x));
        let right_target = plus(times(Exp.int(2), x), Exp.int(6));
        let left_source = plus(diff(power(x, Exp.int(2)), x), Exp.int(5));
        let left_target = plus(times(Exp.int(2), x), Exp.int(5));
        [
          (
            "right derivative context",
            "/tmp/hazel_stepper_rocq_derivative_right_context.v",
            right_source,
            right_target,
          ),
          (
            "left derivative context",
            "/tmp/hazel_stepper_rocq_derivative_left_context.v",
            left_source,
            left_target,
          ),
        ]
        |> List.iter(((label, path, source, target)) =>
             switch (certificate(source, target)) {
             | Some(program) =>
               write_text_file(path, program);
               check(
                 bool,
                 label,
                 true,
                 string_contains(
                   "Hazel profile-directed derivative certificate",
                   program,
                 ),
               );
             | None => fail(label ++ " should produce a certificate")
             }
           );
        check(
          bool,
          "wrong contextual target is rejected",
          true,
          certificate(
            right_source,
            plus(times(Exp.int(2), x), Exp.int(7)),
          )
          |> Option.is_none,
        );
        let product_disabled = {
          ...profile,
          step_policy: {
            ...profile.step_policy,
            visible_rules:
              profile.step_policy.visible_rules
              |> List.filter((rule: Axioms.visible_rule_policy) =>
                   rule.rule_id != "calc.diff_product"
                 ),
          },
        };
        check(
          bool,
          "profile-disabled product rule is rejected in context",
          true,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=product_disabled,
            right_source,
            right_target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "calculus emits compositional certificate fixtures",
      `Quick,
      () => {
        let x = Exp.var("x");
        let emit = (name, source, target) => {
          let coq =
            Web.ProofSearchBackend.rocq_search_program({
              backend: JSCoqTacticSearch,
              level: Calculus,
              max_depth: 4,
              max_states: 80,
              source,
              target,
            });
          write_text_file(
            "/tmp/hazel_stepper_rocq_derivative_" ++ name ++ ".v",
            coq,
          );
          coq;
        };
        let product_coq =
          emit(
            "product",
            diff(times(x, builtin_sin(x)), x),
            plus(builtin_sin(x), times(x, builtin_cos(x))),
          );
        check(
          bool,
          "product certificate uses the product lemma",
          true,
          string_contains("derivable_pt_lim_mult", product_coq),
        );
        let sin_squared_coq =
          emit(
            "sin_squared",
            diff(power(builtin_sin(x), Exp.int(2)), x),
            times(times(Exp.int(2), builtin_sin(x)), builtin_cos(x)),
          );
        check(
          bool,
          "sine-squared certificate imports trig definitions",
          true,
          string_contains("derivable_pt_lim_Rsqr", sin_squared_coq)
          && string_contains("derivable_pt_lim_sin", sin_squared_coq)
          && string_contains("Rtrigo1", sin_squared_coq),
        );
        let chain_coq =
          emit(
            "sin_chain",
            diff(builtin_sin(power(x, Exp.int(2))), x),
            times(
              builtin_cos(power(x, Exp.int(2))),
              times(Exp.int(2), x),
            ),
          );
        check(
          bool,
          "chain certificate uses sine and composition lemmas",
          true,
          string_contains("derivable_pt_lim_sin", chain_coq)
          && string_contains("derivable_pt_lim_comp", chain_coq)
          && string_contains("Rtrigo1", chain_coq),
        );
        let cos_chain_coq =
          emit(
            "cos_chain",
            diff(builtin_cos(power(x, Exp.int(2))), x),
            times(
              negate(builtin_sin(power(x, Exp.int(2)))),
              times(Exp.int(2), x),
            ),
          );
        check(
          bool,
          "cosine chain certificate imports and uses trig derivatives",
          true,
          string_contains("derivable_pt_lim_cos", cos_chain_coq)
          && string_contains("derivable_pt_lim_comp", cos_chain_coq)
          && string_contains("Rtrigo1", cos_chain_coq),
        );
        let denominator = plus(x, Exp.int(1));
        let quotient_coq =
          emit(
            "quotient",
            diff(divide(x, denominator), x),
            divide(minus(denominator, x), power(denominator, Exp.int(2))),
          );
        check(
          bool,
          "quotient certificate exposes its nonzero hypothesis",
          true,
          string_contains("(x + 1) <> 0 ->", quotient_coq),
        );
        let named_function =
          Exp.fn(Pat.var("x"), power(x, Exp.int(2)), None, Some("f"));
        ignore(
          emit(
            "named_function",
            diff(named_function, x),
            times(Exp.int(2), x),
          ),
        );
      },
    ),
    test_case(
      "math profiles parameterize Rocq tactic search",
      `Quick,
      () => {
        let arithmetic_profile = Axioms.math_profile(Arithmetic);
        check(
          string,
          "arithmetic macro rule",
          "rocq.arithmetic_tactic_search",
          arithmetic_profile.Axioms.rocq_macro_rule_id,
        );
        check(
          string,
          "arithmetic tactic group",
          "hazel_arithmetic",
          arithmetic_profile.rocq_tactic_group,
        );
        check(
          list(string),
          "arithmetic profile groups",
          ["arithmetic"],
          arithmetic_profile.groups |> List.map(rewrite_group_name),
        );
        check(
          string,
          "arithmetic distribution policy",
          "strict_distributed_form",
          Axioms.distribution_step_policy_label(
            arithmetic_profile.one_step_policy.distribution_step_policy,
          ),
        );
        check(
          bool,
          "arithmetic polynomial expansion disabled",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(arithmetic_profile, Manual),
            "alg.expand_polynomial",
          ),
        );
        switch (
          Axioms.visible_rule_policy_for_rule(
            arithmetic_profile.step_policy,
            "arith.mul_const",
          )
        ) {
        | Some(rule_policy) =>
          check(
            list(string),
            "arithmetic distribution cleanup",
            ["add.assoc", "mul.assoc"],
            rule_policy.allowed_cleanup |> List.map(cleanup_capability_label),
          )
        | None => fail("expected arithmetic distribution rule policy")
        };
        check(
          string,
          "arithmetic tactic plan",
          "hazel_arithmetic_plan",
          arithmetic_profile.rocq_tactic_plan.id,
        );
        check(
          list(string),
          "arithmetic tactic plan purposes",
          [
            "validate_primitive_step",
            "validate_macro_step",
            "check_result",
            "auto_simplify",
          ],
          arithmetic_profile.rocq_tactic_plans
          |> List.map(rocq_tactic_plan_purpose_label),
        );
        check(
          string,
          "arithmetic primitive tactic plan",
          "hazel_arithmetic_primitive_plan",
          Axioms.rocq_tactic_plan_for_profile(
            arithmetic_profile,
            ValidatePrimitiveStep,
          ).
            id,
        );
        check(
          list(string),
          "arithmetic tactic step ids",
          ["arith_power_normalize", "arith_mul_reorder", "arith_finish"],
          arithmetic_profile.rocq_tactic_plan.steps
          |> List.map(rocq_tactic_step_id),
        );

        let algebra_profile = Axioms.math_profile(Algebra);
        check(
          string,
          "algebra macro rule",
          "rocq.algebra_tactic_search",
          algebra_profile.rocq_macro_rule_id,
        );
        check(
          string,
          "algebra tactic group",
          "hazel_algebra",
          algebra_profile.rocq_tactic_group,
        );
        check(
          list(string),
          "algebra profile groups",
          ["arithmetic", "algebra"],
          algebra_profile.groups |> List.map(rewrite_group_name),
        );
        check(
          string,
          "algebra distribution policy",
          "strict_distributed_form",
          Axioms.distribution_step_policy_label(
            algebra_profile.one_step_policy.distribution_step_policy,
          ),
        );
        check(
          bool,
          "algebra polynomial expansion enabled",
          true,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(algebra_profile, Manual),
            "alg.expand_polynomial",
          ),
        );
        switch (
          Axioms.visible_rule_policy_for_rule(
            algebra_profile.step_policy,
            "alg.distribute_mul_add",
          )
        ) {
        | Some(rule_policy) =>
          check(
            list(string),
            "algebra distribution cleanup",
            [
              "add.assoc",
              "add.comm",
              "mul.assoc",
              "mul.comm",
              "power.notation",
            ],
            rule_policy.allowed_cleanup |> List.map(cleanup_capability_label),
          )
        | None => fail("expected algebra distribution rule policy")
        };
        check(
          string,
          "algebra tactic plan",
          "hazel_algebra_plan",
          algebra_profile.rocq_tactic_plan.id,
        );
        check(
          list(string),
          "algebra tactic step modes",
          [
            "try_once",
            "try_once",
            "finish_only",
            "try_once",
            "try_once",
            "try_once",
            "finish_only",
          ],
          algebra_profile.rocq_tactic_plan.steps
          |> List.map(rocq_tactic_step_mode),
        );
        check(
          list(string),
          "algebra primitive tactic step modes",
          ["try_once", "once", "try_once", "once"],
          Axioms.rocq_tactic_plan_for_profile(
            algebra_profile,
            ValidatePrimitiveStep,
          ).
            steps
          |> List.map(rocq_tactic_step_mode),
        );
        check(
          string,
          "algebra macro tactic plan",
          "hazel_algebra_macro_plan",
          Axioms.rocq_tactic_plan_for_profile(
            algebra_profile,
            ValidateMacroStep,
          ).
            id,
        );

        let trig_profile = Axioms.math_profile(Trigonometry);
        check(
          string,
          "trig macro rule",
          "rocq.trigonometry_tactic_search",
          trig_profile.rocq_macro_rule_id,
        );
        check(
          string,
          "trig tactic group",
          "hazel_trigonometry",
          trig_profile.rocq_tactic_group,
        );
        check(
          string,
          "trig tactic plan",
          "hazel_trigonometry_plan",
          trig_profile.rocq_tactic_plan.id,
        );
        check(
          string,
          "trig auto simplify tactic plan",
          "hazel_trigonometry_plan_auto_simplify",
          Axioms.rocq_tactic_plan_for_profile(trig_profile, AutoSimplify).id,
        );
        let calculus_profile = Axioms.math_profile(Calculus);
        check(
          bool,
          "calculus exposes inherited trig rules",
          true,
          Axioms.visible_rule_enabled(
            calculus_profile.step_policy,
            "trig.sin_sum",
          ),
        );
        check(
          bool,
          "calculus exposes inherited algebra rules",
          true,
          Axioms.visible_rule_enabled(
            calculus_profile.step_policy,
            "alg.distribute_mul_add",
          ),
        );
        let functions_profile = Axioms.math_profile(FunctionsAndLists);
        check(
          bool,
          "functions branch exposes inherited algebra rules",
          true,
          Axioms.visible_rule_enabled(
            functions_profile.step_policy,
            "alg.distribute_mul_add",
          ),
        );
        check(
          bool,
          "functions branch excludes sibling trig rules",
          false,
          Axioms.visible_rule_enabled(
            functions_profile.step_policy,
            "trig.sin_sum",
          ),
        );
        check(
          bool,
          "calculus cleanup inherits algebra power cleanup",
          true,
          List.mem(
            Axioms.PowerIdentity,
            calculus_profile.step_policy.default_cleanup,
          ),
        );
        check(
          bool,
          "macro lookup",
          true,
          Axioms.rocq_tactic_group_for_macro_rule_id(
            "rocq.algebra_tactic_search",
          )
          == Some("hazel_algebra"),
        );
      },
    ),
    test_case(
      "Rocq tactic plans compile tactic modes",
      `Quick,
      () => {
        let plan =
          Axioms.{
            id: "test_plan",
            label: "test plan",
            steps: [
              rocq_tactic_step(
                ~id="once",
                ~label="once",
                ~tactic="hazel_once",
                ~mode=Once,
                ~rule_ids=[],
              ),
              rocq_tactic_step(
                ~id="try_once",
                ~label="try once",
                ~tactic="hazel_try",
                ~mode=TryOnce,
                ~rule_ids=[],
              ),
              rocq_tactic_step(
                ~id="repeat",
                ~label="repeat",
                ~tactic="hazel_repeat",
                ~mode=RepeatUntilStuck,
                ~rule_ids=[],
              ),
              rocq_tactic_step(
                ~id="fuel",
                ~label="fuel",
                ~tactic="hazel_fuel",
                ~mode=RepeatFuel(3),
                ~rule_ids=[],
              ),
              rocq_tactic_step(
                ~id="finish",
                ~label="finish",
                ~tactic="hazel_finish",
                ~mode=FinishOnly,
                ~rule_ids=[],
              ),
            ],
          };
        check(
          string,
          "compiled tactic script",
          "hazel_once; try hazel_try; repeat progress hazel_repeat; hazel_repeat_fuel 3%nat hazel_fuel; try solve [hazel_finish]; reflexivity",
          Web.ProofSearchBackend.rocq_tactic_plan_script(plan),
        );
      },
    ),
    test_case(
      "math rule catalog covers profile-visible Hazel and Rocq backends",
      `Quick,
      () => {
      [Arithmetic, Algebra, Trigonometry]
      |> List.iter(level => {
           let profile = Axioms.math_profile(level);
           check(
             list(string),
             Axioms.rewrite_level_label(level) ++ " unresolved rules",
             [],
             Axioms.unresolved_visible_rule_ids(profile.step_policy),
           );
           profile.step_policy.visible_rules
           |> List.iter((policy: Axioms.visible_rule_policy) =>
                switch (Axioms.catalog_rule_by_id(policy.rule_id)) {
                | Some(rule) =>
                  check(
                    bool,
                    policy.rule_id ++ " has Hazel backend",
                    true,
                    rule.hazel_backend |> Option.is_some,
                  );
                  check(
                    bool,
                    policy.rule_id ++ " has Rocq backend",
                    true,
                    rule.rocq_backend |> Option.is_some,
                  );
                | None => fail("missing catalog rule " ++ policy.rule_id)
                }
              );
         })
    }),
    test_case(
      "math rule catalog owns domain-specific Rocq search branches",
      `Quick,
      () => {
        let rule_tactics = (rule_id, domain) =>
          switch (Axioms.catalog_rule_by_id(rule_id)) {
          | Some({rocq_backend: Some(backend), _}) =>
            Axioms.rocq_tactics_for_domain(~domain, backend.search_tactics)
          | _ => []
          };
        check(
          list(string),
          "integer distribution branches",
          ["rewrite Z.mul_add_distr_l", "rewrite Z.mul_add_distr_r"],
          rule_tactics("alg.distribute_mul_add", Axioms.RocqIntegers),
        );
        check(
          list(string),
          "real distribution branches",
          ["rewrite Rmult_plus_distr_l", "rewrite Rmult_plus_distr_r"],
          rule_tactics("alg.distribute_mul_add", Axioms.RocqReals),
        );
        check(
          list(string),
          "trig branch is unavailable for integer goals",
          [],
          rule_tactics("trig.sin_sum", Axioms.RocqIntegers),
        );
        check(
          list(string),
          "trig branch is cataloged for real goals",
          ["rewrite sin_plus"],
          rule_tactics("trig.sin_sum", Axioms.RocqReals),
        );
        check(
          list(string),
          "multiplicative identity cleanup comes from cleanup catalog",
          [
            "repeat rewrite Z.mul_0_l",
            "repeat rewrite Z.mul_0_r",
            "repeat rewrite Z.mul_1_l",
            "repeat rewrite Z.mul_1_r",
          ],
          Axioms.rocq_cleanup_tactics(
            ~domain=Axioms.RocqIntegers,
            MulIdentity,
          ),
        );
        check(
          bool,
          "polynomial expansion retains explicit Rocq metadata",
          true,
          switch (Axioms.catalog_rule_by_id("alg.expand_polynomial")) {
          | Some({rocq_backend: Some(_), _}) => true
          | _ => false
          },
        );
        switch (Axioms.catalog_rule_by_id("alg.distribute_mul_add")) {
        | Some(rule) =>
          check(
            list(string),
            "distribution profile membership comes from catalog",
            ["Algebra"],
            rule.introduced_levels |> List.map(Axioms.rewrite_level_label),
          );
          check(
            bool,
            "distribution Hazel implementation comes from catalog",
            true,
            rule.hazel_backend == Some(Axioms.AlgebraDistributeMulAdd),
          );
          let shared_replay =
            switch (rule.rocq_backend) {
            | Some(backend) =>
              Axioms.rocq_tactics_for_domain(
                ~domain=Axioms.RocqIntegers,
                backend.replay_tactics,
              )
            | None => []
            };
          check(
            list(string),
            "proof export consumes catalog replay tactics",
            shared_replay,
            Web.CoqProofExport.rewrite_tactics_for_rule_id(
              ~domain=Web.CoqExport.Integers,
              rule.id,
            ),
          );
        | None => fail("missing distribution catalog rule")
        };
        check(
          list(string),
          "non-visible catalog rule retains export replay",
          ["try rewrite Z.mul_comm"],
          Web.CoqProofExport.rewrite_tactics_for_rule_id(
            ~domain=Web.CoqExport.Integers,
            "arith.mul_comm",
          ),
        );
        switch (Axioms.catalog_rule_by_id("arith.mul_comm")) {
        | Some(rule) =>
          check(
            bool,
            "replay-only catalog rule has no Hazel implementation",
            true,
            rule.hazel_backend == None,
          )
        | None => fail("missing replay-only multiplication rule")
        };
        switch (Axioms.catalog_rule_by_id("alg.power_mul")) {
        | Some(rule) =>
          check(
            bool,
            "power multiplication is cataloged as normalization",
            true,
            rule.kind == Axioms.NormalizationRule,
          );
          check(
            bool,
            "power normalization is a finishing tactic",
            true,
            switch (rule.rocq_backend) {
            | Some(backend) => backend.mode == Axioms.FinishOnly
            | None => false
            },
          );
          check(
            list(string),
            "real power normalization is searchable",
            ["hazel_power_normalize"],
            switch (rule.rocq_backend) {
            | Some(backend) =>
              Axioms.rocq_tactics_for_domain(
                ~domain=Axioms.RocqReals,
                backend.search_tactics,
              )
            | None => []
            },
          );
        | None => fail("missing power multiplication catalog rule")
        };
        switch (Axioms.catalog_rule_by_id("arith.affine_normalize")) {
        | Some(rule) =>
          check(
            bool,
            "affine normalization is syntax guarded",
            true,
            rule.kind == Axioms.GuardedNormalizationRule,
          );
          check(
            list(string),
            "real affine certificate comes from the catalog",
            ["lra"],
            switch (rule.rocq_backend) {
            | Some(backend) =>
              Axioms.rocq_tactics_for_domain(
                ~domain=Axioms.RocqReals,
                backend.search_tactics,
              )
            | None => []
            },
          );
        | None => fail("missing guarded affine normalization rule")
        };
      },
    ),
    test_case(
      "stage plans compile profile policy and automation purpose",
      `Quick,
      () => {
        let profile = Axioms.math_profile(Algebra);
        let manual = Axioms.stage_plan_for_profile(profile, Manual);
        check(
          list(string),
          "manual visible rules",
          [
            "arith.add_comm",
            "arith.const_fold",
            "arith.mul_const",
            "arith.mul_identity",
            "arith.simplify_scalar_products",
            "alg.distribute_mul_add",
            "alg.distribute_div_add",
            "alg.factor_common",
            "alg.cancel_common_add",
            "alg.collect_like_terms",
            "alg.expand_polynomial",
            "alg.difference_of_squares",
            "alg.square_of_sum",
            "alg.square_of_difference",
            "alg.difference_of_cubes",
            "alg.sum_of_cubes",
            "alg.cube_of_sum",
            "alg.cube_of_difference",
          ],
          manual.visible_rules
          |> List.map((planned: Axioms.planned_visible_rule) =>
               planned.rule.id
             ),
        );
        switch (manual.visible_rules) {
        | [
            _add_comm,
            _const_fold,
            _mul_const,
            _mul_identity,
            _scalar_normalize,
            distribution,
            ..._,
          ] =>
          check(
            list(string),
            "distribution-specific cleanup",
            [
              "add.assoc",
              "add.comm",
              "mul.assoc",
              "mul.comm",
              "power.notation",
            ],
            distribution.allowed_cleanup |> List.map(cleanup_capability_label),
          )
        | _ => fail("expected planned algebra rules")
        };
        check(
          list(string),
          "manual pre-cleanup",
          [
            "add.assoc",
            "add.comm",
            "mul.assoc",
            "mul.comm",
            "add.identity",
            "mul.identity",
            "const.fold",
            "collect.like_terms",
            "power.identity",
            "power.notation",
          ],
          manual.pre_cleanup |> List.map(cleanup_capability_label),
        );
        check(
          list(string),
          "manual post-cleanup",
          [
            "add.assoc",
            "add.comm",
            "mul.assoc",
            "mul.comm",
            "add.identity",
            "mul.identity",
            "const.fold",
            "collect.like_terms",
            "power.identity",
            "power.notation",
          ],
          manual.post_cleanup |> List.map(cleanup_capability_label),
        );
        check(
          string,
          "manual Rocq plan",
          "hazel_algebra_primitive_plan",
          manual.rocq_plan.id,
        );
        check(
          string,
          "check-result Rocq plan",
          "hazel_algebra_plan",
          Axioms.stage_plan_for_profile(profile, MultiStepCheck).rocq_plan.id,
        );
        check(
          string,
          "auto-eval Rocq plan",
          "hazel_algebra_plan_auto_simplify",
          Axioms.stage_plan_for_profile(profile, AutoEval).rocq_plan.id,
        );
        let disabled_profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            profile,
          );
        check(
          bool,
          "disabled rule is absent from stage plan",
          false,
          Axioms.stage_plan_for_profile(disabled_profile, Manual).
            visible_rules
          |> List.exists((planned: Axioms.planned_visible_rule) =>
               planned.rule.id == "alg.distribute_mul_add"
             ),
        );
      },
    ),
    test_case(
      "One Step budgets only the active math level",
      `Quick,
      () => {
        let usage = (plan, capability_id) =>
          Axioms.compiled_capability_for_id(plan, capability_id)
          |> Option.map((capability: Axioms.compiled_capability) =>
               capability.usage
             );
        let trig =
          Axioms.stage_plan_for_profile(
            Axioms.math_profile(Trigonometry),
            Manual,
          );
        check(
          bool,
          "inherited arithmetic is bounded automatic work",
          true,
          switch (usage(trig, "arith.const_fold")) {
          | Some(Axioms.BoundedClosure(_)) => true
          | _ => false
          },
        );
        check(
          bool,
          "inherited algebra is bounded automatic work",
          true,
          switch (usage(trig, "alg.distribute_mul_add")) {
          | Some(Axioms.BoundedClosure(_)) => true
          | _ => false
          },
        );
        check(
          bool,
          "active trig remains a single step",
          true,
          usage(trig, "trig.pythagorean_sin_cos") == Some(Axioms.AtMostOne),
        );
        check(
          option(string),
          "multiple inherited rules fit one Trig step",
          None,
          Axioms.validate_capability_use_counts(
            trig,
            [("arith.const_fold", 3), ("alg.distribute_mul_add", 2)],
          ),
        );
        check(
          bool,
          "two distinct Trig rules exceed the shared foreground budget",
          true,
          Axioms.validate_foreground_rule_uses(
            trig,
            ["trig.pythagorean_sin_cos", "trig.sin_double"],
          )
          |> Option.is_some,
        );
        let two_folds_source =
          plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3));
        check(
          bool,
          "search prunes a second foreground Arithmetic rule",
          true,
          Web.AxiomSearch.search(
            ~level=Arithmetic,
            ~max_depth=2,
            ~allowed_rule_ids=["arith.const_fold"],
            ~rule_use_limits=[("arith.const_fold", 1)],
            ~foreground_rule_ids=["arith.const_fold"],
            ~max_foreground_uses=1,
            ~log=false,
            two_folds_source,
            Exp.int(6),
          )
          |> Option.is_none,
        );
        check(
          bool,
          "the same two folds are searchable as inherited Algebra work",
          true,
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=2,
            ~allowed_rule_ids=["arith.const_fold"],
            ~rule_use_limits=[("arith.const_fold", 12)],
            ~foreground_rule_ids=[],
            ~max_foreground_uses=1,
            ~log=false,
            two_folds_source,
            Exp.int(6),
          )
          |> Option.is_some,
        );
        let calculus =
          Axioms.stage_plan_for_profile(
            Axioms.math_profile(Calculus),
            Manual,
          );
        check(
          bool,
          "Trig becomes inherited automatic work in Calculus",
          true,
          switch (usage(calculus, "trig.pythagorean_sin_cos")) {
          | Some(Axioms.BoundedClosure(_)) => true
          | _ => false
          },
        );
        check(
          bool,
          "Calculus still budgets one derivative rule",
          true,
          usage(calculus, "calc.diff_sum") == Some(Axioms.AtMostOne),
        );
        let disabled =
          Axioms.profile_with_capability_disabled(
            Axioms.math_profile(Trigonometry),
            "alg.distribute_mul_add",
          )
          |> Axioms.stage_plan_for_profile(_, Manual);
        check(
          bool,
          "inheritance preserves explicit disabled overrides",
          true,
          usage(disabled, "alg.distribute_mul_add") == Some(Axioms.Disabled),
        );
      },
    ),
    test_case(
      "One Step inherits lower math levels as automatic background work",
      `Quick,
      () => {
        let authorize = (profile, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage: Axioms.Manual,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 4,
            max_states: 80,
          });
        let accepts = (profile, source, target) =>
          switch (authorize(profile, source, target)) {
          | Web.ProfileProofPlan.Authorized(_) => true
          | Rejected(_) => false
          };
        let x = Exp.var("x");
        let y = Exp.var("y");
        let square = exp => power(exp, Exp.int(2));
        let identity = argument =>
          plus(
            square(builtin_sin(argument)),
            square(builtin_cos(argument)),
          );
        let trig = Axioms.math_profile(Trigonometry);
        let arithmetic_source =
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            square(builtin_sin(x)),
          );
        let arithmetic_target = plus(Exp.int(6), square(builtin_sin(x)));
        check(
          bool,
          "Trig accepts inherited multi-fold arithmetic with no trig rule",
          true,
          accepts(trig, arithmetic_source, arithmetic_target),
        );
        check(
          bool,
          "Trig accepts inherited arithmetic around one trig identity",
          true,
          accepts(trig, times(Exp.int(2), identity(x)), Exp.int(2)),
        );
        check(
          bool,
          "Trig still rejects two trig identities",
          false,
          accepts(trig, plus(identity(x), identity(y)), Exp.int(2)),
        );
        let without_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            trig,
          );
        check(
          bool,
          "a disabled inherited algebra rule stays disabled",
          false,
          accepts(
            without_distribution,
            times(x, plus(y, Exp.int(2))),
            plus(times(x, y), times(x, Exp.int(2))),
          ),
        );
        let calculus = Axioms.math_profile(Calculus);
        check(
          bool,
          "Calculus accepts multiple inherited trig identities",
          true,
          accepts(calculus, plus(identity(x), identity(y)), Exp.int(2)),
        );
        check(
          bool,
          "Calculus can simplify inherited trig inside a derivative",
          true,
          accepts(calculus, diff(identity(x), x), diff(Exp.int(1), x)),
        );
        check(
          bool,
          "Calculus composes inherited trig with one derivative rule",
          true,
          accepts(calculus, diff(identity(x), x), Exp.int(0)),
        );
        check(
          bool,
          "Calculus permits repeated inherited arithmetic before one derivative rule",
          true,
          accepts(
            calculus,
            diff(plus(plus(Exp.int(1), Exp.int(2)), x), x),
            plus(diff(Exp.int(3), x), diff(x, x)),
          ),
        );
        check(
          bool,
          "Calculus still rejects two derivative rules",
          false,
          accepts(
            calculus,
            plus(diff(builtin_sin(x), x), diff(builtin_cos(x), x)),
            plus(builtin_cos(x), negate(builtin_sin(x))),
          ),
        );
      },
    ),
    test_case(
      "incomplete live One Step targets stop before proof planning",
      `Quick,
      () => {
        let source = plus(Exp.int(1), Exp.int(2));
        let nested_hole =
          Exp.tuple([
            Exp.int(3),
            Exp.ap(Forward, Exp.var("f"), Exp.empty_hole()),
          ]);
        check(
          bool,
          "hole detection traverses nested expression forms",
          true,
          Web.AxiomSearch.has_hole(nested_hole),
        );
        let request: Web.ProfileProofPlan.request = {
          profile: Axioms.math_profile(Trigonometry),
          stage: Axioms.Manual,
          candidate_origin: Web.ProfileProofPlan.UserEntered,
          settings,
          env,
          source,
          target: plus(Exp.int(3), nested_hole),
          max_depth: 4,
          max_states: 80,
        };
        check(
          bool,
          "an incomplete target returns synchronously without a search state",
          true,
          switch (Web.ProfileProofPlan.start_authorize(request)) {
          | Web.ProfileProofPlan.PlanningComplete(
              Rejected(Web.ProfileProofPlan.NoSemanticRoute),
            ) =>
            true
          | PlanningComplete(_)
          | PlanningSearch(_) => false
          },
        );
      },
    ),
    test_case(
      "stage plan rejects unknown profile rule ids",
      `Quick,
      () => {
        let base = Axioms.math_profile(Algebra);
        let step_policy: Axioms.step_policy = {
          ...base.step_policy,
          visible_rules: [
            {
              rule_id: "unknown.rule",
              metadata: Axioms.visible_rule_metadata("unknown.rule"),
              allowed_cleanup: [],
              session_rewrite: None,
            },
            ...base.step_policy.visible_rules,
          ],
        };
        let profile = {
          ...base,
          step_policy,
        };
        check_raises(
          "unknown ids fail loudly",
          Invalid_argument(
            "Unknown math rule in profile stage plan: unknown.rule",
          ),
          () =>
          Axioms.stage_plan_for_profile(profile, Manual) |> ignore
        );
      },
    ),
    test_case(
      "capability override configuration errors are structured",
      `Quick,
      () => {
        let base = Axioms.math_profile(Algebra);
        let override =
            (capability_id, usage): Axioms.capability_usage_override => {
          capability_id,
          stage: MultiStepCheck,
          usage,
        };
        let unknown = {
          ...base,
          capability_usage_overrides: [
            override("unknown.capability", Disabled),
          ],
        };
        check(
          bool,
          "unknown override",
          true,
          switch (Axioms.validate_profile_configuration(unknown)) {
          | Some(Axioms.UnknownCapabilityOverride("unknown.capability")) =>
            true
          | _ => false
          },
        );
        let invalid_bound = {
          ...base,
          capability_usage_overrides: [
            override(
              "alg.distribute_mul_add",
              BoundedClosure({
                max_uses: 0,
                max_states: 20,
                cost: 1,
              }),
            ),
          ],
        };
        check(
          bool,
          "invalid closure bound",
          true,
          switch (Axioms.validate_profile_configuration(invalid_bound)) {
          | Some(Axioms.InvalidCapabilityUsage("alg.distribute_mul_add")) =>
            true
          | _ => false
          },
        );
        let duplicate = {
          ...base,
          capability_usage_overrides: [
            override("alg.distribute_mul_add", AtMostOne),
            override("alg.distribute_mul_add", Disabled),
          ],
        };
        check(
          bool,
          "duplicate stage override",
          true,
          switch (Axioms.validate_profile_configuration(duplicate)) {
          | Some(
              Axioms.DuplicateCapabilityOverride(
                "alg.distribute_mul_add",
                MultiStepCheck,
              ),
            ) =>
            true
          | _ => false
          },
        );
        let cannot_widen = {
          ...Axioms.math_profile(Arithmetic),
          capability_usage_overrides: [override("calc.diff_sum", AtMostOne)],
        };
        check(
          bool,
          "an override cannot add an unavailable inherited capability",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(cannot_widen, MultiStepCheck),
            "calc.diff_sum",
          ),
        );
        let bounded = {
          ...base,
          capability_usage_overrides: [
            override(
              "alg.distribute_mul_add",
              BoundedClosure({
                max_uses: 4,
                max_states: 7,
                cost: 1,
              }),
            ),
          ],
        };
        let bounded_plan =
          Axioms.stage_plan_for_profile(bounded, MultiStepCheck);
        check(
          int,
          "compiled closure tightens the caller search budget",
          7,
          Axioms.compiled_search_state_limit(~requested=80, bounded_plan),
        );
        check(
          int,
          "caller may request a tighter search budget",
          3,
          Axioms.compiled_search_state_limit(~requested=3, bounded_plan),
        );
      },
    ),
    test_case(
      "custom noncommutative profile is enforced by the shared authorizer",
      `Quick,
      () => {
        let profile =
          Axioms.profile_with_capability_disabled(
            Axioms.math_profile(Algebra),
            "mul.comm",
          );
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let authorize = (stage, candidate_origin, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin,
            settings,
            env,
            source,
            target,
            max_depth: 4,
            max_states: 80,
          });
        let accepted = result =>
          switch (result) {
          | Web.ProfileProofPlan.Authorized(_) => true
          | Rejected(_) => false
          };
        let accepted_in_both_stages = (source, target) =>
          accepted(authorize(Manual, UserEntered, source, target))
          && accepted(authorize(MultiStepCheck, UserEntered, source, target));
        [
          (
            "multiplication associativity",
            times(times(a, b), c),
            times(a, times(b, c)),
          ),
          (
            "left distribution",
            times(a, plus(b, c)),
            plus(times(a, b), times(a, c)),
          ),
          (
            "right distribution",
            times(plus(a, b), c),
            plus(times(a, c), times(b, c)),
          ),
          ("multiplicative identity", times(a, Exp.int(1)), a),
        ]
        |> List.iter(((label, source, target)) =>
             check(
               bool,
               label ++ " is accepted in One Step and Check Result",
               true,
               accepted_in_both_stages(source, target),
             )
           );
        [
          ("single commute", times(a, b), times(b, a)),
          (
            "three-factor reversal",
            times(times(a, b), c),
            times(times(c, b), a),
          ),
          (
            "distribution plus hidden commute",
            times(a, plus(b, c)),
            plus(times(b, a), times(c, a)),
          ),
        ]
        |> List.iter(((label, source, target)) =>
             check(
               bool,
               label ++ " is rejected in One Step and Check Result",
               false,
               accepted(authorize(Manual, UserEntered, source, target))
               || accepted(
                    authorize(MultiStepCheck, UserEntered, source, target),
                  ),
             )
           );
        let source = times(a, plus(b, c));
        let target = plus(times(a, b), times(a, c));
        let manual_candidate =
          authorize(MultiStepCheck, UserEntered, source, target);
        let automatic_candidate =
          authorize(AutoEval, AutomaticSimplify, source, target);
        check(
          bool,
          "candidate origin does not change authorization or fingerprint",
          true,
          switch (manual_candidate, automatic_candidate) {
          | (Authorized(left), Authorized(right)) =>
            left.capability_use_counts == right.capability_use_counts
            && left.profile_fingerprint == right.profile_fingerprint
          | _ => false
          },
        );
        switch (manual_candidate) {
        | Authorized(plan) =>
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Algebra,
              max_depth: 4,
              max_states: 80,
              source,
              target,
            };
          let program =
            Web.ProofSearchBackend.rocq_program_for_authorized_plan(
              ~profile,
              request,
              plan,
            );
          check(
            bool,
            "typed noncommutative certificate replays distribution only",
            true,
            string_contains("Rmult_plus_distr", program)
            && !string_contains("Rmult_comm", program)
            && !string_contains("ring", program)
            && !string_contains("lra", program),
          );
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        check(
          bool,
          "multiplication commutativity compiles to zero use",
          true,
          switch (
            Axioms.compiled_capability_for_id(
              Axioms.stage_plan_for_profile(profile, MultiStepCheck),
              "mul.comm",
            )
          ) {
          | Some({usage: Axioms.Disabled, _}) => true
          | _ => false
          },
        );
      },
    ),
    test_case(
      "scalar normalization suggestions use a focused catalog capability",
      `Quick,
      () => {
        let profile = Axioms.math_profile(Algebra);
        let x = Exp.var("x");
        let authorize = (~profile=profile, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage: Manual,
            candidate_origin: DisplayedSuggestion,
            settings,
            env,
            source,
            target,
            max_depth: 1,
            max_states: 80,
          });
        [
          ("double negative", negate(negate(x)), x),
          (
            "integer scalar product",
            times(times(Exp.int(2), x), Exp.int(3)),
            times(Exp.int(6), x),
          ),
          (
            "rational scalar reduction",
            divide(times(Exp.int(6), x), Exp.int(4)),
            divide(times(Exp.int(3), x), Exp.int(2)),
          ),
        ]
        |> List.iter(((label, source, target)) =>
             switch (authorize(source, target)) {
             | Authorized(plan) =>
               check(
                 bool,
                 label ++ " uses the scalar capability",
                 true,
                 List.mem(
                   "arith.simplify_scalar_products",
                   plan.capability_ids,
                 ),
               )
             | Rejected(rejection) =>
               fail(
                 label
                 ++ ": "
                 ++ Web.ProfileProofPlan.rejection_message(rejection),
               )
             }
           );
        check(
          bool,
          "an incorrect scalar result is rejected",
          true,
          switch (authorize(times(Exp.int(2), x), times(Exp.int(3), x))) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        let disabled =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.simplify_scalar_products",
          );
        check(
          bool,
          "a disabled scalar capability cannot authorize the same target",
          true,
          switch (authorize(~profile=disabled, negate(negate(x)), x)) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
      },
    ),
    test_case(
      "serializable custom math-mode DAG resolves into the shared profile",
      `Quick,
      () => {
        let usage_overrides =
          Axioms.automation_stages
          |> List.map((stage): Axioms.capability_usage_override =>
               {
                 capability_id: "mul.comm",
                 stage,
                 usage: Disabled,
               }
             );
        let matrix_mode: CustomMathMode.definition = {
          id: "matrix-algebra",
          label: "Matrix algebra",
          detail: "Associative and distributive multiplication without commutation",
          parents: [BuiltInParent(Algebra)],
          rule_overrides: [],
          cleanup_overrides: [
            {
              capability_id: "mul.comm",
              enabled: false,
            },
          ],
          usage_overrides,
          teacher_rewrites: [],
        };
        let json = CustomMathMode.yojson_of_definition(matrix_mode);
        check(
          string,
          "definition JSON round trip",
          Yojson.Safe.to_string(json),
          json
          |> CustomMathMode.definition_of_yojson
          |> CustomMathMode.yojson_of_definition
          |> Yojson.Safe.to_string,
        );
        let profile =
          switch (
            CustomMathMode.resolve(
              ~definitions=[matrix_mode],
              "matrix-algebra",
            )
          ) {
          | Ok(profile) => profile
          | Error(error) =>
            fail(CustomMathMode.resolution_error_message(error))
          };
        let library: CustomMathMode.library = {
          schema_version: CustomMathMode.current_schema_version,
          definitions: [matrix_mode],
          active_id: Some(matrix_mode.id),
        };
        let library_json = CustomMathMode.yojson_of_library(library);
        let round_tripped_library =
          library_json |> CustomMathMode.library_of_yojson;
        check(
          string,
          "versioned library JSON round trip",
          Yojson.Safe.to_string(library_json),
          round_tripped_library
          |> CustomMathMode.yojson_of_library
          |> Yojson.Safe.to_string,
        );
        check(
          bool,
          "round-tripped library revalidates",
          true,
          switch (CustomMathMode.validate_library(round_tripped_library)) {
          | Ok () => true
          | Error(_) => false
          },
        );
        let round_tripped_profile =
          switch (
            CustomMathMode.resolve(
              ~definitions=round_tripped_library.definitions,
              "matrix-algebra",
            )
          ) {
          | Ok(profile) => profile
          | Error(error) =>
            fail(CustomMathMode.resolution_error_message(error))
          };
        check(
          string,
          "round trip preserves the compiled profile fingerprint",
          Web.ProfileProofPlan.profile_fingerprint(profile, MultiStepCheck),
          Web.ProfileProofPlan.profile_fingerprint(
            round_tripped_profile,
            MultiStepCheck,
          ),
        );
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let authorize = (source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage: MultiStepCheck,
            candidate_origin: UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 4,
            max_states: 80,
          });
        check(
          bool,
          "resolved mode retains associativity",
          true,
          switch (authorize(times(times(a, b), c), times(a, times(b, c)))) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        check(
          bool,
          "resolved mode rejects commutation",
          true,
          switch (authorize(times(a, b), times(b, a))) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        check(
          bool,
          "resolved mode prunes operations that depend on commutation",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, MultiStepCheck),
            "arith.simplify_scalar_products",
          ),
        );
        let left: CustomMathMode.definition = {
          ...matrix_mode,
          id: "left",
          parents: [CustomParent("right")],
        };
        let right: CustomMathMode.definition = {
          ...matrix_mode,
          id: "right",
          parents: [CustomParent("left")],
        };
        check(
          bool,
          "cycles are rejected structurally",
          true,
          switch (CustomMathMode.resolve(~definitions=[left, right], "left")) {
          | Error(CustomMathMode.InheritanceCycle(_)) => true
          | _ => false
          },
        );
        check(
          bool,
          "library load revalidates inheritance cycles",
          true,
          switch (
            CustomMathMode.validate_library({
              schema_version: CustomMathMode.current_schema_version,
              definitions: [left, right],
              active_id: None,
            })
          ) {
          | Error(
              CustomMathMode.InvalidLibraryDefinition(_, InheritanceCycle(_)),
            ) =>
            true
          | _ => false
          },
        );
        let unknown: CustomMathMode.definition = {
          ...matrix_mode,
          id: "unknown",
          rule_overrides: [
            {
              rule_id: "missing.rule",
              enabled: true,
            },
          ],
        };
        check(
          bool,
          "unknown rule IDs are rejected",
          true,
          switch (CustomMathMode.resolve(~definitions=[unknown], "unknown")) {
          | Error(CustomMathMode.UnknownRule("missing.rule")) => true
          | _ => false
          },
        );
      },
    ),
    test_case(
      "extracted proof traces preserve durable JSON",
      `Quick,
      () => {
        let source = times(Exp.var("x"), Exp.int(1));
        let target = Exp.var("x");
        let summary: Web.ProofTrace.trace_summary = {
          justification: "multiplicative identity",
          group_name: Some("arithmetic"),
          from_normal_exp: target,
          to_normal_exp: target,
          from_rule_ids: ["mul.identity"],
          to_rule_ids: [],
          rule_ids: ["mul.identity"],
          prover_steps: [
            Web.ProofTrace.prover_step(
              ~origin=Web.ProofTrace.Normalization,
              ~rule_id="mul.identity",
              ~before_full_exp=source,
              ~after_full_exp=target,
              ~before_exp=source,
              ~after_exp=target,
              ~detail="JSON compatibility fixture",
            ),
          ],
          exportable: true,
        };
        let extracted_json = Web.ProofTrace.yojson_of_trace_summary(summary);
        check(
          string,
          "extracted JSON round-trips without schema changes",
          Yojson.Safe.to_string(extracted_json),
          extracted_json
          |> Web.ProofTrace.trace_summary_of_yojson
          |> Web.ProofTrace.yojson_of_trace_summary
          |> Yojson.Safe.to_string,
        );
      },
    ),
    test_case(
      "teacher builder compiles through the serializable custom-mode model",
      `Quick,
      () => {
        let model =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetLabel("Noncommutative lab"),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetParent(Algebra),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetCleanupEnabled(
                 "mul.comm",
                 false,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetUsage(
                 "mul.comm",
                 Manual,
                 Disabled,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetUsage(
                 "mul.comm",
                 MultiStepCheck,
                 Disabled,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetUsage(
                 "mul.comm",
                 AutoEval,
                 Disabled,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetActive(true),
             );
        let profile =
          Web.MathModeBuilder.effective_profile(
            ~fallback=Axioms.math_profile(Algebra),
            model,
          );
        check(string, "custom label", "Noncommutative lab", profile.label);
        check(
          bool,
          "builder disables multiplication commutation in Check Result",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, MultiStepCheck),
            "mul.comm",
          ),
        );
        check(
          bool,
          "builder also prunes dependent scalar simplification",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, MultiStepCheck),
            "arith.simplify_scalar_products",
          ),
        );
        let inspected_profile =
          Web.ProfileBoard.apply_model_to_profile(
            Web.ProfileBoard.Model.init,
            profile,
          );
        check(
          bool,
          "Profile inspection preserves Builder usage policy",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(inspected_profile, MultiStepCheck),
            "mul.comm",
          ),
        );
        let json = Web.MathModeBuilder.Model.yojson_of_t(model);
        check(
          string,
          "builder state JSON round trip",
          Yojson.Safe.to_string(json),
          json
          |> Web.MathModeBuilder.Model.t_of_yojson
          |> Web.MathModeBuilder.Model.yojson_of_t
          |> Yojson.Safe.to_string,
        );
        let saved =
          model
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SaveDefinition,
             );
        check(
          int,
          "save stores a reusable definition",
          1,
          List.length(saved.saved_definitions),
        );
        let duplicated =
          saved
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.DuplicateDefinition,
             );
        check(
          bool,
          "duplicates are inactive drafts",
          false,
          duplicated.active,
        );
        check(
          string,
          "duplicate gets a stable copy id",
          "my-math-mode-copy",
          duplicated.id,
        );
        let exported =
          Web.MathModeBuilder.definition(model)
          |> CustomMathMode.yojson_of_definition
          |> Yojson.Safe.to_string;
        let imported =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetImportJson(exported),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.ImportDefinition,
             );
        check(
          string,
          "import restores the definition",
          model.label,
          imported.label,
        );
        let multi_parent =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetAdditionalParent(
                 Trigonometry,
                 true,
               ),
             );
        check(
          int,
          "builder serializes multiple parents",
          2,
          List.length(Web.MathModeBuilder.definition(multi_parent).parents),
        );
        let upward_usage =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetUsage(
                 "alg.distribute_mul_add",
                 MultiStepCheck,
                 Disabled,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetUsage(
                 "alg.distribute_mul_add",
                 Manual,
                 AtMostOne,
               ),
             );
        check(
          bool,
          "One Step availability removes a conflicting higher-stage Never override",
          false,
          upward_usage.usage_overrides
          |> List.exists((override: Axioms.capability_usage_override) =>
               override.capability_id == "alg.distribute_mul_add"
               && override.stage == MultiStepCheck
               && override.usage == Disabled
             ),
        );
        let draft_added =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRewriteDraftSource(
                 "sin($a + $b)",
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRewriteDraftTarget(
                 "sin($a)*cos($b) + cos($a)*sin($b)",
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.AddRewriteDraft,
             );
        check(
          int,
          "reviewed text-entry contract is accepted",
          1,
          List.length(draft_added.teacher_rewrites),
        );
        let malformed_draft =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRewriteDraftSource(
                 "sin($unbound)",
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.AddRewriteDraft,
             );
        check(
          int,
          "incomplete text-entry contract is rejected",
          0,
          List.length(malformed_draft.teacher_rewrites),
        );
      },
    ),
    test_case(
      "untrusted session rewrites are manual-only and export as admitted",
      `Quick,
      () => {
        let source_pattern = "sin($a)*sin($b)";
        let target_pattern = "(cos($a - $b) - cos($a + $b))/2";
        let model =
          Web.MathModeBuilder.Model.blank
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetParent(Trigonometry),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRewriteDraftSource(
                 source_pattern,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRewriteDraftTarget(
                 target_pattern,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.AddRewriteDraft,
             );
        check(
          int,
          "arbitrary pattern becomes a session rewrite",
          1,
          List.length(model.session_rewrites),
        );
        check(
          int,
          "arbitrary pattern does not become a reviewed rewrite",
          0,
          List.length(model.teacher_rewrites),
        );
        let active_model = {
          ...model,
          active: true,
        };
        let profile =
          Web.MathModeBuilder.effective_profile(
            ~fallback=Axioms.math_profile(Trigonometry),
            active_model,
          );
        let stepper_with_custom_mode = {
          ...Web.StepperView.Model.init,
          rewrite_level: Calculus,
          math_mode_builder: active_model,
        };
        let level_change_while_custom =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.SelectRewriteLevel(Algebra),
            stepper_with_custom_mode,
          ).
            model;
        check(
          bool,
          "active custom mode pauses built-in level selection",
          true,
          level_change_while_custom.rewrite_level == Calculus,
        );
        let automation_change_while_custom =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.SelectAutomationStage(AutoEval),
            level_change_while_custom,
          ).
            model;
        check(
          bool,
          "active custom mode preserves independent automation selection",
          true,
          automation_change_while_custom.automation_stage == AutoEval,
        );
        let custom_mode_off =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.MathModeBuilderAction(
              Web.MathModeBuilder.Update.SetActive(false),
            ),
            automation_change_while_custom,
          ).
            model;
        let built_in_level_selected =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.SelectRewriteLevel(Algebra),
            custom_mode_off,
          ).
            model;
        check(
          bool,
          "turning custom mode off restores built-in level selection",
          true,
          built_in_level_selected.rewrite_level == Algebra,
        );
        let definition = List.hd(model.session_rewrites);
        check(
          bool,
          "session rule is visible on the effective profile",
          true,
          Axioms.visible_rule_enabled(profile.step_policy, definition.id),
        );
        check(
          bool,
          "session rule is excluded from Manual stage planning",
          false,
          Axioms.stage_plan_for_profile(profile, Manual).visible_rules
          |> List.exists((rule: Axioms.planned_visible_rule) =>
               rule.rule.id == definition.id
             ),
        );
        let product_to_sum = (a, b) =>
          divide(minus(cos(minus(a, b)), cos(plus(a, b))), Exp.int(2));
        let authorize = (profile, stage, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin: UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 1,
            max_states: 40,
          });
        let x = Exp.var("x");
        let y = Exp.var("y");
        let source = times(sin(x), sin(y));
        let target = product_to_sum(x, y);
        let plan =
          switch (authorize(profile, Manual, source, target)) {
          | Authorized(plan) => plan
          | Rejected(rejection) =>
            fail(
              "expected session rewrite authorization: "
              ++ Web.ProfileProofPlan.rejection_message(rejection),
            )
          };
        check(bool, "manual plan is non-exportable", false, plan.exportable);
        check(
          bool,
          "manual plan records only the session rule",
          true,
          plan.summary.rule_ids == [definition.id]
          && plan.summary.exportable == false,
        );
        let nested_a = plus(x, Exp.int(1));
        let nested_b = times(Exp.int(2), y);
        check(
          bool,
          "the same schema matches a structurally different instance",
          true,
          switch (
            authorize(
              profile,
              Manual,
              times(sin(nested_a), sin(nested_b)),
              product_to_sum(nested_a, nested_b),
            )
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        check(
          bool,
          "an incorrect target is rejected",
          false,
          switch (
            authorize(profile, Manual, source, plus(target, Exp.int(1)))
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        let forward_model =
          Web.MathModeBuilder.Update.update(
            Web.MathModeBuilder.Update.SetSessionRewriteDirection(
              definition.id,
              Forward,
            ),
            active_model,
          );
        let forward_profile =
          Web.MathModeBuilder.effective_profile(
            ~fallback=Axioms.math_profile(Trigonometry),
            forward_model,
          );
        check(
          bool,
          "forward-only session rewrite rejects contraction",
          false,
          switch (authorize(forward_profile, Manual, target, source)) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        let inactive_profile =
          Web.MathModeBuilder.effective_profile(
            ~fallback=Axioms.math_profile(Trigonometry),
            {
              ...model,
              active: false,
            },
          );
        check(
          bool,
          "inactive custom mode does not expose the session rewrite",
          false,
          Axioms.visible_rule_enabled(
            inactive_profile.step_policy,
            definition.id,
          ),
        );
        check(
          bool,
          "Check Result stage never receives the session capability",
          false,
          Axioms.stage_plan_for_profile(profile, MultiStepCheck).capabilities
          |> List.exists((capability: Axioms.compiled_capability) =>
               capability.id == definition.id
             ),
        );
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 1,
            max_states: 40,
            source,
            target,
          };
        let validation_program =
          Web.ProofSearchBackend.rocq_program_for_authorized_plan(
            ~profile,
            request,
            plan,
          );
        write_text_file(
          "/tmp/hazel_untrusted_validation.v",
          validation_program,
        );
        check(
          bool,
          "Rocq validation isolates the session rewrite as admitted",
          true,
          string_contains("BEGIN UNSOUND CUSTOM REWRITES", validation_program)
          && string_contains("Admitted.", validation_program)
          && string_contains("Theorem hazel_rocq_search", validation_program),
        );
        let proof_export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_written_step_export_chain(
                ~source,
                ~target,
                ~trace=plan.summary,
              ),
            )
          ) {
          | Some(export) => export
          | None => fail("expected an admitted custom-rewrite export")
          };
        write_text_file("/tmp/hazel_untrusted_export.v", proof_export);
        check(
          bool,
          "final proof export keeps admissions in a dedicated section",
          true,
          string_contains("BEGIN UNSOUND CUSTOM REWRITES", proof_export)
          && string_contains(definition.id, proof_export)
          && string_contains("Admitted.", proof_export)
          && string_contains("Theorem equiv_exp", proof_export),
        );
        check(
          bool,
          "target-only metavariables are rejected",
          true,
          switch (
            Web.SessionRewrite.make(
              ~id="session.untrusted.bad",
              ~source_pattern="$a + 1",
              ~target_pattern="$b",
            )
          ) {
          | Error(Web.SessionRewrite.TargetOnlyMetavariable("b")) => true
          | _ => false
          },
        );
        check(
          bool,
          "session rewrites are omitted from saved custom-mode definitions",
          true,
          Web.MathModeBuilder.definition(active_model).teacher_rewrites == [],
        );
      },
    ),
    test_case(
      "approved teacher trig rewrites enforce schema subset direction and stage",
      `Quick,
      () => {
        let approved =
          TeacherRewrite.approved_schema("trig.sin_sum") |> Option.get;
        check(
          bool,
          "approved schema validates",
          true,
          TeacherRewrite.validate(approved) == Ok(approved),
        );
        check(
          bool,
          "empty pattern is rejected",
          true,
          switch (
            TeacherRewrite.validate({
              ...approved,
              source_pattern: "",
            })
          ) {
          | Error(TeacherRewrite.MalformedPatterns("trig.sin_sum")) => true
          | _ => false
          },
        );
        check(
          bool,
          "mismatched certificate is rejected",
          true,
          switch (
            TeacherRewrite.validate({
              ...approved,
              certificate_ref: "rocq.rewrite.cos_plus",
            })
          ) {
          | Error(TeacherRewrite.MismatchedCertificate("trig.sin_sum")) =>
            true
          | _ => false
          },
        );
        check(
          bool,
          "One Step-only imported availability is rejected",
          true,
          switch (
            TeacherRewrite.validate({
              ...approved,
              stages: [Manual],
            })
          ) {
          | Error(TeacherRewrite.InvalidStages("trig.sin_sum")) => true
          | _ => false
          },
        );
        let trig_ids =
          TeacherRewrite.approved_schemas
          |> List.map((definition: TeacherRewrite.definition) =>
               definition.id
             );
        trig_ids
        |> List.iter(selected_id => {
             let base_model =
               Web.MathModeBuilder.Model.init
               |> Web.MathModeBuilder.Update.update(
                    Web.MathModeBuilder.Update.SetParent(Trigonometry),
                  );
             let isolated_model =
               trig_ids
               |> List.fold_left(
                    (model, rule_id) =>
                      Web.MathModeBuilder.Update.update(
                        Web.MathModeBuilder.Update.SetRuleEnabled(
                          rule_id,
                          false,
                        ),
                        model,
                      ),
                    base_model,
                  )
               |> Web.MathModeBuilder.Update.update(
                    Web.MathModeBuilder.Update.SetTeacherRewriteEnabled(
                      selected_id,
                      true,
                    ),
                  )
               |> Web.MathModeBuilder.Update.update(
                    Web.MathModeBuilder.Update.SetActive(true),
                  );
             let isolated_profile =
               Web.MathModeBuilder.effective_profile(
                 ~fallback=Axioms.math_profile(Trigonometry),
                 isolated_model,
               );
             trig_ids
             |> List.iter(rule_id =>
                  check(
                    bool,
                    selected_id ++ " independently controls " ++ rule_id,
                    rule_id == selected_id,
                    Axioms.visible_rule_enabled(
                      isolated_profile.step_policy,
                      rule_id,
                    ),
                  )
                );
           });
        let model =
          Web.MathModeBuilder.Model.init
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetParent(Trigonometry),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRuleEnabled(
                 "trig.sin_sum",
                 false,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRuleEnabled(
                 "trig.cos_sum",
                 false,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRuleEnabled(
                 "trig.sin_diff",
                 false,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetRuleEnabled(
                 "trig.cos_diff",
                 false,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetTeacherRewriteEnabled(
                 "trig.sin_sum",
                 true,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetTeacherRewriteDirection(
                 "trig.sin_sum",
                 Forward,
               ),
             )
          |> Web.MathModeBuilder.Update.update(
               Web.MathModeBuilder.Update.SetActive(true),
             );
        let profile =
          Web.MathModeBuilder.effective_profile(
            ~fallback=Axioms.math_profile(Trigonometry),
            model,
          );
        let authorize = (stage, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin: UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 4,
            max_states: 100,
          });
        let expansion = (a, b) =>
          plus(times(sin(a), cos(b)), times(cos(a), sin(b)));
        let a = Exp.var("a");
        let b = Exp.var("b");
        let source = sin(plus(a, b));
        let target = expansion(a, b);
        let forward = authorize(Manual, source, target);
        check(
          bool,
          "forward schema is authorized",
          true,
          switch (forward) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        [MultiStepCheck, AutoEval]
        |> List.iter(stage =>
             check(
               bool,
               "One Step availability inherits upward",
               true,
               switch (authorize(stage, source, target)) {
               | Authorized(_) => true
               | Rejected(_) => false
               },
             )
           );
        let x = Exp.var("x");
        let y = Exp.var("y");
        let nested_a = plus(x, Exp.int(1));
        let nested_b = times(Exp.int(2), y);
        check(
          bool,
          "structurally different metavariable instances are authorized",
          true,
          switch (
            authorize(
              Manual,
              sin(plus(nested_a, nested_b)),
              expansion(nested_a, nested_b),
            )
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        [
          ("reverse direction", authorize(Manual, target, source)),
          (
            "incorrect target",
            authorize(Manual, source, plus(target, Exp.int(1))),
          ),
          (
            "disabled subset identity",
            authorize(
              Manual,
              cos(plus(a, b)),
              minus(times(cos(a), cos(b)), times(sin(a), sin(b))),
            ),
          ),
        ]
        |> List.iter(((label, result)) =>
             check(
               bool,
               label,
               true,
               switch (result) {
               | Web.ProfileProofPlan.Rejected(_) => true
               | Web.ProfileProofPlan.Authorized(_) => false
               },
             )
           );
        switch (forward) {
        | Authorized(plan) =>
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Trigonometry,
              max_depth: 4,
              max_states: 100,
              source,
              target,
            };
          let program =
            Web.ProofSearchBackend.rocq_program_for_authorized_plan(
              ~profile,
              request,
              plan,
            );
          check(
            bool,
            "Rocq replay uses the approved sine-sum certificate",
            true,
            string_contains("sin_plus", program),
          );
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
      },
    ),
    test_case(
      "automation stages choose Rocq tactic plan purposes",
      `Quick,
      () => {
        check(
          string,
          "manual uses primitive validation",
          "validate_primitive_step",
          Axioms.rocq_tactic_plan_purpose_label(
            Web.ProofSearchBackend.tactic_plan_purpose_for_automation_stage(
              Axioms.Manual,
            ),
          ),
        );
        check(
          string,
          "check result uses check-result validation",
          "check_result",
          Axioms.rocq_tactic_plan_purpose_label(
            Web.ProofSearchBackend.tactic_plan_purpose_for_automation_stage(
              Axioms.MultiStepCheck,
            ),
          ),
        );
        check(
          string,
          "auto simplify uses auto-simplify validation",
          "auto_simplify",
          Axioms.rocq_tactic_plan_purpose_label(
            Web.ProofSearchBackend.tactic_plan_purpose_for_automation_stage(
              Axioms.AutoEval,
            ),
          ),
        );
      },
    ),
    test_case(
      "rational binomial squares use visible primitive rewrite paths",
      `Quick,
      () => {
        let x = Exp.var("x");
        let c = builtin_cos(times(Exp.int(2), x));
        let source_quotient =
          power(divide(minus(Exp.int(1), c), Exp.int(2)), Exp.int(2));
        let source_distributed =
          power(
            minus(divide(Exp.int(1), Exp.int(2)), divide(c, Exp.int(2))),
            Exp.int(2),
          );
        let square_expansion = (left, right) =>
          plus(
            minus(
              power(left, Exp.int(2)),
              times(times(Exp.int(2), left), right),
            ),
            power(right, Exp.int(2)),
          );
        let half = divide(Exp.int(1), Exp.int(2));
        let c_half = divide(c, Exp.int(2));
        let source_expansion = square_expansion(half, c_half);
        let screenshot_target =
          times(
            half,
            plus(minus(half, c), times(half, power(c, Exp.int(2)))),
          );
        let visible_search = (profile, source, target) =>
          Web.AxiomSearch.search(
            ~level=profile.Axioms.level,
            ~max_depth=1,
            ~allowed_rule_ids=
              Axioms.stage_plan_for_profile(profile, Manual).visible_rules
              |> List.map((planned: Axioms.planned_visible_rule) =>
                   planned.rule.id
                 ),
            ~log=false,
            source,
            target,
          )
          |> Option.is_some;
        [Trigonometry, Calculus]
        |> List.iter(level => {
             let profile = Axioms.math_profile(level);
             check(
               bool,
               Axioms.rewrite_level_label(level)
               ++ " distributes a quotient using the visible primitive",
               true,
               visible_search(profile, source_quotient, source_distributed),
             );
             check(
               bool,
               Axioms.rewrite_level_label(level)
               ++ " expands the resulting square using the visible identity",
               true,
               visible_search(profile, source_distributed, source_expansion),
             );
           });
        let profile = Axioms.math_profile(Trigonometry);
        let authorize = (profile, source) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage: Axioms.MultiStepCheck,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings,
            env,
            source,
            target: screenshot_target,
            max_depth: 4,
            max_states: 80,
          });
        [
          ("distributed quotient square", source_distributed),
          ("nested quotient square", source_quotient),
        ]
        |> List.iter(((label, source)) => {
             switch (authorize(profile, source)) {
             | Authorized(plan) =>
               check(
                 bool,
                 label
                 ++ " reaches its rational polynomial target without search",
                 true,
                 List.mem("alg.square_of_difference", plan.capability_ids)
                 && List.mem("alg.expand_polynomial", plan.capability_ids),
               );
               let request =
                 Web.ProofSearchBackend.{
                   backend: JSCoqTacticSearch,
                   level: Trigonometry,
                   max_depth: 4,
                   max_states: 80,
                   source,
                   target: screenshot_target,
                 };
               let program =
                 Web.ProofSearchBackend.rocq_program_for_authorized_plan(
                   ~profile,
                   request,
                   plan,
                 );
               let path =
                 label == "nested quotient square"
                   ? "/tmp/hazel_nested_quotient_square.v"
                   : "/tmp/hazel_distributed_quotient_square.v";
               write_text_file(path, program);
               check(
                 bool,
                 label ++ " emits exact field replay for rational division",
                 true,
                 string_contains("field.", program),
               );
             | Rejected(rejection) =>
               fail(Web.ProfileProofPlan.rejection_message(rejection))
             }
           });
        let three_quarters = divide(Exp.int(3), Exp.int(4));
        let c_fifth = divide(c, Exp.int(5));
        let different_source =
          power(minus(three_quarters, c_fifth), Exp.int(2));
        let different_expansion = square_expansion(three_quarters, c_fifth);
        check(
          bool,
          "the primitive square identity handles different rational terms",
          true,
          visible_search(profile, different_source, different_expansion),
        );
        check(
          bool,
          "the primitive path rejects an incorrect expansion",
          false,
          visible_search(
            profile,
            different_source,
            plus(different_expansion, Exp.int(1)),
          ),
        );
        let without_division_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_div_add",
            profile,
          );
        check(
          bool,
          "disabling quotient distribution removes that primitive path",
          false,
          visible_search(
            without_division_distribution,
            source_quotient,
            source_distributed,
          ),
        );
        let without_square_identity =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.square_of_difference",
            profile,
          );
        check(
          bool,
          "the composed screenshot route still respects a disabled square identity",
          false,
          switch (authorize(without_square_identity, source_distributed)) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        check(
          bool,
          "disabling the square identity removes that primitive path",
          false,
          visible_search(
            without_square_identity,
            source_distributed,
            source_expansion,
          ),
        );
        check(
          bool,
          "Calculus non-derivatives do not trigger derivative auto simplify",
          false,
          Web.MissingStep.auto_simplify_uses_profile(
            Calculus,
            source_quotient,
          ),
        );
        check(
          bool,
          "Calculus derivatives retain profile-driven differentiation",
          true,
          Web.MissingStep.auto_simplify_uses_profile(
            Calculus,
            diff(power(x, Exp.int(2)), x),
          ),
        );
      },
    ),
    test_case(
      "auto simplify validates with check-result proof power at every math level",
      `Quick,
      () => {
        let x = Exp.var("x");
        let request = (level, source, target) =>
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        [
          (
            "arithmetic",
            Arithmetic,
            request(Arithmetic, plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
          ),
          (
            "algebra",
            Algebra,
            request(
              Algebra,
              times(x, plus(x, Exp.int(1))),
              plus(times(x, x), x),
            ),
          ),
          (
            "trigonometry",
            Trigonometry,
            request(
              Trigonometry,
              plus(
                power(builtin_sin(x), Exp.int(2)),
                power(builtin_cos(x), Exp.int(2)),
              ),
              Exp.int(1),
            ),
          ),
          (
            "functions and lists",
            FunctionsAndLists,
            request(FunctionsAndLists, plus(x, Exp.int(0)), x),
          ),
          (
            "calculus",
            Calculus,
            request(
              Calculus,
              diff(power(x, Exp.int(2)), x),
              times(Exp.int(2), x),
            ),
          ),
        ]
        |> List.iter(((label, level, candidate)) => {
             let profile = Axioms.math_profile(level);
             let program = purpose =>
               Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
                 ~profile,
                 ~purpose,
                 candidate,
               );
             check(
               string,
               label ++ " uses identical profile validation",
               program(CheckResult),
               program(AutoSimplify),
             );
           });
      },
    ),
    test_case(
      "rational polynomial cleanup composes inherited scalar normalization and expansion",
      `Quick,
      () => {
        let x = Exp.var("x");
        let c2 = builtin_cos(times(Exp.int(2), x));
        let c4 = builtin_cos(times(Exp.int(4), x));
        let source =
          times(
            Exp.int(2),
            times(
              divide(Exp.int(1), Exp.int(2)),
              plus(
                minus(divide(Exp.int(1), Exp.int(2)), c2),
                times(
                  divide(Exp.int(1), Exp.int(2)),
                  power(c2, Exp.int(2)),
                ),
              ),
            ),
          );
        let target =
          plus(
            minus(divide(Exp.int(1), Exp.int(2)), c2),
            times(divide(Exp.int(1), Exp.int(2)), power(c2, Exp.int(2))),
          );
        let nested_argument_source =
          plus(
            Exp.int(1),
            plus(
              minus(divide(Exp.int(1), Exp.int(2)), c2),
              times(
                divide(Exp.int(1), Exp.int(2)),
                divide(
                  plus(
                    Exp.int(1),
                    builtin_cos(times(Exp.int(2), times(Exp.int(2), x))),
                  ),
                  Exp.int(2),
                ),
              ),
            ),
          );
        let nested_argument_target =
          plus(
            minus(divide(Exp.int(7), Exp.int(4)), c2),
            times(divide(Exp.int(1), Exp.int(4)), c4),
          );
        let authorize =
            (~profile, ~stage=Axioms.MultiStepCheck, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin:
              stage == Axioms.AutoEval
                ? Web.ProfileProofPlan.AutomaticSimplify : UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 6,
            max_states: 80,
          });
        let scalar_nested_source =
          Web.ArithmeticNormalization.simplify_scalar_products(
            nested_argument_source,
          );
        check(
          bool,
          "the nested screenshot retains a rational expansion shape after scalar cleanup",
          true,
          Web.RewriteChecker.contains_rational_polynomial_expansion_shape(
            nested_argument_source,
          )
          || Web.RewriteChecker.contains_rational_polynomial_expansion_shape(
               scalar_nested_source,
             ),
        );
        check(
          bool,
          "the nested screenshot endpoints have the same exact rational polynomial",
          true,
          Web.RewriteChecker.rational_polynomial_equivalent(
            scalar_nested_source,
            nested_argument_target,
          ),
        );
        check(
          bool,
          "the nested screenshot transition has a direct catalog expansion witness",
          true,
          Web.RewriteChecker.rational_polynomial_expansion_trace_for_profile(
            ~profile=Axioms.math_profile(Trigonometry),
            ~stage=Axioms.MultiStepCheck,
            nested_argument_source,
            nested_argument_target,
          )
          |> Option.is_some,
        );
        [Trigonometry, Calculus]
        |> List.iter(level => {
             let profile = Axioms.math_profile(level);
             [
               (source, target, false),
               (nested_argument_source, nested_argument_target, true),
             ]
             |> List.iter(((source, target, needs_expansion)) =>
                  switch (authorize(~profile, source, target)) {
                  | Authorized(plan) =>
                    check(
                      bool,
                      Axioms.rewrite_level_label(level)
                      ++ " records scalar cleanup and polynomial expansion",
                      true,
                      List.mem(
                        "arith.simplify_scalar_products",
                        plan.capability_ids,
                      )
                      && (
                        !needs_expansion
                        || List.mem(
                             "alg.expand_polynomial",
                             plan.capability_ids,
                           )
                      ),
                    )
                  | Rejected(rejection) =>
                    fail(Web.ProfileProofPlan.rejection_message(rejection))
                  }
                );
             check(
               bool,
               Axioms.rewrite_level_label(level)
               ++ " Auto Simplify has the same rational-polynomial authority",
               true,
               switch (
                 authorize(~profile, ~stage=Axioms.AutoEval, source, target)
               ) {
               | Authorized(_) => true
               | Rejected(_) => false
               },
             );
           });
        let profile = Axioms.math_profile(Trigonometry);
        let half = divide(Exp.int(1), Exp.int(2));
        let scalar_target = minus(half, c2);
        let scalar_source = times(times(Exp.int(2), half), scalar_target);
        let opaque_fx = app("f", x);
        let opaque_target =
          plus(opaque_fx, divide(Exp.int(3), Exp.int(4)));
        let opaque_source = times(times(Exp.int(2), half), opaque_target);
        [
          (
            "cosine scalar cleanup",
            "/tmp/hazel_profile_rational_scalar_cos.v",
            scalar_source,
            scalar_target,
          ),
          (
            "opaque-function scalar cleanup",
            "/tmp/hazel_profile_rational_scalar_opaque.v",
            opaque_source,
            opaque_target,
          ),
          (
            "rational polynomial cleanup",
            "/tmp/hazel_profile_rational_polynomial_cos.v",
            source,
            target,
          ),
        ]
        |> List.iter(((label, path, source, target)) => {
             let plan =
               switch (authorize(~profile, source, target)) {
               | Authorized(plan) => plan
               | Rejected(rejection) =>
                 fail(
                   label
                   ++ ": "
                   ++ Web.ProfileProofPlan.rejection_message(rejection),
                 )
               };
             let request =
               Web.ProofSearchBackend.{
                 backend: JSCoqTacticSearch,
                 level: Trigonometry,
                 max_depth: 6,
                 max_states: 80,
                 source,
                 target,
               };
             let program =
               Web.ProofSearchBackend.rocq_program_for_authorized_plan(
                 ~profile,
                 request,
                 plan,
               );
             write_text_file(path, program);
             check(
               bool,
               label ++ " emits exact profile replay",
               true,
               string_contains(
                 "Exact replay of the Hazel profile trace",
                 program,
               )
               && !string_contains("ERROR", program),
             );
             if (label == "rational polynomial cleanup") {
               check(
                 bool,
                 "multiple recorded transitions compose by equality transitivity",
                 true,
                 string_contains("exact (eq_trans", program)
                 && !string_contains("try rewrite <- H_hazel_step_", program),
               );
               let contextual =
                 Web.CoqProofExport.tactic_for_written_summary(
                   ~forall_str="forall x : R,",
                   ~domain=Web.CoqExport.Reals,
                   plan.summary,
                 );
               check(
                 bool,
                 "embedded derivation replay rewrites local transitions forward",
                 true,
                 string_contains("rewrite H_hazel_step_1.", contextual)
                 && string_contains("rewrite H_hazel_step_2.", contextual)
                 && !string_contains("exact (eq_trans", contextual),
               );
               let embedded_source = plus(Exp.int(1), target);
               let embedded_target = plus(Exp.int(1), source);
               let contextual_program =
                 Web.CoqProofExport.real_prelude
                 ++ "\nTheorem hazel_contextual_replay:forall x : R,"
                 ++ Web.CoqExport.string_of_d_for_domain(
                      ~domain=Web.CoqExport.Reals,
                      embedded_source,
                    )
                 ++ "="
                 ++ Web.CoqExport.string_of_d_for_domain(
                      ~domain=Web.CoqExport.Reals,
                      embedded_target,
                    )
                 ++ ".\nProof.\nintros.\n"
                 ++ contextual
                 ++ "\nQed.\n";
               write_text_file(
                 "/tmp/hazel_profile_contextual_rational_polynomial.v",
                 contextual_program,
               );
             };
           });
        let scalar_replay_step =
          Web.ProofTrace.prover_step(
            ~origin=Web.ProofTrace.Normalization,
            ~rule_id="arith.simplify_scalar_products",
            ~before_full_exp=scalar_source,
            ~after_full_exp=scalar_target,
            ~before_exp=scalar_source,
            ~after_exp=scalar_target,
            ~detail="normalize rational scalars around an opaque atom",
          );
        check(
          bool,
          "real scalar replay avoids ring on rational opaque expressions",
          true,
          Web.CoqProofExport.tactic_for_prover_step(
            ~domain=Web.CoqExport.Reals,
            scalar_replay_step,
          )
          |> string_contains("first [lra | field"),
        );
        check(
          bool,
          "an incorrect rational-polynomial target is rejected",
          false,
          switch (authorize(~profile, source, plus(target, Exp.int(1)))) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        let without_expansion =
          Axioms.profile_with_capability_disabled(
            profile,
            "alg.expand_polynomial",
          );
        switch (authorize(~profile=without_expansion, source, target)) {
        | Rejected(_) => ()
        | Authorized(plan) =>
          fail(
            "disabled polynomial expansion admitted route: "
            ++ String.concat(", ", plan.summary.rule_ids),
          )
        };
        let without_scalar_cleanup =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.simplify_scalar_products",
          );
        check(
          bool,
          "disabled scalar cleanup is not recovered by the expansion macro",
          false,
          switch (authorize(~profile=without_scalar_cleanup, source, target)) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        let noncommutative =
          Axioms.profile_with_capability_disabled(profile, "mul.comm");
        let commutativity_required_source =
          times(
            divide(Exp.int(1), Exp.int(2)),
            times(target, Exp.int(2)),
          );
        check(
          bool,
          "compact commutative polynomial cleanup is unavailable without MulComm",
          true,
          Web.RewriteChecker.rational_polynomial_expansion_trace_for_profile(
            ~profile=noncommutative,
            ~stage=Axioms.MultiStepCheck,
            commutativity_required_source,
            target,
          )
          |> Option.is_none,
        );
        let a = Exp.var("a");
        let b = Exp.var("b");
        let d = Exp.var("d");
        let structurally_different_source =
          times(
            Exp.int(3),
            plus(
              times(divide(Exp.int(1), Exp.int(3)), plus(a, b)),
              times(divide(Exp.int(2), Exp.int(3)), d),
            ),
          );
        let structurally_different_target =
          plus(plus(a, b), times(Exp.int(2), d));
        check(
          bool,
          "a structurally different rational distribution is authorized",
          true,
          switch (
            authorize(
              ~profile,
              structurally_different_source,
              structurally_different_target,
            )
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
      },
    ),
    test_case(
      "profile board default examples run through the real checker",
      `Quick,
      () => {
        let results = Web.ProfileBoard.run_default_examples(~settings, ~env);
        check(
          list(string),
          "example ids",
          [
            "arith.distribute.strict",
            "arith.distribute.folded",
            "alg.distribute.strict",
            "alg.distribute.simplified",
            "alg.distribute.ac",
            "alg.distribute.folded",
            "trig.pythagorean",
          ],
          results |> List.map(profile_board_result_id),
        );
        check(
          list(bool),
          "example statuses",
          [true, false, true, true, true, true, true],
          results |> List.map(profile_board_result_status),
        );
        switch (
          results
          |> List.find_opt((result: Web.ProfileBoard.example_result) =>
               result.example.id == "alg.distribute.ac"
             )
        ) {
        | Some(result) =>
          check(
            list(string),
            "AC distribution rule ids",
            ["alg.distribute_mul_add"],
            result.rule_ids,
          );
          check(
            list(string),
            "AC distribution cleanup labels",
            [
              "add.assoc",
              "add.comm",
              "mul.assoc",
              "mul.comm",
              "power.notation",
            ],
            result.cleanup_labels,
          );
        | None => fail("expected AC distribution board result")
        };
      },
    ),
    test_case(
      "profile board summarizes visible rules and cleanup",
      `Quick,
      () => {
        let summary =
          Web.ProfileBoard.profile_summary(Axioms.math_profile(Algebra));
        check(string, "level label", "Algebra", summary.level_label);
        check(
          list(string),
          "visible rule ids",
          [
            "arith.add_comm",
            "arith.const_fold",
            "arith.mul_const",
            "arith.mul_identity",
            "arith.simplify_scalar_products",
            "alg.distribute_mul_add",
            "alg.distribute_div_add",
            "alg.factor_common",
            "alg.cancel_common_add",
            "alg.collect_like_terms",
            "alg.expand_polynomial",
            "alg.difference_of_squares",
            "alg.square_of_sum",
            "alg.square_of_difference",
            "alg.difference_of_cubes",
            "alg.sum_of_cubes",
            "alg.cube_of_sum",
            "alg.cube_of_difference",
          ],
          summary.visible_rules |> List.map(visible_rule_summary_id),
        );
        switch (summary.visible_rules) {
        | [
            _add_comm,
            _const_fold,
            _mul_const,
            _mul_identity,
            _scalar_normalize,
            distribution,
            _division_distribution,
            _factor,
            _cancel,
            ..._,
          ] =>
          check(
            string,
            "distribution display name",
            "Distribute multiplication over addition",
            visible_rule_summary_name(distribution),
          );
          check(
            string,
            "distribution example",
            "x * (a + b) = x * a + x * b",
            visible_rule_summary_example(distribution),
          );
          check(
            list(string),
            "distribution cleanup",
            [
              "add.assoc",
              "add.comm",
              "mul.assoc",
              "mul.comm",
              "power.notation",
            ],
            visible_rule_summary_cleanup(distribution),
          );
        | _ => fail("expected algebra visible rule summaries")
        };
        check(
          list(string),
          "default cleanup",
          [
            "add.assoc",
            "add.comm",
            "mul.assoc",
            "mul.comm",
            "add.identity",
            "mul.identity",
            "const.fold",
            "collect.like_terms",
            "power.identity",
            "power.notation",
          ],
          summary.default_cleanup_labels,
        );
      },
    ),
    test_case(
      "trigonometry profile includes algebra and trig visible rules",
      `Quick,
      () => {
        let summary =
          Web.ProfileBoard.profile_summary(
            Axioms.math_profile(Trigonometry),
          );
        let rule_ids =
          summary.visible_rules |> List.map(visible_rule_summary_id);
        check(
          bool,
          "trig profile keeps algebra distribution",
          true,
          List.mem("alg.distribute_mul_add", rule_ids),
        );
        check(
          bool,
          "trig profile exposes sine sum identity",
          true,
          List.mem("trig.sin_sum", rule_ids),
        );
        check(
          bool,
          "trig profile exposes cofunction identity",
          true,
          List.mem("trig.sin_cofunction", rule_ids),
        );
      },
    ),
    test_case(
      "profile board update toggles draft policy knobs",
      `Quick,
      () => {
        let cleanup_model =
          Web.ProfileBoard.Model.init
          |> Web.ProfileBoard.Update.update(
               Web.ProfileBoard.Update.SetCleanupEnabled("mul.comm", false),
             );
        check(
          bool,
          "distribution starts enabled",
          true,
          Web.ProfileBoard.rule_enabled(
            cleanup_model,
            "alg.distribute_mul_add",
          ),
        );
        check(
          bool,
          "mul comm disabled",
          false,
          Web.ProfileBoard.cleanup_capability_enabled(
            cleanup_model,
            Axioms.MulComm,
          ),
        );
        check(
          list(string),
          "cleanup after update",
          ["add.assoc", "add.comm", "mul.assoc", "power.notation"],
          Web.ProfileBoard.cleanup_labels_for_rule(
            Web.ProfileBoard.apply_model_to_profile(
              cleanup_model,
              Axioms.math_profile(Algebra),
            ),
            "alg.distribute_mul_add",
          ),
        );
        let disabled_model =
          cleanup_model
          |> Web.ProfileBoard.Update.update(
               Web.ProfileBoard.Update.SetRuleEnabled(
                 "alg.distribute_mul_add",
                 false,
               ),
             );
        check(
          bool,
          "distribution disabled",
          false,
          Web.ProfileBoard.rule_enabled(
            disabled_model,
            "alg.distribute_mul_add",
          ),
        );
      },
    ),
    test_case(
      "polynomial expansion macro has stage-specific usage",
      `Quick,
      () => {
        let base_profile = Axioms.math_profile(Algebra);
        check(
          bool,
          "Algebra defaults to repeated distribution enabled",
          true,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(base_profile, Manual),
            "alg.expand_polynomial",
          ),
        );
        let profile =
          Axioms.profile_with_capability_usage_overrides(
            base_profile,
            [
              {
                capability_id: "alg.expand_polynomial",
                stage: Manual,
                usage: Disabled,
              },
            ],
          );
        check(
          bool,
          "Profile option disables repeated One Step distribution",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, Manual),
            "alg.expand_polynomial",
          ),
        );
        check(
          bool,
          "Check Result expansion selection remains enabled",
          true,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, MultiStepCheck),
            "alg.expand_polynomial",
          ),
        );
        check(
          bool,
          "primitive distribution remains enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.distribute_mul_add",
          ),
        );
        let reenabled = base_profile;
        check(
          bool,
          "re-enabling restores repeated One Step distribution",
          true,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(reenabled, Manual),
            "alg.expand_polynomial",
          ),
        );
      },
    ),
    test_case(
      "profile board math-level sections are independently collapsible",
      `Quick,
      () => {
        let model = Web.ProfileBoard.Model.init;
        check(
          bool,
          "active calculus section starts expanded",
          true,
          Web.ProfileBoard.section_expanded(
            ~active_level=Calculus,
            model,
            Calculus,
          ),
        );
        check(
          bool,
          "inherited trig section starts collapsed",
          false,
          Web.ProfileBoard.section_expanded(
            ~active_level=Calculus,
            model,
            Trigonometry,
          ),
        );
        let model =
          Web.ProfileBoard.Update.update(
            SetSectionExpanded("Trigonometry", true),
            model,
          );
        check(
          bool,
          "trig section expands independently",
          true,
          Web.ProfileBoard.section_expanded(
            ~active_level=Calculus,
            model,
            Trigonometry,
          ),
        );
      },
    ),
    test_case(
      "Algebra, Trig, and Calculus profile subgroups come from rule metadata",
      `Quick,
      () => {
        let subgroup = rule_id =>
          Axioms.visible_rule_metadata(rule_id).profile_group;
        check(
          option(string),
          "sine sum subgroup",
          Some("Sum and difference identities"),
          subgroup("trig.sin_sum"),
        );
        check(
          option(string),
          "Pythagorean subgroup",
          Some("Pythagorean identities"),
          subgroup("trig.pythagorean_sin_cos"),
        );
        check(
          option(string),
          "calculus linearity subgroup",
          Some("Linearity"),
          subgroup("calc.diff_sum"),
        );
        check(
          option(string),
          "trigonometric derivative subgroup",
          Some("Trigonometric chain rules"),
          subgroup("calc.diff_chain_cos"),
        );
        check(
          option(string),
          "Algebra distribution subgroup",
          Some("Distribution and factoring"),
          subgroup("alg.factor_common"),
        );
        check(
          option(string),
          "Algebra square identity subgroup",
          Some("Square identities"),
          subgroup("alg.square_of_sum"),
        );
        let subgroup_id = "visible-subgroup:Calculus:Linearity";
        check(
          bool,
          "nested subgroup starts collapsed",
          false,
          Web.ProfileBoard.named_section_expanded(
            ~default=false,
            Web.ProfileBoard.Model.init,
            subgroup_id,
          ),
        );
        let expanded =
          Web.ProfileBoard.Model.init
          |> Web.ProfileBoard.Update.update(
               SetSectionExpanded(subgroup_id, true),
             );
        check(
          bool,
          "nested subgroup expands independently",
          true,
          Web.ProfileBoard.named_section_expanded(
            ~default=false,
            expanded,
            subgroup_id,
          ),
        );
        let builder =
          Web.MathModeBuilder.Model.blank
          |> Web.MathModeBuilder.Update.update(
               SetOperationGroupExpanded("Distribution and factoring", true),
             );
        check(
          bool,
          "builder operation subgroup expands independently",
          true,
          List.mem(
            "Distribution and factoring",
            builder.Web.MathModeBuilder.Model.expanded_operation_groups,
          ),
        );
        check(
          bool,
          "builder exposes inherited Check Result-only normalizers",
          true,
          Web.MathModeBuilder.check_result_normalizers_for_level(Trigonometry)
          |> List.exists((rule: Axioms.math_rule) =>
               rule.id == "arith.affine_normalize"
             ),
        );
        check(
          bool,
          "builder does not duplicate visible operations as helpers",
          false,
          Web.MathModeBuilder.check_result_normalizers_for_level(Trigonometry)
          |> List.exists((rule: Axioms.math_rule) =>
               rule.id == "alg.expand_polynomial"
             ),
        );
        let without_affine_normalization =
          Web.MathModeBuilder.Model.blank
          |> Web.MathModeBuilder.Update.update(
               SetUsage(
                 "arith.affine_normalize",
                 Axioms.MultiStepCheck,
                 Axioms.Disabled,
               ),
             );
        switch (
          Web.MathModeBuilder.resolved_profile(without_affine_normalization)
        ) {
        | Ok(profile) =>
          check(
            bool,
            "builder Check Result helper control disables its normalizer",
            false,
            Axioms.normalization_rule_id_enabled_for_profile(
              profile,
              MultiStepCheck,
              "arith.affine_normalize",
            ),
          )
        | Error(error) =>
          fail(CustomMathMode.resolution_error_message(error))
        };
      },
    ),
    test_case(
      "custom profile can disable a trig visible rule",
      `Quick,
      () => {
        let theta = Exp.var("theta");
        let source =
          builtin_sin(minus(divide(Exp.var("pi"), Exp.int(2)), theta));
        let target = builtin_cos(theta);
        let profile =
          Web.ProfileBoard.apply_model_to_profile(
            Web.ProfileBoard.Model.init
            |> Web.ProfileBoard.Update.update(
                 Web.ProfileBoard.Update.SetRuleEnabled(
                   "trig.sin_cofunction",
                   false,
                 ),
               ),
            Axioms.math_profile(Trigonometry),
          );
        check(
          bool,
          "default trig profile accepts cofunction",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=Axioms.math_profile(Trigonometry),
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "disabled cofunction is rejected",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "custom profile cleanup changes one-step validation",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(plus(Exp.int(1), Exp.int(2)), x));
        let ordered_target =
          plus(
            plus(times(x, Exp.int(1)), times(x, Exp.int(2))),
            times(x, x),
          );
        let commuted_target =
          plus(
            plus(times(Exp.int(1), x), times(Exp.int(2), x)),
            times(x, x),
          );
        let strict_profile =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=[Axioms.AddAssoc, Axioms.MulAssoc],
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "ordered distribution still valid without comm cleanup",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=strict_profile,
            ~settings,
            ~env,
            source,
            ordered_target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "commuted distribution rejected without comm cleanup",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=strict_profile,
            ~settings,
            ~env,
            source,
            commuted_target,
          )
          |> Option.is_none,
        );
        check(
          bool,
          "default profile accepts commuted distribution",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=Axioms.math_profile(Algebra),
            ~settings,
            ~env,
            source,
            commuted_target,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "one step absorbs all profile-enabled association cleanup",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let d = Exp.var("d");
        let association_only =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=[Axioms.AddAssoc, Axioms.MulAssoc],
            Axioms.math_profile(Algebra),
          );
        let addition_source = plus(a, plus(b, plus(c, d)));
        let addition_target = plus(plus(plus(a, b), c), d);
        let multiplication_source = times(times(a, b), times(c, d));
        let multiplication_target = times(a, times(b, times(c, d)));
        let accepted = (profile, source, target) =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let addition_result =
          accepted(association_only, addition_source, addition_target);
        check(
          bool,
          "all nested addition parentheses are one cleanup step",
          true,
          addition_result |> Option.is_some,
        );
        check(
          bool,
          "the one learner step retains multiple primitive proof steps",
          true,
          addition_result
          |> Option.map((result: Web.RewriteChecker.check_result) =>
               List.length(result.prover_steps) >= 2
             )
          |> Option.value(~default=false),
        );
        check(
          bool,
          "nested multiplication parentheses are also cleanup",
          true,
          accepted(
            association_only,
            multiplication_source,
            multiplication_target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "association cleanup does not reorder terms",
          true,
          Web.RewriteChecker.association_cleanup_result_for_profile(
            ~profile=association_only,
            addition_source,
            plus(b, plus(a, plus(c, d))),
          )
          |> Option.is_none,
        );
        let association_disabled =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=[],
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "disabled association remains unavailable",
          true,
          Web.RewriteChecker.association_cleanup_result_for_profile(
            ~profile=association_disabled,
            addition_source,
            addition_target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "custom profile can disable distribution",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(Exp.int(1), x));
        let target = plus(times(x, Exp.int(1)), times(x, x));
        let profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "distribution disabled",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "collected FOIL one-step requires collect-like-terms",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          times(
            plus(times(Exp.int(2), x), Exp.int(3)),
            plus(x, Exp.int(4)),
          );
        let target =
          plus(
            plus(times(times(Exp.int(2), x), x), times(Exp.int(11), x)),
            Exp.int(12),
          );
        let algebra = Axioms.math_profile(Algebra);
        let without_collect =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              algebra.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.CollectLikeTerms
                 ),
            algebra,
          );
        let accepted = profile =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some;
        check(
          bool,
          "default Algebra accepts collected expansion",
          true,
          accepted(algebra),
        );
        check(
          bool,
          "disabled Collect rejects collected expansion",
          false,
          accepted(without_collect),
        );
        check(
          bool,
          "Calculus inherits collected Algebra expansion",
          true,
          accepted(Axioms.math_profile(Calculus)),
        );
      },
    ),
    test_case(
      "uncollected FOIL one-step does not require collect-like-terms",
      `Quick,
      () => {
        let x = Exp.var("x");
        let algebra = Axioms.math_profile(Algebra);
        let without_collect =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              algebra.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.CollectLikeTerms
                 ),
            algebra,
          );
        let accepted = (source, target) =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_collect,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some;
        let profile_trace = (source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile=without_collect,
            ~settings,
            ~env,
            source,
            target,
          );
        let simple_source =
          times(plus(x, Exp.int(1)), plus(x, Exp.int(2)));
        let simple_uncollected =
          plus(
            plus(
              plus(times(x, x), times(Exp.int(1), x)),
              times(Exp.int(2), x),
            ),
            Exp.int(2),
          );
        check(
          bool,
          "four-term FOIL remains valid without collection",
          true,
          accepted(simple_source, simple_uncollected),
        );
        check(
          bool,
          "Check Result replays uncollected FOIL through the profile",
          true,
          profile_trace(simple_source, simple_uncollected) |> Option.is_some,
        );
        check(
          bool,
          "coefficient FOIL remains valid without collection",
          true,
          accepted(
            times(
              plus(times(Exp.int(2), x), Exp.int(3)),
              plus(x, Exp.int(4)),
            ),
            plus(
              plus(
                plus(
                  times(times(Exp.int(2), x), x),
                  times(Exp.int(8), x),
                ),
                times(Exp.int(3), x),
              ),
              Exp.int(12),
            ),
          ),
        );
        let real_signed_source =
          real_times(
            real_minus(real_times(real(2), x), real(3)),
            real_plus(x, real(4)),
          );
        let real_signed_uncollected =
          real_minus(
            real_plus(
              real_minus(
                real_times(real_times(real(2), x), x),
                real_times(real(3), x),
              ),
              real_times(real_times(real(4), real(2)), x),
            ),
            real(12),
          );
        check(
          bool,
          "Real signed FOIL accepts inherited scalar cleanup",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_collect,
            ~settings,
            ~env,
            real_signed_source,
            real_signed_uncollected,
          )
          |> Option.is_some,
        );
        let wrong_real_signed_target =
          real_minus(
            real_plus(
              real_minus(
                real_times(real_times(real(2), x), x),
                real_times(real(3), x),
              ),
              real_times(real_times(real(5), real(2)), x),
            ),
            real(12),
          );
        check(
          bool,
          "Real signed FOIL still rejects an incorrect coefficient",
          false,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_collect,
            ~settings,
            ~env,
            real_signed_source,
            wrong_real_signed_target,
          )
          |> Option.is_some,
        );
        let without_real_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            without_collect,
          );
        check(
          bool,
          "disabled distribution blocks the same Real expansion",
          false,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_real_distribution,
            ~settings,
            ~env,
            real_signed_source,
            real_signed_uncollected,
          )
          |> Option.is_some,
        );
        let coefficient_source =
          times(
            plus(times(Exp.int(2), x), Exp.int(3)),
            plus(x, Exp.int(4)),
          );
        let coefficient_power_uncollected =
          plus(
            plus(
              plus(
                times(Exp.int(2), power(x, Exp.int(2))),
                times(Exp.int(8), x),
              ),
              times(Exp.int(3), x),
            ),
            Exp.int(12),
          );
        check(
          bool,
          "power notation composes with a coefficient product",
          true,
          accepted(coefficient_source, coefficient_power_uncollected),
        );
        check(
          bool,
          "Check Result replays coefficient power notation without collection",
          true,
          profile_trace(coefficient_source, coefficient_power_uncollected)
          |> Option.is_some,
        );
        let reverse_coefficient_source =
          times(
            plus(x, Exp.int(2)),
            plus(times(Exp.int(3), x), Exp.int(1)),
          );
        let reverse_coefficient_target =
          plus(
            plus(
              plus(times(Exp.int(3), power(x, Exp.int(2))), x),
              times(Exp.int(6), x),
            ),
            Exp.int(2),
          );
        check(
          bool,
          "power notation composes when the coefficient is in the other factor",
          true,
          accepted(reverse_coefficient_source, reverse_coefficient_target),
        );
        let cubic_source =
          times(
            plus(times(times(Exp.int(4), x), x), Exp.int(2)),
            plus(x, Exp.int(1)),
          );
        let cubic_target =
          plus(
            plus(
              plus(
                times(Exp.int(4), power(x, Exp.int(3))),
                times(Exp.int(4), power(x, Exp.int(2))),
              ),
              times(Exp.int(2), x),
            ),
            Exp.int(2),
          );
        check(
          bool,
          "coefficient cubic term matches repeated factors",
          true,
          Web.RewriteChecker.product_term_same_under_cleanup(
            without_collect.step_policy.default_cleanup,
            times(times(times(Exp.int(4), x), x), x),
            times(Exp.int(4), power(x, Exp.int(3))),
          ),
        );
        check(
          bool,
          "coefficient square term matches after identity cleanup",
          true,
          Web.RewriteChecker.product_term_same_under_cleanup(
            without_collect.step_policy.default_cleanup,
            times(times(times(Exp.int(4), x), x), Exp.int(1)),
            times(Exp.int(4), power(x, Exp.int(2))),
          ),
        );
        let cubic_repeated_target =
          plus(
            plus(
              plus(
                times(times(times(Exp.int(4), x), x), x),
                times(times(Exp.int(4), x), x),
              ),
              times(Exp.int(2), x),
            ),
            Exp.int(2),
          );
        check(
          bool,
          "cubic polynomial distribution works before power cleanup",
          true,
          accepted(cubic_source, cubic_repeated_target),
        );
        let cubic_only_power_target =
          plus(
            plus(
              plus(
                times(Exp.int(4), power(x, Exp.int(3))),
                times(times(Exp.int(4), x), x),
              ),
              times(Exp.int(2), x),
            ),
            Exp.int(2),
          );
        check(
          bool,
          "cubic power composes independently of square notation",
          true,
          accepted(cubic_source, cubic_only_power_target),
        );
        check(
          bool,
          "power notation composes for cubic polynomial terms",
          true,
          Web.RewriteChecker.uncollected_full_distribution_matches(
            without_collect,
            cubic_source,
            cubic_target,
          ),
        );
        check(
          bool,
          "cubic powered target remains polynomial-equivalent",
          true,
          Web.RewriteChecker.polynomial_equivalent_exps(
            cubic_source,
            cubic_target,
          ),
        );
        check(
          bool,
          "One Step accepts the composed cubic power target",
          true,
          accepted(cubic_source, cubic_target),
        );
        check(
          bool,
          "incorrect squared coefficient remains invalid",
          false,
          accepted(
            coefficient_source,
            plus(
              plus(
                plus(
                  times(Exp.int(4), power(x, Exp.int(2))),
                  times(Exp.int(8), x),
                ),
                times(Exp.int(3), x),
              ),
              Exp.int(12),
            ),
          ),
        );
        let without_power =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              without_collect.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.PowerNotation),
            without_collect,
          );
        let accepted_without_power = target =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_power,
            ~settings,
            ~env,
            coefficient_source,
            target,
          )
          |> Option.is_some;
        check(
          bool,
          "disabled power notation rejects the powered target",
          false,
          accepted_without_power(coefficient_power_uncollected),
        );
        check(
          bool,
          "disabled power notation still accepts repeated factors",
          true,
          accepted_without_power(
            plus(
              plus(
                plus(
                  times(times(Exp.int(2), x), x),
                  times(Exp.int(8), x),
                ),
                times(Exp.int(3), x),
              ),
              Exp.int(12),
            ),
          ),
        );
        let without_fold =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              algebra.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.ConstFold),
            algebra,
          );
        let constant_source =
          times(plus(x, Exp.int(2)), plus(x, Exp.int(3)));
        let constant_products =
          plus(
            plus(
              plus(times(x, x), times(x, Exp.int(3))),
              times(Exp.int(2), x),
            ),
            times(Exp.int(2), Exp.int(3)),
          );
        let folded_constant =
          plus(
            plus(
              plus(times(x, x), times(Exp.int(3), x)),
              times(Exp.int(2), x),
            ),
            Exp.int(6),
          );
        let accepted_without_fold = target =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_fold,
            ~settings,
            ~env,
            constant_source,
            target,
          )
          |> Option.is_some;
        let profile_trace_without_fold = target =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile=without_fold,
            ~settings,
            ~env,
            constant_source,
            target,
          );
        check(
          bool,
          "disabled constant folding retains the distributed product",
          true,
          accepted_without_fold(constant_products),
        );
        check(
          bool,
          "Check Result retains products without constant folding",
          true,
          profile_trace_without_fold(constant_products) |> Option.is_some,
        );
        check(
          bool,
          "enabled collection does not bypass disabled constant folding",
          false,
          accepted_without_fold(folded_constant),
        );
        check(
          bool,
          "Check Result cannot bypass disabled constant folding",
          true,
          profile_trace_without_fold(folded_constant) |> Option.is_none,
        );
        check(
          bool,
          "incorrect distributed coefficient is rejected",
          false,
          accepted(
            times(plus(x, Exp.int(1)), plus(x, Exp.int(2))),
            plus(
              plus(
                plus(times(x, x), times(Exp.int(1), x)),
                times(Exp.int(4), x),
              ),
              Exp.int(2),
            ),
          ),
        );
        check(
          bool,
          "Check Result does not replay collected FOIL without collection",
          true,
          profile_trace(
            simple_source,
            plus(plus(times(x, x), times(Exp.int(3), x)), Exp.int(2)),
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "calculus one-step accepts parsed builtin diff linearity",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          builtin_app("diff", Exp.tuple([plus(x, Exp.int(2)), x]));
        let target = plus(diff(x, x), diff(Exp.int(2), x));
        check(
          bool,
          "builtin diff sum is one calculus step",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "Check Result direct cleanup follows power toggles",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Algebra);
        let check_trace = (~profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let without = (capability, profile: Axioms.math_profile) =>
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(candidate => candidate != capability),
            profile,
          );
        check(
          bool,
          "x**1 cleanup is enabled",
          true,
          check_trace(~profile, power(x, Exp.int(1)), x) |> Option.is_some,
        );
        check(
          bool,
          "x**1 cleanup is disabled",
          false,
          check_trace(
            ~profile=without(Axioms.PowerIdentity, profile),
            power(x, Exp.int(1)),
            x,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "x*x to x**2 notation is enabled",
          true,
          check_trace(~profile, times(x, x), power(x, Exp.int(2)))
          |> Option.is_some,
        );
        check(
          bool,
          "x*x to x**2 notation is disabled",
          false,
          check_trace(
            ~profile=without(Axioms.PowerNotation, profile),
            times(x, x),
            power(x, Exp.int(2)),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "reported Algebra Check Result goals produce exact real replay",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Algebra);
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let replay = (label, path, source, target) => {
          let request = request(source, target);
          switch (local_profile_trace(~profile, ~settings, ~env, request)) {
          | Some(summary) =>
            let program =
              Web.ProofSearchBackend.rocq_replay_program(request, summary);
            write_text_file(path, program);
            check(
              bool,
              label ++ " uses real variables",
              true,
              string_contains(" : R,", program),
            );
            check(
              bool,
              label ++ " has a deterministic polynomial certificate",
              true,
              string_contains("ring", program)
              || string_contains("nra", program),
            );
          | None =>
            switch (
              Web.ProfileProofPlan.authorize({
                profile,
                stage: Axioms.MultiStepCheck,
                candidate_origin: Web.ProfileProofPlan.UserEntered,
                settings,
                env,
                source,
                target,
                max_depth: request.max_depth,
                max_states: request.max_states,
              })
            ) {
            | Web.ProfileProofPlan.Rejected(rejection) =>
              fail(
                label
                ++ " should have an enabled profile trace: "
                ++ Web.ProfileProofPlan.rejection_message(rejection),
              )
            | Authorized(_) =>
              fail(
                label ++ " compatibility facade dropped an authorized plan",
              )
            }
          };
        };
        replay(
          "factored polynomial",
          "/tmp/hazel_profile_factor_real.v",
          minus(
            plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
            Exp.int(4),
          ),
          times(minus(x, Exp.int(1)), plus(x, Exp.int(4))),
        );
        replay(
          "collected FOIL with power notation",
          "/tmp/hazel_profile_foil_real.v",
          times(
            plus(times(Exp.int(2), x), Exp.int(3)),
            plus(x, Exp.int(4)),
          ),
          plus(
            plus(
              times(Exp.int(2), power(x, Exp.int(2))),
              times(Exp.int(11), x),
            ),
            Exp.int(12),
          ),
        );
        replay(
          "power notation cleanup",
          "/tmp/hazel_profile_power_notation_real.v",
          times(x, x),
          power(x, Exp.int(2)),
        );
        let identity_request = request(power(x, Exp.int(1)), x);
        switch (
          local_profile_trace(~profile, ~settings, ~env, identity_request)
        ) {
        | Some(summary) =>
          let program =
            Web.ProofSearchBackend.rocq_replay_program(
              identity_request,
              summary,
            );
          write_text_file(
            "/tmp/hazel_profile_power_identity_real.v",
            program,
          );
          check(
            bool,
            "power identity uses its exact cleanup",
            true,
            string_contains("rewrite pow_1", program),
          );
        | None => fail("power identity should have an enabled profile trace")
        };
      },
    ),
    test_case(
      "disabled Trig equivalence fallback is bounded",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile =
          Axioms.math_profile(Trigonometry)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="trig.sin_double",
             );
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: builtin_sin(times(Exp.int(2), x)),
            target:
              times(times(Exp.int(2), builtin_sin(x)), builtin_cos(x)),
          };
        check(
          bool,
          "disabled identity has no profile trace",
          true,
          local_profile_trace(~profile, ~settings, ~env, request)
          |> Option.is_none,
        );
        let program =
          Web.ProofSearchBackend.rocq_equivalence_program_for_profile(
            ~profile,
            request,
          );
        check(
          bool,
          "fallback does not invoke broad recursive Trig search",
          false,
          string_contains("hazel_real_algebra", program),
        );
        check(
          bool,
          "fallback terminates at reflexivity",
          true,
          string_contains("intros.\nreflexivity.\nQed.", program),
        );
      },
    ),
    test_case(
      "Search suggestions respect disabled visible operations",
      `Quick,
      () => {
        let x = Exp.var("x");
        let rewrite =
          Web.TrigRewrite.{
            rule_id: "alg.distribute_mul_add",
            label: "distribute multiplication",
            before_exp: times(x, plus(Exp.int(1), x)),
            after_exp: plus(times(x, Exp.int(1)), times(x, x)),
          };
        let default_profile = Axioms.math_profile(Algebra);
        let disabled_profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            default_profile,
          );
        check(
          bool,
          "default Search includes distribution",
          true,
          Web.AxiomsBox.rewrite_enabled_for_profile(default_profile, rewrite),
        );
        check(
          bool,
          "custom Search excludes distribution",
          false,
          Web.AxiomsBox.rewrite_enabled_for_profile(
            disabled_profile,
            rewrite,
          ),
        );
      },
    ),
    test_case(
      "calculus Search suggestions show linearity before cleanup",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let polynomial =
          diff(
            plus(
              plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
              Exp.int(2),
            ),
            x,
          );
        let expected_polynomial_step =
          plus(
            plus(
              diff(power(x, Exp.int(2)), x),
              diff(times(Exp.int(3), x), x),
            ),
            diff(Exp.int(2), x),
          );
        let different_shape = diff(plus(sin(x), power(x, Exp.int(3))), x);
        let expected_different_shape_step =
          plus(diff(sin(x), x), diff(power(x, Exp.int(3)), x));
        let expect_linearity = (label, source, expected) =>
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
          ) {
          | [suggestion] =>
            check(
              string,
              label ++ " label",
              "linearity (sum rule)",
              suggestion.label,
            );
            check_exp_equal(label, expected, suggestion.after_exp);
          | _ => fail(label ++ " should have one linearity suggestion")
          };
        expect_linearity(
          "left-associated polynomial",
          polynomial,
          expected_polynomial_step,
        );
        expect_linearity(
          "function plus power",
          different_shape,
          expected_different_shape_step,
        );
        let disabled_profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_sum",
            profile,
          );
        check(
          int,
          "disabled linearity is not suggested",
          0,
          Web.AxiomsBox.calculus_actions_for_profile(
            ~profile=disabled_profile,
            polynomial,
          )
          |> List.length,
        );
      },
    ),
    test_case(
      "calculus Search suggestions preserve visible-rule boundaries",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let check_action = (name, source, rule_id, expected) =>
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
          ) {
          | [action] =>
            check(string, name ++ " rule", rule_id, action.rule_id);
            check_exp_equal(name ++ " target", expected, action.after_exp);
          | _ => fail(name ++ " should have exactly one visible action")
          };
        check_action(
          "product",
          diff(times(x, Exp.int(3)), x),
          "calc.diff_product",
          Exp.int(3),
        );
        check_action(
          "power",
          diff(power(x, Exp.int(3)), x),
          "calc.diff_power",
          times(Exp.int(3), power(x, Exp.int(2))),
        );
        check_action(
          "sine chain",
          diff(sin(power(x, Exp.int(2))), x),
          "calc.diff_chain_sin",
          times(
            Web.DifferentiationRewrite.app_exp("cos", power(x, Exp.int(2))),
            diff(power(x, Exp.int(2)), x),
          ),
        );
        check_action(
          "quotient",
          diff(divide(x, plus(x, Exp.int(1))), x),
          "calc.diff_quotient",
          divide(
            minus(
              plus(x, Exp.int(1)),
              times(x, diff(plus(x, Exp.int(1)), x)),
            ),
            power(plus(x, Exp.int(1)), Exp.int(2)),
          ),
        );
      },
    ),
    test_case(
      "calculus power and product cleanup follows the active profile",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let action = (~profile, source) =>
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
          ) {
          | [action] => action
          | _ => fail("expected one calculus action")
          };
        let power_source = diff(power(x, Exp.int(2)), x);
        let product_source = diff(times(x, Exp.int(3)), x);
        check_exp_equal(
          "power cleans to 2 * x",
          times(Exp.int(2), x),
          action(~profile, power_source).after_exp,
        );
        check_exp_equal(
          "product cleans to 3",
          Exp.int(3),
          action(~profile, product_source).after_exp,
        );
        let without = capability =>
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(candidate => candidate != capability),
            profile,
          );
        check_exp_equal(
          "disabled power identity stays visible",
          times(Exp.int(2), power(x, Exp.int(1))),
          action(~profile=without(Axioms.PowerIdentity), power_source).
            after_exp,
        );
        check_exp_equal(
          "disabled derivative basics stays visible",
          times(times(Exp.int(2), x), diff(x, x)),
          action(~profile=without(Axioms.DerivativeBasics), power_source).
            after_exp,
        );
        let without_power_or_basics =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.PowerIdentity
                   && capability != Axioms.DerivativeBasics
                 ),
            profile,
          );
        check_exp_equal(
          "disabled power identity and derivative basics stay fully visible",
          times(times(Exp.int(2), power(x, Exp.int(1))), diff(x, x)),
          action(~profile=without_power_or_basics, power_source).after_exp,
        );
        check_exp_equal(
          "disabled multiplicative identity stays visible",
          times(times(Exp.int(2), x), Exp.int(1)),
          action(~profile=without(Axioms.MulIdentity), power_source).
            after_exp,
        );
      },
    ),
    test_case(
      "all non-linearity calculus rules apply profile basic cleanup",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let without_basics =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.DerivativeBasics
                 ),
            profile,
          );
        let action = (~profile, ~rule_id, source) =>
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
            |> List.find_opt((rewrite: Web.TrigRewrite.rewrite) =>
                 rewrite.rule_id == rule_id
               )
          ) {
          | Some(action) => action
          | None => fail("expected calculus action " ++ rule_id)
          };
        [
          ("product", "calc.diff_product", diff(times(x, Exp.int(3)), x)),
          (
            "quotient",
            "calc.diff_quotient",
            diff(divide(x, Exp.int(3)), x),
          ),
          ("power", "calc.diff_power", diff(power(x, Exp.int(2)), x)),
          ("sine chain", "calc.diff_chain_sin", diff(builtin_sin(x), x)),
          ("cosine chain", "calc.diff_chain_cos", diff(builtin_cos(x), x)),
        ]
        |> List.iter(((label, rule_id, source)) => {
             let enabled = action(~profile, ~rule_id, source);
             let disabled = action(~profile=without_basics, ~rule_id, source);
             check(
               bool,
               label ++ " removes nested basic derivatives when enabled",
               false,
               Web.DifferentiationRewrite.contains_diff(enabled.after_exp),
             );
             check(
               bool,
               label ++ " preserves nested basic derivatives when disabled",
               true,
               Web.DifferentiationRewrite.contains_diff(disabled.after_exp),
             );
           });
        let sine_source = diff(builtin_sin(x), x);
        let sine_action =
          action(~profile, ~rule_id="calc.diff_chain_sin", sine_source);
        check_exp_equal(
          "sine chain cleans to cosine",
          Web.DifferentiationRewrite.app_exp("cos", x),
          sine_action.after_exp,
        );
        let sine_trace =
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Manual,
              candidate_origin: DisplayedSuggestion,
              settings,
              env,
              source: sine_source,
              target: sine_action.after_exp,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(plan) => plan.summary
          | Rejected(rejection) =>
            fail(Web.ProfileProofPlan.rejection_message(rejection))
          };
        check(
          bool,
          "sine cleanup remains explicit in the proof trace",
          true,
          List.mem("derivative.basics", sine_trace.rule_ids),
        );
      },
    ),
    test_case(
      "automatic calculus cleanup is explicit in the export trace",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let source = diff(times(x, Exp.int(3)), x);
        let action =
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
          ) {
          | [action] => action
          | _ => fail("expected one product action")
          };
        let summary =
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Manual,
              candidate_origin: DisplayedSuggestion,
              settings,
              env,
              source,
              target: action.after_exp,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(plan) => plan.summary
          | Rejected(rejection) =>
            fail(Web.ProfileProofPlan.rejection_message(rejection))
          };
        check(bool, "trace remains exportable", true, summary.exportable);
        check(
          bool,
          "trace includes product rule",
          true,
          List.mem("calc.diff_product", summary.rule_ids),
        );
        check(
          bool,
          "trace includes derivative cleanup",
          true,
          List.mem("derivative.basics", summary.rule_ids),
        );
        check(
          bool,
          "trace includes multiplicative cleanup",
          true,
          List.mem("mul.identity", summary.rule_ids),
        );
        check(
          bool,
          "trace includes additive cleanup",
          true,
          List.mem("add.identity", summary.rule_ids),
        );
        check_exp_equal(
          "trace ends at displayed result",
          action.after_exp,
          summary.prover_steps
          |> List.rev
          |> ListUtil.hd_opt
          |> Option.map((step: Web.ProofTrace.prover_step) => step.after_exp)
          |> Option.value(~default=source),
        );
        let replay =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile,
            source,
            action.after_exp,
          );
        check(
          bool,
          "Rocq export certifies the compact product result",
          true,
          switch (replay) {
          | Some(program) =>
            string_contains(
              "Hazel profile-directed derivative certificate",
              program,
            )
            && string_contains("H_hazel_derivative", program)
          | None => false
          },
        );
        let sum_source = diff(plus(x, Exp.int(2)), x);
        let sum_action =
          switch (
            Web.AxiomsBox.calculus_actions_for_profile(~profile, sum_source)
          ) {
          | [action] => action
          | _ => fail("expected one sum action")
          };
        let sum_summary =
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Manual,
              candidate_origin: DisplayedSuggestion,
              settings,
              env,
              source: sum_source,
              target: sum_action.after_exp,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(plan) => plan.summary
          | Rejected(rejection) =>
            fail(Web.ProfileProofPlan.rejection_message(rejection))
          };
        check(
          int,
          "linearity remains one visible step",
          1,
          List.length(sum_summary.prover_steps),
        );
        check_exp_equal(
          "linearity result remains explicit",
          plus(diff(x, x), diff(Exp.int(2), x)),
          sum_action.after_exp,
        );
      },
    ),
    test_case(
      "calculus Check Result preserves Float arithmetic",
      `Quick,
      () => {
        let t = Exp.var("t");
        let profile = Axioms.math_profile(Calculus);
        let authorize = (~profile=profile, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage: MultiStepCheck,
            candidate_origin: UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 6,
            max_states: 300,
          });
        let trig_body =
          float_plus(
            float_minus(
              float_divide(float(7.0), float(4.0)),
              builtin_cos(float_times(float(2.0), t)),
            ),
            float_times(
              float_divide(float(1.0), float(4.0)),
              builtin_cos(float_times(float(4.0), t)),
            ),
          );
        let trig_source =
          function_derivative(Exp.fn(Pat.var("t"), trig_body, None, None));
        let trig_target =
          Exp.fn(
            Pat.var("t"),
            float_minus(
              float_times(
                float(2.0),
                builtin_sin(float_times(float(2.0), t)),
              ),
              builtin_sin(float_times(float(4.0), t)),
            ),
            None,
            None,
          );
        switch (authorize(trig_source, trig_target)) {
        | Authorized(_) => ()
        | Rejected(rejection) =>
          let normalized =
            Web.DifferentiationRewrite.normalize(
              ~rule_enabled=
                rule_id =>
                  Axioms.visible_rule_enabled(profile.step_policy, rule_id),
              trig_source,
            );
          let cleaned =
            Web.DifferentiationRewrite.cleanup(
              ~cleanup_enabled=
                capability =>
                  List.mem(capability, profile.step_policy.default_cleanup),
              normalized.exp,
            );
          fail(
            Web.ProfileProofPlan.rejection_message(rejection)
            ++ "; normalized to "
            ++ Language.Exp.show(cleaned),
          );
        };
        let polynomial_source =
          expression_derivative(
            float_plus(
              float_power(t, float(3.0)),
              float_times(float(2.0), t),
            ),
            t,
          );
        let polynomial_target =
          float_plus(
            float_times(float(3.0), float_power(t, float(2.0))),
            float(2.0),
          );
        switch (authorize(polynomial_source, polynomial_target)) {
        | Authorized(_) => ()
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        let third_source =
          function_derivative(
            Exp.fn(
              Pat.var("t"),
              float_minus(
                float_times(
                  float(4.0),
                  builtin_cos(float_times(float(2.0), t)),
                ),
                float_times(
                  float(4.0),
                  builtin_cos(float_times(float(4.0), t)),
                ),
              ),
              None,
              None,
            ),
          );
        let third_target =
          Exp.fn(
            Pat.var("t"),
            float_plus(
              float_times(
                float_minus(float(0.0), float(8.0)),
                builtin_sin(float_times(float(2.0), t)),
              ),
              float_times(
                float(16.0),
                builtin_sin(float_times(float(4.0), t)),
              ),
            ),
            None,
            None,
          );
        switch (authorize(third_source, third_target)) {
        | Authorized(_) => ()
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        let wrong_target = Exp.fn(Pat.var("t"), float(0.0), None, None);
        check(
          bool,
          "wrong Float derivative rejected",
          true,
          switch (authorize(trig_source, wrong_target)) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        let without_cos_chain =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_chain_cos",
            profile,
          );
        check(
          bool,
          "disabled Float derivative rule rejected",
          true,
          switch (
            authorize(~profile=without_cos_chain, trig_source, trig_target)
          ) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
      },
    ),
    test_case(
      "calculus Search prioritizes structured constants and rejects invalid generic chain output",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let profile = Axioms.math_profile(Calculus);
        [
          ("constant sum", diff(plus(y, Exp.int(2)), x)),
          ("constant trig expression", diff(sin(Exp.int(2)), x)),
        ]
        |> List.iter(((name, source)) =>
             switch (
               Web.AxiomsBox.calculus_actions_for_profile(~profile, source)
             ) {
             | [action] =>
               check(
                 string,
                 name ++ " rule",
                 "calc.diff_constant",
                 action.rule_id,
               );
               check_exp_equal(
                 name ++ " target",
                 Exp.int(0),
                 action.after_exp,
               );
             | _ => fail(name ++ " should differentiate directly to zero")
             }
           );
        let unknown_function_chain =
          diff(
            Exp.ap(Operators.Forward, Exp.var("f"), power(x, Exp.int(2))),
            x,
          );
        check(
          int,
          "unknown-function chain is not emitted with an invalid diff argument",
          0,
          Web.AxiomsBox.calculus_actions_for_profile(
            ~profile,
            unknown_function_chain,
          )
          |> List.length,
        );
      },
    ),
    test_case(
      "calculus cleanup suggestions are exact sequential profile steps",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let derivative = diff(power(x, Exp.int(2)), x);
        let after_linearity = plus(derivative, diff(Exp.int(2), x));
        let after_constant = plus(derivative, Exp.int(0));
        let first =
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile,
            after_linearity,
          );
        let second =
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile,
            after_constant,
          );
        switch (first) {
        | [action] =>
          check(
            string,
            "first cleanup capability",
            "derivative.basics",
            action.rule_id,
          );
          check_exp_equal(
            "first cleanup preserves additive zero",
            after_constant,
            action.after_exp,
          );
        | _ => fail("expected one derivative cleanup suggestion")
        };
        switch (second) {
        | [action] =>
          check(
            string,
            "second cleanup capability",
            "add.identity",
            action.rule_id,
          );
          check_exp_equal(
            "second cleanup removes only additive zero",
            derivative,
            action.after_exp,
          );
        | _ => fail("expected one additive cleanup suggestion")
        };
        let power_zero_derivative = diff(power(x, Exp.int(0)), x);
        switch (
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile,
            power_zero_derivative,
          )
        ) {
        | [action] =>
          check(
            string,
            "power-zero cleanup capability",
            "power.identity",
            action.rule_id,
          );
          check_exp_equal(
            "power-zero cleanup stays explicit inside diff",
            diff(Exp.int(1), x),
            action.after_exp,
          );
        | _ => fail("expected one power-zero cleanup suggestion")
        };
        switch (
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile,
            power(x, Exp.int(1)),
          )
        ) {
        | [action] =>
          check_exp_equal("power-one cleanup target", x, action.after_exp)
        | _ => fail("expected one power-one cleanup suggestion")
        };
        let disabled =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.DerivativeBasics
                 ),
            profile,
          );
        check(
          int,
          "disabled derivative cleanup is not suggested",
          0,
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile=disabled,
            after_linearity,
          )
          |> List.length,
        );
      },
    ),
    test_case(
      "algebra Search labels named polynomial expansion before prerequisites",
      `Quick,
      () => {
      check(
        string,
        "named expansion wins over distribution prerequisite",
        "expand polynomial",
        Web.AxiomsBox.algebra_shape_label([
          "alg.expand_polynomial",
          "alg.distribute_mul_add",
          "alg.collect_like_terms",
        ]),
      )
    }),
    test_case(
      "typed proof-search verdicts control labels and replacement",
      `Quick,
      () => {
        let verdicts = [
          Web.MissingStep.Model.Ready,
          Checking,
          ProfileValid,
          EquivalentOutsideProfile,
          Invalid,
        ];
        check(
          list(string),
          "verdict labels",
          [
            "Ready",
            "Rocq checking...",
            "Valid",
            "Equivalent, but blocked by profile",
            "Invalid",
          ],
          verdicts
          |> List.map(verdict =>
               Web.MissingStep.proof_search_verdict_label(
                 ~has_candidate=false,
                 verdict,
               )
             ),
        );
        check(
          string,
          "Algebrite remains an unvalidated candidate",
          "Candidate ready",
          Web.MissingStep.proof_search_verdict_label(
            ~has_candidate=true,
            Ready,
          ),
        );
        check(
          list(bool),
          "only profile-valid proof can replace",
          [false, false, true, false, false],
          verdicts |> List.map(Web.MissingStep.proof_search_can_replace),
        );
        check(
          string,
          "expected outside-profile rejection hides raw Rocq pretty-print data",
          "No proof is available using the active Profile.",
          Web.MissingStep.proof_search_failure_message(
            ~has_profile_trace=false,
            "JSCoq failed: Pp_glue Pp_string Unable to unify",
          ),
        );
        check(
          string,
          "failed exact replay is identified as a certificate problem",
          "Rocq could not verify the enabled Profile proof certificate. See the browser console for details.",
          Web.MissingStep.proof_search_failure_message(
            ~has_profile_trace=true,
            "JSCoq failed: Pp_glue Pp_string No applicable tactic",
          ),
        );
        check(
          string,
          "worker failures remain distinguishable from invalid results",
          "Rocq checker failed unexpectedly. See the browser console for details.",
          Web.MissingStep.proof_search_failure_message(
            ~has_profile_trace=false,
            "JSCoq failed: Stack overflow.",
          ),
        );
        let cancellation_summary: Web.ProofTrace.trace_summary = {
          justification: "algebra one step",
          group_name: Some("Algebra"),
          from_normal_exp: Exp.var("x"),
          to_normal_exp: Exp.var("x"),
          from_rule_ids: [],
          to_rule_ids: [],
          rule_ids: ["alg.cancel_common_add"],
          prover_steps: [],
          exportable: true,
        };
        check(
          string,
          "valid verdict exposes its named Profile route",
          "Cancel a common additive term",
          Web.MissingStep.proof_search_route_label(cancellation_summary),
        );
      },
    ),
    test_case(
      "open proof box invalidates cached state when math level changes",
      `Quick,
      () => {
        check(
          bool,
          "same level keeps the current result",
          false,
          Web.MissingStep.proof_search_state_is_stale(
            ~calculated_rewrite_level=Some(Axioms.Algebra),
            ~rewrite_level=Axioms.Algebra,
            ~calculated_automation_stage=Some(Axioms.MultiStepCheck),
            ~automation_stage=Axioms.MultiStepCheck,
            ~target_exp_changed=false,
            ~proof_search_source=None,
          ),
        );
        check(
          bool,
          "new level invalidates the current result",
          true,
          Web.MissingStep.proof_search_state_is_stale(
            ~calculated_rewrite_level=Some(Axioms.Algebra),
            ~rewrite_level=Axioms.Calculus,
            ~calculated_automation_stage=Some(Axioms.MultiStepCheck),
            ~automation_stage=Axioms.MultiStepCheck,
            ~target_exp_changed=false,
            ~proof_search_source=Some("automatic candidate"),
          ),
        );
        check(
          bool,
          "new automation mode invalidates the current result",
          true,
          Web.MissingStep.proof_search_state_is_stale(
            ~calculated_rewrite_level=Some(Axioms.Algebra),
            ~rewrite_level=Axioms.Algebra,
            ~calculated_automation_stage=Some(Axioms.MultiStepCheck),
            ~automation_stage=Axioms.Manual,
            ~target_exp_changed=false,
            ~proof_search_source=None,
          ),
        );
        check(
          bool,
          "manual mode uses one-step validation",
          true,
          Web.MissingStep.check_mode_for_automation_stage(Axioms.Manual)
          == Web.MissingStep.Model.SingleEvalStep,
        );
        check(
          bool,
          "automatic mode uses profile search",
          true,
          Web.MissingStep.check_mode_for_automation_stage(Axioms.AutoEval)
          == Web.MissingStep.Model.ProofSearch,
        );
      },
    ),
    test_case(
      "open rewrite boxes refresh a changed source selection",
      `Quick,
      () => {
        let original = times(Exp.int(4), Exp.int(4));
        let replacement_source = plus(Exp.int(3), Exp.int(16));
        let (refreshed, changed) =
          Web.MissingStep.refresh_captured_source(
            ~captured=Some(original),
            ~live=Some(replacement_source),
          );
        let (retained, focus_only) =
          Web.MissingStep.refresh_captured_source(
            ~captured=Some(original),
            ~live=None,
          );
        check(bool, "new structural selection is detected", true, changed);
        check(
          bool,
          "captured source follows the new selection",
          true,
          Web.MissingStep.option_exp_equal(
            refreshed,
            Some(replacement_source),
          ),
        );
        check(
          bool,
          "mini-editor focus is not a source change",
          false,
          focus_only,
        );
        check(
          bool,
          "mini-editor focus retains the captured source",
          true,
          Web.MissingStep.option_exp_equal(retained, Some(original)),
        );
      },
    ),
    test_case(
      "proof-search cancellation resets only the active check",
      `Quick,
      () => {
        let check_id = 42;
        let checking_model =
          Web.MissingStep.Model.{
            ...init,
            open_box:
              WrittenStepOpen({
                editor:
                  Web.CodeEditable.Model.mk(
                    Haz3lcore.Editor.Model.mk(
                      ~root=Exp,
                      Haz3lcore.Zipper.init(),
                    ),
                  ),
                check_mode: ProofSearch,
                axioms_model: Web.AxiomsBox.Model.init,
                rewrite_selected_exp: None,
                rewrite_reparenthesized_exp: None,
                source_full_visible_exp: None,
                proof_search_requested: true,
                proof_search_verdict: Checking,
                proof_search_check_id: Some(check_id),
                proof_search_message: Some("Rocq checking..."),
                proof_search_max_depth: 4,
                proof_search_max_states: 80,
                proof_search_source: None,
                calculated_rewrite_level: None,
                calculated_automation_stage: None,
                cached_exp: Calc.Pending,
                cached_result: Calc.Pending,
              }),
          };
        let stale_result =
          Web.MissingStep.Update.update(
            ~settings=Web.Settings.Model.init,
            RocqProofSearchCancelled(check_id + 1),
            checking_model,
          ).
            model;
        let cancelled_result =
          Web.MissingStep.Update.update(
            ~settings=Web.Settings.Model.init,
            RocqProofSearchCancelled(check_id),
            checking_model,
          ).
            model;
        let state = model =>
          switch (model.Web.MissingStep.Model.open_box) {
          | WrittenStepOpen({
              proof_search_requested,
              proof_search_verdict,
              proof_search_check_id,
              _,
            }) => (
              proof_search_requested,
              proof_search_verdict,
              proof_search_check_id,
            )
          | _ => fail("expected written-step proof-search box")
          };
        check(
          bool,
          "stale cancellation leaves the active search alone",
          true,
          state(stale_result) == (true, Checking, Some(check_id)),
        );
        check(
          bool,
          "matching cancellation records Cancelled",
          true,
          state(cancelled_result) == (false, Cancelled, None),
        );
      },
    ),
    test_case(
      "proof-search request IDs are unique without relying on wall-clock time",
      `Quick,
      () => {
        let first = Web.ProofSearchBackend.fresh_search_id();
        let second = Web.ProofSearchBackend.fresh_search_id();
        check(bool, "successive IDs differ", true, first != second);
      },
    ),
    test_case(
      "incremental axiom search preserves positive negative and disabled-rule behavior",
      `Quick,
      () => {
        let finish = progress => {
          let rec loop = (slices, progress) =>
            switch (Web.AxiomSearch.continue_search(~work_budget=1, progress)) {
            | SearchComplete(result) => (slices + 1, result)
            | SearchPending(_) as progress => loop(slices + 1, progress)
            };
          loop(0, progress);
        };
        let x = Exp.var("x");
        let y = Exp.var("y");
        let z = Exp.var("z");
        let distribution_source = times(x, plus(y, z));
        let distribution_target = plus(times(x, y), times(x, z));
        let (distribution_slices, distribution_result) =
          Web.AxiomSearch.start_search(
            ~level=Axioms.Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.distribute_mul_add"],
            distribution_source,
            distribution_target,
          )
          |> finish;
        let (_identity_slices, identity_result) =
          Web.AxiomSearch.start_search(
            ~level=Axioms.Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["arith.add_zero"],
            plus(x, Exp.int(0)),
            x,
          )
          |> finish;
        let (_negative_slices, negative_result) =
          Web.AxiomSearch.start_search(
            ~level=Axioms.Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["arith.add_zero"],
            plus(x, Exp.int(0)),
            plus(x, Exp.int(1)),
          )
          |> finish;
        let (_disabled_slices, disabled_result) =
          Web.AxiomSearch.start_search(
            ~level=Axioms.Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["arith.mul_identity"],
            plus(x, Exp.int(0)),
            x,
          )
          |> finish;
        check(
          bool,
          "distribution spans task slices",
          true,
          distribution_slices > 1,
        );
        check(
          bool,
          "distribution remains provable",
          true,
          Option.is_some(distribution_result),
        );
        check(
          bool,
          "different identity remains provable",
          true,
          Option.is_some(identity_result),
        );
        check(
          bool,
          "wrong target remains rejected",
          true,
          Option.is_none(negative_result),
        );
        check(
          bool,
          "disabled required rule remains rejected",
          true,
          Option.is_none(disabled_result),
        );
      },
    ),
    test_case(
      "effective math profile lowers preserved trig rewrites",
      `Quick,
      () => {
        let x = Exp.var("x");
        let quotient_square =
          divide(
            minus(Exp.int(1), builtin_cos(times(Exp.int(2), x))),
            Exp.int(2),
          );
        let profile =
          Axioms.effective_profile_for_rewrite(
            ~requested_level=Trigonometry,
            power(quotient_square, Exp.int(2)),
            times(quotient_square, quotient_square),
          );
        check(
          string,
          "preserved trig lowers to algebra tactic",
          "hazel_algebra",
          profile.Axioms.rocq_tactic_group,
        );
        check(
          string,
          "preserved trig lowers to algebra macro",
          "rocq.algebra_tactic_search",
          profile.rocq_macro_rule_id,
        );

        let trig_profile =
          Axioms.effective_profile_for_rewrite(
            ~requested_level=Trigonometry,
            plus(
              power(builtin_sin(x), Exp.int(2)),
              power(builtin_cos(x), Exp.int(2)),
            ),
            Exp.int(1),
          );
        check(
          string,
          "changed trig stays trig tactic",
          "hazel_trigonometry",
          trig_profile.rocq_tactic_group,
        );
      },
    ),
    test_case(
      "algebrite suggestion serializes trig power expression",
      `Quick,
      () => {
        let x = Exp.var("x");
        let expr =
          power(
            divide(
              minus(Exp.int(1), builtin_cos(times(Exp.int(2), x))),
              Exp.int(2),
            ),
            Exp.int(2),
          );
        check(
          option(string),
          "Algebrite input",
          Some("(((1 - cos((2 * x))) / 2) ^ 2)"),
          Web.AlgebriteSuggestion.serialize_for_algebrite(expr),
        );
      },
    ),
    test_case(
      "algebrite suggestion normalizes output back to Hazel syntax",
      `Quick,
      () => {
        let hazel_text =
          Web.AlgebriteSuggestion.hazel_syntax_of_algebrite(
            "1/2-cos(2*x)+1/2*cos(2*x)^2",
          );
        check(
          string,
          "Hazel power syntax",
          "1/2-cos(2*x)+1/2*cos(2*x)**2",
          hazel_text,
        );
        check(
          bool,
          "candidate seeds editor",
          true,
          Web.AlgebriteSuggestion.editor_of_hazel_text(
            ~settings=CoreSettings.on,
            hazel_text,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "algebrite factoring is offered only for expanded polynomials",
      `Quick,
      () => {
        let x = Exp.var("x");
        let expanded =
          minus(
            plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
            Exp.int(4),
          );
        let factored = times(minus(x, Exp.int(1)), plus(x, Exp.int(4)));
        check(
          bool,
          "expanded quadratic is a factor candidate",
          true,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(expanded),
        );
        check(
          bool,
          "already-factored product is not a factor candidate",
          false,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(factored),
        );
        check(
          bool,
          "monomial is not a factor candidate",
          false,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(
            times(Exp.int(3), power(x, Exp.int(2))),
          ),
        );
        check(
          bool,
          "trig expression is not a factor candidate",
          false,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(
            plus(power(builtin_sin(x), Exp.int(2)), Exp.int(1)),
          ),
        );
        check(
          bool,
          "multivariable polynomial is outside the factor boundary",
          false,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(
            plus(times(x, Exp.var("y")), x),
          ),
        );
        check(
          bool,
          "cubic polynomial is outside the factor boundary",
          false,
          Web.AlgebriteSuggestion.is_factor_candidate_shape(
            minus(power(x, Exp.int(3)), Exp.int(1)),
          ),
        );
        check(
          bool,
          "factored output seeds the Hazel editor",
          true,
          Web.AlgebriteSuggestion.editor_of_hazel_text(
            ~settings=CoreSettings.on,
            "(x-1)*(x+4)",
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "algebrite factor control follows level inheritance and profile overrides",
      `Quick,
      () => {
        let enabled = level =>
          Web.AlgebriteSuggestion.factor_suggestion_enabled_for_profile(
            Axioms.math_profile(level),
          );
        check(bool, "enabled in Algebra", true, enabled(Algebra));
        check(bool, "hidden in Arithmetic", false, enabled(Arithmetic));
        check(
          bool,
          "inherited by Trigonometry",
          true,
          enabled(Trigonometry),
        );
        check(
          bool,
          "inherited by Functions/lists",
          true,
          enabled(FunctionsAndLists),
        );
        check(bool, "inherited by Calculus", true, enabled(Calculus));
        let inherited_exercise_profile =
          Web.ExerciseMathPolicy.make(
            ~id="factor-inheritance-test",
            ~label="Factor inheritance test",
            ~detail="Exercise profile inheriting Calculus",
            ~parent_level=Calculus,
            ~automation_stage=MultiStepCheck,
            (),
          )
          |> Web.ExerciseMathPolicy.resolved_profile;
        check(
          bool,
          "inherited by a Calculus exercise profile",
          true,
          Web.AlgebriteSuggestion.factor_suggestion_enabled_for_profile(
            inherited_exercise_profile,
          ),
        );
        let disabled_profile =
          Axioms.profile_with_capability_disabled(
            Axioms.math_profile(Algebra),
            "alg.factor_polynomial_normalize",
          );
        check(
          bool,
          "profile can disable factor control",
          false,
          Web.AlgebriteSuggestion.factor_suggestion_enabled_for_profile(
            disabled_profile,
          ),
        );
      },
    ),
    test_case(
      "arithmetic group carries level metadata",
      `Quick,
      () => {
        check(
          bool,
          "arithmetic group level",
          true,
          rewrite_group_level(Axioms.arithmetic_rewrite_group) == Arithmetic,
        );
        check(
          int,
          "arithmetic group rank",
          Axioms.rewrite_level_rank(Arithmetic),
          Axioms.arithmetic_rewrite_group.rank,
        );
      },
    ),
    test_case(
      "algebra group is cataloged and enabled",
      `Quick,
      () => {
        check(
          bool,
          "algebra group level",
          true,
          rewrite_group_level(Axioms.algebra_rewrite_group) == Algebra,
        );
        check(
          int,
          "algebra group rank",
          Axioms.rewrite_level_rank(Algebra),
          Axioms.algebra_rewrite_group.rank,
        );
        check(
          bool,
          "has distribution rule",
          true,
          Axioms.algebra_rewrite_group.rules
          |> List.exists(rule =>
               rewrite_rule_id(rule) == "alg.distribute_mul_add"
             ),
        );
        check(
          bool,
          "algebra UI enabled",
          true,
          Axioms.rewrite_level_enabled(Algebra),
        );
      },
    ),
    test_case(
      "trigonometry group is cataloged and cumulative",
      `Quick,
      () => {
        check(
          list(string),
          "trig includes earlier groups",
          ["arithmetic", "algebra", "trigonometry"],
          Axioms.allowed_groups(Trigonometry) |> List.map(rewrite_group_name),
        );
        check(
          bool,
          "trig group level",
          true,
          rewrite_group_level(Axioms.trigonometry_rewrite_group)
          == Trigonometry,
        );
        check(
          bool,
          "has cofunction rule",
          true,
          Axioms.trigonometry_rewrite_group.rules
          |> List.exists(rule =>
               rewrite_rule_id(rule) == "trig.sin_cofunction"
             ),
        );
      },
    ),
    test_case(
      "proof search reports variables below algebra mode",
      `Quick,
      () => {
        let message =
          Web.AxiomSearch.unsupported_constructs_message(
            ~level=Arithmetic,
            [plus(Exp.var("x"), Exp.int(1))],
          );
        check(
          option(string),
          "variable gate message",
          Some("Needs Algebra"),
          message,
        );
      },
    ),
    test_case(
      "proof search reports trig below trigonometry mode",
      `Quick,
      () => {
        let exp = builtin_sin(Exp.var("x"));
        let message =
          Web.AxiomSearch.unsupported_constructs_message(
            ~level=Algebra,
            [exp],
          );
        check(
          option(string),
          "trig gate message",
          Some("Needs Trigonometry"),
          message,
        );
        check(
          bool,
          "trig gate decorates function application",
          true,
          Web.AxiomSearch.unsupported_construct_ids(~level=Algebra, [exp])
          |> List.mem(Language.Exp.rep_id(exp)),
        );
      },
    ),
    test_case("affine commutes variable addition", `Quick, () =>
      check_written(
        "3 + x = x + 3",
        plus(Exp.int(3), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("affine checking uses selected cumulative level", `Quick, () =>
      check_written_at_level(
        "algebra currently includes arithmetic",
        Algebra,
        plus(Exp.int(3), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("algebra level accepts distribution", `Quick, () =>
      check_written_result_at_level(
        "x * (y + z) = x * y + x * z",
        Algebra,
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        plus(
          times(Exp.var("x"), Exp.var("y")),
          times(Exp.var("x"), Exp.var("z")),
        ),
        Some("algebra"),
      )
    ),
    test_case("arithmetic level rejects distribution", `Quick, () =>
      check_written_result_at_level(
        "arithmetic cannot distribute x * (y + z)",
        Arithmetic,
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        plus(
          times(Exp.var("x"), Exp.var("y")),
          times(Exp.var("x"), Exp.var("z")),
        ),
        None,
      )
    ),
    test_case(
      "arithmetic level accepts integer distribution as one step",
      `Quick,
      () => {
        let source = times(Exp.int(2), plus(Exp.int(1), Exp.int(2)));
        let distributed =
          plus(
            times(Exp.int(2), Exp.int(1)),
            times(Exp.int(2), Exp.int(2)),
          );
        check_written_result_at_level(
          "2 * (1 + 2) = 2 * 1 + 2 * 2",
          Arithmetic,
          source,
          distributed,
          Some("arithmetic"),
        );
        let result =
          require_single_step_result_at_level(
            Arithmetic,
            source,
            distributed,
          );
        check(
          bool,
          "integer distribution uses arithmetic constant multiplication",
          true,
          has_trace_rule("arith.mul_const", result),
        );
      },
    ),
    test_case(
      "arithmetic one step rejects distribution plus constant folding",
      `Quick,
      () =>
      check(
        bool,
        "2 * (1 + 2) cannot jump directly to 6 as one step",
        true,
        Web.RewriteChecker.check_single_step_trace_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          times(Exp.int(2), plus(Exp.int(1), Exp.int(2))),
          Exp.int(6),
        )
        |> Option.is_none,
      )
    ),
    test_case(
      "algebra level accepts factoring as reverse distribution", `Quick, () =>
      check_written_result_at_level(
        "x * y + x * z = x * (y + z)",
        Algebra,
        plus(
          times(Exp.var("x"), Exp.var("y")),
          times(Exp.var("x"), Exp.var("z")),
        ),
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        Some("algebra"),
      )
    ),
    test_case("algebra accepts left-side distribution", `Quick, () =>
      check_written_result_at_level(
        "(y + z) * x = y * x + z * x",
        Algebra,
        times(plus(Exp.var("y"), Exp.var("z")), Exp.var("x")),
        plus(
          times(Exp.var("y"), Exp.var("x")),
          times(Exp.var("z"), Exp.var("x")),
        ),
        Some("algebra"),
      )
    ),
    test_case("algebra factors common right-side term", `Quick, () =>
      check_written_result_at_level(
        "y * x + z * x = x * (y + z)",
        Algebra,
        plus(
          times(Exp.var("y"), Exp.var("x")),
          times(Exp.var("z"), Exp.var("x")),
        ),
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        Some("algebra"),
      )
    ),
    test_case("algebra factors mixed-side common term", `Quick, () =>
      check_written_result_at_level(
        "x * y + z * x = x * (y + z)",
        Algebra,
        plus(
          times(Exp.var("x"), Exp.var("y")),
          times(Exp.var("z"), Exp.var("x")),
        ),
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        Some("algebra"),
      )
    ),
    test_case("algebra factors swapped addends", `Quick, () =>
      check_written_result_at_level(
        "x * z + x * y = x * (y + z)",
        Algebra,
        plus(
          times(Exp.var("x"), Exp.var("z")),
          times(Exp.var("x"), Exp.var("y")),
        ),
        times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
        Some("algebra"),
      )
    ),
    test_case(
      "algebra factoring trace records factor and distribution rules",
      `Quick,
      () => {
        let result =
          require_written_result_at_level(
            Algebra,
            plus(
              times(Exp.var("y"), Exp.var("x")),
              times(Exp.var("z"), Exp.var("x")),
            ),
            times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
          );
        check(
          bool,
          "trace has factoring",
          true,
          has_trace_rule("alg.factor_common", result),
        );
        check(
          bool,
          "trace has distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case("algebra expands FOIL product", `Quick, () =>
      check_written_result_at_level(
        "(x + 1) * (x + 2) = x * x + 3 * x + 2",
        Algebra,
        times(
          plus(Exp.var("x"), Exp.int(1)),
          plus(Exp.var("x"), Exp.int(2)),
        ),
        plus(
          plus(
            times(Exp.var("x"), Exp.var("x")),
            times(Exp.int(3), Exp.var("x")),
          ),
          Exp.int(2),
        ),
        Some("algebra"),
      )
    ),
    test_case("algebra collects expanded FOIL terms", `Quick, () =>
      check_written_result_at_level(
        "x * x + 2 * x + x + 2 = x * x + 3 * x + 2",
        Algebra,
        plus(
          plus(
            plus(
              times(Exp.var("x"), Exp.var("x")),
              times(Exp.int(2), Exp.var("x")),
            ),
            Exp.var("x"),
          ),
          Exp.int(2),
        ),
        plus(
          plus(
            times(Exp.var("x"), Exp.var("x")),
            times(Exp.int(3), Exp.var("x")),
          ),
          Exp.int(2),
        ),
        Some("algebra"),
      )
    ),
    test_case(
      "algebra accepts power notation for polynomial degree", `Quick, () =>
      check_written_result_at_level(
        "(x + 1) * (x + 2) = x ** 2 + 3 * x + 2",
        Algebra,
        times(
          plus(Exp.var("x"), Exp.int(1)),
          plus(Exp.var("x"), Exp.int(2)),
        ),
        plus(
          plus(
            power(Exp.var("x"), Exp.int(2)),
            times(Exp.int(3), Exp.var("x")),
          ),
          Exp.int(2),
        ),
        Some("algebra"),
      )
    ),
    test_case("arithmetic still rejects FOIL product", `Quick, () =>
      check_written_result_at_level(
        "arithmetic cannot FOIL (x + 1) * (x + 2)",
        Arithmetic,
        times(
          plus(Exp.var("x"), Exp.int(1)),
          plus(Exp.var("x"), Exp.int(2)),
        ),
        plus(
          plus(
            times(Exp.var("x"), Exp.var("x")),
            times(Exp.int(3), Exp.var("x")),
          ),
          Exp.int(2),
        ),
        None,
      )
    ),
    test_case(
      "scope experiment: arithmetic does not collect opaque trig factors",
      `Quick,
      () => {
        let x = Exp.var("x");
        let cos_x = builtin_cos(x);
        check_written_result_at_level(
          "arithmetic keeps trig applications out of affine collection",
          Arithmetic,
          plus(times(Exp.int(2), cos_x), times(Exp.int(3), cos_x)),
          times(Exp.int(5), cos_x),
          None,
        );
      },
    ),
    test_case(
      "scope experiment: algebra distributes across preserved trig calls",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(Exp.int(2), plus(builtin_cos(x), Exp.int(1)));
        let target =
          plus(
            times(Exp.int(2), builtin_cos(x)),
            times(Exp.int(2), Exp.int(1)),
          );
        check(
          bool,
          "arithmetic cannot distribute over an unchanged trig call",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Arithmetic,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_none,
        );
        check(
          option(string),
          "local algebra treats preserved trig calls as opaque terms",
          Some("algebra one step"),
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Algebra,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.map((trace: Web.ProofTrace.trace_summary) =>
               trace.justification
             ),
        );
        check(
          option(string),
          "rewrite-aware UI gate accepts preserved trig calls at algebra",
          None,
          Web.AxiomSearch.unsupported_constructs_message_for_rewrite(
            ~level=Algebra,
            ~source,
            ~target,
          ),
        );
        check(
          option(string),
          "arithmetic still rejects the same rewrite",
          Some("Needs Trigonometry"),
          Web.AxiomSearch.unsupported_constructs_message_for_rewrite(
            ~level=Arithmetic,
            ~source,
            ~target,
          ),
        );
        check(
          bool,
          "bounded axiom search can prove the algebra step",
          true,
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.distribute_mul_add"],
            ~log=false,
            source,
            target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "suggestion target is available at algebra",
          true,
          switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
          | Some((suggested, rule_ids)) =>
            List.mem("alg.distribute_mul_add", rule_ids)
            && Web.RewriteChecker.exp_same(suggested, target)
          | None => false
          },
        );
      },
    ),
    test_case(
      "scope experiment: trig identity does not leak into arithmetic",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          plus(
            power(builtin_sin(x), Exp.int(2)),
            power(builtin_cos(x), Exp.int(2)),
          );
        check_written_result_at_level(
          "arithmetic cannot use pythagorean trig identity",
          Arithmetic,
          source,
          Exp.int(1),
          None,
        );
        check_written_result_at_level(
          "algebra cannot use pythagorean trig identity",
          Algebra,
          source,
          Exp.int(1),
          None,
        );
        check_written_result_at_level(
          "trigonometry can use pythagorean trig identity",
          Trigonometry,
          source,
          Exp.int(1),
          Some("trigonometry one step"),
        );
      },
    ),
    test_case(
      "calculus inherits trig checking and respects a disabled trig rule",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let source = builtin_sin(plus(x, y));
        let target =
          plus(
            times(builtin_sin(x), builtin_cos(y)),
            times(builtin_cos(x), builtin_sin(y)),
          );
        let calculus_profile = Axioms.math_profile(Calculus);
        let check_with_profile = profile =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        check(
          bool,
          "calculus accepts inherited sine-sum rule",
          true,
          check_with_profile(calculus_profile) |> Option.is_some,
        );
        let without_sine_sum =
          calculus_profile
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="trig.sin_sum",
             );
        check(
          bool,
          "disabling inherited sine-sum blocks it in calculus",
          false,
          check_with_profile(without_sine_sum) |> Option.is_some,
        );
        check(
          bool,
          "disabled sine-sum is absent from Check Result stage plan",
          false,
          Axioms.stage_plan_for_profile(without_sine_sum, MultiStepCheck).
            visible_rules
          |> List.exists((planned: Axioms.planned_visible_rule) =>
               planned.rule.id == "trig.sin_sum"
             ),
        );
      },
    ),
    test_case(
      "algebra polynomial trace records expansion",
      `Quick,
      () => {
        let result =
          require_written_result_at_level(
            Algebra,
            times(
              plus(Exp.var("x"), Exp.int(1)),
              plus(Exp.var("x"), Exp.int(2)),
            ),
            plus(
              plus(
                times(Exp.var("x"), Exp.var("x")),
                times(Exp.int(3), Exp.var("x")),
              ),
              Exp.int(2),
            ),
          );
        check(
          bool,
          "trace has polynomial expansion",
          true,
          has_trace_rule("alg.expand_polynomial", result),
        );
        check(
          bool,
          "trace has collect like terms",
          true,
          has_trace_rule("alg.collect_like_terms", result),
        );
      },
    ),
    test_case(
      "algebra profile validates quadratic expansion without Rocq search",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(plus(x, Exp.int(1)), minus(x, Exp.int(2)));
        let target = minus(minus(power(x, Exp.int(2)), x), Exp.int(2));
        let algebra_profile = Axioms.math_profile(Algebra);
        let check_with_profile = profile =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let enabled_result = check_with_profile(algebra_profile);
        check(
          bool,
          "enabled algebra profile finds local trace",
          true,
          enabled_result |> Option.is_some,
        );
        switch (enabled_result) {
        | Some(summary) =>
          check(
            bool,
            "local trace records distribution",
            true,
            List.mem("alg.distribute_mul_add", summary.rule_ids),
          );
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Algebra,
              max_depth: 4,
              max_states: 80,
              source,
              target,
            };
          let coq =
            Web.ProofSearchBackend.rocq_replay_program(request, summary);
          check(
            bool,
            "FOIL replay coalesces one transition with multiple rule ids",
            false,
            string_contains("H_hazel_step_2", coq),
          );
          check(
            bool,
            "real signed FOIL replay prepares square notation",
            true,
            string_contains("unfold Rsqr; nra", coq),
          );
        | None => fail("expected enabled algebra profile trace")
        };
        let without_distribution =
          algebra_profile
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_mul_add",
             );
        check(
          bool,
          "disabled distribution blocks the expansion",
          false,
          check_with_profile(without_distribution) |> Option.is_some,
        );
      },
    ),
    test_case(
      "disabled polynomial expansion blocks composite FOIL traces",
      `Quick,
      () => {
        let x = Exp.var("x");
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let d = Exp.var("d");
        let profile = Axioms.math_profile(Algebra);
        let without_expansion =
          Axioms.profile_with_capability_disabled(
            profile,
            "alg.expand_polynomial",
          );
        let trace = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let numeric_source =
          times(
            plus(times(Exp.int(2), x), Exp.int(3)),
            plus(x, Exp.int(4)),
          );
        let numeric_target =
          plus(
            plus(
              times(Exp.int(2), power(x, Exp.int(2))),
              times(Exp.int(11), x),
            ),
            Exp.int(12),
          );
        let enabled = trace(profile, numeric_source, numeric_target);
        check(
          bool,
          "enabled expansion accepts collected coefficient FOIL",
          true,
          switch (enabled) {
          | Some(summary) =>
            List.mem("alg.expand_polynomial", summary.rule_ids)
          | None => false
          },
        );
        check(
          bool,
          "disabled expansion rejects collected coefficient FOIL",
          false,
          trace(without_expansion, numeric_source, numeric_target)
          |> Option.is_some,
        );
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source: numeric_source,
            target: numeric_target,
          };
        check(
          bool,
          "disabled expansion is not recovered by later profile search",
          false,
          local_profile_trace(
            ~profile=without_expansion,
            ~settings,
            ~env,
            request,
          )
          |> Option.is_some,
        );
        let symbolic_source = times(plus(a, b), plus(c, d));
        let symbolic_target =
          plus(
            plus(plus(times(a, c), times(a, d)), times(b, c)),
            times(b, d),
          );
        check(
          bool,
          "disabled expansion rejects structurally different full FOIL",
          false,
          trace(without_expansion, symbolic_source, symbolic_target)
          |> Option.is_some,
        );
        let single_distribution_target =
          plus(times(plus(a, b), c), times(plus(a, b), d));
        check(
          bool,
          "single enabled distribution remains available",
          true,
          switch (
            trace(
              without_expansion,
              symbolic_source,
              single_distribution_target,
            )
          ) {
          | Some(summary) =>
            List.mem("alg.distribute_mul_add", summary.rule_ids)
            && !List.mem("alg.expand_polynomial", summary.rule_ids)
          | None => false
          },
        );
        let incorrect_target =
          plus(
            plus(
              times(Exp.int(2), power(x, Exp.int(2))),
              times(Exp.int(10), x),
            ),
            Exp.int(12),
          );
        check(
          bool,
          "incorrect polynomial remains invalid",
          false,
          trace(profile, numeric_source, incorrect_target) |> Option.is_some,
        );
      },
    ),
    test_case(
      "One Step can limit distribution without limiting Check Result",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(minus(x, Exp.int(2)), plus(x, Exp.int(5)));
        let right_distribution =
          plus(
            times(minus(x, Exp.int(2)), x),
            times(minus(x, Exp.int(2)), Exp.int(5)),
          );
        let left_distribution =
          minus(
            times(x, plus(x, Exp.int(5))),
            times(Exp.int(2), plus(x, Exp.int(5))),
          );
        let full_expansion =
          minus(
            plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
            Exp.int(10),
          );
        let base_profile = Axioms.math_profile(Algebra);
        let single_distribution_profile =
          Axioms.profile_with_capability_usage_overrides(
            base_profile,
            [
              {
                capability_id: "alg.expand_polynomial",
                stage: Manual,
                usage: Disabled,
              },
            ],
          );
        let one_step = (profile, target) =>
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        [
          ("distribute the right factor", right_distribution),
          ("distribute the left factor", left_distribution),
        ]
        |> List.iter(((label, target)) =>
             switch (one_step(single_distribution_profile, target)) {
             | Some(result) =>
               check(
                 bool,
                 label ++ " records primitive distribution",
                 true,
                 has_trace_rule("alg.distribute_mul_add", result)
                 && !has_trace_rule("alg.expand_polynomial", result),
               )
             | None => fail(label ++ " should remain a valid One Step result")
             }
           );
        check(
          bool,
          "complete FOIL is not one step when repetition is disabled",
          false,
          one_step(single_distribution_profile, full_expansion)
          |> Option.is_some,
        );
        check(
          bool,
          "complete FOIL remains one step when repetition is enabled",
          true,
          one_step(base_profile, full_expansion) |> Option.is_some,
        );
        let check_result =
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile=single_distribution_profile,
            ~settings,
            ~env,
            source,
            full_expansion,
          );
        check(
          bool,
          "Check Result remains independent from One Step granularity",
          true,
          switch (check_result) {
          | Some(summary) =>
            List.mem("alg.expand_polynomial", summary.rule_ids)
          | None => false
          },
        );
        let incorrect =
          minus(
            plus(power(x, Exp.int(2)), times(Exp.int(4), x)),
            Exp.int(10),
          );
        check(
          bool,
          "inequivalent expansion remains invalid",
          false,
          one_step(base_profile, incorrect) |> Option.is_some,
        );
      },
    ),
    test_case(
      "Calculus Check Result replays composite polynomial normalization",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let trace = (~profile, source, target) =>
          local_profile_trace(
            ~profile,
            ~settings,
            ~env,
            request(source, target),
          );
        let source =
          plus(
            plus(
              plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
              Exp.int(5),
            ),
            times(Exp.int(3), x),
          );
        let target =
          plus(
            plus(power(x, Exp.int(2)), times(Exp.int(6), x)),
            Exp.int(5),
          );
        let summary =
          switch (trace(~profile, source, target)) {
          | Some(summary) => summary
          | None => fail("expected Calculus polynomial profile trace")
          };
        check(
          bool,
          "trace records polynomial expansion",
          true,
          List.mem("alg.expand_polynomial", summary.rule_ids),
        );
        check(
          bool,
          "trace records like-term collection",
          true,
          List.mem("alg.collect_like_terms", summary.rule_ids),
        );
        let coq =
          Web.ProofSearchBackend.rocq_replay_program(
            request(source, target),
            summary,
          );
        write_text_file("/tmp/hazel_calculus_polynomial_replay.v", coq);
        check(
          bool,
          "composite normalization has one replay assertion",
          false,
          string_contains("H_hazel_step_2", coq),
        );
        check(
          bool,
          "composite replay uses its profile-authorized polynomial certificate",
          true,
          string_contains("unfold Rsqr; nra", coq),
        );
        check(
          bool,
          "exact replay does not include recursive Hazel search",
          false,
          string_contains("hazel_rewrite_search", coq),
        );
        check(
          bool,
          "exact replay omits unrelated tactic definitions",
          false,
          string_contains("Ltac hazel_trigonometry", coq),
        );
        check(
          bool,
          "exact replay program stays compact",
          true,
          String.length(coq) < 1500,
        );
        check(
          bool,
          "all Check Result replay quantifies variables over reals",
          true,
          string_contains("forall x : R", coq)
          && string_contains("Open Scope R_scope.", coq)
          && !string_contains("Open Scope Z_scope.", coq),
        );
        let y = Exp.var("y");
        check(
          bool,
          "structurally different expansion is admitted",
          true,
          trace(
            ~profile,
            times(plus(y, Exp.int(2)), plus(y, Exp.int(3))),
            plus(plus(times(y, y), times(Exp.int(5), y)), Exp.int(6)),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "incorrect collected coefficient is rejected",
          false,
          trace(
            ~profile,
            source,
            plus(plus(times(x, x), times(Exp.int(7), x)), Exp.int(5)),
          )
          |> Option.is_some,
        );
        let without_collection =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.CollectLikeTerms
                 ),
            profile,
          );
        check(
          bool,
          "disabled collection cleanup blocks the composite trace",
          false,
          trace(~profile=without_collection, source, target) |> Option.is_some,
        );
      },
    ),
    test_case(
      "named algebra identities use a stable real polynomial certificate",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let source = times(plus(x, y), minus(x, y));
        let target = minus(power(x, Exp.int(2)), power(y, Exp.int(2)));
        let group =
          switch (Axioms.rewrite_group_by_name("algebra")) {
          | Some(group) => group
          | None => fail("expected algebra rewrite group")
          };
        let summary =
          switch (
            Web.RewriteChecker.check_single_algebra_identity(
              group,
              Axioms.math_profile(Algebra),
              source,
              target,
            )
          ) {
          | Some(result) =>
            Web.RewriteChecker.trace_summary_of_result(result)
          | None => fail("expected difference-of-squares profile trace")
          };
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq =
          Web.ProofSearchBackend.rocq_replay_program(request, summary);
        check(
          bool,
          "named real identity avoids JSCoq's first-ring overflow",
          true,
          string_contains("unfold Rsqr; nra", coq),
        );
        check(
          bool,
          "single identity transition is replayed directly",
          false,
          string_contains("H_hazel_step_", coq),
        );
      },
    ),
    test_case(
      "calculus cleanup replay handles identity orientations",
      `Quick,
      () => {
        let source = plus(Exp.int(6), Exp.int(0));
        let target = Exp.int(6);
        let step: Web.ProofTrace.prover_step = {
          origin: Normalization,
          rule_id: "add.identity",
          before_full_exp: source,
          after_full_exp: target,
          before_exp: source,
          after_exp: target,
          occurrence: 1,
          detail: Some("selected one profile cleanup rewrite"),
        };
        let summary: Web.ProofTrace.trace_summary = {
          justification: "calculus cleanup",
          group_name: Some("calculus"),
          from_normal_exp: source,
          to_normal_exp: target,
          from_rule_ids: ["add.identity"],
          to_rule_ids: [],
          rule_ids: ["add.identity"],
          prover_steps: [step],
          exportable: true,
        };
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq =
          Web.ProofSearchBackend.rocq_replay_program(request, summary);
        write_text_file("/tmp/hazel_calculus_add_identity_replay.v", coq);
        check(
          bool,
          "right identity replay is non-failing",
          true,
          string_contains("repeat rewrite Rplus_0_r", coq),
        );
        check(
          bool,
          "single whole-expression transition replays directly",
          true,
          !string_contains("H_hazel_step_1", coq),
        );
        let profile = Axioms.math_profile(Calculus);
        [
          (plus(Exp.int(0), Exp.int(6)), Exp.int(6)),
          (plus(Exp.int(6), Exp.int(0)), Exp.int(6)),
          (minus(Exp.int(6), Exp.int(0)), Exp.int(6)),
          (times(Exp.int(1), Exp.int(6)), Exp.int(6)),
          (times(Exp.int(6), Exp.int(1)), Exp.int(6)),
        ]
        |> List.iter(((source, expected)) =>
             switch (
               Web.AxiomsBox.calculus_cleanup_actions_for_profile(
                 ~profile,
                 source,
               )
             ) {
             | [action] =>
               check_exp_equal("identity cleanup", expected, action.after_exp)
             | _ => fail("expected one identity cleanup action")
             }
           );
        let without_add_identity =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.AddIdentity),
            profile,
          );
        check(
          int,
          "disabled additive identity has no cleanup action",
          0,
          Web.AxiomsBox.calculus_cleanup_actions_for_profile(
            ~profile=without_add_identity,
            source,
          )
          |> List.length,
        );
      },
    ),
    test_case(
      "algebra polynomial trace records additive cancellation",
      `Quick,
      () => {
        let result =
          require_written_result_at_level(
            Algebra,
            minus(
              plus(times(Exp.var("x"), Exp.var("x")), Exp.var("y")),
              Exp.var("y"),
            ),
            times(Exp.var("x"), Exp.var("x")),
          );
        check(
          bool,
          "trace has cancellation",
          true,
          has_trace_rule("alg.cancel_common_add", result),
        );
        check(
          bool,
          "from trace has cancellation",
          true,
          result.from_trace
          |> List.exists(rule =>
               rewrite_rule_id(rule) == "alg.cancel_common_add"
             ),
        );
        check(
          bool,
          "to trace does not invent cancellation",
          false,
          result.to_trace
          |> List.exists(rule =>
               rewrite_rule_id(rule) == "alg.cancel_common_add"
             ),
        );
      },
    ),
    test_case(
      "Trig Check Result admits profile-enabled nested power normalization",
      `Quick,
      () => {
        let x = Exp.var("x");
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: LocalAxiomSearch,
            level: Trigonometry,
            max_depth: 2,
            max_states: 80,
            source,
            target,
          };
        let profile = Axioms.math_profile(Trigonometry);
        let check_trace = (~profile, source, target) =>
          local_profile_trace(
            ~profile,
            ~settings,
            ~env,
            request(source, target),
          );
        let cases = [
          (
            power(builtin_sin(x), Exp.int(4)),
            power(power(builtin_sin(x), Exp.int(2)), Exp.int(2)),
          ),
          (
            power(builtin_cos(plus(x, Exp.int(1))), Exp.int(6)),
            power(
              power(builtin_cos(plus(x, Exp.int(1))), Exp.int(2)),
              Exp.int(3),
            ),
          ),
        ];
        cases
        |> List.iter(((source, target)) =>
             switch (check_trace(~profile, source, target)) {
             | Some(summary) =>
               check(
                 bool,
                 "trace records catalogued power normalization",
                 true,
                 List.mem("alg.power_mul", summary.rule_ids),
               )
             | None => fail("expected profile-enabled nested power trace")
             }
           );
        check(
          bool,
          "non-equivalent nested power is rejected",
          false,
          check_trace(
            ~profile,
            power(builtin_sin(x), Exp.int(4)),
            power(power(builtin_sin(x), Exp.int(2)), Exp.int(3)),
          )
          |> Option.is_some,
        );
        let without_power_cleanup =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.PowerNotation),
            profile,
          );
        let source = power(builtin_sin(x), Exp.int(4));
        let target = power(power(builtin_sin(x), Exp.int(2)), Exp.int(2));
        check(
          bool,
          "disabled power cleanup blocks power normalization",
          false,
          check_trace(~profile=without_power_cleanup, source, target)
          |> Option.is_some,
        );
        let without_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            profile,
          );
        check(
          bool,
          "disabled catalog prerequisite blocks power normalization",
          false,
          check_trace(~profile=without_distribution, source, target)
          |> Option.is_some,
        );
      },
    ),
    test_case("affine folds and reorders constants", `Quick, () =>
      check_written(
        "(1 + 2) + x = x + 3",
        plus(plus(Exp.int(1), Exp.int(2)), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("affine scales symbolic terms by constants", `Quick, () =>
      check_written(
        "2 * x + 3 = x + x + 3",
        plus(times(Exp.int(2), Exp.var("x")), Exp.int(3)),
        plus(plus(Exp.var("x"), Exp.var("x")), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("non-affine multiplication is rejected", `Quick, () =>
      check_written(
        "x * x != x + x",
        times(Exp.var("x"), Exp.var("x")),
        plus(Exp.var("x"), Exp.var("x")),
        None,
      )
    ),
    test_case("affine subtraction normalizes coefficients", `Quick, () =>
      check_written(
        "x + 3 - x = 3",
        minus(plus(Exp.var("x"), Exp.int(3)), Exp.var("x")),
        Exp.int(3),
        Some("arithmetic"),
      )
    ),
    test_case(
      "polynomial normalization renders negative trailing terms as subtraction",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          plus(plus(plus(x, x), negate(Exp.int(1))), Exp.int(0));
        let expected = minus(times(Exp.int(2), x), Exp.int(1));
        let actual =
          Web.RewriteChecker.simplify_for_profile(
            ~profile=Axioms.math_profile(Calculus),
            ~settings,
            ~env,
            source,
          )
          |> Option.get;
        check_exp_equal(
          "negative constant uses subtraction syntax",
          expected,
          actual,
        );
        check_written_at_level(
          "subtraction result remains profile-certifiable",
          Calculus,
          source,
          expected,
          Some("arithmetic"),
        );
      },
    ),
    test_case(
      "single eval step accepts one evaluator transition",
      `Quick,
      () => {
        let result =
          require_single_eval_result(
            plus(Exp.int(1), Exp.int(2)),
            Exp.int(3),
          );
        check(bool, "not exportable yet", false, result.exportable);
        check(
          option(string),
          "no rule group yet",
          None,
          result.group |> Option.map(rewrite_group_name),
        );
        check(int, "no trusted trace yet", 0, result.trace |> List.length);
      },
    ),
    test_case("single eval step rejects pure algebraic reordering", `Quick, () =>
      check(
        bool,
        "3 + x -> x + 3 is not one evaluator step",
        true,
        Web.RewriteChecker.check_single_eval_step_trace(
          ~settings,
          ~env,
          plus(Exp.int(3), Exp.var("x")),
          plus(Exp.var("x"), Exp.int(3)),
        )
        |> Option.is_none,
      )
    ),
    test_case(
      "single arithmetic step accepts one adjacent addition swap",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Arithmetic,
            plus(
              plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
              Exp.int(4),
            ),
            plus(
              plus(plus(Exp.int(1), Exp.int(2)), Exp.int(4)),
              Exp.int(3),
            ),
          );
        check(
          string,
          "justification",
          "arithmetic one step",
          result.justification,
        );
        check(bool, "exportable", true, result.exportable);
        check(
          bool,
          "trace has commutativity",
          true,
          has_trace_rule("arith.add_comm", result),
        );
      },
    ),
    test_case(
      "single arithmetic step records manual prover replay step",
      `Quick,
      () => {
        let from_ =
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            Exp.int(4),
          );
        let to_ =
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(4)),
            Exp.int(3),
          );
        let result =
          require_single_step_result_at_level(Arithmetic, from_, to_);
        switch (result.prover_steps) {
        | [step] =>
          check(string, "origin", "manual", prover_step_origin(step));
          check_prover_step(
            "manual adjacent swap",
            "arith.add_comm",
            from_,
            to_,
            step,
          );
        | _ => fail("expected one manual arithmetic prover step")
        };
      },
    ),
    test_case(
      "single arithmetic step folds middle adjacent constants", `Quick, () =>
      check(
        bool,
        "1 + 2 + 3 + 4 -> 1 + 5 + 4",
        true,
        Web.RewriteChecker.check_single_step_trace_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            Exp.int(4),
          ),
          plus(plus(Exp.int(1), Exp.int(5)), Exp.int(4)),
        )
        |> Option.is_some,
      )
    ),
    test_case(
      "single arithmetic step folds right adjacent constants", `Quick, () =>
      check(
        bool,
        "1 + 2 + 3 + 4 -> 1 + 2 + 7",
        true,
        Web.RewriteChecker.check_single_step_trace_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            Exp.int(4),
          ),
          plus(plus(Exp.int(1), Exp.int(2)), Exp.int(7)),
        )
        |> Option.is_some,
      )
    ),
    test_case("single arithmetic step rejects two addition swaps", `Quick, () =>
      check(
        bool,
        "1 + 2 + 3 + 4 -> 2 + 1 + 4 + 3 is too large",
        true,
        Web.RewriteChecker.check_single_step_trace_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            Exp.int(4),
          ),
          plus(
            plus(plus(Exp.int(2), Exp.int(1)), Exp.int(4)),
            Exp.int(3),
          ),
        )
        |> Option.is_none,
      )
    ),
    test_case(
      "single algebra step accepts one distribution",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
            plus(
              times(Exp.var("x"), Exp.var("y")),
              times(Exp.var("x"), Exp.var("z")),
            ),
          );
        check(
          string,
          "justification",
          "algebra one step",
          result.justification,
        );
        check(bool, "exportable", true, result.exportable);
        check(
          option(string),
          "group",
          Some("algebra"),
          result.group |> Option.map(rewrite_group_name),
        );
        check(
          bool,
          "trace has distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case(
      "algebra one step removes multiplicative identity",
      `Quick,
      () => {
        let x = Exp.var("x");
        [times(Exp.int(1), x), times(x, Exp.int(1))]
        |> List.iter(source => {
             let result =
               require_single_step_result_at_level(Algebra, source, x);
             check(
               bool,
               "trace has multiplicative identity",
               true,
               has_trace_rule("arith.mul_identity", result),
             );
             check(
               bool,
               "identity step is exportable",
               true,
               result.exportable,
             );
           });
        let disabled_profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="arith.mul_identity",
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "disabled multiplicative identity is rejected",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=disabled_profile,
            ~settings,
            ~env,
            times(Exp.int(1), x),
            x,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "algebra one step supports named square and cube identities",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let identity_env = [("a", a), ("b", b)];
        Web.AlgebraIdentityRewrite.specs
        |> List.iter((spec: Web.TrigRewrite.spec) => {
             let left = Web.TrigRewrite.instantiate(spec.left, identity_env);
             let right =
               Web.TrigRewrite.instantiate(spec.right, identity_env);
             [(left, right), (right, left)]
             |> List.iter(((source, target)) => {
                  let result =
                    require_single_step_result_at_level(
                      Algebra,
                      source,
                      target,
                    );
                  check(
                    bool,
                    spec.rule_id ++ " trace",
                    true,
                    has_trace_rule(spec.rule_id, result),
                  );
                  check(
                    bool,
                    spec.rule_id ++ " exportable",
                    true,
                    result.exportable,
                  );
                });
           });
        let square_sum = power(plus(a, b), Exp.int(2));
        let wrong_coefficient =
          plus(
            plus(power(a, Exp.int(2)), times(times(Exp.int(3), a), b)),
            power(b, Exp.int(2)),
          );
        check(
          bool,
          "wrong square coefficient is not a named identity",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=Axioms.math_profile(Algebra),
            ~settings,
            ~env,
            square_sum,
            wrong_coefficient,
          )
          |> Option.is_none,
        );
        let square_sum_target =
          plus(
            plus(power(a, Exp.int(2)), times(times(Exp.int(2), a), b)),
            power(b, Exp.int(2)),
          );
        let disabled_profile =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.square_of_sum",
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "disabled square-of-sum identity is rejected",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=disabled_profile,
            ~settings,
            ~env,
            square_sum,
            square_sum_target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "single algebra step records manual prover replay step",
      `Quick,
      () => {
        let from_ = times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
        let to_ =
          plus(
            times(Exp.var("x"), Exp.var("y")),
            times(Exp.var("x"), Exp.var("z")),
          );
        let result = require_single_step_result_at_level(Algebra, from_, to_);
        switch (result.prover_steps) {
        | [step] =>
          check(string, "origin", "manual", prover_step_origin(step));
          check_prover_step(
            "manual distribution",
            "alg.distribute_mul_add",
            from_,
            to_,
            step,
          );
        | _ => fail("expected one manual algebra prover step")
        };
      },
    ),
    test_case(
      "algebra shape target distributes subtraction",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          times(
            Exp.int(2),
            minus(Exp.int(2), builtin_cos(times(Exp.int(2), x))),
          );
        let expected =
          minus(
            times(Exp.int(2), Exp.int(2)),
            times(Exp.int(2), builtin_cos(times(Exp.int(2), x))),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("distributed subtraction target", expected, actual);
          check(
            bool,
            "records distribution",
            true,
            List.mem("alg.distribute_mul_add", rule_ids),
          );
        | None => fail("expected subtraction distribution target")
        };
      },
    ),
    test_case(
      "algebra shape target distributes quotient numerator",
      `Quick,
      () => {
        let x = Exp.var("x");
        let cos_2x = builtin_cos(times(Exp.int(2), x));
        let source =
          times(
            Exp.int(4),
            divide(minus(Exp.int(1), cos_2x), Exp.int(2)),
          );
        let expected =
          minus(
            divide(times(Exp.int(4), Exp.int(1)), Exp.int(2)),
            divide(times(Exp.int(4), cos_2x), Exp.int(2)),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal(
            "distributed quotient numerator target",
            expected,
            actual,
          );
          check(
            bool,
            "records distribution",
            true,
            List.mem("alg.distribute_mul_add", rule_ids),
          );
        | None => fail("expected quotient-numerator distribution target")
        };
      },
    ),
    test_case(
      "algebra shape target expands binomial square",
      `Quick,
      () => {
        let source = power(plus(Exp.var("a"), Exp.var("b")), Exp.int(2));
        let expected =
          plus(
            plus(
              power(Exp.var("a"), Exp.int(2)),
              times(times(Exp.int(2), Exp.var("a")), Exp.var("b")),
            ),
            power(Exp.var("b"), Exp.int(2)),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("expanded binomial square", expected, actual);
          check(
            bool,
            "records expansion",
            true,
            List.mem("alg.expand_polynomial", rule_ids),
          );
        | None => fail("expected binomial square expansion target")
        };
      },
    ),
    test_case(
      "algebra shape target expands difference square",
      `Quick,
      () => {
        let source = power(minus(Exp.var("a"), Exp.var("b")), Exp.int(2));
        let expected =
          plus(
            minus(
              power(Exp.var("a"), Exp.int(2)),
              times(times(Exp.int(2), Exp.var("a")), Exp.var("b")),
            ),
            power(Exp.var("b"), Exp.int(2)),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("expanded difference square", expected, actual);
          check(
            bool,
            "records expansion",
            true,
            List.mem("alg.expand_polynomial", rule_ids),
          );
        | None => fail("expected difference square expansion target")
        };
      },
    ),
    test_case(
      "algebra shape target multiplies conjugates",
      `Quick,
      () => {
        let source =
          times(
            plus(Exp.var("a"), Exp.var("b")),
            minus(Exp.var("a"), Exp.var("b")),
          );
        let expected =
          minus(
            power(Exp.var("a"), Exp.int(2)),
            power(Exp.var("b"), Exp.int(2)),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("difference of squares target", expected, actual);
          check(
            bool,
            "records expansion",
            true,
            List.mem("alg.expand_polynomial", rule_ids),
          );
        | None => fail("expected conjugate multiplication target")
        };
      },
    ),
    test_case(
      "algebra shape target expands trinomial square",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let source = power(plus(plus(a, b), c), Exp.int(2));
        let two_times = exp => times(Exp.int(2), exp);
        let expected =
          plus(
            plus(
              plus(
                plus(
                  plus(times(a, a), two_times(times(a, b))),
                  two_times(times(a, c)),
                ),
                times(b, b),
              ),
              two_times(times(b, c)),
            ),
            times(c, c),
          );
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("expanded trinomial square", expected, actual);
          check(
            bool,
            "records polynomial expansion",
            true,
            List.mem("alg.expand_polynomial", rule_ids),
          );
        | None => fail("expected trinomial square expansion target")
        };
      },
    ),
    test_case(
      "algebra shape target completes positive square",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let source =
          plus(
            plus(power(a, Exp.int(2)), times(times(Exp.int(2), a), b)),
            power(b, Exp.int(2)),
          );
        let expected = power(plus(a, b), Exp.int(2));
        switch (Web.RewriteChecker.normalize_algebra_shape(source)) {
        | Some((actual, rule_ids)) =>
          check_exp_equal("completed square target", expected, actual);
          check(
            bool,
            "records factoring",
            true,
            List.mem("alg.factor_common", rule_ids),
          );
        | None => fail("expected complete-square target")
        };
      },
    ),
    test_case(
      "algebra completes numeric and symbolic monic squares",
      `Quick,
      () => {
        let x = Exp.var("x");
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let two = Exp.int(2);
        let four = Exp.int(4);
        let square = exp => power(exp, two);
        let profile = Axioms.math_profile(Algebra);
        let authorize = (profile, stage, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings,
            env,
            source,
            target,
            max_depth: 5,
            max_states: 160,
          });
        let accepted = result =>
          switch (result) {
          | Web.ProfileProofPlan.Authorized(_) => true
          | Rejected(_) => false
          };
        let numeric_source =
          plus(plus(square(x), times(Exp.int(6), x)), Exp.int(5));
        let numeric_target = minus(square(plus(x, Exp.int(3))), four);
        let numeric_authorization =
          authorize(profile, MultiStepCheck, numeric_source, numeric_target);
        check(
          bool,
          "Check Result completes a standard numeric square",
          true,
          accepted(numeric_authorization),
        );
        switch (numeric_authorization) {
        | Authorized(plan) =>
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Algebra,
              max_depth: 5,
              max_states: 160,
              source: numeric_source,
              target: numeric_target,
            };
          Web.ProofSearchBackend.rocq_program_for_authorized_plan(
            ~profile,
            request,
            plan,
          )
          |> write_text_file("/tmp/hazel_complete_square_numeric.v");
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        let equals = (left, right) =>
          Exp.bin_op(Operators.Poly(Operators.Equals), left, right);
        check(
          bool,
          "equation-level completing-square is not yet profile-authorized",
          false,
          accepted(
            authorize(
              profile,
              MultiStepCheck,
              equals(numeric_source, Exp.int(0)),
              equals(numeric_target, Exp.int(0)),
            ),
          ),
        );
        let exact_square_source =
          plus(plus(square(x), times(Exp.int(6), x)), Exp.int(9));
        let exact_square_target = square(plus(x, Exp.int(3)));
        check(
          bool,
          "One Step currently rejects reverse perfect-square factoring",
          false,
          accepted(
            authorize(
              profile,
              Manual,
              exact_square_source,
              exact_square_target,
            ),
          ),
        );
        let monic_source = plus(plus(square(x), times(b, x)), c);
        let monic_target =
          minus(
            plus(square(plus(x, divide(b, two))), c),
            divide(square(b), four),
          );
        check(
          bool,
          "Check Result currently rejects a symbolic monic square",
          false,
          accepted(
            authorize(profile, MultiStepCheck, monic_source, monic_target),
          ),
        );
        let scaled_numeric_source =
          plus(
            plus(times(two, square(x)), times(Exp.int(8), x)),
            Exp.int(3),
          );
        let scaled_numeric_target =
          minus(times(two, square(plus(x, two))), Exp.int(5));
        let scaled_numeric_authorization =
          authorize(
            profile,
            MultiStepCheck,
            scaled_numeric_source,
            scaled_numeric_target,
          );
        check(
          bool,
          "Check Result completes a scaled numeric square",
          true,
          accepted(scaled_numeric_authorization),
        );
        switch (scaled_numeric_authorization) {
        | Authorized(plan) =>
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Algebra,
              max_depth: 5,
              max_states: 160,
              source: scaled_numeric_source,
              target: scaled_numeric_target,
            };
          Web.ProofSearchBackend.rocq_program_for_authorized_plan(
            ~profile,
            request,
            plan,
          )
          |> write_text_file("/tmp/hazel_complete_square_scaled_numeric.v");
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        let general_source =
          plus(plus(times(a, square(x)), times(b, x)), c);
        let general_target =
          minus(
            times(a, square(plus(x, divide(b, times(two, a))))),
            divide(
              minus(square(b), times(times(four, a), c)),
              times(four, a),
            ),
          );
        check(
          bool,
          "the fully general form is rejected without a != 0 assumptions",
          false,
          accepted(
            authorize(
              profile,
              MultiStepCheck,
              general_source,
              general_target,
            ),
          ),
        );
        check(
          bool,
          "the Arithmetic profile cannot complete a polynomial square",
          false,
          accepted(
            authorize(
              Axioms.math_profile(Arithmetic),
              MultiStepCheck,
              numeric_source,
              numeric_target,
            ),
          ),
        );
      },
    ),
    test_case(
      "single algebra step records additive occurrence for distribution",
      `Quick,
      () => {
        let local_from =
          times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
        let local_to =
          plus(
            times(Exp.var("x"), Exp.var("y")),
            times(Exp.var("x"), Exp.var("z")),
          );
        let from_ = plus(Exp.int(1), local_from);
        let to_ = plus(Exp.int(1), local_to);
        let result = require_single_step_result_at_level(Algebra, from_, to_);
        switch (result.prover_steps) {
        | [step] =>
          check(string, "rule", "alg.distribute_mul_add", step.rule_id);
          check(int, "occurrence", 2, step.occurrence);
          check_exp_equal("local before", local_from, step.before_exp);
          check_exp_equal("local after", local_to, step.after_exp);
          check_exp_equal("whole before", from_, step.before_full_exp);
          check_exp_equal("whole after", to_, step.after_full_exp);
        | _ => fail("expected one manual algebra prover step")
        };
      },
    ),
    test_case(
      "axiom search finds constant fold proof",
      `Quick,
      () => {
        let result =
          Web.AxiomSearch.search(
            ~level=Arithmetic,
            ~max_depth=1,
            ~allowed_rule_ids=["arith.const_fold"],
            ~log=false,
            plus(Exp.int(1), Exp.int(2)),
            Exp.int(3),
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "arith.const_fold", step.rule_id);
            check_exp_equal(
              "whole before",
              plus(Exp.int(1), Exp.int(2)),
              step.before_full_exp,
            );
            check_exp_equal("whole after", Exp.int(3), step.after_full_exp);
          | _ => fail("expected one search step")
          };
        | None => fail("expected axiom search proof")
        };
      },
    ),
    test_case(
      "exact rational constant folding is general and profile gated",
      `Quick,
      () => {
        let profile = Axioms.math_profile(Arithmetic);
        let visible_rule_ids = profile =>
          Axioms.stage_plan_for_profile(profile, Manual).visible_rules
          |> List.map((planned: Axioms.planned_visible_rule) =>
               planned.rule.id
             );
        let accepts = (profile, source, target) =>
          Web.AxiomSearch.search(
            ~level=profile.Axioms.level,
            ~max_depth=1,
            ~allowed_rule_ids=visible_rule_ids(profile),
            ~log=false,
            source,
            target,
          )
          |> Option.is_some;
        let half = divide(Exp.int(1), Exp.int(2));
        [
          (
            "rational power",
            power(half, Exp.int(2)),
            divide(Exp.int(1), Exp.int(4)),
          ),
          (
            "rational addition",
            plus(half, divide(Exp.int(1), Exp.int(3))),
            divide(Exp.int(5), Exp.int(6)),
          ),
          (
            "nested rational division",
            divide(
              divide(Exp.int(3), Exp.int(4)),
              divide(Exp.int(2), Exp.int(5)),
            ),
            divide(Exp.int(15), Exp.int(8)),
          ),
        ]
        |> List.iter(((label, source, target)) =>
             check(bool, label, true, accepts(profile, source, target))
           );
        check(
          bool,
          "an incorrect rational result is rejected",
          false,
          accepts(
            profile,
            plus(half, divide(Exp.int(1), Exp.int(3))),
            divide(Exp.int(4), Exp.int(5)),
          ),
        );
        let without_constant_folding =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="arith.const_fold",
            profile,
          );
        check(
          bool,
          "disabling Evaluate constants removes rational folding",
          false,
          accepts(
            without_constant_folding,
            power(half, Exp.int(2)),
            divide(Exp.int(1), Exp.int(4)),
          ),
        );
      },
    ),
    test_case(
      "rational constant and scalar cleanup finish before breadth-first expansion",
      `Quick,
      () => {
        let half = divide(Exp.int(1), Exp.int(2));
        switch (
          Web.AxiomSearch.start_search(
            ~level=Axioms.Arithmetic,
            ~max_depth=1,
            ~max_states=1,
            ~allowed_rule_ids=["arith.const_fold"],
            power(half, Exp.int(2)),
            divide(Exp.int(1), Exp.int(4)),
          )
        ) {
        | SearchComplete(Some(result)) =>
          check(
            list(string),
            "constant fold evidence",
            ["arith.const_fold"],
            result.applications
            |> List.map((app: Web.AxiomSearch.application) => app.rule.id),
          )
        | _ => fail("expected immediate rational constant-fold finish")
        };
        let x = Exp.var("x");
        switch (
          Web.AxiomSearch.start_search(
            ~level=Axioms.Algebra,
            ~max_depth=1,
            ~max_states=1,
            ~allowed_rule_ids=["arith.simplify_scalar_products"],
            times(Exp.int(2), times(Exp.int(3), x)),
            times(Exp.int(6), x),
          )
        ) {
        | SearchComplete(Some(result)) =>
          check(
            list(string),
            "scalar cleanup evidence",
            ["arith.simplify_scalar_products"],
            result.applications
            |> List.map((app: Web.AxiomSearch.application) => app.rule.id),
          )
        | _ => fail("expected immediate scalar-cleanup finish")
        };
      },
    ),
    test_case(
      "real rational constant-fold replay uses an exact field certificate",
      `Quick,
      () => {
        let source = power(divide(Exp.int(1), Exp.int(2)), Exp.int(2));
        let target = divide(Exp.int(1), Exp.int(4));
        let step =
          Web.ProofTrace.prover_step(
            ~origin=Web.ProofTrace.Normalization,
            ~rule_id="arith.const_fold",
            ~before_full_exp=source,
            ~after_full_exp=target,
            ~before_exp=source,
            ~after_exp=target,
            ~detail="exact rational constant fold",
          );
        check(
          string,
          "field replay",
          "try unfold Rsqr; field.",
          Web.CoqProofExport.recorded_transition_replay_script(
            ~domain=Web.CoqExport.Reals,
            [step],
          ),
        );
      },
    ),
    test_case(
      "proof search backend routes local axiom search",
      `Quick,
      () => {
        let summary =
          Web.ProofSearchBackend.search_trace({
            backend: Web.ProofSearchBackend.LocalAxiomSearch,
            level: Arithmetic,
            max_depth: 1,
            max_states: 20,
            source: plus(Exp.int(1), Exp.int(2)),
            target: Exp.int(3),
          });
        switch (summary) {
        | Some(summary) =>
          check(
            string,
            "label",
            "bounded axiom search",
            summary.justification,
          );
          check(
            list(string),
            "rules",
            ["arith.const_fold"],
            summary.rule_ids,
          );
          check(bool, "exportable", true, summary.exportable);
        | None => fail("expected backend proof search trace")
        };
      },
    ),
    test_case(
      "proof search backend dumps Rocq tactic-search candidate",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          plus(
            power(builtin_sin(x), Exp.int(2)),
            power(builtin_cos(x), Exp.int(2)),
          );
        let target = Exp.int(1);
        let request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq = Web.ProofSearchBackend.rocq_search_program(request);
        write_text_file("/tmp/hazel_stepper_rocq_tactic_search.v", coq);
        check(
          bool,
          "uses Rocq tactic-search theorem",
          true,
          string_contains("Theorem hazel_rocq_search", coq),
        );
        check(
          bool,
          "uses generated tactic group",
          true,
          string_contains("hazel_rewrite_search 8%nat", coq),
        );
        check(
          bool,
          "uses generated power tactic",
          true,
          string_contains("hazel_power_normalize", coq),
        );
        check(
          bool,
          "prints without ERROR",
          false,
          string_contains("ERROR", coq),
        );
      },
    ),
    test_case(
      "proof search backend dumps Rocq arithmetic and algebra candidates",
      `Quick,
      () => {
        let arithmetic_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Arithmetic,
            max_depth: 4,
            max_states: 80,
            source: plus(Exp.var("x"), Exp.int(3)),
            target: plus(Exp.int(3), Exp.var("x")),
          };
        let arithmetic_coq =
          Web.ProofSearchBackend.rocq_search_program(arithmetic_request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_arithmetic_search.v",
          arithmetic_coq,
        );
        check(
          bool,
          "arithmetic candidate uses arithmetic tactic",
          true,
          string_contains("solve [\n    lra", arithmetic_coq),
        );
        check(
          bool,
          "arithmetic macro is labeled",
          true,
          collapsed_macro_summary(arithmetic_request)
          |> (
            summary =>
              List.mem("rocq.arithmetic_tactic_search", summary.rule_ids)
          ),
        );
        check(
          bool,
          "arithmetic candidate exports without ERROR",
          false,
          string_contains("ERROR", arithmetic_coq),
        );

        let algebra_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source: times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
            target:
              plus(
                times(Exp.var("x"), Exp.var("y")),
                times(Exp.var("x"), Exp.var("z")),
              ),
          };
        let algebra_coq =
          Web.ProofSearchBackend.rocq_search_program(algebra_request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_algebra_search.v",
          algebra_coq,
        );
        check(
          bool,
          "algebra candidate uses algebra tactic",
          true,
          string_contains("hazel_algebra", algebra_coq),
        );
        check(
          bool,
          "algebra macro is labeled",
          true,
          collapsed_macro_summary(algebra_request)
          |> (
            summary =>
              List.mem("rocq.algebra_tactic_search", summary.rule_ids)
          ),
        );
        check(
          bool,
          "algebra candidate exports without ERROR",
          false,
          string_contains("ERROR", algebra_coq),
        );
      },
    ),
    test_case(
      "Rocq tactic-search programs select purpose-specific plans",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let z = Exp.var("z");
        let algebra_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source: times(x, plus(y, z)),
            target: plus(times(x, y), times(x, z)),
          };
        let check_result_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=CheckResult,
            algebra_request,
          );
        let cleanup_request =
          Web.ProofSearchBackend.{
            ...algebra_request,
            source: times(x, plus(Exp.int(1), x)),
            target: times(x, plus(x, Exp.int(1))),
          };
        let cleanup_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=CheckResult,
            cleanup_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_cleanup_search.v",
          cleanup_coq,
        );
        let default_coq =
          Web.ProofSearchBackend.rocq_search_program(algebra_request);
        let equivalence_coq =
          Web.ProofSearchBackend.rocq_equivalence_program_for_profile(
            ~profile=Axioms.math_profile(Algebra),
            algebra_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_algebra_equivalence.v",
          equivalence_coq,
        );
        let primitive_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=ValidatePrimitiveStep,
            algebra_request,
          );
        let macro_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=ValidateMacroStep,
            algebra_request,
          );
        check(
          string,
          "default Rocq search is check-result search",
          check_result_coq,
          default_coq,
        );
        check(
          bool,
          "check-result search uses profile-constrained bounded search",
          true,
          string_contains(
            "intros.\nhazel_profile_search.\nQed.",
            check_result_coq,
          ),
        );
        check(
          bool,
          "profile-constrained theorem omits broad finisher",
          false,
          string_contains(
            "intros.\nfirst [hazel_integer_polynomial | reflexivity]",
            check_result_coq,
          ),
        );
        check(
          bool,
          "equivalence fallback contains broad finisher",
          true,
          string_contains(
            "intros.\nfirst [hazel_real_algebra | reflexivity]",
            equivalence_coq,
          ),
        );
        check(
          bool,
          "check-result search uses requested maximum depth",
          true,
          string_contains(
            "hazel_profile_search_exact 4%nat",
            check_result_coq,
          ),
        );
        check(
          bool,
          "check-result search does not exceed requested depth",
          false,
          string_contains(
            "hazel_profile_search_exact 5%nat",
            check_result_coq,
          ),
        );
        check(
          bool,
          "check-result search does not repeat smaller exact-depth searches",
          false,
          string_contains(
            "\n  | hazel_profile_search_exact 3%nat",
            check_result_coq,
          ),
        );
        let cancellation_only_profile =
          Axioms.math_profile(Algebra)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_mul_add",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_div_add",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.factor_common",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.mul_identity",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.add_comm",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.const_fold",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.mul_const",
             );
        let disabled_distribution_request =
          Web.ProofSearchBackend.{
            ...algebra_request,
            source: times(x, plus(Exp.int(1), x)),
            target: plus(times(x, Exp.int(1)), times(x, x)),
          };
        let cancellation_only_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=cancellation_only_profile,
            ~purpose=CheckResult,
            disabled_distribution_request,
          );
        let cancellation_only_equivalence_coq =
          Web.ProofSearchBackend.rocq_equivalence_program_for_profile(
            ~profile=cancellation_only_profile,
            disabled_distribution_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_disabled_distribution_constrained.v",
          cancellation_only_coq,
        );
        write_text_file(
          "/tmp/hazel_stepper_rocq_disabled_distribution_equivalence.v",
          cancellation_only_equivalence_coq,
        );
        check(
          bool,
          "profile-visible tactic contains only enabled cancellation",
          true,
          string_contains(
            "Ltac hazel_profile_visible_step :=\n  first [\n    unfold Rminus; rewrite <- Rplus_assoc; rewrite Rplus_opp_r; rewrite Rplus_0_r\n  | unfold Rminus; rewrite Rplus_assoc; rewrite Rplus_opp_l; rewrite Rplus_0_l\n  ].\n\nLtac hazel_profile_normalization_step",
            cancellation_only_coq,
          ),
        );
        check(
          bool,
          "primitive search omits broad algebra finisher",
          false,
          string_contains("try solve [hazel_algebra]", primitive_coq),
        );
        check(
          bool,
          "primitive search uses bounded one-step algebra search",
          true,
          string_contains("hazel_rewrite_search 1%nat", primitive_coq),
        );
        check(
          bool,
          "macro search can repeat a bounded rewrite step",
          true,
          string_contains(
            "hazel_repeat_fuel 10%nat hazel_rewrite_step",
            macro_coq,
          ),
        );
        let primitive_summary =
          collapsed_macro_summary_for_purpose(
            ~purpose=ValidatePrimitiveStep,
            algebra_request,
          );
        let primitive_detail =
          switch (primitive_summary.prover_steps) {
          | [{detail: Some(detail), _}, ..._] => detail
          | _ => ""
          };
        check(
          bool,
          "primitive summary records primitive plan",
          true,
          string_contains("hazel_algebra_primitive_plan", primitive_detail),
        );
        let auto_summary =
          collapsed_macro_summary_for_purpose(
            ~purpose=AutoSimplify,
            algebra_request,
          );
        let auto_detail =
          switch (auto_summary.prover_steps) {
          | [{detail: Some(detail), _}, ..._] => detail
          | _ => ""
          };
        check(
          bool,
          "auto summary records shared check-result validation plan",
          true,
          string_contains("hazel_algebra_plan", auto_detail),
        );

        let x = Exp.var("x");
        let trig_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source:
              plus(
                power(builtin_sin(x), Exp.int(2)),
                power(builtin_cos(x), Exp.int(2)),
              ),
            target: Exp.int(1),
          };
        let primitive_trig_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=ValidatePrimitiveStep,
            trig_request,
          );
        check(
          bool,
          "primitive trig search omits broad trig finisher",
          false,
          string_contains(
            "try solve [hazel_trigonometry]",
            primitive_trig_coq,
          ),
        );
      },
    ),
    test_case(
      "Rocq profile search uses guarded affine certificates",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let cos_2x = builtin_cos(times(Exp.int(2), x));
        let half = divide(Exp.int(1), Exp.int(2));
        let source = plus(Exp.int(1), minus(half, cos_2x));
        let target = minus(divide(Exp.int(3), Exp.int(2)), cos_2x);
        check(
          bool,
          "exact rational constants normalize around an opaque trig term",
          true,
          Web.RewriteChecker.rational_affine_equivalent(source, target),
        );
        let float_bin = (op, left, right) =>
          Exp.bin_op(Operators.Float(op), left, right);
        let float_source =
          float_bin(
            Operators.Plus,
            Exp.int(1),
            float_bin(
              Operators.Minus,
              float_bin(Operators.Divide, Exp.int(1), Exp.int(2)),
              cos_2x,
            ),
          );
        let float_target =
          float_bin(
            Operators.Minus,
            float_bin(Operators.Divide, Exp.int(3), Exp.int(2)),
            builtin_cos(float_bin(Operators.Times, Exp.int(2), x)),
          );
        check(
          bool,
          "UI-elaborated float operators use the same rational certificate",
          true,
          Web.RewriteChecker.rational_affine_equivalent(
            float_source,
            float_target,
          ),
        );
        check(
          bool,
          "target-editor trig variables match selected builtin functions",
          true,
          Web.RewriteChecker.rational_affine_equivalent(
            source,
            minus(
              divide(Exp.int(3), Exp.int(2)),
              cos(times(Exp.int(2), x)),
            ),
          ),
        );
        check(
          bool,
          "arbitrary unresolved functions do not match builtins",
          false,
          Web.RewriteChecker.same_math_exp(
            Language.Exp.fresh(BuiltinFun("map")),
            Exp.var("map"),
          ),
        );
        check(
          bool,
          "reordered symbolic terms are not hidden by affine normalization",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            plus(x, y),
            plus(y, x),
          ),
        );
        check(
          bool,
          "moving a constant across a symbolic term is not hidden",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            plus(plus(x, Exp.int(1)), y),
            plus(plus(x, y), Exp.int(1)),
          ),
        );
        check(
          bool,
          "profile-enabled AC cleanup can move constants across opaque terms",
          true,
          Web.RewriteChecker.rational_affine_equivalent_with_constant_reordering(
            plus(plus(x, Exp.int(1)), y),
            plus(plus(x, y), Exp.int(1)),
          ),
        );
        check(
          bool,
          "separated like terms are not collected across a constant",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            plus(plus(x, Exp.int(1)), x),
            plus(times(Exp.int(2), x), Exp.int(1)),
          ),
        );
        check(
          bool,
          "constant reordering does not collect separated like terms",
          false,
          Web.RewriteChecker.rational_affine_equivalent_with_constant_reordering(
            plus(plus(x, Exp.int(1)), x),
            plus(times(Exp.int(2), x), Exp.int(1)),
          ),
        );
        check(
          bool,
          "distribution remains a separate visible capability",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            times(Exp.int(2), plus(x, Exp.int(1))),
            plus(times(Exp.int(2), x), Exp.int(2)),
          ),
        );
        check(
          bool,
          "nonlinear power conversion is not classified as affine",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            times(x, x),
            power(x, Exp.int(2)),
          ),
        );
        check(
          bool,
          "inequivalent affine constants are rejected before Rocq",
          false,
          Web.RewriteChecker.rational_affine_equivalent(
            plus(x, Exp.int(1)),
            plus(x, Exp.int(2)),
          ),
        );
        let request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq = Web.ProofSearchBackend.rocq_search_program(request);
        write_text_file("/tmp/hazel_stepper_rocq_affine_opaque_trig.v", coq);
        check(
          bool,
          "guarded affine certificate runs before branching search",
          true,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [\n    lra",
            coq,
          ),
        );
        check(
          bool,
          "guarded affine certificate uses the catalog tactic",
          true,
          string_contains("solve [\n    lra", coq),
        );
        check(
          bool,
          "guarded affine certificate omits the branching trig prelude",
          false,
          string_contains("Ltac hazel_trig_identity_context", coq),
        );
        check(
          bool,
          "guarded affine certificate stays compact",
          true,
          String.length(coq) < 1000,
        );
        check(
          bool,
          "guarded affine certificate remains a profile search result",
          true,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [\n    lra",
            coq,
          ),
        );
        let cos_4x = builtin_cos(times(Exp.int(4), x));
        let long_source =
          plus(
            plus(
              plus(Exp.int(1), minus(half, cos_2x)),
              divide(times(half, cos_4x), Exp.int(2)),
            ),
            divide(half, Exp.int(2)),
          );
        let long_target =
          plus(
            minus(divide(Exp.int(7), Exp.int(4)), cos_2x),
            times(divide(Exp.int(1), Exp.int(4)), cos_4x),
          );
        let long_request = {
          ...request,
          source: long_source,
          target: long_target,
        };
        let long_coq =
          Web.ProofSearchBackend.rocq_search_program(long_request);
        check(
          bool,
          "AC-enabled trig profile emits a compact affine certificate",
          true,
          String.length(long_coq) < 1000
          && string_contains("solve [\n    lra", long_coq),
        );
        let no_add_comm_profile =
          Web.ProfileBoard.apply_model_to_profile(
            Web.ProfileBoard.Model.init
            |> Web.ProfileBoard.Update.update(
                 Web.ProfileBoard.Update.SetCleanupEnabled("add.comm", false),
               ),
            Axioms.math_profile(Trigonometry),
          );
        let constrained_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=no_add_comm_profile,
            ~purpose=CheckResult,
            long_request,
          );
        check(
          bool,
          "disabling additive commutativity disables the compact certificate",
          false,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [\n    lra",
            constrained_coq,
          ),
        );
        let factored_source =
          plus(
            divide(times(half, cos_4x), Exp.int(2)),
            divide(half, Exp.int(2)),
          );
        let factored_target =
          times(
            divide(Exp.int(1), Exp.int(4)),
            plus(Exp.int(1), cos_4x),
          );
        let factored_request = {
          ...request,
          source: factored_source,
          target: factored_target,
        };
        let factored_coq =
          Web.ProofSearchBackend.rocq_search_program(factored_request);
        check(
          bool,
          "enabled distribution gives factored affine goals a compact certificate",
          true,
          String.length(factored_coq) < 1000
          && string_contains("solve [\n    lra", factored_coq),
        );
        let opaque = Exp.var("u");
        let opaque_factored_coq =
          Web.ProofSearchBackend.rocq_search_program({
            ...factored_request,
            source:
              plus(
                divide(times(half, opaque), Exp.int(2)),
                divide(half, Exp.int(2)),
              ),
            target:
              times(
                divide(Exp.int(1), Exp.int(4)),
                plus(Exp.int(1), opaque),
              ),
          });
        check(
          bool,
          "factored affine certificates are independent of trig syntax",
          true,
          String.length(opaque_factored_coq) < 1000
          && (
            string_contains("solve [\n    lia", opaque_factored_coq)
            || string_contains("solve [\n    lra", opaque_factored_coq)
          ),
        );
        let no_distribution_profile =
          Axioms.math_profile(Trigonometry)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_mul_add",
             );
        let no_distribution_factored_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=no_distribution_profile,
            ~purpose=CheckResult,
            factored_request,
          );
        check(
          bool,
          "factoring does not depend on the forward distribution rule",
          true,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [\n    lra",
            no_distribution_factored_coq,
          ),
        );
        let no_factor_profile =
          Axioms.math_profile(Trigonometry)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.factor_common",
             );
        let no_factor_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=no_factor_profile,
            ~purpose=CheckResult,
            factored_request,
          );
        check(
          bool,
          "disabling factoring disables the factored affine certificate",
          false,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [\n    lra",
            no_factor_coq,
          ),
        );
        let distribution_coq =
          Web.ProofSearchBackend.rocq_search_program({
            ...request,
            level: Algebra,
            source: times(Exp.int(2), plus(x, Exp.int(1))),
            target: plus(times(Exp.int(2), x), Exp.int(2)),
          });
        check(
          bool,
          "enabled numeric distribution receives the affine certificate",
          true,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [",
            distribution_coq,
          ),
        );
        let no_distribution_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=no_distribution_profile,
            ~purpose=CheckResult,
            {
              ...request,
              level: Algebra,
              source: times(Exp.int(2), plus(x, Exp.int(1))),
              target: plus(times(Exp.int(2), x), Exp.int(2)),
            },
          );
        check(
          bool,
          "disabling distribution disables the expanded affine certificate",
          false,
          string_contains(
            "Ltac hazel_profile_search :=\n  solve [",
            no_distribution_coq,
          ),
        );
      },
    ),
    test_case(
      "Rocq profile search emits representative branch fixtures",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let z = Exp.var("z");
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        [
          (
            "distribute_left",
            times(x, plus(y, z)),
            plus(times(x, y), times(x, z)),
          ),
          (
            "distribute_right",
            times(plus(x, y), z),
            plus(times(x, z), times(y, z)),
          ),
          (
            "distribute_simplify_identity",
            times(x, plus(x, Exp.int(1))),
            plus(times(x, x), x),
          ),
          (
            "factor_common",
            plus(times(x, y), times(x, z)),
            times(x, plus(y, z)),
          ),
          (
            "factor_quadratic",
            minus(
              plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
              Exp.int(4),
            ),
            times(minus(x, Exp.int(1)), plus(x, Exp.int(4))),
          ),
          ("cancel_add", minus(plus(x, y), y), x),
          ("power_cleanup", times(x, x), power(x, Exp.int(2))),
          (
            "ac_cleanup",
            times(x, plus(Exp.int(1), x)),
            times(x, plus(x, Exp.int(1))),
          ),
        ]
        |> List.iter(((name, source, target)) => {
             let coq =
               Web.ProofSearchBackend.rocq_search_program(
                 request(source, target),
               );
             write_text_file(
               "/tmp/hazel_stepper_rocq_profile_" ++ name ++ ".v",
               coq,
             );
             if (name == "distribute_left") {
               check(
                 bool,
                 "distribution trace uses a bounded directed replay",
                 true,
                 (
                   string_contains("first [rewrite Rmult_plus_distr_l", coq)
                   || string_contains(
                        "progress (rewrite Rmult_plus_distr_l)",
                        coq,
                      )
                 )
                 && !string_contains("repeat rewrite Rmult_plus_distr_l", coq),
               );
             };
             check(
               bool,
               name ++ " uses profile search",
               true,
               string_contains("hazel_profile_search", coq),
             );
           });
        let invalid_ac_coq =
          Web.ProofSearchBackend.rocq_search_program(
            request(plus(plus(x, y), z), times(times(x, y), z)),
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_profile_invalid_ac.v",
          invalid_ac_coq,
        );
        let trig_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source:
              plus(
                power(builtin_sin(x), Exp.int(2)),
                power(builtin_cos(x), Exp.int(2)),
              ),
            target: Exp.int(1),
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_profile_trig_identity.v",
          Web.ProofSearchBackend.rocq_search_program(trig_request),
        );
        let integer_trig_profile_coq =
          Web.ProofSearchBackend.rocq_search_program_for_profile_and_purpose(
            ~profile=Axioms.math_profile(Trigonometry),
            ~purpose=CheckResult,
            {
              ...
                request(
                  times(x, plus(Exp.int(1), x)),
                  times(x, plus(x, Exp.int(1))),
                ),
              level: Trigonometry,
            },
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_profile_integer_trig_mode.v",
          integer_trig_profile_coq,
        );
        check(
          bool,
          "universal real trig profile includes trig normalization",
          true,
          string_contains(
            "hazel_trig_argument_algebra",
            integer_trig_profile_coq,
          ),
        );
      },
    ),
    test_case(
      "proof search export group comes from shared math taxonomy",
      `Quick,
      () => {
        let x = Exp.var("x");
        let quotient_square_numerator =
          minus(Exp.int(1), builtin_cos(times(Exp.int(2), x)));
        let quotient_square = divide(quotient_square_numerator, Exp.int(2));
        let algebra_in_trig_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: power(quotient_square, Exp.int(2)),
            target: times(quotient_square, quotient_square),
          };
        let algebra_in_trig_coq =
          Web.ProofSearchBackend.rocq_search_program(algebra_in_trig_request);
        check(
          bool,
          "preserved trig calls lower to algebra export group",
          true,
          Axioms.export_level_for_rewrite(
            ~requested_level=Trigonometry,
            algebra_in_trig_request.source,
            algebra_in_trig_request.target,
          )
          == Algebra,
        );
        check(
          bool,
          "trig-mode algebra request uses constrained real-algebra search",
          true,
          string_contains(
            "intros.\nhazel_profile_search.\nQed.",
            algebra_in_trig_coq,
          ),
        );
        check(
          bool,
          "trig-mode algebra request does not run trig tactic",
          false,
          string_contains(
            "try solve [hazel_trigonometry]; reflexivity",
            algebra_in_trig_coq,
          ),
        );
        check(
          bool,
          "trig-mode algebra macro is labeled algebra",
          true,
          collapsed_macro_summary(algebra_in_trig_request)
          |> (
            summary =>
              List.mem("rocq.algebra_tactic_search", summary.rule_ids)
          ),
        );

        let trig_identity_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source:
              plus(
                power(builtin_sin(x), Exp.int(2)),
                power(builtin_cos(x), Exp.int(2)),
              ),
            target: Exp.int(1),
          };
        let trig_identity_coq =
          Web.ProofSearchBackend.rocq_search_program(trig_identity_request);
        check(
          bool,
          "changed trig calls stay in trigonometry export group",
          true,
          Axioms.export_level_for_rewrite(
            ~requested_level=Trigonometry,
            trig_identity_request.source,
            trig_identity_request.target,
          )
          == Trigonometry,
        );
        check(
          bool,
          "trig identity request uses constrained profile search",
          true,
          string_contains(
            "intros.\nhazel_profile_search.\nQed.",
            trig_identity_coq,
          ),
        );
      },
    ),
    test_case(
      "search suggestions simplify scalar products recursively",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = builtin_cos(times(Exp.int(2), times(Exp.int(2), x)));
        let target = builtin_cos(times(Exp.int(4), x));
        let suggestions =
          Web.TrigRewrite.scalar_product_simplifications_at_root(source);
        switch (suggestions) {
        | [suggestion] =>
          check(
            string,
            "simplification label",
            "simplify scalar products",
            suggestion.label,
          );
          check_exp_equal(
            "scalar product suggestion",
            target,
            suggestion.after_exp,
          );
        | _ => fail("expected one scalar-product simplification")
        };
        let request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq = Web.ProofSearchBackend.rocq_search_program(request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_trig_argument_scalar_simplify.v",
          coq,
        );
        check(
          bool,
          "scalar product simplification exports without ERROR",
          false,
          string_contains("ERROR", coq),
        );
      },
    ),
    test_case(
      "trig argument scalar simplification exports with adjacent unchanged trig",
      `Quick,
      () => {
        let x = Exp.var("x");
        let unchanged = builtin_cos(times(Exp.int(2), x));
        let source =
          plus(
            unchanged,
            builtin_cos(times(Exp.int(2), times(Exp.int(2), x))),
          );
        let target = plus(unchanged, builtin_cos(times(Exp.int(4), x)));
        let request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source,
            target,
          };
        let coq = Web.ProofSearchBackend.rocq_search_program(request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_trig_argument_context.v",
          coq,
        );
        check(
          bool,
          "uses trig tactic for argument simplification",
          true,
          string_contains("hazel_trigonometry", coq),
        );
        check(
          bool,
          "context scalar argument simplification exports without ERROR",
          false,
          string_contains("ERROR", coq),
        );
      },
    ),
    test_case(
      "proof search backend dumps Rocq power normalization candidates",
      `Quick,
      () => {
        let x = Exp.var("x");
        let sin_x = builtin_sin(x);
        let real_power_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: power(sin_x, Exp.int(4)),
            target:
              times(power(sin_x, Exp.int(2)), power(sin_x, Exp.int(2))),
          };
        let real_coq =
          Web.ProofSearchBackend.rocq_search_program(real_power_request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_power_split_reals.v",
          real_coq,
        );
        check(
          bool,
          "real power candidate uses power tactic",
          true,
          string_contains("hazel_power_normalize", real_coq),
        );
        check(
          bool,
          "real power candidate exports without ERROR",
          false,
          string_contains("ERROR", real_coq),
        );
        let real_nested_power_request =
          Web.ProofSearchBackend.{
            ...real_power_request,
            target: power(power(sin_x, Exp.int(2)), Exp.int(2)),
          };
        let real_nested_power_coq =
          Web.ProofSearchBackend.rocq_search_program_for_purpose(
            ~purpose=Axioms.CheckResult,
            real_nested_power_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_trig_nested_power_profile.v",
          real_nested_power_coq,
        );
        check(
          bool,
          "trig Check Result tries the power normalizer as a finisher",
          true,
          string_contains(
            "solve [hazel_power_normalize]",
            real_nested_power_coq,
          ),
        );
        check(
          bool,
          "power finisher is not followed by recursive search",
          false,
          string_contains(
            "progress (hazel_power_normalize); hazel_profile_search_exact n'",
            real_nested_power_coq,
          ),
        );
        check(
          bool,
          "nested-power theorem uses constrained profile search",
          true,
          string_contains(
            "intros.\nhazel_profile_search.\nQed.",
            real_nested_power_coq,
          ),
        );
        check(
          bool,
          "generated real profile avoids unavailable JSCoq cancellation lemmas",
          false,
          string_contains("Rplus_minus_", real_nested_power_coq),
        );
        let quotient_square_numerator =
          minus(Exp.int(1), builtin_cos(times(Exp.int(2), x)));
        let quotient_square = divide(quotient_square_numerator, Exp.int(2));
        let real_quotient_square_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: power(quotient_square, Exp.int(2)),
            target: times(quotient_square, quotient_square),
          };
        let real_quotient_square_coq =
          Web.ProofSearchBackend.rocq_search_program(
            real_quotient_square_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_real_quotient_square.v",
          real_quotient_square_coq,
        );
        check(
          bool,
          "real quotient square candidate uses real algebra tactic",
          true,
          string_contains("hazel_real_algebra", real_quotient_square_coq),
        );
        check(
          bool,
          "real quotient square candidate exports without ERROR",
          false,
          string_contains("ERROR", real_quotient_square_coq),
        );
        let real_quotient_square_over_four_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: power(quotient_square, Exp.int(2)),
            target:
              divide(
                times(quotient_square_numerator, quotient_square_numerator),
                Exp.int(4),
              ),
          };
        let real_quotient_square_over_four_coq =
          Web.ProofSearchBackend.rocq_search_program(
            real_quotient_square_over_four_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_real_quotient_square_over_four.v",
          real_quotient_square_over_four_coq,
        );
        check(
          bool,
          "real quotient square over four candidate uses real algebra tactic",
          true,
          string_contains(
            "hazel_real_algebra",
            real_quotient_square_over_four_coq,
          ),
        );
        check(
          bool,
          "real quotient square over four candidate exports without ERROR",
          false,
          string_contains("ERROR", real_quotient_square_over_four_coq),
        );
        let real_quotient_square_expanded_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: power(quotient_square, Exp.int(2)),
            target:
              plus(
                minus(
                  divide(Exp.int(1), Exp.int(4)),
                  divide(builtin_cos(times(Exp.int(2), x)), Exp.int(2)),
                ),
                divide(
                  power(builtin_cos(times(Exp.int(2), x)), Exp.int(2)),
                  Exp.int(4),
                ),
              ),
          };
        let real_quotient_square_expanded_coq =
          Web.ProofSearchBackend.rocq_search_program(
            real_quotient_square_expanded_request,
          );
        write_text_file(
          "/tmp/hazel_stepper_rocq_real_quotient_square_expanded.v",
          real_quotient_square_expanded_coq,
        );
        check(
          bool,
          "real quotient square expanded candidate uses real algebra tactic",
          true,
          string_contains(
            "hazel_real_algebra",
            real_quotient_square_expanded_coq,
          ),
        );
        check(
          bool,
          "real quotient square expanded candidate exports without ERROR",
          false,
          string_contains("ERROR", real_quotient_square_expanded_coq),
        );
        let dump_case_study_step = (name, source, target) => {
          let request =
            Web.ProofSearchBackend.{
              backend: Web.ProofSearchBackend.JSCoqTacticSearch,
              level: Trigonometry,
              max_depth: 4,
              max_states: 80,
              source,
              target,
            };
          let coq = Web.ProofSearchBackend.rocq_search_program(request);
          write_text_file(
            "/tmp/hazel_stepper_case_study_" ++ name ++ ".v",
            coq,
          );
          check(
            bool,
            "case study " ++ name ++ " exports without ERROR",
            false,
            string_contains("ERROR", coq),
          );
          check(
            bool,
            "case study " ++ name ++ " quantifies variables over reals",
            true,
            string_contains("forall x : R", coq)
            && string_contains("Open Scope R_scope", coq)
            && !string_contains("Open Scope Z_scope", coq),
          );
        };
        let sin_x = builtin_sin(x);
        let cos_2x = builtin_cos(times(Exp.int(2), x));
        let cos_4x = builtin_cos(times(Exp.int(4), x));
        let one_minus_cos_2x = minus(Exp.int(1), cos_2x);
        let one_minus_cos_2x_over_two = divide(one_minus_cos_2x, Exp.int(2));
        let one_minus_cos_2x_square_expanded =
          times(one_minus_cos_2x, one_minus_cos_2x);
        let one_minus_cos_2x_square_poly =
          plus(
            minus(Exp.int(1), times(Exp.int(2), cos_2x)),
            power(cos_2x, Exp.int(2)),
          );
        let case_study_source =
          plus(Exp.int(1), times(Exp.int(2), power(sin_x, Exp.int(4))));
        let case_study_nested_power =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              power(power(sin_x, Exp.int(2)), Exp.int(2)),
            ),
          );
        let case_study_after_trig_substitution =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              power(one_minus_cos_2x_over_two, Exp.int(2)),
            ),
          );
        let case_study_square_as_product =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              times(one_minus_cos_2x_over_two, one_minus_cos_2x_over_two),
            ),
          );
        let case_study_square_over_four =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              divide(one_minus_cos_2x_square_expanded, Exp.int(4)),
            ),
          );
        let case_study_expanded_square =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              divide(one_minus_cos_2x_square_poly, Exp.int(4)),
            ),
          );
        let case_study_cancel_two =
          plus(
            Exp.int(1),
            divide(one_minus_cos_2x_square_poly, Exp.int(2)),
          );
        let case_study_collected =
          plus(
            minus(divide(Exp.int(3), Exp.int(2)), cos_2x),
            divide(power(cos_2x, Exp.int(2)), Exp.int(2)),
          );
        let case_study_cos_square_substitution =
          plus(
            minus(divide(Exp.int(3), Exp.int(2)), cos_2x),
            divide(
              divide(plus(Exp.int(1), cos_4x), Exp.int(2)),
              Exp.int(2),
            ),
          );
        let case_study_final =
          plus(
            minus(divide(Exp.int(7), Exp.int(4)), cos_2x),
            times(divide(Exp.int(1), Exp.int(4)), cos_4x),
          );
        dump_case_study_step(
          "nested_sine_power",
          case_study_source,
          case_study_nested_power,
        );
        dump_case_study_step(
          "sine_power_reduction",
          case_study_nested_power,
          case_study_after_trig_substitution,
        );
        dump_case_study_step(
          "square_as_product",
          case_study_after_trig_substitution,
          case_study_square_as_product,
        );
        dump_case_study_step(
          "square_over_four",
          case_study_after_trig_substitution,
          case_study_square_over_four,
        );
        dump_case_study_step(
          "expanded_square",
          case_study_square_over_four,
          case_study_expanded_square,
        );
        dump_case_study_step(
          "cancel_two",
          case_study_expanded_square,
          case_study_cancel_two,
        );
        dump_case_study_step(
          "collect_terms",
          case_study_cancel_two,
          case_study_collected,
        );
        dump_case_study_step(
          "cos_square_subterm",
          power(cos_2x, Exp.int(2)),
          divide(plus(Exp.int(1), cos_4x), Exp.int(2)),
        );
        dump_case_study_step(
          "final_collect",
          case_study_cos_square_substitution,
          case_study_final,
        );
        let int_power_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Algebra,
            max_depth: 4,
            max_states: 80,
            source: power(x, Exp.int(4)),
            target: times(power(x, Exp.int(1)), power(x, Exp.int(3))),
          };
        let int_coq =
          Web.ProofSearchBackend.rocq_search_program(int_power_request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_power_split_ints.v",
          int_coq,
        );
        check(
          bool,
          "integer power candidate uses power tactic",
          true,
          string_contains("hazel_power_normalize", int_coq),
        );
        check(
          bool,
          "integer power candidate exports without ERROR",
          false,
          string_contains("ERROR", int_coq),
        );
      },
    ),
    test_case(
      "axiom search finds algebra distribution proof",
      `Quick,
      () => {
        let from_ = times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
        let to_ =
          plus(
            times(Exp.var("x"), Exp.var("y")),
            times(Exp.var("x"), Exp.var("z")),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.distribute_mul_add"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.distribute_mul_add", step.rule_id);
            check_exp_equal("local before", from_, step.before_exp);
            check_exp_equal("local after", to_, step.after_exp);
          | _ => fail("expected one search step")
          };
        | None => fail("expected algebra axiom search proof")
        };
      },
    ),
    test_case(
      "division distribution is structural, inherited, and profile controlled",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let plus_source = divide(plus(a, b), c);
        let plus_target = plus(divide(a, c), divide(b, c));
        let minus_source = divide(minus(a, b), c);
        let minus_target = minus(divide(a, c), divide(b, c));
        let direct_trace = (profile, source, target) =>
          Web.RewriteChecker.check_single_algebra_rule_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.map(Web.RewriteChecker.trace_summary_of_result);
        [Algebra, Trigonometry, Calculus]
        |> List.iter(level =>
             check(
               bool,
               Axioms.rewrite_level_label(level)
               ++ " inherits division distribution",
               true,
               direct_trace(
                 Axioms.math_profile(level),
                 minus_source,
                 minus_target,
               )
               |> Option.map((summary: Web.ProofTrace.trace_summary) =>
                    summary.rule_ids == ["alg.distribute_div_add"]
                  )
               |> Option.value(~default=false),
             )
           );
        check(
          bool,
          "addition uses the same structural rule",
          true,
          direct_trace(
            Axioms.math_profile(Algebra),
            plus_source,
            plus_target,
          )
          |> Option.is_some,
        );
        let x = Exp.var("x");
        let trig_source =
          divide(
            minus(Exp.int(1), builtin_cos(times(Exp.int(2), x))),
            Exp.int(2),
          );
        let trig_target =
          minus(
            divide(Exp.int(1), Exp.int(2)),
            divide(builtin_cos(times(Exp.int(2), x)), Exp.int(2)),
          );
        check(
          bool,
          "the trig case uses inherited Algebra distribution",
          true,
          direct_trace(
            Axioms.math_profile(Trigonometry),
            trig_source,
            trig_target,
          )
          |> Option.is_some,
        );
        let disabled_profile =
          Axioms.math_profile(Algebra)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_div_add",
             );
        check(
          bool,
          "disabled profile rejects division distribution",
          true,
          direct_trace(disabled_profile, minus_source, minus_target)
          |> Option.is_none,
        );
        let check_result_request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: trig_source,
            target: trig_target,
          };
        check(
          bool,
          "Check Result records the inherited division rule",
          true,
          local_profile_trace(
            ~profile=Axioms.math_profile(Trigonometry),
            ~settings,
            ~env,
            check_result_request,
          )
          |> Option.map((summary: Web.ProofTrace.trace_summary) =>
               List.mem("alg.distribute_div_add", summary.rule_ids)
             )
          |> Option.value(~default=false),
        );
        let disabled_trig_profile =
          Axioms.math_profile(Trigonometry)
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_div_add",
             );
        check(
          bool,
          "Check Result rejects the same target when the rule is disabled",
          true,
          local_profile_trace(
            ~profile=disabled_trig_profile,
            ~settings,
            ~env,
            check_result_request,
          )
          |> Option.is_none,
        );
        check(
          bool,
          "an incorrect denominator remains invalid",
          true,
          direct_trace(
            Axioms.math_profile(Algebra),
            minus_source,
            minus(divide(a, c), divide(b, Exp.var("d"))),
          )
          |> Option.is_none,
        );
        let summary =
          switch (
            direct_trace(
              Axioms.math_profile(Algebra),
              minus_source,
              minus_target,
            )
          ) {
          | Some(summary) => summary
          | None => fail("expected exact division-distribution trace")
          };
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Algebra,
            max_depth: 1,
            max_states: 80,
            source: minus_source,
            target: minus_target,
          };
        let replay =
          Web.ProofSearchBackend.rocq_replay_program(request, summary);
        write_text_file(
          "/tmp/hazel_stepper_rocq_division_distribution.v",
          replay,
        );
        check(
          bool,
          "Rocq replay uses the exact real-division lemma",
          true,
          string_contains("rewrite Rdiv_minus_distr", replay)
          && !string_contains("nra", replay),
        );
      },
    ),
    test_case(
      "axiom search finds one division-distribution step",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let from_ = divide(plus(a, b), c);
        let to_ = plus(divide(a, c), divide(b, c));
        switch (
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.distribute_div_add"],
            ~log=false,
            from_,
            to_,
          )
        ) {
        | Some({steps: [step], _}) =>
          check(string, "rule", "alg.distribute_div_add", step.rule_id);
          check_exp_equal("local before", from_, step.before_exp);
          check_exp_equal("local after", to_, step.after_exp);
        | Some(_)
        | None => fail("expected one division-distribution search step")
        };
      },
    ),
    test_case(
      "axiom search splits exponent addition for any base expression",
      `Quick,
      () => {
        let base = plus(Exp.var("x"), Exp.int(1));
        let from_ = power(base, plus(Exp.int(2), Exp.int(3)));
        let to_ = times(power(base, Exp.int(2)), power(base, Exp.int(3)));
        let result =
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_add"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.power_add", step.rule_id);
            check_exp_equal("local before", from_, step.before_exp);
            check_exp_equal("local after", to_, step.after_exp);
          | _ => fail("expected one power-add search step")
          };
        | None => fail("expected exponent addition split proof")
        };
      },
    ),
    test_case(
      "axiom search splits literal powers for trig bases",
      `Quick,
      () => {
        let x = Exp.var("x");
        let base = builtin_sin(x);
        let from_ =
          plus(Exp.int(1), times(Exp.int(2), power(base, Exp.int(4))));
        let to_ =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              times(power(base, Exp.int(2)), power(base, Exp.int(2))),
            ),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_add"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.power_add", step.rule_id);
            check_exp_equal(
              "local before",
              power(base, Exp.int(4)),
              step.before_exp,
            );
            check_exp_equal(
              "local after",
              times(power(base, Exp.int(2)), power(base, Exp.int(2))),
              step.after_exp,
            );
          | _ => fail("expected one literal power split step")
          };
        | None => fail("expected literal exponent split proof")
        };
      },
    ),
    test_case(
      "axiom search splits literal powers at arbitrary additive exponent split",
      `Quick,
      () => {
        let base = Exp.var("x");
        let from_ = power(base, Exp.int(4));
        let to_ = times(power(base, Exp.int(1)), power(base, Exp.int(3)));
        let result =
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_add"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.power_add", step.rule_id);
            check_exp_equal("local before", from_, step.before_exp);
            check_exp_equal("local after", to_, step.after_exp);
          | _ => fail("expected one additive literal power split step")
          };
        | None => fail("expected additive literal exponent split proof")
        };
      },
    ),
    test_case(
      "axiom search splits literal powers into nested powers",
      `Quick,
      () => {
        let base = Exp.var("x");
        let from_ = power(base, Exp.int(4));
        let to_ = power(power(base, Exp.int(2)), Exp.int(2));
        let result =
          Web.AxiomSearch.search(
            ~level=Algebra,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_mul"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.power_mul", step.rule_id);
            check_exp_equal("local before", from_, step.before_exp);
            check_exp_equal("local after", to_, step.after_exp);
          | _ => fail("expected one multiplicative literal power split step")
          };
        | None => fail("expected nested literal exponent split proof")
        };
      },
    ),
    test_case(
      "axiom search splits trig literal powers into nested powers in context",
      `Quick,
      () => {
        let x = Exp.var("x");
        let base = builtin_sin(x);
        let from_ =
          plus(Exp.int(1), times(Exp.int(2), power(base, Exp.int(4))));
        let to_ =
          plus(
            Exp.int(1),
            times(
              Exp.int(2),
              power(power(base, Exp.int(2)), Exp.int(2)),
            ),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_mul"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "alg.power_mul", step.rule_id);
            check_exp_equal(
              "local before",
              power(base, Exp.int(4)),
              step.before_exp,
            );
            check_exp_equal(
              "local after",
              power(power(base, Exp.int(2)), Exp.int(2)),
              step.after_exp,
            );
          | _ => fail("expected one nested trig power split step")
          };
        | None => fail("expected nested trig literal exponent split proof")
        };
      },
    ),
    test_case(
      "axiom search preserves Float operators in literal power splits",
      `Quick,
      () => {
        let x = Exp.var("x");
        let base = builtin_sin(x);
        let check_split = (exponent, inner, outer) => {
          let from_ = float_power(base, float(exponent));
          let to_ =
            float_power(float_power(base, float(inner)), float(outer));
          switch (
            Web.AxiomSearch.search(
              ~level=Trigonometry,
              ~max_depth=1,
              ~allowed_rule_ids=["alg.power_mul"],
              ~log=false,
              from_,
              to_,
            )
          ) {
          | Some(result) =>
            switch (result.steps) {
            | [step] => check_exp_equal("Float split", to_, step.after_exp)
            | _ => fail("expected one Float power split step")
            }
          | None => fail("expected Float literal exponent split proof")
          };
        };
        check_split(4.0, 2.0, 2.0);
        check_split(6.0, 3.0, 2.0);
        check(
          bool,
          "non-factorization rejected",
          true,
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.power_mul"],
            ~log=false,
            float_power(base, float(5.0)),
            float_power(float_power(base, float(2.0)), float(2.0)),
          )
          |> Option.is_none,
        );
        check(
          bool,
          "disabled rule cannot split Float power",
          true,
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.add_comm"],
            ~log=false,
            float_power(base, float(4.0)),
            float_power(float_power(base, float(2.0)), float(2.0)),
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "trig rewrites preserve Float operators and literals",
      `Quick,
      () => {
        let x = Exp.var("x");
        let sin_squared = float_power(builtin_sin(x), float(2.0));
        let reduced =
          float_divide(
            float_minus(float(1.0), cos(float_times(float(2.0), x))),
            float(2.0),
          );
        let rewrites =
          Web.TrigRewrite.apply_rule_at_root(
            "trig.sin_squared_double",
            sin_squared,
          );
        switch (rewrites) {
        | [rewrite] =>
          check_exp_equal("Float trig result", reduced, rewrite.after_exp)
        | _ => fail("expected one Float power-reduction rewrite")
        };
        check(
          bool,
          "unrelated rule does not rewrite",
          true,
          Web.TrigRewrite.apply_rule_at_root("trig.sin_sum", sin_squared)
          == [],
        );
        check(
          bool,
          "disabled trig rule cannot search",
          true,
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["alg.add_comm"],
            ~log=false,
            sin_squared,
            reduced,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "axiom search splits literal powers with UI multiplication association",
      `Quick,
      () => {
        let x = Exp.var("x");
        let base = builtin_sin(x);
        let squared = power(base, Exp.int(2));
        let from_ =
          plus(Exp.int(1), times(Exp.int(2), power(base, Exp.int(4))));
        let to_ =
          plus(Exp.int(1), times(times(Exp.int(2), squared), squared));
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=2,
            ~allowed_rule_ids=["alg.power_add", "arith.mul_assoc"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "two search steps", 2, result.steps |> List.length);
          check(
            string,
            "first rule",
            "alg.power_add",
            List.nth(result.steps, 0).rule_id,
          );
          check(
            string,
            "second rule",
            "arith.mul_assoc",
            List.nth(result.steps, 1).rule_id,
          );
        | None => fail("expected literal exponent split plus reassociation")
        };
      },
    ),
    test_case(
      "single trigonometry step accepts cofunction identity",
      `Quick,
      () => {
        let theta = Exp.var("theta");
        let from_ = sin(minus(divide(Exp.var("pi"), Exp.int(2)), theta));
        let to_ = cos(theta);
        let result =
          require_single_step_result_at_level(Trigonometry, from_, to_);
        check(
          string,
          "justification",
          "trigonometry one step",
          result.justification,
        );
        check(bool, "not exportable yet", false, result.exportable);
        check(
          bool,
          "trace has cofunction",
          true,
          has_trace_rule("trig.sin_cofunction", result),
        );
      },
    ),
    test_case(
      "trigonometry level rejects trig identity below trig level",
      `Quick,
      () => {
        let theta = Exp.var("theta");
        check(
          bool,
          "algebra cannot use cofunction identity",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Algebra,
            ~settings,
            ~env,
            sin(minus(divide(Exp.var("pi"), Exp.int(2)), theta)),
            cos(theta),
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "axiom search finds sine sum identity",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let from_ = sin(plus(x, y));
        let to_ = plus(times(sin(x), cos(y)), times(cos(x), sin(y)));
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["trig.sin_sum"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          switch (result.steps) {
          | [step] =>
            check(string, "rule", "trig.sin_sum", step.rule_id);
            check_exp_equal("local before", from_, step.before_exp);
            check_exp_equal("local after", to_, step.after_exp);
          | _ => fail("expected one trig search step")
          };
        | None => fail("expected trig axiom search proof")
        };
      },
    ),
    test_case(
      "axiom search accepts builtin sine sum target",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let from_ = builtin_sin(plus(x, y));
        let to_ =
          plus(
            times(builtin_sin(x), builtin_cos(y)),
            times(builtin_cos(x), builtin_sin(y)),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["trig.sin_sum"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "one search step", 1, result.steps |> List.length);
          check(
            bool,
            "trig search is exportable",
            true,
            Web.AxiomSearch.trace_summary(result).exportable,
          );
        | None => fail("expected builtin trig axiom search proof")
        };
      },
    ),
    test_case(
      "axiom search rejects incomplete sine sum target under budget",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let incomplete_target =
          plus(times(builtin_sin(x), builtin_cos(y)), builtin_cos(x));
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=4,
            ~max_states=25,
            ~allowed_rule_ids=["trig.sin_sum", "trig.cos_sum"],
            ~log=false,
            builtin_sin(plus(x, y)),
            incomplete_target,
          );
        check(
          bool,
          "no proof for incomplete target",
          true,
          result |> Option.is_none,
        );
      },
    ),
    test_case(
      "axiom search accepts sine sum with commuted product",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let from_ = builtin_sin(plus(x, y));
        let to_ =
          plus(
            times(builtin_sin(x), builtin_cos(y)),
            times(builtin_sin(y), builtin_cos(x)),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=2,
            ~allowed_rule_ids=["trig.sin_sum", "arith.mul_comm"],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "two search steps", 2, result.steps |> List.length);
          check(
            string,
            "first rule",
            "trig.sin_sum",
            List.nth(result.steps, 0).rule_id,
          );
          check(
            string,
            "second rule",
            "arith.mul_comm",
            List.nth(result.steps, 1).rule_id,
          );
        | None => fail("expected trig sum plus multiplication commute proof")
        };
      },
    ),
    test_case(
      "axiom search accepts sine sum with nested reordered product under all rules",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let from_ = builtin_sin(plus(x, y));
        let to_ =
          plus(
            times(builtin_sin(x), builtin_cos(y)),
            times(builtin_sin(y), builtin_cos(x)),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=4,
            ~max_states=80,
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(
            bool,
            "trace includes sine sum",
            true,
            result.steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "trig.sin_sum"
               ),
          );
          check(
            bool,
            "trace includes multiplication reorder",
            true,
            result.steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "arith.reorder_mul_factors"
               ),
          );
        | None =>
          fail("expected all-rules sine sum plus nested product reorder")
        };
      },
    ),
    test_case(
      "axiom search accepts builtin trig sum and difference identities",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let cases = [
          (
            "trig.sin_diff",
            builtin_sin(minus(x, y)),
            minus(
              times(builtin_sin(x), builtin_cos(y)),
              times(builtin_cos(x), builtin_sin(y)),
            ),
          ),
          (
            "trig.cos_sum",
            builtin_cos(plus(x, y)),
            minus(
              times(builtin_cos(x), builtin_cos(y)),
              times(builtin_sin(x), builtin_sin(y)),
            ),
          ),
          (
            "trig.cos_diff",
            builtin_cos(minus(x, y)),
            plus(
              times(builtin_cos(x), builtin_cos(y)),
              times(builtin_sin(x), builtin_sin(y)),
            ),
          ),
        ];
        cases
        |> List.iter(((rule_id, from_, to_)) => {
             let result =
               Web.AxiomSearch.search(
                 ~level=Trigonometry,
                 ~max_depth=1,
                 ~allowed_rule_ids=[rule_id],
                 ~log=false,
                 from_,
                 to_,
               );
             switch (result) {
             | Some(result) =>
               check(
                 string,
                 rule_id ++ " proof rule",
                 rule_id,
                 switch (result.steps) {
                 | [step] => step.rule_id
                 | _ => "wrong step count"
                 },
               )
             | None =>
               fail("expected trig axiom search proof for " ++ rule_id)
             };
           });
      },
    ),
    test_case(
      "trigonometry handles reflection and negative angle identities",
      `Quick,
      () => {
        let theta = Exp.var("theta");
        check(
          bool,
          "cos(pi - theta) -> -cos(theta)",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            cos(minus(Exp.var("pi"), theta)),
            negate(cos(theta)),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "cos(-theta) -> cos(theta)",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            cos(negate(theta)),
            cos(theta),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "trigonometry handles substituted-pi reflection in check result",
      `Quick,
      () => {
        let x = Exp.var("x");
        check(
          bool,
          "builtin sin(pi - x) -> builtin sin(x)",
          true,
          Web.RewriteChecker.check_written_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            builtin_sin(float_minus(float(Float.pi), x)),
            builtin_sin(x),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "trigonometry matches statics-substituted pi and builtin trig functions",
      `Quick,
      () => {
        let theta = Exp.var("theta");
        let from_ =
          builtin_sin(
            float_minus(float_divide(float(Float.pi), float(2.0)), theta),
          );
        let to_ = builtin_cos(theta);
        check(
          bool,
          "builtin sin(pi/2 - theta) -> builtin cos(theta)",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            from_,
            to_,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "trigonometry matches builtin double-angle with float multiplication",
      `Quick,
      () => {
        let x = Exp.var("x");
        let from_ = builtin_cos(float_times(float(2.0), x));
        check(
          bool,
          "cos(2*x) has applicable double-angle rewrites",
          true,
          Web.TrigRewrite.applicable_at_root(from_)
          |> List.exists((rewrite: Web.TrigRewrite.rewrite) =>
               rewrite.rule_id == "trig.cos_double_square"
             ),
        );
      },
    ),
    test_case(
      "axiom search accepts sine double-angle with reordered factors",
      `Quick,
      () => {
        let x = Exp.var("x");
        let from_ = builtin_sin(times(Exp.int(2), x));
        let to_ =
          times(times(Exp.int(2), builtin_cos(x)), builtin_sin(x));
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=2,
            ~allowed_rule_ids=[
              "trig.sin_double",
              "arith.reorder_mul_factors",
            ],
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(int, "two search steps", 2, result.steps |> List.length);
          check(
            string,
            "first rule",
            "trig.sin_double",
            List.nth(result.steps, 0).rule_id,
          );
          check(
            string,
            "second rule",
            "arith.reorder_mul_factors",
            List.nth(result.steps, 1).rule_id,
          );
        | None =>
          fail("expected sine double-angle plus multiplication reorder")
        };
      },
    ),
    test_case(
      "axiom search accepts sine double-angle with all trigonometry rules",
      `Quick,
      () => {
        let x = Exp.var("x");
        let from_ = builtin_sin(times(Exp.int(2), x));
        let to_ =
          times(times(Exp.int(2), builtin_cos(x)), builtin_sin(x));
        let result =
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=4,
            ~max_states=80,
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(
            bool,
            "trace includes sine double-angle",
            true,
            result.steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "trig.sin_double"
               ),
          );
          check(
            bool,
            "trace includes multiplication reorder",
            true,
            result.steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "arith.reorder_mul_factors"
               ),
          );
        | None =>
          fail(
            "expected all-rules sine double-angle plus multiplication reorder",
          )
        };
      },
    ),
    test_case(
      "trigonometry exposes Pythagorean identities",
      `Quick,
      () => {
        let x = Exp.var("x");
        let cos_squared = power(cos(x), Exp.int(2));
        check(
          bool,
          "cos(x)^2 has Pythagorean rewrite",
          true,
          Web.TrigRewrite.applicable_at_root(cos_squared)
          |> List.exists((rewrite: Web.TrigRewrite.rewrite) =>
               rewrite.rule_id == "trig.cos_squared_pythagorean"
             ),
        );
        check(
          bool,
          "sin(x)^2 + cos(x)^2 -> 1",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            plus(power(sin(x), Exp.int(2)), cos_squared),
            Exp.int(1),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "trigonometry handles UI-style Pythagorean power",
      `Quick,
      () => {
        let x = Exp.var("x");
        let from_ =
          plus(
            float_power(builtin_sin(x), float(2.0)),
            float_power(builtin_cos(x), float(2.0)),
          );
        check(
          bool,
          "sin(x)**2 + cos(x)**2 -> 1",
          true,
          Web.RewriteChecker.check_written_step_trace_at_level(
            ~level=Trigonometry,
            ~settings,
            ~env,
            from_,
            Exp.int(1),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "proof search finds UI-style Pythagorean target",
          true,
          Web.AxiomSearch.search(
            ~level=Trigonometry,
            ~max_depth=1,
            ~allowed_rule_ids=["trig.pythagorean_sin_cos"],
            ~log=false,
            from_,
            Exp.int(1),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "axiom search reorders a four-term arithmetic chain",
      `Quick,
      () => {
        let from_ =
          plus(
            plus(plus(Exp.int(1), Exp.int(2)), Exp.int(3)),
            Exp.int(4),
          );
        let to_ =
          plus(
            plus(plus(Exp.int(4), Exp.int(3)), Exp.int(2)),
            Exp.int(1),
          );
        let result =
          Web.AxiomSearch.search(
            ~level=Arithmetic,
            ~max_depth=4,
            ~log=false,
            from_,
            to_,
          );
        switch (result) {
        | Some(result) =>
          check(bool, "has at least one step", true, result.steps != []);
          check(
            bool,
            "uses small addition reorder",
            true,
            result.steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "arith.reorder_add_terms"
               ),
          );
        | None => fail("expected arithmetic reorder search proof")
        };
      },
    ),
    test_case(
      "axiom search respects depth bound",
      `Quick,
      () => {
        let result =
          Web.AxiomSearch.search(
            ~level=Arithmetic,
            ~max_depth=0,
            ~allowed_rule_ids=["arith.const_fold"],
            ~log=false,
            plus(Exp.int(1), Exp.int(2)),
            Exp.int(3),
          );
        check(bool, "no proof at depth zero", true, result |> Option.is_none);
      },
    ),
    test_case("single algebra step is level-gated", `Quick, () =>
      check(
        bool,
        "arithmetic cannot use distribution as one step",
        true,
        Web.RewriteChecker.check_single_step_trace_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
          plus(
            times(Exp.var("x"), Exp.var("y")),
            times(Exp.var("x"), Exp.var("z")),
          ),
        )
        |> Option.is_none,
      )
    ),
    test_case(
      "single algebra step accepts additive cancellation",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            minus(plus(Exp.var("x"), Exp.var("y")), Exp.var("y")),
            Exp.var("x"),
          );
        check(
          string,
          "justification",
          "algebra one step",
          result.justification,
        );
        check(
          bool,
          "trace has cancellation",
          true,
          has_trace_rule("alg.cancel_common_add", result),
        );
      },
    ),
    test_case(
      "single algebra step cancels identical terms to zero",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            minus(Exp.var("x"), Exp.var("x")),
            Exp.int(0),
          );
        check(
          bool,
          "trace has cancellation",
          true,
          has_trace_rule("alg.cancel_common_add", result),
        );
      },
    ),
    test_case(
      "single algebra step rejects distribution plus simplification",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(Exp.int(1), x));
        let distributed = plus(times(x, Exp.int(1)), times(x, x));
        let simplified = plus(x, power(x, Exp.int(2)));
        let result =
          require_single_step_result_at_level(Algebra, source, distributed);
        check(
          bool,
          "raw distribution is one step",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
        check(
          bool,
          "x * (1 + x) cannot jump directly to x + x**2",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Algebra,
            ~settings,
            ~env,
            source,
            simplified,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "single algebra step distributes subtraction as addition of a negative",
      `Quick,
      () => {
        let x = Exp.var("x");
        let factor = plus(x, Exp.int(1));
        let source = times(factor, minus(x, Exp.int(2)));
        let target =
          plus(times(factor, x), times(factor, negate(Exp.int(2))));
        let result =
          require_single_step_result_at_level(Algebra, source, target);
        check(
          bool,
          "subtraction distribution uses the named rule",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
        let without_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_mul_add",
            Axioms.math_profile(Algebra),
          );
        check(
          bool,
          "disabled distribution rejects the negative form",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile=without_distribution,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "single algebra step rejects distribution plus constant folding",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(plus(Exp.int(1), Exp.int(2)), x));
        let distributed =
          plus(
            plus(times(x, Exp.int(1)), times(x, Exp.int(2))),
            times(x, x),
          );
        let simplified = plus(times(x, Exp.int(3)), times(x, x));
        let result =
          require_single_step_result_at_level(Algebra, source, distributed);
        check(
          bool,
          "three-term distribution is one step",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
        check(
          bool,
          "x * (1 + 2 + x) cannot jump directly to x * 3 + x * x",
          true,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Algebra,
            ~settings,
            ~env,
            source,
            simplified,
          )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "single algebra step permits power notation cleanup around distribution",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(plus(Exp.int(1), Exp.int(2)), x));
        let power_distributed =
          plus(
            plus(times(x, Exp.int(1)), times(x, Exp.int(2))),
            power(x, Exp.int(2)),
          );
        let result =
          require_single_step_result_at_level(
            Algebra,
            source,
            power_distributed,
          );
        check(
          bool,
          "distribution can rewrite x*x as x**2 cleanup",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case(
      "single algebra step permits AC cleanup around distribution",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(plus(Exp.int(1), Exp.int(2)), x));
        let ac_distributed =
          plus(
            plus(times(Exp.int(1), x), times(Exp.int(2), x)),
            times(x, x),
          );
        let result =
          require_single_step_result_at_level(
            Algebra,
            source,
            ac_distributed,
          );
        check(
          bool,
          "AC-distributed target is one distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case(
      "single algebra step accepts one factoring move",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            plus(
              times(Exp.var("y"), Exp.var("x")),
              times(Exp.var("z"), Exp.var("x")),
            ),
            times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z"))),
          );
        check(
          string,
          "justification",
          "algebra one step",
          result.justification,
        );
        check(bool, "exportable", true, result.exportable);
        check(
          bool,
          "trace has factoring",
          true,
          has_trace_rule("alg.factor_common", result),
        );
      },
    ),
    test_case(
      "single algebra step expands three-term distribution",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            times(
              Exp.var("x"),
              plus(plus(Exp.var("a"), Exp.var("b")), Exp.var("c")),
            ),
            plus(
              plus(
                times(Exp.var("x"), Exp.var("a")),
                times(Exp.var("x"), Exp.var("b")),
              ),
              times(Exp.var("x"), Exp.var("c")),
            ),
          );
        check(
          bool,
          "trace has distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case(
      "single algebra step accepts full FOIL expansion",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            times(
              plus(Exp.var("x"), Exp.int(1)),
              plus(Exp.var("x"), Exp.int(2)),
            ),
            plus(
              plus(
                times(Exp.var("x"), Exp.var("x")),
                times(Exp.int(3), Exp.var("x")),
              ),
              Exp.int(2),
            ),
          );
        check(
          bool,
          "trace has polynomial expansion",
          true,
          has_trace_rule("alg.expand_polynomial", result),
        );
        check(
          bool,
          "trace has distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
      },
    ),
    test_case(
      "single algebra step accepts signed FOIL expansion",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(minus(x, Exp.int(2)), plus(x, Exp.int(5)));
        let target =
          minus(
            plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
            Exp.int(10),
          );
        let result =
          require_single_step_result_at_level(Algebra, source, target);
        check(
          bool,
          "signed trace has polynomial expansion",
          true,
          has_trace_rule("alg.expand_polynomial", result),
        );
        check(
          bool,
          "signed trace has distribution",
          true,
          has_trace_rule("alg.distribute_mul_add", result),
        );
        check(
          bool,
          "wrong signed coefficient is rejected",
          false,
          Web.RewriteChecker.check_single_step_trace_at_level(
            ~level=Algebra,
            ~settings,
            ~env,
            source,
            minus(
              plus(power(x, Exp.int(2)), times(Exp.int(4), x)),
              Exp.int(10),
            ),
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "single algebra step accepts symbolic FOIL expansion",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let c = Exp.var("c");
        let d = Exp.var("d");
        let source = times(plus(a, b), plus(c, d));
        let target =
          plus(
            plus(plus(times(a, c), times(a, d)), times(b, c)),
            times(b, d),
          );
        let result =
          require_single_step_result_at_level(Algebra, source, target);
        check(
          bool,
          "symbolic trace has polynomial expansion",
          true,
          has_trace_rule("alg.expand_polynomial", result),
        );
      },
    ),
    test_case(
      "single algebra step factors a numeric coefficient",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            plus(
              times(Exp.int(2), Exp.var("x")),
              times(Exp.int(2), Exp.var("y")),
            ),
            times(Exp.int(2), plus(Exp.var("x"), Exp.var("y"))),
          );
        check(
          bool,
          "trace has factoring",
          true,
          has_trace_rule("alg.factor_common", result),
        );
      },
    ),
    test_case(
      "single algebra step factors a repeated variable",
      `Quick,
      () => {
        let result =
          require_single_step_result_at_level(
            Algebra,
            plus(
              times(Exp.var("x"), Exp.var("x")),
              times(Exp.var("x"), Exp.var("y")),
            ),
            times(Exp.var("x"), plus(Exp.var("x"), Exp.var("y"))),
          );
        check(
          bool,
          "trace has factoring",
          true,
          has_trace_rule("alg.factor_common", result),
        );
      },
    ),
    test_case("affine simplifier produces grouped expression", `Quick, () =>
      check_simplifies(
        "1 + 2 + x -> x + 3",
        plus(plus(Exp.int(1), Exp.int(2)), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
      )
    ),
    test_case("affine simplifier groups coefficients", `Quick, () =>
      check_simplifies(
        "x + x + x + 10 -> 3 * x + 10",
        plus(
          plus(plus(Exp.var("x"), Exp.var("x")), Exp.var("x")),
          Exp.int(10),
        ),
        plus(times(Exp.int(3), Exp.var("x")), Exp.int(10)),
      )
    ),
    test_case(
      "algebra auto simplify expands FOIL and groups terms", `Quick, () =>
      check_simplifies_at_level(
        "(x + 1) * (x + 2) -> x * x + 3 * x + 2",
        Algebra,
        times(
          plus(Exp.var("x"), Exp.int(1)),
          plus(Exp.var("x"), Exp.int(2)),
        ),
        plus(
          plus(
            times(Exp.var("x"), Exp.var("x")),
            times(Exp.int(3), Exp.var("x")),
          ),
          Exp.int(2),
        ),
      )
    ),
    test_case("arithmetic auto simplify does not FOIL", `Quick, () =>
      check(
        bool,
        "arithmetic level cannot simplify polynomial multiplication",
        true,
        Web.RewriteChecker.simplify_at_level(
          ~level=Arithmetic,
          ~settings,
          ~env,
          times(
            plus(Exp.var("x"), Exp.int(1)),
            plus(Exp.var("x"), Exp.int(2)),
          ),
        )
        |> Option.is_none,
      )
    ),
    test_case(
      "affine result names arithmetic group",
      `Quick,
      () => {
        let result =
          require_written_result(
            plus(Exp.int(3), Exp.var("x")),
            plus(Exp.var("x"), Exp.int(3)),
          );
        check(string, "justification", "arithmetic", result.justification);
        check(
          option(string),
          "group name",
          Some("arithmetic"),
          result.group |> Option.map(rewrite_group_name),
        );
        check(bool, "exportable", true, result.exportable);
        check(
          bool,
          "trace has commutativity",
          true,
          has_trace_rule("arith.add_comm", result),
        );
        check(
          bool,
          "trace has collection",
          true,
          has_trace_rule("arith.collect_like_terms", result),
        );
        check(
          bool,
          "trace rules have Lean hints",
          true,
          result.trace |> List.for_all(has_lean_hint),
        );
      },
    ),
    test_case(
      "affine written step exposes exportable trace summary",
      `Quick,
      () => {
        let summary =
          require_written_trace(
            plus(Exp.int(3), Exp.var("x")),
            plus(Exp.var("x"), Exp.int(3)),
          );
        check(
          string,
          "label",
          "affine normalization",
          Web.ProofTrace.trace_summary_label(summary),
        );
        check(
          option(string),
          "group",
          Some("arithmetic"),
          summary.group_name,
        );
        check(bool, "exportable", true, summary.exportable);
        check(
          bool,
          "trace has semantic affine operation",
          true,
          summary.rule_ids == ["arith.affine_normalize"],
        );
        check(
          bool,
          "trace hides prerequisite implementation rules",
          false,
          has_rule_id("arith.collect_like_terms", summary.rule_ids),
        );
        check(
          bool,
          "from trace records semantic operation",
          true,
          summary.from_rule_ids == ["arith.affine_normalize"],
        );
        check(
          bool,
          "to trace needs no reverse operation",
          false,
          has_rule_id("arith.affine_normalize", summary.to_rule_ids),
        );
        check_exp_equal(
          "from normal expression",
          plus(Exp.var("x"), Exp.int(3)),
          summary.from_normal_exp,
        );
        check_exp_equal(
          "to normal expression",
          plus(Exp.var("x"), Exp.int(3)),
          summary.to_normal_exp,
        );
        let (from_hints, to_hints) =
          Web.RewriteChecker.trace_summary_prover_hints(
            ~prover="lean",
            summary,
          );
        check(
          bool,
          "semantic normalizer has no legacy Lean rewrite hint",
          false,
          from_hints != [],
        );
        check(
          bool,
          "reverse side has no legacy Lean rewrite hint",
          false,
          to_hints != [],
        );
      },
    ),
    test_case(
      "arithmetic normalization records ordered prover replay steps",
      `Quick,
      () => {
        let summary =
          require_written_trace(
            plus(
              plus(plus(Exp.var("x"), Exp.int(1)), Exp.var("x")),
              Exp.int(2),
            ),
            plus(times(Exp.int(2), Exp.var("x")), Exp.int(3)),
          );
        check(
          list(string),
          "normalizer prover rule order",
          ["arith.affine_normalize"],
          summary.prover_steps |> List.map(prover_step_rule_id),
        );
        check(
          list(string),
          "all normalizer steps",
          ["normalization"],
          summary.prover_steps |> List.map(prover_step_origin),
        );
      },
    ),
    test_case(
      "arithmetic normalization prover replay steps form expression path",
      `Quick,
      () => {
        let source =
          plus(
            plus(plus(Exp.var("x"), Exp.int(1)), Exp.var("x")),
            Exp.int(2),
          );
        let collected = plus(times(Exp.int(2), Exp.var("x")), Exp.int(3));
        let summary = require_written_trace(source, collected);
        switch (summary.prover_steps) {
        | [affine] =>
          check_prover_step(
            "affine semantic operation",
            "arith.affine_normalize",
            source,
            collected,
            affine,
          )
        | _ => fail("expected one affine semantic prover step")
        };
      },
    ),
    test_case(
      "evaluation equality fallback is not exportable",
      `Quick,
      () => {
        let result = require_written_result(Exp.bool(true), Exp.bool(true));
        check(
          string,
          "justification",
          "same evaluated result",
          result.justification,
        );
        check(
          option(string),
          "no group",
          None,
          result.group |> Option.map(rewrite_group_name),
        );
        check(bool, "not exportable", false, result.exportable);
        check(int, "no trace", 0, result.trace |> List.length);
        check(int, "no from trace", 0, result.from_trace |> List.length);
        check(int, "no to trace", 0, result.to_trace |> List.length);
        check_exp_equal(
          "from normal value",
          Exp.bool(true),
          result.from_normal_exp,
        );
        check_exp_equal(
          "to normal value",
          Exp.bool(true),
          result.to_normal_exp,
        );
        let summary = Web.RewriteChecker.trace_summary_of_result(result);
        let (from_hints, to_hints) =
          Web.RewriteChecker.trace_summary_prover_hints(
            ~prover="lean",
            summary,
          );
        check(int, "no from hints", 0, from_hints |> List.length);
        check(int, "no to hints", 0, to_hints |> List.length);
      },
    ),
    test_case(
      "Rocq export emits ordered theorems for a linear let development",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_linear_let_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected a linear let-development export")
          };
        write_text_file("/tmp/hazel_linear_let_development.v", export);
        check(
          bool,
          "exports definitions in dependency order",
          true,
          string_contains("Definition a : R := 3.", export)
          && string_contains("Definition b : R := (a + 5).", export),
        );
        check(
          bool,
          "each stepped binding receives a named theorem",
          true,
          string_contains("Theorem hazel_a_correct", export)
          && string_contains("Theorem hazel_b_correct", export)
          && string_contains("unfold a.", export),
        );
        check(
          bool,
          "the stepped continuation becomes the final theorem",
          true,
          string_contains("Theorem hazel_final_value", export)
          && string_contains("hazel_final_value_step_1", export),
        );
        check(
          bool,
          "whole Hazel let syntax never reaches the expression printer",
          false,
          string_contains("unsupported Coq real export term", export),
        );
      },
    ),
    test_case(
      "Rocq export composes named first and second derivative bindings",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_derivative_let_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected a derivative let-development export")
          };
        write_text_file("/tmp/hazel_derivative_let_development.v", export);
        check(
          bool,
          "exports the final function definitions",
          true,
          string_contains("Definition f (x : R)", export)
          && string_contains("Definition f1 (x : R)", export)
          && string_contains("Definition f2 (x : R)", export),
        );
        check(
          bool,
          "first derivative theorem refers to the original function",
          true,
          string_contains(
            "Theorem hazel_f1_correct : derivative_of f1 f",
            export,
          ),
        );
        check(
          bool,
          "second derivative theorem refers to the first derivative",
          true,
          string_contains(
            "Theorem hazel_f2_correct : derivative_of f2 f1",
            export,
          ),
        );
        check(
          bool,
          "each derivative has a semantic certificate",
          true,
          string_contains("hazel_f1_derivative_certificate", export)
          && string_contains("hazel_f2_derivative_certificate", export)
          && string_contains("derivable_pt_lim", export),
        );
      },
    ),
    test_case(
      "Rocq export follows bindings after evaluator substitutions",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_evaluated_derivative_let_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected an evaluated let-development export")
          };
        write_text_file("/tmp/hazel_evaluated_let_development.v", export);
        check(
          bool,
          "retains dependency theorems after let elimination",
          true,
          string_contains(
            "Theorem hazel_f1_correct : derivative_of f1 f",
            export,
          )
          && string_contains(
               "Theorem hazel_f2_correct : derivative_of f2 f1",
               export,
             ),
        );
        check(
          bool,
          "exports the computed final value without a function literal",
          true,
          string_contains("Theorem hazel_final_value : f2 (2) = 12", export)
          && !string_contains("Function literal", export),
        );
      },
    ),
    test_case(
      "Rocq export names an anonymous derivative source and beta-reduces its uses",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_anonymous_derivative_let_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected an anonymous derivative export")
          };
        write_text_file(
          "/tmp/hazel_anonymous_derivative_let_development.v",
          export,
        );
        check(
          bool,
          "introduces a stable Rocq name for the anonymous source",
          true,
          string_contains("Definition hazel_source_for_f1", export)
          && string_contains(
               "Theorem hazel_f1_correct : derivative_of f1 hazel_source_for_f1",
               export,
             ),
        );
        check(
          bool,
          "prints the beta-redex in the final theorem",
          true,
          string_contains("(fun x : R =>", export)
          && string_contains("Theorem hazel_final_value", export)
          && string_contains(
               "etransitivity; [apply hazel_final_value_step_1 |].",
               export,
             )
          && !string_contains("rewrite hazel_final_value_step_1.", export),
        );
      },
    ),
    test_case(
      "Rocq export composes the full trig Taylor derivative chain",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_trig_taylor_derivative_let_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected a trig Taylor derivative export")
          };
        write_text_file("/tmp/hazel_trig_taylor_derivative_chain.v", export);
        check(
          bool,
          "exports all three derivative dependencies",
          true,
          string_contains("Theorem hazel_f1_correct", export)
          && string_contains("Theorem hazel_f2_correct", export)
          && string_contains("Theorem hazel_f3_correct", export),
        );
        check(
          bool,
          "exports anonymous trig source and transcendental certificates",
          true,
          string_contains("Definition hazel_source_for_f1", export)
          && string_contains("derivable_pt_lim_sin", export)
          && string_contains("derivable_pt_lim_cos", export)
          && !string_contains("forall sin cos", export),
        );
      },
    ),
    test_case(
      "anonymous derivative export preserves recorded profile boundaries",
      `Quick,
      () =>
      check_raises(
        "missing recorded power rule",
        Failure(
          "the recorded calculus profile cannot certify let-bound derivative f1",
        ),
        () =>
        Web.StepperBase.Stepper.export_coq(
          sample_anonymous_derivative_let_export_chain(
            ~drop_power_rule=true,
            (),
          ),
        )
        |> ignore
      )
    ),
    test_case(
      "let derivative export replays an atomic recorded cleanup finish",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_derivative_let_export_chain(~atomic_f2_finish=true, ()),
            )
          ) {
          | Some(export) => export
          | None => fail("expected atomic derivative cleanup export")
          };
        write_text_file(
          "/tmp/hazel_derivative_atomic_cleanup_export.v",
          export,
        );
        check(
          bool,
          "certifies f2 without restoring the affine normalization macro",
          true,
          string_contains("Theorem hazel_f2_correct", export)
          && string_contains("H_hazel_recorded", export)
          && !string_contains("hazel_affine_normalize", export),
        );
      },
    ),
    test_case(
      "let derivative export requires its recorded scalar cleanup rule",
      `Quick,
      () =>
      check_raises(
        "missing recorded scalar normalization rule",
        Failure(
          "the recorded calculus profile cannot certify let-bound derivative f2",
        ),
        () =>
        Web.StepperBase.Stepper.export_coq(
          sample_derivative_let_export_chain(
            ~atomic_f2_finish=true,
            ~drop_atomic_scalar_rule=true,
            (),
          ),
        )
        |> ignore
      )
    ),
    test_case(
      "let derivative export does not restore a disabled calculus rule",
      `Quick,
      () =>
      check_raises(
        "missing recorded power rule",
        Failure(
          "the recorded calculus profile cannot certify let-bound derivative f1",
        ),
        () =>
        Web.StepperBase.Stepper.export_coq(
          sample_derivative_let_export_chain(~drop_power_rule=true, ()),
        )
        |> ignore
      )
    ),
    test_case(
      "let proof export rejects destructuring patterns explicitly",
      `Quick,
      () => {
        let unsupported =
          Exp.let_(
            Pat.tuple([Pat.var("a"), Pat.var("b")]),
            Exp.tuple([Exp.int(1), Exp.int(2)]),
            Exp.var("a"),
          );
        let model =
          step_model(
            ~expr=unsupported,
            ~step_kind=MissingStep(Web.MissingStep.Model.init),
            ~next_step=None,
          );
        check_raises(
          "simple variable lets only",
          Failure(
            "Rocq export currently supports only nonrecursive variable let-bindings",
          ),
          () =>
          Web.StepperBase.Stepper.export_coq(model) |> ignore
        );
      },
    ),
    test_case(
      "coq export prints parenthesized arithmetic without ERROR",
      `Quick,
      () => {
        let expr =
          Language.Exp.fresh(Parens(plus(Exp.var("x"), Exp.int(1))));
        let printed = Web.CoqExport.string_of_d(expr);
        check(string, "printed expression", "(x+1)", printed);
        check(
          bool,
          "does not emit ERROR",
          false,
          printed |> String.contains(_, 'E'),
        );
      },
    ),
    test_case(
      "coq real export prints trig and symbolic PI without ERROR",
      `Quick,
      () => {
        let expr = builtin_sin(minus(Exp.var("pi"), Exp.var("x")));
        let printed =
          Web.CoqExport.string_of_d_for_domain(
            ~domain=Web.CoqExport.Reals,
            expr,
          );
        check(string, "printed expression", "sin ((PI - x))", printed);
        check(
          bool,
          "does not emit ERROR",
          false,
          printed |> String.contains(_, 'E'),
        );
      },
    ),
    test_case(
      "coq real export prints unary function literals with lexical binders",
      `Quick,
      () => {
        let body = times(Exp.var("a"), Exp.var("x"));
        let function_ = Exp.fn(Pat.var("x"), body, None, None);
        let applied =
          Language.Exp.fresh(Ap(Forward, function_, Exp.int(2)));
        check(
          string,
          "printed application",
          "(fun x : R => (a * x)) (2)",
          Web.CoqExport.string_of_d_for_domain(
            ~domain=Web.CoqExport.Reals,
            applied,
          ),
        );
        check(
          string,
          "only the closure variable is quantified",
          "forall a : R,",
          Web.CoqExport.forall_string_for_domain(
            ~domain=Web.CoqExport.Reals,
            [applied],
          ),
        );
      },
    ),
    test_case(
      "coq real export rejects non-unary function patterns explicitly",
      `Quick,
      () => {
        let unsupported =
          Exp.fn(
            Pat.tuple([Pat.var("x"), Pat.var("y")]),
            plus(Exp.var("x"), Exp.var("y")),
            None,
            None,
          );
        check_raises(
          "tuple parameter",
          Failure(
            "unsupported Coq real function pattern: expected one variable parameter",
          ),
          () =>
          Web.CoqExport.string_of_d_for_domain(
            ~domain=Web.CoqExport.Reals,
            unsupported,
          )
          |> ignore
        );
      },
    ),
    test_case(
      "coq real export treats singleton math tuples as grouping",
      `Quick,
      () => {
        let x = Exp.var("x");
        let grouped_six = Language.Exp.fresh(Tuple([Exp.int(6)]));
        let source = plus(times(Exp.int(2), x), grouped_six);
        let target = plus(times(Exp.int(2), x), Exp.int(6));
        let printed =
          Web.CoqExport.string_of_d_for_domain(
            ~domain=Web.CoqExport.Reals,
            source,
          );
        check(string, "printed expression", "((2 * x) + 6)", printed);
        check(
          list(string),
          "finds variables inside grouping",
          ["x"],
          Web.CoqExport.unique_vars_in_ast(source),
        );

        let trace =
          collapsed_macro_summary(
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Arithmetic,
              max_depth: 1,
              max_states: 8,
              source,
              target,
            },
          );
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_written_step_export_chain(~source, ~target, ~trace),
            )
          ) {
          | Some(export) => export
          | None => fail("expected grouped arithmetic export")
          };
        write_text_file("/tmp/hazel_singleton_grouping_export.v", export);
        check(
          bool,
          "exports universally over reals",
          true,
          string_contains("forall x : R", export),
        );
        check(
          bool,
          "does not expose Hazel tuple syntax",
          false,
          string_contains("Tuple literal", export),
        );
      },
    ),
    test_case(
      "coq export detects trig applications through variable functions",
      `Quick,
      () => {
        let expr = cos(times(Exp.int(2), Exp.var("x")));
        check(
          bool,
          "variable-form cos requires real export",
          true,
          Web.CoqExport.requires_reals(expr),
        );
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_rocq_algebra_with_var_trig_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected trig algebra export")
          };
        write_text_file("/tmp/hazel_stepper_rocq_algebra_var_trig.v", export);
        check(
          bool,
          "uses real prelude",
          true,
          string_contains("Open Scope R_scope.", export),
        );
        check(
          bool,
          "uses algebra tactic search",
          true,
          string_contains("rocq.algebra_tactic_search", export),
        );
        check(
          bool,
          "prints variable-form cos application",
          true,
          string_contains("cos (2 * x)", export),
        );
        check(
          bool,
          "does not use integer prelude",
          false,
          string_contains("Open Scope Z_scope.", export),
        );
      },
    ),
    test_case(
      "coq export emits semantic derivative certificates",
      `Quick,
      () => {
        let x = Exp.var("x");
        let body = plus(power(x, Exp.int(2)), times(Exp.int(2), x));
        let source = diff(Exp.fn(Pat.var("x"), body, None, None), x);
        let target = plus(times(Exp.int(2), x), Exp.int(2));
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_calculus_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected derivative export")
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_derivative_export.v",
          export,
        );
        check(
          bool,
          "exports a derivative proposition",
          true,
          string_contains("Theorem hazel_derivative", export)
          && string_contains("derivable_pt_lim (fun x : R =>", export),
        );
        check(
          bool,
          "replays compositional derivative lemmas",
          true,
          string_contains("derivable_pt_lim_plus", export)
          && string_contains("derivable_pt_lim_Rsqr", export)
          && string_contains("derivable_pt_lim_mult", export),
        );
        check(
          bool,
          "does not print Hazel diff tuple syntax",
          false,
          string_contains("diff", export)
          || string_contains("Tuple literal", export),
        );
        let operator_source =
          expression_derivative(Exp.fn(Pat.var("x"), body, None, None), x);
        let operator_export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_calculus_export_chain(~source=operator_source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected new derivative-operator export")
          };
        check(
          bool,
          "new expression operator emits the same semantic certificate",
          true,
          string_contains("Theorem hazel_derivative", operator_export)
          && string_contains("derivable_pt_lim_Rsqr", operator_export)
          && !string_contains("$hazel.derivative", operator_export),
        );

        let denominator = plus(x, Exp.int(1));
        let quotient_source = diff(divide(x, denominator), x);
        let quotient_target =
          divide(minus(denominator, x), power(denominator, Exp.int(2)));
        let quotient_export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_calculus_export_chain(
                ~source=quotient_source,
                ~target=quotient_target,
              ),
            )
          ) {
          | Some(export) => export
          | None => fail("expected quotient derivative export")
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_derivative_quotient_export.v",
          quotient_export,
        );
        check(
          bool,
          "quotient export preserves its domain hypothesis",
          true,
          string_contains("(x + 1) <> 0 ->", quotient_export)
          && string_contains("derivable_pt_lim_div", quotient_export),
        );

        let reported_body =
          plus(
            plus(
              plus(power(x, Exp.int(2)), times(Exp.int(3), x)),
              Exp.int(5),
            ),
            times(Exp.int(3), x),
          );
        let reported_source = diff(reported_body, x);
        let reported_target = plus(Exp.int(6), times(Exp.int(2), x));
        let reported_profile = Axioms.math_profile(Calculus);
        let reported_normalized =
          Web.DifferentiationRewrite.normalize(
            ~rule_enabled=
              rule_id =>
                !Web.DifferentiationRewrite.is_basic_cleanup_rule_id(rule_id)
                && Axioms.visible_rule_enabled(
                     reported_profile.step_policy,
                     rule_id,
                   ),
            ~fuel=128,
            reported_source,
          );
        let reported_expected =
          Web.DifferentiationRewrite.cleanup(
            ~cleanup_enabled=
              capability =>
                List.mem(
                  capability,
                  reported_profile.step_policy.default_cleanup,
                ),
            reported_normalized.exp,
          );
        check(
          bool,
          "reported polynomial derivative normalization completes",
          true,
          reported_normalized.complete,
        );
        check(
          bool,
          "reported polynomial derivative removes diff",
          false,
          Web.DifferentiationRewrite.contains_diff(reported_expected),
        );
        check(
          bool,
          "reported polynomial derivative has the displayed affine normal form (expected "
          ++ Web.CoqExport.string_of_d_for_domain(
               ~domain=Web.CoqExport.Reals,
               reported_expected,
             )
          ++ ")",
          true,
          Web.RewriteChecker.rational_affine_normal_forms_equal(
            reported_expected,
            reported_target,
          ),
        );
        check(
          bool,
          "reported profile authorizes affine finishing",
          true,
          Axioms.guarded_normalization_backend_for_profile(
            reported_profile,
            "arith.affine_normalize",
          )
          |> Option.is_some,
        );
        let reported_export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_calculus_export_chain(
                ~source=reported_source,
                ~target=reported_target,
              ),
            )
          ) {
          | Some(export) => export
          | None => fail("expected collected polynomial derivative export")
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_derivative_collected_export.v",
          reported_export,
        );
        check(
          bool,
          "collected polynomial remains a derivative certificate",
          true,
          string_contains("Theorem hazel_derivative", reported_export),
        );
        check(
          bool,
          "collected polynomial does not fall back to tuple serialization",
          false,
          string_contains("Tuple literal", reported_export),
        );

        let reported_trace_rule_ids = [
          "alg.expand_polynomial",
          "alg.collect_like_terms",
          "calc.diff_sum",
          "derivative.basics",
          "calc.diff_product",
          "mul.identity",
          "calc.diff_power",
          "power.identity",
          "add.identity",
          "arith.add_assoc",
          "arith.add_comm",
          "arith.const_fold",
          "arith.collect_like_terms",
          "arith.mul_const",
        ];
        let base_profile = Axioms.math_profile(Calculus);
        let cleanup_enabled = capability =>
          reported_trace_rule_ids
          |> List.exists(rule_id =>
               Axioms.cleanup_capability_for_id(rule_id) == Some(capability)
             );
        let recorded_cleanup =
          base_profile.step_policy.default_cleanup
          |> List.filter(cleanup_enabled);
        let reconstructed_profile: Axioms.math_profile = {
          ...base_profile,
          step_policy: {
            default_cleanup: recorded_cleanup,
            visible_rules:
              base_profile.step_policy.visible_rules
              |> List.filter((rule: Axioms.visible_rule_policy) =>
                   List.mem(rule.rule_id, reported_trace_rule_ids)
                 ),
          },
        };
        let reconstructed_export =
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=reconstructed_profile,
            ~recorded_cleanup,
            reported_source,
            reported_target,
          );
        check(
          bool,
          "reported trace profile still produces calculus certificate",
          true,
          reconstructed_export |> Option.is_some,
        );
        reconstructed_export
        |> Option.iter(export =>
             write_text_file(
               "/tmp/hazel_stepper_rocq_derivative_reconstructed_profile.v",
               export,
             )
           );

        let other_source =
          diff(
            plus(
              plus(power(x, Exp.int(2)), times(Exp.int(4), x)),
              Exp.int(7),
            ),
            x,
          );
        let other_target = plus(Exp.int(4), times(Exp.int(2), x));
        check(
          bool,
          "recorded affine cleanup covers a structurally different polynomial",
          true,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=reconstructed_profile,
            ~recorded_cleanup,
            other_source,
            other_target,
          )
          |> Option.is_some,
        );
        check(
          bool,
          "recorded affine cleanup rejects an inequivalent derivative result",
          false,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=reconstructed_profile,
            ~recorded_cleanup,
            other_source,
            plus(Exp.int(5), times(Exp.int(2), x)),
          )
          |> Option.is_some,
        );
        let cleanup_without_commute =
          recorded_cleanup
          |> List.filter(capability => capability != Axioms.AddComm);
        let profile_without_commute: Axioms.math_profile = {
          ...reconstructed_profile,
          step_policy: {
            ...reconstructed_profile.step_policy,
            default_cleanup: cleanup_without_commute,
          },
        };
        check(
          bool,
          "reordered affine finish is unavailable when its recorded capability is disabled",
          false,
          Web.ProofSearchBackend.calculus_export_program_for_profile(
            ~profile=profile_without_commute,
            ~recorded_cleanup=cleanup_without_commute,
            other_source,
            other_target,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "coq export replays local algebra proof under trig context",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_local_algebra_under_trig_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected local algebra under trig export")
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_local_algebra_under_cos.v",
          export,
        );
        check(
          bool,
          "uses real prelude from row context",
          true,
          string_contains("Open Scope R_scope.", export),
        );
        check(
          bool,
          "uses whole-step algebra tactic search instead of local assertion replay",
          false,
          string_contains("assert (H_hazel_step_1", export),
        );
        check(
          bool,
          "uses algebra to prove the step",
          true,
          string_contains("\nhazel_algebra.\nQed.", export),
        );
      },
    ),
    test_case(
      "coq export proves local algebra under multiplication context without over-rewrite",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_local_algebra_under_mul_context_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected local algebra under multiplication export")
          };
        write_text_file(
          "/tmp/hazel_stepper_rocq_local_algebra_under_mul.v",
          export,
        );
        check(
          bool,
          "uses real prelude from trig context",
          true,
          string_contains("Open Scope R_scope.", export),
        );
        check(
          bool,
          "does not replay a local assertion that can over-rewrite",
          false,
          string_contains("assert (H_hazel_step_1", export),
        );
        check(
          bool,
          "uses algebra to prove the whole step",
          true,
          string_contains("\nhazel_algebra.\nQed.", export),
        );
      },
    ),
    test_case(
      "stepper coq export fixture dumps real generated proof",
      `Quick,
      () => {
        let export =
          switch (Web.StepperBase.Stepper.export_coq(sample_export_chain())) {
          | Some(export) => export
          | None => fail("expected stepper export")
          };
        write_text_file("/tmp/hazel_stepper_export_fixture.v", export);
        check(
          bool,
          "no ERROR marker",
          false,
          string_contains("ERROR", export),
        );
        check(
          bool,
          "imports universal real-number support",
          true,
          string_contains(
            "Require Import Rbase Rfunctions Rtrigo1 Cos_plus Lra Ring",
            export,
          ),
        );
        check(
          bool,
          "no ring tactic",
          false,
          string_contains("ring.", export),
        );
        check(
          bool,
          "exports prover replay steps",
          true,
          string_contains(
            "Hazel prover step 1: arith.affine_normalize",
            export,
          ),
        );
        check(
          bool,
          "exports semantic normalizer detail",
          true,
          string_contains(
            "detail: profile-enabled affine normalization",
            export,
          ),
        );
      },
    ),
    test_case(
      "stepper coq export dumps named algebra identity proofs",
      `Quick,
      () => {
        let a = Exp.var("a");
        let b = Exp.var("b");
        let identity_env = [("a", a), ("b", b)];
        let fixtures = [
          ("alg.square_of_sum", false, "/tmp/hazel_square_of_sum.v"),
          (
            "alg.difference_of_squares",
            true,
            "/tmp/hazel_difference_of_squares_factor.v",
          ),
          (
            "alg.cube_of_difference",
            false,
            "/tmp/hazel_cube_of_difference.v",
          ),
          ("alg.sum_of_cubes", true, "/tmp/hazel_sum_of_cubes_factor.v"),
        ];
        fixtures
        |> List.iter(((rule_id, reverse, path)) => {
             let spec =
               Web.AlgebraIdentityRewrite.specs
               |> List.find((spec: Web.TrigRewrite.spec) =>
                    spec.rule_id == rule_id
                  );
             let left = Web.TrigRewrite.instantiate(spec.left, identity_env);
             let right =
               Web.TrigRewrite.instantiate(spec.right, identity_env);
             let (source, target) = reverse ? (right, left) : (left, right);
             let export =
               switch (
                 Web.StepperBase.Stepper.export_coq(
                   sample_single_algebra_export_chain(~source, ~target),
                 )
               ) {
               | Some(export) => export
               | None => fail("expected named algebra identity export")
               };
             write_text_file(path, export);
             check(
               bool,
               rule_id ++ " breadcrumb",
               true,
               string_contains(rule_id, export),
             );
             check(
               bool,
               rule_id ++ " ring replay",
               true,
               string_contains("ring", export),
             );
             check(
               bool,
               rule_id ++ " no error marker",
               false,
               string_contains("ERROR", export),
             );
           });
      },
    ),
    test_case(
      "stepper coq export dumps tiny one-step arithmetic proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_one_step_constant_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected tiny one-step export")
          };
        write_text_file("/tmp/hazel_stepper_tiny_one_step.v", export);
        check(
          bool,
          "exports arithmetic one-step label",
          true,
          string_contains("Hazel written step: arithmetic one step", export),
        );
        check(
          bool,
          "exports direct single-transition replay",
          true,
          !string_contains("assert (H_hazel_step_1", export),
        );
        check(
          bool,
          "exports const fold breadcrumb",
          true,
          string_contains("arith.const_fold", export),
        );
        check(
          bool,
          "no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps tiny check-result arithmetic proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_check_result_constant_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected tiny check-result export")
          };
        write_text_file("/tmp/hazel_stepper_tiny_check_result.v", export);
        check(
          bool,
          "exports arithmetic check label",
          true,
          string_contains("Hazel written step: affine normalization", export),
        );
        check(
          bool,
          "exports direct single-transition replay",
          true,
          !string_contains("assert (H_hazel_step_1", export),
        );
        check(
          bool,
          "exports affine semantic operation",
          true,
          string_contains("arith.affine_normalize", export),
        );
        check(
          bool,
          "no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps reverse-order arithmetic proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_reverse_constant_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected reverse arithmetic export")
          };
        write_text_file("/tmp/hazel_stepper_reverse_constants.v", export);
        check(
          bool,
          "exports arithmetic check label",
          true,
          string_contains("Hazel written step: affine normalization", export),
        );
        check(
          bool,
          "exports direct single-transition replay",
          true,
          !string_contains("assert (H_hazel_step_1", export),
        );
        check(
          bool,
          "exports affine semantic operation",
          true,
          string_contains("arith.affine_normalize", export),
        );
        check(
          bool,
          "no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export guards no-op reparenthesization replay",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_reparenthesized_reverse_constant_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected reparenthesized reverse arithmetic export")
          };
        write_text_file(
          "/tmp/hazel_stepper_reparenthesized_reverse_constants.v",
          export,
        );
        check(
          bool,
          "exports guarded reparenthesization rewrite",
          true,
          string_contains("try rewrite <- equiv_exp2.", export),
        );
        check(
          bool,
          "exports direct single-transition replay",
          true,
          !string_contains("assert (H_hazel_step_1", export),
        );
      },
    ),
    test_case(
      "stepper coq export infers real domain from trig reparenthesization",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_reparenthesized_trig_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected reparenthesized trig export")
          };
        write_text_file("/tmp/hazel_stepper_reparenthesized_trig.v", export);
        check(
          bool,
          "uses real prelude",
          true,
          string_contains("Open Scope R_scope.", export),
        );
        check(
          bool,
          "does not use integer prelude",
          false,
          string_contains("Open Scope Z_scope.", export),
        );
        check(
          bool,
          "prints trig application",
          true,
          string_contains("sin x", export),
        );
      },
    ),
    test_case(
      "stepper coq export proves trinomial square macro over reals",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_integer_trinomial_square_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected integer trinomial-square export")
          };
        write_text_file(
          "/tmp/hazel_stepper_integer_trinomial_square.v",
          export,
        );
        check(
          bool,
          "exports algebra tactic-search label",
          true,
          string_contains("rocq.algebra_tactic_search", export),
        );
        check(
          bool,
          "exports universal real domain",
          true,
          string_contains("Open Scope R_scope.", export)
          && !string_contains("Open Scope Z_scope.", export)
          && !string_contains("Ltac hazel_integer_polynomial", export),
        );
        check(
          bool,
          "uses algebra tactic for macro lemma",
          true,
          string_contains("hazel_algebra.\nQed.", export),
        );
        check(
          bool,
          "does not fall back to generic rewrite search for macro",
          false,
          string_contains(
            "cbn.\nfirst [hazel_rewrite_search 8%nat | reflexivity].",
            export,
          ),
        );
      },
    ),
    test_case(
      "stepper coq export dumps symbolic affine arithmetic proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_reparenthesized_affine_variable_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected symbolic affine arithmetic export")
          };
        write_text_file("/tmp/hazel_stepper_affine_variables.v", export);
        check(
          bool,
          "exports exact affine replay",
          true,
          !string_contains("assert (H_hazel_step_", export)
          && !string_contains("Temporary affine fallback", export)
          && string_contains("arith.affine_normalize", export),
        );
        check(
          bool,
          "exports no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps algebra distribution breadcrumb",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_algebra_distribution_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected algebra distribution export")
          };
        write_text_file("/tmp/hazel_stepper_algebra_distribution.v", export);
        check(
          bool,
          "exports algebra label",
          true,
          string_contains("Hazel written step: algebra", export),
        );
        check(
          bool,
          "exports local distribution occurrence",
          true,
          string_contains("alg.distribute_mul_add", export)
          && string_contains("occurrence 2", export),
        );
        check(
          bool,
          "exports no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export replays the full cleanup FOIL route",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_foil_cleanup_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected cleanup FOIL export")
          };
        write_text_file("/tmp/hazel_stepper_foil_cleanup.v", export);
        check(
          bool,
          "exports all six visible transitions",
          true,
          string_contains("Lemma equiv_exp6", export),
        );
        check(
          bool,
          "exports distribution and collection evidence",
          true,
          string_contains("alg.distribute_mul_add", export)
          && string_contains("alg.collect_like_terms", export),
        );
        check(
          bool,
          "contains no export error",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export replays subtraction distribution written with a negative",
      `Quick,
      () => {
        let x = Exp.var("x");
        let factor = plus(x, Exp.int(1));
        let source = times(factor, minus(x, Exp.int(2)));
        let target =
          plus(times(factor, x), times(factor, negate(Exp.int(2))));
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_single_algebra_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected subtraction distribution export")
          };
        write_text_file(
          "/tmp/hazel_stepper_subtraction_distribution.v",
          export,
        );
        check(
          bool,
          "exports the named distribution rule",
          true,
          string_contains("alg.distribute_mul_add", export),
        );
        check(
          bool,
          "exports no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps scalar product simplification breadcrumb",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_scalar_product_simplification_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected scalar product simplification export")
          };
        write_text_file(
          "/tmp/hazel_stepper_scalar_product_simplification.v",
          export,
        );
        check(
          bool,
          "exports scalar simplification rule",
          true,
          string_contains("arith.simplify_scalar_products", export),
        );
        check(
          bool,
          "uses algebra tactic for scalar simplification",
          true,
          string_contains("hazel_algebra", export),
        );
      },
    ),
    test_case(
      "real export replays trig-argument scalar cleanup and rational distribution",
      `Quick,
      () => {
        let x = Exp.var("x");
        let nested_cos =
          builtin_cos(times(Exp.int(2), times(Exp.int(2), x)));
        let cos_4x = builtin_cos(times(Exp.int(4), x));
        let scalar_step =
          Web.ProofTrace.prover_step(
            ~origin=Web.ProofTrace.Normalization,
            ~rule_id="arith.simplify_scalar_products",
            ~before_full_exp=nested_cos,
            ~after_full_exp=cos_4x,
            ~before_exp=nested_cos,
            ~after_exp=cos_4x,
            ~detail="normalize a trigonometric argument",
          );
        check(
          bool,
          "scalar cleanup tries the general argument congruence certificate",
          true,
          Web.CoqProofExport.tactic_for_prover_step(
            ~domain=Web.CoqExport.Reals,
            scalar_step,
          )
          |> string_contains("hazel_function_argument_algebra"),
        );
        let half = divide(Exp.int(1), Exp.int(2));
        let contextual_source = plus(half, nested_cos);
        let contextual_target = plus(half, cos_4x);
        let contextual_step =
          Web.ProofTrace.prover_step(
            ~origin=Web.ProofTrace.Normalization,
            ~rule_id="arith.simplify_scalar_products",
            ~before_full_exp=contextual_source,
            ~after_full_exp=contextual_target,
            ~before_exp=contextual_source,
            ~after_exp=contextual_target,
            ~detail="normalize an argument beneath a rational context",
          );
        check(
          string,
          "argument congruence precedes rational field closure",
          "repeat progress hazel_function_argument_algebra; try unfold Rsqr; field.",
          Web.CoqProofExport.recorded_transition_replay_script(
            ~domain=Web.CoqExport.Reals,
            [contextual_step],
          ),
        );
        let distribution_source =
          times(half, divide(plus(Exp.int(1), cos_4x), Exp.int(2)));
        let distribution_target =
          plus(
            divide(times(half, cos_4x), Exp.int(2)),
            divide(times(half, Exp.int(1)), Exp.int(2)),
          );
        let distribution_step =
          Web.ProofTrace.prover_step(
            ~origin=Web.ProofTrace.ManualRewrite,
            ~rule_id="alg.distribute_mul_add",
            ~before_full_exp=distribution_source,
            ~after_full_exp=distribution_target,
            ~before_exp=distribution_source,
            ~after_exp=distribution_target,
            ~detail="distribute a rational scalar",
          );
        check(
          string,
          "rational distribution uses an exact field certificate",
          "try unfold Rsqr; field.",
          Web.CoqProofExport.recorded_transition_replay_script(
            ~domain=Web.CoqExport.Reals,
            [distribution_step],
          ),
        );
      },
    ),
    test_case(
      "real export normalizes arguments beneath opaque functions within profile boundaries",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          plus(
            divide(Exp.int(1), Exp.int(2)),
            app("f", times(Exp.int(3), times(Exp.int(2), x))),
          );
        let target =
          plus(
            divide(Exp.int(1), Exp.int(2)),
            app("f", times(Exp.int(6), x)),
          );
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_function_argument_scalar_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected opaque-function scalar export")
          };
        write_text_file(
          "/tmp/hazel_stepper_function_argument_scalar.v",
          export,
        );
        check(
          bool,
          "export quantifies the opaque function",
          true,
          string_contains("(f : R -> R)", export),
        );
        check(
          bool,
          "export proves application congruence before field closure",
          true,
          string_contains(
            "repeat progress hazel_function_argument_algebra; try unfold Rsqr; field.",
            export,
          ),
        );
        let profile = Axioms.math_profile(Trigonometry);
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 2,
            max_states: 40,
            source,
            target,
          };
        check(
          bool,
          "enabled scalar capability authorizes the opaque argument rewrite",
          true,
          local_profile_trace(~profile, ~settings, ~env, request)
          |> Option.is_some,
        );
        let disabled_profile =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.simplify_scalar_products",
          )
          |> (
            profile =>
              Axioms.profile_with_capability_disabled(
                profile,
                "arith.affine_normalize",
              )
          )
          |> Web.ProfileBoard.profile_with_cleanup(~cleanup=[]);
        check(
          bool,
          "disabled scalar operation and cleanup reject the same argument rewrite",
          false,
          local_profile_trace(
            ~profile=disabled_profile,
            ~settings,
            ~env,
            request,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "stepper coq export dumps three-term distribution proof",
      `Quick,
      () => {
        let source =
          times(
            Exp.var("x"),
            plus(plus(Exp.var("a"), Exp.var("b")), Exp.var("c")),
          );
        let target =
          plus(
            plus(
              times(Exp.var("x"), Exp.var("a")),
              times(Exp.var("x"), Exp.var("b")),
            ),
            times(Exp.var("x"), Exp.var("c")),
          );
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_single_algebra_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected three-term distribution export")
          };
        write_text_file(
          "/tmp/hazel_stepper_algebra_distribution_three_terms.v",
          export,
        );
        check(
          bool,
          "exports distribution rule",
          true,
          string_contains("alg.distribute_mul_add", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps AC-cleaned distribution proof",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = times(x, plus(plus(Exp.int(1), Exp.int(2)), x));
        let target =
          plus(
            plus(times(Exp.int(1), x), times(Exp.int(2), x)),
            times(x, x),
          );
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_single_algebra_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected AC-cleaned distribution export")
          };
        write_text_file(
          "/tmp/hazel_stepper_algebra_distribution_ac_cleanup.v",
          export,
        );
        check(
          bool,
          "exports distribution rule",
          true,
          string_contains("alg.distribute_mul_add", export),
        );
        check(
          bool,
          "exports AC-cleaned whole target",
          true,
          string_contains(
            "whole: (x * ((1 + 2) + x)) -> (((1 * x) + (2 * x)) + (x * x))",
            export,
          ),
        );
      },
    ),
    test_case(
      "stepper coq export dumps real-domain distribution proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_real_distribution_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected real-domain distribution export")
          };
        write_text_file(
          "/tmp/hazel_stepper_algebra_real_distribution.v",
          export,
        );
        check(
          bool,
          "exports distribution rule",
          true,
          string_contains("alg.distribute_mul_add", export),
        );
        check(
          bool,
          "uses real prelude",
          true,
          string_contains("Open Scope R_scope", export),
        );
      },
    ),
    test_case(
      "stepper coq export proves real distribution followed by cleanup",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_real_distribution_with_cleanup_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected real-domain distribution cleanup export")
          };
        write_text_file(
          "/tmp/hazel_stepper_algebra_real_distribution_cleanup.v",
          export,
        );
        check(
          bool,
          "exports auto simplify cleanup",
          true,
          string_contains("Hazel auto simplify step", export),
        );
        check(
          bool,
          "uses real arithmetic cleanup",
          true,
          string_contains("hazel_arithmetic", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps algebra factoring proof",
      `Quick,
      () => {
        let source =
          plus(
            times(Exp.var("y"), Exp.var("x")),
            times(Exp.var("z"), Exp.var("x")),
          );
        let target =
          times(Exp.var("x"), plus(Exp.var("y"), Exp.var("z")));
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_single_algebra_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected algebra factoring export")
          };
        write_text_file("/tmp/hazel_stepper_algebra_factor_common.v", export);
        check(
          bool,
          "exports factoring rule",
          true,
          string_contains("alg.factor_common", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps algebra cancellation proof",
      `Quick,
      () => {
        let source =
          minus(plus(Exp.var("x"), Exp.var("y")), Exp.var("y"));
        let target = Exp.var("x");
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_single_algebra_export_chain(~source, ~target),
            )
          ) {
          | Some(export) => export
          | None => fail("expected algebra cancellation export")
          };
        write_text_file("/tmp/hazel_stepper_algebra_cancel_common.v", export);
        check(
          bool,
          "exports cancellation rule",
          true,
          string_contains("alg.cancel_common_add", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps axiom search proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_axiom_search_distribution_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected axiom search export")
          };
        write_text_file(
          "/tmp/hazel_stepper_axiom_search_distribution.v",
          export,
        );
        check(
          bool,
          "exports search label",
          true,
          string_contains("Hazel written step: bounded axiom search", export),
        );
        check(
          bool,
          "exports distribution rule",
          true,
          string_contains("alg.distribute_mul_add", export),
        );
        check(
          bool,
          "exports no ring tactic",
          false,
          string_contains("ring.", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps axiom search addition reorder proof",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_axiom_search_add_reorder_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected axiom search addition reorder export")
          };
        write_text_file(
          "/tmp/hazel_stepper_axiom_search_add_reorder.v",
          export,
        );
        check(
          bool,
          "exports search label",
          true,
          string_contains("Hazel written step: bounded axiom search", export),
        );
        check(
          bool,
          "exports addition reorder rule",
          true,
          string_contains("arith.reorder_add_terms", export),
        );
        check(
          bool,
          "exports no ring tactic",
          false,
          string_contains("ring.", export),
        );
        check(
          bool,
          "prints without ERROR",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps trig proof over reals",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_trig_sin_sum_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected trig export")
          };
        write_text_file("/tmp/hazel_stepper_trig_sin_sum.v", export);
        check(
          bool,
          "imports narrow trig prelude",
          true,
          string_contains(
            "Require Import Rbase Rfunctions Rtrigo1 Cos_plus",
            export,
          ),
        );
        check(
          bool,
          "does not import Reals umbrella",
          false,
          string_contains("Require Import Reals", export),
        );
        check(
          bool,
          "quantifies real variables",
          true,
          string_contains("forall x y : R", export)
          || string_contains("forall y x : R", export),
        );
        check(
          bool,
          "exports sine sum breadcrumb",
          true,
          string_contains("trig.sin_sum", export),
        );
        check(
          bool,
          "prints sin without ERROR",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps trig double-angle proof over reals",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_trig_sin_double_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected trig double-angle export")
          };
        write_text_file("/tmp/hazel_stepper_trig_sin_double.v", export);
        check(
          bool,
          "imports narrow trig prelude",
          true,
          string_contains(
            "Require Import Rbase Rfunctions Rtrigo1 Cos_plus",
            export,
          ),
        );
        check(
          bool,
          "exports sine double-angle breadcrumb",
          true,
          string_contains("trig.sin_double", export),
        );
        check(
          bool,
          "exports multiplication reorder breadcrumb",
          true,
          string_contains("arith.reorder_mul_factors", export),
        );
        check(
          bool,
          "uses Coq sine double-angle lemma",
          true,
          string_contains("sin_2a", export),
        );
        check(
          bool,
          "prints without ERROR",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps trig power split proof over reals",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_trig_power_split_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected trig power split export")
          };
        write_text_file("/tmp/hazel_stepper_trig_power_split.v", export);
        check(
          bool,
          "exports power split breadcrumb",
          true,
          string_contains("alg.power_add", export),
        );
        check(
          bool,
          "exports multiplication association breadcrumb",
          true,
          string_contains("arith.mul_assoc", export),
        );
        check(
          bool,
          "prints without ERROR",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps trig nested power proof over reals",
      `Quick,
      () => {
        let export =
          switch (
            Web.StepperBase.Stepper.export_coq(
              sample_trig_power_nested_export_chain(),
            )
          ) {
          | Some(export) => export
          | None => fail("expected trig nested power split export")
          };
        write_text_file("/tmp/hazel_stepper_trig_power_nested.v", export);
        check(
          bool,
          "exports nested power split breadcrumb",
          true,
          string_contains("alg.power_mul", export),
        );
        check(
          bool,
          "prints without ERROR",
          false,
          string_contains("ERROR", export),
        );
      },
    ),
    test_case(
      "stepper coq export dumps core trig identity fixtures over reals",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let two = Exp.int(2);
        let sin_x = builtin_sin(x);
        let cos_x = builtin_cos(x);
        let sin_y = builtin_sin(y);
        let cos_y = builtin_cos(y);
        let tan_x = tan(x);
        let sin_x2 = power(sin_x, two);
        let cos_x2 = power(cos_x, two);
        let pi = Exp.var("pi");
        let pi_over_two = divide(pi, two);
        [
          (
            "pythagorean",
            "trig.pythagorean_sin_cos",
            plus(sin_x2, cos_x2),
            Exp.int(1),
            "sin2_cos2",
            "/tmp/hazel_stepper_trig_pythagorean.v",
          ),
          (
            "pythagorean swapped",
            "trig.pythagorean_cos_sin",
            plus(cos_x2, sin_x2),
            Exp.int(1),
            "sin2_cos2",
            "/tmp/hazel_stepper_trig_pythagorean_swapped.v",
          ),
          (
            "cos squared",
            "trig.cos_squared_pythagorean",
            cos_x2,
            minus(Exp.int(1), sin_x2),
            "cos2",
            "/tmp/hazel_stepper_trig_cos_squared.v",
          ),
          (
            "sin squared",
            "trig.sin_squared_pythagorean",
            sin_x2,
            minus(Exp.int(1), cos_x2),
            "sin2",
            "/tmp/hazel_stepper_trig_sin_squared.v",
          ),
          (
            "sin difference",
            "trig.sin_diff",
            builtin_sin(minus(x, y)),
            minus(times(sin_x, cos_y), times(cos_x, sin_y)),
            "sin_minus",
            "/tmp/hazel_stepper_trig_sin_diff.v",
          ),
          (
            "cos sum",
            "trig.cos_sum",
            builtin_cos(plus(x, y)),
            minus(times(cos_x, cos_y), times(sin_x, sin_y)),
            "cos_plus",
            "/tmp/hazel_stepper_trig_cos_sum.v",
          ),
          (
            "cos difference",
            "trig.cos_diff",
            builtin_cos(minus(x, y)),
            plus(times(cos_x, cos_y), times(sin_x, sin_y)),
            "cos_minus",
            "/tmp/hazel_stepper_trig_cos_diff.v",
          ),
          (
            "cos double square",
            "trig.cos_double_square",
            builtin_cos(times(two, x)),
            minus(cos_x2, sin_x2),
            "cos_2a",
            "/tmp/hazel_stepper_trig_cos_double_square.v",
          ),
          (
            "cos double cos",
            "trig.cos_double_cos",
            builtin_cos(times(two, x)),
            minus(times(two, cos_x2), Exp.int(1)),
            "cos_2a_cos",
            "/tmp/hazel_stepper_trig_cos_double_cos.v",
          ),
          (
            "cos double sin",
            "trig.cos_double_sin",
            builtin_cos(times(two, x)),
            minus(Exp.int(1), times(two, sin_x2)),
            "cos_2a_sin",
            "/tmp/hazel_stepper_trig_cos_double_sin.v",
          ),
          (
            "sin double sum square",
            "trig.sin_double_sum_square",
            builtin_sin(times(two, x)),
            minus(power(plus(sin_x, cos_x), two), Exp.int(1)),
            "hazel_sin_double_sum_square",
            "/tmp/hazel_stepper_trig_sin_double_sum_square.v",
          ),
          (
            "sin double sum square reverse",
            "trig.sin_double_sum_square",
            minus(power(plus(sin_x, cos_x), two), Exp.int(1)),
            builtin_sin(times(two, x)),
            "hazel_sin_double_sum_square",
            "/tmp/hazel_stepper_trig_sin_double_sum_square_reverse.v",
          ),
          (
            "sin squared double",
            "trig.sin_squared_double",
            sin_x2,
            divide(minus(Exp.int(1), builtin_cos(times(two, x))), two),
            "hazel_sin_squared_double",
            "/tmp/hazel_stepper_trig_sin_squared_double.v",
          ),
          (
            "sin squared double reverse",
            "trig.sin_squared_double",
            divide(minus(Exp.int(1), builtin_cos(times(two, x))), two),
            sin_x2,
            "hazel_sin_squared_double",
            "/tmp/hazel_stepper_trig_sin_squared_double_reverse.v",
          ),
          (
            "cos squared double",
            "trig.cos_squared_double",
            cos_x2,
            divide(plus(Exp.int(1), builtin_cos(times(two, x))), two),
            "hazel_cos_squared_double",
            "/tmp/hazel_stepper_trig_cos_squared_double.v",
          ),
          (
            "cos squared double reverse",
            "trig.cos_squared_double",
            divide(plus(Exp.int(1), builtin_cos(times(two, x))), two),
            cos_x2,
            "hazel_cos_squared_double",
            "/tmp/hazel_stepper_trig_cos_squared_double_reverse.v",
          ),
          (
            "sin half squared",
            "trig.sin_half_squared",
            power(builtin_sin(divide(x, two)), two),
            divide(minus(Exp.int(1), builtin_cos(x)), two),
            "hazel_sin_half_squared",
            "/tmp/hazel_stepper_trig_sin_half_squared.v",
          ),
          (
            "cos half squared",
            "trig.cos_half_squared",
            power(builtin_cos(divide(x, two)), two),
            divide(plus(Exp.int(1), builtin_cos(x)), two),
            "hazel_cos_half_squared",
            "/tmp/hazel_stepper_trig_cos_half_squared.v",
          ),
          (
            "sin cofunction",
            "trig.sin_cofunction",
            builtin_sin(minus(pi_over_two, x)),
            cos_x,
            "sin_shift",
            "/tmp/hazel_stepper_trig_sin_cofunction.v",
          ),
          (
            "cos cofunction",
            "trig.cos_cofunction",
            builtin_cos(minus(pi_over_two, x)),
            sin_x,
            "cos_shift",
            "/tmp/hazel_stepper_trig_cos_cofunction.v",
          ),
          (
            "sin reflection",
            "trig.sin_pi_sub",
            builtin_sin(minus(pi, x)),
            sin_x,
            "sin_PI_x",
            "/tmp/hazel_stepper_trig_sin_pi_sub.v",
          ),
          (
            "cos reflection",
            "trig.cos_pi_sub",
            builtin_cos(minus(pi, x)),
            negate(cos_x),
            "hazel_cos_pi_sub",
            "/tmp/hazel_stepper_trig_cos_pi_sub.v",
          ),
          (
            "sin negative",
            "trig.sin_neg",
            builtin_sin(negate(x)),
            negate(sin_x),
            "sin_neg",
            "/tmp/hazel_stepper_trig_sin_neg.v",
          ),
          (
            "cos negative",
            "trig.cos_neg",
            builtin_cos(negate(x)),
            cos_x,
            "cos_neg",
            "/tmp/hazel_stepper_trig_cos_neg.v",
          ),
          (
            "tan negative",
            "trig.tan_neg",
            tan(negate(x)),
            negate(tan_x),
            "tan_neg",
            "/tmp/hazel_stepper_trig_tan_neg.v",
          ),
        ]
        |> List.iter(((name, rule_id, source, target, lemma_name, path)) => {
             let export =
               switch (
                 Web.StepperBase.Stepper.export_coq(
                   sample_trig_rule_export_chain(~rule_id, ~source, ~target),
                 )
               ) {
               | Some(export) => export
               | None => fail("expected trig fixture export for " ++ name)
               };
             write_text_file(path, export);
             check(
               bool,
               name ++ " exports rule id",
               true,
               string_contains(rule_id, export),
             );
             check(
               bool,
               name ++ " exports Coq lemma",
               true,
               string_contains(lemma_name, export),
             );
             check(
               bool,
               name ++ " does not print Hazel power syntax",
               false,
               string_contains("**", export),
             );
             check(
               bool,
               name ++ " prints without ERROR",
               false,
               string_contains("ERROR", export),
             );
           });
      },
    ),
    test_case(
      "semantic stage plans gate Check Result normalizers by prerequisites",
      `Quick,
      () => {
        let profile = Axioms.math_profile(Arithmetic);
        let manual = Axioms.stage_plan_for_profile(profile, Manual);
        let check_result =
          Axioms.stage_plan_for_profile(profile, MultiStepCheck);
        check(
          bool,
          "manual stage has no Check Result normalizer",
          false,
          manual.atoms
          |> List.exists((atom: Axioms.semantic_proof_atom) =>
               atom.kind == Axioms.CheckNormalizerAtom
             ),
        );
        check(
          bool,
          "Check Result stage contains affine semantic operation",
          true,
          check_result.atoms
          |> List.exists((atom: Axioms.semantic_proof_atom) =>
               atom.id == "arith.affine_normalize"
               && atom.kind == Axioms.CheckNormalizerAtom
             ),
        );
        let without_comm =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.AddComm),
            profile,
          );
        check(
          bool,
          "missing cleanup prerequisite disables affine operation",
          false,
          Axioms.normalization_rule_id_enabled_for_profile(
            without_comm,
            MultiStepCheck,
            "arith.affine_normalize",
          ),
        );
        let without_operation =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.affine_normalize",
          );
        check(
          bool,
          "explicit operation toggle disables affine operation",
          false,
          Axioms.normalization_rule_id_enabled_for_profile(
            without_operation,
            MultiStepCheck,
            "arith.affine_normalize",
          ),
        );
      },
    ),
    test_case(
      "profile-bound affine Check Result has positive negative and disabled cases",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Arithmetic);
        let accepts = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some;
        check(
          bool,
          "collects adjacent variable terms",
          true,
          accepts(profile, plus(x, x), times(Exp.int(2), x)),
        );
        check(
          bool,
          "folds a structurally different affine constant sum",
          true,
          accepts(
            profile,
            plus(plus(x, Exp.int(1)), Exp.int(2)),
            plus(x, Exp.int(3)),
          ),
        );
        check(
          bool,
          "rejects inequivalent affine expressions",
          false,
          accepts(profile, plus(x, Exp.int(1)), plus(x, Exp.int(2))),
        );
        let without_collect =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.CollectLikeTerms
                 ),
            profile,
          );
        check(
          bool,
          "disabled prerequisite rejects formerly valid result",
          false,
          accepts(without_collect, plus(x, x), times(Exp.int(2), x)),
        );
      },
    ),
    test_case(
      "core profile checks preserve Int and browser-Real parity", `Quick, () => {
      exact_numeric_syntaxes
      |> List.iter((syntax: exact_numeric_syntax) => {
           let {label, number, plus, minus, times, power} = syntax;
           let x = Exp.var("x");
           let y = Exp.var("y");
           let one_step = (profile, source, target) =>
             Web.RewriteChecker.check_single_step_result_for_profile(
               ~profile,
               ~settings,
               ~env,
               source,
               target,
             )
             |> Option.is_some;
           let check_result = (profile, source, target) =>
             Web.RewriteChecker.check_written_step_trace_for_profile(
               ~profile,
               ~settings,
               ~env,
               source,
               target,
             )
             |> Option.is_some;
           let arithmetic = Axioms.math_profile(Arithmetic);
           check(
             bool,
             label ++ " One Step folds an exact constant",
             true,
             one_step(arithmetic, plus(number(1), number(2)), number(3)),
           );
           check(
             bool,
             label ++ " Check Result normalizes an affine sum",
             true,
             check_result(
               arithmetic,
               plus(plus(x, number(1)), number(2)),
               plus(x, number(3)),
             ),
           );
           let without_constant_folding =
             Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.const_fold",
               arithmetic,
             )
             |> Web.ProfileBoard.profile_with_cleanup(
                  ~cleanup=
                    arithmetic.step_policy.default_cleanup
                    |> List.filter(capability =>
                         capability != Axioms.ConstFold
                       ),
                );
           check(
             bool,
             label ++ " disabled constant folding stays disabled",
             false,
             one_step(
               without_constant_folding,
               plus(plus(x, number(1)), number(2)),
               plus(x, number(3)),
             ),
           );
           let algebra = Axioms.math_profile(Algebra);
           let distributed_source = times(x, plus(y, number(2)));
           let distributed_target = plus(times(x, y), times(x, number(2)));
           check(
             bool,
             label ++ " One Step distributes multiplication",
             true,
             one_step(algebra, distributed_source, distributed_target),
           );
           check(
             bool,
             label ++ " One Step factors a common scalar",
             true,
             one_step(algebra, distributed_target, distributed_source),
           );
           let foil_source =
             times(
               minus(times(number(2), x), number(3)),
               plus(x, number(4)),
             );
           let uncollected_foil =
             minus(
               plus(
                 minus(
                   times(times(number(2), x), x),
                   times(number(3), x),
                 ),
                 times(times(number(4), number(2)), x),
               ),
               number(12),
             );
           check(
             bool,
             label ++ " One Step accepts signed uncollected FOIL",
             true,
             one_step(algebra, foil_source, uncollected_foil),
           );
           let collected_foil =
             minus(
               plus(
                 times(number(2), power(x, number(2))),
                 times(number(5), x),
               ),
               number(12),
             );
           check(
             bool,
             label ++ " Check Result accepts collected signed FOIL",
             true,
             check_result(algebra, foil_source, collected_foil),
           );
           let wrong_foil =
             minus(
               plus(
                 minus(
                   times(times(number(2), x), x),
                   times(number(3), x),
                 ),
                 times(times(number(5), number(2)), x),
               ),
               number(12),
             );
           check(
             bool,
             label ++ " rejects an inequivalent FOIL coefficient",
             false,
             one_step(algebra, foil_source, wrong_foil),
           );
           let without_distribution =
             Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_mul_add",
               algebra,
             );
           check(
             bool,
             label ++ " disabled distribution blocks FOIL",
             false,
             one_step(without_distribution, foil_source, uncollected_foil),
           );
         })
    }),
    test_case(
      "exact Real polynomial normalization is general and profile-bound",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Algebra);
        let accepts = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some;
        let first_source =
          real_times(real_plus(x, real(1)), real_plus(x, real(2)));
        let first_target =
          real_plus(
            real_plus(real_power(x, real(2)), real_times(real(3), x)),
            real(2),
          );
        check(
          bool,
          "expands a Real binomial product",
          true,
          accepts(profile, first_source, first_target),
        );
        let second_source =
          real_times(
            real_plus(real_times(real(2), x), real(3)),
            real_plus(x, real(4)),
          );
        let second_target =
          real_plus(
            real_plus(
              real_times(real(2), real_power(x, real(2))),
              real_times(real(11), x),
            ),
            real(12),
          );
        check(
          bool,
          "expands a structurally different Real polynomial",
          true,
          accepts(profile, second_source, second_target),
        );
        check(
          bool,
          "rejects an inequivalent Real polynomial",
          false,
          accepts(
            profile,
            first_source,
            real_plus(
              real_plus(real_power(x, real(2)), real_times(real(4), x)),
              real(2),
            ),
          ),
        );
        let without_expansion =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.expand_polynomial",
            profile,
          )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="alg.distribute_mul_add",
             );
        check(
          bool,
          "disabled expansion routes reject the same Real transformation",
          false,
          accepts(without_expansion, first_source, first_target),
        );
      },
    ),
    test_case(
      "rational scalar and sign normalization is general and profile-bound",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let sin_2x = builtin_sin(times(Exp.int(2), x));
        let scaled_negative =
          divide(
            times(times(negate(sin_2x), Exp.int(2)), Exp.int(2)),
            Exp.int(4),
          );
        let source = minus(Exp.int(0), scaled_negative);
        let profile = Axioms.math_profile(Calculus);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        check(
          bool,
          "rational affine model recognizes the scalar equality",
          true,
          Web.RewriteChecker.rational_affine_equivalent_with_constant_reordering(
            source,
            sin_2x,
          ),
        );
        let scalar_summary =
          switch (trace(profile, source, sin_2x)) {
          | Some(summary) => summary
          | None => fail("expected profile-bound rational scalar trace")
          };
        check(
          bool,
          "cancels a rational scalar and two negatives around an opaque atom",
          true,
          scalar_summary.rule_ids == ["arith.affine_normalize"],
        );
        let replay_request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source,
            target: sin_2x,
          };
        let replay =
          Web.ProofSearchBackend.rocq_replay_program(
            replay_request,
            scalar_summary,
          );
        check(
          bool,
          "Rocq uses the exact affine certificate without recursive search",
          true,
          string_contains("lra", replay)
          && !string_contains("hazel_rewrite_search", replay),
        );
        check_exp_equal(
          "suggestion normalizer reaches the same exact target",
          sin_2x,
          Web.TrigRewrite.simplify_scalar_products(source),
        );
        check_exp_equal(
          "reduces a structurally different rational coefficient",
          divide(times(Exp.int(3), y), Exp.int(2)),
          Web.TrigRewrite.simplify_scalar_products(
            divide(times(Exp.int(6), y), Exp.int(4)),
          ),
        );
        check_exp_equal(
          "normalizes subtraction of a negative without a special atom",
          plus(x, y),
          Web.TrigRewrite.simplify_scalar_products(minus(x, negate(y))),
        );
        let a = Exp.var("a");
        let b = Exp.var("b");
        check(
          bool,
          "affine model recognizes subtraction of a negative",
          true,
          Web.RewriteChecker.rational_affine_equivalent_with_constant_reordering(
            minus(a, negate(b)),
            plus(a, b),
          ),
        );
        check(
          bool,
          "profile affine authorizer recognizes subtraction of a negative",
          true,
          Web.RewriteChecker.rational_affine_trace_for_profile(
            ~profile,
            minus(a, negate(b)),
            plus(a, b),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "no earlier cleanup shadows the affine route",
          false,
          Web.RewriteChecker.direct_cleanup_trace_for_profile(
            ~profile,
            minus(a, negate(b)),
            plus(a, b),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "no calculus route shadows the affine route",
          false,
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            minus(a, negate(b)),
            plus(a, b),
          )
          |> Option.is_some,
        );
        check(
          bool,
          "the focused Manual scalar operation recognizes the same target",
          true,
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings,
            ~env,
            minus(a, negate(b)),
            plus(a, b),
          )
          |> Option.is_some,
        );
        let subtraction_summary =
          switch (trace(profile, minus(a, negate(b)), plus(a, b))) {
          | Some(summary) => summary
          | None => fail("expected general subtraction-of-negative trace")
          };
        check(
          bool,
          "subtraction of a negative uses the exact affine operation",
          true,
          subtraction_summary.rule_ids == ["arith.affine_normalize"],
        );
        let t = Exp.var("t");
        let f_t = app("f", t);
        let function_summary =
          switch (trace(profile, negate(negate(f_t)), f_t)) {
          | Some(summary) => summary
          | None =>
            fail("expected double-negative function application trace")
          };
        let function_request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Calculus,
            max_depth: 4,
            max_states: 80,
            source: negate(negate(f_t)),
            target: f_t,
          };
        let function_replay =
          Web.ProofSearchBackend.rocq_replay_program(
            function_request,
            function_summary,
          );
        check(
          bool,
          "Rocq quantifies an opaque unary real function and its argument",
          true,
          string_contains("(f : R -> R)", function_replay)
          && string_contains("(t : R)", function_replay),
        );
        check(
          bool,
          "rejects a wrong scalar result",
          false,
          trace(profile, source, times(Exp.int(2), sin_2x))
          |> Option.is_some,
        );
        let without_const_folding =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.ConstFold),
            profile,
          );
        check(
          bool,
          "disabled affine prerequisite rejects scalar normalization",
          false,
          trace(without_const_folding, source, sin_2x) |> Option.is_some,
        );
        let without_affine_operation =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.affine_normalize",
          );
        check(
          bool,
          "disabled affine operation rejects subtraction of a negative",
          false,
          trace(without_affine_operation, minus(a, negate(b)), plus(a, b))
          |> Option.is_some,
        );
        check(
          bool,
          "disabled affine operation rejects double-negative function cleanup",
          false,
          trace(without_affine_operation, negate(negate(f_t)), f_t)
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "division distribution handles the trig power-reduction shape",
      `Quick,
      () => {
        let x = Exp.var("x");
        let two = Exp.int(2);
        let cos_2x = builtin_cos(times(two, x));
        let source = divide(minus(Exp.int(1), cos_2x), two);
        let target = minus(divide(Exp.int(1), two), divide(cos_2x, two));
        let profile = Axioms.math_profile(Calculus);
        let trace =
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        check(
          bool,
          "calculus inherits exact division distribution",
          true,
          switch (trace) {
          | Some(summary) =>
            List.mem("alg.distribute_div_add", summary.rule_ids)
          | None => false
          },
        );
        let without_distribution =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.distribute_div_add",
            profile,
          );
        check(
          bool,
          "disabled division distribution rejects the same target",
          false,
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile=without_distribution,
            ~settings,
            ~env,
            source,
            target,
          )
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "profile-bound calculus Check Result records cleanup steps",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            source,
            target,
          );
        let variable = trace(profile, diff(x, x), Exp.int(1));
        let constant = trace(profile, diff(Exp.int(7), x), Exp.int(0));
        check(bool, "variable derivative", true, Option.is_some(variable));
        check(bool, "constant derivative", true, Option.is_some(constant));
        check(
          bool,
          "visible variable rule is recorded explicitly",
          true,
          switch (variable) {
          | Some(summary) =>
            List.mem("calc.diff_variable", summary.rule_ids)
            && summary.prover_steps
            |> List.exists((step: Web.ProofTrace.prover_step) =>
                 step.rule_id == "calc.diff_variable"
               )
          | None => false
          },
        );
        check(
          bool,
          "rejects incorrect derivative",
          false,
          trace(profile, diff(x, x), Exp.int(2)) |> Option.is_some,
        );
        let without_basics =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability =>
                   capability != Axioms.DerivativeBasics
                 ),
            profile,
          );
        check(
          bool,
          "disabled automatic cleanup preserves visible basic derivative",
          true,
          trace(without_basics, diff(x, x), Exp.int(1)) |> Option.is_some,
        );
        let without_variable =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_variable",
            without_basics,
          );
        check(
          bool,
          "disabled visible variable rule blocks basic derivative",
          false,
          trace(without_variable, diff(x, x), Exp.int(1)) |> Option.is_some,
        );
      },
    ),
    test_case(
      "exact Real derivatives preserve profile boundaries",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let cubic = real_power(x, real(3));
        let cubic_result = real_times(real(3), real_power(x, real(2)));
        check(
          bool,
          "certifies a Real power derivative",
          true,
          trace(profile, diff(cubic, x), cubic_result) |> Option.is_some,
        );
        let sum_source = diff(real_plus(cubic, real_times(real(2), x)), x);
        let sum_target =
          real_plus(diff(cubic, x), diff(real_times(real(2), x), x));
        check(
          bool,
          "certifies Real derivative linearity without cleanup",
          true,
          trace(profile, sum_source, sum_target) |> Option.is_some,
        );
        check(
          bool,
          "rejects changing a power beneath deriv",
          false,
          trace(profile, diff(cubic, x), diff(real_power(x, real(2)), x))
          |> Option.is_some,
        );
        let without_power =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_power",
            profile,
          );
        check(
          bool,
          "disabled power rule blocks the Real derivative",
          false,
          trace(without_power, diff(cubic, x), cubic_result)
          |> Option.is_some,
        );
      },
    ),
    test_case(
      "new derivative operators use profile-bound calculus search",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            source,
            target,
          );
        let square_source = expression_derivative(power(x, Exp.int(2)), x);
        let square_target = times(Exp.int(2), x);
        check(
          bool,
          "expression operator reaches the power-rule result",
          true,
          trace(profile, square_source, square_target) |> Option.is_some,
        );
        let cubic =
          Exp.fn(Pat.var("x"), power(x, Exp.int(3)), None, Some("f"));
        let cubic_derivative =
          Exp.fn(
            Pat.var("x"),
            times(Exp.int(3), power(x, Exp.int(2))),
            None,
            Some("f"),
          );
        check(
          bool,
          "function operator reaches a structurally different derivative",
          true,
          trace(profile, function_derivative(cubic), cubic_derivative)
          |> Option.is_some,
        );
        check(
          bool,
          "an incorrect derivative remains rejected",
          false,
          trace(profile, square_source, times(Exp.int(3), x))
          |> Option.is_some,
        );
        let without_power =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_power",
            profile,
          );
        check(
          bool,
          "disabling the power rule blocks both new operator forms",
          true,
          trace(without_power, square_source, square_target)
          |> Option.is_none
          && trace(
               without_power,
               function_derivative(cubic),
               cubic_derivative,
             )
          |> Option.is_none,
        );
      },
    ),
    test_case(
      "calculus composes nested product differentiation with profile arithmetic",
      `Quick,
      () => {
        let x = Exp.var("x");
        let profile = Axioms.math_profile(Calculus);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile,
            source,
            target,
          );
        let nested_left =
          diff(times(Exp.int(3), times(Exp.int(2), x)), x);
        let nested_right =
          diff(times(times(x, Exp.int(4)), Exp.int(3)), x);
        let left_trace = trace(profile, nested_left, Exp.int(6));
        let right_trace = trace(profile, nested_right, Exp.int(12));
        check(
          bool,
          "nested constants on the left are certified",
          true,
          switch (left_trace) {
          | Some(summary) =>
            List.mem("calc.diff_product", summary.rule_ids)
            && List.mem("arith.affine_normalize", summary.rule_ids)
          | None => false
          },
        );
        check(
          bool,
          "a different nesting and constant orientation is certified",
          true,
          Option.is_some(right_trace),
        );
        check(
          bool,
          "an incorrect nested-product derivative is rejected",
          false,
          trace(profile, nested_left, Exp.int(7)) |> Option.is_some,
        );
        let without_product =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_product",
            profile,
          );
        check(
          bool,
          "disabled product rule blocks the derivative",
          false,
          trace(without_product, nested_left, Exp.int(6)) |> Option.is_some,
        );
        let without_constant_folding =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.ConstFold),
            profile,
          );
        check(
          bool,
          "disabled affine prerequisite blocks scalar completion",
          false,
          trace(without_constant_folding, nested_left, Exp.int(6))
          |> Option.is_some,
        );
        let export = (source, target, path) => {
          let coq =
            Web.ProofSearchBackend.calculus_export_program_for_profile(
              ~profile,
              source,
              target,
            )
            |> Option.value(~default="");
          write_text_file(path, coq);
          check(
            bool,
            "nested product has a profile-directed Rocq certificate",
            true,
            string_contains(
              "Hazel profile-directed derivative certificate",
              coq,
            ),
          );
        };
        export(
          nested_left,
          Exp.int(6),
          "/tmp/hazel_stepper_rocq_derivative_nested_left_product.v",
        );
        export(
          nested_right,
          Exp.int(12),
          "/tmp/hazel_stepper_rocq_derivative_nested_right_product.v",
        );
      },
    ),
    test_case(
      "direct cancellation is independent from affine normalization",
      `Quick,
      () => {
        let x = Exp.var("x");
        let y = Exp.var("y");
        let profile = Axioms.math_profile(Algebra);
        let trace = (profile, source, target) =>
          Web.RewriteChecker.check_written_step_trace_for_profile(
            ~profile,
            ~settings,
            ~env,
            source,
            target,
          );
        let without_affine =
          Axioms.profile_with_capability_disabled(
            profile,
            "arith.affine_normalize",
          );
        let without_cancellation =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.cancel_common_add",
            profile,
          );
        let without_either =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="alg.cancel_common_add",
            without_affine,
          );
        let source = minus(plus(x, y), y);
        check(
          bool,
          "enabled cancellation supplies an independent route",
          true,
          switch (trace(without_affine, source, x)) {
          | Some(summary) =>
            List.mem("alg.cancel_common_add", summary.rule_ids)
          | None => false
          },
        );
        check(
          bool,
          "direct cancellation is preferred when both routes are enabled",
          true,
          switch (trace(profile, source, x)) {
          | Some(summary) =>
            List.mem("alg.cancel_common_add", summary.rule_ids)
          | None => false
          },
        );
        check(
          bool,
          "affine normalization remains an alternate authorized route",
          true,
          switch (trace(without_cancellation, source, x)) {
          | Some(summary) => summary.rule_ids == ["arith.affine_normalize"]
          | None => false
          },
        );
        check(
          bool,
          "disabling both routes rejects the step",
          false,
          trace(without_either, source, x) |> Option.is_some,
        );
        check(
          bool,
          "a structurally different cancellation uses the same direct rule",
          true,
          switch (
            trace(
              without_affine,
              minus(plus(times(Exp.int(2), x), y), y),
              times(Exp.int(2), x),
            )
          ) {
          | Some(summary) =>
            List.mem("alg.cancel_common_add", summary.rule_ids)
          | None => false
          },
        );
        check(
          bool,
          "direct cancellation rejects an inequivalent target",
          false,
          trace(without_affine, source, y) |> Option.is_some,
        );
        let replay = source => {
          let summary =
            switch (trace(without_affine, source, x)) {
            | Some(summary) => summary
            | None => fail("expected direct cancellation trace")
            };
          let request =
            Web.ProofSearchBackend.{
              backend: JSCoqTacticSearch,
              level: Algebra,
              max_depth: 4,
              max_states: 80,
              source,
              target: x,
            };
          Web.ProofSearchBackend.rocq_replay_program(request, summary);
        };
        let right_replay = replay(source);
        let left_replay = replay(plus(y, minus(x, y)));
        [right_replay, left_replay]
        |> List.iter(program => {
             check(
               bool,
               "Rocq replay uses a deterministic cancellation certificate",
               true,
               string_contains("unfold Rminus; lra", program),
             )
           });
      },
    ),
    test_case(
      "Rocq replay program certifies the stored Hazel trace",
      `Quick,
      () => {
        let x = Exp.var("x");
        let request =
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Arithmetic,
            max_depth: 4,
            max_states: 80,
            source: plus(x, x),
            target: times(Exp.int(2), x),
          };
        let summary = require_written_trace(request.source, request.target);
        let coq =
          Web.ProofSearchBackend.rocq_replay_program(request, summary);
        write_text_file("/tmp/hazel_stepper_exact_profile_replay.v", coq);
        check(
          bool,
          "replay names the exact-trace contract",
          true,
          string_contains("Exact replay of the Hazel profile trace", coq),
        );
        check(
          bool,
          "single-transition replay avoids redundant assertions",
          true,
          !string_contains("assert (H_hazel_step_", coq)
          && string_contains("ring", coq),
        );
      },
    ),
    test_case(
      "Print exact Reals for Algebrite",
      `Quick,
      () => {
        check(string, "pi", "pi", print_for_algebrite(Exp.real(Real.Pi)));
        check(
          string,
          "rational",
          "(1/3)",
          print_for_algebrite(
            Exp.real(Real.normalize(Bigint.one, Bigint.of_int(3), None)),
          ),
        );
        check(
          string,
          "negative integer",
          "-2",
          print_for_algebrite(Exp.real(Real.of_bigint(Bigint.of_int(-2)))),
        );
      },
    ),
    test_case(
      "Print recognized functions for Algebrite",
      `Quick,
      () => {
        check(
          string,
          "elaborated built-in function",
          "sin(pi)",
          print_for_algebrite(
            Exp.ap(Forward, Exp.builtin_fun("sin"), Exp.real(Real.Pi)),
          ),
        );
        check(
          string,
          "exact rational argument",
          "cos((1/3))",
          print_for_algebrite(
            Exp.ap(
              Forward,
              Exp.builtin_fun("cos"),
              Exp.real(Real.normalize(Bigint.one, Bigint.of_int(3), None)),
            ),
          ),
        );
        check(
          string,
          "shadowed function is not treated as a built-in",
          "unknown",
          print_for_algebrite(
            Exp.ap(Forward, Exp.var("sin"), Exp.real(Real.Pi)),
          ),
        );
      },
    ),
    test_case(
      "Print exact Real operators for Algebrite",
      `Quick,
      () => {
        check(
          string,
          "power",
          "(2 ^ 3)",
          print_for_algebrite(
            Exp.bin_op(
              Operators.Real(Power),
              Exp.real(Real.of_bigint(Bigint.of_int(2))),
              Exp.real(Real.of_bigint(Bigint.of_int(3))),
            ),
          ),
        );
        check(
          string,
          "negation",
          "(-2)",
          print_for_algebrite(
            Exp.un_op(
              Operators.Real(Minus),
              Exp.real(Real.of_bigint(Bigint.of_int(2))),
            ),
          ),
        );
      },
    ),
  ],
);
