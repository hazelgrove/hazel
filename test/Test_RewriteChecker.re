open Alcotest;

open Language;
open IdTagged.FreshGrammar;
open Util;

let settings = CoreSettings.on;
let env = Environment.empty;

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

let sin = arg => app("sin", arg);
let cos = arg => app("cos", arg);
let tan = arg => app("tan", arg);
let builtin_sin = arg => builtin_app("sin", arg);
let builtin_cos = arg => builtin_app("cos", arg);

let float = value => Exp.float(value);

let float_minus = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Minus), left, right);

let float_divide = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Divide), left, right);

let float_times = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Times), left, right);

let float_power = (left, right) =>
  Exp.bin_op(Operators.Float(Operators.Power), left, right);

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

let prover_step_rule_id = (step: Web.RewriteChecker.prover_step) =>
  step.rule_id;

let prover_step_origin = (step: Web.RewriteChecker.prover_step) =>
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

let visible_rule_summary_mode =
    (summary: Web.ProfileBoard.visible_rule_summary) =>
  summary.mode_label;

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
    (
      name,
      rule_id,
      before_exp,
      after_exp,
      step: Web.RewriteChecker.prover_step,
    ) => {
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
      Web.RewriteChecker.check_written_step_trace_at_level(
        ~level=Axioms.Arithmetic,
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
          justification: Web.RewriteChecker.trace_summary_label(trace),
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
        justification: Web.RewriteChecker.trace_summary_label(trace),
        trace_summary: Some(trace),
        next_exp: saved(target),
      }),
    ~next_step=Some(final_step),
  );
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
    Web.ProofSearchBackend.collapsed_macro_summary(
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
    Web.ProofSearchBackend.collapsed_macro_summary(
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

let sample_local_algebra_under_trig_export_chain = () => {
  let x = Exp.var("x");
  let local_source = times(Exp.int(2), times(Exp.int(2), x));
  let local_target = times(Exp.int(4), x);
  let trace =
    Web.ProofSearchBackend.collapsed_macro_summary(
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
    Web.ProofSearchBackend.collapsed_macro_summary(
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
    Web.ProofSearchBackend.collapsed_macro_summary(
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
      Web.RewriteChecker.check_written_step_trace_at_level(
        ~level=Axioms.Arithmetic,
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
      Web.RewriteChecker.check_written_step_trace_at_level(
        ~level=Axioms.Arithmetic,
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
      Web.RewriteChecker.check_written_step_trace_at_level(
        ~level=Axioms.Arithmetic,
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
      Web.RewriteChecker.check_written_step_trace_at_level(
        ~level=Axioms.Arithmetic,
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
    Web.RewriteChecker.{
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
    Web.RewriteChecker.{
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
    Web.RewriteChecker.{
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
        justification: Web.RewriteChecker.trace_summary_label(trace),
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
      "rewrite levels are cumulative",
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
          "future levels include earlier groups",
          ["arithmetic", "algebra", "trigonometry", "calculus"],
          Axioms.allowed_groups(Calculus) |> List.map(rewrite_group_name),
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
      "calculus one step applies enabled cleanup immediately",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source = diff(plus(power(x, Exp.int(2)), Exp.int(2)), x);
        let target = diff(power(x, Exp.int(2)), x);
        check_written_at_level(
          "sum rule drops constant derivative and additive zero",
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
          && string_contains("derivable_pt_lim_comp", chain_coq),
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
          arithmetic_profile.one_step_policy.allow_polynomial_expansion,
        );
        switch (
          Axioms.visible_rule_policy_for_rule(
            arithmetic_profile.step_policy,
            "arith.mul_const",
          )
        ) {
        | Some(rule_policy) =>
          check(
            string,
            "arithmetic distribution step mode",
            "once",
            Axioms.visible_step_mode_label(rule_policy.mode),
          );
          check(
            list(string),
            "arithmetic distribution cleanup",
            ["add.assoc", "mul.assoc"],
            rule_policy.allowed_cleanup |> List.map(cleanup_capability_label),
          );
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
          algebra_profile.one_step_policy.allow_polynomial_expansion,
        );
        switch (
          Axioms.visible_rule_policy_for_rule(
            algebra_profile.step_policy,
            "alg.distribute_mul_add",
          )
        ) {
        | Some(rule_policy) =>
          check(
            string,
            "algebra distribution step mode",
            "once",
            Axioms.visible_step_mode_label(rule_policy.mode),
          );
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
          );
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
          ["try_once", "try_once", "try_once", "finish_only"],
          algebra_profile.rocq_tactic_plan.steps
          |> List.map(rocq_tactic_step_mode),
        );
        check(
          list(string),
          "algebra primitive tactic step modes",
          ["try_once", "once"],
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
          ["rewrite Z.mul_1_l", "rewrite Z.mul_1_r"],
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
            ["Algebra", "Trigonometry", "Calculus"],
            rule.visible_levels |> List.map(Axioms.rewrite_level_label),
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
            "arith.mul_identity",
            "alg.distribute_mul_add",
            "alg.factor_common",
            "alg.cancel_common_add",
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
        | [_mul_identity, distribution, ..._] =>
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
      "stage plan rejects unknown profile rule ids",
      `Quick,
      () => {
        let base = Axioms.math_profile(Algebra);
        let step_policy: Axioms.step_policy = {
          ...base.step_policy,
          visible_rules: [
            Axioms.visible_once_rule(
              ~rule_id="unknown.rule",
              ~allowed_cleanup=[],
            ),
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
          [true, false, true, false, true, false, true],
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
            "arith.mul_identity",
            "alg.distribute_mul_add",
            "alg.factor_common",
            "alg.cancel_common_add",
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
        | [_mul_identity, distribution, _factor, _cancel, ..._] =>
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
            string,
            "distribution step behavior",
            "Counts as one step",
            visible_rule_summary_mode(distribution),
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
            "Equivalent, outside profile",
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
          "matching cancellation restores Ready",
          true,
          state(cancelled_result) == (false, Ready, None),
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
          |> Option.map((trace: Web.RewriteChecker.trace_summary) =>
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
          )
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
          string_contains("hazel_arithmetic", arithmetic_coq),
        );
        check(
          bool,
          "arithmetic macro is labeled",
          true,
          Web.ProofSearchBackend.collapsed_macro_summary(arithmetic_request)
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
          Web.ProofSearchBackend.collapsed_macro_summary(algebra_request)
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
            "intros.\nfirst [hazel_integer_polynomial | reflexivity]",
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
               ~rule_id="alg.factor_common",
             )
          |> Web.ProfileBoard.profile_without_visible_rule(
               ~rule_id="arith.mul_identity",
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
            "Ltac hazel_profile_visible_step :=\n  first [\n    rewrite Z.add_simpl_r\n  | rewrite Z.add_simpl_l\n  | rewrite Z.sub_simpl_r\n  | rewrite Z.sub_add\n  ].\n\nLtac hazel_profile_normalization_step",
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
          Web.ProofSearchBackend.collapsed_macro_summary_for_purpose(
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
          Web.ProofSearchBackend.collapsed_macro_summary_for_purpose(
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
          "auto summary records auto-simplify plan",
          true,
          string_contains("hazel_algebra_plan_auto_simplify", auto_detail),
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
          "integer goal omits real-only trig normalization",
          false,
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
          Web.ProofSearchBackend.collapsed_macro_summary(
            algebra_in_trig_request,
          )
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
        let cos_2x = builtin_cos(times(Exp.int(2), x));
        let rational_square_request =
          Web.ProofSearchBackend.{
            backend: Web.ProofSearchBackend.JSCoqTacticSearch,
            level: Trigonometry,
            max_depth: 4,
            max_states: 80,
            source: times(Exp.int(2), power(quotient_square, Exp.int(2))),
            target:
              plus(
                minus(divide(Exp.int(1), Exp.int(2)), cos_2x),
                times(
                  divide(Exp.int(1), Exp.int(2)),
                  power(cos_2x, Exp.int(2)),
                ),
              ),
          };
        let rational_square_coq =
          Web.ProofSearchBackend.rocq_search_program(rational_square_request);
        write_text_file(
          "/tmp/hazel_stepper_rocq_rational_square_normalize.v",
          rational_square_coq,
        );
        check(
          bool,
          "trig Check Result includes bounded rational-square normalization",
          true,
          string_contains(
            "solve [hazel_rational_square_normalize]",
            rational_square_coq,
          ),
        );
        check(
          bool,
          "rational-square proof avoids Rocq 9-only division lemmas",
          false,
          string_contains("Rdiv_mult_distr", rational_square_coq)
          || string_contains("Rmult_div_assoc", rational_square_coq)
          || string_contains("Rmult_div_r", rational_square_coq),
        );
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
        };
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
            |> List.exists((step: Web.RewriteChecker.prover_step) =>
                 step.rule_id == "trig.sin_sum"
               ),
          );
          check(
            bool,
            "trace includes multiplication reorder",
            true,
            result.steps
            |> List.exists((step: Web.RewriteChecker.prover_step) =>
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
            |> List.exists((step: Web.RewriteChecker.prover_step) =>
                 step.rule_id == "trig.sin_double"
               ),
          );
          check(
            bool,
            "trace includes multiplication reorder",
            true,
            result.steps
            |> List.exists((step: Web.RewriteChecker.prover_step) =>
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
            |> List.exists((step: Web.RewriteChecker.prover_step) =>
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
          "arithmetic",
          Web.RewriteChecker.trace_summary_label(summary),
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
          "trace has commutativity",
          true,
          has_rule_id("arith.add_comm", summary.rule_ids),
        );
        check(
          bool,
          "trace has collection",
          true,
          has_rule_id("arith.collect_like_terms", summary.rule_ids),
        );
        check(
          bool,
          "from trace keeps side rules",
          true,
          has_rule_id("arith.add_comm", summary.from_rule_ids),
        );
        check(
          bool,
          "to trace keeps side rules",
          true,
          has_rule_id("arith.add_comm", summary.to_rule_ids),
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
          "from trace has lean hint",
          true,
          from_hints
          |> List.exists(hint => prover_hint_tactic(hint) == "rw [add_comm]"),
        );
        check(
          bool,
          "to trace has lean hint",
          true,
          to_hints
          |> List.exists(hint => prover_hint_tactic(hint) == "rw [add_comm]"),
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
          [
            "arith.add_assoc",
            "arith.add_comm",
            "arith.const_fold",
            "arith.collect_like_terms",
            "arith.mul_const",
          ],
          summary.prover_steps |> List.map(prover_step_rule_id),
        );
        check(
          list(string),
          "all normalizer steps",
          [
            "normalization",
            "normalization",
            "normalization",
            "normalization",
            "normalization",
          ],
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
        let reordered =
          plus(
            plus(plus(Exp.var("x"), Exp.var("x")), Exp.int(1)),
            Exp.int(2),
          );
        let folded = plus(plus(Exp.var("x"), Exp.var("x")), Exp.int(3));
        let collected = plus(times(Exp.int(2), Exp.var("x")), Exp.int(3));
        let summary = require_written_trace(source, collected);
        switch (summary.prover_steps) {
        | [assoc, comm, fold, collect, mul_const] =>
          check_prover_step(
            "assoc",
            "arith.add_assoc",
            source,
            source,
            assoc,
          );
          check_prover_step(
            "commute",
            "arith.add_comm",
            source,
            reordered,
            comm,
          );
          check_prover_step(
            "constant fold",
            "arith.const_fold",
            reordered,
            folded,
            fold,
          );
          check_prover_step(
            "collect",
            "arith.collect_like_terms",
            folded,
            collected,
            collect,
          );
          check_prover_step(
            "constant multiple",
            "arith.mul_const",
            collected,
            collected,
            mul_const,
          );
        | _ => fail("expected five arithmetic normalization prover steps")
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
          "imports Ring for integer polynomial fallback",
          true,
          string_contains("Require Import ZArith Lia Ring", export),
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
          string_contains("Hazel prover step 1: arith.add_assoc", export),
        );
        check(
          bool,
          "exports folded constant detail",
          true,
          string_contains("detail: fold integer constants", export),
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
          "exports named prover assertion",
          true,
          string_contains("assert (H_hazel_step_1", export),
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
          string_contains("Hazel written step: arithmetic", export),
        );
        check(
          bool,
          "exports named prover assertion",
          true,
          string_contains("assert (H_hazel_step_3", export),
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
          string_contains("Hazel written step: arithmetic", export),
        );
        check(
          bool,
          "exports named prover assertion",
          true,
          string_contains("assert (H_hazel_step_4", export),
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
          "exports named prover assertion",
          true,
          string_contains("assert (H_hazel_step_4", export),
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
      "stepper coq export proves integer trinomial square macro",
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
          "exports integer polynomial helper",
          true,
          string_contains("Ltac hazel_integer_polynomial", export),
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
          "exports temporary affine fallback",
          true,
          string_contains("Temporary affine fallback", export),
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
            "whole: (x*((1+2)+x)) -> (((1*x)+(2*x))+(x*x))",
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
  ],
);
