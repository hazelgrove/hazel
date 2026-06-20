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
  check(bool, name, true, Language.Exp.fast_equal(expected, actual));

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
          ["arithmetic", "algebra", "trigonometry"],
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
        check(string, "printed expression", "sin (PI - x)", printed);
        check(
          bool,
          "does not emit ERROR",
          false,
          printed |> String.contains(_, 'E'),
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
          "does not import Ring",
          false,
          string_contains("Require Import Ring", export),
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
          string_contains("Require Import Rbase Rtrigo1 Cos_plus", export),
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
  ],
);
