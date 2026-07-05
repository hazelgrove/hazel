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
          "trig-mode algebra request uses algebra tactic",
          true,
          string_contains("\nhazel_algebra.\nQed.", algebra_in_trig_coq),
        );
        check(
          bool,
          "trig-mode algebra request does not run trig tactic",
          false,
          string_contains("\nhazel_trigonometry.\nQed.", algebra_in_trig_coq),
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
          "trig identity request uses trig tactic",
          true,
          string_contains("\nhazel_trigonometry.\nQed.", trig_identity_coq),
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
          "replays local algebra equality as assertion",
          true,
          string_contains(
            "assert (H_hazel_step_1 : (2 * (2 * x)) = (4 * x)).",
            export,
          ),
        );
        check(
          bool,
          "uses algebra to prove local equality",
          true,
          string_contains("{ hazel_algebra. }", export),
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
