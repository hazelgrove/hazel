open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/**
 * Tests for probe sample call_stack behavior.
 *
 * The call_stack of a probe sample represents the active function calls
 * at the time the probed expression was evaluated. This affects how samples
 * are filtered and displayed in the UI's "single" mode.
 *
 * Key behaviors:
 * - Top-level expressions should have empty call_stacks
 * - Expressions inside a function body should have the function's app_id in call_stack
 * - A probed function application should have the same call_stack as before entering the function
 */

/* Helper to get all samples from evaluated code with probes */
let get_all_samples = (code: string): list(Sample.t) => {
  let (_term, elaborated, _info_map, targets) = parse_with_probes(code);
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets),
      ~env=Builtins.env_init,
      elaborated,
    );
  let probes = EvaluatorState.get_probes(state);
  Id.Map.bindings(probes) |> List.concat_map(snd);
};

/* Show call stack for debugging */
let show_call_stack = (cs: CallStack.t): string =>
  "["
  ++ String.concat(
       ", ",
       List.map((f: CallStack.frame) => Id.str3(f.id), cs),
     )
  ++ "]";

let call_stack_testable =
  testable(Fmt.using(show_call_stack, Fmt.string), CallStack.equal);

/* Test that multiple top-level probed applications have the same (empty) call_stack.
 * This is the bug: if they have different call_stacks (containing their own app_ids),
 * then clicking on one hides the other in "single" mode. */
let top_level_apps_tests = [
  test_case(
    "Top-level probed applications should have same call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let f = fun x -> x + 1
in ^^probe(f(1)); ^^probe(f(2))|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          call_stack_testable,
          "Both applications should have same call_stack",
          s1.call_stack,
          s2.call_stack,
        )
      | _ =>
        fail(
          "Expected exactly 2 samples, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Top-level probed application should have empty call_stack",
    `Quick,
    () => {
      let samples = get_all_samples({|let f = fun x -> x in ^^probe(f(42))|});
      switch (samples) {
      | [s] =>
        check(
          call_stack_testable,
          "Top-level app should have empty call_stack",
          [],
          s.call_stack,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Multiple top-level apps should all have empty call_stacks",
    `Quick,
    () => {
      /* This is the temperature conversion example from the bug report */
      let samples =
        get_all_samples(
          {|let celsius = fun farenheit ->
  (farenheit -. 32.) *. 5. /. 9.
in ^^probe(celsius(72.)); ^^probe(celsius(100.))|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          call_stack_testable,
          "First app should have empty call_stack",
          [],
          s1.call_stack,
        );
        check(
          call_stack_testable,
          "Second app should have empty call_stack",
          [],
          s2.call_stack,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* Test that probes inside function bodies correctly have the app_id in call_stack */
let inside_function_tests = [
  test_case(
    "Probe inside function body has app_id in call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples({|let f = fun x -> ^^probe(x + 1)
in f(5)|});
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Inner probe should have non-empty call_stack",
          true,
          List.length(s.call_stack) > 0,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Different calls to same function have different call_stacks",
    `Quick,
    () => {
      let samples =
        get_all_samples({|let f = fun x -> ^^probe(x)
in f(1); f(2)|});
      switch (samples) {
      | [s1, s2] =>
        check(
          bool,
          "Both probes should have non-empty call_stacks",
          true,
          List.length(s1.call_stack) > 0 && List.length(s2.call_stack) > 0,
        );
        /* They should have DIFFERENT call_stacks (different app_ids) */
        check(
          bool,
          "Different calls should have different call_stacks",
          true,
          s1.call_stack != s2.call_stack,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* Test the relationship between probed app and probe inside function body */
let app_vs_body_tests = [
  test_case(
    "Probed app and probe inside function have correct relationship",
    `Quick,
    () => {
      /* The probe on the app should have empty call_stack (top-level).
       * The probe inside the function should have the app_id in call_stack.
       * The inner probe's call_stack should be the app's call_stack + app_id. */
      let samples =
        get_all_samples({|let f = fun x -> ^^probe(x)
in ^^probe(f(5))|});
      switch (samples) {
      | [s1, s2] =>
        /* One sample should be from the app probe (empty call_stack),
         * one from inside the function (non-empty call_stack) */
        let (app_sample, inner_sample) =
          if (List.length(s1.call_stack) == 0) {
            (s1, s2);
          } else {
            (s2, s1);
          };
        check(
          call_stack_testable,
          "App probe should have empty call_stack",
          [],
          app_sample.call_stack,
        );
        check(
          bool,
          "Inner probe should have one app_id in call_stack",
          true,
          List.length(inner_sample.call_stack) == 1,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* Test that functions defined in modules have correct call_stack behavior */
let module_function_tests = [
  test_case(
    "Function from module has app_id in call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let m = { let f = fun x -> ^^probe(x + 1) }
in m.f(5)|},
        );
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Probe inside module function should have non-empty call_stack",
          true,
          List.length(s.call_stack) > 0,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Module keyword function has app_id in call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|module m = { let f = fun x -> ^^probe(x * 2) }
in m.f(3)|},
        );
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Probe inside module keyword function should have non-empty call_stack",
          true,
          List.length(s.call_stack) > 0,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Multiple calls to module function have different call_stacks",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let m = { let f = fun x -> ^^probe(x) }
in m.f(1); m.f(2)|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          bool,
          "Both should have non-empty call_stacks",
          true,
          List.length(s1.call_stack) > 0 && List.length(s2.call_stack) > 0,
        );
        check(
          bool,
          "Different calls should have different call_stacks",
          true,
          s1.call_stack != s2.call_stack,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got "
          ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* The dynamic fn_def_id carried on a call sample's frame is what drives
 * step-into for higher-order and partial-application calls (where the static
 * binding site is only a parameter). These tests pin down which calls record
 * a navigable fn_def_id. */
let frame_fn_def_id = (s: Sample.t): option(Id.t) =>
  Option.bind(s.frame, (f: CallStack.frame) => f.fn_def_id);

let single_sample = (label, code): Sample.t => {
  let samples = get_all_samples(code);
  switch (samples) {
  | [s] => s
  | _ =>
    fail(
      label
      ++ ": expected exactly 1 sample, got "
      ++ string_of_int(List.length(samples)),
    )
  };
};

let step_into_frame_tests = [
  test_case(
    "Deferred call (incl. through a type-annotated HOF / cast) resolves fn_def_id to a Fun",
    `Quick,
    () => {
      let term_kind = (info_map, id): string =>
        switch (Statics.Map.lookup(id, info_map)) {
        | None => "NOT_IN_MAP"
        | Some(Info.InfoExp({user_term: {term, _}, _})) =>
          switch (term) {
          | Fun(_) => "Fun"
          | TypFun(_) => "TypFun"
          | Parens(_) => "Parens"
          | Var(_) => "Var"
          | Ap(_) => "Ap"
          | DeferredAp(_) => "DeferredAp"
          | _ => "OtherExp"
          }
        | Some(_) => "NonExpInfo"
        };
      let check_resolves = (label, code) => {
        let (_term, elaborated, info_map, targets) = parse_with_probes(code);
        let (_, state) =
          Evaluator.evaluate(
            ~eval_info=EvalInfo.of_targets(targets),
            ~env=Builtins.env_init,
            elaborated,
          );
        let samples =
          EvaluatorState.get_probes(state)
          |> Id.Map.bindings
          |> List.concat_map(snd);
        let dump =
          List.map(
            (s: Sample.t) =>
              switch (frame_fn_def_id(s)) {
              | None => "noFnDef"
              | Some(id) => term_kind(info_map, id)
              },
            samples,
          )
          |> String.concat(",");
        let resolves = (s: Sample.t) =>
          switch (frame_fn_def_id(s)) {
          | None => false
          | Some(id) =>
            switch (Statics.Map.lookup(id, info_map)) {
            | Some(Info.InfoExp({user_term: {term: Fun(_), _}, _})) => true
            | _ => false
            }
          };
        let total = List.length(samples);
        let ok = List.length(List.filter(resolves, samples));
        check(int, label ++ ": resolved [" ++ dump ++ "]", total, ok);
      };
      /* annotated let-bound fn (like crop-plotter's setCell) via deferred */
      check_resolves(
        "annotated-deferred",
        {|let setCell: (Int, Int) -> Int = fun (g, x) -> g + x in
let updateGrove = fun (m, f) -> ^^probe(f(m)) in
updateGrove(10, setCell(_, 5))|},
      );
      /* cast from a RETURN-type annotation (a function factory) rather than a
         parameter annotation: mk(1) is a deferred cast to Int -> Int, then
         applied through a HOF. Exercises the same cast path from another site. */
      check_resolves(
        "returned-cast-deferred",
        {|let add = fun (a, b) -> a + b in
let mk: Int -> (Int -> Int) = fun a -> add(a, _) in
let apply = fun (f, x) -> ^^probe(f(x)) in
apply(mk(1), 5)|},
      );
      /* the user's shape: a deferred branch in a case, alongside a lambda
         branch, both reaching the same updateGrove probe */
      check_resolves(
        "case-mixed-deferred",
        {|type Action = +Lit + Def in
let setCell: (Int, Int) -> Int = fun (g, x) -> g + x in
let setAll: Int -> Int = fun g -> g * 2 in
let updateGrove: (Int, Int -> Int) -> Int =
  fun (m, f) -> ^^probe(f(m)) in
let update: (Int, Action) -> Int =
  fun (m, action) ->
    case action
    | Lit => updateGrove(m, fun g -> setAll(g))
    | Def => updateGrove(m, setCell(_, 5))
    end in
(update(10, Lit), update(10, Def))|},
      );
    },
  ),
  test_case(
    "HOF call with a function literal records a navigable fn_def_id",
    `Quick,
    () => {
      let s =
        single_sample(
          "hof-literal",
          {|let apply = fun (f, x) -> ^^probe(f(x))
in apply(fun n -> n + 1, 5)|},
        );
      check(
        bool,
        "call frame carries Some fn_def_id (the passed lambda)",
        true,
        Option.is_some(frame_fn_def_id(s)),
      );
    },
  ),
  test_case(
    "HOF call with a named function records a navigable fn_def_id",
    `Quick,
    () => {
      let s =
        single_sample(
          "hof-named",
          {|let inc = fun n -> n + 1 in
let apply = fun (f, x) -> ^^probe(f(x))
in apply(inc, 5)|},
        );
      check(
        bool,
        "call frame carries Some fn_def_id (inc)",
        true,
        Option.is_some(frame_fn_def_id(s)),
      );
    },
  ),
  test_case(
    "Partial-application call resolves fn_def_id to the underlying function",
    `Quick,
    () => {
      let s =
        single_sample(
          "partial-app",
          {|let add = fun (a, b) -> a + b in
let apply = fun (f, x) -> ^^probe(f(x))
in apply(add(_, 10), 5)|},
        );
      check(
        bool,
        "deferred call frame carries Some fn_def_id (add)",
        true,
        Option.is_some(frame_fn_def_id(s)),
      );
    },
  ),
  test_case(
    "Library call (map) DOES carry an fn_def_id (predicate must exclude by call site)",
    `Quick,
    () => {
      /* map/fold_left/... are Hazel Funs in env_init, so their frame carries
         a (non-navigable) fn_def_id pointing at library code. This is why the
         step-into predicate can't rely on fn_def_id alone and must suppress
         builtin call sites separately. */
      let samples =
        get_all_samples({|^^probe(map([1, 2, 3], fun n -> n + 1))|});
      switch (samples) {
      | [s, ..._] =>
        check(
          bool,
          "library call frame carries an fn_def_id",
          true,
          Option.is_some(frame_fn_def_id(s)),
        )
      | [] => fail("Expected at least 1 sample")
      };
    },
  ),
];

let tests = (
  "Evaluator.ProbeCallStack",
  List.concat([
    top_level_apps_tests,
    inside_function_tests,
    app_vs_body_tests,
    module_function_tests,
    step_into_frame_tests,
  ]),
);
