open Util;

module Map = RuleSpec.Map;
type map = RuleSpec.map;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = (Drv.Exp.t, list(Drv.Exp.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type tests = list(RuleTest.test);

type failure =
  | FailSpec(RuleSpec.failure)
  // | FailMatch(specced)
  // | NotEqual(specced, specced)
  | FailTest(RuleTest.failure)
  // | FailUnbox(specced, Drv.Any.cls);
  // | FailTest(map, test)
  | Mismatch(int, int); /* expected, actual */

let failure_msg = (failure: failure): string =>
  switch (failure) {
  | Mismatch(expected, actual) =>
    Printf.sprintf("Expected %d premises, but got %d", expected, actual)
  | FailSpec(failure) => RuleSpec.failure_msg(failure)
  | FailTest(failure) => RuleTest.failure_msg(failure)
  };

type res = list(failure);

let rec fold_left2_safe: (list('a), list('b), 'c, ('a, 'b, 'c) => 'c) => 'c =
  (xs, ys, acc, f) =>
    switch (xs, ys) {
    | ([x, ...xs], [y, ...ys]) => fold_left2_safe(xs, ys, f(x, y, acc), f)
    | _ => acc
    };

let go_spec: (list(Drv.Exp.t), list(Drv.Exp.t)) => (map, res) =
  (specs, syntaxes) => {
    let (map, res) =
      fold_left2_safe(specs, syntaxes, (Map.empty, []), RuleSpec.go_exp);
    (
      map,
      List.map(
        failure => {
          // print_endline(failure |> RuleSpec.show_failure);
          FailSpec(
            failure,
          )
        },
        res,
      ),
    );
  };

let go_test: (map, res, tests) => res =
  map => {
    List.fold_left((res, test) => {
      switch (RuleTest.go(map, test)) {
      | None => res
      | Some(failure) => [FailTest(failure), ...res]
      }
    });
  };

let verify: (spec, tests, (Drv.Exp.t, list(Drv.Exp.t))) => res =
  (spec, tests, (concl, prems)) => {
    let (concl_spec, prems_spec) = spec;
    // We simply stick conclusion and premises together

    // print_endline("Actual Conclusion:");
    // print_endline(Drv.Exp.show(concl));
    // print_endline("Actual Premises:");
    // List.iter(prem => print_endline(Drv.Exp.show(prem)), prems);
    // print_endline("Spec Conclusion:");
    // print_endline(Drv.Exp.show(concl_spec));
    // print_endline("Spec Premises:");
    // List.iter(prem => print_endline(Drv.Exp.show(prem)), prems_spec);

    let (map, res) =
      go_spec([concl_spec, ...prems_spec], [concl, ...prems]);
    let (m, n) = (List.length(prems_spec), List.length(prems));
    // If premises number mismatch or there is any previous error, we don't run tests
    let res = m != n ? [Mismatch(m, n), ...res] : res;
    List.is_empty(res) ? go_test(map, res, tests) : res;
  };

// Debugging function
let __print_all_specs_and_tests = () => {
  Rule.all
  |> List.iter(rule => {
       let (concl, prems) = RuleSpec.of_spec(rule);
       let tests = RuleTest.of_tests(rule);
       List.iter(prem => print_endline("  " ++ Drv.Exp.show(prem)), prems);
       List.iter(
         test => print_endline("  {Test} " ++ RuleTest.show_test(test)),
         tests,
       );
       print_endline(
         "——————————————————————["
         ++ Rule.show(rule)
         ++ "]\n  "
         ++ Drv.Exp.show(concl)
         ++ "\n",
       );
     });
};

// Note(zhiyao): never mind

/**
  The following functions are utilized in the frontend to address the problem
  of representing a specific type of checking. For example, in the case
  of `E_Let`, the initial structure is as follows:

  Premises := [ e_def ⇓ v_def , e_body' ⇓ v' ]
  Conclusion := let x = e_def in e_body ⇓ v ]
  Tests := [ e_body' = [v_def/x]e_body ]

  To simplify definitions, we can convert the `Tests` into `Premises` by
  substituting `e_body'` with `[v_def/x]e_body`. The updated structure becomes:

  Premises := [ e_def ⇓ v_def , [v_def/x]e_body ⇓ v' ]
  Conclusion := let x = e_def in e_body ⇓ v
  Tests: []
 */;

// let spec_fill_eq_test: (RuleTest.test, Drv.Exp.t) => Drv.Exp.t =
//   fun
//   | Eq(Get(s'), op) =>
//     RuleSpec.map_reg(s => s == s' ? RuleTest.Operation.show(op) : s)
//   | _ => Fun.id;

// let spec_fill_eq_tests: (spec, tests) => spec =
//   List.fold_left(((concl, prems), test) =>
//     (
//       concl |> spec_fill_eq_test(test),
//       prems |> List.map(spec_fill_eq_test(test)),
//     )
//   );

// let tests_fill_eq_tests: tests => tests =
//   List.map(
//     fun
//     | RuleTest.Eq(Get(_), op) =>
//       RuleTest.Eq(Get(RuleTest.Operation.show(op)), op)
//     | _ as test => test,
//   );

// let fill_eq_tests: (spec, tests) => (spec, tests) =
//   (spec, tests) => (
//     spec_fill_eq_tests(spec, tests),
//     tests_fill_eq_tests(tests),
//   );

// let test_remove_eq_test: tests => tests =
//   List.filter(
//     fun
//     | RuleTest.Eq(Get(_), _) => false
//     | _ => true,
//   );
