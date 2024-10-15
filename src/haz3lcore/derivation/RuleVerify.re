open Util;

module Map = RuleSpec.Map;
type map = RuleSpec.map;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = (RuleSpec.t, list(RuleSpec.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type tests = list(RuleTest.t);

let spec_fill_eq_test: (RuleTest.t, RuleSpec.t) => RuleSpec.t =
  fun
  | Eq(Get(s'), op) =>
    RuleSpec.map_reg(s => s == s' ? RuleTest.Operation.show(op) : s)
  | _ => Fun.id;

let spec_fill_eq_tests: (spec, tests) => spec =
  List.fold_left(((concl, prems), test) =>
    (
      concl |> spec_fill_eq_test(test),
      prems |> List.map(spec_fill_eq_test(test)),
    )
  );

let tests_fill_eq_tests: tests => tests =
  List.map(
    fun
    | RuleTest.Eq(Get(_), op) =>
      RuleTest.Eq(Get(RuleTest.Operation.show(op)), op)
    | _ as test => test,
  );

let fill_eq_tests: (spec, tests) => (spec, tests) =
  (spec, tests) => (
    spec_fill_eq_tests(spec, tests),
    tests_fill_eq_tests(tests),
  );

let test_remove_eq_test: tests => tests =
  List.filter(
    fun
    | RuleTest.Eq(Get(_), _) => false
    | _ => true,
  );

type failure =
  | Mismatch(int, int) /* expected, actual */
  | FailSpec(RuleSpec.failure)
  | FailTest(map, RuleTest.t);

let failure_msg = (failure: failure): string =>
  switch (failure) {
  | Mismatch(expected, actual) =>
    Printf.sprintf("Expected %d premises, but got %d", expected, actual)
  | FailSpec(failure) => RuleSpec.failure_msg(failure)
  | FailTest(map, test) =>
    Printf.sprintf("Failed to verify %s", RuleTest.show_linked(map, test))
  };

type res = list(failure);

let rec fold_left2_safe:
  (list('a), list('b), 'c, ('c, ('a, 'b)) => 'c) => 'c =
  (xs, ys, acc, f) =>
    switch (xs, ys) {
    | ([x, ...xs], [y, ...ys]) =>
      fold_left2_safe(xs, ys, f(acc, (x, y)), f)
    | _ => acc
    };

let go_test: (map, res, tests) => res =
  map => {
    List.fold_left((res, test) => {
      RuleTest.go(map, test) ? res : [FailTest(map, test), ...res]
    });
  };

let go_spec: (list(RuleSpec.t), list(DrvSyntax.t)) => (map, res) =
  (specs, syntaxes) => {
    let (map, res) =
      fold_left2_safe(specs, syntaxes, (Map.empty, []), RuleSpec.go);
    (map, List.map(failure => FailSpec(failure), res));
  };

let verify: (spec, tests, (DrvSyntax.t, list(DrvSyntax.t))) => res =
  (spec, tests, (concl, prems)) => {
    let (concl_spec, prems_spec) = spec;
    // We simply stick conclusion and premises together
    let (map, res) =
      go_spec([concl_spec, ...prems_spec], [concl, ...prems]);
    let (m, n) = (List.length(prems_spec), List.length(prems));
    // If premises number mismatch or there is any previous error, we don't run tests
    let res = m != n ? [Mismatch(m, n), ...res] : res;
    List.is_empty(res) ? go_test(map, res, tests) : res;
  };

let __print_all_specs_and_tests = () => {
  Rule.all
  |> List.iter(rule => {
       let (concl, prems) = RuleSpec.of_spec(rule);
       let tests = RuleTest.of_tests(rule);
       List.iter(prem => print_endline("  " ++ RuleSpec.show(prem)), prems);
       List.iter(
         test => print_endline("  {Test} " ++ RuleTest.show(test)),
         tests,
       );
       print_endline(
         "——————————————————————["
         ++ Rule.show(rule)
         ++ "]",
       );
       print_endline("  " ++ RuleSpec.show(concl));
       print_endline("");
     });
};
