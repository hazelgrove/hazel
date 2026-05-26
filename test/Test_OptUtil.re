open Alcotest;
open Util;

let tests = (
  "OptUtil",
  [
    test_case(
      "get with Some",
      `Quick,
      () => {
        let o = Some(5);
        let fallback = () => 0;
        check(int, "Returns value", 5, OptUtil.get(fallback, o));
      },
    ),
    test_case(
      "get with None",
      `Quick,
      () => {
        let o = None;
        let fallback = () => 0;
        check(int, "Returns fallback", 0, OptUtil.get(fallback, o));
      },
    ),
    test_case(
      "get_or_fail with Some",
      `Quick,
      () => {
        let o = Some(5);
        check(int, "Returns value", 5, OptUtil.get_or_fail("fail", o));
      },
    ),
    test_case(
      "get_or_fail with None",
      `Quick,
      () => {
        let o = None;
        check_raises(
          "Raises failure",
          Failure("fail"),
          () => {
            let _ = OptUtil.get_or_fail("fail", o);
            ();
          },
        );
      },
    ),
    test_case(
      "get_or_raise with Some",
      `Quick,
      () => {
        let o = Some(5);
        check(int, "Returns value", 5, OptUtil.get_or_raise(Not_found, o));
      },
    ),
    test_case(
      "get_or_raise with None",
      `Quick,
      () => {
        let o = None;
        check_raises(
          "Raises exception",
          Not_found,
          () => {
            let _ = OptUtil.get_or_raise(Not_found, o);
            ();
          },
        );
      },
    ),
    test_case(
      "map2 with both Some",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = Some(3);
        let f = (a, b) => a + b;
        check(option(int), "Both Some", Some(5), OptUtil.map2(f, o1, o2));
      },
    ),
    test_case(
      "map2 with first None",
      `Quick,
      () => {
        let o1 = None;
        let o2 = Some(3);
        let f = (a, b) => a + b;
        check(option(int), "First None", None, OptUtil.map2(f, o1, o2));
      },
    ),
    test_case(
      "map2 with second None",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = None;
        let f = (a, b) => a + b;
        check(option(int), "Second None", None, OptUtil.map2(f, o1, o2));
      },
    ),
    test_case("some_if with true", `Quick, () => {
      check(
        option(int),
        "True condition",
        Some(5),
        OptUtil.some_if(true, 5),
      )
    }),
    test_case("some_if with false", `Quick, () => {
      check(option(int), "False condition", None, OptUtil.some_if(false, 5))
    }),
    test_case(
      "zip with both Some",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = Some(3);
        check(
          option(pair(int, int)),
          "Both Some",
          Some((2, 3)),
          OptUtil.zip(o1, o2),
        );
      },
    ),
    test_case(
      "zip with first None",
      `Quick,
      () => {
        let o1 = None;
        let o2 = Some(3);
        check(
          option(pair(int, int)),
          "First None",
          None,
          OptUtil.zip(o1, o2),
        );
      },
    ),
    test_case(
      "zip with second None",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = None;
        check(
          option(pair(int, int)),
          "Second None",
          None,
          OptUtil.zip(o1, o2),
        );
      },
    ),
    test_case(
      "unzip with Some",
      `Quick,
      () => {
        let o = Some((2, 3));
        check(
          pair(option(int), option(int)),
          "Some tuple",
          (Some(2), Some(3)),
          OptUtil.unzip(o),
        );
      },
    ),
    test_case(
      "unzip with None",
      `Quick,
      () => {
        let o = None;
        check(
          pair(option(int), option(int)),
          "None",
          (None, None),
          OptUtil.unzip(o),
        );
      },
    ),
    test_case(
      "traverse with empty list",
      `Quick,
      () => {
        let xs = [];
        let f = x => Some(x * 2);
        check(
          option(list(int)),
          "Empty list",
          Some([]),
          OptUtil.traverse(f, xs),
        );
      },
    ),
    test_case(
      "traverse with all successful transformations",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        let f = x => Some(x * 2);
        check(
          option(list(int)),
          "All succeed",
          Some([2, 4, 6]),
          OptUtil.traverse(f, xs),
        );
      },
    ),
    test_case(
      "traverse with one failure",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        let f = x => x == 2 ? None : Some(x * 2);
        check(
          option(list(int)),
          "One fails",
          None,
          OptUtil.traverse(f, xs),
        );
      },
    ),
    test_case(
      "sequence with empty list",
      `Quick,
      () => {
        let xs = [];
        check(
          option(list(int)),
          "Empty list",
          Some([]),
          OptUtil.sequence(xs),
        );
      },
    ),
    test_case(
      "sequence with all Some values",
      `Quick,
      () => {
        let xs = [Some(1), Some(2), Some(3)];
        check(
          option(list(int)),
          "All Some",
          Some([1, 2, 3]),
          OptUtil.sequence(xs),
        );
      },
    ),
    test_case(
      "sequence with one None value",
      `Quick,
      () => {
        let xs = [Some(1), None, Some(3)];
        check(option(list(int)), "One None", None, OptUtil.sequence(xs));
      },
    ),
    test_case(
      "and_then with Some",
      `Quick,
      () => {
        let o = Some(5);
        let f = x => Some(x * 2);
        check(
          option(int),
          "Some chaining",
          Some(10),
          OptUtil.and_then(f, o),
        );
      },
    ),
    test_case(
      "and_then with None",
      `Quick,
      () => {
        let o = None;
        let f = x => Some(x * 2);
        check(option(int), "None chaining", None, OptUtil.and_then(f, o));
      },
    ),
    test_case(
      "replace with Some",
      `Quick,
      () => {
        let o = 5;
        let f = x => Some(x * 2);
        check(int, "Replaces with Some", 10, OptUtil.replace(f, o));
      },
    ),
    test_case(
      "replace with None",
      `Quick,
      () => {
        let o = 5;
        let f = _ => None;
        check(int, "Keeps original with None", 5, OptUtil.replace(f, o));
      },
    ),
    test_case(
      "fold_left_opt with successful accumulation",
      `Quick,
      () => {
        let list = [1, 2, 3];
        let f = (acc, x) => Some(acc + x);
        let init = 0;
        check(
          option(int),
          "Successful fold",
          Some(6),
          OptUtil.fold_left_opt(f, list, init),
        );
      },
    ),
    test_case(
      "fold_left_opt with failure",
      `Quick,
      () => {
        let list = [1, 2, 3];
        let f = (acc, x) => x == 2 ? None : Some(acc + x);
        let init = 0;
        check(
          option(int),
          "Failed fold",
          None,
          OptUtil.fold_left_opt(f, list, init),
        );
      },
    ),
    test_case(
      "syntax let* with Some",
      `Quick,
      () => {
        let o = Some(5);
        let result = {
          open OptUtil.Syntax;
          let* x = o;
          Some(x * 2);
        };
        check(option(int), "let* with Some", Some(10), result);
      },
    ),
    test_case(
      "syntax let* with None",
      `Quick,
      () => {
        let o = None;
        let result = {
          open OptUtil.Syntax;
          let* x = o;
          Some(x * 2);
        };
        check(option(int), "let* with None", None, result);
      },
    ),
    test_case(
      "syntax let+ with Some",
      `Quick,
      () => {
        let o = Some(5);
        let result = {
          open OptUtil.Syntax;
          let+ x = o;
          x * 2;
        };
        check(option(int), "let+ with Some", Some(10), result);
      },
    ),
    test_case(
      "syntax let+ with None",
      `Quick,
      () => {
        let o = None;
        let result = {
          open OptUtil.Syntax;
          let+ x = o;
          x * 2;
        };
        check(option(int), "let+ with None", None, result);
      },
    ),
    test_case(
      "syntax and+ with both Some",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = Some(3);
        let result = OptUtil.zip(o1, o2);
        check(
          option(pair(int, int)),
          "and+ with both Some",
          Some((2, 3)),
          result,
        );
      },
    ),
    test_case(
      "syntax and+ with one None",
      `Quick,
      () => {
        let o1 = Some(2);
        let o2 = None;
        let result = OptUtil.zip(o1, o2);
        check(option(pair(int, int)), "and+ with one None", None, result);
      },
    ),
  ],
);
