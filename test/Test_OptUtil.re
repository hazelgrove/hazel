open Alcotest;
open Util;

let tests = (
  "OptUtil",
  [
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
        check(
          int,
          "Returns value",
          5,
          OptUtil.get_or_raise(Stdlib.Not_found, o),
        );
      },
    ),
    test_case(
      "get_or_raise with None",
      `Quick,
      () => {
        let o = None;
        check_raises(
          "Raises exception",
          Stdlib.Not_found,
          () => {
            let _ = OptUtil.get_or_raise(Stdlib.Not_found, o);
            ();
          },
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
  ],
);
