open Alcotest;
open Util;

let tests = (
  "Calc",
  [
    test_case(
      "set treats the same object as unchanged whatever eq says",
      `Quick,
      () => {
        let x = [1, 2, 3];
        let never = (_, _) => false;
        check(
          bool,
          "set(~eq=never, x, Calculated(x)) is old",
          false,
          Calc.is_new(Calc.set(~eq=never, x, Calc.Calculated(x))),
        );
        check(
          bool,
          "set(~eq=never, x, Calculated(copy of x)) is new",
          true,
          Calc.is_new(
            Calc.set(~eq=never, x, Calc.Calculated(List.map(Fun.id, x))),
          ),
        );
      },
    ),
  ],
);
