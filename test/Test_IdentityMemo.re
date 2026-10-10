/* Util.IdentityMemo: a pure function's answers remembered by the identity
   of its argument. MvuShape.close_value and ProbeUtil.abbreviated_seg_of
   use it, so a redraw that shows the same cached values reuses their
   closed and printed forms. */
open Alcotest;

let tests = (
  "IdentityMemo",
  [
    test_case(
      "the same object is answered once, from memory after",
      `Quick,
      () => {
        let calls = ref(0);
        let f =
          Util.IdentityMemo.memo((xs: list(int)) => {
            incr(calls);
            List.map(x => x * 2, xs);
          });
        let a = [1, 2, 3];
        let r1 = f(a);
        let r2 = f(a);
        check(int, "one call", 1, calls^);
        check(bool, "the same answer object", true, r1 === r2);
        check(list(int), "the answer", [2, 4, 6], r1);
      },
    ),
    test_case(
      "an equal but different object is a new question",
      `Quick,
      () => {
        let calls = ref(0);
        let f =
          Util.IdentityMemo.memo((xs: list(int)) => {
            incr(calls);
            List.length(xs);
          });
        ignore(f([1, 2]));
        ignore(f([1, 2]));
        check(int, "two calls", 2, calls^);
      },
    ),
    test_case(
      "numbers and strings are not remembered: the function runs",
      `Quick,
      () => {
        let calls = ref(0);
        let f =
          Util.IdentityMemo.memo((n: int) => {
            incr(calls);
            n + 1;
          });
        check(int, "3 + 1", 4, f(3));
        check(int, "again", 4, f(3));
        check(int, "two calls", 2, calls^);
      },
    ),
    test_case(
      "close_value answers the same closed value again",
      `Quick,
      () => {
        let d = Language.DHExp.fresh(Atom(Int(Util.Bigint.of_int(1))));
        check(
          bool,
          "same object back",
          true,
          Haz3lcore.MvuShape.close_value(d)
          === Haz3lcore.MvuShape.close_value(d),
        );
      },
    ),
  ],
);
