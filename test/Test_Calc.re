open Alcotest;

module Calc = Util.Calc;

/* `Calc` is the incrementality primitive every `docs/ui-architecture.md`
 * component is built on: `Calc.t` carries "did this input change", `Calc.saved`
 * holds a previously computed result in the model, and `let.calc` decides
 * whether to recompute. Every `Update.calculate` in the app leans on it, so a
 * regression here is a regression everywhere -- either recomputing statics and
 * evaluation on every frame, or serving stale results forever.
 *
 * The contract worth pinning is not what these functions return but WHETHER THE
 * WORK RUNS, so the tests below count calls rather than only inspecting values. */

/* A function that records how many times it was applied. */
let counting = () => {
  let calls = ref(0);
  let f = x => {
    incr(calls);
    x * 10;
  };
  (calls, f);
};

let tag = (x: Calc.t(int)) => (Calc.is_new(x), Calc.get_value(x));
let tagged = pair(bool, int);

let update_tests = [
  /* Nothing saved yet: the work has to run whatever the input says. */
  test_case(
    "update runs when nothing is saved",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out = Calc.update(Calc.OldValue(1), f, Calc.Pending);
      check(int, "calls", 1, calls^);
      check(tagged, "result", (true, 10), tag(out));
    },
  ),
  test_case(
    "update runs when the input is new",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out = Calc.update(Calc.NewValue(1), f, Calc.Calculated(99));
      check(int, "calls", 1, calls^);
      check(tagged, "result", (true, 10), tag(out));
    },
  ),
  /* The whole point: an unchanged input with a saved result must not recompute,
     and must report itself as old so downstream steps skip too. */
  test_case(
    "update skips when the input is old and a result is saved",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out = Calc.update(Calc.OldValue(1), f, Calc.Calculated(99));
      check(int, "calls", 0, calls^);
      check(
        tagged,
        "the saved value is returned, marked old",
        (false, 99),
        tag(out),
      );
    },
  ),
  /* The form components actually write: `saved |> { let.calc x = input; ... }`. */
  test_case(
    "let.calc is update",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out =
        Calc.Calculated(99)
        |> {
          open Calc.Syntax;
          let.calc x = Calc.OldValue(1);
          f(x);
        };
      check(int, "calls when old", 0, calls^);
      check(tagged, "result when old", (false, 99), tag(out));
      let (calls, f) = counting();
      let out =
        Calc.Calculated(99)
        |> {
          open Calc.Syntax;
          let.calc x = Calc.NewValue(2);
          f(x);
        };
      check(int, "calls when new", 1, calls^);
      check(tagged, "result when new", (true, 20), tag(out));
    },
  ),
];

let update'_tests = [
  test_case(
    "update' passes the old result through without running",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out = Calc.update'(Calc.OldValue(1), f, Calc.OldValue(99));
      check(int, "calls", 0, calls^);
      check(tagged, "result", (false, 99), tag(out));
    },
  ),
  test_case(
    "update' runs when the input is new",
    `Quick,
    () => {
      let (calls, f) = counting();
      let out = Calc.update'(Calc.NewValue(1), f, Calc.OldValue(99));
      check(int, "calls", 1, calls^);
      check(tagged, "result", (true, 10), tag(out));
    },
  ),
];

let combine_tests = [
  /* Combining is how a step with several inputs decides to run: it must stay
     old only when EVERY input is old. */
  test_case(
    "combine is old only when both are old",
    `Quick,
    () => {
      let old = Calc.OldValue(1);
      let nw = Calc.NewValue(2);
      check(bool, "old + old", false, Calc.is_new(Calc.combine(old, old)));
      check(bool, "old + new", true, Calc.is_new(Calc.combine(old, nw)));
      check(bool, "new + old", true, Calc.is_new(Calc.combine(nw, old)));
      check(bool, "new + new", true, Calc.is_new(Calc.combine(nw, nw)));
    },
  ),
  test_case(
    "combine_list is old only when all are old",
    `Quick,
    () => {
      let old = Calc.OldValue(1);
      check(
        bool,
        "all old",
        false,
        Calc.is_new(Calc.combine_list([old, old, old])),
      );
      check(
        bool,
        "one new",
        true,
        Calc.is_new(Calc.combine_list([old, Calc.NewValue(2), old])),
      );
      check(bool, "empty", false, Calc.is_new(Calc.combine_list([])));
    },
  ),
  /* combine_list folds over a reversed list, so order is easy to get wrong and
     silently reorders whatever the caller was zipping together. */
  test_case("combine_list preserves order", `Quick, () =>
    check(
      list(int),
      "values",
      [1, 2, 3],
      Calc.get_value(
        Calc.combine_list([
          Calc.OldValue(1),
          Calc.NewValue(2),
          Calc.OldValue(3),
        ]),
      ),
    )
  ),
];

let set_tests = [
  /* `set` is how a component asks "did my input change since last frame". */
  test_case("set is new against nothing saved", `Quick, () =>
    check(tagged, "result", (true, 1), tag(Calc.set(1, Calc.Pending)))
  ),
  test_case("set is old when equal to the saved value", `Quick, () =>
    check(
      tagged,
      "result",
      (false, 1),
      tag(Calc.set(1, Calc.Calculated(1))),
    )
  ),
  test_case("set is new when different from the saved value", `Quick, () =>
    check(
      tagged,
      "result",
      (true, 2),
      tag(Calc.set(2, Calc.Calculated(1))),
    )
  ),
  /* Callers pass ~eq for structural comparison (EvalResult uses
     Exp.fast_equal); the default is physical equality. */
  test_case(
    "set honours a custom eq",
    `Quick,
    () => {
      let eq = (a, b) => a mod 10 == b mod 10;
      check(
        tagged,
        "equal under eq",
        (false, 11),
        tag(Calc.set(~eq, 11, Calc.Calculated(21))),
      );
      check(
        tagged,
        "unequal under eq",
        (true, 12),
        tag(Calc.set(~eq, 12, Calc.Calculated(21))),
      );
    },
  ),
];

let saved_tests = [
  test_case("save then read round-trips", `Quick, () =>
    check(
      option(int),
      "value",
      Some(5),
      Calc.get_saved_opt(Calc.save(Calc.NewValue(5))),
    )
  ),
  test_case("get_saved falls back on Pending", `Quick, () =>
    check(int, "default", 7, Calc.get_saved(7, Calc.Pending))
  ),
  test_case("get_saved_exc raises on Pending", `Quick, () =>
    check_raises("PendingValue", Calc.PendingValue, () =>
      ignore(Calc.get_saved_exc(Calc.Pending: Calc.saved(int)))
    )
  ),
  /* Grouped inputs must stay Pending until every part exists, or a component
     reads a default and caches it as though it were real. */
  test_case(
    "saved_pair is pending if either side is",
    `Quick,
    () => {
      check(
        bool,
        "left pending",
        true,
        Calc.saved_pair((Calc.Pending, Calc.Calculated(1))) == Calc.Pending,
      );
      check(
        bool,
        "right pending",
        true,
        Calc.saved_pair((Calc.Calculated(1), Calc.Pending)) == Calc.Pending,
      );
      check(
        option(pair(int, int)),
        "both present",
        Some((1, 2)),
        Calc.get_saved_opt(
          Calc.saved_pair((Calc.Calculated(1), Calc.Calculated(2))),
        ),
      );
    },
  ),
];

let misc_tests = [
  /* map_if_new marks its result OLD even though it just recomputed. That reads
     like a bug and is not: it stops a one-shot fixup from re-triggering
     downstream work every frame. Pinned so nobody "fixes" it. */
  test_case(
    "map_if_new marks the mapped result old",
    `Quick,
    () => {
      check(
        tagged,
        "new input",
        (false, 10),
        tag(Calc.map_if_new(x => x * 10, Calc.NewValue(1))),
      );
      check(
        tagged,
        "old input",
        (false, 1),
        tag(Calc.map_if_new(x => x * 10, Calc.OldValue(1))),
      );
    },
  ),
  test_case(
    "old_if_same demotes an equal new value",
    `Quick,
    () => {
      check(
        tagged,
        "equal",
        (false, 1),
        tag(Calc.old_if_same(1, Calc.NewValue(1))),
      );
      check(
        tagged,
        "unequal",
        (true, 1),
        tag(Calc.old_if_same(1, Calc.NewValue(2))),
      );
      check(
        tagged,
        "already old",
        (true, 1),
        tag(Calc.old_if_same(1, Calc.OldValue(1))),
      );
    },
  ),
  test_case(
    "to_option keeps the freshness of a present value",
    `Quick,
    () => {
      check(
        bool,
        "new Some",
        true,
        switch (Calc.to_option(Calc.NewValue(Some(1)))) {
        | Some(x) => Calc.is_new(x)
        | None => false
        },
      );
      check(
        bool,
        "old Some",
        false,
        switch (Calc.to_option(Calc.OldValue(Some(1)))) {
        | Some(x) => Calc.is_new(x)
        | None => true
        },
      );
      check(
        bool,
        "None collapses",
        true,
        Calc.to_option(Calc.NewValue(None: option(int))) == None,
      );
    },
  ),
  test_case(
    "to_pair distributes freshness",
    `Quick,
    () => {
      let (a, b) = Calc.to_pair(Calc.NewValue((1, 2)));
      check(bool, "left new", true, Calc.is_new(a));
      check(bool, "right new", true, Calc.is_new(b));
      let (a, b) = Calc.to_pair(Calc.OldValue((1, 2)));
      check(bool, "left old", false, Calc.is_new(a));
      check(bool, "right old", false, Calc.is_new(b));
    },
  ),
];

let tests = (
  "Calc",
  update_tests
  @ update'_tests
  @ combine_tests
  @ set_tests
  @ saved_tests
  @ misc_tests,
);
