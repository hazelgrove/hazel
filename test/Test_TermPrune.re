open Alcotest;
open Haz3lcore;
open Language;

/* value pruning marks its elisions, so a value the worker already cut
   down still reads as truncated */

let exp_of = (src: string): Exp.t =>
  switch (FastParse.of_text(~root=Exp, src)) {
  | Some(seg) => MakeTerm.Incr.term_of(seg)
  | None => fail("parse failed: " ++ src)
  };

let ints = n =>
  "[" ++ String.concat(", ", List.init(n, string_of_int)) ++ "]";

let constructor_value = () => {
  let (pruned, truncated) =
    TermPrune.prune(~budget=20, exp_of("Some(" ++ ints(200) ++ ")"));
  check(bool, "truncated", true, truncated);
  check(bool, "elision marked", true, TermPrune.has_elision(pruned));
  check(
    bool,
    "small enough to pass the display's size check",
    false,
    Web.EvalResult.exceeds_display_budget(pruned),
  );
  check(
    bool,
    "still reads as truncated",
    true,
    Web.EvalResult.value_truncated(pruned),
  );
  switch (pruned.term) {
  | Ap(_, {term: Constructor("Some", _), _}, _) => ()
  | _ => fail("the outer constructor was lost")
  };
};

let list_tail = () => {
  let (pruned, truncated) = TermPrune.prune(~budget=20, exp_of(ints(200)));
  check(bool, "truncated", true, truncated);
  check(bool, "elision marked", true, TermPrune.has_elision(pruned));
  switch (pruned.term) {
  | ListLit(items) =>
    check(bool, "a head survives", true, List.length(items) > 1)
  | _ => fail("expected a list")
  };
};

let own_holes = () => {
  let e = exp_of("(?, 1, [?])");
  let (pruned, truncated) = TermPrune.prune(~budget=100, e);
  check(bool, "not truncated", false, truncated);
  check(bool, "intact", true, pruned === e);
  check(bool, "no elision", false, TermPrune.has_elision(e));
  check(
    bool,
    "not truncated for display",
    false,
    Web.EvalResult.value_truncated(e),
  );
};

let tests = (
  "TermPrune",
  [
    test_case("an over-budget constructor value", `Quick, constructor_value),
    test_case("a long list keeps its head", `Quick, list_tail),
    test_case("a value's own holes are not elisions", `Quick, own_holes),
  ],
);
