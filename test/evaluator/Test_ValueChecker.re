/* A probe sample is a closure: the closure carries the environment the form
   needs. Whether such a sample is a value is a question about the form inside,
   so a stuck result (`if ? then ...`) must not be mistaken for a function. */
open Alcotest;
open Language;
open IdTagged.FreshGrammar;

let var_f = Exp.var("f");
let a_fun = Exp.fn(Pat.var("x"), Exp.var("x"), None, None);
let a_stuck_if = Exp.if_(Exp.empty_hole(), Exp.int(1), Exp.int(2));

let env_with = (bindings) =>
  List.fold_left(
    (env, (name, v)) => Environment.extend(env, (name, v)),
    Environment.empty,
    bindings,
  );

let check_is_value = (name, expected, env, body) => {
  let sample = Exp.closure(env, body);
  check(bool, name, expected, ValueChecker.is_value(sample));
};

let tests = (
  "ValueChecker.Closure",
  [
    test_case("a finished form is a value", `Quick, () => {
      check_is_value("a literal", true, Environment.empty, Exp.int(1));
      check_is_value("a function", true, Environment.empty, a_fun);
      check_is_value(
        "a type function",
        true,
        Environment.empty,
        Exp.typ_fun(TPat.var("a"), Exp.var("x"), None),
      );
    }),
    test_case("a stuck result is not a value", `Quick, () => {
      check_is_value("an if on a hole", false, Environment.empty, a_stuck_if);
      check_is_value("a bare variable", false, Environment.empty, var_f);
    }),
    /* A recursive function is held as a variable naming its own fixpoint, so
       the form has to be resolved before it can be classified. */
    test_case("a variable resolves through the closure's env", `Quick, () => {
      check_is_value(
        "var -> Function",
        true,
        env_with([("f", a_fun)]),
        var_f,
      );
      check_is_value(
        "var -> fixpoint of a function",
        true,
        env_with([("f", Exp.fix_f(Pat.var("f"), a_fun, None))]),
        var_f,
      );
      check_is_value(
        "var -> var -> Function",
        true,
        env_with([("f", Exp.var("h")), ("h", a_fun)]),
        var_f,
      );
      check_is_value(
        "var -> fixpoint of a stuck form",
        false,
        env_with([("f", Exp.fix_f(Pat.var("f"), a_stuck_if, None))]),
        var_f,
      );
      /* Resolving must terminate even when a binding names itself. */
      check_is_value(
        "var -> var (self-referential)",
        false,
        env_with([("f", var_f)]),
        var_f,
      );
    }),
  ],
);
