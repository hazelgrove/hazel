/* A probe sample is a closure: the closure carries the environment the form
   needs. Whether such a sample is a value is a question about the form inside,
   so a stuck result (`if ? then ...`) must not be mistaken for a function. */
open Alcotest;
open Language;
open IdTagged.FreshGrammar;

let var_f = Exp.var("f");
let a_fun = Exp.fn(Pat.var("x"), Exp.var("x"), None, None);
let a_stuck_if = Exp.if_(Exp.empty_hole(), Exp.int(1), Exp.int(2));

let env_with = bindings =>
  List.fold(
    ~f=(env, (name, v)) => Environment.extend(env, (name, v)),
    ~init=Environment.empty,
    bindings,
  );

let check_is_value = (name, expected, env, body) => {
  let sample = Exp.closure(env, body);
  check(bool, name, expected, ValueChecker.is_value(sample));
};

let tests = (
  "ValueChecker.Closure",
  [
    /* The elaborated fixpoint ascribes its body when the body's type differs
       from the binder's; the fixpoint is still a function. */
    test_case(
      "A recursive function with an ascribed body is a value",
      `Quick,
      () => {
        let uexp =
          switch (
            Haz3lcore.Parser.to_term("let x = fun y -> x in x", ~root=Exp)
          ) {
          | Some(e) => e
          | None => fail("Failed to parse")
          };
        let (_, elab) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
        let fixes = ref([]);
        let _ =
          Language.Exp.map_term(
            ~f_exp=
              (continue, e: Language.Exp.t) => {
                switch (e.term) {
                | FixF(_) => fixes := [e, ...fixes^]
                | _ => ()
                };
                continue(e);
              },
            elab,
          );
        switch (fixes^) {
        | [fix] =>
          check_is_value("let x = fun y -> x", true, Environment.empty, fix)
        | _ => fail("expected one fixpoint in " ++ Language.Exp.show(elab))
        };
      },
    ),
    test_case(
      "a finished form is a value",
      `Quick,
      () => {
        check_is_value("a literal", true, Environment.empty, Exp.int(1));
        check_is_value("a function", true, Environment.empty, a_fun);
        check_is_value(
          "a type function",
          true,
          Environment.empty,
          Exp.typ_fun(TPat.var("a"), Exp.var("x"), None),
        );
      },
    ),
    test_case(
      "a stuck result is not a value",
      `Quick,
      () => {
        check_is_value(
          "an if on a hole",
          false,
          Environment.empty,
          a_stuck_if,
        );
        check_is_value("a bare variable", false, Environment.empty, var_f);
      },
    ),
    /* A recursive function is held as a variable naming its own fixpoint, so
       the form has to be resolved before it can be classified. */
    test_case(
      "a variable resolves through the closure's env",
      `Quick,
      () => {
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
      },
    ),
  ],
);
