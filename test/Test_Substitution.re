open Alcotest;
open Language;

let exp = testable(Fmt.using(Exp.show, Fmt.string), Equality.semantic.exp);

open IdTagged.FreshGrammar;

/* The rules at the top of FreeVars.re, checked on generated environments and
   terms over a few names, so binders often shadow: closures and fixpoints
   carrying environments, values with free names, and `x ↦ x` entries. */
module Rules = {
  module Names = FreeVars.Names;

  let gen_name = QCheck.Gen.oneofl(["a", "b", "x", "y"]);

  let rec gen_term = (depth: int): QCheck.Gen.t(Language.Exp.t) => {
    let leaf =
      QCheck.Gen.oneof([
        QCheck.Gen.map(Exp.var, gen_name),
        QCheck.Gen.return(Exp.int(1)),
      ]);
    if (depth <= 0) {
      leaf;
    } else {
      let sub = gen_term(depth - 1);
      QCheck.Gen.frequency([
        (2, leaf),
        (
          2,
          QCheck.Gen.map2(
            (x, body) => Exp.fn(Pat.var(x), body, None, None),
            gen_name,
            sub,
          ),
        ),
        (
          2,
          QCheck.Gen.map3(
            (x, def, body) => Exp.let_(Pat.var(x), def, body),
            gen_name,
            sub,
            sub,
          ),
        ),
        (1, QCheck.Gen.map2((f, a) => Exp.ap(Forward, f, a), sub, sub)),
        (
          1,
          QCheck.Gen.map2(
            (env, body) => Exp.closure(Environment.of_list(env), body),
            gen_env(depth - 1),
            sub,
          ),
        ),
        (
          1,
          QCheck.Gen.map3(
            (x, body, env) =>
              Exp.fix_f(Pat.var(x), body, Some(Environment.of_list(env))),
            gen_name,
            sub,
            gen_env(depth - 1),
          ),
        ),
      ]);
    };
  }
  and gen_env = (depth: int): QCheck.Gen.t(list((Var.t, Language.Exp.t))) => {
    let entry =
      QCheck.Gen.(
        gen_name
        >>= (
          x =>
            frequency([
              (1, return((x, Exp.var(x)))),
              (3, map(value => (x, value), gen_term(depth))),
            ])
        )
      );
    QCheck.Gen.map(
      entries =>
        List.fold(
          ~f=
            (acc, (x, value)) =>
              List.Assoc.mem(acc, x, ~equal=String.equal)
                ? acc : acc @ [(x, value)],
          ~init=[],
          entries,
        ),
      QCheck.Gen.(list_size(int_bound(3), entry)),
    );
  };

  let rec show_term = (e: Language.Exp.t): string =>
    switch (e.term) {
    | Var(x) => x
    | Atom(_) => "1"
    | Fun({term: Var(x), _}, body, _, _) =>
      "(fun " ++ x ++ " -> " ++ show_term(body) ++ ")"
    | Let({term: Var(x), _}, def, body) =>
      "(let "
      ++ x
      ++ " = "
      ++ show_term(def)
      ++ " in "
      ++ show_term(body)
      ++ ")"
    | Ap(_, f, a) => show_term(f) ++ "(" ++ show_term(a) ++ ")"
    | Closure(env, body) => "[" ++ show_env(env) ++ "]" ++ show_term(body)
    | FixF({term: Var(x), _}, body, env) =>
      "(fix "
      ++ x
      ++ " -> "
      ++ show_term(body)
      ++ ")"
      ++ Option.value_map(
           ~default="",
           ~f=env => "[" ++ show_env(env) ++ "]",
           env,
         )
    | _ => Language.Exp.show(e)
    }
  and show_env = env =>
    Environment.to_list(env)
    |> List.map(~f=((x, value)) => x ++ " ↦ " ++ show_term(value))
    |> String.concat(~sep=", ");

  let free_names = (e: Language.Exp.t): Names.t =>
    FreeVars.all(FreeVars.exp(e));

  /* Rule: the result's names are the term's open names less those replaced,
     the names of the values inserted, and the term's sealed names. */
  let expected_names = (env, e: Language.Exp.t): Names.t => {
    let names = FreeVars.exp(e);
    Names.fold(
      (x, acc) =>
        switch (FreeVars.replacement(env, x)) {
        | Some(value) => Names.union(free_names(value), acc)
        | None => Names.add(x, acc)
        },
      names.vars,
      names.sealed,
    );
  };

  let has_closure = (e: Language.Exp.t): bool => {
    let found = ref(false);
    let _ =
      Language.Exp.map_term(
        ~f_exp=
          (cont, e) => {
            switch (e.term) {
            | Closure(_)
            | FixF(_, _, Some(_)) => found := true
            | _ => ()
            };
            cont(e);
          },
        e,
      );
    found^;
  };

  /* Rule: a binder is renamed only if its name is sealed beneath it or free in
     a value inserted beneath it. Walks the term and the result together;
     `inserts` maps each name the environment in force replaces to the names
     its value brings in. */
  let needless_renames =
      (env, e: Language.Exp.t, result: Language.Exp.t): list(string) => {
    let found = ref([]);
    let inserts_of = env =>
      Environment.fold(
        ((x, _), acc) =>
          switch (FreeVars.replacement(env, x)) {
          | Some(value) =>
            Util.Maps.StringMap.add(x, free_names(value), acc)
          | None => acc
          },
        Util.Maps.StringMap.empty,
        env,
      );
    let rec walk = (inserts, e: Language.Exp.t, r: Language.Exp.t) =>
      switch (e.term, r.term) {
      | (Closure(env, body), _) => walk(inserts_of(env), body, r)
      | (FixF(p, body, Some(env)), FixF(p', body', None)) =>
        binder(inserts_of(env), p, body, p', body')
      | (Fun(p, body, _, _), Fun(p', body', _, _))
      | (FixF(p, body, None), FixF(p', body', None)) =>
        binder(inserts, p, body, p', body')
      | (Let(p, def, body), Let(p', def', body')) =>
        walk(inserts, def, def');
        binder(inserts, p, body, p', body');
      | (Ap(_, f, a), Ap(_, f', a')) =>
        walk(inserts, f, f');
        walk(inserts, a, a');
      | _ => ()
      }
    and binder = (inserts, p: Language.Pat.t, body, p': Language.Pat.t, body') =>
      switch (p.term, p'.term) {
      | (Var(x), Var(x')) =>
        let beneath = FreeVars.exp(body);
        let inserted =
          Names.fold(
            (y, acc) =>
              switch (Util.Maps.StringMap.find_opt(y, inserts)) {
              | Some(names) when !String.equal(y, x) =>
                Names.union(names, acc)
              | _ => acc
              },
            beneath.vars,
            Names.empty,
          );
        if (!String.equal(x, x')
            && !Names.mem(x, inserted)
            && !Names.mem(x, beneath.sealed)) {
          found := [x ++ " became " ++ x', ...found^];
        };
        let inserts =
          String.equal(x, x')
            ? Util.Maps.StringMap.remove(x, inserts)
            : Util.Maps.StringMap.add(x, Names.singleton(x'), inserts);
        walk(inserts, body, body');
      | _ => ()
      };
    walk(inserts_of(env), e, result);
    found^;
  };

  let property =
    QCheck.Test.make(
      ~name="Substitution keeps every name and renames only to avoid capture",
      ~count=2000,
      QCheck.make(
        ~print=
          ((env, e)) =>
            show_env(Environment.of_list(env)) ++ " into " ++ show_term(e),
        QCheck.Gen.pair(gen_env(2), gen_term(4)),
      ),
      ((entries, e)) => {
        let env = Environment.of_list(entries);
        let result = Substitution.in_exp(env, e);
        let show_names = names =>
          String.concat(~sep=", ", Names.elements(names));
        if (has_closure(result)) {
          QCheck.Test.fail_reportf("closure left in %s", show_term(result));
        };
        let expected = expected_names(env, e);
        let actual = free_names(result);
        if (!Names.equal(expected, actual)) {
          QCheck.Test.fail_reportf(
            "result %s has free names {%s}, expected {%s}",
            show_term(result),
            show_names(actual),
            show_names(expected),
          );
        };
        switch (needless_renames(env, e, result)) {
        | [] => true
        | renames =>
          QCheck.Test.fail_reportf(
            "result %s renames without need: %s",
            show_term(result),
            String.concat(~sep=", ", renames),
          )
        };
      },
    );
};

let tests = (
  "Substitution",
  [
    // Basic variable substitution
    test_case(
      "substitute variable with integer",
      `Quick,
      () => {
        let env = Environment.of_list([("x", Exp.int(42))]);
        let expr = Exp.tuple([Exp.var("x"), Exp.var("x")]);
        let result = Substitution.in_exp(env, expr);
        check(
          exp,
          "x -> 42",
          Exp.tuple([Exp.int(42), Exp.int(42)]),
          result,
        );
      },
    ),
    /* A renamed binder must also avoid the names free in the body it scopes
       over: in `let f = fun z -> x in fun x -> f(x')`, with x and x'
       unbound, `fun x` must not become `fun x'`, capturing the body's x'. */
    test_case(
      "a renamed binder avoids the names free in its body",
      `Quick,
      () => {
        let env =
          Environment.of_list([
            ("f", Exp.fn(Pat.var("z"), Exp.var("x"), None, None)),
          ]);
        let expr =
          Exp.fn(
            Pat.var("x"),
            Exp.ap(Forward, Exp.var("f"), Exp.var("x'")),
            None,
            None,
          );
        let result = Substitution.in_exp(env, expr);
        switch (result.term) {
        | Fun({term: Var(binder), _}, _, _, _) =>
          check(
            bool,
            "binder " ++ binder ++ " is neither x nor x'",
            true,
            !String.equal(binder, "x") && !String.equal(binder, "x'"),
          )
        | _ => fail("expected a function, got " ++ Language.Exp.show(result))
        };
      },
    ),
    /* Stepping `let y = 1 in (fun x -> x + y)(x)` with x unbound: x is free
       in the term but nothing inserted mentions it, so `fun x` keeps its
       name. */
    test_case(
      "a binder that captures nothing inserted keeps its name",
      `Quick,
      () => {
        let env = Environment.of_list([("y", Exp.int(1))]);
        let expr =
          Exp.ap(
            Forward,
            Exp.fn(
              Pat.var("x"),
              Exp.bin_op(Int(Plus), Exp.var("x"), Exp.var("y")),
              None,
              None,
            ),
            Exp.var("x"),
          );
        let result = Substitution.in_exp(env, expr);
        switch (result.term) {
        | Ap(_, {term: Fun({term: Var(binder), _}, _, _, _), _}, _) =>
          check(string, "binder", "x", binder)
        | _ =>
          fail("expected an application, got " ++ Language.Exp.show(result))
        };
      },
    ),
    /* A closure brings in the names its environment's values mention:
       substituting f := (fun a -> b with b = n) into `fun n -> f` must
       rename the `n`. */
    test_case(
      "a name a nested closure brings in is avoided",
      `Quick,
      () => {
        let closure =
          Exp.closure(
            Environment.of_list([("b", Exp.var("n"))]),
            Exp.fn(Pat.var("a"), Exp.var("b"), None, None),
          );
        let env = Environment.of_list([("f", closure)]);
        let expr = Exp.fn(Pat.var("n"), Exp.var("f"), None, None);
        let result = Substitution.in_exp(env, expr);
        switch (result.term) {
        | Fun({term: Var(binder), _}, _, _, _) =>
          check(
            bool,
            "binder " ++ binder ++ " is not n",
            true,
            !String.equal(binder, "n"),
          )
        | _ => fail("expected a function, got " ++ Language.Exp.show(result))
        };
      },
    ),
    /* Likewise a fixpoint the evaluator closed over its environment:
       substituting f := (fix g -> fun a -> b with b = n) into `fun n -> f`
       must rename the `n`. */
    test_case(
      "a name a nested fixpoint brings in is avoided",
      `Quick,
      () => {
        let fixpoint =
          Exp.fix_f(
            Pat.var("g"),
            Exp.fn(Pat.var("a"), Exp.var("b"), None, None),
            Some(Environment.of_list([("b", Exp.var("n"))])),
          );
        let env = Environment.of_list([("f", fixpoint)]);
        let expr = Exp.fn(Pat.var("n"), Exp.var("f"), None, None);
        let result = Substitution.in_exp(env, expr);
        switch (result.term) {
        | Fun({term: Var(binder), _}, _, _, _) =>
          check(
            bool,
            "binder " ++ binder ++ " is not n",
            true,
            !String.equal(binder, "n"),
          )
        | _ => fail("expected a function, got " ++ Language.Exp.show(result))
        };
      },
    ),
    /* A closure seals the names it mentions: substituting b' := 2 into
       `(fun b -> (v with v = b' + 0)) with b = 1` replaces nothing inside,
       and `fun b` must not become `fun b'`, capturing the b' the inner
       closure brings in. */
    test_case(
      "a name a nested closure brings in stays free",
      `Quick,
      () => {
        let env = Environment.of_list([("b'", Exp.int(2))]);
        let inner =
          Exp.closure(
            Environment.of_list([
              ("v", Exp.bin_op(Int(Plus), Exp.var("b'"), Exp.int(0))),
            ]),
            Exp.var("v"),
          );
        let expr =
          Exp.closure(
            Environment.of_list([("b", Exp.int(1))]),
            Exp.fn(Pat.var("b"), inner, None, None),
          );
        let result = Substitution.in_exp(env, expr);
        switch (result.term) {
        | Fun({term: Var(binder), _}, _, _, _) =>
          check(
            bool,
            "binder " ++ binder ++ " is not b'",
            true,
            !String.equal(binder, "b'"),
          );
          check(
            bool,
            "b' is free in " ++ Language.Exp.show(result),
            true,
            FreeVars.Names.mem("b'", FreeVars.all(FreeVars.exp(result))),
          );
        | _ => fail("expected a function, got " ++ Language.Exp.show(result))
        };
      },
    ),
    // Capture avoidance
    test_case(
      "capture avoidance",
      `Quick,
      () => {
        let env =
          Environment.of_list([("x", Exp.var("x")), ("y", Exp.var("x"))]);
        let expr = Exp.fn(Pat.var("x"), Exp.var("y"), None, None);
        let result = Substitution.in_exp(env, expr);
        let expected = Exp.fn(Pat.var("x'"), Exp.var("x"), None, None);
        check(exp, "x -> x in fn x. x", expected, result);
      },
    ),
    // Shadowing
    test_case(
      "shadowing",
      `Quick,
      () => {
        let env =
          Environment.of_list([("x", Exp.int(1)), ("y", Exp.int(2))]);
        let expr =
          Exp.let_(
            Pat.var("x"),
            Exp.int(3),
            Exp.tuple([Exp.var("x"), Exp.var("y")]),
          );
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.let_(
            Pat.var("x"),
            Exp.int(3),
            Exp.tuple([Exp.var("x"), Exp.int(2)]),
          );
        check(exp, "x -> 1, y -> 2 in let x = 3 in (x, y)", expected, result);
      },
    ),
    /* Freshening a shadowing binder must not rewrite free refs on the RHS. */
    test_case(
      "shadowing let RHS keeps outer var",
      `Quick,
      () => {
        let env = Environment.of_list([("y", Exp.var("y"))]);
        let expr = Exp.let_(Pat.var("y"), Exp.var("y"), Exp.var("y"));
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.let_(Pat.var("y'"), Exp.var("y"), Exp.var("y'"));
        check(exp, "let y = y in y  =>  let y' = y in y'", expected, result);
      },
    ),
    /* A `Sig` type carries patterns, so substituting through a function's
       annotation must reach under it. Statics desugars `Sig` away, so build
       it directly. */
    test_case(
      "substitute through a Sig-annotated function",
      `Quick,
      () => {
        let sig_ty = Language.Typ.fresh(Sig([Sig.sig_let(Pat.var("y"))]));
        let env = Environment.of_list([("x", Exp.int(1))]);
        let expr = Exp.fn(Pat.var("p"), Exp.var("x"), Some(sig_ty), None);
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.fn(Pat.var("p"), Exp.int(1), Some(sig_ty), None);
        check(
          exp,
          "substitution reaches under a Sig annotation",
          expected,
          result,
        );
      },
    ),
    // Fixpoints
    test_case(
      "substitute in fixpoint",
      `Quick,
      () => {
        let env =
          Environment.of_list([("f", Exp.var("g")), ("g", Exp.var("h"))]);
        let expr = Exp.fix_f(Pat.var("f"), Exp.var("f"), None);
        let result = Substitution.in_exp(env, expr);
        let expected = Exp.fix_f(Pat.var("f"), Exp.var("f"), None);
        check(exp, "f -> g, g -> h in fix f. f", expected, result);
      },
    ),
    /* A let's binder scopes over its body, not its definition: in
       `let x = (x, 2) in x` the definition's x is the OUTER x. The binder
       is renamed away from capture (x is in env), and the definition once
       got that renaming too, `let x' = (x', 2) in x'`, which left it
       unbound. */
    test_case(
      "a let's definition sees the outer binding",
      `Quick,
      () => {
        let env = Environment.of_list([("x", Exp.int(1))]);
        let expr =
          Exp.let_(
            Pat.var("x"),
            Exp.tuple([Exp.var("x"), Exp.int(2)]),
            Exp.var("x"),
          );
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.let_(
            Pat.var("x"),
            Exp.tuple([Exp.int(1), Exp.int(2)]),
            Exp.var("x"),
          );
        check(exp, "x -> 1 in let x = (x, 2) in x", expected, result);
      },
    ),
    QCheck_alcotest.to_alcotest(Rules.property),
  ],
);
